module Visa.Resources where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Foldable (foldrM)

import Control.Exception (bracket)

import Visa.Status
import Visa.Dll.Visa

vi_char_buffer_size :: Int
vi_char_buffer_size = 256 -- bytes

_defaultSession :: (Ptr (ViSession) -> IO (ViStatus)) -> String -> IO (ViSession)
_defaultSession f name = alloca (\session -> do
    error <- f session
    value <- peek session
    checkDetails value error value name)

-- Open the Default Resource Manager Session
--
-- The session should be closed using `close` one the session is complete
--
-- Example:
-- > s <- defaultSession
-- > devices <- find s "?*::INSTR"
-- > close s
defaultSession :: IO (ViSession)
defaultSession = _defaultSession viOpenDefaultRM "viOpenDefaultRM"

-- Open the Default Session and automatically close when complete
--
-- Example:
--
-- with (\s -> do
--     devices <- find s "?*::INSTR"
--     putStrLn (unlines devices)
-- )
withSession :: (ViSession -> IO b) -> IO b
withSession = bracket defaultSession (\s -> close s) 

_close :: (ViObject -> IO (ViStatus)) -> String -> ViObject -> IO ()
_close f msg obj = do
    error <- f obj
    check error () msg

-- Close the opened object.
--
-- This object can be a ViSession, ViFindList, or another thing. 
close :: ViObject -> IO ()
close = _close viClose "viClose"

-- Find all attached devices based on the query, and return a list
-- 
-- Args:
--   session -> From `defaultSession`
--   query -> Query String. REGEX Like. A good default is "?*::INSTR"
--
-- Returns:
--   List of connected devices (as a list of strings)
--
-- Example:
-- 
-- withSession (\s -> do 
--   devices <- find s "?*::INSTR"
--   putStrLn (unlines devices)
--   )
find :: ViSession -> String -> IO [String]
find session query = bracket 
    (findResource session query) 
    (\(s, d, c) -> close s) 
    (\(find_session, description, connected) -> do
        if connected < 1
        then return []
        else do
            if connected == 1
            then return [description]
            else do -- > 1
                others <- _find find_session (connected - 1)
                return ([description] ++ others)
    )

findResource :: ViSession -> String -> IO (ViFindList, String, Integer)
findResource = _findResource viFindRsrc "viFindRsrc"

-- Type for the viFindRsrc function
type FindResourceFunction = (ViSession -> ViConstString -> Ptr (ViFindList) -> Ptr (ViUInt32) -> ViChar -> IO (ViStatus)) 

-- Used by `find` to get the number of connected devices
_findResource :: FindResourceFunction -> String 
                 -> ViSession 
                 -> String 
                 -> IO (ViFindList, String, Integer)
_findResource vi msg session query = withCString query (\c_query -> 
    allocaBytes vi_char_buffer_size (\description -> 
        alloca (\find_list -> 
            alloca (\count -> do
                error <- vi session c_query find_list count description
                total <- fmap toInteger (peek count)
                find_session <- peek find_list
                desc <- peekCString description
                checkDetails session error (find_session, desc, total) msg 
                ))))

type FindNextFunction = ViFindList -> ViChar -> IO (ViStatus)

_findNext :: FindNextFunction -> String -> ViFindList -> IO (String)
_findNext vi msg find_session = allocaBytes vi_char_buffer_size (\description -> do
    error <- vi find_session description
    desc <- peekCString description
    checkDetails find_session error desc msg)

-- Used by `find` to get the next connected device
findNext :: ViFindList -> IO (String)
findNext = _findNext viFindNext "viFindNext"
            
-- Find the remaining devices using `findNext`
_find fs r = foldrM x [] [1..r]
    where x _ a = do
            desc <- findNext fs
            return $ (desc:a)

data InterfaceType = RESERVED_INTERFACE_TYPE 
                   | GPIB
                   | VXI
                   | GPIB_VXI
                   | ASRL
                   | PXI
                   | TCPIP
                   | RIO
                   | FIREWIRE deriving (Enum, Eq, Show)

-- Get some information about the resource
--
-- Args:
--   session -> Session from `defaultSession`
--   name -> resource name from `find`
--
-- Returns:
--   (InterfaceType, interface_board_number)
--
parseResource :: ViSession -> String -> IO (InterfaceType, Integer)
parseResource session name = withCString name (\c_name ->
    alloca (\c_it ->
        alloca (\c_ibn-> do
            error <- viParseRsrc session c_name c_it c_ibn
            interface_type <- fmap (toEnum . fromIntegral) (peek c_it)
            interface_board_number <- fmap toInteger (peek c_ibn)
            checkDetails session error (interface_type, interface_board_number) "viParseRsrc")))

-- Get more information about the resource
--
-- Args:
--   session -> Session from `defaultSession`
--   name -> resource name from `find`
--
-- Returns:
--   (interface_type, interface_board_number, Resource class, Expanded Resource name, Alias)
parseResourceEx :: ViSession -> String -> IO (InterfaceType, Integer, String, String, String)
parseResourceEx session name = withCString name (\c_name ->
    alloca (\c_it ->
        alloca (\c_ibn -> 
            allocaBytes vi_char_buffer_size (\c_rsrc_name -> 
                allocaBytes vi_char_buffer_size (\c_rsrc_name_ex -> 
                    allocaBytes vi_char_buffer_size (\c_alias -> do
                        error <- viParseRsrcEx session c_name c_it c_ibn c_rsrc_name c_rsrc_name_ex c_alias
                        interface_type <- fmap (toEnum . fromIntegral) (peek c_it)
                        interface_board_number <- fmap toInteger (peek c_ibn)
                        resource_name <- peekCString c_rsrc_name
                        resource_name_ex <- peekCString c_rsrc_name_ex
                        alias <- peekCString c_alias
                        checkDetails session error (interface_type, interface_board_number, resource_name, resource_name_ex, alias) "viParseRsrc"))))))

data AccessMode = NO_LOCK
                | EXCLUSIVE_LOCK 
                | SHARED_LOCK deriving (Show, Eq, Enum)

-- Open the specified Resource
--
-- Resources need to be closed using `close` if explicitly opened using this
-- function
--
-- Args:
--   session -> Session from `defaultSession`
--   name -> resource name from `find`
--   access -> Access Mode (NO_LOCK, EXCLUSIVE_LOCK, SHARED_LOCK). A good Default is NO_LOCK
--   timeout -> Resource Access Timeout in ms. A good default is 2000 (ms)
--
-- Returns:
--   Resource Session
--
-- Example:
-- 
-- withSession (\s -> do
--   devices <- find s "?*::INSTR"
--   resource <- openResource s (head devices) NO_LOCK 2000
--   response <- query resource "*IDN?"
--   putStrLn response
--   close resource
-- )
openResource :: ViSession -> String -> AccessMode -> Integer -> IO (ViSession)
openResource session name access timeout = withCString name (\c_name -> 
    alloca (\c_resource -> do
        let c_access = fromIntegral (fromEnum access) :: ViAccessMode
        let c_timeout = (fromInteger timeout) :: ViUInt32
        error <- viOpen session c_name c_access c_timeout c_resource
        resource <- peek c_resource
        checkDetails session error resource "viOpen"))

-- Open the specified Resource with Access Mode NO_LOCK
--
-- Resources need to be closed using `close` if explicitly opened using this
-- function
--
-- This is a version of `openResource` just using the default access mode
--
-- Args:
--   session -> Session from `defaultSession`
--   name -> resource name from `find`
--   timeout -> Resource Access Timeout in ms. A good default is 2000 (ms)
--
-- Returns:
--   Resource Session
--
-- withSession (\s -> do
--   devices <- find s "?*::INSTR"
--   resource <- open s (head devices) 2000
--   response <- query resource "*IDN?"
--   putStrLn response
--   close resource
-- )
open :: ViSession -> String -> Integer -> IO (ViSession)
open session name timeout = openResource session name NO_LOCK timeout

-- Open the specified Resource with the provided device string
--
-- Example:
-- withSession (\s -> do
--   devices <- find s "?*::INSTR"
--   withResource s (head devices) 2000 (\resource -> do
--     response <- query "*IDN?"
--     putStrLn response
--   )
-- )
withResource :: ViSession -> String -> Integer -> (ViSession -> IO (b)) -> IO b
withResource session device_name timeout = bracket
    (open session device_name timeout)
    (\s -> close s)

success_max_count_read :: ViStatus
success_max_count_read = 0x3FFF0006

-- Read Raw
--
-- This function calls `viRead` until the status is not "Success Max Count Read"
--
-- Args:
--   resource -> Opened resource session (from `open`)
--   size -> Number of bytes to read. Default from PyVisa is 2048
--
-- Returns:
--   Read bytes
readBytes :: ViSession -> Integer -> IO B.ByteString
readBytes resource size = let size_int = fromIntegral size in
    allocaBytes size_int (\buffer -> 
        alloca (\return_count -> do
            let size_c = fromIntegral size :: ViUInt32

            status <- viRead resource buffer size_c return_count
            length <- fmap fromIntegral $ peek return_count
            chunk_bs <- B.packCStringLen ((castPtr buffer), length)

            if status == success_max_count_read
            then do
                c2 <- readBytes resource size
                (return $ B.append chunk_bs c2)
            else return chunk_bs
            ))

-- Read a String 
--
-- Args:
--   resource -> Opened resource session (from `open`)
--
-- Returns:
--   Response as a String
readString :: ViSession -> IO String
readString resource = do
    bytes <- readBytes resource 2048
    let result = BC.unpack bytes
    return result

-- Write Bytes to the Visa Resourece
--
-- Args:
--   resource -> Opened resource session (from `open`)
--   bytes -> Bytestring to write to the resource
--
-- Returns:
--   Number of Bytes written to the Device
writeBytes :: ViSession -> B.ByteString -> IO (Integer)
writeBytes resource bytes = do
    alloca (\bytes_written -> 
        B.useAsCStringLen bytes (\(c_string, length) -> do
            let c_bytes = castPtr c_string
            let c_len = fromIntegral length :: ViUInt32
            error <- viWrite resource c_bytes c_len bytes_written
            written <- fmap toInteger $ peek bytes_written
            checkDetails resource error written "viWRite"))

-- Write the provided string to the Visa Resource
--
-- Args:
--   resource -> Opened resource session (from `open`)
--   message -> Message to send
--
-- Returns:
--   Number of Bytes written to the Device
writeString :: ViSession -> String -> IO (Integer)
writeString resource message = do
    writeBytes resource (BC.pack message)

-- Write the provided string to the Visa Resource
--
-- Args:
--   resource -> Opened resource session (from `open`)
--   message -> Message to send
--
-- Returns:
--   Number of Bytes written to the Device
write = writeString

-- Write a message to the Visa Resource, and read the response
--
-- Uses: `writeString` and `readString`
--
-- Args:
--   resource -> Opened resource session (from `open`)
--   message -> Message to send
--
-- Returns:
--   Response 
--
-- Example:
-- withSession (\s -> do
--   devices <- find s "?*::INSTR"
--   withResource s (head devices) 2000 (\resource -> do
--     response <- query "*IDN?"
--     putStrLn response
--   )
-- )
query :: ViSession -> String -> IO (String)
query resource message = do
    writeString resource message
    readString resource

-- Write a message to the Visa Resource and read the response (but in Bytes)
queryBytes :: ViSession -> B.ByteString -> IO (B.ByteString)
queryBytes resource msg = do
    writeBytes resource msg
    readBytes resource 2048
