module Visa.Resources (defaultSession
                      ,close
                      ,find

                      ,parseResource
                      ,parseResourceEx

                      ,open
                      ,AccessMode(..)
                      ,InterfaceType(..)
                      ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.Foldable (foldrM)

import Visa.Status
import Visa.Dll.Visa

-- TODO withSession + bracket

vi_char_buffer_size :: Int
vi_char_buffer_size = 256 -- bytes

defaultSession :: IO (ViSession)
defaultSession = alloca (\session -> do
    error <- viOpenDefaultRM session
    value <- peek session
    check error value "viOpenDefaultRM")

close :: ViObject -> IO ()
close obj = do
    error <- viClose obj 
    check error () "viClose"

-- Find all attached devices based on the query, and return a list
-- 
-- Args:
--   session -> From `defaultSession`
--   query -> Query String. REGEX Like. A good default is "?*::INSTR"
--
-- Returns:
--   List of connected devices (as a list of strings)
find :: ViSession -> String -> IO [String]
find session query = do
    (find_session, description, connected) <- findResource session query
    if connected < 1
    then do
        close find_session
        return []
    else do
        if connected == 1
        then do 
            close find_session
            return [description]
        else do -- > 1
            others <- _find find_session (connected - 1)
            close find_session
            return ([description] ++ others)

-- Internal Only
findResource :: ViSession -> String -> IO (ViFindList, String, Integer)
findResource session query = withCString query (\c_query -> 
    allocaBytes vi_char_buffer_size (\description -> 
        alloca (\find_list -> 
            alloca (\count -> do
                error <- viFindRsrc session c_query find_list count description
                total <- fmap toInteger (peek count)
                find_session <- peek find_list
                desc <- peekCString description
                check error (find_session, desc, total) "viFindRsrc"
                ))))

findNext :: ViFindList -> IO (String)
findNext find_session = allocaBytes vi_char_buffer_size (\description -> do
    error <- viFindNext find_session description
    desc <- peekCString description
    -- Check the error 
    check error desc "viFindNext")
            
-- Find the remainind devices using `findNext`
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
            check error (interface_type, interface_board_number) "viParseRsrc")))

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
                        check error (interface_type, interface_board_number, resource_name, resource_name_ex, alias) "viParseRsrc"))))))

data AccessMode = NO_LOCK
                | EXCLUSIVE_LOCK 
                | SHARED_LOCK deriving (Show, Eq, Enum)

-- Open the specified Resource
--
-- Args:
--   session -> Session from `defaultSession`
--   name -> resource name from `find`
--   access -> Access Mode (NO_LOCK, EXCLUSIVE_LOCK, SHARED_LOCK). A good Default is NO_LOCK
--   timeout -> Resource Access Timeout in ms. A good default is 2000 (ms)
--
-- Returns:
--   Resource Session
open :: ViSession -> String -> AccessMode -> Integer -> IO (ViSession)
open session name access timeout = withCString name (\c_name -> 
    alloca (\c_resource -> do
        let c_access = fromIntegral (fromEnum access) :: ViAccessMode
        let c_timeout = (fromInteger timeout) :: ViUInt32
        error <- viOpen session c_name c_access c_timeout c_resource
        resource <- peek c_resource
        check error resource "viOpen"))

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
-- readRaw :: ViSession -> Integer -> IO (String)
-- readRaw resource size = allocaBytes size (\buffer -> 
--     alloca (\return_count -> do
--         status <- viRead session buffer return_count
--         length <- peek return_count
--         chunk <- peekCStringLen (buffer, length)
--         if status == SUCCESS_MAX_COUNT_READ
--         then (return chuck:(readRaw resource size))
--         else (return [])
--         ))
