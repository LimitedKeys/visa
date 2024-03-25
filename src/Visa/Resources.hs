module Visa.Resources (defaultSession
                      ,close
                      ,find
                      ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.Foldable (foldrM)

import Visa.Dll
import Visa.Status

-- TODO withSession + bracket

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
    allocaBytes 256 (\description -> 
        alloca (\find_list -> 
            alloca (\count -> do
                error <- viFindRsrc session c_query find_list count description
                total <- fmap toInteger (peek count)
                find_session <- peek find_list
                desc <- peekCString description
                check error (find_session, desc, total) "viFindRsrc"
                ))))

findNext :: ViFindList -> IO (String)
findNext find_session = allocaBytes 256 (\description -> do
    error <- viFindNext find_session description
    desc <- peekCString description
    check error desc "viFindNext")
            
-- Find the remainind devices using `findNext`
_find fs r = foldrM x [] [0..r]
    where x _ a = do
            desc <- findNext fs
            return $ (desc:a)

