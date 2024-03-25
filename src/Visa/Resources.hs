module Visa.Resources (defaultSession
                      ,close
                      ,findResource
                      ,findNext
                      ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- Local imports
import Visa.Dll
import Visa.Status

defaultSession :: IO (ViSession)
defaultSession = alloca (\session -> do
    error <- viOpenDefaultRM session
    value <- peek session
    check error value "viOpenDefaultRM")

close :: ViObject -> IO ()
close obj = do
    error <- viClose obj 
    check error () "viClose"

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

-- Find all attached devices based on the query, and return a list
-- find :: ViSession -> String -> IO ([String])
-- find session query = do
