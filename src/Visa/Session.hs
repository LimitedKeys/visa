module Visa.Session (defaultSession
                    ,close
                    ,getAttribute
                    ) where

import Foreign
import Foreign.C.Types

-- Local imports
import Visa.Dll
import Visa.Status

defaultSession :: IO (ViSession)
defaultSession = alloca (\session -> do
    error <- dll_viOpenDefaultRM session
    value <- peek session
    check error value "viOpenDefaultRM")

close :: ViSession -> IO ()
close session = do
    error <- dll_viClose session
    check error () "viClose"

-- Get Attributes 
--
-- This is tricky - attributes can be strings. 
-- There are only a few string attributes. The String will be 256 bytes long.
--
-- This (may) require a case statement, where the Special Case String ones can
-- be handled.
--
-- Move this to Attributes?
getAttribute :: ViSession -> Integer -> IO (Integer)
getAttribute session attr = alloca (\value -> do
    let attr_c = fromInteger attr :: ViAttr
    error <- dll_viGetAttribute session attr_c value
    final <- peek (castPtr value) :: IO CUInt
    putStrLn ("Value: " ++ (show final))
    check error (toInteger final) "viGetAttribute")
