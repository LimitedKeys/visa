module Visa.Session (defaultSession
                    ,close
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
