
import Control.Exception 

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Visa.Resources
import Visa.Status
import Visa.Dll.Visa

mock_viOpenDefaultRM :: ViSession -> ViStatus -> (Ptr (ViSession) -> IO (ViStatus))
mock_viOpenDefaultRM value status = (\session -> do
    poke session value
    return status)

test_create_default_session_ok = do
    let mock = mock_viOpenDefaultRM 123 0
    session <- _defaultSession mock "mock_viOpenDefaultRM"
    if session == 123
    then pure ()
    else throwIO (userError ("Expected session to be 123: session=" ++ (show session)))

test_create_default_session_error = do
    let mock = mock_viOpenDefaultRM 123 1
    result <- try (_defaultSession mock "expected") :: IO (Either ViError ViSession)
    case result of
        Left ex -> pure ()
        Right value -> throwIO (userError ("Expected an exception"))

main :: IO ()
main = do
    test_create_default_session_ok
    test_create_default_session_error
