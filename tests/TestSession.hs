
module TestSession where

-- Test supporting functions for the Visa library.
--
-- Supporting functions are the functions between the User APIs and the Driver
-- wrappers. The goal here is to add *some* tests between the DLL calls and the
-- User APIs.

import Control.Exception 

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Visa.Resources
import Visa.Status
import Visa.Dll.Visa

-- Using the specified value, create a "session" and return the "status"
--
-- A non-zero status is an error to most APIs
mock_viOpenDefaultRM :: ViSession -> ViStatus -> (Ptr (ViSession) -> IO (ViStatus))
mock_viOpenDefaultRM value status = (\session -> do
    poke session value
    return status)

-- Simple test for _defaultSession
test_create_default_session_ok :: IO ()
test_create_default_session_ok = do
    let mock = mock_viOpenDefaultRM 123 0
    session <- _defaultSession mock "mock_viOpenDefaultRM"
    if session == 123
    then pure ()
    else throwIO (userError ("Expected session to be 123: session=" ++ (show session)))

-- Verify error is thrown in _defaultSession
test_create_default_session_error :: IO ()
test_create_default_session_error = do
    let mock = mock_viOpenDefaultRM 123 1
    result <- try (_defaultSession mock "expected") :: IO (Either ViError ViSession)
    case result of
        Left ex -> pure ()
        Right value -> throwIO (userError ("Expected an exception"))

mock_viClose :: ViStatus -> ViObject -> IO (ViStatus)
mock_viClose status _ = return status

-- Simple test for _close (status is OK)
test_close_ok :: IO ()
test_close_ok = do
    let mock = mock_viClose 0
    _close mock "viClose" 123

-- _close (Status is an Error)
test_close_error :: IO () 
test_close_error = do
    let mock = mock_viClose 1
    result <- try (_close mock "Expected" 123) :: IO (Either ViError ())
    case result of
        Left ex -> pure ()
        Right value -> throwIO (userError ("Expected an exception"))

testSession_run :: IO ()
testSession_run = do
    test_create_default_session_ok
    test_create_default_session_error
    test_close_ok
    test_close_error
