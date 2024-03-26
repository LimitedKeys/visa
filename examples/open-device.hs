module Main where

import Visa.Resources

import Control.Monad (mapM_)

-- Bracket ...

main :: IO ()
main = do
    session <- defaultSession
    devices <- find session "?*::INSTR"

    if (length devices) == 0
    then do 
        putStrLn "No Devices Found"
        close session
    else do
        putStrLn "Devices"
        mapM_ (\a -> putStrLn (" " ++ a)) devices

        -- Open the First Device
        first_device <- head devices
        putStrLn ("Openning the first device: " ++ first_device)

        device <- open sesion first_device NO_LOCK 2000

        -- Query the device ID
        putStrLn "> *IDN?"
        response <- query device "*IDN?"
        putStrLn response

        close device
        close session
