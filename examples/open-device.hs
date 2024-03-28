module Main where

import Visa.Resources

import Control.Monad (mapM_)

-- Bracket ...
-- Allow device to be specified from CLI String (to work with list exe)

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

        -- Open the Device
        -- first_device <- head devices
        let first_device = "GPIB0::10::INSTR"
        putStrLn ("Openning " ++ first_device)

        device <- open session first_device 2000

        -- Query the device ID
        putStrLn "> *IDN?"
        response <- query device "*IDN?"
        putStrLn response

        close device
        close session
