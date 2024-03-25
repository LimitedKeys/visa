module Main where

import Visa.Resources

import Control.Monad (mapM_)

main :: IO ()
main = do
    session <- defaultSession

    devices <- find session "?*::INSTR"
    
    if (length devices) > 0
    then do
        putStrLn "Devices"
        mapM_ (\a -> putStrLn (" " ++ a)) devices
    else pure ()

    close session
