module Main where

import Visa.Resources

import Control.Monad

find_and_print fs _ = do
    desc <- findNext fs
    putStrLn ("  " ++ desc)

find_more find_session remaining = forM_ [0..remaining] (find_and_print find_session)

main :: IO ()
main = do
    session <- defaultSession
    (find_session, description, connected) <- findResource session "?*::INSTR"

    if connected < 1
    then putStrLn "No Devices Connected"
    else do
        putStrLn "Devices"
        putStrLn ("  " ++ description)
        if connected > 1
        then find_more find_session (connected - 1)
        else pure ()

    close find_session
    close session
