module Main where

import qualified Visa.Session as RM

main :: IO ()
main = do
    session <- RM.defaultSession
    putStrLn ("Session: " ++ (show session))
    RM.close session
