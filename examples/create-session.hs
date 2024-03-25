module Main where

import Visa.Resources
import Visa.Attributes

main :: IO ()
main = do
    session <- defaultSession
    putStrLn ("Session: " ++ (show session))

    (major, minor, sub_minor) <- getAttributeVersion session vi_attr_rsrc_impl_version
    putStrLn ("Impl Version: " ++ (show major) ++ "." ++ (show minor) ++ "." ++ (show sub_minor))

    (major, minor, sub_minor) <- getAttributeVersion session vi_attr_rsrc_spec_version
    putStrLn ("Spec Version: " ++ (show major) ++ "." ++ (show minor) ++ "." ++ (show sub_minor))

    close session
