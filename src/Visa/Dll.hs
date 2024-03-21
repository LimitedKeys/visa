{-# LANGUAGE CPP, CApiFFI, ForeignFunctionInterface #-}

module Visa.Dll (ViStatus
                ,ViObject
                ,ViSession
                ,dll_viOpenDefaultRM
                ,dll_viClose
                ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- Types
type ViStatus = CInt

type ViObject = CULong
type ViSession = ViObject

-- Functions

foreign import capi "visa.h viOpenDefaultRM"
    dll_viOpenDefaultRM :: Ptr (ViSession) -> IO (ViStatus)

foreign import capi "visa.h viClose"
    dll_viClose :: ViSession -> IO (ViStatus)
