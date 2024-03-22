{-# LANGUAGE CPP, CApiFFI, ForeignFunctionInterface #-}

module Visa.Dll (ViStatus
                ,ViObject
                ,ViSession
                ,ViAttr

                -- Functions
                ,dll_viOpenDefaultRM
                ,dll_viClose
                ,dll_viGetAttribute
                ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- Types
type ViStatus = CInt

type ViUInt32 = CULong

type ViObject  = ViUInt32
type ViSession = ViObject
type ViAttr    = ViUInt32

-- Functions
foreign import capi "visa.h viOpenDefaultRM"
    dll_viOpenDefaultRM :: Ptr (ViSession) -> IO (ViStatus)

foreign import capi "visa.h viClose"
    dll_viClose :: ViSession -> IO (ViStatus)

foreign import capi "visa.h viGetAttribute"
    dll_viGetAttribute :: ViSession -> ViAttr -> Ptr () -> IO (ViStatus)
