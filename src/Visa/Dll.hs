{-# LANGUAGE CPP, CApiFFI, ForeignFunctionInterface #-}

module Visa.Dll (ViStatus
                ,ViObject
                ,ViSession
                ,ViAttr
                ,ViFindList

                -- Functions
                ,viOpenDefaultRM
                ,viClose
                ,viGetAttribute
                ,viFindRsrc
                ,viFindNext
                ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- Types
type ViStatus      = CInt

type ViUInt32      = CULong

type ViObject      = ViUInt32
type ViSession     = ViObject
type ViFindList    = ViObject

type ViAttr        = ViUInt32
type ViChar        = CString
type ViConstString = CString

-- Resource Management

foreign import capi "visa.h viOpenDefaultRM"
    viOpenDefaultRM :: Ptr (ViSession) -> IO (ViStatus)

foreign import capi "visa.h viClose"
    viClose :: ViSession -> IO (ViStatus)

foreign import capi "visa.h viGetAttribute"
    viGetAttribute :: ViSession -> ViAttr -> Ptr () -> IO (ViStatus)

-- Finding Resources

foreign import capi "visa.h viFindRsrc"
    viFindRsrc :: ViSession -> ViConstString -> Ptr (ViFindList) -> Ptr (ViUInt32) -> ViChar -> IO (ViStatus)

foreign import capi "visa.h viFindNext"
    viFindNext :: ViFindList -> ViChar -> IO (ViStatus)
