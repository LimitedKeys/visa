{-# LANGUAGE CPP, CApiFFI, ForeignFunctionInterface #-}

module Visa.Dll.Visa where

import Foreign
import Foreign.C.Types
import Foreign.C.String

type ViStatus      = CInt

-- This changes based on if we're a 32 bit or 64 bit in 'visatype.h'
type ViUInt32      = CULong

type ViUInt16      = CUShort
type ViByte        = CUChar

type ViObject      = ViUInt32
type ViSession     = ViObject
type ViFindList    = ViObject

type ViAttr        = ViUInt32
type ViAccessMode  = ViUInt32
type ViChar        = CString
type ViConstString = CString
type ViConstRsrc   = CString

#ifndef TEST

foreign import capi "visa.h viOpenDefaultRM"
    viOpenDefaultRM :: Ptr (ViSession) -> IO (ViStatus)
foreign import capi "visa.h viClose"
    viClose :: ViSession -> IO (ViStatus)
foreign import capi "visa.h viGetAttribute"
    viGetAttribute :: ViSession -> ViAttr -> Ptr () -> IO (ViStatus)
foreign import capi "visa.h viFindRsrc"
    viFindRsrc :: ViSession -> ViConstString -> Ptr (ViFindList) -> Ptr (ViUInt32) -> ViChar -> IO (ViStatus)
foreign import capi "visa.h viFindNext"
    viFindNext :: ViFindList -> ViChar -> IO (ViStatus)
foreign import capi "visa.h viParseRsrc"
    viParseRsrc :: ViSession -> ViConstRsrc -> Ptr (ViUInt16) -> Ptr (ViUInt16) -> IO (ViStatus)
foreign import capi "visa.h viParseRsrcEx"
    viParseRsrcEx :: ViSession -> ViConstRsrc -> Ptr (ViUInt16) -> Ptr (ViUInt16) -> ViChar -> ViChar -> ViChar -> IO (ViStatus)
foreign import capi "visa.h viOpen"
    viOpen :: ViSession -> ViConstRsrc -> ViAccessMode -> ViUInt32 -> Ptr (ViSession) -> IO (ViStatus)
foreign import capi "visa.h viRead"
    viRead :: ViSession -> Ptr (ViByte) -> ViUInt32 -> Ptr (ViUInt32) -> IO (ViStatus)
foreign import capi "visa.h viWrite"
    viWrite :: ViSession -> Ptr (ViByte) -> ViUInt32 -> Ptr (ViUInt32) -> IO (ViStatus)
foreign import capi "visa.h viStatusDesc"
    viStatusDesc :: ViObject -> ViStatus -> ViChar -> IO (ViStatus)

#else

test_session :: ViSession
test_session = 1234

test_status :: ViStatus
test_status = 0

viOpenDefaultRM :: Ptr (ViSession) -> IO (ViStatus)
viOpenDefaultRM p_session = do
    poke p_session test_session
    return test_status

viClose :: ViSession -> IO (ViStatus)
viClose session = return test_status

#endif
