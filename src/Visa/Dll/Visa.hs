{-# LANGUAGE CPP, CApiFFI, ForeignFunctionInterface #-}

module Visa.Dll.Visa where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- Types
type ViStatus      = CInt

-- This changes based on if we're a 32 bit or 64 bit in 'visatype.h'
type ViUInt32      = CULong

type ViUInt16      = CUShort

type ViObject      = ViUInt32
type ViSession     = ViObject
type ViFindList    = ViObject

type ViAttr        = ViUInt32
type ViAccessMode  = ViUInt32
type ViChar        = CString
type ViConstString = CString
type ViConstRsrc   = CString

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

-- ViStatus _VI_FUNC  viParseRsrc     (ViSession rmSesn, ViConstRsrc rsrcName,
                                    -- ViPUInt16 intfType, ViPUInt16 intfNum);

foreign import capi "visa.h viParseRsrc"
    viParseRsrc :: ViSession -> ViConstRsrc -> Ptr (ViUInt16) -> Ptr (ViUInt16) -> IO (ViStatus)

-- ViStatus _VI_FUNC  viParseRsrcEx   (ViSession rmSesn, ViConstRsrc rsrcName, ViPUInt16 intfType,
                                    -- ViPUInt16 intfNum, ViChar _VI_FAR rsrcClass[],
                                    -- ViChar _VI_FAR expandedUnaliasedName[],
                                    -- ViChar _VI_FAR aliasIfExists[]);

foreign import capi "visa.h viParseRsrcEx"
    viParseRsrcEx :: ViSession -> ViConstRsrc -> Ptr (ViUInt16) -> Ptr (ViUInt16) -> ViChar -> ViChar -> ViChar -> IO (ViStatus)

-- ViStatus _VI_FUNC  viOpen          (ViSession sesn, ViConstRsrc name, ViAccessMode mode,
                                    -- ViUInt32 timeout, ViPSession vi);

foreign import capi "visa.h viOpen"
    viOpen :: ViSession -> ViConstRsrc -> ViAccessMode -> ViUInt32 -> Ptr (ViSession) -> IO (ViStatus)
