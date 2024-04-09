{-# LANGUAGE BinaryLiterals #-} -- Enable Hex / Octal / Binary literals

module Visa.Status (check, checkDetails, ViError(..)) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.Maybe
import Control.Exception (throwIO, Exception)

import Visa.Dll.Visa

-- Error Constants from visa.h
vi_success = 0
vi_success_event_en = 0x3fff0002
vi_success_event_dis = 0x3fff0003
vi_success_queue_empty = 0x3fff0004
vi_success_term_char = 0x3fff0005
vi_success_max_cnt = 0x3fff0006
vi_success_dev_npresent = 0x3fff007d
vi_success_trig_mapped = 0x3fff007e
vi_success_queue_nempty = 0x3fff0080
vi_success_nchain = 0x3fff0098
vi_success_nested_shared = 0x3fff0099
vi_success_nested_exclusive = 0x3fff009a
vi_success_sync = 0x3fff009b

vi_warn_queue_overflow = 0x3fff000c
vi_warn_config_nloaded = 0x3fff0077
vi_warn_null_object = 0x3fff0082
vi_warn_nsup_attr_state = 0x3fff0084
vi_warn_unknown_status = 0x3fff0085
vi_warn_nsup_buf = 0x3fff0088
vi_warn_ext_func_nimpl = 0x3fff00a9

vi_error = 0x80000000
vi_error_system_error = (vi_error+0x3fff0000)
vi_error_inv_object = (vi_error+0x3fff000e)
vi_error_rsrc_locked = (vi_error+0x3fff000f)
vi_error_inv_expr = (vi_error+0x3fff0010)
vi_error_rsrc_nfound = (vi_error+0x3fff0011)
vi_error_inv_rsrc_name = (vi_error+0x3fff0012)
vi_error_inv_acc_mode = (vi_error+0x3fff0013)
vi_error_tmo = (vi_error+0x3fff0015)
vi_error_closing_failed = (vi_error+0x3fff0016)
vi_error_inv_degree = (vi_error+0x3fff001b)
vi_error_inv_job_id = (vi_error+0x3fff001c)
vi_error_nsup_attr = (vi_error+0x3fff001d)
vi_error_nsup_attr_state = (vi_error+0x3fff001e)
vi_error_attr_readonly = (vi_error+0x3fff001f)
vi_error_inv_lock_type = (vi_error+0x3fff0020)
vi_error_inv_access_key = (vi_error+0x3fff0021)
vi_error_inv_event = (vi_error+0x3fff0026)
vi_error_inv_mech = (vi_error+0x3fff0027)
vi_error_hndlr_ninstalled = (vi_error+0x3fff0028)
vi_error_inv_hndlr_ref = (vi_error+0x3fff0029)
vi_error_inv_context = (vi_error+0x3fff002a)
vi_error_queue_overflow = (vi_error+0x3fff002d)
vi_error_nenabled = (vi_error+0x3fff002f)
vi_error_abort = (vi_error+0x3fff0030)
vi_error_raw_wr_prot_viol = (vi_error+0x3fff0034)
vi_error_raw_rd_prot_viol = (vi_error+0x3fff0035)
vi_error_outp_prot_viol = (vi_error+0x3fff0036)
vi_error_inp_prot_viol = (vi_error+0x3fff0037)
vi_error_berr = (vi_error+0x3fff0038)
vi_error_in_progress = (vi_error+0x3fff0039)
vi_error_inv_setup = (vi_error+0x3fff003a)
vi_error_queue_error = (vi_error+0x3fff003b)
vi_error_alloc = (vi_error+0x3fff003c)
vi_error_inv_mask = (vi_error+0x3fff003d)
vi_error_io = (vi_error+0x3fff003e)
vi_error_inv_fmt = (vi_error+0x3fff003f)
vi_error_nsup_fmt = (vi_error+0x3fff0041)
vi_error_line_in_use = (vi_error+0x3fff0042)
vi_error_line_nreserved = (vi_error+0x3fff0043)
vi_error_nsup_mode = (vi_error+0x3fff0046)
vi_error_srq_noccurred = (vi_error+0x3fff004a)
vi_error_inv_space = (vi_error+0x3fff004e)
vi_error_inv_offset = (vi_error+0x3fff0051)
vi_error_inv_width = (vi_error+0x3fff0052)
vi_error_nsup_offset = (vi_error+0x3fff0054)
vi_error_nsup_var_width = (vi_error+0x3fff0055)
vi_error_window_nmapped = (vi_error+0x3fff0057)
vi_error_resp_pending = (vi_error+0x3fff0059)
vi_error_nlisteners = (vi_error+0x3fff005f)
vi_error_ncic = (vi_error+0x3fff0060)
vi_error_nsys_cntlr = (vi_error+0x3fff0061)
vi_error_nsup_oper = (vi_error+0x3fff0067)
vi_error_intr_pending = (vi_error+0x3fff0068)
vi_error_asrl_parity = (vi_error+0x3fff006a)
vi_error_asrl_framing = (vi_error+0x3fff006b)
vi_error_asrl_overrun = (vi_error+0x3fff006c)
vi_error_trig_nmapped = (vi_error+0x3fff006e)
vi_error_nsup_align_offset = (vi_error+0x3fff0070)
vi_error_user_buf = (vi_error+0x3fff0071)
vi_error_rsrc_busy = (vi_error+0x3fff0072)
vi_error_nsup_width = (vi_error+0x3fff0076)
vi_error_inv_parameter = (vi_error+0x3fff0078)
vi_error_inv_prot = (vi_error+0x3fff0079)
vi_error_inv_size = (vi_error+0x3fff007b)
vi_error_window_mapped = (vi_error+0x3fff0080)
vi_error_nimpl_oper = (vi_error+0x3fff0081)
vi_error_inv_length = (vi_error+0x3fff0083)
vi_error_inv_mode = (vi_error+0x3fff0091)
vi_error_sesn_nlocked = (vi_error+0x3fff009c)
vi_error_mem_nshared = (vi_error+0x3fff009d)
vi_error_library_nfound = (vi_error+0x3fff009e)
vi_error_nsup_intr = (vi_error+0x3fff009f)
vi_error_inv_line = (vi_error+0x3fff00a0)
vi_error_file_access = (vi_error+0x3fff00a1)
vi_error_file_io = (vi_error+0x3fff00a2)
vi_error_nsup_line = (vi_error+0x3fff00a3)
vi_error_nsup_mech = (vi_error+0x3fff00a4)
vi_error_intf_num_nconfig = (vi_error+0x3fff00a5)
vi_error_conn_lost = (vi_error+0x3fff00a6)
vi_error_machine_navail = (vi_error+0x3fff00a7)
vi_error_npermission = (vi_error+0x3fff00a8)

data ViError = ViError String
             | ViErrorSystemError String
             | ViErrorInvalidObject String
             | ViErrorResourceLocked String
             | ViErrorInvalidExpression String
             | ViErrorResourceNotFound String
             | ViErrorInvalidResourceName String
             | ViErrorInvalidAccessMode String
             | ViErrorTMO String
             | ViErrorClosingFailed String
             | ViErrorInvalidDegree String
             | ViErrorJobID String
             | ViErrorNotSupportedAttribute String
             | ViErrorNotSupportedAttributeState String
             | ViErrorAttributeReadOnly String
             | ViErrorInvalidLockType String
             | ViErrorInvalidAccessKey String
             | ViErrorInvalidEvent String
             | ViErrorInvalidMech String
             | ViErrorHandlerNotInstalled String
             | ViErrorInvalidHandlerReference String
             | ViErrorInvalidContext String
             | ViErrorQueueOverflow String
             | ViErrorNotEnabled String
             | ViErrorAbort String
             | ViErrorRawWriteProtectionViolation String
             | ViErrorRawReadProtectionViolation String
             | ViErrorOutPointerProtectionViolation String
             | ViErrorInPointerProtectionViolation String
             | ViErrorBErr String
             | ViErrorInProgress String
             | ViErrorInvalidSetup String
             | ViErrorQueueError String
             | ViErrorAlloc String
             | ViErrorInvalidMask String
             | ViErrorIO String
             | ViErrorInvalidFormat String
             | ViErrorNotSupportedFormat String
             | ViErrorLineInUse String
             | ViErrorLineNotReserved String
             | ViErrorNotSupportedMode String
             | ViErrorSRQNotOccurred String
             | ViErrorInvalidSpace String
             | ViErrorInvalidOffset String
             | ViErrorInvalidWidth String
             | ViErrorNotSupportedOffset String
             | ViErrorNotSupportedVarWidth String
             | ViErrorWindowNotMapped String
             | ViErrorResponsePending String
             | ViErrorNListeners String
             | ViErrorNCIC String
             | ViErrorNSYSController String
             | ViErrorNotSupportedOperation String
             | ViErrorIntrPending String
             | ViErrorASRLParity String
             | ViErrorASRLFraming String
             | ViErrorASRLOverrun String
             | ViErrorTriggerNotMapped String
             | ViErrorNotSupportedAlignOffset String
             | ViErrorUserBuffer String
             | ViErrorResourceBusy String
             | ViErrorNotSupportedWidth String
             | ViErrorInvalidParameter String
             | ViErrorInvalidProtection String
             | ViErrorInvalidSize String
             | ViErrorWindowMapped String
             | ViErrorNotImplementedOperation String
             | ViErrorInvalidLength String
             | ViErrorInvalidMode String
             | ViErrorSessionNotLocked String
             | ViErrorMemoryNotShared String
             | ViErrorLibraryNotFound String
             | ViErrorNotSupportedIntr String
             | ViErrorInvalidLine String
             | ViErrorFileAccess String
             | ViErrorFileIO String
             | ViErrorNotSupportedLine String
             | ViErrorNotSupportedMech String
             | ViErrorIntfNumNConfig String
             | ViErrorConnectionLost String
             | ViErrorMachineNotAvailable String
             | ViErrorNoPermission String
             deriving (Show)

instance Exception ViError 

statusToError :: ViStatus -> String -> Maybe ViError
statusToError status message 
    | status == vi_success = Nothing
    | status == vi_success_event_en = Nothing
    | status == vi_success_event_dis = Nothing
    | status == vi_success_queue_empty = Nothing
    | status == vi_success_term_char = Nothing
    | status == vi_success_max_cnt = Nothing
    | status == vi_success_dev_npresent = Nothing
    | status == vi_success_trig_mapped = Nothing
    | status == vi_success_queue_nempty = Nothing
    | status == vi_success_nchain = Nothing
    | status == vi_success_nested_shared = Nothing
    | status == vi_success_nested_exclusive = Nothing
    | status == vi_success_sync = Nothing
    | status == vi_warn_queue_overflow = Nothing
    | status == vi_warn_config_nloaded = Nothing
    | status == vi_warn_null_object = Nothing
    | status == vi_warn_nsup_attr_state = Nothing
    | status == vi_warn_unknown_status = Nothing
    | status == vi_warn_nsup_buf = Nothing
    | status == vi_warn_ext_func_nimpl = Nothing
    | status == vi_error_system_error = Just (ViErrorSystemError message)
    | status == vi_error_inv_object = Just (ViErrorInvalidObject message)
    | status == vi_error_rsrc_locked = Just (ViErrorResourceLocked message)
    | status == vi_error_inv_expr = Just (ViErrorInvalidExpression message)
    | status == vi_error_rsrc_nfound = Just (ViErrorResourceNotFound message)
    | status == vi_error_inv_rsrc_name = Just (ViErrorInvalidResourceName message)
    | status == vi_error_inv_acc_mode = Just (ViErrorInvalidAccessMode message)
    | status == vi_error_tmo = Just (ViErrorTMO message)
    | status == vi_error_closing_failed = Just (ViErrorClosingFailed message)
    | status == vi_error_inv_degree = Just (ViErrorInvalidDegree message)
    | status == vi_error_inv_job_id = Just (ViErrorJobID message)
    | status == vi_error_nsup_attr = Just (ViErrorNotSupportedAttribute message)
    | status == vi_error_nsup_attr_state = Just (ViErrorNotSupportedAttributeState message)
    | status == vi_error_attr_readonly = Just (ViErrorAttributeReadOnly message)
    | status == vi_error_inv_lock_type = Just (ViErrorInvalidLockType message)
    | status == vi_error_inv_access_key = Just (ViErrorInvalidAccessKey message)
    | status == vi_error_inv_event = Just (ViErrorInvalidEvent message)
    | status == vi_error_inv_mech = Just (ViErrorInvalidMech message)
    | status == vi_error_hndlr_ninstalled = Just (ViErrorHandlerNotInstalled message)
    | status == vi_error_inv_hndlr_ref = Just (ViErrorInvalidHandlerReference message)
    | status == vi_error_inv_context = Just (ViErrorInvalidContext message)
    | status == vi_error_queue_overflow = Just (ViErrorQueueOverflow message)
    | status == vi_error_nenabled = Just (ViErrorNotEnabled message)
    | status == vi_error_abort = Just (ViErrorAbort message)
    | status == vi_error_raw_wr_prot_viol = Just (ViErrorRawWriteProtectionViolation message)
    | status == vi_error_raw_rd_prot_viol = Just (ViErrorRawReadProtectionViolation message)
    | status == vi_error_outp_prot_viol = Just (ViErrorOutPointerProtectionViolation message)
    | status == vi_error_inp_prot_viol = Just (ViErrorInPointerProtectionViolation message)
    | status == vi_error_berr = Just (ViErrorBErr message)
    | status == vi_error_in_progress = Just (ViErrorInProgress message)
    | status == vi_error_inv_setup = Just (ViErrorInvalidSetup message)
    | status == vi_error_queue_error = Just (ViErrorQueueError message)
    | status == vi_error_alloc = Just (ViErrorAlloc message)
    | status == vi_error_inv_mask = Just (ViErrorInvalidMask message)
    | status == vi_error_io = Just (ViErrorIO message)
    | status == vi_error_inv_fmt = Just (ViErrorInvalidFormat message)
    | status == vi_error_nsup_fmt = Just (ViErrorNotSupportedFormat message)
    | status == vi_error_line_in_use = Just (ViErrorLineInUse message)
    | status == vi_error_line_nreserved = Just (ViErrorLineNotReserved message)
    | status == vi_error_nsup_mode = Just (ViErrorNotSupportedMode message)
    | status == vi_error_srq_noccurred = Just (ViErrorSRQNotOccurred message)
    | status == vi_error_inv_space = Just (ViErrorInvalidSpace message)
    | status == vi_error_inv_offset = Just (ViErrorInvalidOffset message)
    | status == vi_error_inv_width = Just (ViErrorInvalidWidth message)
    | status == vi_error_nsup_offset = Just (ViErrorNotSupportedOffset message)
    | status == vi_error_nsup_var_width = Just (ViErrorNotSupportedVarWidth message)
    | status == vi_error_window_nmapped = Just (ViErrorWindowNotMapped message)
    | status == vi_error_resp_pending = Just (ViErrorResponsePending message)
    | status == vi_error_nlisteners = Just (ViErrorNListeners message)
    | status == vi_error_ncic = Just (ViErrorNCIC message)
    | status == vi_error_nsys_cntlr = Just (ViErrorNSYSController message)
    | status == vi_error_nsup_oper = Just (ViErrorNotSupportedOperation message)
    | status == vi_error_intr_pending = Just (ViErrorIntrPending message)
    | status == vi_error_asrl_parity = Just (ViErrorASRLParity message)
    | status == vi_error_asrl_framing = Just (ViErrorASRLFraming message)
    | status == vi_error_asrl_overrun = Just (ViErrorASRLOverrun message)
    | status == vi_error_trig_nmapped = Just (ViErrorTriggerNotMapped message)
    | status == vi_error_nsup_align_offset = Just (ViErrorNotSupportedAlignOffset message)
    | status == vi_error_user_buf = Just (ViErrorUserBuffer message)
    | status == vi_error_rsrc_busy = Just (ViErrorResourceBusy message)
    | status == vi_error_nsup_width = Just (ViErrorNotSupportedWidth message)
    | status == vi_error_inv_parameter = Just (ViErrorInvalidParameter message)
    | status == vi_error_inv_prot = Just (ViErrorInvalidProtection message)
    | status == vi_error_inv_size = Just (ViErrorInvalidSize message)
    | status == vi_error_window_mapped = Just (ViErrorWindowMapped message)
    | status == vi_error_nimpl_oper = Just (ViErrorNotImplementedOperation message)
    | status == vi_error_inv_length = Just (ViErrorInvalidLength message)
    | status == vi_error_inv_mode = Just (ViErrorInvalidMode message)
    | status == vi_error_sesn_nlocked = Just (ViErrorSessionNotLocked message)
    | status == vi_error_mem_nshared = Just (ViErrorMemoryNotShared message)
    | status == vi_error_library_nfound = Just (ViErrorLibraryNotFound message)
    | status == vi_error_nsup_intr = Just (ViErrorNotSupportedIntr message)
    | status == vi_error_inv_line = Just (ViErrorInvalidLine message)
    | status == vi_error_file_access = Just (ViErrorFileAccess message)
    | status == vi_error_file_io = Just (ViErrorFileIO message)
    | status == vi_error_nsup_line = Just (ViErrorNotSupportedLine message)
    | status == vi_error_nsup_mech = Just (ViErrorNotSupportedMech message)
    | status == vi_error_intf_num_nconfig = Just (ViErrorIntfNumNConfig message)
    | status == vi_error_conn_lost = Just (ViErrorConnectionLost message)
    | status == vi_error_machine_navail = Just (ViErrorMachineNotAvailable message)
    | status == vi_error_npermission = Just (ViErrorNoPermission message)

statusToError status message = Just (ViError message)

message name status description = (name ++ " Error: " ++ (show status) ++ description)

description :: ViObject -> ViStatus -> IO (String)
description session status = allocaBytes 256 (\buffer -> do
    error <- viStatusDesc session status buffer 
    if error == 0x0
    then peekCString buffer
    else return "Could not get description (viStatusDesc)")

checkDetails session status value name = case statusToError status (message name status "") of
    Just e -> do
        details <- description session status
        let msg = message name status details
        throwIO (fromJust (statusToError status msg))
    _ -> return value

check status value name = case (statusToError status (message name status "")) of
    Just e -> throwIO e 
    _ -> return value
