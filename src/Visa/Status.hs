{-# LANGUAGE BinaryLiterals #-} -- Enable Hex / Octal / Binary literals

module Visa.Status (check, checkDetails) where


import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Exception (throwIO)

import Visa.Dll.Visa

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

success = [vi_success_event_en
          ,vi_success_event_dis
          ,vi_success_queue_empty
          ,vi_success_term_char
          ,vi_success_max_cnt
          ,vi_success_dev_npresent
          ,vi_success_trig_mapped
          ,vi_success_queue_nempty
          ,vi_success_nchain
          ,vi_success_nested_shared
          ,vi_success_nested_exclusive
          ,vi_success_sync]

message name status description = (name ++ " Error: " ++ (show status) ++ description)

description :: ViObject -> ViStatus -> IO (String)
description session status = allocaBytes 256 (\buffer -> do
    error <- viStatusDesc session status buffer 
    if error == 0x0
    then peekCString buffer
    else return "Could not get description (viStatusDesc)")

checkDetails session status value name = if (status == 0x0)
    then return value 
    else do
        -- TODO Check if status is in `success` list
        desc <- description session status
        throwIO (userError (message name status desc))

check status value name = if (status == 0x0)
    then return value 
    else throwIO (userError (message name status ""))

_check status value name = if (status == vi_success)
    then return value 
    else if any (status ==) success 
        then return value
        else throwIO (userError (message name status ""))
