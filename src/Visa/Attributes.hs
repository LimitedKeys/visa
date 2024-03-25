{-# LANGUAGE BinaryLiterals #-} -- Enable Hex / Octal / Binary literals

module Visa.Attributes (getAttribute
                       ,getAttributeString
                       ,getAttributeVersion

                       -- Attributes
                       ,vi_attr_rsrc_class
                       ,vi_attr_rsrc_name
                       ,vi_attr_rsrc_impl_version
                       ,vi_attr_rsrc_lock_state
                       ,vi_attr_max_queue_length
                       ,vi_attr_user_data_32
                       ,vi_attr_fdc_chnl
                       ,vi_attr_fdc_mode
                       ,vi_attr_fdc_gen_signal_en
                       ,vi_attr_fdc_use_pair
                       ,vi_attr_send_end_en
                       ,vi_attr_termchar
                       ,vi_attr_tmo_value
                       ,vi_attr_gpib_readdr_en
                       ,vi_attr_io_prot
                       ,vi_attr_dma_allow_en
                       ,vi_attr_asrl_baud
                       ,vi_attr_asrl_data_bits
                       ,vi_attr_asrl_parity
                       ,vi_attr_asrl_stop_bits
                       ,vi_attr_asrl_flow_cntrl
                       ,vi_attr_rd_buf_oper_mode
                       ,vi_attr_rd_buf_size
                       ,vi_attr_wr_buf_oper_mode
                       ,vi_attr_wr_buf_size
                       ,vi_attr_suppress_end_en
                       ,vi_attr_termchar_en
                       ,vi_attr_dest_access_priv
                       ,vi_attr_dest_byte_order
                       ,vi_attr_src_access_priv
                       ,vi_attr_src_byte_order
                       ,vi_attr_src_increment
                       ,vi_attr_dest_increment
                       ,vi_attr_win_access_priv
                       ,vi_attr_win_byte_order
                       ,vi_attr_gpib_atn_state
                       ,vi_attr_gpib_addr_state
                       ,vi_attr_gpib_cic_state
                       ,vi_attr_gpib_ndac_state
                       ,vi_attr_gpib_srq_state
                       ,vi_attr_gpib_sys_cntrl_state
                       ,vi_attr_gpib_hs488_cbl_len
                       ,vi_attr_cmdr_la
                       ,vi_attr_vxi_dev_class
                       ,vi_attr_mainframe_la
                       ,vi_attr_manf_name
                       ,vi_attr_model_name
                       ,vi_attr_vxi_vme_intr_status
                       ,vi_attr_vxi_trig_status
                       ,vi_attr_vxi_vme_sysfail_state
                       ,vi_attr_win_base_addr_32
                       ,vi_attr_win_size_32
                       ,vi_attr_asrl_avail_num
                       ,vi_attr_mem_base_32
                       ,vi_attr_asrl_cts_state
                       ,vi_attr_asrl_dcd_state
                       ,vi_attr_asrl_dsr_state
                       ,vi_attr_asrl_dtr_state
                       ,vi_attr_asrl_end_in
                       ,vi_attr_asrl_end_out
                       ,vi_attr_asrl_replace_char
                       ,vi_attr_asrl_ri_state
                       ,vi_attr_asrl_rts_state
                       ,vi_attr_asrl_xon_char
                       ,vi_attr_asrl_xoff_char
                       ,vi_attr_win_access
                       ,vi_attr_rm_session
                       ,vi_attr_vxi_la
                       ,vi_attr_manf_id
                       ,vi_attr_mem_size_32
                       ,vi_attr_mem_space
                       ,vi_attr_model_code
                       ,vi_attr_slot
                       ,vi_attr_intf_inst_name
                       ,vi_attr_immediate_serv
                       ,vi_attr_intf_parent_num
                       ,vi_attr_rsrc_spec_version
                       ,vi_attr_intf_type
                       ,vi_attr_gpib_primary_addr
                       ,vi_attr_gpib_secondary_addr
                       ,vi_attr_rsrc_manf_name
                       ,vi_attr_rsrc_manf_id
                       ,vi_attr_intf_num
                       ,vi_attr_trig_id
                       ,vi_attr_gpib_ren_state
                       ,vi_attr_gpib_unaddr_en
                       ,vi_attr_dev_status_byte
                       ,vi_attr_file_append_en
                       ,vi_attr_vxi_trig_support
                       ,vi_attr_tcpip_addr
                       ,vi_attr_tcpip_hostname
                       ,vi_attr_tcpip_port
                       ,vi_attr_tcpip_device_name
                       ,vi_attr_tcpip_nodelay
                       ,vi_attr_tcpip_keepalive
                       ,vi_attr_4882_compliant
                       ,vi_attr_usb_serial_num
                       ,vi_attr_usb_intfc_num
                       ,vi_attr_usb_protocol
                       ,vi_attr_usb_max_intr_size
                       ,vi_attr_pxi_dev_num
                       ,vi_attr_pxi_func_num
                       ,vi_attr_pxi_bus_num
                       ,vi_attr_pxi_chassis
                       ,vi_attr_pxi_slotpath
                       ,vi_attr_pxi_slot_lbus_left
                       ,vi_attr_pxi_slot_lbus_right
                       ,vi_attr_pxi_trig_bus
                       ,vi_attr_pxi_star_trig_bus
                       ,vi_attr_pxi_star_trig_line
                       ,vi_attr_pxi_src_trig_bus
                       ,vi_attr_pxi_dest_trig_bus
                       ,vi_attr_pxi_mem_type_bar0
                       ,vi_attr_pxi_mem_type_bar1
                       ,vi_attr_pxi_mem_type_bar2
                       ,vi_attr_pxi_mem_type_bar3
                       ,vi_attr_pxi_mem_type_bar4
                       ,vi_attr_pxi_mem_type_bar5
                       ,vi_attr_pxi_mem_base_bar0_32
                       ,vi_attr_pxi_mem_base_bar1_32
                       ,vi_attr_pxi_mem_base_bar2_32
                       ,vi_attr_pxi_mem_base_bar3_32
                       ,vi_attr_pxi_mem_base_bar4_32
                       ,vi_attr_pxi_mem_base_bar5_32
                       ,vi_attr_pxi_mem_base_bar0_64
                       ,vi_attr_pxi_mem_base_bar1_64
                       ,vi_attr_pxi_mem_base_bar2_64
                       ,vi_attr_pxi_mem_base_bar3_64
                       ,vi_attr_pxi_mem_base_bar4_64
                       ,vi_attr_pxi_mem_base_bar5_64
                       ,vi_attr_pxi_mem_size_bar0_32
                       ,vi_attr_pxi_mem_size_bar1_32
                       ,vi_attr_pxi_mem_size_bar2_32
                       ,vi_attr_pxi_mem_size_bar3_32
                       ,vi_attr_pxi_mem_size_bar4_32
                       ,vi_attr_pxi_mem_size_bar5_32
                       ,vi_attr_pxi_mem_size_bar0_64
                       ,vi_attr_pxi_mem_size_bar1_64
                       ,vi_attr_pxi_mem_size_bar2_64
                       ,vi_attr_pxi_mem_size_bar3_64
                       ,vi_attr_pxi_mem_size_bar4_64
                       ,vi_attr_pxi_mem_size_bar5_64
                       ,vi_attr_pxi_is_express
                       ,vi_attr_pxi_slot_lwidth
                       ,vi_attr_pxi_max_lwidth
                       ,vi_attr_pxi_actual_lwidth
                       ,vi_attr_pxi_dstar_bus
                       ,vi_attr_pxi_dstar_set
                       ,vi_attr_pxi_allow_write_combine
                       ,vi_attr_tcpip_hislip_overlap_en
                       ,vi_attr_tcpip_hislip_version
                       ,vi_attr_tcpip_hislip_max_message_kb
                       ,vi_attr_tcpip_is_hislip
                       ,vi_attr_job_id
                       ,vi_attr_event_type
                       ,vi_attr_sigp_status_id
                       ,vi_attr_recv_trig_id
                       ,vi_attr_intr_status_id
                       ,vi_attr_status
                       ,vi_attr_ret_count_32
                       ,vi_attr_buffer
                       ,vi_attr_recv_intr_level
                       ,vi_attr_oper_name
                       ,vi_attr_gpib_recv_cic_state
                       ,vi_attr_recv_tcpip_addr
                       ,vi_attr_usb_recv_intr_size
                       ,vi_attr_usb_recv_intr_data
                       ,vi_attr_pxi_recv_intr_seq
                       ,vi_attr_pxi_recv_intr_data
                       ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.Bits

-- Local imports
import Visa.Dll
import Visa.Status

-- Get Attributes 
getAttribute:: ViSession -> Integer -> IO (Integer)
getAttribute session attr = alloca (\value -> do
    let attr_c = fromInteger attr :: ViAttr
    error <- viGetAttribute session attr_c value
    final <- peek (castPtr value) :: IO CUInt
    check error (toInteger final) "viGetAttribute")

getAttributeString :: ViSession -> Integer -> IO (String)
getAttributeString session attr = allocaBytes 256 (\buffer -> do
    let attr_c = fromInteger attr :: ViAttr
    error <- viGetAttribute session attr_c buffer
    result <- peekCString (castPtr buffer)
    check error result "viGetAttribute")

getAttributeVersion :: ViSession -> Integer -> IO (Integer, Integer, Integer)
getAttributeVersion session attr = do
    value <- fmap fromInteger (getAttribute session attr)
    let major = shiftR (value .&. 0xFFF00000) 20
    let minor = shiftR (value .&. 0x000FFF00) 8
    let sub_minor = (value .&. 0x00000FFF)
    return (major, minor, sub_minor)

-- Attributes

vi_attr_rsrc_class = 0xbfff0001
vi_attr_rsrc_name = 0xbfff0002
vi_attr_rsrc_impl_version = 0x3fff0003 -- 0xFFF Majro 0xFFF Minor 0xFF Sub Minor
vi_attr_rsrc_lock_state = 0x3fff0004
vi_attr_max_queue_length = 0x3fff0005
vi_attr_user_data_32 = 0x3fff0007
vi_attr_fdc_chnl = 0x3fff000d
vi_attr_fdc_mode = 0x3fff000f
vi_attr_fdc_gen_signal_en = 0x3fff0011
vi_attr_fdc_use_pair = 0x3fff0013
vi_attr_send_end_en = 0x3fff0016
vi_attr_termchar = 0x3fff0018
vi_attr_tmo_value = 0x3fff001a
vi_attr_gpib_readdr_en = 0x3fff001b
vi_attr_io_prot = 0x3fff001c
vi_attr_dma_allow_en = 0x3fff001e
vi_attr_asrl_baud = 0x3fff0021
vi_attr_asrl_data_bits = 0x3fff0022
vi_attr_asrl_parity = 0x3fff0023
vi_attr_asrl_stop_bits = 0x3fff0024
vi_attr_asrl_flow_cntrl = 0x3fff0025
vi_attr_rd_buf_oper_mode = 0x3fff002a
vi_attr_rd_buf_size = 0x3fff002b
vi_attr_wr_buf_oper_mode = 0x3fff002d
vi_attr_wr_buf_size = 0x3fff002e
vi_attr_suppress_end_en = 0x3fff0036
vi_attr_termchar_en = 0x3fff0038
vi_attr_dest_access_priv = 0x3fff0039
vi_attr_dest_byte_order = 0x3fff003a
vi_attr_src_access_priv = 0x3fff003c
vi_attr_src_byte_order = 0x3fff003d
vi_attr_src_increment = 0x3fff0040
vi_attr_dest_increment = 0x3fff0041
vi_attr_win_access_priv = 0x3fff0045
vi_attr_win_byte_order = 0x3fff0047
vi_attr_gpib_atn_state = 0x3fff0057
vi_attr_gpib_addr_state = 0x3fff005c
vi_attr_gpib_cic_state = 0x3fff005e
vi_attr_gpib_ndac_state = 0x3fff0062
vi_attr_gpib_srq_state = 0x3fff0067
vi_attr_gpib_sys_cntrl_state = 0x3fff0068
vi_attr_gpib_hs488_cbl_len = 0x3fff0069
vi_attr_cmdr_la = 0x3fff006b
vi_attr_vxi_dev_class = 0x3fff006c
vi_attr_mainframe_la = 0x3fff0070
vi_attr_manf_name = 0xbfff0072
vi_attr_model_name = 0xbfff0077
vi_attr_vxi_vme_intr_status = 0x3fff008b
vi_attr_vxi_trig_status = 0x3fff008d
vi_attr_vxi_vme_sysfail_state = 0x3fff0094
vi_attr_win_base_addr_32 = 0x3fff0098
vi_attr_win_size_32 = 0x3fff009a
vi_attr_asrl_avail_num = 0x3fff00ac
vi_attr_mem_base_32 = 0x3fff00ad
vi_attr_asrl_cts_state = 0x3fff00ae
vi_attr_asrl_dcd_state = 0x3fff00af
vi_attr_asrl_dsr_state = 0x3fff00b1
vi_attr_asrl_dtr_state = 0x3fff00b2
vi_attr_asrl_end_in = 0x3fff00b3
vi_attr_asrl_end_out = 0x3fff00b4
vi_attr_asrl_replace_char = 0x3fff00be
vi_attr_asrl_ri_state = 0x3fff00bf
vi_attr_asrl_rts_state = 0x3fff00c0
vi_attr_asrl_xon_char = 0x3fff00c1
vi_attr_asrl_xoff_char = 0x3fff00c2
vi_attr_win_access = 0x3fff00c3
vi_attr_rm_session = 0x3fff00c4
vi_attr_vxi_la = 0x3fff00d5
vi_attr_manf_id = 0x3fff00d9
vi_attr_mem_size_32 = 0x3fff00dd
vi_attr_mem_space = 0x3fff00de
vi_attr_model_code = 0x3fff00df
vi_attr_slot = 0x3fff00e8
vi_attr_intf_inst_name = 0xbfff00e9
vi_attr_immediate_serv = 0x3fff0100
vi_attr_intf_parent_num = 0x3fff0101
vi_attr_rsrc_spec_version = 0x3fff0170
vi_attr_intf_type = 0x3fff0171
vi_attr_gpib_primary_addr = 0x3fff0172
vi_attr_gpib_secondary_addr = 0x3fff0173
vi_attr_rsrc_manf_name = 0xbfff0174
vi_attr_rsrc_manf_id = 0x3fff0175
vi_attr_intf_num = 0x3fff0176
vi_attr_trig_id = 0x3fff0177
vi_attr_gpib_ren_state = 0x3fff0181
vi_attr_gpib_unaddr_en = 0x3fff0184
vi_attr_dev_status_byte = 0x3fff0189
vi_attr_file_append_en = 0x3fff0192
vi_attr_vxi_trig_support = 0x3fff0194
vi_attr_tcpip_addr = 0xbfff0195
vi_attr_tcpip_hostname = 0xbfff0196
vi_attr_tcpip_port = 0x3fff0197
vi_attr_tcpip_device_name = 0xbfff0199
vi_attr_tcpip_nodelay = 0x3fff019a
vi_attr_tcpip_keepalive = 0x3fff019b
vi_attr_4882_compliant = 0x3fff019f
vi_attr_usb_serial_num = 0xbfff01a0
vi_attr_usb_intfc_num = 0x3fff01a1
vi_attr_usb_protocol = 0x3fff01a7
vi_attr_usb_max_intr_size = 0x3fff01af
vi_attr_pxi_dev_num = 0x3fff0201
vi_attr_pxi_func_num = 0x3fff0202
vi_attr_pxi_bus_num = 0x3fff0205
vi_attr_pxi_chassis = 0x3fff0206
vi_attr_pxi_slotpath = 0xbfff0207
vi_attr_pxi_slot_lbus_left = 0x3fff0208
vi_attr_pxi_slot_lbus_right = 0x3fff0209
vi_attr_pxi_trig_bus = 0x3fff020a
vi_attr_pxi_star_trig_bus = 0x3fff020b
vi_attr_pxi_star_trig_line = 0x3fff020c
vi_attr_pxi_src_trig_bus = 0x3fff020d
vi_attr_pxi_dest_trig_bus = 0x3fff020e
vi_attr_pxi_mem_type_bar0 = 0x3fff0211
vi_attr_pxi_mem_type_bar1 = 0x3fff0212
vi_attr_pxi_mem_type_bar2 = 0x3fff0213
vi_attr_pxi_mem_type_bar3 = 0x3fff0214
vi_attr_pxi_mem_type_bar4 = 0x3fff0215
vi_attr_pxi_mem_type_bar5 = 0x3fff0216
vi_attr_pxi_mem_base_bar0_32 = 0x3fff0221
vi_attr_pxi_mem_base_bar1_32 = 0x3fff0222
vi_attr_pxi_mem_base_bar2_32 = 0x3fff0223
vi_attr_pxi_mem_base_bar3_32 = 0x3fff0224
vi_attr_pxi_mem_base_bar4_32 = 0x3fff0225
vi_attr_pxi_mem_base_bar5_32 = 0x3fff0226
vi_attr_pxi_mem_base_bar0_64 = 0x3fff0228
vi_attr_pxi_mem_base_bar1_64 = 0x3fff0229
vi_attr_pxi_mem_base_bar2_64 = 0x3fff022a
vi_attr_pxi_mem_base_bar3_64 = 0x3fff022b
vi_attr_pxi_mem_base_bar4_64 = 0x3fff022c
vi_attr_pxi_mem_base_bar5_64 = 0x3fff022d
vi_attr_pxi_mem_size_bar0_32 = 0x3fff0231
vi_attr_pxi_mem_size_bar1_32 = 0x3fff0232
vi_attr_pxi_mem_size_bar2_32 = 0x3fff0233
vi_attr_pxi_mem_size_bar3_32 = 0x3fff0234
vi_attr_pxi_mem_size_bar4_32 = 0x3fff0235
vi_attr_pxi_mem_size_bar5_32 = 0x3fff0236
vi_attr_pxi_mem_size_bar0_64 = 0x3fff0238
vi_attr_pxi_mem_size_bar1_64 = 0x3fff0239
vi_attr_pxi_mem_size_bar2_64 = 0x3fff023a
vi_attr_pxi_mem_size_bar3_64 = 0x3fff023b
vi_attr_pxi_mem_size_bar4_64 = 0x3fff023c
vi_attr_pxi_mem_size_bar5_64 = 0x3fff023d
vi_attr_pxi_is_express = 0x3fff0240
vi_attr_pxi_slot_lwidth = 0x3fff0241
vi_attr_pxi_max_lwidth = 0x3fff0242
vi_attr_pxi_actual_lwidth = 0x3fff0243
vi_attr_pxi_dstar_bus = 0x3fff0244
vi_attr_pxi_dstar_set = 0x3fff0245
vi_attr_pxi_allow_write_combine = 0x3fff0246
vi_attr_tcpip_hislip_overlap_en = 0x3fff0300
vi_attr_tcpip_hislip_version = 0x3fff0301
vi_attr_tcpip_hislip_max_message_kb = 0x3fff0302
vi_attr_tcpip_is_hislip = 0x3fff0303

vi_attr_job_id = 0x3fff4006
vi_attr_event_type = 0x3fff4010
vi_attr_sigp_status_id = 0x3fff4011
vi_attr_recv_trig_id = 0x3fff4012
vi_attr_intr_status_id = 0x3fff4023
vi_attr_status = 0x3fff4025
vi_attr_ret_count_32 = 0x3fff4026
vi_attr_buffer = 0x3fff4027
vi_attr_recv_intr_level = 0x3fff4041
vi_attr_oper_name = 0xbfff4042
vi_attr_gpib_recv_cic_state = 0x3fff4193
vi_attr_recv_tcpip_addr = 0xbfff4198
vi_attr_usb_recv_intr_size = 0x3fff41b0
vi_attr_usb_recv_intr_data = 0xbfff41b1
vi_attr_pxi_recv_intr_seq = 0x3fff4240
vi_attr_pxi_recv_intr_data = 0x3fff4241

