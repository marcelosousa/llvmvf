; ModuleID = 'demo-opentlmo.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%"class.std::ios_base::Init" = type { i8 }
%"class.sc_core::sc_api_version_2_2_0" = type { i8 }
%"class.sc_core::sc_process_b" = type { %"class.sc_core::sc_object", i8*, i32, i32, %"class.std::vector.10", i8, i8, %"class.sc_core::sc_event"*, i32, %"class.sc_core::sc_event_list"*, %"class.sc_core::sc_process_b"*, i8, %"class.sc_core::sc_report"*, %"class.sc_core::sc_name_gen"*, i32, i32, i8, %"class.sc_core::sc_reset"*, %"class.sc_core::sc_process_b"*, %"class.sc_core::sc_process_host"*, { i64, i64 }, %"class.std::vector.43", %"class.sc_core::sc_event"*, i32, i8, %"class.sc_core::sc_event"*, i32, i8 }
%"class.sc_core::sc_object" = type { i32 (...)**, %"class.sc_core::sc_simcontext"*, i8*, %"class.sc_core::sc_attr_cltn"*, %"class.sc_core::sc_object"* }
%"class.sc_core::sc_simcontext" = type { %"class.sc_core::sc_object_manager"*, %"class.sc_core::sc_module_registry"*, %"class.sc_core::sc_port_registry"*, %"class.sc_core::sc_export_registry"*, %"class.sc_core::sc_prim_channel_registry"*, %"class.sc_core::sc_name_gen"*, %"class.sc_core::sc_process_table"*, %"struct.sc_core::sc_curr_proc_info", %"class.sc_core::sc_object"*, i8, i32, %"class.std::vector.10", %"class.std::vector.15", %"class.sc_core::sc_ppq"*, %"class.std::vector.30", i8, %"class.sc_core::sc_runnable"*, %"struct.sc_core::sc_time_params"*, %"class.sc_core::sc_time", i64, i8, i8, i8, i32, i8, i8, i8, i8, %"class.sc_core::sc_event"*, %"class.sc_core::sc_cor_pkg"*, %"class.sc_core::sc_cor"* }
%"class.sc_core::sc_object_manager" = type opaque
%"class.sc_core::sc_module_registry" = type opaque
%"class.sc_core::sc_port_registry" = type { %"class.sc_core::sc_simcontext"*, %"class.std::vector" }
%"class.std::vector" = type { %"struct.std::_Vector_base" }
%"struct.std::_Vector_base" = type { %"struct.std::_Vector_base<sc_core::sc_port_base *, std::allocator<sc_core::sc_port_base *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_port_base *, std::allocator<sc_core::sc_port_base *> >::_Vector_impl" = type { %"class.sc_core::sc_port_base"**, %"class.sc_core::sc_port_base"**, %"class.sc_core::sc_port_base"** }
%"class.sc_core::sc_port_base" = type { %"class.sc_core::sc_object", %"struct.sc_core::sc_bind_info"* }
%"struct.sc_core::sc_bind_info" = type opaque
%"class.sc_core::sc_export_registry" = type { %"class.sc_core::sc_simcontext"*, %"class.std::vector.0" }
%"class.std::vector.0" = type { %"struct.std::_Vector_base.1" }
%"struct.std::_Vector_base.1" = type { %"struct.std::_Vector_base<sc_core::sc_export_base *, std::allocator<sc_core::sc_export_base *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_export_base *, std::allocator<sc_core::sc_export_base *> >::_Vector_impl" = type { %"class.sc_core::sc_export_base"**, %"class.sc_core::sc_export_base"**, %"class.sc_core::sc_export_base"** }
%"class.sc_core::sc_export_base" = type { %"class.sc_core::sc_object" }
%"class.sc_core::sc_prim_channel_registry" = type { %"class.sc_core::sc_simcontext"*, %"class.std::vector.5", %"class.sc_core::sc_prim_channel"* }
%"class.std::vector.5" = type { %"struct.std::_Vector_base.6" }
%"struct.std::_Vector_base.6" = type { %"struct.std::_Vector_base<sc_core::sc_prim_channel *, std::allocator<sc_core::sc_prim_channel *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_prim_channel *, std::allocator<sc_core::sc_prim_channel *> >::_Vector_impl" = type { %"class.sc_core::sc_prim_channel"**, %"class.sc_core::sc_prim_channel"**, %"class.sc_core::sc_prim_channel"** }
%"class.sc_core::sc_prim_channel" = type { %"class.sc_core::sc_object", %"class.sc_core::sc_prim_channel_registry"*, %"class.sc_core::sc_prim_channel"* }
%"class.sc_core::sc_name_gen" = type opaque
%"class.sc_core::sc_process_table" = type opaque
%"struct.sc_core::sc_curr_proc_info" = type { %"class.sc_core::sc_process_b"*, i32 }
%"class.std::vector.10" = type { %"struct.std::_Vector_base.11" }
%"struct.std::_Vector_base.11" = type { %"struct.std::_Vector_base<sc_core::sc_object *, std::allocator<sc_core::sc_object *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_object *, std::allocator<sc_core::sc_object *> >::_Vector_impl" = type { %"class.sc_core::sc_object"**, %"class.sc_core::sc_object"**, %"class.sc_core::sc_object"** }
%"class.std::vector.15" = type { %"struct.std::_Vector_base.16" }
%"struct.std::_Vector_base.16" = type { %"struct.std::_Vector_base<sc_core::sc_event *, std::allocator<sc_core::sc_event *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_event *, std::allocator<sc_core::sc_event *> >::_Vector_impl" = type { %"class.sc_core::sc_event"**, %"class.sc_core::sc_event"**, %"class.sc_core::sc_event"** }
%"class.sc_core::sc_event" = type { %"class.sc_core::sc_simcontext"*, i32, i32, %"class.sc_core::sc_event_timed"*, %"class.std::vector.20", %"class.std::vector.20", %"class.std::vector.25", %"class.std::vector.25" }
%"class.sc_core::sc_event_timed" = type { %"class.sc_core::sc_event"*, %"class.sc_core::sc_time" }
%"class.sc_core::sc_time" = type { i64 }
%"class.std::vector.20" = type { %"struct.std::_Vector_base.21" }
%"struct.std::_Vector_base.21" = type { %"struct.std::_Vector_base<sc_core::sc_method_process *, std::allocator<sc_core::sc_method_process *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_method_process *, std::allocator<sc_core::sc_method_process *> >::_Vector_impl" = type { %"class.sc_core::sc_method_process"**, %"class.sc_core::sc_method_process"**, %"class.sc_core::sc_method_process"** }
%"class.sc_core::sc_method_process" = type opaque
%"class.std::vector.25" = type { %"struct.std::_Vector_base.26" }
%"struct.std::_Vector_base.26" = type { %"struct.std::_Vector_base<sc_core::sc_thread_process *, std::allocator<sc_core::sc_thread_process *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_thread_process *, std::allocator<sc_core::sc_thread_process *> >::_Vector_impl" = type { %"class.sc_core::sc_thread_process"**, %"class.sc_core::sc_thread_process"**, %"class.sc_core::sc_thread_process"** }
%"class.sc_core::sc_thread_process" = type opaque
%"class.sc_core::sc_ppq" = type { %"class.sc_core::sc_ppq_base" }
%"class.sc_core::sc_ppq_base" = type { i8**, i32, i32, i32 (i8*, i8*)* }
%"class.std::vector.30" = type { %"struct.std::_Vector_base.31" }
%"struct.std::_Vector_base.31" = type { %"struct.std::_Vector_base<sc_core::sc_trace_file *, std::allocator<sc_core::sc_trace_file *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_trace_file *, std::allocator<sc_core::sc_trace_file *> >::_Vector_impl" = type { %"class.sc_core::sc_trace_file"**, %"class.sc_core::sc_trace_file"**, %"class.sc_core::sc_trace_file"** }
%"class.sc_core::sc_trace_file" = type { i32 (...)**, i8, double, i8 }
%"class.sc_core::sc_runnable" = type opaque
%"struct.sc_core::sc_time_params" = type { double, i8, i8, i64, i8 }
%"class.sc_core::sc_cor_pkg" = type opaque
%"class.sc_core::sc_cor" = type opaque
%"class.sc_core::sc_attr_cltn" = type { %"class.sc_core::sc_pvector" }
%"class.sc_core::sc_pvector" = type { %"class.std::vector.35" }
%"class.std::vector.35" = type { %"struct.std::_Vector_base.36" }
%"struct.std::_Vector_base.36" = type { %"struct.std::_Vector_base<sc_core::sc_attr_base *, std::allocator<sc_core::sc_attr_base *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_attr_base *, std::allocator<sc_core::sc_attr_base *> >::_Vector_impl" = type { %"class.sc_core::sc_attr_base"**, %"class.sc_core::sc_attr_base"**, %"class.sc_core::sc_attr_base"** }
%"class.sc_core::sc_attr_base" = type { i32 (...)**, %"class.std::basic_string" }
%"class.std::basic_string" = type { %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Alloc_hider" }
%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Alloc_hider" = type { i8* }
%"class.sc_core::sc_event_list" = type { %"class.std::vector.43", i8, i8 }
%"class.std::vector.43" = type { %"struct.std::_Vector_base.44" }
%"struct.std::_Vector_base.44" = type { %"struct.std::_Vector_base<const sc_core::sc_event *, std::allocator<const sc_core::sc_event *> >::_Vector_impl" }
%"struct.std::_Vector_base<const sc_core::sc_event *, std::allocator<const sc_core::sc_event *> >::_Vector_impl" = type { %"class.sc_core::sc_event"**, %"class.sc_core::sc_event"**, %"class.sc_core::sc_event"** }
%"class.sc_core::sc_report" = type { %"class.std::exception", i32, %"struct.sc_core::sc_msg_def"*, i8*, i8*, i32, %"class.sc_core::sc_time"*, %"class.sc_core::sc_object"*, i8* }
%"class.std::exception" = type { i32 (...)** }
%"struct.sc_core::sc_msg_def" = type { i8*, i32, [4 x i32], i32, [4 x i32], i32, i32, [4 x i32], i8*, i32 }
%"class.sc_core::sc_reset" = type { %"class.sc_core::sc_signal_in_if.48"*, %"class.std::vector.49" }
%"class.sc_core::sc_signal_in_if.48" = type { %"class.sc_core::sc_interface" }
%"class.sc_core::sc_interface" = type { i32 (...)** }
%"class.std::vector.49" = type { %"struct.std::_Vector_base.50" }
%"struct.std::_Vector_base.50" = type { %"struct.std::_Vector_base<sc_core::sc_process_b *, std::allocator<sc_core::sc_process_b *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_process_b *, std::allocator<sc_core::sc_process_b *> >::_Vector_impl" = type { %"class.sc_core::sc_process_b"**, %"class.sc_core::sc_process_b"**, %"class.sc_core::sc_process_b"** }
%"class.sc_core::sc_process_host" = type { i32 (...)** }
%"class.std::allocator.40" = type { i8 }
%"class.sc_core::sc_signal.54" = type { %"class.sc_core::sc_signal_inout_if.55", %"class.sc_core::sc_prim_channel", %"class.sc_core::sc_event"*, i8, i64, %"class.sc_core::sc_event"*, i8, %"class.sc_core::sc_port_base"*, %"class.sc_core::sc_event"*, %"class.sc_core::sc_reset"*, %"class.sc_core::sc_object"* }
%"class.sc_core::sc_signal_inout_if.55" = type { %"class.sc_core::sc_signal_in_if.48", %"class.sc_core::sc_signal_write_if.56" }
%"class.sc_core::sc_signal_write_if.56" = type { %"class.sc_core::sc_interface" }
%class.Source = type { %"class.sc_core::sc_module", %"class.sc_core::sc_out" }
%"class.sc_core::sc_module" = type { %"class.sc_core::sc_object", %"class.sc_core::sc_process_host", %"class.sc_core::sc_sensitive", %"class.sc_core::sc_sensitive_pos", %"class.sc_core::sc_sensitive_neg", i8, %"class.std::vector"*, i32, %"class.sc_core::sc_name_gen"*, %"class.std::vector.10", %"class.sc_core::sc_module_name"* }
%"class.sc_core::sc_sensitive" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_pos" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_neg" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_module_name" = type { i8*, %"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*, %"class.sc_core::sc_simcontext"*, i8 }
%"class.sc_core::sc_out" = type { %"class.sc_core::sc_inout" }
%"class.sc_core::sc_inout" = type { %"class.sc_core::sc_port", i8*, %"class.std::vector.62"*, %"class.sc_core::sc_event_finder"*, %"class.sc_core::sc_event_finder"*, %"class.sc_core::sc_event_finder"* }
%"class.sc_core::sc_port" = type { %"class.sc_core::sc_port_b" }
%"class.sc_core::sc_port_b" = type { %"class.sc_core::sc_port_base", %"class.sc_core::sc_signal_inout_if.55"*, %"class.std::vector.57" }
%"class.std::vector.57" = type { %"struct.std::_Vector_base.58" }
%"struct.std::_Vector_base.58" = type { %"struct.std::_Vector_base<sc_core::sc_signal_inout_if<bool> *, std::allocator<sc_core::sc_signal_inout_if<bool> *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_signal_inout_if<bool> *, std::allocator<sc_core::sc_signal_inout_if<bool> *> >::_Vector_impl" = type { %"class.sc_core::sc_signal_inout_if.55"**, %"class.sc_core::sc_signal_inout_if.55"**, %"class.sc_core::sc_signal_inout_if.55"** }
%"class.std::vector.62" = type { %"struct.std::_Vector_base.63" }
%"struct.std::_Vector_base.63" = type { %"struct.std::_Vector_base<sc_core::sc_trace_params *, std::allocator<sc_core::sc_trace_params *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_trace_params *, std::allocator<sc_core::sc_trace_params *> >::_Vector_impl" = type { %"struct.sc_core::sc_trace_params"**, %"struct.sc_core::sc_trace_params"**, %"struct.sc_core::sc_trace_params"** }
%"struct.sc_core::sc_trace_params" = type { %"class.sc_core::sc_trace_file"*, %"class.std::basic_string" }
%"class.sc_core::sc_event_finder" = type { i32 (...)**, %"class.sc_core::sc_port_base"* }
%class.Target = type { %"class.sc_core::sc_module", %"class.sc_core::sc_in" }
%"class.sc_core::sc_in" = type { %"class.sc_core::sc_port.67", %"class.std::vector.62"*, %"class.sc_core::sc_event_finder"*, %"class.sc_core::sc_event_finder"*, %"class.sc_core::sc_event_finder"* }
%"class.sc_core::sc_port.67" = type { %"class.sc_core::sc_port_b.68" }
%"class.sc_core::sc_port_b.68" = type { %"class.sc_core::sc_port_base", %"class.sc_core::sc_signal_in_if.48"*, %"class.std::vector.69" }
%"class.std::vector.69" = type { %"struct.std::_Vector_base.70" }
%"struct.std::_Vector_base.70" = type { %"struct.std::_Vector_base<sc_core::sc_signal_in_if<bool> *, std::allocator<sc_core::sc_signal_in_if<bool> *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_signal_in_if<bool> *, std::allocator<sc_core::sc_signal_in_if<bool> *> >::_Vector_impl" = type { %"class.sc_core::sc_signal_in_if.48"**, %"class.sc_core::sc_signal_in_if.48"**, %"class.sc_core::sc_signal_in_if.48"** }
%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep" = type { %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep_base" }
%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep_base" = type { i64, i64, i32 }
%"class.std::basic_ostream" = type { i32 (...)**, %"class.std::basic_ios" }
%"class.std::basic_ios" = type { %"class.std::ios_base", %"class.std::basic_ostream"*, i8, i8, %"class.std::basic_streambuf"*, %"class.std::ctype"*, %"class.std::num_put"*, %"class.std::num_get"* }
%"class.std::ios_base" = type { i32 (...)**, i64, i64, i32, i32, i32, %"struct.std::ios_base::_Callback_list"*, %"struct.std::ios_base::_Words", [8 x %"struct.std::ios_base::_Words"], i32, %"struct.std::ios_base::_Words"*, %"class.std::locale" }
%"struct.std::ios_base::_Callback_list" = type { %"struct.std::ios_base::_Callback_list"*, void (i32, %"class.std::ios_base"*, i32)*, i32, i32 }
%"struct.std::ios_base::_Words" = type { i8*, i64 }
%"class.std::locale" = type { %"class.std::locale::_Impl"* }
%"class.std::locale::_Impl" = type { i32, %"class.std::locale::facet"**, i64, %"class.std::locale::facet"**, i8** }
%"class.std::locale::facet" = type { i32 (...)**, i32 }
%"class.std::basic_streambuf" = type { i32 (...)**, i8*, i8*, i8*, i8*, i8*, i8*, %"class.std::locale" }
%"class.std::ctype" = type { %"class.std::locale::facet", %struct.__locale_struct*, i8, i32*, i32*, i16*, i8, [256 x i8], [256 x i8], i8 }
%struct.__locale_struct = type { [13 x %struct.__locale_data*], i16*, i32*, i32*, [13 x i8*] }
%struct.__locale_data = type opaque
%"class.std::num_put" = type { %"class.std::locale::facet" }
%"class.std::num_get" = type { %"class.std::locale::facet" }
%"class.sc_core::sc_process_handle" = type { %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_spawn_options" = type opaque

@_ZStL8__ioinit = internal global %"class.std::ios_base::Init" zeroinitializer, align 1
@__dso_handle = external global i8
@_ZN7sc_coreL17api_version_checkE = internal global %"class.sc_core::sc_api_version_2_2_0" zeroinitializer, align 1
@_ZTVN10__cxxabiv121__vmi_class_type_infoE = external global i8*
@_ZTVN10__cxxabiv117__class_type_infoE = external global i8*
@_ZTSN7sc_core12sc_interfaceE = available_externally constant [25 x i8] c"N7sc_core12sc_interfaceE\00"
@_ZTIN7sc_core12sc_interfaceE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([25 x i8]* @_ZTSN7sc_core12sc_interfaceE, i32 0, i32 0) }
@.str = private unnamed_addr constant [7 x i8] c"source\00", align 1
@.str2 = private unnamed_addr constant [7 x i8] c"target\00", align 1
@_ZTVN10__cxxabiv120__si_class_type_infoE = external global i8*
@_ZTSN7sc_core15sc_prim_channelE = available_externally constant [28 x i8] c"N7sc_core15sc_prim_channelE\00"
@_ZTSN7sc_core9sc_objectE = available_externally constant [21 x i8] c"N7sc_core9sc_objectE\00"
@_ZTIN7sc_core9sc_objectE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_objectE, i32 0, i32 0) }
@_ZTIN7sc_core15sc_prim_channelE = available_externally unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_prim_channelE, i32 0, i32 0), i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*) }
@_ZNSs4_Rep20_S_empty_rep_storageE = external global [0 x i64]
@.str3 = private unnamed_addr constant [10 x i8] c"sc_signal\00", align 1
@_ZN7sc_core21SC_ID_NOTIFY_DELAYED_E = external constant [0 x i8]
@.str4 = private unnamed_addr constant [42 x i8] c"/usr/local/include/sysc/kernel/sc_event.h\00", align 1
@.str6 = private unnamed_addr constant [13 x i8] c"     name = \00", align 1
@.str7 = private unnamed_addr constant [13 x i8] c"    value = \00", align 1
@.str8 = private unnamed_addr constant [13 x i8] c"new value = \00", align 1
@_ZTV6Source = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI6Source to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.Source*)* @_ZN6SourceD1Ev to i8*), i8* bitcast (void (%class.Source*)* @_ZN6SourceD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI6Source to i8*), i8* bitcast (void (%class.Source*)* @_ZThn40_N6SourceD1Ev to i8*), i8* bitcast (void (%class.Source*)* @_ZThn40_N6SourceD0Ev to i8*)]
@_ZTS6Source = linkonce_odr constant [8 x i8] c"6Source\00"
@_ZTSN7sc_core9sc_moduleE = available_externally constant [21 x i8] c"N7sc_core9sc_moduleE\00"
@_ZTSN7sc_core15sc_process_hostE = linkonce_odr constant [28 x i8] c"N7sc_core15sc_process_hostE\00"
@_ZTIN7sc_core15sc_process_hostE = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_process_hostE, i32 0, i32 0) }
@_ZTIN7sc_core9sc_moduleE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_moduleE, i32 0, i32 0), i32 0, i32 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*), i64 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core15sc_process_hostE to i8*), i64 10242 }
@_ZTI6Source = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([8 x i8]* @_ZTS6Source, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@.str9 = private unnamed_addr constant [10 x i8] c"sc_module\00", align 1
@_ZTV6Target = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI6Target to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.Target*)* @_ZN6TargetD1Ev to i8*), i8* bitcast (void (%class.Target*)* @_ZN6TargetD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI6Target to i8*), i8* bitcast (void (%class.Target*)* @_ZThn40_N6TargetD1Ev to i8*), i8* bitcast (void (%class.Target*)* @_ZThn40_N6TargetD0Ev to i8*)]
@_ZTS6Target = linkonce_odr constant [8 x i8] c"6Target\00"
@_ZTI6Target = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([8 x i8]* @_ZTS6Target, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@_ZTVN7sc_core5sc_inIbEE = available_externally unnamed_addr constant [22 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core5sc_inIbEE to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_in"*)* @_ZNK7sc_core5sc_inIbE4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_object"*)* @_ZNK7sc_core9sc_object17get_child_objectsEv to i8*), i8* bitcast (void (%"class.sc_core::sc_in"*)* @_ZN7sc_core5sc_inIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_in"*)* @_ZN7sc_core5sc_inIbED0Ev to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b.68"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13get_interfaceEv to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b.68"*)* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13get_interfaceEv to i8*), i8* bitcast (i32 (%"class.sc_core::sc_in"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core5sc_inIbE5vbindERNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_in"*, %"class.sc_core::sc_port_base"*)* @_ZN7sc_core5sc_inIbE5vbindERNS_12sc_port_baseE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.68"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13add_interfaceEPNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.68"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE15interface_countEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_b.68"*)* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE11if_typenameEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_in"*)* @_ZN7sc_core5sc_inIbE18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base17end_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.68"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.68"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE to i8*)]
@_ZTSN7sc_core5sc_inIbEE = available_externally constant [20 x i8] c"N7sc_core5sc_inIbEE\00"
@_ZTSN7sc_core7sc_portINS_15sc_signal_in_ifIbEELi1ELNS_14sc_port_policyE0EEE = linkonce_odr constant [72 x i8] c"N7sc_core7sc_portINS_15sc_signal_in_ifIbEELi1ELNS_14sc_port_policyE0EEE\00"
@_ZTSN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE = linkonce_odr constant [47 x i8] c"N7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE\00"
@_ZTSN7sc_core12sc_port_baseE = available_externally constant [25 x i8] c"N7sc_core12sc_port_baseE\00"
@_ZTIN7sc_core12sc_port_baseE = available_externally unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([25 x i8]* @_ZTSN7sc_core12sc_port_baseE, i32 0, i32 0), i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*) }
@_ZTIN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([47 x i8]* @_ZTSN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core12sc_port_baseE to i8*) }
@_ZTIN7sc_core7sc_portINS_15sc_signal_in_ifIbEELi1ELNS_14sc_port_policyE0EEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([72 x i8]* @_ZTSN7sc_core7sc_portINS_15sc_signal_in_ifIbEELi1ELNS_14sc_port_policyE0EEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE to i8*) }
@_ZTIN7sc_core5sc_inIbEE = available_externally unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([20 x i8]* @_ZTSN7sc_core5sc_inIbEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core7sc_portINS_15sc_signal_in_ifIbEELi1ELNS_14sc_port_policyE0EEE to i8*) }
@.str10 = private unnamed_addr constant [13 x i8] c"iface_p != 0\00", align 1
@.str11 = private unnamed_addr constant [48 x i8] c"/usr/local/include/sysc/communication/sc_port.h\00", align 1
@__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE = private unnamed_addr constant [171 x i8] c"virtual void sc_core::sc_port_b<sc_core::sc_signal_in_if<bool> >::make_sensitive(sc_method_handle, sc_core::sc_event_finder *) const [IF = sc_core::sc_signal_in_if<bool>]\00", align 1
@__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE = private unnamed_addr constant [171 x i8] c"virtual void sc_core::sc_port_b<sc_core::sc_signal_in_if<bool> >::make_sensitive(sc_thread_handle, sc_core::sc_event_finder *) const [IF = sc_core::sc_signal_in_if<bool>]\00", align 1
@_ZTSN7sc_core15sc_signal_in_ifIbEE = linkonce_odr constant [31 x i8] c"N7sc_core15sc_signal_in_ifIbEE\00"
@_ZTIN7sc_core15sc_signal_in_ifIbEE = linkonce_odr unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([31 x i8]* @_ZTSN7sc_core15sc_signal_in_ifIbEE, i32 0, i32 0), i32 0, i32 1, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i64 -12285 }
@.str12 = private unnamed_addr constant [11 x i8] c"iface != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13add_interfaceEPNS_12sc_interfaceE = private unnamed_addr constant [143 x i8] c"virtual void sc_core::sc_port_b<sc_core::sc_signal_in_if<bool> >::add_interface(sc_core::sc_interface *) [IF = sc_core::sc_signal_in_if<bool>]\00", align 1
@_ZN7sc_core22SC_ID_BIND_IF_TO_PORT_E = external constant [0 x i8]
@.str13 = private unnamed_addr constant [32 x i8] c"interface already bound to port\00", align 1
@.str14 = private unnamed_addr constant [6 x i8] c"sc_in\00", align 1
@_ZTVN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE = linkonce_odr unnamed_addr constant [22 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_base"*)* @_ZNK7sc_core12sc_port_base4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_object"*)* @_ZNK7sc_core9sc_object17get_child_objectsEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.68"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.68"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED0Ev to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b.68"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13get_interfaceEv to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b.68"*)* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13get_interfaceEv to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.68"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE5vbindERNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.68"*, %"class.sc_core::sc_port_base"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE5vbindERNS_12sc_port_baseE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.68"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13add_interfaceEPNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.68"*)* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE15interface_countEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_b.68"*)* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE11if_typenameEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base17end_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.68"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.68"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE to i8*)]
@.str15 = private unnamed_addr constant [13 x i8] c"sc_port_base\00", align 1
@.str16 = private unnamed_addr constant [8 x i8] c"compute\00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str17 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str18 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str19 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_increment()\00", align 1
@_ZN7sc_core13SC_ID_GET_IF_E = external constant [0 x i8]
@.str20 = private unnamed_addr constant [18 x i8] c"port is not bound\00", align 1
@_ZTVN7sc_core6sc_outIbEE = linkonce_odr unnamed_addr constant [22 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core6sc_outIbEE to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_out"*)* @_ZNK7sc_core6sc_outIbE4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_object"*)* @_ZNK7sc_core9sc_object17get_child_objectsEv to i8*), i8* bitcast (void (%"class.sc_core::sc_out"*)* @_ZN7sc_core6sc_outIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_out"*)* @_ZN7sc_core6sc_outIbED0Ev to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b"*)* @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE13get_interfaceEv to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b"*)* @_ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE13get_interfaceEv to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE5vbindERNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_port_base"*)* @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE5vbindERNS_12sc_port_baseE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE13add_interfaceEPNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*)* @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE15interface_countEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_b"*)* @_ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE11if_typenameEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_inout"*)* @_ZN7sc_core8sc_inoutIbE18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base17end_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE to i8*)]
@_ZTSN7sc_core6sc_outIbEE = linkonce_odr constant [21 x i8] c"N7sc_core6sc_outIbEE\00"
@_ZTSN7sc_core8sc_inoutIbEE = available_externally constant [23 x i8] c"N7sc_core8sc_inoutIbEE\00"
@_ZTSN7sc_core7sc_portINS_18sc_signal_inout_ifIbEELi1ELNS_14sc_port_policyE0EEE = linkonce_odr constant [75 x i8] c"N7sc_core7sc_portINS_18sc_signal_inout_ifIbEELi1ELNS_14sc_port_policyE0EEE\00"
@_ZTSN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEEE = linkonce_odr constant [50 x i8] c"N7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEEE\00"
@_ZTIN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([50 x i8]* @_ZTSN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core12sc_port_baseE to i8*) }
@_ZTIN7sc_core7sc_portINS_18sc_signal_inout_ifIbEELi1ELNS_14sc_port_policyE0EEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([75 x i8]* @_ZTSN7sc_core7sc_portINS_18sc_signal_inout_ifIbEELi1ELNS_14sc_port_policyE0EEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEEE to i8*) }
@_ZTIN7sc_core8sc_inoutIbEE = available_externally unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([23 x i8]* @_ZTSN7sc_core8sc_inoutIbEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core7sc_portINS_18sc_signal_inout_ifIbEELi1ELNS_14sc_port_policyE0EEE to i8*) }
@_ZTIN7sc_core6sc_outIbEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core6sc_outIbEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core8sc_inoutIbEE to i8*) }
@__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE = private unnamed_addr constant [177 x i8] c"virtual void sc_core::sc_port_b<sc_core::sc_signal_inout_if<bool> >::make_sensitive(sc_method_handle, sc_core::sc_event_finder *) const [IF = sc_core::sc_signal_inout_if<bool>]\00", align 1
@__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE = private unnamed_addr constant [177 x i8] c"virtual void sc_core::sc_port_b<sc_core::sc_signal_inout_if<bool> >::make_sensitive(sc_thread_handle, sc_core::sc_event_finder *) const [IF = sc_core::sc_signal_inout_if<bool>]\00", align 1
@_ZTSN7sc_core18sc_signal_inout_ifIbEE = linkonce_odr constant [34 x i8] c"N7sc_core18sc_signal_inout_ifIbEE\00"
@_ZTSN7sc_core18sc_signal_write_ifIbEE = linkonce_odr constant [34 x i8] c"N7sc_core18sc_signal_write_ifIbEE\00"
@_ZTIN7sc_core18sc_signal_write_ifIbEE = linkonce_odr unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([34 x i8]* @_ZTSN7sc_core18sc_signal_write_ifIbEE, i32 0, i32 0), i32 0, i32 1, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i64 -12285 }
@_ZTIN7sc_core18sc_signal_inout_ifIbEE = linkonce_odr unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([34 x i8]* @_ZTSN7sc_core18sc_signal_inout_ifIbEE, i32 0, i32 0), i32 2, i32 2, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTIN7sc_core15sc_signal_in_ifIbEE to i8*), i64 2, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTIN7sc_core18sc_signal_write_ifIbEE to i8*), i64 2050 }
@__PRETTY_FUNCTION__._ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE13add_interfaceEPNS_12sc_interfaceE = private unnamed_addr constant [149 x i8] c"virtual void sc_core::sc_port_b<sc_core::sc_signal_inout_if<bool> >::add_interface(sc_core::sc_interface *) [IF = sc_core::sc_signal_inout_if<bool>]\00", align 1
@.str22 = private unnamed_addr constant [7 x i8] c"sc_out\00", align 1
@_ZTVN7sc_core9sc_signalIbEE = available_externally unnamed_addr constant [51 x i8*] [i8* null, i8* null, i8* null, i8* null, i8* null, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_signalIbEE to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, %"class.sc_core::sc_port_base"*, i8*)* @_ZN7sc_core9sc_signalIbE13register_portERNS_12sc_port_baseEPKc to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE13default_eventEv to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*)* @_ZN7sc_core9sc_signalIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*)* @_ZN7sc_core9sc_signalIbED0Ev to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE19value_changed_eventEv to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE13posedge_eventEv to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE13negedge_eventEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE4readEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE12get_data_refEv to i8*), i8* bitcast (i1 (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE5eventEv to i8*), i8* bitcast (i1 (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE7posedgeEv to i8*), i8* bitcast (i1 (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE7negedgeEv to i8*), i8* bitcast (%"class.sc_core::sc_reset"* (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE8is_resetEv to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, i8*)* @_ZN7sc_core9sc_signalIbE5writeERKb to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_signalIbE5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_signalIbE5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_signalIbE4dumpERSo to i8*), i8* bitcast (i8* (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE4kindEv to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*)* @_ZN7sc_core9sc_signalIbE6updateEv to i8*), i8* bitcast (i1 (%"class.sc_core::sc_signal.54"*)* @_ZNK7sc_core9sc_signalIbE8is_clockEv to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_signalIbEE to i8*), i8* null, i8* null, i8* bitcast (void (%"class.sc_core::sc_signal.54"*)* @_ZThn8_N7sc_core9sc_signalIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*)* @_ZThn8_N7sc_core9sc_signalIbED0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, i8*)* @_ZThn8_N7sc_core9sc_signalIbE5writeERKb to i8*), i8* inttoptr (i64 -16 to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_signalIbEE to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, %"class.std::basic_ostream"*)* @_ZThn16_NK7sc_core9sc_signalIbE5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, %"class.std::basic_ostream"*)* @_ZThn16_NK7sc_core9sc_signalIbE4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*, %"class.sc_core::sc_trace_file"*)* @_ZThn16_NK7sc_core9sc_signalIbE5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_signal.54"*)* @_ZThn16_NK7sc_core9sc_signalIbE4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_object"*)* @_ZNK7sc_core9sc_object17get_child_objectsEv to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*)* @_ZThn16_N7sc_core9sc_signalIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*)* @_ZThn16_N7sc_core9sc_signalIbED0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal.54"*)* @_ZThn16_N7sc_core9sc_signalIbE6updateEv to i8*), i8* bitcast (void (%"class.sc_core::sc_prim_channel"*)* @_ZN7sc_core15sc_prim_channel25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_prim_channel"*)* @_ZN7sc_core15sc_prim_channel18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_prim_channel"*)* @_ZN7sc_core15sc_prim_channel19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_prim_channel"*)* @_ZN7sc_core15sc_prim_channel17end_of_simulationEv to i8*)]
@.str24 = private unnamed_addr constant [7 x i8] c"signal\00", align 1
@_ZN5sc_dt10UINT64_ONEE = external constant i64
@_ZTSN7sc_core9sc_signalIbEE = available_externally constant [24 x i8] c"N7sc_core9sc_signalIbEE\00"
@_ZTIN7sc_core9sc_signalIbEE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([24 x i8]* @_ZTSN7sc_core9sc_signalIbEE, i32 0, i32 0), i32 2, i32 2, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core18sc_signal_inout_ifIbEE to i8*), i64 2, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core15sc_prim_channelE to i8*), i64 4098 }
@_ZTCN7sc_core9sc_signalIbEE0_NS_18sc_signal_inout_ifIbEE = available_externally unnamed_addr constant [30 x i8*] [i8* null, i8* null, i8* null, i8* null, i8* null, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core18sc_signal_inout_ifIbEE to i8*), i8* bitcast (void (%"class.sc_core::sc_interface"*, %"class.sc_core::sc_port_base"*, i8*)* @_ZN7sc_core12sc_interface13register_portERNS_12sc_port_baseEPKc to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)* @_ZNK7sc_core12sc_interface13default_eventEv to i8*), i8* bitcast (void (%"class.sc_core::sc_signal_inout_if.55"*)* @_ZN7sc_core18sc_signal_inout_ifIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal_inout_if.55"*)* @_ZN7sc_core18sc_signal_inout_ifIbED0Ev to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core18sc_signal_inout_ifIbEE to i8*), i8* null, i8* null, i8* bitcast (void (%"class.sc_core::sc_signal_inout_if.55"*)* @_ZThn8_N7sc_core18sc_signal_inout_ifIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal_inout_if.55"*)* @_ZThn8_N7sc_core18sc_signal_inout_ifIbED0Ev to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*)]
@_ZTCN7sc_core9sc_signalIbEE0_NS_15sc_signal_in_ifIbEE = available_externally unnamed_addr constant [19 x i8*] [i8* null, i8* null, i8* null, i8* null, i8* null, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTIN7sc_core15sc_signal_in_ifIbEE to i8*), i8* bitcast (void (%"class.sc_core::sc_interface"*, %"class.sc_core::sc_port_base"*, i8*)* @_ZN7sc_core12sc_interface13register_portERNS_12sc_port_baseEPKc to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)* @_ZNK7sc_core12sc_interface13default_eventEv to i8*), i8* bitcast (void (%"class.sc_core::sc_signal_in_if.48"*)* @_ZN7sc_core15sc_signal_in_ifIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal_in_if.48"*)* @_ZN7sc_core15sc_signal_in_ifIbED0Ev to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*)]
@_ZTCN7sc_core9sc_signalIbEE8_NS_18sc_signal_write_ifIbEE = available_externally unnamed_addr constant [20 x i8*] [i8* inttoptr (i64 -8 to i8*), i8* null, i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* null, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTIN7sc_core18sc_signal_write_ifIbEE to i8*), i8* null, i8* null, i8* bitcast (void (%"class.sc_core::sc_signal_write_if.56"*)* @_ZN7sc_core18sc_signal_write_ifIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal_write_if.56"*)* @_ZN7sc_core18sc_signal_write_ifIbED0Ev to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* inttoptr (i64 8 to i8*), i8* null, i8* null, i8* inttoptr (i64 8 to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTIN7sc_core18sc_signal_write_ifIbEE to i8*), i8* bitcast (void (%"class.sc_core::sc_interface"*, %"class.sc_core::sc_port_base"*, i8*)* @_ZN7sc_core12sc_interface13register_portERNS_12sc_port_baseEPKc to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)* @_ZNK7sc_core12sc_interface13default_eventEv to i8*), i8* bitcast (void (%"class.sc_core::sc_signal_write_if.56"*)* @_ZTv0_n40_N7sc_core18sc_signal_write_ifIbED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_signal_write_if.56"*)* @_ZTv0_n40_N7sc_core18sc_signal_write_ifIbED0Ev to i8*)]
@_ZN7sc_core18sc_curr_simcontextE = external global %"class.sc_core::sc_simcontext"*
@_ZN7sc_core25sc_default_global_contextE = external global %"class.sc_core::sc_simcontext"*
@llvm.global_ctors = appending global [1 x { i32, void ()* }] [{ i32, void ()* } { i32 65535, void ()* @_GLOBAL__I_a }]

declare void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"*)

declare void @_ZNSt8ios_base4InitD1Ev(%"class.std::ios_base::Init"*)

declare i32 @__cxa_atexit(void (i8*)*, i8*, i8*) nounwind

declare void @_ZN7sc_core20sc_api_version_2_2_0C1Ev(%"class.sc_core::sc_api_version_2_2_0"*)

declare void @_ZNSsC1EPKcRKSaIcE(%"class.std::basic_string"*, i8*, %"class.std::allocator.40"*)

declare i32 @__gxx_personality_v0(...)

declare void @_ZN7sc_core24sc_signal_invalid_writerEPNS_9sc_objectES1_S1_(%"class.sc_core::sc_object"*, %"class.sc_core::sc_object"*, %"class.sc_core::sc_object"*)

declare void @_ZSt9terminatev()

define i32 @sc_main(i32 %argc, i8** nocapture %argv) uwtable {
_ZN7sc_core9sc_signalIbEC1Ev.exit:
  %tmp = alloca %"class.sc_core::sc_time", align 8
  %s1 = alloca %"class.sc_core::sc_signal.54", align 8
  %source = alloca %class.Source, align 8
  %tmp1 = alloca %"class.sc_core::sc_module_name", align 8
  %target = alloca %class.Target, align 8
  %tmp2 = alloca %"class.sc_core::sc_module_name", align 8
  %tmp3 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 0, i32 0, i32 0
  call void @_ZN7sc_core12sc_interfaceC2Ev(%"class.sc_core::sc_interface"* %tmp3)
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 0, i32 0, i32 0, i32 0
  %tmp5 = load i64* bitcast ([19 x i8*]* @_ZTCN7sc_core9sc_signalIbEE0_NS_15sc_signal_in_ifIbEE to i64*), align 8
  %tmp6 = bitcast %"class.sc_core::sc_signal.54"* %s1 to i8*
  %tmp7 = getelementptr i8* %tmp6, i64 %tmp5
  %tmp8 = bitcast i8* %tmp7 to i8**
  store i8* bitcast (i8** getelementptr inbounds ([19 x i8*]* @_ZTCN7sc_core9sc_signalIbEE0_NS_15sc_signal_in_ifIbEE, i64 0, i64 6) to i8*), i8** %tmp8, align 8, !tbaa !0
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 0, i32 1
  %tmp10 = getelementptr inbounds %"class.sc_core::sc_signal_write_if.56"* %tmp9, i64 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([20 x i8*]* @_ZTCN7sc_core9sc_signalIbEE8_NS_18sc_signal_write_ifIbEE, i64 0, i64 6) to i32 (...)**), i32 (...)*** %tmp10, align 8, !tbaa !0
  %tmp11 = getelementptr inbounds %"class.sc_core::sc_signal_write_if.56"* %tmp9, i64 -1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([20 x i8*]* @_ZTCN7sc_core9sc_signalIbEE8_NS_18sc_signal_write_ifIbEE, i64 0, i64 16) to i32 (...)**), i32 (...)*** %tmp11, align 8, !tbaa !0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([30 x i8*]* @_ZTCN7sc_core9sc_signalIbEE0_NS_18sc_signal_inout_ifIbEE, i64 0, i64 6) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !0
  %tmp12 = load i64* bitcast ([30 x i8*]* @_ZTCN7sc_core9sc_signalIbEE0_NS_18sc_signal_inout_ifIbEE to i64*), align 8
  %tmp13 = getelementptr i8* %tmp6, i64 %tmp12
  %tmp14 = bitcast i8* %tmp13 to i8**
  store i8* bitcast (i8** getelementptr inbounds ([30 x i8*]* @_ZTCN7sc_core9sc_signalIbEE0_NS_18sc_signal_inout_ifIbEE, i64 0, i64 6) to i8*), i8** %tmp14, align 8, !tbaa !0
  %tmp15 = getelementptr %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 0, i32 1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([30 x i8*]* @_ZTCN7sc_core9sc_signalIbEE0_NS_18sc_signal_inout_ifIbEE, i64 0, i64 25) to i32 (...)**), i32 (...)*** %tmp15, align 8, !tbaa !0
  %tmp16 = call i8* @_ZN7sc_core18sc_gen_unique_nameEPKcb(i8* getelementptr inbounds ([7 x i8]* @.str24, i64 0, i64 0), i1 zeroext false)
  %tmp17 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 1
  call void @_ZN7sc_core15sc_prim_channelC2EPKc(%"class.sc_core::sc_prim_channel"* %tmp17, i8* %tmp16)
  store i32 (...)** bitcast (i8** getelementptr inbounds ([51 x i8*]* @_ZTVN7sc_core9sc_signalIbEE, i64 0, i64 6) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([51 x i8*]* @_ZTVN7sc_core9sc_signalIbEE, i64 0, i64 32) to i32 (...)**), i32 (...)*** %tmp15, align 8, !tbaa !0
  %tmp18 = getelementptr %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([51 x i8*]* @_ZTVN7sc_core9sc_signalIbEE, i64 0, i64 39) to i32 (...)**), i32 (...)*** %tmp18, align 8, !tbaa !0
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 2
  store %"class.sc_core::sc_event"* null, %"class.sc_core::sc_event"** %tmp19, align 8, !tbaa !2
  %tmp20 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 3
  store i8 0, i8* %tmp20, align 8, !tbaa !4
  %tmp21 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 4
  %tmp22 = load i64* @_ZN5sc_dt10UINT64_ONEE, align 8, !tbaa !5
  %tmp23 = xor i64 %tmp22, -1
  store i64 %tmp23, i64* %tmp21, align 8, !tbaa !5
  %tmp24 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 5
  store %"class.sc_core::sc_event"* null, %"class.sc_core::sc_event"** %tmp24, align 8, !tbaa !2
  %tmp25 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 6
  store i8 0, i8* %tmp25, align 8, !tbaa !4
  %tmp26 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %s1, i64 0, i32 7
  %tmp27 = bitcast %"class.sc_core::sc_port_base"** %tmp26 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp27, i8 0, i64 32, i32 8, i1 false)
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp1, i8* getelementptr inbounds ([7 x i8]* @.str, i64 0, i64 0))
  call void @_ZN6SourceC2EN7sc_core14sc_module_nameE(%class.Source* %source, %"class.sc_core::sc_module_name"* undef)
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp1)
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp2, i8* getelementptr inbounds ([7 x i8]* @.str2, i64 0, i64 0))
  call void @_ZN6TargetC2EN7sc_core14sc_module_nameE(%class.Target* %target, %"class.sc_core::sc_module_name"* undef)
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp2)
  %tmp28 = getelementptr inbounds %class.Source* %source, i64 0, i32 1, i32 0, i32 0, i32 0, i32 0
  %tmp29 = bitcast %"class.sc_core::sc_signal.54"* %s1 to i8**
  %tmp30 = load i8** %tmp29, align 8, !tbaa !0
  %tmp31 = getelementptr i8* %tmp30, i64 -48
  %tmp32 = bitcast i8* %tmp31 to i64*
  %tmp33 = load i64* %tmp32, align 8
  %tmp34 = getelementptr i8* %tmp6, i64 %tmp33
  %tmp35 = bitcast i8* %tmp34 to %"class.sc_core::sc_interface"*
  call void @_ZN7sc_core12sc_port_base4bindERNS_12sc_interfaceE(%"class.sc_core::sc_port_base"* %tmp28, %"class.sc_core::sc_interface"* %tmp35)
  %tmp36 = getelementptr inbounds %class.Target* %target, i64 0, i32 1
  %tmp37 = getelementptr inbounds %"class.sc_core::sc_in"* %tmp36, i64 0, i32 0, i32 0, i32 0
  %tmp38 = load i8** %tmp29, align 8, !tbaa !0
  %tmp39 = getelementptr i8* %tmp38, i64 -48
  %tmp40 = bitcast i8* %tmp39 to i64*
  %tmp41 = load i64* %tmp40, align 8
  %tmp42 = getelementptr i8* %tmp6, i64 %tmp41
  %tmp43 = bitcast i8* %tmp42 to %"class.sc_core::sc_interface"*
  call void @_ZN7sc_core12sc_port_base4bindERNS_12sc_interfaceE(%"class.sc_core::sc_port_base"* %tmp37, %"class.sc_core::sc_interface"* %tmp43)
  %tmp44 = bitcast %"class.sc_core::sc_time"* %tmp to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp44)
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitE(%"class.sc_core::sc_time"* %tmp, double 2.000000e+02, i32 2)
  call void @_ZN7sc_core8sc_startERKNS_7sc_timeE(%"class.sc_core::sc_time"* %tmp)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp44)
  %tmp45 = getelementptr inbounds %class.Target* %target, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp45, align 8, !tbaa !0
  %tmp46 = getelementptr %class.Target* %target, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp46, align 8, !tbaa !0
  call void @_ZN7sc_core5sc_inIbED2Ev(%"class.sc_core::sc_in"* %tmp36)
  %tmp47 = getelementptr inbounds %class.Target* %target, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp47)
  %tmp48 = getelementptr inbounds %class.Source* %source, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp48, align 8, !tbaa !0
  %tmp49 = getelementptr %class.Source* %source, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp49, align 8, !tbaa !0
  %tmp50 = getelementptr inbounds %class.Source* %source, i64 0, i32 1, i32 0
  call void @_ZN7sc_core8sc_inoutIbED2Ev(%"class.sc_core::sc_inout"* %tmp50)
  %tmp51 = getelementptr inbounds %class.Source* %source, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp51)
  call void @_ZN7sc_core9sc_signalIbED1Ev(%"class.sc_core::sc_signal.54"* %s1)
  ret i32 0
}

declare void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"*, i8*)

declare void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"*)

define linkonce_odr void @_ZN6TargetD1Ev(%class.Target* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN6TargetD2Ev.exit:
  %tmp = getelementptr inbounds %class.Target* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.Target* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %class.Target* %this, i64 0, i32 1
  tail call void @_ZN7sc_core5sc_inIbED2Ev(%"class.sc_core::sc_in"* %tmp2)
  %tmp3 = getelementptr inbounds %class.Target* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  ret void
}

define linkonce_odr void @_ZN6SourceD1Ev(%class.Source* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN6SourceD2Ev.exit:
  %tmp = getelementptr inbounds %class.Source* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.Source* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %class.Source* %this, i64 0, i32 1, i32 0
  tail call void @_ZN7sc_core8sc_inoutIbED2Ev(%"class.sc_core::sc_inout"* %tmp2)
  %tmp3 = getelementptr inbounds %class.Source* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  ret void
}

declare void @_ZN7sc_core9sc_signalIbED1Ev(%"class.sc_core::sc_signal.54"*)

define linkonce_odr noalias %"class.std::vector.10"* @_ZNK7sc_core9sc_object17get_child_objectsEv(%"class.sc_core::sc_object"* nocapture %this) uwtable align 2 {
bb:
  %tmp = tail call noalias i8* @_Znwm(i64 24)
  %tmp1 = bitcast i8* %tmp to %"class.std::vector.10"*
  tail call void @llvm.memset.p0i8.i64(i8* %tmp, i8 0, i64 24, i32 8, i1 false) nounwind
  ret %"class.std::vector.10"* %tmp1
}

declare void @_ZN7sc_core15sc_prim_channel25before_end_of_elaborationEv(%"class.sc_core::sc_prim_channel"*)

declare void @_ZN7sc_core15sc_prim_channel18end_of_elaborationEv(%"class.sc_core::sc_prim_channel"*)

declare void @_ZN7sc_core15sc_prim_channel19start_of_simulationEv(%"class.sc_core::sc_prim_channel"*)

declare void @_ZN7sc_core15sc_prim_channel17end_of_simulationEv(%"class.sc_core::sc_prim_channel"*)

declare void @_ZN7sc_core12sc_interface13register_portERNS_12sc_port_baseEPKc(%"class.sc_core::sc_interface"*, %"class.sc_core::sc_port_base"*, i8*)

declare %"class.sc_core::sc_event"* @_ZNK7sc_core12sc_interface13default_eventEv(%"class.sc_core::sc_interface"*)

declare void @__cxa_pure_virtual()

declare void @_ZN7sc_core12sc_port_base4bindERNS_12sc_interfaceE(%"class.sc_core::sc_port_base"*, %"class.sc_core::sc_interface"*)

declare i32 @_ZNKSs7compareEPKc(%"class.std::basic_string"*, i8*)

declare void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*, %"class.std::allocator.40"*) nounwind

declare extern_weak i32 @pthread_cancel(i64)

declare void @_ZdlPv(i8*) nounwind

declare void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"*)

declare noalias i8* @_Znwm(i64)

declare void @_ZN7sc_core19sc_deprecated_traceEv()

declare void @_ZN7sc_core17sc_report_handler6reportENS_11sc_severityEPKcS3_S3_i(i32, i8*, i8*, i8*, i32)

define linkonce_odr void @_ZNSt6vectorIPN7sc_core8sc_eventESaIS2_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS2_S4_EERKS2_(%"class.std::vector.15"* nocapture %this, %"class.sc_core::sc_event"** %__position.coerce, %"class.sc_core::sc_event"** nocapture %__x) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.std::vector.15"* %this, i64 0, i32 0, i32 0, i32 1
  %tmp1 = load %"class.sc_core::sc_event"*** %tmp, align 8, !tbaa !2
  %tmp2 = getelementptr inbounds %"class.std::vector.15"* %this, i64 0, i32 0, i32 0, i32 2
  %tmp3 = load %"class.sc_core::sc_event"*** %tmp2, align 8, !tbaa !2
  %tmp4 = icmp eq %"class.sc_core::sc_event"** %tmp1, %tmp3
  br i1 %tmp4, label %_ZNKSt6vectorIPN7sc_core8sc_eventESaIS2_EE12_M_check_lenEmPKc.exit, label %bb5

bb5:                                              ; preds = %bb
  %tmp6 = icmp eq %"class.sc_core::sc_event"** %tmp1, null
  br i1 %tmp6, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit, label %bb7

bb7:                                              ; preds = %bb5
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp1, i64 -1
  %tmp9 = load %"class.sc_core::sc_event"** %tmp8, align 8, !tbaa !2
  store %"class.sc_core::sc_event"* %tmp9, %"class.sc_core::sc_event"** %tmp1, align 8, !tbaa !2
  %.pre = load %"class.sc_core::sc_event"*** %tmp, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit: ; preds = %bb7, %bb5
  %tmp10 = phi %"class.sc_core::sc_event"** [ null, %bb5 ], [ %.pre, %bb7 ]
  %tmp11 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp10, i64 1
  store %"class.sc_core::sc_event"** %tmp11, %"class.sc_core::sc_event"*** %tmp, align 8, !tbaa !2
  %tmp12 = load %"class.sc_core::sc_event"** %__x, align 8, !tbaa !2
  %tmp13 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp10, i64 -1
  %tmp14 = ptrtoint %"class.sc_core::sc_event"** %tmp13 to i64
  %tmp15 = ptrtoint %"class.sc_core::sc_event"** %__position.coerce to i64
  %tmp16 = sub i64 %tmp14, %tmp15
  %tmp17 = ashr exact i64 %tmp16, 3
  %tmp18 = icmp eq i64 %tmp17, 0
  br i1 %tmp18, label %_ZSt13copy_backwardIPPN7sc_core8sc_eventES3_ET0_T_S5_S4_.exit, label %bb19

bb19:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit
  %.pre.i.i.i.i = sub i64 0, %tmp17
  %.pre1.i.i.i.i = getelementptr inbounds %"class.sc_core::sc_event"** %tmp10, i64 %.pre.i.i.i.i
  %tmp20 = bitcast %"class.sc_core::sc_event"** %.pre1.i.i.i.i to i8*
  %tmp21 = bitcast %"class.sc_core::sc_event"** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp20, i8* %tmp21, i64 %tmp16, i32 8, i1 false) nounwind
  br label %_ZSt13copy_backwardIPPN7sc_core8sc_eventES3_ET0_T_S5_S4_.exit

_ZSt13copy_backwardIPPN7sc_core8sc_eventES3_ET0_T_S5_S4_.exit: ; preds = %bb19, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit
  store %"class.sc_core::sc_event"* %tmp12, %"class.sc_core::sc_event"** %__position.coerce, align 8, !tbaa !2
  br label %bb67

_ZNKSt6vectorIPN7sc_core8sc_eventESaIS2_EE12_M_check_lenEmPKc.exit: ; preds = %bb
  %tmp22 = getelementptr inbounds %"class.std::vector.15"* %this, i64 0, i32 0, i32 0, i32 0
  %tmp23 = load %"class.sc_core::sc_event"*** %tmp22, align 8, !tbaa !2
  %tmp24 = ptrtoint %"class.sc_core::sc_event"** %tmp1 to i64
  %tmp25 = ptrtoint %"class.sc_core::sc_event"** %tmp23 to i64
  %tmp26 = sub i64 %tmp24, %tmp25
  %tmp27 = ashr exact i64 %tmp26, 3
  %tmp28 = icmp eq i64 %tmp27, 0
  %tmp29 = select i1 %tmp28, i64 1, i64 %tmp27
  %uadd.i = tail call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %tmp27, i64 %tmp29)
  %tmp30 = extractvalue { i64, i1 } %uadd.i, 0
  %tmp31 = extractvalue { i64, i1 } %uadd.i, 1
  %tmp32 = icmp ugt i64 %tmp30, 2305843009213693951
  %or.cond.i = or i1 %tmp31, %tmp32
  %tmp33 = select i1 %or.cond.i, i64 2305843009213693951, i64 %tmp30
  %tmp34 = icmp eq i64 %tmp33, 0
  br i1 %tmp34, label %_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE11_M_allocateEm.exit, label %bb35

bb35:                                             ; preds = %_ZNKSt6vectorIPN7sc_core8sc_eventESaIS2_EE12_M_check_lenEmPKc.exit
  %tmp36 = icmp ugt i64 %tmp33, 2305843009213693951
  br i1 %tmp36, label %bb37, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE8allocateEmPKv.exit.i

bb37:                                             ; preds = %bb35
  tail call void @_ZSt17__throw_bad_allocv() noreturn
  unreachable

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE8allocateEmPKv.exit.i: ; preds = %bb35
  %tmp38 = shl i64 %tmp33, 3
  %tmp39 = tail call noalias i8* @_Znwm(i64 %tmp38)
  %tmp40 = bitcast i8* %tmp39 to %"class.sc_core::sc_event"**
  br label %_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE11_M_allocateEm.exit

_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE11_M_allocateEm.exit: ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE8allocateEmPKv.exit.i, %_ZNKSt6vectorIPN7sc_core8sc_eventESaIS2_EE12_M_check_lenEmPKc.exit
  %tmp41 = phi %"class.sc_core::sc_event"** [ %tmp40, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE8allocateEmPKv.exit.i ], [ null, %_ZNKSt6vectorIPN7sc_core8sc_eventESaIS2_EE12_M_check_lenEmPKc.exit ]
  %tmp42 = ptrtoint %"class.sc_core::sc_event"** %__position.coerce to i64
  %tmp43 = sub i64 %tmp42, %tmp25
  %tmp44 = ashr exact i64 %tmp43, 3
  %tmp45 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp41, i64 %tmp44
  %tmp46 = icmp eq %"class.sc_core::sc_event"** %tmp45, null
  br i1 %tmp46, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit3, label %bb47

bb47:                                             ; preds = %_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE11_M_allocateEm.exit
  %tmp48 = load %"class.sc_core::sc_event"** %__x, align 8, !tbaa !2
  store %"class.sc_core::sc_event"* %tmp48, %"class.sc_core::sc_event"** %tmp45, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit3

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit3: ; preds = %bb47, %_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE11_M_allocateEm.exit
  %tmp49 = icmp eq i64 %tmp44, 0
  br i1 %tmp49, label %bb53, label %bb50

bb50:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit3
  %tmp51 = bitcast %"class.sc_core::sc_event"** %tmp41 to i8*
  %tmp52 = bitcast %"class.sc_core::sc_event"** %tmp23 to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp51, i8* %tmp52, i64 %tmp43, i32 8, i1 false) nounwind
  br label %bb53

bb53:                                             ; preds = %bb50, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit3
  %.sum = add i64 %tmp44, 1
  %tmp54 = sub i64 %tmp24, %tmp42
  %tmp55 = ashr exact i64 %tmp54, 3
  %tmp56 = icmp eq i64 %tmp55, 0
  br i1 %tmp56, label %bb61, label %bb57

bb57:                                             ; preds = %bb53
  %tmp58 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp41, i64 %.sum
  %tmp59 = bitcast %"class.sc_core::sc_event"** %tmp58 to i8*
  %tmp60 = bitcast %"class.sc_core::sc_event"** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp59, i8* %tmp60, i64 %tmp54, i32 8, i1 false) nounwind
  br label %bb61

bb61:                                             ; preds = %bb57, %bb53
  %tmp62 = icmp eq %"class.sc_core::sc_event"** %tmp23, null
  br i1 %tmp62, label %_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE13_M_deallocateEPS2_m.exit1, label %bb63

bb63:                                             ; preds = %bb61
  %tmp64 = bitcast %"class.sc_core::sc_event"** %tmp23 to i8*
  tail call void @_ZdlPv(i8* %tmp64) nounwind
  br label %_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE13_M_deallocateEPS2_m.exit1

_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE13_M_deallocateEPS2_m.exit1: ; preds = %bb63, %bb61
  %.sum4 = add i64 %tmp55, %.sum
  %tmp65 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp41, i64 %.sum4
  store %"class.sc_core::sc_event"** %tmp41, %"class.sc_core::sc_event"*** %tmp22, align 8, !tbaa !2
  store %"class.sc_core::sc_event"** %tmp65, %"class.sc_core::sc_event"*** %tmp, align 8, !tbaa !2
  %tmp66 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp41, i64 %tmp33
  store %"class.sc_core::sc_event"** %tmp66, %"class.sc_core::sc_event"*** %tmp2, align 8, !tbaa !2
  br label %bb67

bb67:                                             ; preds = %_ZNSt12_Vector_baseIPN7sc_core8sc_eventESaIS2_EE13_M_deallocateEPS2_m.exit1, %_ZSt13copy_backwardIPPN7sc_core8sc_eventES3_ET0_T_S5_S4_.exit
  ret void
}

declare void @llvm.memmove.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i32, i1) nounwind

declare void @_ZSt17__throw_bad_allocv() noreturn

declare %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"*, i8*, i64)

declare %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"*, i8 signext)

declare void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"*)

declare void @_ZSt16__throw_bad_castv() noreturn

declare %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"*)

declare i64 @strlen(i8* nocapture) nounwind readonly

declare void @_ZNSt9basic_iosIcSt11char_traitsIcEE5clearESt12_Ios_Iostate(%"class.std::basic_ios"*, i32)

declare void @_ZN7sc_core26sc_deprecated_get_data_refEv()

define linkonce_odr void @_ZThn40_N6SourceD1Ev(%class.Source* %this) {
_ZN6SourceD1Ev.exit:
  %tmp = getelementptr inbounds %class.Source* %this, i64 -1, i32 1, i32 0, i32 1
  store i8* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 2) to i8*), i8** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr i8** %tmp, i64 5
  store i8* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 15) to i8*), i8** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds i8** %tmp, i64 23
  %tmp3 = bitcast i8** %tmp2 to %"class.sc_core::sc_inout"*
  tail call void @_ZN7sc_core8sc_inoutIbED2Ev(%"class.sc_core::sc_inout"* %tmp3)
  %tmp4 = bitcast i8** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  ret void
}

define linkonce_odr void @_ZN7sc_core6sc_outIbED1Ev(%"class.sc_core::sc_out"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_out"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core8sc_inoutIbED2Ev(%"class.sc_core::sc_inout"* %tmp)
  ret void
}

declare void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"*)

declare void @_ZNK7sc_core9sc_object5printERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object4dumpERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)

define linkonce_odr i8* @_ZNK7sc_core9sc_module4kindEv(%"class.sc_core::sc_module"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str9, i64 0, i64 0)
}

declare %"class.std::vector.10"* @_ZNK7sc_core9sc_module17get_child_objectsEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN6SourceD0Ev(%class.Source* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN6SourceD2Ev.exit.i:
  %tmp = getelementptr inbounds %class.Source* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.Source* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %class.Source* %this, i64 0, i32 1, i32 0
  tail call void @_ZN7sc_core8sc_inoutIbED2Ev(%"class.sc_core::sc_inout"* %tmp2)
  %tmp3 = getelementptr inbounds %class.Source* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  %tmp4 = bitcast %class.Source* %this to i8*
  tail call void @_ZdlPv(i8* %tmp4) nounwind
  ret void
}

declare void @_ZN7sc_core9sc_module25before_end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module18end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module19start_of_simulationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module17end_of_simulationEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZThn40_N6SourceD0Ev(%class.Source* %this) {
_ZN6SourceD2Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.Source* %this, i64 -1, i32 1, i32 0, i32 1
  store i8* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 2) to i8*), i8** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr i8** %tmp, i64 5
  store i8* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 15) to i8*), i8** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds i8** %tmp, i64 23
  %tmp3 = bitcast i8** %tmp2 to %"class.sc_core::sc_inout"*
  tail call void @_ZN7sc_core8sc_inoutIbED2Ev(%"class.sc_core::sc_inout"* %tmp3)
  %tmp4 = bitcast i8** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  %tmp5 = bitcast i8** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  ret void
}

declare void @_ZN7sc_core8sc_inoutIbED2Ev(%"class.sc_core::sc_inout"*)

define linkonce_odr void @_ZThn40_N6TargetD1Ev(%class.Target* %this) {
_ZN6TargetD1Ev.exit:
  %tmp = getelementptr inbounds %class.Target* %this, i64 -1, i32 1, i32 0, i32 0, i32 2, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_signal_in_if.48"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 2) to %"class.sc_core::sc_signal_in_if.48"**), %"class.sc_core::sc_signal_in_if.48"*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"class.sc_core::sc_signal_in_if.48"*** %tmp, i64 5
  store %"class.sc_core::sc_signal_in_if.48"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 15) to %"class.sc_core::sc_signal_in_if.48"**), %"class.sc_core::sc_signal_in_if.48"*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"*** %tmp, i64 23
  %tmp3 = bitcast %"class.sc_core::sc_signal_in_if.48"*** %tmp2 to %"class.sc_core::sc_in"*
  tail call void @_ZN7sc_core5sc_inIbED2Ev(%"class.sc_core::sc_in"* %tmp3)
  %tmp4 = bitcast %"class.sc_core::sc_signal_in_if.48"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  ret void
}

define linkonce_odr void @_ZN7sc_core5sc_inIbED1Ev(%"class.sc_core::sc_in"* %this) unnamed_addr uwtable align 2 {
bb:
  tail call void @_ZN7sc_core5sc_inIbED2Ev(%"class.sc_core::sc_in"* %this)
  ret void
}

define linkonce_odr void @_ZN6TargetD0Ev(%class.Target* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN6TargetD2Ev.exit.i:
  %tmp = getelementptr inbounds %class.Target* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.Target* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %class.Target* %this, i64 0, i32 1
  tail call void @_ZN7sc_core5sc_inIbED2Ev(%"class.sc_core::sc_in"* %tmp2)
  %tmp3 = getelementptr inbounds %class.Target* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  %tmp4 = bitcast %class.Target* %this to i8*
  tail call void @_ZdlPv(i8* %tmp4) nounwind
  ret void
}

define linkonce_odr void @_ZThn40_N6TargetD0Ev(%class.Target* %this) {
_ZN6TargetD2Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.Target* %this, i64 -1, i32 1, i32 0, i32 0, i32 2, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_signal_in_if.48"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 2) to %"class.sc_core::sc_signal_in_if.48"**), %"class.sc_core::sc_signal_in_if.48"*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"class.sc_core::sc_signal_in_if.48"*** %tmp, i64 5
  store %"class.sc_core::sc_signal_in_if.48"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 15) to %"class.sc_core::sc_signal_in_if.48"**), %"class.sc_core::sc_signal_in_if.48"*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"*** %tmp, i64 23
  %tmp3 = bitcast %"class.sc_core::sc_signal_in_if.48"*** %tmp2 to %"class.sc_core::sc_in"*
  tail call void @_ZN7sc_core5sc_inIbED2Ev(%"class.sc_core::sc_in"* %tmp3)
  %tmp4 = bitcast %"class.sc_core::sc_signal_in_if.48"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  %tmp5 = bitcast %"class.sc_core::sc_signal_in_if.48"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  ret void
}

define linkonce_odr void @_ZN7sc_core5sc_inIbED2Ev(%"class.sc_core::sc_in"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_in"* %this, i64 0, i32 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core5sc_inIbEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  tail call void @_ZNK7sc_core5sc_inIbE13remove_tracesEv(%"class.sc_core::sc_in"* %this)
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_in"* %this, i64 0, i32 2
  %tmp2 = load %"class.sc_core::sc_event_finder"** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq %"class.sc_core::sc_event_finder"* %tmp2, null
  br i1 %tmp3, label %bb9, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %"class.sc_core::sc_event_finder"* %tmp2 to void (%"class.sc_core::sc_event_finder"*)***
  %tmp6 = load void (%"class.sc_core::sc_event_finder"*)*** %tmp5, align 8, !tbaa !0
  %tmp7 = getelementptr inbounds void (%"class.sc_core::sc_event_finder"*)** %tmp6, i64 1
  %tmp8 = load void (%"class.sc_core::sc_event_finder"*)** %tmp7, align 8
  tail call void %tmp8(%"class.sc_core::sc_event_finder"* %tmp2)
  br label %bb9

bb9:                                              ; preds = %bb4, %bb
  %tmp10 = getelementptr inbounds %"class.sc_core::sc_in"* %this, i64 0, i32 3
  %tmp11 = load %"class.sc_core::sc_event_finder"** %tmp10, align 8, !tbaa !2
  %tmp12 = icmp eq %"class.sc_core::sc_event_finder"* %tmp11, null
  br i1 %tmp12, label %bb18, label %bb13

bb13:                                             ; preds = %bb9
  %tmp14 = bitcast %"class.sc_core::sc_event_finder"* %tmp11 to void (%"class.sc_core::sc_event_finder"*)***
  %tmp15 = load void (%"class.sc_core::sc_event_finder"*)*** %tmp14, align 8, !tbaa !0
  %tmp16 = getelementptr inbounds void (%"class.sc_core::sc_event_finder"*)** %tmp15, i64 1
  %tmp17 = load void (%"class.sc_core::sc_event_finder"*)** %tmp16, align 8
  tail call void %tmp17(%"class.sc_core::sc_event_finder"* %tmp11)
  br label %bb18

bb18:                                             ; preds = %bb13, %bb9
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_in"* %this, i64 0, i32 4
  %tmp20 = load %"class.sc_core::sc_event_finder"** %tmp19, align 8, !tbaa !2
  %tmp21 = icmp eq %"class.sc_core::sc_event_finder"* %tmp20, null
  br i1 %tmp21, label %bb27, label %bb22

bb22:                                             ; preds = %bb18
  %tmp23 = bitcast %"class.sc_core::sc_event_finder"* %tmp20 to void (%"class.sc_core::sc_event_finder"*)***
  %tmp24 = load void (%"class.sc_core::sc_event_finder"*)*** %tmp23, align 8, !tbaa !0
  %tmp25 = getelementptr inbounds void (%"class.sc_core::sc_event_finder"*)** %tmp24, i64 1
  %tmp26 = load void (%"class.sc_core::sc_event_finder"*)** %tmp25, align 8
  tail call void %tmp26(%"class.sc_core::sc_event_finder"* %tmp20)
  br label %bb27

bb27:                                             ; preds = %bb22, %bb18
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp28 = getelementptr inbounds %"class.sc_core::sc_in"* %this, i64 0, i32 0, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp29 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp28, align 8, !tbaa !2
  %tmp30 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp29, null
  br i1 %tmp30, label %_ZN7sc_core7sc_portINS_15sc_signal_in_ifIbEELi1ELNS_14sc_port_policyE0EED2Ev.exit2, label %bb31

bb31:                                             ; preds = %bb27
  %tmp32 = bitcast %"class.sc_core::sc_signal_in_if.48"** %tmp29 to i8*
  tail call void @_ZdlPv(i8* %tmp32) nounwind
  br label %_ZN7sc_core7sc_portINS_15sc_signal_in_ifIbEELi1ELNS_14sc_port_policyE0EED2Ev.exit2

_ZN7sc_core7sc_portINS_15sc_signal_in_ifIbEELi1ELNS_14sc_port_policyE0EED2Ev.exit2: ; preds = %bb31, %bb27
  %tmp33 = getelementptr inbounds %"class.sc_core::sc_in"* %this, i64 0, i32 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp33)
  ret void
}

declare void @_ZNK7sc_core5sc_inIbE13remove_tracesEv(%"class.sc_core::sc_in"*)

define linkonce_odr i8* @_ZNK7sc_core5sc_inIbE4kindEv(%"class.sc_core::sc_in"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([6 x i8]* @.str14, i64 0, i64 0)
}

define linkonce_odr void @_ZN7sc_core5sc_inIbED0Ev(%"class.sc_core::sc_in"* %this) unnamed_addr uwtable align 2 {
_ZN7sc_core5sc_inIbED1Ev.exit:
  tail call void @_ZN7sc_core5sc_inIbED2Ev(%"class.sc_core::sc_in"* %this)
  %tmp = bitcast %"class.sc_core::sc_in"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp) nounwind
  ret void
}

define linkonce_odr %"class.sc_core::sc_interface"* @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13get_interfaceEv(%"class.sc_core::sc_port_b.68"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 1
  %tmp1 = load %"class.sc_core::sc_signal_in_if.48"** %tmp, align 8, !tbaa !2
  %tmp2 = icmp eq %"class.sc_core::sc_signal_in_if.48"* %tmp1, null
  br i1 %tmp2, label %bb12, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp1 to i8**
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = getelementptr i8* %tmp5, i64 -48
  %tmp7 = bitcast i8* %tmp6 to i64*
  %tmp8 = load i64* %tmp7, align 8
  %tmp9 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp1 to i8*
  %tmp10 = getelementptr i8* %tmp9, i64 %tmp8
  %tmp11 = bitcast i8* %tmp10 to %"class.sc_core::sc_interface"*
  br label %bb12

bb12:                                             ; preds = %bb3, %bb
  %tmp13 = phi %"class.sc_core::sc_interface"* [ %tmp11, %bb3 ], [ null, %bb ]
  ret %"class.sc_core::sc_interface"* %tmp13
}

define linkonce_odr %"class.sc_core::sc_interface"* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13get_interfaceEv(%"class.sc_core::sc_port_b.68"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 1
  %tmp1 = load %"class.sc_core::sc_signal_in_if.48"** %tmp, align 8, !tbaa !2
  %tmp2 = icmp eq %"class.sc_core::sc_signal_in_if.48"* %tmp1, null
  br i1 %tmp2, label %bb12, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp1 to i8**
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = getelementptr i8* %tmp5, i64 -48
  %tmp7 = bitcast i8* %tmp6 to i64*
  %tmp8 = load i64* %tmp7, align 8
  %tmp9 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp1 to i8*
  %tmp10 = getelementptr i8* %tmp9, i64 %tmp8
  %tmp11 = bitcast i8* %tmp10 to %"class.sc_core::sc_interface"*
  br label %bb12

bb12:                                             ; preds = %bb3, %bb
  %tmp13 = phi %"class.sc_core::sc_interface"* [ %tmp11, %bb3 ], [ null, %bb ]
  ret %"class.sc_core::sc_interface"* %tmp13
}

declare i32 @_ZN7sc_core5sc_inIbE5vbindERNS_12sc_interfaceE(%"class.sc_core::sc_in"*, %"class.sc_core::sc_interface"*)

declare i32 @_ZN7sc_core5sc_inIbE5vbindERNS_12sc_port_baseE(%"class.sc_core::sc_in"*, %"class.sc_core::sc_port_base"*)

define linkonce_odr void @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13add_interfaceEPNS_12sc_interfaceE(%"class.sc_core::sc_port_b.68"* %this, %"class.sc_core::sc_interface"* %interface_) uwtable align 2 {
bb:
  %iface = alloca %"class.sc_core::sc_signal_in_if.48"*, align 8
  %tmp = icmp eq %"class.sc_core::sc_interface"* %interface_, null
  br i1 %tmp, label %.thread, label %bb1

.thread:                                          ; preds = %bb
  store %"class.sc_core::sc_signal_in_if.48"* null, %"class.sc_core::sc_signal_in_if.48"** %iface, align 8, !tbaa !2
  br label %bb6

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_interface"* %interface_ to i8*
  %tmp3 = call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTIN7sc_core15sc_signal_in_ifIbEE to i8*), i64 -1)
  %tmp4 = bitcast i8* %tmp3 to %"class.sc_core::sc_signal_in_if.48"*
  store %"class.sc_core::sc_signal_in_if.48"* %tmp4, %"class.sc_core::sc_signal_in_if.48"** %iface, align 8, !tbaa !2
  %tmp5 = icmp eq i8* %tmp3, null
  br i1 %tmp5, label %bb6, label %bb7

bb6:                                              ; preds = %bb1, %.thread
  call void @__assert_fail(i8* getelementptr inbounds ([11 x i8]* @.str12, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str11, i64 0, i64 0), i32 580, i8* getelementptr inbounds ([143 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE13add_interfaceEPNS_12sc_interfaceE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb7:                                              ; preds = %bb1
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp10 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp9, align 8, !tbaa !2
  %tmp11 = getelementptr inbounds %"class.std::vector.69"* %tmp8, i64 0, i32 0, i32 0, i32 0
  %tmp12 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp11, align 8, !tbaa !2
  %tmp13 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp10 to i64
  %tmp14 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp12 to i64
  %tmp15 = sub i64 %tmp13, %tmp14
  %tmp16 = lshr exact i64 %tmp15, 3
  %tmp17 = trunc i64 %tmp16 to i32
  %tmp18 = icmp sgt i32 %tmp17, 0
  br i1 %tmp18, label %.lr.ph, label %bb27

.lr.ph:                                           ; preds = %bb7
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0
  br label %bb20

bb20:                                             ; preds = %._crit_edge2, %.lr.ph
  %tmp21 = phi %"class.sc_core::sc_signal_in_if.48"** [ %tmp12, %.lr.ph ], [ %.pre3, %._crit_edge2 ]
  %indvars.iv = phi i64 [ 0, %.lr.ph ], [ %indvars.iv.next, %._crit_edge2 ]
  %tmp22 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp21, i64 %indvars.iv
  %tmp23 = load %"class.sc_core::sc_signal_in_if.48"** %tmp22, align 8, !tbaa !2
  %tmp24 = icmp eq %"class.sc_core::sc_signal_in_if.48"* %tmp4, %tmp23
  br i1 %tmp24, label %bb25, label %bb26

bb25:                                             ; preds = %bb20
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp19, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core22SC_ID_BIND_IF_TO_PORT_E, i64 0, i64 0), i8* getelementptr inbounds ([32 x i8]* @.str13, i64 0, i64 0))
  br label %bb26

bb26:                                             ; preds = %bb25, %bb20
  %indvars.iv.next = add i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %tmp17
  br i1 %exitcond, label %._crit_edge, label %._crit_edge2

._crit_edge2:                                     ; preds = %bb26
  %.pre3 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp11, align 8, !tbaa !2
  br label %bb20

._crit_edge:                                      ; preds = %bb26
  %.pre = load %"class.sc_core::sc_signal_in_if.48"*** %tmp9, align 8, !tbaa !2
  br label %bb27

bb27:                                             ; preds = %._crit_edge, %bb7
  %tmp28 = phi %"class.sc_core::sc_signal_in_if.48"** [ %.pre, %._crit_edge ], [ %tmp10, %bb7 ]
  %tmp29 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 2
  %tmp30 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp29, align 8, !tbaa !2
  %tmp31 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp28, %tmp30
  br i1 %tmp31, label %bb37, label %bb32

bb32:                                             ; preds = %bb27
  %tmp33 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp28, null
  br i1 %tmp33, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit.i, label %bb34

bb34:                                             ; preds = %bb32
  store %"class.sc_core::sc_signal_in_if.48"* %tmp4, %"class.sc_core::sc_signal_in_if.48"** %tmp28, align 8, !tbaa !2
  %.pre.i = load %"class.sc_core::sc_signal_in_if.48"*** %tmp9, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit.i

_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit.i: ; preds = %bb34, %bb32
  %tmp35 = phi %"class.sc_core::sc_signal_in_if.48"** [ null, %bb32 ], [ %.pre.i, %bb34 ]
  %tmp36 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp35, i64 1
  store %"class.sc_core::sc_signal_in_if.48"** %tmp36, %"class.sc_core::sc_signal_in_if.48"*** %tmp9, align 8, !tbaa !2
  br label %_ZNSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE9push_backERKS3_.exit

bb37:                                             ; preds = %bb27
  call void @_ZNSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS3_S5_EERKS3_(%"class.std::vector.69"* %tmp8, %"class.sc_core::sc_signal_in_if.48"** %tmp28, %"class.sc_core::sc_signal_in_if.48"** %iface)
  br label %_ZNSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE9push_backERKS3_.exit

_ZNSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE9push_backERKS3_.exit: ; preds = %bb37, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit.i
  %tmp38 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp11, align 8, !tbaa !2
  %tmp39 = load %"class.sc_core::sc_signal_in_if.48"** %tmp38, align 8, !tbaa !2
  %tmp40 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 1
  store %"class.sc_core::sc_signal_in_if.48"* %tmp39, %"class.sc_core::sc_signal_in_if.48"** %tmp40, align 8, !tbaa !2
  ret void
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE15interface_countEv(%"class.sc_core::sc_port_b.68"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp1 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp, align 8, !tbaa !2
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp3 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp2, align 8, !tbaa !2
  %tmp4 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp1 to i64
  %tmp5 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp3 to i64
  %tmp6 = sub i64 %tmp4, %tmp5
  %tmp7 = lshr exact i64 %tmp6, 3
  %tmp8 = trunc i64 %tmp7 to i32
  ret i32 %tmp8
}

define linkonce_odr i8* @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE11if_typenameEv(%"class.sc_core::sc_port_b.68"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  ret i8* getelementptr inbounds ([31 x i8]* @_ZTSN7sc_core15sc_signal_in_ifIbEE, i64 0, i64 0)
}

declare void @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv(%"class.sc_core::sc_port_base"*)

declare void @_ZN7sc_core5sc_inIbE18end_of_elaborationEv(%"class.sc_core::sc_in"*)

declare void @_ZN7sc_core12sc_port_base19start_of_simulationEv(%"class.sc_core::sc_port_base"*)

declare void @_ZN7sc_core12sc_port_base17end_of_simulationEv(%"class.sc_core::sc_port_base"*)

define linkonce_odr void @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_b.68"* %this, %"class.sc_core::sc_thread_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0, i32 1
  %tmp2 = load %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb37

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp6 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp5, align 8, !tbaa !2
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp8 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp7, align 8, !tbaa !2
  %tmp9 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp6 to i64
  %tmp10 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp8 to i64
  %tmp11 = sub i64 %tmp9, %tmp10
  %tmp12 = lshr exact i64 %tmp11, 3
  %tmp13 = trunc i64 %tmp12 to i32
  br label %bb14

bb14:                                             ; preds = %bb23, %bb4
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb23 ], [ 0, %bb4 ]
  %tmp15 = trunc i64 %indvars.iv to i32
  %tmp16 = icmp slt i32 %tmp15, %tmp13
  br i1 %tmp16, label %bb17, label %.loopexit.loopexit

bb17:                                             ; preds = %bb14
  %tmp18 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp7, align 8, !tbaa !2
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp18, i64 %indvars.iv
  %tmp20 = load %"class.sc_core::sc_signal_in_if.48"** %tmp19, align 8, !tbaa !2
  %tmp21 = icmp eq %"class.sc_core::sc_signal_in_if.48"* %tmp20, null
  br i1 %tmp21, label %bb22, label %bb23

bb22:                                             ; preds = %bb17
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str10, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str11, i64 0, i64 0), i32 627, i8* getelementptr inbounds ([171 x i8]* @__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb23:                                             ; preds = %bb17
  %tmp24 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp20 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -48
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp20 to i8*
  %tmp30 = getelementptr i8* %tmp29, i64 %tmp28
  %tmp31 = bitcast i8* %tmp30 to %"class.sc_core::sc_interface"*
  %tmp32 = bitcast i8* %tmp30 to %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)***
  %tmp33 = load %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)*** %tmp32, align 8, !tbaa !0
  %tmp34 = getelementptr inbounds %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)** %tmp33, i64 1
  %tmp35 = load %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)** %tmp34, align 8
  %tmp36 = tail call %"class.sc_core::sc_event"* %tmp35(%"class.sc_core::sc_interface"* %tmp31)
  tail call void @_ZNK7sc_core12sc_port_base16add_static_eventEPNS_17sc_thread_processERKNS_8sc_eventE(%"class.sc_core::sc_port_base"* %tmp, %"class.sc_core::sc_thread_process"* %handle_p, %"class.sc_core::sc_event"* %tmp36)
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb14

bb37:                                             ; preds = %bb
  tail call void @_ZNK7sc_core12sc_port_base14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_base"* %tmp, %"class.sc_core::sc_thread_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_)
  br label %.loopexit

.loopexit.loopexit:                               ; preds = %bb14
  br label %.loopexit

.loopexit:                                        ; preds = %.loopexit.loopexit, %bb37
  ret void
}

define linkonce_odr void @_ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_b.68"* %this, %"class.sc_core::sc_method_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0, i32 1
  %tmp2 = load %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb37

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp6 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp5, align 8, !tbaa !2
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp8 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp7, align 8, !tbaa !2
  %tmp9 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp6 to i64
  %tmp10 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp8 to i64
  %tmp11 = sub i64 %tmp9, %tmp10
  %tmp12 = lshr exact i64 %tmp11, 3
  %tmp13 = trunc i64 %tmp12 to i32
  br label %bb14

bb14:                                             ; preds = %bb23, %bb4
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb23 ], [ 0, %bb4 ]
  %tmp15 = trunc i64 %indvars.iv to i32
  %tmp16 = icmp slt i32 %tmp15, %tmp13
  br i1 %tmp16, label %bb17, label %.loopexit.loopexit

bb17:                                             ; preds = %bb14
  %tmp18 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp7, align 8, !tbaa !2
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp18, i64 %indvars.iv
  %tmp20 = load %"class.sc_core::sc_signal_in_if.48"** %tmp19, align 8, !tbaa !2
  %tmp21 = icmp eq %"class.sc_core::sc_signal_in_if.48"* %tmp20, null
  br i1 %tmp21, label %bb22, label %bb23

bb22:                                             ; preds = %bb17
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str10, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str11, i64 0, i64 0), i32 648, i8* getelementptr inbounds ([171 x i8]* @__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb23:                                             ; preds = %bb17
  %tmp24 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp20 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -48
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp20 to i8*
  %tmp30 = getelementptr i8* %tmp29, i64 %tmp28
  %tmp31 = bitcast i8* %tmp30 to %"class.sc_core::sc_interface"*
  %tmp32 = bitcast i8* %tmp30 to %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)***
  %tmp33 = load %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)*** %tmp32, align 8, !tbaa !0
  %tmp34 = getelementptr inbounds %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)** %tmp33, i64 1
  %tmp35 = load %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)** %tmp34, align 8
  %tmp36 = tail call %"class.sc_core::sc_event"* %tmp35(%"class.sc_core::sc_interface"* %tmp31)
  tail call void @_ZNK7sc_core12sc_port_base16add_static_eventEPNS_17sc_method_processERKNS_8sc_eventE(%"class.sc_core::sc_port_base"* %tmp, %"class.sc_core::sc_method_process"* %handle_p, %"class.sc_core::sc_event"* %tmp36)
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb14

bb37:                                             ; preds = %bb
  tail call void @_ZNK7sc_core12sc_port_base14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_base"* %tmp, %"class.sc_core::sc_method_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_)
  br label %.loopexit

.loopexit.loopexit:                               ; preds = %bb14
  br label %.loopexit

.loopexit:                                        ; preds = %.loopexit.loopexit, %bb37
  ret void
}

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

declare void @_ZNK7sc_core12sc_port_base16add_static_eventEPNS_17sc_method_processERKNS_8sc_eventE(%"class.sc_core::sc_port_base"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event"*)

declare void @_ZNK7sc_core12sc_port_base14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_base"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event_finder"*)

declare void @_ZNK7sc_core12sc_port_base16add_static_eventEPNS_17sc_thread_processERKNS_8sc_eventE(%"class.sc_core::sc_port_base"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event"*)

declare void @_ZNK7sc_core12sc_port_base14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_base"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event_finder"*)

declare i8* @__dynamic_cast(i8*, i8*, i8*, i64)

declare void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"*, i8*, i8*)

define linkonce_odr void @_ZNSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS3_S5_EERKS3_(%"class.std::vector.69"* nocapture %this, %"class.sc_core::sc_signal_in_if.48"** %__position.coerce, %"class.sc_core::sc_signal_in_if.48"** nocapture %__x) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.std::vector.69"* %this, i64 0, i32 0, i32 0, i32 1
  %tmp1 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp, align 8, !tbaa !2
  %tmp2 = getelementptr inbounds %"class.std::vector.69"* %this, i64 0, i32 0, i32 0, i32 2
  %tmp3 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp2, align 8, !tbaa !2
  %tmp4 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp1, %tmp3
  br i1 %tmp4, label %_ZNKSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit, label %bb5

bb5:                                              ; preds = %bb
  %tmp6 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp1, null
  br i1 %tmp6, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit, label %bb7

bb7:                                              ; preds = %bb5
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp1, i64 -1
  %tmp9 = load %"class.sc_core::sc_signal_in_if.48"** %tmp8, align 8, !tbaa !2
  store %"class.sc_core::sc_signal_in_if.48"* %tmp9, %"class.sc_core::sc_signal_in_if.48"** %tmp1, align 8, !tbaa !2
  %.pre = load %"class.sc_core::sc_signal_in_if.48"*** %tmp, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit

_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit: ; preds = %bb7, %bb5
  %tmp10 = phi %"class.sc_core::sc_signal_in_if.48"** [ null, %bb5 ], [ %.pre, %bb7 ]
  %tmp11 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp10, i64 1
  store %"class.sc_core::sc_signal_in_if.48"** %tmp11, %"class.sc_core::sc_signal_in_if.48"*** %tmp, align 8, !tbaa !2
  %tmp12 = load %"class.sc_core::sc_signal_in_if.48"** %__x, align 8, !tbaa !2
  %tmp13 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp10, i64 -1
  %tmp14 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp13 to i64
  %tmp15 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %__position.coerce to i64
  %tmp16 = sub i64 %tmp14, %tmp15
  %tmp17 = ashr exact i64 %tmp16, 3
  %tmp18 = icmp eq i64 %tmp17, 0
  br i1 %tmp18, label %_ZSt13copy_backwardIPPN7sc_core15sc_signal_in_ifIbEES4_ET0_T_S6_S5_.exit, label %bb19

bb19:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit
  %.pre.i.i.i.i = sub i64 0, %tmp17
  %.pre1.i.i.i.i = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp10, i64 %.pre.i.i.i.i
  %tmp20 = bitcast %"class.sc_core::sc_signal_in_if.48"** %.pre1.i.i.i.i to i8*
  %tmp21 = bitcast %"class.sc_core::sc_signal_in_if.48"** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp20, i8* %tmp21, i64 %tmp16, i32 8, i1 false) nounwind
  br label %_ZSt13copy_backwardIPPN7sc_core15sc_signal_in_ifIbEES4_ET0_T_S6_S5_.exit

_ZSt13copy_backwardIPPN7sc_core15sc_signal_in_ifIbEES4_ET0_T_S6_S5_.exit: ; preds = %bb19, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit
  store %"class.sc_core::sc_signal_in_if.48"* %tmp12, %"class.sc_core::sc_signal_in_if.48"** %__position.coerce, align 8, !tbaa !2
  br label %bb67

_ZNKSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit: ; preds = %bb
  %tmp22 = getelementptr inbounds %"class.std::vector.69"* %this, i64 0, i32 0, i32 0, i32 0
  %tmp23 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp22, align 8, !tbaa !2
  %tmp24 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp1 to i64
  %tmp25 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %tmp23 to i64
  %tmp26 = sub i64 %tmp24, %tmp25
  %tmp27 = ashr exact i64 %tmp26, 3
  %tmp28 = icmp eq i64 %tmp27, 0
  %tmp29 = select i1 %tmp28, i64 1, i64 %tmp27
  %uadd.i = tail call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %tmp27, i64 %tmp29)
  %tmp30 = extractvalue { i64, i1 } %uadd.i, 0
  %tmp31 = extractvalue { i64, i1 } %uadd.i, 1
  %tmp32 = icmp ugt i64 %tmp30, 2305843009213693951
  %or.cond.i = or i1 %tmp31, %tmp32
  %tmp33 = select i1 %or.cond.i, i64 2305843009213693951, i64 %tmp30
  %tmp34 = icmp eq i64 %tmp33, 0
  br i1 %tmp34, label %_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE11_M_allocateEm.exit, label %bb35

bb35:                                             ; preds = %_ZNKSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit
  %tmp36 = icmp ugt i64 %tmp33, 2305843009213693951
  br i1 %tmp36, label %bb37, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE8allocateEmPKv.exit.i

bb37:                                             ; preds = %bb35
  tail call void @_ZSt17__throw_bad_allocv() noreturn
  unreachable

_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE8allocateEmPKv.exit.i: ; preds = %bb35
  %tmp38 = shl i64 %tmp33, 3
  %tmp39 = tail call noalias i8* @_Znwm(i64 %tmp38)
  %tmp40 = bitcast i8* %tmp39 to %"class.sc_core::sc_signal_in_if.48"**
  br label %_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE11_M_allocateEm.exit

_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE11_M_allocateEm.exit: ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE8allocateEmPKv.exit.i, %_ZNKSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit
  %tmp41 = phi %"class.sc_core::sc_signal_in_if.48"** [ %tmp40, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE8allocateEmPKv.exit.i ], [ null, %_ZNKSt6vectorIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit ]
  %tmp42 = ptrtoint %"class.sc_core::sc_signal_in_if.48"** %__position.coerce to i64
  %tmp43 = sub i64 %tmp42, %tmp25
  %tmp44 = ashr exact i64 %tmp43, 3
  %tmp45 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp41, i64 %tmp44
  %tmp46 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp45, null
  br i1 %tmp46, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit3, label %bb47

bb47:                                             ; preds = %_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE11_M_allocateEm.exit
  %tmp48 = load %"class.sc_core::sc_signal_in_if.48"** %__x, align 8, !tbaa !2
  store %"class.sc_core::sc_signal_in_if.48"* %tmp48, %"class.sc_core::sc_signal_in_if.48"** %tmp45, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit3

_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit3: ; preds = %bb47, %_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE11_M_allocateEm.exit
  %tmp49 = icmp eq i64 %tmp44, 0
  br i1 %tmp49, label %bb53, label %bb50

bb50:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit3
  %tmp51 = bitcast %"class.sc_core::sc_signal_in_if.48"** %tmp41 to i8*
  %tmp52 = bitcast %"class.sc_core::sc_signal_in_if.48"** %tmp23 to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp51, i8* %tmp52, i64 %tmp43, i32 8, i1 false) nounwind
  br label %bb53

bb53:                                             ; preds = %bb50, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core15sc_signal_in_ifIbEEE9constructEPS4_RKS4_.exit3
  %.sum = add i64 %tmp44, 1
  %tmp54 = sub i64 %tmp24, %tmp42
  %tmp55 = ashr exact i64 %tmp54, 3
  %tmp56 = icmp eq i64 %tmp55, 0
  br i1 %tmp56, label %bb61, label %bb57

bb57:                                             ; preds = %bb53
  %tmp58 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp41, i64 %.sum
  %tmp59 = bitcast %"class.sc_core::sc_signal_in_if.48"** %tmp58 to i8*
  %tmp60 = bitcast %"class.sc_core::sc_signal_in_if.48"** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp59, i8* %tmp60, i64 %tmp54, i32 8, i1 false) nounwind
  br label %bb61

bb61:                                             ; preds = %bb57, %bb53
  %tmp62 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp23, null
  br i1 %tmp62, label %_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE13_M_deallocateEPS3_m.exit1, label %bb63

bb63:                                             ; preds = %bb61
  %tmp64 = bitcast %"class.sc_core::sc_signal_in_if.48"** %tmp23 to i8*
  tail call void @_ZdlPv(i8* %tmp64) nounwind
  br label %_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE13_M_deallocateEPS3_m.exit1

_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE13_M_deallocateEPS3_m.exit1: ; preds = %bb63, %bb61
  %.sum4 = add i64 %tmp55, %.sum
  %tmp65 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp41, i64 %.sum4
  store %"class.sc_core::sc_signal_in_if.48"** %tmp41, %"class.sc_core::sc_signal_in_if.48"*** %tmp22, align 8, !tbaa !2
  store %"class.sc_core::sc_signal_in_if.48"** %tmp65, %"class.sc_core::sc_signal_in_if.48"*** %tmp, align 8, !tbaa !2
  %tmp66 = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"** %tmp41, i64 %tmp33
  store %"class.sc_core::sc_signal_in_if.48"** %tmp66, %"class.sc_core::sc_signal_in_if.48"*** %tmp2, align 8, !tbaa !2
  br label %bb67

bb67:                                             ; preds = %_ZNSt12_Vector_baseIPN7sc_core15sc_signal_in_ifIbEESaIS3_EE13_M_deallocateEPS3_m.exit1, %_ZSt13copy_backwardIPPN7sc_core15sc_signal_in_ifIbEES4_ET0_T_S6_S5_.exit
  ret void
}

declare void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"*)

define linkonce_odr i8* @_ZNK7sc_core12sc_port_base4kindEv(%"class.sc_core::sc_port_base"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([13 x i8]* @.str15, i64 0, i64 0)
}

define linkonce_odr void @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED1Ev(%"class.sc_core::sc_port_b.68"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED2Ev.exit, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %"class.sc_core::sc_signal_in_if.48"** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED2Ev.exit

_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED2Ev.exit: ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  ret void
}

define linkonce_odr void @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED0Ev(%"class.sc_core::sc_port_b.68"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %"class.sc_core::sc_signal_in_if.48"*** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq %"class.sc_core::sc_signal_in_if.48"** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED2Ev.exit.i, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %"class.sc_core::sc_signal_in_if.48"** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED2Ev.exit.i

_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEED2Ev.exit.i: ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  %tmp7 = bitcast %"class.sc_core::sc_port_b.68"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  ret void
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE5vbindERNS_12sc_interfaceE(%"class.sc_core::sc_port_b.68"* %this, %"class.sc_core::sc_interface"* %interface_) uwtable align 2 {
bb:
  %tmp = icmp eq %"class.sc_core::sc_interface"* %interface_, null
  br i1 %tmp, label %.thread, label %bb1

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_interface"* %interface_ to i8*
  %tmp3 = tail call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTIN7sc_core15sc_signal_in_ifIbEE to i8*), i64 -1)
  %tmp4 = icmp eq i8* %tmp3, null
  br i1 %tmp4, label %.thread, label %bb5

bb5:                                              ; preds = %bb1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0
  %tmp7 = bitcast i8* %tmp3 to i8**
  %tmp8 = load i8** %tmp7, align 8, !tbaa !0
  %tmp9 = getelementptr i8* %tmp8, i64 -48
  %tmp10 = bitcast i8* %tmp9 to i64*
  %tmp11 = load i64* %tmp10, align 8
  %tmp12 = getelementptr i8* %tmp3, i64 %tmp11
  %tmp13 = bitcast i8* %tmp12 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_port_base4bindERNS_12sc_interfaceE(%"class.sc_core::sc_port_base"* %tmp6, %"class.sc_core::sc_interface"* %tmp13)
  br label %.thread

.thread:                                          ; preds = %bb5, %bb1, %bb
  %.0 = phi i32 [ 0, %bb5 ], [ 2, %bb1 ], [ 2, %bb ]
  ret i32 %.0
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEE5vbindERNS_12sc_port_baseE(%"class.sc_core::sc_port_b.68"* %this, %"class.sc_core::sc_port_base"* %parent_) uwtable align 2 {
bb:
  %tmp = icmp eq %"class.sc_core::sc_port_base"* %parent_, null
  br i1 %tmp, label %.thread, label %bb1

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_port_base"* %parent_ to i8*
  %tmp3 = tail call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core12sc_port_baseE to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bINS_15sc_signal_in_ifIbEEEE to i8*), i64 -1)
  %tmp4 = icmp eq i8* %tmp3, null
  br i1 %tmp4, label %.thread, label %bb5

bb5:                                              ; preds = %bb1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b.68"* %this, i64 0, i32 0
  %tmp7 = bitcast i8* %tmp3 to %"class.sc_core::sc_port_base"*
  tail call void @_ZN7sc_core12sc_port_base4bindERS0_(%"class.sc_core::sc_port_base"* %tmp6, %"class.sc_core::sc_port_base"* %tmp7)
  br label %.thread

.thread:                                          ; preds = %bb5, %bb1, %bb
  %.0 = phi i32 [ 0, %bb5 ], [ 2, %bb1 ], [ 2, %bb ]
  ret i32 %.0
}

declare void @_ZN7sc_core12sc_port_base18end_of_elaborationEv(%"class.sc_core::sc_port_base"*)

declare void @_ZN7sc_core12sc_port_base4bindERS0_(%"class.sc_core::sc_port_base"*, %"class.sc_core::sc_port_base"*)

declare void @_ZN7sc_core8sc_startERKNS_7sc_timeE(%"class.sc_core::sc_time"*)

declare void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitE(%"class.sc_core::sc_time"*, double, i32)

define linkonce_odr void @_ZN6TargetC2EN7sc_core14sc_module_nameE(%class.Target* %this, %"class.sc_core::sc_module_name"* nocapture %arg) unnamed_addr uwtable align 2 {
bb:
  %compute_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = getelementptr inbounds %class.Target* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2Ev(%"class.sc_core::sc_module"* %tmp3)
  %tmp4 = getelementptr inbounds %class.Target* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !0
  %tmp5 = getelementptr %class.Target* %this, i64 0, i32 0, i32 1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp5, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Target, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp6, align 8, !tbaa !0
  %tmp7 = getelementptr inbounds %class.Target* %this, i64 0, i32 1
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_in"* %tmp7, i64 0, i32 0, i32 0, i32 0
  call void @_ZN7sc_core12sc_port_baseC2EiNS_14sc_port_policyE(%"class.sc_core::sc_port_base"* %tmp8, i32 1, i32 0)
  %tmp9 = getelementptr inbounds %class.Target* %this, i64 0, i32 1, i32 0, i32 0, i32 1
  %tmp10 = bitcast %"class.sc_core::sc_signal_in_if.48"** %tmp9 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp10, i8 0, i64 32, i32 8, i1 false)
  %tmp11 = getelementptr inbounds %"class.sc_core::sc_in"* %tmp7, i64 0, i32 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core5sc_inIbEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp11, align 8, !tbaa !0
  %tmp12 = getelementptr inbounds %class.Target* %this, i64 0, i32 1, i32 1
  %tmp13 = bitcast %"class.std::vector.62"** %tmp12 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp13, i8 0, i64 32, i32 8, i1 false)
  %tmp14 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp15 = icmp eq %"class.sc_core::sc_simcontext"* %tmp14, null
  br i1 %tmp15, label %.noexc, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc:                                           ; preds = %bb
  %tmp16 = call noalias i8* @_Znwm(i64 248)
  %tmp17 = bitcast i8* %tmp16 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp17)
  store %"class.sc_core::sc_simcontext"* %tmp17, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp17, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc, %bb
  %tmp18 = phi %"class.sc_core::sc_simcontext"* [ %tmp17, %.noexc ], [ %tmp14, %bb ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %compute_handle, %"class.sc_core::sc_simcontext"* %tmp18, i8* getelementptr inbounds ([8 x i8]* @.str16, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.Target*)* @_ZN6Target7computeEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp5, %"class.sc_core::sc_spawn_options"* null)
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %compute_handle, i64 0, i32 0
  %tmp20 = load %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !2
  %tmp21 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp20, %"class.sc_core::sc_process_b"** %tmp21, align 8, !tbaa !2
  %tmp22 = icmp eq %"class.sc_core::sc_process_b"* %tmp20, null
  br i1 %tmp22, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb23

bb23:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp24 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp20, i64 0, i32 15
  %tmp25 = load i32* %tmp24, align 4, !tbaa !6
  %tmp26 = icmp eq i32 %tmp25, 0
  br i1 %tmp26, label %bb27, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb27:                                             ; preds = %bb23
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb23
  %tmp28 = add nsw i32 %tmp25, 1
  store i32 %tmp28, i32* %tmp24, align 4, !tbaa !6
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp29 = getelementptr inbounds %class.Target* %this, i64 0, i32 0, i32 2
  %tmp30 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp29, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp31 = load %"class.sc_core::sc_process_b"** %tmp21, align 8, !tbaa !2
  %tmp32 = icmp eq %"class.sc_core::sc_process_b"* %tmp31, null
  br i1 %tmp32, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb33

bb33:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp34 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp31, i64 0, i32 15
  %tmp35 = load i32* %tmp34, align 4, !tbaa !6
  %tmp36 = add nsw i32 %tmp35, -1
  store i32 %tmp36, i32* %tmp34, align 4, !tbaa !6
  %tmp37 = icmp eq i32 %tmp36, 0
  br i1 %tmp37, label %bb38, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb38:                                             ; preds = %bb33
  %tmp39 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  %tmp40 = icmp eq %"class.sc_core::sc_process_b"* %tmp39, null
  br i1 %tmp40, label %bb45, label %.noexc6

.noexc6:                                          ; preds = %bb38
  %tmp41 = bitcast %"class.sc_core::sc_process_b"* %tmp39 to void (%"class.sc_core::sc_process_b"*)***
  %tmp42 = load void (%"class.sc_core::sc_process_b"*)*** %tmp41, align 8, !tbaa !0
  %tmp43 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp42, i64 6
  %tmp44 = load void (%"class.sc_core::sc_process_b"*)** %tmp43, align 8
  call void %tmp44(%"class.sc_core::sc_process_b"* %tmp39)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %bb45

bb45:                                             ; preds = %.noexc6, %bb38
  %tmp46 = phi %"class.sc_core::sc_process_b"* [ null, %bb38 ], [ %.pre.i.i.i, %.noexc6 ]
  %tmp47 = icmp eq %"class.sc_core::sc_process_b"* %tmp46, %tmp31
  br i1 %tmp47, label %bb48, label %bb49

bb48:                                             ; preds = %bb45
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb49:                                             ; preds = %bb45
  store %"class.sc_core::sc_process_b"* %tmp31, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb49, %bb33, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp50 = load %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !2
  %tmp51 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp50, %"class.sc_core::sc_process_b"** %tmp51, align 8, !tbaa !2
  %tmp52 = icmp eq %"class.sc_core::sc_process_b"* %tmp50, null
  br i1 %tmp52, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8, label %bb53

bb53:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp54 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp50, i64 0, i32 15
  %tmp55 = load i32* %tmp54, align 4, !tbaa !6
  %tmp56 = icmp eq i32 %tmp55, 0
  br i1 %tmp56, label %bb57, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7

bb57:                                             ; preds = %bb53
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7: ; preds = %bb53
  %tmp58 = add nsw i32 %tmp55, 1
  store i32 %tmp58, i32* %tmp54, align 4, !tbaa !6
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8

_ZN7sc_core17sc_process_handleC1ERKS0_.exit8:     ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp59 = getelementptr inbounds %class.Target* %this, i64 0, i32 0, i32 3
  %tmp60 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp59, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp61 = load %"class.sc_core::sc_process_b"** %tmp51, align 8, !tbaa !2
  %tmp62 = icmp eq %"class.sc_core::sc_process_b"* %tmp61, null
  br i1 %tmp62, label %_ZN7sc_core17sc_process_handleD1Ev.exit11, label %bb63

bb63:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8
  %tmp64 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp61, i64 0, i32 15
  %tmp65 = load i32* %tmp64, align 4, !tbaa !6
  %tmp66 = add nsw i32 %tmp65, -1
  store i32 %tmp66, i32* %tmp64, align 4, !tbaa !6
  %tmp67 = icmp eq i32 %tmp66, 0
  br i1 %tmp67, label %bb68, label %_ZN7sc_core17sc_process_handleD1Ev.exit11

bb68:                                             ; preds = %bb63
  %tmp69 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  %tmp70 = icmp eq %"class.sc_core::sc_process_b"* %tmp69, null
  br i1 %tmp70, label %bb75, label %.noexc10

.noexc10:                                         ; preds = %bb68
  %tmp71 = bitcast %"class.sc_core::sc_process_b"* %tmp69 to void (%"class.sc_core::sc_process_b"*)***
  %tmp72 = load void (%"class.sc_core::sc_process_b"*)*** %tmp71, align 8, !tbaa !0
  %tmp73 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp72, i64 6
  %tmp74 = load void (%"class.sc_core::sc_process_b"*)** %tmp73, align 8
  call void %tmp74(%"class.sc_core::sc_process_b"* %tmp69)
  %.pre.i.i.i9 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %bb75

bb75:                                             ; preds = %.noexc10, %bb68
  %tmp76 = phi %"class.sc_core::sc_process_b"* [ null, %bb68 ], [ %.pre.i.i.i9, %.noexc10 ]
  %tmp77 = icmp eq %"class.sc_core::sc_process_b"* %tmp76, %tmp61
  br i1 %tmp77, label %bb78, label %bb79

bb78:                                             ; preds = %bb75
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb79:                                             ; preds = %bb75
  store %"class.sc_core::sc_process_b"* %tmp61, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit11

_ZN7sc_core17sc_process_handleD1Ev.exit11:        ; preds = %bb79, %bb63, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8
  %tmp80 = load %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !2
  %tmp81 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp80, %"class.sc_core::sc_process_b"** %tmp81, align 8, !tbaa !2
  %tmp82 = icmp eq %"class.sc_core::sc_process_b"* %tmp80, null
  br i1 %tmp82, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13, label %bb83

bb83:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit11
  %tmp84 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp80, i64 0, i32 15
  %tmp85 = load i32* %tmp84, align 4, !tbaa !6
  %tmp86 = icmp eq i32 %tmp85, 0
  br i1 %tmp86, label %bb87, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12

bb87:                                             ; preds = %bb83
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12: ; preds = %bb83
  %tmp88 = add nsw i32 %tmp85, 1
  store i32 %tmp88, i32* %tmp84, align 4, !tbaa !6
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13

_ZN7sc_core17sc_process_handleC1ERKS0_.exit13:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12, %_ZN7sc_core17sc_process_handleD1Ev.exit11
  %tmp89 = getelementptr inbounds %class.Target* %this, i64 0, i32 0, i32 4
  %tmp90 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp89, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp91 = load %"class.sc_core::sc_process_b"** %tmp81, align 8, !tbaa !2
  %tmp92 = icmp eq %"class.sc_core::sc_process_b"* %tmp91, null
  br i1 %tmp92, label %_ZN7sc_core17sc_process_handleD1Ev.exit16, label %bb93

bb93:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp94 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp91, i64 0, i32 15
  %tmp95 = load i32* %tmp94, align 4, !tbaa !6
  %tmp96 = add nsw i32 %tmp95, -1
  store i32 %tmp96, i32* %tmp94, align 4, !tbaa !6
  %tmp97 = icmp eq i32 %tmp96, 0
  br i1 %tmp97, label %bb98, label %_ZN7sc_core17sc_process_handleD1Ev.exit16

bb98:                                             ; preds = %bb93
  %tmp99 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  %tmp100 = icmp eq %"class.sc_core::sc_process_b"* %tmp99, null
  br i1 %tmp100, label %bb105, label %.noexc15

.noexc15:                                         ; preds = %bb98
  %tmp101 = bitcast %"class.sc_core::sc_process_b"* %tmp99 to void (%"class.sc_core::sc_process_b"*)***
  %tmp102 = load void (%"class.sc_core::sc_process_b"*)*** %tmp101, align 8, !tbaa !0
  %tmp103 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp102, i64 6
  %tmp104 = load void (%"class.sc_core::sc_process_b"*)** %tmp103, align 8
  call void %tmp104(%"class.sc_core::sc_process_b"* %tmp99)
  %.pre.i.i.i14 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %bb105

bb105:                                            ; preds = %.noexc15, %bb98
  %tmp106 = phi %"class.sc_core::sc_process_b"* [ null, %bb98 ], [ %.pre.i.i.i14, %.noexc15 ]
  %tmp107 = icmp eq %"class.sc_core::sc_process_b"* %tmp106, %tmp91
  br i1 %tmp107, label %bb108, label %bb109

bb108:                                            ; preds = %bb105
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb109:                                            ; preds = %bb105
  store %"class.sc_core::sc_process_b"* %tmp91, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit16

_ZN7sc_core17sc_process_handleD1Ev.exit16:        ; preds = %bb109, %bb93, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp110 = load %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !2
  %tmp111 = icmp eq %"class.sc_core::sc_process_b"* %tmp110, null
  br i1 %tmp111, label %_ZN7sc_core17sc_process_handleD1Ev.exit19, label %bb112

bb112:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit16
  %tmp113 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp110, i64 0, i32 15
  %tmp114 = load i32* %tmp113, align 4, !tbaa !6
  %tmp115 = add nsw i32 %tmp114, -1
  store i32 %tmp115, i32* %tmp113, align 4, !tbaa !6
  %tmp116 = icmp eq i32 %tmp115, 0
  br i1 %tmp116, label %bb117, label %_ZN7sc_core17sc_process_handleD1Ev.exit19

bb117:                                            ; preds = %bb112
  %tmp118 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  %tmp119 = icmp eq %"class.sc_core::sc_process_b"* %tmp118, null
  br i1 %tmp119, label %bb124, label %.noexc18

.noexc18:                                         ; preds = %bb117
  %tmp120 = bitcast %"class.sc_core::sc_process_b"* %tmp118 to void (%"class.sc_core::sc_process_b"*)***
  %tmp121 = load void (%"class.sc_core::sc_process_b"*)*** %tmp120, align 8, !tbaa !0
  %tmp122 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp121, i64 6
  %tmp123 = load void (%"class.sc_core::sc_process_b"*)** %tmp122, align 8
  call void %tmp123(%"class.sc_core::sc_process_b"* %tmp118)
  %.pre.i.i.i17 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %bb124

bb124:                                            ; preds = %.noexc18, %bb117
  %tmp125 = phi %"class.sc_core::sc_process_b"* [ null, %bb117 ], [ %.pre.i.i.i17, %.noexc18 ]
  %tmp126 = icmp eq %"class.sc_core::sc_process_b"* %tmp125, %tmp110
  br i1 %tmp126, label %bb127, label %bb128

bb127:                                            ; preds = %bb124
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb128:                                            ; preds = %bb124
  store %"class.sc_core::sc_process_b"* %tmp110, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit19

_ZN7sc_core17sc_process_handleD1Ev.exit19:        ; preds = %bb128, %bb112, %_ZN7sc_core17sc_process_handleD1Ev.exit16
  ret void
}

declare void @_ZN7sc_core9sc_moduleC2Ev(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

define linkonce_odr void @_ZN6Target7computeEv(%class.Target* %this) uwtable align 2 {
bb:
  %tmp = alloca %"class.sc_core::sc_time", align 8
  %tmp1 = getelementptr inbounds %class.Target* %this, i64 0, i32 1, i32 0, i32 0, i32 1
  %tmp2 = getelementptr inbounds %class.Target* %this, i64 0, i32 1, i32 0, i32 0, i32 0
  %tmp3 = bitcast %"class.sc_core::sc_time"* %tmp to i8*
  %tmp4 = getelementptr inbounds %class.Target* %this, i64 0, i32 0, i32 0, i32 1
  br label %bb5

bb5:                                              ; preds = %bb18, %bb
  %i.0 = phi i32 [ 0, %bb ], [ %tmp21, %bb18 ]
  %tmp6 = load %"class.sc_core::sc_signal_in_if.48"** %tmp1, align 8, !tbaa !2
  %tmp7 = icmp eq %"class.sc_core::sc_signal_in_if.48"* %tmp6, null
  br i1 %tmp7, label %bb8, label %_ZNK7sc_core5sc_inIbE4readEv.exit

bb8:                                              ; preds = %bb5
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp2, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core13SC_ID_GET_IF_E, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8]* @.str20, i64 0, i64 0))
  %.pre.i.i = load %"class.sc_core::sc_signal_in_if.48"** %tmp1, align 8, !tbaa !2
  br label %_ZNK7sc_core5sc_inIbE4readEv.exit

_ZNK7sc_core5sc_inIbE4readEv.exit:                ; preds = %bb8, %bb5
  %tmp9 = phi %"class.sc_core::sc_signal_in_if.48"* [ %.pre.i.i, %bb8 ], [ %tmp6, %bb5 ]
  %tmp10 = bitcast %"class.sc_core::sc_signal_in_if.48"* %tmp9 to i8* (%"class.sc_core::sc_signal_in_if.48"*)***
  %tmp11 = load i8* (%"class.sc_core::sc_signal_in_if.48"*)*** %tmp10, align 8, !tbaa !0
  %tmp12 = getelementptr inbounds i8* (%"class.sc_core::sc_signal_in_if.48"*)** %tmp11, i64 7
  %tmp13 = load i8* (%"class.sc_core::sc_signal_in_if.48"*)** %tmp12, align 8
  %tmp14 = call i8* %tmp13(%"class.sc_core::sc_signal_in_if.48"* %tmp9)
  %tmp15 = load i8* %tmp14, align 1, !tbaa !4, !range !7
  %tmp16 = icmp ne i8 %tmp15, 0
  %tmp17 = icmp slt i32 %i.0, 10
  %or.cond = and i1 %tmp16, %tmp17
  br i1 %or.cond, label %bb18, label %.critedge

bb18:                                             ; preds = %_ZNK7sc_core5sc_inIbE4readEv.exit
  call void @llvm.lifetime.start(i64 -1, i8* %tmp3)
  %tmp19 = load %"class.sc_core::sc_simcontext"** %tmp4, align 8, !tbaa !2
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, double 0.000000e+00, i32 2, %"class.sc_core::sc_simcontext"* %tmp19)
  %tmp20 = load %"class.sc_core::sc_simcontext"** %tmp4, align 8, !tbaa !2
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, %"class.sc_core::sc_simcontext"* %tmp20)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp3)
  %tmp21 = add nsw i32 %i.0, 1
  br label %bb5

.critedge:                                        ; preds = %_ZNK7sc_core5sc_inIbE4readEv.exit
  ret void
}

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

declare void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, double, i32, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core12sc_port_baseC2EiNS_14sc_port_policyE(%"class.sc_core::sc_port_base"*, i32, i32)

define linkonce_odr void @_ZN6SourceC2EN7sc_core14sc_module_nameE(%class.Source* %this, %"class.sc_core::sc_module_name"* nocapture %arg) unnamed_addr uwtable align 2 {
bb:
  %compute_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = getelementptr inbounds %class.Source* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2Ev(%"class.sc_core::sc_module"* %tmp3)
  %tmp4 = getelementptr inbounds %class.Source* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !0
  %tmp5 = getelementptr %class.Source* %this, i64 0, i32 0, i32 1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp5, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp6, align 8, !tbaa !0
  %tmp7 = getelementptr inbounds %class.Source* %this, i64 0, i32 1
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_out"* %tmp7, i64 0, i32 0, i32 0, i32 0, i32 0
  call void @_ZN7sc_core12sc_port_baseC2EiNS_14sc_port_policyE(%"class.sc_core::sc_port_base"* %tmp8, i32 1, i32 0)
  %tmp9 = getelementptr inbounds %class.Source* %this, i64 0, i32 1, i32 0, i32 0, i32 0, i32 1
  %tmp10 = getelementptr inbounds %"class.sc_core::sc_out"* %tmp7, i64 0, i32 0, i32 0, i32 0, i32 0, i32 0, i32 0
  %tmp11 = bitcast %"class.sc_core::sc_signal_inout_if.55"** %tmp9 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp11, i8 0, i64 72, i32 8, i1 false)
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core6sc_outIbEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp10, align 8, !tbaa !0
  %tmp12 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp13 = icmp eq %"class.sc_core::sc_simcontext"* %tmp12, null
  br i1 %tmp13, label %.noexc, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc:                                           ; preds = %bb
  %tmp14 = call noalias i8* @_Znwm(i64 248)
  %tmp15 = bitcast i8* %tmp14 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp15)
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc, %bb
  %tmp16 = phi %"class.sc_core::sc_simcontext"* [ %tmp15, %.noexc ], [ %tmp12, %bb ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %compute_handle, %"class.sc_core::sc_simcontext"* %tmp16, i8* getelementptr inbounds ([8 x i8]* @.str16, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.Source*)* @_ZN6Source7computeEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp5, %"class.sc_core::sc_spawn_options"* null)
  %tmp17 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %compute_handle, i64 0, i32 0
  %tmp18 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !2
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp18, %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !2
  %tmp20 = icmp eq %"class.sc_core::sc_process_b"* %tmp18, null
  br i1 %tmp20, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb21

bb21:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp22 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp18, i64 0, i32 15
  %tmp23 = load i32* %tmp22, align 4, !tbaa !6
  %tmp24 = icmp eq i32 %tmp23, 0
  br i1 %tmp24, label %bb25, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb25:                                             ; preds = %bb21
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb21
  %tmp26 = add nsw i32 %tmp23, 1
  store i32 %tmp26, i32* %tmp22, align 4, !tbaa !6
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp27 = getelementptr inbounds %class.Source* %this, i64 0, i32 0, i32 2
  %tmp28 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp27, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp29 = load %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !2
  %tmp30 = icmp eq %"class.sc_core::sc_process_b"* %tmp29, null
  br i1 %tmp30, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb31

bb31:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp32 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp29, i64 0, i32 15
  %tmp33 = load i32* %tmp32, align 4, !tbaa !6
  %tmp34 = add nsw i32 %tmp33, -1
  store i32 %tmp34, i32* %tmp32, align 4, !tbaa !6
  %tmp35 = icmp eq i32 %tmp34, 0
  br i1 %tmp35, label %bb36, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb36:                                             ; preds = %bb31
  %tmp37 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  %tmp38 = icmp eq %"class.sc_core::sc_process_b"* %tmp37, null
  br i1 %tmp38, label %bb43, label %.noexc6

.noexc6:                                          ; preds = %bb36
  %tmp39 = bitcast %"class.sc_core::sc_process_b"* %tmp37 to void (%"class.sc_core::sc_process_b"*)***
  %tmp40 = load void (%"class.sc_core::sc_process_b"*)*** %tmp39, align 8, !tbaa !0
  %tmp41 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp40, i64 6
  %tmp42 = load void (%"class.sc_core::sc_process_b"*)** %tmp41, align 8
  call void %tmp42(%"class.sc_core::sc_process_b"* %tmp37)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %bb43

bb43:                                             ; preds = %.noexc6, %bb36
  %tmp44 = phi %"class.sc_core::sc_process_b"* [ null, %bb36 ], [ %.pre.i.i.i, %.noexc6 ]
  %tmp45 = icmp eq %"class.sc_core::sc_process_b"* %tmp44, %tmp29
  br i1 %tmp45, label %bb46, label %bb47

bb46:                                             ; preds = %bb43
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb47:                                             ; preds = %bb43
  store %"class.sc_core::sc_process_b"* %tmp29, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb47, %bb31, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp48 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !2
  %tmp49 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp48, %"class.sc_core::sc_process_b"** %tmp49, align 8, !tbaa !2
  %tmp50 = icmp eq %"class.sc_core::sc_process_b"* %tmp48, null
  br i1 %tmp50, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8, label %bb51

bb51:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp52 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp48, i64 0, i32 15
  %tmp53 = load i32* %tmp52, align 4, !tbaa !6
  %tmp54 = icmp eq i32 %tmp53, 0
  br i1 %tmp54, label %bb55, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7

bb55:                                             ; preds = %bb51
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7: ; preds = %bb51
  %tmp56 = add nsw i32 %tmp53, 1
  store i32 %tmp56, i32* %tmp52, align 4, !tbaa !6
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8

_ZN7sc_core17sc_process_handleC1ERKS0_.exit8:     ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp57 = getelementptr inbounds %class.Source* %this, i64 0, i32 0, i32 3
  %tmp58 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp57, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp59 = load %"class.sc_core::sc_process_b"** %tmp49, align 8, !tbaa !2
  %tmp60 = icmp eq %"class.sc_core::sc_process_b"* %tmp59, null
  br i1 %tmp60, label %_ZN7sc_core17sc_process_handleD1Ev.exit11, label %bb61

bb61:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8
  %tmp62 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp59, i64 0, i32 15
  %tmp63 = load i32* %tmp62, align 4, !tbaa !6
  %tmp64 = add nsw i32 %tmp63, -1
  store i32 %tmp64, i32* %tmp62, align 4, !tbaa !6
  %tmp65 = icmp eq i32 %tmp64, 0
  br i1 %tmp65, label %bb66, label %_ZN7sc_core17sc_process_handleD1Ev.exit11

bb66:                                             ; preds = %bb61
  %tmp67 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  %tmp68 = icmp eq %"class.sc_core::sc_process_b"* %tmp67, null
  br i1 %tmp68, label %bb73, label %.noexc10

.noexc10:                                         ; preds = %bb66
  %tmp69 = bitcast %"class.sc_core::sc_process_b"* %tmp67 to void (%"class.sc_core::sc_process_b"*)***
  %tmp70 = load void (%"class.sc_core::sc_process_b"*)*** %tmp69, align 8, !tbaa !0
  %tmp71 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp70, i64 6
  %tmp72 = load void (%"class.sc_core::sc_process_b"*)** %tmp71, align 8
  call void %tmp72(%"class.sc_core::sc_process_b"* %tmp67)
  %.pre.i.i.i9 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %bb73

bb73:                                             ; preds = %.noexc10, %bb66
  %tmp74 = phi %"class.sc_core::sc_process_b"* [ null, %bb66 ], [ %.pre.i.i.i9, %.noexc10 ]
  %tmp75 = icmp eq %"class.sc_core::sc_process_b"* %tmp74, %tmp59
  br i1 %tmp75, label %bb76, label %bb77

bb76:                                             ; preds = %bb73
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb77:                                             ; preds = %bb73
  store %"class.sc_core::sc_process_b"* %tmp59, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit11

_ZN7sc_core17sc_process_handleD1Ev.exit11:        ; preds = %bb77, %bb61, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8
  %tmp78 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !2
  %tmp79 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp78, %"class.sc_core::sc_process_b"** %tmp79, align 8, !tbaa !2
  %tmp80 = icmp eq %"class.sc_core::sc_process_b"* %tmp78, null
  br i1 %tmp80, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13, label %bb81

bb81:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit11
  %tmp82 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp78, i64 0, i32 15
  %tmp83 = load i32* %tmp82, align 4, !tbaa !6
  %tmp84 = icmp eq i32 %tmp83, 0
  br i1 %tmp84, label %bb85, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12

bb85:                                             ; preds = %bb81
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12: ; preds = %bb81
  %tmp86 = add nsw i32 %tmp83, 1
  store i32 %tmp86, i32* %tmp82, align 4, !tbaa !6
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13

_ZN7sc_core17sc_process_handleC1ERKS0_.exit13:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12, %_ZN7sc_core17sc_process_handleD1Ev.exit11
  %tmp87 = getelementptr inbounds %class.Source* %this, i64 0, i32 0, i32 4
  %tmp88 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp87, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp89 = load %"class.sc_core::sc_process_b"** %tmp79, align 8, !tbaa !2
  %tmp90 = icmp eq %"class.sc_core::sc_process_b"* %tmp89, null
  br i1 %tmp90, label %_ZN7sc_core17sc_process_handleD1Ev.exit16, label %bb91

bb91:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp92 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp89, i64 0, i32 15
  %tmp93 = load i32* %tmp92, align 4, !tbaa !6
  %tmp94 = add nsw i32 %tmp93, -1
  store i32 %tmp94, i32* %tmp92, align 4, !tbaa !6
  %tmp95 = icmp eq i32 %tmp94, 0
  br i1 %tmp95, label %bb96, label %_ZN7sc_core17sc_process_handleD1Ev.exit16

bb96:                                             ; preds = %bb91
  %tmp97 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  %tmp98 = icmp eq %"class.sc_core::sc_process_b"* %tmp97, null
  br i1 %tmp98, label %bb103, label %.noexc15

.noexc15:                                         ; preds = %bb96
  %tmp99 = bitcast %"class.sc_core::sc_process_b"* %tmp97 to void (%"class.sc_core::sc_process_b"*)***
  %tmp100 = load void (%"class.sc_core::sc_process_b"*)*** %tmp99, align 8, !tbaa !0
  %tmp101 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp100, i64 6
  %tmp102 = load void (%"class.sc_core::sc_process_b"*)** %tmp101, align 8
  call void %tmp102(%"class.sc_core::sc_process_b"* %tmp97)
  %.pre.i.i.i14 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %bb103

bb103:                                            ; preds = %.noexc15, %bb96
  %tmp104 = phi %"class.sc_core::sc_process_b"* [ null, %bb96 ], [ %.pre.i.i.i14, %.noexc15 ]
  %tmp105 = icmp eq %"class.sc_core::sc_process_b"* %tmp104, %tmp89
  br i1 %tmp105, label %bb106, label %bb107

bb106:                                            ; preds = %bb103
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb107:                                            ; preds = %bb103
  store %"class.sc_core::sc_process_b"* %tmp89, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit16

_ZN7sc_core17sc_process_handleD1Ev.exit16:        ; preds = %bb107, %bb91, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp108 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !2
  %tmp109 = icmp eq %"class.sc_core::sc_process_b"* %tmp108, null
  br i1 %tmp109, label %_ZN7sc_core17sc_process_handleD1Ev.exit19, label %bb110

bb110:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit16
  %tmp111 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp108, i64 0, i32 15
  %tmp112 = load i32* %tmp111, align 4, !tbaa !6
  %tmp113 = add nsw i32 %tmp112, -1
  store i32 %tmp113, i32* %tmp111, align 4, !tbaa !6
  %tmp114 = icmp eq i32 %tmp113, 0
  br i1 %tmp114, label %bb115, label %_ZN7sc_core17sc_process_handleD1Ev.exit19

bb115:                                            ; preds = %bb110
  %tmp116 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  %tmp117 = icmp eq %"class.sc_core::sc_process_b"* %tmp116, null
  br i1 %tmp117, label %bb122, label %.noexc18

.noexc18:                                         ; preds = %bb115
  %tmp118 = bitcast %"class.sc_core::sc_process_b"* %tmp116 to void (%"class.sc_core::sc_process_b"*)***
  %tmp119 = load void (%"class.sc_core::sc_process_b"*)*** %tmp118, align 8, !tbaa !0
  %tmp120 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp119, i64 6
  %tmp121 = load void (%"class.sc_core::sc_process_b"*)** %tmp120, align 8
  call void %tmp121(%"class.sc_core::sc_process_b"* %tmp116)
  %.pre.i.i.i17 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %bb122

bb122:                                            ; preds = %.noexc18, %bb115
  %tmp123 = phi %"class.sc_core::sc_process_b"* [ null, %bb115 ], [ %.pre.i.i.i17, %.noexc18 ]
  %tmp124 = icmp eq %"class.sc_core::sc_process_b"* %tmp123, %tmp108
  br i1 %tmp124, label %bb125, label %bb126

bb125:                                            ; preds = %bb122
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb126:                                            ; preds = %bb122
  store %"class.sc_core::sc_process_b"* %tmp108, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !2
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit19

_ZN7sc_core17sc_process_handleD1Ev.exit19:        ; preds = %bb126, %bb110, %_ZN7sc_core17sc_process_handleD1Ev.exit16
  ret void
}

define linkonce_odr void @_ZN6Source7computeEv(%class.Source* %this) uwtable align 2 {
bb:
  %tmp = alloca i8, align 1
  store i8 1, i8* %tmp, align 1
  %tmp1 = getelementptr inbounds %class.Source* %this, i64 0, i32 1, i32 0, i32 0, i32 0, i32 1
  %tmp2 = load %"class.sc_core::sc_signal_inout_if.55"** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq %"class.sc_core::sc_signal_inout_if.55"* %tmp2, null
  br i1 %tmp3, label %bb4, label %_ZN7sc_core8sc_inoutIbE5writeERKb.exit

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %class.Source* %this, i64 0, i32 1, i32 0, i32 0, i32 0, i32 0
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp5, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core13SC_ID_GET_IF_E, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8]* @.str20, i64 0, i64 0))
  %.pre.i.i = load %"class.sc_core::sc_signal_inout_if.55"** %tmp1, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_inoutIbE5writeERKb.exit

_ZN7sc_core8sc_inoutIbE5writeERKb.exit:           ; preds = %bb4, %bb
  %tmp6 = phi %"class.sc_core::sc_signal_inout_if.55"* [ %.pre.i.i, %bb4 ], [ %tmp2, %bb ]
  %tmp7 = getelementptr %"class.sc_core::sc_signal_inout_if.55"* %tmp6, i64 0, i32 1
  %tmp8 = bitcast %"class.sc_core::sc_signal_write_if.56"* %tmp7 to void (%"class.sc_core::sc_signal_write_if.56"*, i8*)***
  %tmp9 = load void (%"class.sc_core::sc_signal_write_if.56"*, i8*)*** %tmp8, align 8, !tbaa !0
  %tmp10 = getelementptr inbounds void (%"class.sc_core::sc_signal_write_if.56"*, i8*)** %tmp9, i64 4
  %tmp11 = load void (%"class.sc_core::sc_signal_write_if.56"*, i8*)** %tmp10, align 8
  call void %tmp11(%"class.sc_core::sc_signal_write_if.56"* %tmp7, i8* %tmp)
  ret void
}

define linkonce_odr i8* @_ZNK7sc_core6sc_outIbE4kindEv(%"class.sc_core::sc_out"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([7 x i8]* @.str22, i64 0, i64 0)
}

define linkonce_odr void @_ZN7sc_core6sc_outIbED0Ev(%"class.sc_core::sc_out"* %this) unnamed_addr uwtable align 2 {
_ZN7sc_core6sc_outIbED1Ev.exit:
  %tmp = getelementptr inbounds %"class.sc_core::sc_out"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core8sc_inoutIbED2Ev(%"class.sc_core::sc_inout"* %tmp)
  %tmp1 = bitcast %"class.sc_core::sc_out"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp1) nounwind
  ret void
}

define linkonce_odr %"class.sc_core::sc_interface"* @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE13get_interfaceEv(%"class.sc_core::sc_port_b"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 1
  %tmp1 = load %"class.sc_core::sc_signal_inout_if.55"** %tmp, align 8, !tbaa !2
  %tmp2 = icmp eq %"class.sc_core::sc_signal_inout_if.55"* %tmp1, null
  br i1 %tmp2, label %bb12, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %tmp1 to i8**
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = getelementptr i8* %tmp5, i64 -48
  %tmp7 = bitcast i8* %tmp6 to i64*
  %tmp8 = load i64* %tmp7, align 8
  %tmp9 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %tmp1 to i8*
  %tmp10 = getelementptr i8* %tmp9, i64 %tmp8
  %tmp11 = bitcast i8* %tmp10 to %"class.sc_core::sc_interface"*
  br label %bb12

bb12:                                             ; preds = %bb3, %bb
  %tmp13 = phi %"class.sc_core::sc_interface"* [ %tmp11, %bb3 ], [ null, %bb ]
  ret %"class.sc_core::sc_interface"* %tmp13
}

define linkonce_odr %"class.sc_core::sc_interface"* @_ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE13get_interfaceEv(%"class.sc_core::sc_port_b"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 1
  %tmp1 = load %"class.sc_core::sc_signal_inout_if.55"** %tmp, align 8, !tbaa !2
  %tmp2 = icmp eq %"class.sc_core::sc_signal_inout_if.55"* %tmp1, null
  br i1 %tmp2, label %bb12, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %tmp1 to i8**
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = getelementptr i8* %tmp5, i64 -48
  %tmp7 = bitcast i8* %tmp6 to i64*
  %tmp8 = load i64* %tmp7, align 8
  %tmp9 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %tmp1 to i8*
  %tmp10 = getelementptr i8* %tmp9, i64 %tmp8
  %tmp11 = bitcast i8* %tmp10 to %"class.sc_core::sc_interface"*
  br label %bb12

bb12:                                             ; preds = %bb3, %bb
  %tmp13 = phi %"class.sc_core::sc_interface"* [ %tmp11, %bb3 ], [ null, %bb ]
  ret %"class.sc_core::sc_interface"* %tmp13
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE5vbindERNS_12sc_interfaceE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_interface"* %interface_) uwtable align 2 {
bb:
  %tmp = icmp eq %"class.sc_core::sc_interface"* %interface_, null
  br i1 %tmp, label %.thread, label %bb1

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_interface"* %interface_ to i8*
  %tmp3 = tail call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core18sc_signal_inout_ifIbEE to i8*), i64 -1)
  %tmp4 = icmp eq i8* %tmp3, null
  br i1 %tmp4, label %.thread, label %bb5

bb5:                                              ; preds = %bb1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  %tmp7 = bitcast i8* %tmp3 to i8**
  %tmp8 = load i8** %tmp7, align 8, !tbaa !0
  %tmp9 = getelementptr i8* %tmp8, i64 -48
  %tmp10 = bitcast i8* %tmp9 to i64*
  %tmp11 = load i64* %tmp10, align 8
  %tmp12 = getelementptr i8* %tmp3, i64 %tmp11
  %tmp13 = bitcast i8* %tmp12 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_port_base4bindERNS_12sc_interfaceE(%"class.sc_core::sc_port_base"* %tmp6, %"class.sc_core::sc_interface"* %tmp13)
  br label %.thread

.thread:                                          ; preds = %bb5, %bb1, %bb
  %.0 = phi i32 [ 0, %bb5 ], [ 2, %bb1 ], [ 2, %bb ]
  ret i32 %.0
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE5vbindERNS_12sc_port_baseE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_port_base"* %parent_) uwtable align 2 {
bb:
  %tmp = icmp eq %"class.sc_core::sc_port_base"* %parent_, null
  br i1 %tmp, label %.thread, label %bb1

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_port_base"* %parent_ to i8*
  %tmp3 = tail call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core12sc_port_baseE to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEEE to i8*), i64 -1)
  %tmp4 = icmp eq i8* %tmp3, null
  br i1 %tmp4, label %.thread, label %bb5

bb5:                                              ; preds = %bb1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  %tmp7 = bitcast i8* %tmp3 to %"class.sc_core::sc_port_base"*
  tail call void @_ZN7sc_core12sc_port_base4bindERS0_(%"class.sc_core::sc_port_base"* %tmp6, %"class.sc_core::sc_port_base"* %tmp7)
  br label %.thread

.thread:                                          ; preds = %bb5, %bb1, %bb
  %.0 = phi i32 [ 0, %bb5 ], [ 2, %bb1 ], [ 2, %bb ]
  ret i32 %.0
}

define linkonce_odr void @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE13add_interfaceEPNS_12sc_interfaceE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_interface"* %interface_) uwtable align 2 {
bb:
  %iface = alloca %"class.sc_core::sc_signal_inout_if.55"*, align 8
  %tmp = icmp eq %"class.sc_core::sc_interface"* %interface_, null
  br i1 %tmp, label %.thread, label %bb1

.thread:                                          ; preds = %bb
  store %"class.sc_core::sc_signal_inout_if.55"* null, %"class.sc_core::sc_signal_inout_if.55"** %iface, align 8, !tbaa !2
  br label %bb6

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_interface"* %interface_ to i8*
  %tmp3 = call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core18sc_signal_inout_ifIbEE to i8*), i64 -1)
  %tmp4 = bitcast i8* %tmp3 to %"class.sc_core::sc_signal_inout_if.55"*
  store %"class.sc_core::sc_signal_inout_if.55"* %tmp4, %"class.sc_core::sc_signal_inout_if.55"** %iface, align 8, !tbaa !2
  %tmp5 = icmp eq i8* %tmp3, null
  br i1 %tmp5, label %bb6, label %bb7

bb6:                                              ; preds = %bb1, %.thread
  call void @__assert_fail(i8* getelementptr inbounds ([11 x i8]* @.str12, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str11, i64 0, i64 0), i32 580, i8* getelementptr inbounds ([149 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE13add_interfaceEPNS_12sc_interfaceE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb7:                                              ; preds = %bb1
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp10 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp9, align 8, !tbaa !2
  %tmp11 = getelementptr inbounds %"class.std::vector.57"* %tmp8, i64 0, i32 0, i32 0, i32 0
  %tmp12 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp11, align 8, !tbaa !2
  %tmp13 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp10 to i64
  %tmp14 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp12 to i64
  %tmp15 = sub i64 %tmp13, %tmp14
  %tmp16 = lshr exact i64 %tmp15, 3
  %tmp17 = trunc i64 %tmp16 to i32
  %tmp18 = icmp sgt i32 %tmp17, 0
  br i1 %tmp18, label %.lr.ph, label %bb27

.lr.ph:                                           ; preds = %bb7
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  br label %bb20

bb20:                                             ; preds = %._crit_edge2, %.lr.ph
  %tmp21 = phi %"class.sc_core::sc_signal_inout_if.55"** [ %tmp12, %.lr.ph ], [ %.pre3, %._crit_edge2 ]
  %indvars.iv = phi i64 [ 0, %.lr.ph ], [ %indvars.iv.next, %._crit_edge2 ]
  %tmp22 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp21, i64 %indvars.iv
  %tmp23 = load %"class.sc_core::sc_signal_inout_if.55"** %tmp22, align 8, !tbaa !2
  %tmp24 = icmp eq %"class.sc_core::sc_signal_inout_if.55"* %tmp4, %tmp23
  br i1 %tmp24, label %bb25, label %bb26

bb25:                                             ; preds = %bb20
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp19, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core22SC_ID_BIND_IF_TO_PORT_E, i64 0, i64 0), i8* getelementptr inbounds ([32 x i8]* @.str13, i64 0, i64 0))
  br label %bb26

bb26:                                             ; preds = %bb25, %bb20
  %indvars.iv.next = add i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %tmp17
  br i1 %exitcond, label %._crit_edge, label %._crit_edge2

._crit_edge2:                                     ; preds = %bb26
  %.pre3 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp11, align 8, !tbaa !2
  br label %bb20

._crit_edge:                                      ; preds = %bb26
  %.pre = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp9, align 8, !tbaa !2
  br label %bb27

bb27:                                             ; preds = %._crit_edge, %bb7
  %tmp28 = phi %"class.sc_core::sc_signal_inout_if.55"** [ %.pre, %._crit_edge ], [ %tmp10, %bb7 ]
  %tmp29 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 2
  %tmp30 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp29, align 8, !tbaa !2
  %tmp31 = icmp eq %"class.sc_core::sc_signal_inout_if.55"** %tmp28, %tmp30
  br i1 %tmp31, label %bb37, label %bb32

bb32:                                             ; preds = %bb27
  %tmp33 = icmp eq %"class.sc_core::sc_signal_inout_if.55"** %tmp28, null
  br i1 %tmp33, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit.i, label %bb34

bb34:                                             ; preds = %bb32
  store %"class.sc_core::sc_signal_inout_if.55"* %tmp4, %"class.sc_core::sc_signal_inout_if.55"** %tmp28, align 8, !tbaa !2
  %.pre.i = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp9, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit.i

_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit.i: ; preds = %bb34, %bb32
  %tmp35 = phi %"class.sc_core::sc_signal_inout_if.55"** [ null, %bb32 ], [ %.pre.i, %bb34 ]
  %tmp36 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp35, i64 1
  store %"class.sc_core::sc_signal_inout_if.55"** %tmp36, %"class.sc_core::sc_signal_inout_if.55"*** %tmp9, align 8, !tbaa !2
  br label %_ZNSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE9push_backERKS3_.exit

bb37:                                             ; preds = %bb27
  call void @_ZNSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS3_S5_EERKS3_(%"class.std::vector.57"* %tmp8, %"class.sc_core::sc_signal_inout_if.55"** %tmp28, %"class.sc_core::sc_signal_inout_if.55"** %iface)
  br label %_ZNSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE9push_backERKS3_.exit

_ZNSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE9push_backERKS3_.exit: ; preds = %bb37, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit.i
  %tmp38 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp11, align 8, !tbaa !2
  %tmp39 = load %"class.sc_core::sc_signal_inout_if.55"** %tmp38, align 8, !tbaa !2
  %tmp40 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 1
  store %"class.sc_core::sc_signal_inout_if.55"* %tmp39, %"class.sc_core::sc_signal_inout_if.55"** %tmp40, align 8, !tbaa !2
  ret void
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE15interface_countEv(%"class.sc_core::sc_port_b"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp1 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp, align 8, !tbaa !2
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp3 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp2, align 8, !tbaa !2
  %tmp4 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp1 to i64
  %tmp5 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp3 to i64
  %tmp6 = sub i64 %tmp4, %tmp5
  %tmp7 = lshr exact i64 %tmp6, 3
  %tmp8 = trunc i64 %tmp7 to i32
  ret i32 %tmp8
}

define linkonce_odr i8* @_ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE11if_typenameEv(%"class.sc_core::sc_port_b"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  ret i8* getelementptr inbounds ([34 x i8]* @_ZTSN7sc_core18sc_signal_inout_ifIbEE, i64 0, i64 0)
}

declare void @_ZN7sc_core8sc_inoutIbE18end_of_elaborationEv(%"class.sc_core::sc_inout"*)

define linkonce_odr void @_ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_thread_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0, i32 1
  %tmp2 = load %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb37

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp6 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp5, align 8, !tbaa !2
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp8 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp7, align 8, !tbaa !2
  %tmp9 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp6 to i64
  %tmp10 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp8 to i64
  %tmp11 = sub i64 %tmp9, %tmp10
  %tmp12 = lshr exact i64 %tmp11, 3
  %tmp13 = trunc i64 %tmp12 to i32
  br label %bb14

bb14:                                             ; preds = %bb23, %bb4
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb23 ], [ 0, %bb4 ]
  %tmp15 = trunc i64 %indvars.iv to i32
  %tmp16 = icmp slt i32 %tmp15, %tmp13
  br i1 %tmp16, label %bb17, label %.loopexit.loopexit

bb17:                                             ; preds = %bb14
  %tmp18 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp7, align 8, !tbaa !2
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp18, i64 %indvars.iv
  %tmp20 = load %"class.sc_core::sc_signal_inout_if.55"** %tmp19, align 8, !tbaa !2
  %tmp21 = icmp eq %"class.sc_core::sc_signal_inout_if.55"* %tmp20, null
  br i1 %tmp21, label %bb22, label %bb23

bb22:                                             ; preds = %bb17
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str10, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str11, i64 0, i64 0), i32 627, i8* getelementptr inbounds ([177 x i8]* @__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb23:                                             ; preds = %bb17
  %tmp24 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %tmp20 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -48
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %tmp20 to i8*
  %tmp30 = getelementptr i8* %tmp29, i64 %tmp28
  %tmp31 = bitcast i8* %tmp30 to %"class.sc_core::sc_interface"*
  %tmp32 = bitcast i8* %tmp30 to %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)***
  %tmp33 = load %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)*** %tmp32, align 8, !tbaa !0
  %tmp34 = getelementptr inbounds %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)** %tmp33, i64 1
  %tmp35 = load %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)** %tmp34, align 8
  %tmp36 = tail call %"class.sc_core::sc_event"* %tmp35(%"class.sc_core::sc_interface"* %tmp31)
  tail call void @_ZNK7sc_core12sc_port_base16add_static_eventEPNS_17sc_thread_processERKNS_8sc_eventE(%"class.sc_core::sc_port_base"* %tmp, %"class.sc_core::sc_thread_process"* %handle_p, %"class.sc_core::sc_event"* %tmp36)
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb14

bb37:                                             ; preds = %bb
  tail call void @_ZNK7sc_core12sc_port_base14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_base"* %tmp, %"class.sc_core::sc_thread_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_)
  br label %.loopexit

.loopexit.loopexit:                               ; preds = %bb14
  br label %.loopexit

.loopexit:                                        ; preds = %.loopexit.loopexit, %bb37
  ret void
}

define linkonce_odr void @_ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_method_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0, i32 1
  %tmp2 = load %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb37

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp6 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp5, align 8, !tbaa !2
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp8 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp7, align 8, !tbaa !2
  %tmp9 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp6 to i64
  %tmp10 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp8 to i64
  %tmp11 = sub i64 %tmp9, %tmp10
  %tmp12 = lshr exact i64 %tmp11, 3
  %tmp13 = trunc i64 %tmp12 to i32
  br label %bb14

bb14:                                             ; preds = %bb23, %bb4
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb23 ], [ 0, %bb4 ]
  %tmp15 = trunc i64 %indvars.iv to i32
  %tmp16 = icmp slt i32 %tmp15, %tmp13
  br i1 %tmp16, label %bb17, label %.loopexit.loopexit

bb17:                                             ; preds = %bb14
  %tmp18 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp7, align 8, !tbaa !2
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp18, i64 %indvars.iv
  %tmp20 = load %"class.sc_core::sc_signal_inout_if.55"** %tmp19, align 8, !tbaa !2
  %tmp21 = icmp eq %"class.sc_core::sc_signal_inout_if.55"* %tmp20, null
  br i1 %tmp21, label %bb22, label %bb23

bb22:                                             ; preds = %bb17
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str10, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str11, i64 0, i64 0), i32 648, i8* getelementptr inbounds ([177 x i8]* @__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bINS_18sc_signal_inout_ifIbEEE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb23:                                             ; preds = %bb17
  %tmp24 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %tmp20 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -48
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %tmp20 to i8*
  %tmp30 = getelementptr i8* %tmp29, i64 %tmp28
  %tmp31 = bitcast i8* %tmp30 to %"class.sc_core::sc_interface"*
  %tmp32 = bitcast i8* %tmp30 to %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)***
  %tmp33 = load %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)*** %tmp32, align 8, !tbaa !0
  %tmp34 = getelementptr inbounds %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)** %tmp33, i64 1
  %tmp35 = load %"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)** %tmp34, align 8
  %tmp36 = tail call %"class.sc_core::sc_event"* %tmp35(%"class.sc_core::sc_interface"* %tmp31)
  tail call void @_ZNK7sc_core12sc_port_base16add_static_eventEPNS_17sc_method_processERKNS_8sc_eventE(%"class.sc_core::sc_port_base"* %tmp, %"class.sc_core::sc_method_process"* %handle_p, %"class.sc_core::sc_event"* %tmp36)
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb14

bb37:                                             ; preds = %bb
  tail call void @_ZNK7sc_core12sc_port_base14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_base"* %tmp, %"class.sc_core::sc_method_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_)
  br label %.loopexit

.loopexit.loopexit:                               ; preds = %bb14
  br label %.loopexit

.loopexit:                                        ; preds = %.loopexit.loopexit, %bb37
  ret void
}

define linkonce_odr void @_ZNSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS3_S5_EERKS3_(%"class.std::vector.57"* nocapture %this, %"class.sc_core::sc_signal_inout_if.55"** %__position.coerce, %"class.sc_core::sc_signal_inout_if.55"** nocapture %__x) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.std::vector.57"* %this, i64 0, i32 0, i32 0, i32 1
  %tmp1 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp, align 8, !tbaa !2
  %tmp2 = getelementptr inbounds %"class.std::vector.57"* %this, i64 0, i32 0, i32 0, i32 2
  %tmp3 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp2, align 8, !tbaa !2
  %tmp4 = icmp eq %"class.sc_core::sc_signal_inout_if.55"** %tmp1, %tmp3
  br i1 %tmp4, label %_ZNKSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit, label %bb5

bb5:                                              ; preds = %bb
  %tmp6 = icmp eq %"class.sc_core::sc_signal_inout_if.55"** %tmp1, null
  br i1 %tmp6, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit, label %bb7

bb7:                                              ; preds = %bb5
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp1, i64 -1
  %tmp9 = load %"class.sc_core::sc_signal_inout_if.55"** %tmp8, align 8, !tbaa !2
  store %"class.sc_core::sc_signal_inout_if.55"* %tmp9, %"class.sc_core::sc_signal_inout_if.55"** %tmp1, align 8, !tbaa !2
  %.pre = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit

_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit: ; preds = %bb7, %bb5
  %tmp10 = phi %"class.sc_core::sc_signal_inout_if.55"** [ null, %bb5 ], [ %.pre, %bb7 ]
  %tmp11 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp10, i64 1
  store %"class.sc_core::sc_signal_inout_if.55"** %tmp11, %"class.sc_core::sc_signal_inout_if.55"*** %tmp, align 8, !tbaa !2
  %tmp12 = load %"class.sc_core::sc_signal_inout_if.55"** %__x, align 8, !tbaa !2
  %tmp13 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp10, i64 -1
  %tmp14 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp13 to i64
  %tmp15 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %__position.coerce to i64
  %tmp16 = sub i64 %tmp14, %tmp15
  %tmp17 = ashr exact i64 %tmp16, 3
  %tmp18 = icmp eq i64 %tmp17, 0
  br i1 %tmp18, label %_ZSt13copy_backwardIPPN7sc_core18sc_signal_inout_ifIbEES4_ET0_T_S6_S5_.exit, label %bb19

bb19:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit
  %.pre.i.i.i.i = sub i64 0, %tmp17
  %.pre1.i.i.i.i = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp10, i64 %.pre.i.i.i.i
  %tmp20 = bitcast %"class.sc_core::sc_signal_inout_if.55"** %.pre1.i.i.i.i to i8*
  %tmp21 = bitcast %"class.sc_core::sc_signal_inout_if.55"** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp20, i8* %tmp21, i64 %tmp16, i32 8, i1 false) nounwind
  br label %_ZSt13copy_backwardIPPN7sc_core18sc_signal_inout_ifIbEES4_ET0_T_S6_S5_.exit

_ZSt13copy_backwardIPPN7sc_core18sc_signal_inout_ifIbEES4_ET0_T_S6_S5_.exit: ; preds = %bb19, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit
  store %"class.sc_core::sc_signal_inout_if.55"* %tmp12, %"class.sc_core::sc_signal_inout_if.55"** %__position.coerce, align 8, !tbaa !2
  br label %bb67

_ZNKSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit: ; preds = %bb
  %tmp22 = getelementptr inbounds %"class.std::vector.57"* %this, i64 0, i32 0, i32 0, i32 0
  %tmp23 = load %"class.sc_core::sc_signal_inout_if.55"*** %tmp22, align 8, !tbaa !2
  %tmp24 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp1 to i64
  %tmp25 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %tmp23 to i64
  %tmp26 = sub i64 %tmp24, %tmp25
  %tmp27 = ashr exact i64 %tmp26, 3
  %tmp28 = icmp eq i64 %tmp27, 0
  %tmp29 = select i1 %tmp28, i64 1, i64 %tmp27
  %uadd.i = tail call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %tmp27, i64 %tmp29)
  %tmp30 = extractvalue { i64, i1 } %uadd.i, 0
  %tmp31 = extractvalue { i64, i1 } %uadd.i, 1
  %tmp32 = icmp ugt i64 %tmp30, 2305843009213693951
  %or.cond.i = or i1 %tmp31, %tmp32
  %tmp33 = select i1 %or.cond.i, i64 2305843009213693951, i64 %tmp30
  %tmp34 = icmp eq i64 %tmp33, 0
  br i1 %tmp34, label %_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE11_M_allocateEm.exit, label %bb35

bb35:                                             ; preds = %_ZNKSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit
  %tmp36 = icmp ugt i64 %tmp33, 2305843009213693951
  br i1 %tmp36, label %bb37, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE8allocateEmPKv.exit.i

bb37:                                             ; preds = %bb35
  tail call void @_ZSt17__throw_bad_allocv() noreturn
  unreachable

_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE8allocateEmPKv.exit.i: ; preds = %bb35
  %tmp38 = shl i64 %tmp33, 3
  %tmp39 = tail call noalias i8* @_Znwm(i64 %tmp38)
  %tmp40 = bitcast i8* %tmp39 to %"class.sc_core::sc_signal_inout_if.55"**
  br label %_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE11_M_allocateEm.exit

_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE11_M_allocateEm.exit: ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE8allocateEmPKv.exit.i, %_ZNKSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit
  %tmp41 = phi %"class.sc_core::sc_signal_inout_if.55"** [ %tmp40, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE8allocateEmPKv.exit.i ], [ null, %_ZNKSt6vectorIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE12_M_check_lenEmPKc.exit ]
  %tmp42 = ptrtoint %"class.sc_core::sc_signal_inout_if.55"** %__position.coerce to i64
  %tmp43 = sub i64 %tmp42, %tmp25
  %tmp44 = ashr exact i64 %tmp43, 3
  %tmp45 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp41, i64 %tmp44
  %tmp46 = icmp eq %"class.sc_core::sc_signal_inout_if.55"** %tmp45, null
  br i1 %tmp46, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit3, label %bb47

bb47:                                             ; preds = %_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE11_M_allocateEm.exit
  %tmp48 = load %"class.sc_core::sc_signal_inout_if.55"** %__x, align 8, !tbaa !2
  store %"class.sc_core::sc_signal_inout_if.55"* %tmp48, %"class.sc_core::sc_signal_inout_if.55"** %tmp45, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit3

_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit3: ; preds = %bb47, %_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE11_M_allocateEm.exit
  %tmp49 = icmp eq i64 %tmp44, 0
  br i1 %tmp49, label %bb53, label %bb50

bb50:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit3
  %tmp51 = bitcast %"class.sc_core::sc_signal_inout_if.55"** %tmp41 to i8*
  %tmp52 = bitcast %"class.sc_core::sc_signal_inout_if.55"** %tmp23 to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp51, i8* %tmp52, i64 %tmp43, i32 8, i1 false) nounwind
  br label %bb53

bb53:                                             ; preds = %bb50, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core18sc_signal_inout_ifIbEEE9constructEPS4_RKS4_.exit3
  %.sum = add i64 %tmp44, 1
  %tmp54 = sub i64 %tmp24, %tmp42
  %tmp55 = ashr exact i64 %tmp54, 3
  %tmp56 = icmp eq i64 %tmp55, 0
  br i1 %tmp56, label %bb61, label %bb57

bb57:                                             ; preds = %bb53
  %tmp58 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp41, i64 %.sum
  %tmp59 = bitcast %"class.sc_core::sc_signal_inout_if.55"** %tmp58 to i8*
  %tmp60 = bitcast %"class.sc_core::sc_signal_inout_if.55"** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp59, i8* %tmp60, i64 %tmp54, i32 8, i1 false) nounwind
  br label %bb61

bb61:                                             ; preds = %bb57, %bb53
  %tmp62 = icmp eq %"class.sc_core::sc_signal_inout_if.55"** %tmp23, null
  br i1 %tmp62, label %_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE13_M_deallocateEPS3_m.exit1, label %bb63

bb63:                                             ; preds = %bb61
  %tmp64 = bitcast %"class.sc_core::sc_signal_inout_if.55"** %tmp23 to i8*
  tail call void @_ZdlPv(i8* %tmp64) nounwind
  br label %_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE13_M_deallocateEPS3_m.exit1

_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE13_M_deallocateEPS3_m.exit1: ; preds = %bb63, %bb61
  %.sum4 = add i64 %tmp55, %.sum
  %tmp65 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp41, i64 %.sum4
  store %"class.sc_core::sc_signal_inout_if.55"** %tmp41, %"class.sc_core::sc_signal_inout_if.55"*** %tmp22, align 8, !tbaa !2
  store %"class.sc_core::sc_signal_inout_if.55"** %tmp65, %"class.sc_core::sc_signal_inout_if.55"*** %tmp, align 8, !tbaa !2
  %tmp66 = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"** %tmp41, i64 %tmp33
  store %"class.sc_core::sc_signal_inout_if.55"** %tmp66, %"class.sc_core::sc_signal_inout_if.55"*** %tmp2, align 8, !tbaa !2
  br label %bb67

bb67:                                             ; preds = %_ZNSt12_Vector_baseIPN7sc_core18sc_signal_inout_ifIbEESaIS3_EE13_M_deallocateEPS3_m.exit1, %_ZSt13copy_backwardIPPN7sc_core18sc_signal_inout_ifIbEES4_ET0_T_S6_S5_.exit
  ret void
}

declare void @_ZN7sc_core12sc_interfaceC2Ev(%"class.sc_core::sc_interface"*)

declare void @_ZN7sc_core15sc_prim_channelC2EPKc(%"class.sc_core::sc_prim_channel"*, i8*)

declare i8* @_ZN7sc_core18sc_gen_unique_nameEPKcb(i8*, i1 zeroext)

define linkonce_odr void @_ZN7sc_core9sc_signalIbE13register_portERNS_12sc_port_baseEPKc(%"class.sc_core::sc_signal.54"* %this, %"class.sc_core::sc_port_base"* %port_, i8* %if_typename_) uwtable inlinehint align 2 {
bb:
  %tmp = alloca %"class.std::allocator.40", align 1
  %nm = alloca %"class.std::basic_string", align 8
  %tmp1 = alloca %"class.std::allocator.40", align 1
  %tmp2 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp3 = icmp eq %"class.sc_core::sc_simcontext"* %tmp2, null
  br i1 %tmp3, label %bb4, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

bb4:                                              ; preds = %bb
  %tmp5 = call noalias i8* @_Znwm(i64 248)
  %tmp6 = bitcast i8* %tmp5 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp6)
  store %"class.sc_core::sc_simcontext"* %tmp6, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp6, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %bb4, %bb
  %tmp7 = phi %"class.sc_core::sc_simcontext"* [ %tmp6, %bb4 ], [ %tmp2, %bb ]
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp7, i64 0, i32 9
  %tmp9 = load i8* %tmp8, align 1, !tbaa !4, !range !7
  %tmp10 = icmp eq i8 %tmp9, 0
  br i1 %tmp10, label %_ZNSsD1Ev.exit, label %bb11

bb11:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  call void @_ZNSsC1EPKcRKSaIcE(%"class.std::basic_string"* %nm, i8* %if_typename_, %"class.std::allocator.40"* %tmp1)
  %tmp12 = call i32 @_ZNKSs7compareEPKc(%"class.std::basic_string"* %nm, i8* getelementptr inbounds ([34 x i8]* @_ZTSN7sc_core18sc_signal_inout_ifIbEE, i64 0, i64 0))
  %tmp13 = icmp eq i32 %tmp12, 0
  br i1 %tmp13, label %bb14, label %bb23

bb14:                                             ; preds = %bb11
  %tmp15 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 7
  %tmp16 = load %"class.sc_core::sc_port_base"** %tmp15, align 8, !tbaa !2
  %tmp17 = icmp eq %"class.sc_core::sc_port_base"* %tmp16, null
  br i1 %tmp17, label %bb22, label %bb18

bb18:                                             ; preds = %bb14
  %tmp19 = getelementptr %"class.sc_core::sc_signal.54"* %this, i64 0, i32 1, i32 0
  %tmp20 = getelementptr inbounds %"class.sc_core::sc_port_base"* %tmp16, i64 0, i32 0
  %tmp21 = getelementptr inbounds %"class.sc_core::sc_port_base"* %port_, i64 0, i32 0
  call void @_ZN7sc_core24sc_signal_invalid_writerEPNS_9sc_objectES1_S1_(%"class.sc_core::sc_object"* %tmp19, %"class.sc_core::sc_object"* %tmp20, %"class.sc_core::sc_object"* %tmp21)
  br label %bb22

bb22:                                             ; preds = %bb18, %bb14
  store %"class.sc_core::sc_port_base"* %port_, %"class.sc_core::sc_port_base"** %tmp15, align 8, !tbaa !2
  br label %bb23

bb23:                                             ; preds = %bb22, %bb11
  %tmp24 = getelementptr inbounds %"class.std::allocator.40"* %tmp, i64 0, i32 0
  call void @llvm.lifetime.start(i64 -1, i8* %tmp24)
  %tmp25 = getelementptr inbounds %"class.std::basic_string"* %nm, i64 0, i32 0, i32 0
  %tmp26 = load i8** %tmp25, align 8, !tbaa !2
  %tmp27 = getelementptr inbounds i8* %tmp26, i64 -24
  %tmp28 = icmp eq i8* %tmp27, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp28, label %_ZNSsD1Ev.exit, label %bb29, !prof !8

bb29:                                             ; preds = %bb23
  %tmp30 = getelementptr inbounds i8* %tmp26, i64 -8
  %tmp31 = bitcast i8* %tmp30 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb32, label %bb34

bb32:                                             ; preds = %bb29
  %tmp33 = atomicrmw add i32* %tmp31, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i

bb34:                                             ; preds = %bb29
  %tmp35 = load i32* %tmp31, align 4, !tbaa !6
  %tmp36 = add nsw i32 %tmp35, -1
  store i32 %tmp36, i32* %tmp31, align 4, !tbaa !6
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i: ; preds = %bb34, %bb32
  %.0.i.i.i.i = phi i32 [ %tmp33, %bb32 ], [ %tmp35, %bb34 ]
  %tmp37 = icmp slt i32 %.0.i.i.i.i, 1
  br i1 %tmp37, label %bb38, label %_ZNSsD1Ev.exit

bb38:                                             ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i
  %tmp39 = bitcast i8* %tmp27 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp39, %"class.std::allocator.40"* %tmp) nounwind
  br label %_ZNSsD1Ev.exit

_ZNSsD1Ev.exit:                                   ; preds = %bb38, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i, %bb23, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  ret void
}

define linkonce_odr %"class.sc_core::sc_event"* @_ZNK7sc_core9sc_signalIbE13default_eventEv(%"class.sc_core::sc_signal.54"* nocapture %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 2
  %tmp1 = load %"class.sc_core::sc_event"** %tmp, align 8, !tbaa !2
  %tmp2 = icmp eq %"class.sc_core::sc_event"* %tmp1, null
  br i1 %tmp2, label %bb3, label %bb18

bb3:                                              ; preds = %bb
  %tmp4 = tail call noalias i8* @_Znwm(i64 120)
  %tmp5 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp6 = icmp eq %"class.sc_core::sc_simcontext"* %tmp5, null
  br i1 %tmp6, label %.noexc, label %bb9

.noexc:                                           ; preds = %bb3
  %tmp7 = tail call noalias i8* @_Znwm(i64 248)
  %tmp8 = bitcast i8* %tmp7 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp8)
  store %"class.sc_core::sc_simcontext"* %tmp8, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp8, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %bb9

bb9:                                              ; preds = %.noexc, %bb3
  %tmp10 = phi %"class.sc_core::sc_simcontext"* [ %tmp8, %.noexc ], [ %tmp5, %bb3 ]
  %tmp11 = bitcast i8* %tmp4 to %"class.sc_core::sc_event"*
  %tmp12 = bitcast i8* %tmp4 to %"class.sc_core::sc_simcontext"**
  store %"class.sc_core::sc_simcontext"* %tmp10, %"class.sc_core::sc_simcontext"** %tmp12, align 8, !tbaa !2
  %tmp13 = getelementptr inbounds i8* %tmp4, i64 8
  %tmp14 = bitcast i8* %tmp13 to i32*
  store i32 0, i32* %tmp14, align 4, !tbaa !9
  %tmp15 = getelementptr inbounds i8* %tmp4, i64 12
  %tmp16 = bitcast i8* %tmp15 to i32*
  store i32 -1, i32* %tmp16, align 4, !tbaa !6
  %tmp17 = getelementptr inbounds i8* %tmp4, i64 16
  tail call void @llvm.memset.p0i8.i64(i8* %tmp17, i8 0, i64 104, i32 8, i1 false)
  store %"class.sc_core::sc_event"* %tmp11, %"class.sc_core::sc_event"** %tmp, align 8, !tbaa !2
  br label %bb18

bb18:                                             ; preds = %bb9, %bb
  %tmp19 = phi %"class.sc_core::sc_event"* [ %tmp1, %bb ], [ %tmp11, %bb9 ]
  ret %"class.sc_core::sc_event"* %tmp19
}

declare void @_ZN7sc_core9sc_signalIbED0Ev(%"class.sc_core::sc_signal.54"*)

define linkonce_odr %"class.sc_core::sc_event"* @_ZNK7sc_core9sc_signalIbE19value_changed_eventEv(%"class.sc_core::sc_signal.54"* nocapture %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 2
  %tmp1 = load %"class.sc_core::sc_event"** %tmp, align 8, !tbaa !2
  %tmp2 = icmp eq %"class.sc_core::sc_event"* %tmp1, null
  br i1 %tmp2, label %bb3, label %bb18

bb3:                                              ; preds = %bb
  %tmp4 = tail call noalias i8* @_Znwm(i64 120)
  %tmp5 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp6 = icmp eq %"class.sc_core::sc_simcontext"* %tmp5, null
  br i1 %tmp6, label %.noexc, label %bb9

.noexc:                                           ; preds = %bb3
  %tmp7 = tail call noalias i8* @_Znwm(i64 248)
  %tmp8 = bitcast i8* %tmp7 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp8)
  store %"class.sc_core::sc_simcontext"* %tmp8, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp8, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %bb9

bb9:                                              ; preds = %.noexc, %bb3
  %tmp10 = phi %"class.sc_core::sc_simcontext"* [ %tmp8, %.noexc ], [ %tmp5, %bb3 ]
  %tmp11 = bitcast i8* %tmp4 to %"class.sc_core::sc_event"*
  %tmp12 = bitcast i8* %tmp4 to %"class.sc_core::sc_simcontext"**
  store %"class.sc_core::sc_simcontext"* %tmp10, %"class.sc_core::sc_simcontext"** %tmp12, align 8, !tbaa !2
  %tmp13 = getelementptr inbounds i8* %tmp4, i64 8
  %tmp14 = bitcast i8* %tmp13 to i32*
  store i32 0, i32* %tmp14, align 4, !tbaa !9
  %tmp15 = getelementptr inbounds i8* %tmp4, i64 12
  %tmp16 = bitcast i8* %tmp15 to i32*
  store i32 -1, i32* %tmp16, align 4, !tbaa !6
  %tmp17 = getelementptr inbounds i8* %tmp4, i64 16
  tail call void @llvm.memset.p0i8.i64(i8* %tmp17, i8 0, i64 104, i32 8, i1 false)
  store %"class.sc_core::sc_event"* %tmp11, %"class.sc_core::sc_event"** %tmp, align 8, !tbaa !2
  br label %bb18

bb18:                                             ; preds = %bb9, %bb
  %tmp19 = phi %"class.sc_core::sc_event"* [ %tmp1, %bb ], [ %tmp11, %bb9 ]
  ret %"class.sc_core::sc_event"* %tmp19
}

define linkonce_odr %"class.sc_core::sc_event"* @_ZNK7sc_core9sc_signalIbE13posedge_eventEv(%"class.sc_core::sc_signal.54"* nocapture %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 8
  %tmp1 = load %"class.sc_core::sc_event"** %tmp, align 8, !tbaa !2
  %tmp2 = icmp eq %"class.sc_core::sc_event"* %tmp1, null
  br i1 %tmp2, label %bb3, label %bb18

bb3:                                              ; preds = %bb
  %tmp4 = tail call noalias i8* @_Znwm(i64 120)
  %tmp5 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp6 = icmp eq %"class.sc_core::sc_simcontext"* %tmp5, null
  br i1 %tmp6, label %.noexc, label %bb9

.noexc:                                           ; preds = %bb3
  %tmp7 = tail call noalias i8* @_Znwm(i64 248)
  %tmp8 = bitcast i8* %tmp7 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp8)
  store %"class.sc_core::sc_simcontext"* %tmp8, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp8, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %bb9

bb9:                                              ; preds = %.noexc, %bb3
  %tmp10 = phi %"class.sc_core::sc_simcontext"* [ %tmp8, %.noexc ], [ %tmp5, %bb3 ]
  %tmp11 = bitcast i8* %tmp4 to %"class.sc_core::sc_event"*
  %tmp12 = bitcast i8* %tmp4 to %"class.sc_core::sc_simcontext"**
  store %"class.sc_core::sc_simcontext"* %tmp10, %"class.sc_core::sc_simcontext"** %tmp12, align 8, !tbaa !2
  %tmp13 = getelementptr inbounds i8* %tmp4, i64 8
  %tmp14 = bitcast i8* %tmp13 to i32*
  store i32 0, i32* %tmp14, align 4, !tbaa !9
  %tmp15 = getelementptr inbounds i8* %tmp4, i64 12
  %tmp16 = bitcast i8* %tmp15 to i32*
  store i32 -1, i32* %tmp16, align 4, !tbaa !6
  %tmp17 = getelementptr inbounds i8* %tmp4, i64 16
  tail call void @llvm.memset.p0i8.i64(i8* %tmp17, i8 0, i64 104, i32 8, i1 false)
  store %"class.sc_core::sc_event"* %tmp11, %"class.sc_core::sc_event"** %tmp, align 8, !tbaa !2
  br label %bb18

bb18:                                             ; preds = %bb9, %bb
  %tmp19 = phi %"class.sc_core::sc_event"* [ %tmp1, %bb ], [ %tmp11, %bb9 ]
  ret %"class.sc_core::sc_event"* %tmp19
}

define linkonce_odr %"class.sc_core::sc_event"* @_ZNK7sc_core9sc_signalIbE13negedge_eventEv(%"class.sc_core::sc_signal.54"* nocapture %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 5
  %tmp1 = load %"class.sc_core::sc_event"** %tmp, align 8, !tbaa !2
  %tmp2 = icmp eq %"class.sc_core::sc_event"* %tmp1, null
  br i1 %tmp2, label %bb3, label %bb18

bb3:                                              ; preds = %bb
  %tmp4 = tail call noalias i8* @_Znwm(i64 120)
  %tmp5 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp6 = icmp eq %"class.sc_core::sc_simcontext"* %tmp5, null
  br i1 %tmp6, label %.noexc, label %bb9

.noexc:                                           ; preds = %bb3
  %tmp7 = tail call noalias i8* @_Znwm(i64 248)
  %tmp8 = bitcast i8* %tmp7 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp8)
  store %"class.sc_core::sc_simcontext"* %tmp8, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp8, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %bb9

bb9:                                              ; preds = %.noexc, %bb3
  %tmp10 = phi %"class.sc_core::sc_simcontext"* [ %tmp8, %.noexc ], [ %tmp5, %bb3 ]
  %tmp11 = bitcast i8* %tmp4 to %"class.sc_core::sc_event"*
  %tmp12 = bitcast i8* %tmp4 to %"class.sc_core::sc_simcontext"**
  store %"class.sc_core::sc_simcontext"* %tmp10, %"class.sc_core::sc_simcontext"** %tmp12, align 8, !tbaa !2
  %tmp13 = getelementptr inbounds i8* %tmp4, i64 8
  %tmp14 = bitcast i8* %tmp13 to i32*
  store i32 0, i32* %tmp14, align 4, !tbaa !9
  %tmp15 = getelementptr inbounds i8* %tmp4, i64 12
  %tmp16 = bitcast i8* %tmp15 to i32*
  store i32 -1, i32* %tmp16, align 4, !tbaa !6
  %tmp17 = getelementptr inbounds i8* %tmp4, i64 16
  tail call void @llvm.memset.p0i8.i64(i8* %tmp17, i8 0, i64 104, i32 8, i1 false)
  store %"class.sc_core::sc_event"* %tmp11, %"class.sc_core::sc_event"** %tmp, align 8, !tbaa !2
  br label %bb18

bb18:                                             ; preds = %bb9, %bb
  %tmp19 = phi %"class.sc_core::sc_event"* [ %tmp1, %bb ], [ %tmp11, %bb9 ]
  ret %"class.sc_core::sc_event"* %tmp19
}

define linkonce_odr i8* @_ZNK7sc_core9sc_signalIbE4readEv(%"class.sc_core::sc_signal.54"* %this) nounwind uwtable readnone align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 3
  ret i8* %tmp
}

define linkonce_odr i8* @_ZNK7sc_core9sc_signalIbE12get_data_refEv(%"class.sc_core::sc_signal.54"* %this) uwtable align 2 {
bb:
  tail call void @_ZN7sc_core26sc_deprecated_get_data_refEv()
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 3
  ret i8* %tmp
}

define linkonce_odr zeroext i1 @_ZNK7sc_core9sc_signalIbE5eventEv(%"class.sc_core::sc_signal.54"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr %"class.sc_core::sc_signal.54"* %this, i64 0, i32 1, i32 0, i32 1
  %tmp1 = load %"class.sc_core::sc_simcontext"** %tmp, align 8, !tbaa !2
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 4
  %tmp3 = load i64* %tmp2, align 8, !tbaa !5
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp1, i64 0, i32 19
  %tmp5 = load i64* %tmp4, align 8, !tbaa !5
  %tmp6 = icmp eq i64 %tmp5, %tmp3
  ret i1 %tmp6
}

define linkonce_odr zeroext i1 @_ZNK7sc_core9sc_signalIbE7posedgeEv(%"class.sc_core::sc_signal.54"* %this) uwtable align 2 {
bb:
  %tmp = bitcast %"class.sc_core::sc_signal.54"* %this to i1 (%"class.sc_core::sc_signal.54"*)***
  %tmp1 = load i1 (%"class.sc_core::sc_signal.54"*)*** %tmp, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds i1 (%"class.sc_core::sc_signal.54"*)** %tmp1, i64 9
  %tmp3 = load i1 (%"class.sc_core::sc_signal.54"*)** %tmp2, align 8
  %tmp4 = tail call zeroext i1 %tmp3(%"class.sc_core::sc_signal.54"* %this)
  br i1 %tmp4, label %bb5, label %bb9

bb5:                                              ; preds = %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 3
  %tmp7 = load i8* %tmp6, align 1, !tbaa !4, !range !7
  %tmp8 = icmp ne i8 %tmp7, 0
  br label %bb9

bb9:                                              ; preds = %bb5, %bb
  %tmp10 = phi i1 [ false, %bb ], [ %tmp8, %bb5 ]
  ret i1 %tmp10
}

define linkonce_odr zeroext i1 @_ZNK7sc_core9sc_signalIbE7negedgeEv(%"class.sc_core::sc_signal.54"* %this) uwtable align 2 {
bb:
  %tmp = bitcast %"class.sc_core::sc_signal.54"* %this to i1 (%"class.sc_core::sc_signal.54"*)***
  %tmp1 = load i1 (%"class.sc_core::sc_signal.54"*)*** %tmp, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds i1 (%"class.sc_core::sc_signal.54"*)** %tmp1, i64 9
  %tmp3 = load i1 (%"class.sc_core::sc_signal.54"*)** %tmp2, align 8
  %tmp4 = tail call zeroext i1 %tmp3(%"class.sc_core::sc_signal.54"* %this)
  br i1 %tmp4, label %bb5, label %bb9

bb5:                                              ; preds = %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 3
  %tmp7 = load i8* %tmp6, align 1, !tbaa !4, !range !7
  %tmp8 = icmp eq i8 %tmp7, 0
  br label %bb9

bb9:                                              ; preds = %bb5, %bb
  %tmp10 = phi i1 [ false, %bb ], [ %tmp8, %bb5 ]
  ret i1 %tmp10
}

declare %"class.sc_core::sc_reset"* @_ZNK7sc_core9sc_signalIbE8is_resetEv(%"class.sc_core::sc_signal.54"*)

define linkonce_odr void @_ZN7sc_core9sc_signalIbE5writeERKb(%"class.sc_core::sc_signal.54"* %this, i8* nocapture %value_) uwtable inlinehint align 2 {
bb:
  %tmp = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp1 = icmp eq %"class.sc_core::sc_simcontext"* %tmp, null
  br i1 %tmp1, label %bb2, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

bb2:                                              ; preds = %bb
  %tmp3 = tail call noalias i8* @_Znwm(i64 248)
  %tmp4 = bitcast i8* %tmp3 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp4)
  store %"class.sc_core::sc_simcontext"* %tmp4, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp4, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %bb2, %bb
  %tmp5 = phi %"class.sc_core::sc_simcontext"* [ %tmp4, %bb2 ], [ %tmp, %bb ]
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp5, i64 0, i32 8
  %tmp7 = load %"class.sc_core::sc_object"** %tmp6, align 8, !tbaa !2
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 10
  %tmp9 = load %"class.sc_core::sc_object"** %tmp8, align 8, !tbaa !2
  %tmp10 = icmp eq %"class.sc_core::sc_object"* %tmp9, null
  br i1 %tmp10, label %bb11, label %bb12

bb11:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  store %"class.sc_core::sc_object"* %tmp7, %"class.sc_core::sc_object"** %tmp8, align 8, !tbaa !2
  br label %bb16

bb12:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp13 = icmp eq %"class.sc_core::sc_object"* %tmp9, %tmp7
  br i1 %tmp13, label %bb16, label %bb14

bb14:                                             ; preds = %bb12
  %tmp15 = getelementptr %"class.sc_core::sc_signal.54"* %this, i64 0, i32 1, i32 0
  tail call void @_ZN7sc_core24sc_signal_invalid_writerEPNS_9sc_objectES1_S1_(%"class.sc_core::sc_object"* %tmp15, %"class.sc_core::sc_object"* %tmp9, %"class.sc_core::sc_object"* %tmp7)
  br label %bb16

bb16:                                             ; preds = %bb14, %bb12, %bb11
  %tmp17 = load i8* %value_, align 1, !tbaa !4, !range !7
  %tmp18 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 6
  store i8 %tmp17, i8* %tmp18, align 1, !tbaa !4
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 3
  %tmp20 = load i8* %tmp19, align 1, !tbaa !4, !range !7
  %tmp21 = icmp eq i8 %tmp17, %tmp20
  br i1 %tmp21, label %_ZN7sc_core15sc_prim_channel14request_updateEv.exit, label %bb22

bb22:                                             ; preds = %bb16
  %tmp23 = getelementptr %"class.sc_core::sc_signal.54"* %this, i64 0, i32 1, i32 2
  %tmp24 = load %"class.sc_core::sc_prim_channel"** %tmp23, align 8, !tbaa !2
  %tmp25 = icmp eq %"class.sc_core::sc_prim_channel"* %tmp24, null
  br i1 %tmp25, label %bb26, label %_ZN7sc_core15sc_prim_channel14request_updateEv.exit

bb26:                                             ; preds = %bb22
  %tmp27 = getelementptr %"class.sc_core::sc_signal.54"* %this, i64 0, i32 1
  %tmp28 = getelementptr %"class.sc_core::sc_signal.54"* %this, i64 0, i32 1, i32 1
  %tmp29 = load %"class.sc_core::sc_prim_channel_registry"** %tmp28, align 8, !tbaa !2
  %tmp30 = getelementptr inbounds %"class.sc_core::sc_prim_channel_registry"* %tmp29, i64 0, i32 2
  %tmp31 = load %"class.sc_core::sc_prim_channel"** %tmp30, align 8, !tbaa !2
  store %"class.sc_core::sc_prim_channel"* %tmp31, %"class.sc_core::sc_prim_channel"** %tmp23, align 8, !tbaa !2
  store %"class.sc_core::sc_prim_channel"* %tmp27, %"class.sc_core::sc_prim_channel"** %tmp30, align 8, !tbaa !2
  br label %_ZN7sc_core15sc_prim_channel14request_updateEv.exit

_ZN7sc_core15sc_prim_channel14request_updateEv.exit: ; preds = %bb26, %bb22, %bb16
  ret void
}

define linkonce_odr void @_ZNK7sc_core9sc_signalIbE5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_signal.54"* nocapture %this, %"class.sc_core::sc_trace_file"* nocapture %tf) uwtable align 2 {
bb:
  tail call void @_ZN7sc_core19sc_deprecated_traceEv()
  ret void
}

define linkonce_odr void @_ZNK7sc_core9sc_signalIbE5printERSo(%"class.sc_core::sc_signal.54"* nocapture %this, %"class.std::basic_ostream"* %os) uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 3
  %tmp1 = load i8* %tmp, align 1, !tbaa !4, !range !7
  %tmp2 = icmp ne i8 %tmp1, 0
  %tmp3 = tail call %"class.std::basic_ostream"* @_ZNSo9_M_insertIbEERSoT_(%"class.std::basic_ostream"* %os, i1 zeroext %tmp2)
  ret void
}

define linkonce_odr void @_ZNK7sc_core9sc_signalIbE4dumpERSo(%"class.sc_core::sc_signal.54"* nocapture %this, %"class.std::basic_ostream"* %os) uwtable inlinehint align 2 {
bb:
  %tmp = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %os, i8* getelementptr inbounds ([13 x i8]* @.str6, i64 0, i64 0), i64 12)
  %tmp1 = getelementptr %"class.sc_core::sc_signal.54"* %this, i64 0, i32 1, i32 0, i32 2
  %tmp2 = load i8** %tmp1, align 8, !tbaa !2
  %tmp3 = icmp eq i8* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb17

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %"class.std::basic_ostream"* %os to i8**
  %tmp6 = load i8** %tmp5, align 8, !tbaa !0
  %tmp7 = getelementptr i8* %tmp6, i64 -24
  %tmp8 = bitcast i8* %tmp7 to i64*
  %tmp9 = load i64* %tmp8, align 8
  %tmp10 = bitcast %"class.std::basic_ostream"* %os to i8*
  %tmp11 = getelementptr i8* %tmp10, i64 %tmp9
  %tmp12 = bitcast i8* %tmp11 to %"class.std::basic_ios"*
  %.sum.i = add i64 %tmp9, 32
  %tmp13 = getelementptr inbounds i8* %tmp10, i64 %.sum.i
  %tmp14 = bitcast i8* %tmp13 to i32*
  %tmp15 = load i32* %tmp14, align 4, !tbaa !10
  %tmp16 = or i32 %tmp15, 1
  tail call void @_ZNSt9basic_iosIcSt11char_traitsIcEE5clearESt12_Ios_Iostate(%"class.std::basic_ios"* %tmp12, i32 %tmp16)
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit

bb17:                                             ; preds = %bb
  %tmp18 = tail call i64 @strlen(i8* %tmp2) nounwind
  %tmp19 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %os, i8* %tmp2, i64 %tmp18)
  %.pre = bitcast %"class.std::basic_ostream"* %os to i8**
  %.pre10 = bitcast %"class.std::basic_ostream"* %os to i8*
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit: ; preds = %bb17, %bb4
  %.pre-phi11 = phi i8* [ %tmp10, %bb4 ], [ %.pre10, %bb17 ]
  %.pre-phi = phi i8** [ %tmp5, %bb4 ], [ %.pre, %bb17 ]
  %tmp20 = load i8** %.pre-phi, align 8, !tbaa !0
  %tmp21 = getelementptr i8* %tmp20, i64 -24
  %tmp22 = bitcast i8* %tmp21 to i64*
  %tmp23 = load i64* %tmp22, align 8
  %.sum.i1 = add i64 %tmp23, 240
  %tmp24 = getelementptr inbounds i8* %.pre-phi11, i64 %.sum.i1
  %tmp25 = bitcast i8* %tmp24 to %"class.std::ctype"**
  %tmp26 = load %"class.std::ctype"** %tmp25, align 8, !tbaa !2
  %tmp27 = icmp eq %"class.std::ctype"* %tmp26, null
  br i1 %tmp27, label %bb28, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb28:                                             ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit
  %tmp29 = getelementptr inbounds %"class.std::ctype"* %tmp26, i64 0, i32 6
  %tmp30 = load i8* %tmp29, align 1, !tbaa !3
  %tmp31 = icmp eq i8 %tmp30, 0
  br i1 %tmp31, label %bb35, label %bb32

bb32:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp33 = getelementptr inbounds %"class.std::ctype"* %tmp26, i64 0, i32 7, i64 10
  %tmp34 = load i8* %tmp33, align 1, !tbaa !3
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb35:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp26)
  %tmp36 = bitcast %"class.std::ctype"* %tmp26 to i8 (%"class.std::ctype"*, i8)***
  %tmp37 = load i8 (%"class.std::ctype"*, i8)*** %tmp36, align 8, !tbaa !0
  %tmp38 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp37, i64 6
  %tmp39 = load i8 (%"class.std::ctype"*, i8)** %tmp38, align 8
  %tmp40 = tail call signext i8 %tmp39(%"class.std::ctype"* %tmp26, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb35, %bb32
  %.0.i.i.i = phi i8 [ %tmp34, %bb32 ], [ %tmp40, %bb35 ]
  %tmp41 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %os, i8 signext %.0.i.i.i)
  %tmp42 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp41)
  %tmp43 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %os, i8* getelementptr inbounds ([13 x i8]* @.str7, i64 0, i64 0), i64 12)
  %tmp44 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 3
  %tmp45 = load i8* %tmp44, align 1, !tbaa !4, !range !7
  %tmp46 = icmp ne i8 %tmp45, 0
  %tmp47 = tail call %"class.std::basic_ostream"* @_ZNSo9_M_insertIbEERSoT_(%"class.std::basic_ostream"* %os, i1 zeroext %tmp46)
  %tmp48 = bitcast %"class.std::basic_ostream"* %tmp47 to i8**
  %tmp49 = load i8** %tmp48, align 8, !tbaa !0
  %tmp50 = getelementptr i8* %tmp49, i64 -24
  %tmp51 = bitcast i8* %tmp50 to i64*
  %tmp52 = load i64* %tmp51, align 8
  %tmp53 = bitcast %"class.std::basic_ostream"* %tmp47 to i8*
  %.sum.i2 = add i64 %tmp52, 240
  %tmp54 = getelementptr inbounds i8* %tmp53, i64 %.sum.i2
  %tmp55 = bitcast i8* %tmp54 to %"class.std::ctype"**
  %tmp56 = load %"class.std::ctype"** %tmp55, align 8, !tbaa !2
  %tmp57 = icmp eq %"class.std::ctype"* %tmp56, null
  br i1 %tmp57, label %bb58, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i3

bb58:                                             ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i3: ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit
  %tmp59 = getelementptr inbounds %"class.std::ctype"* %tmp56, i64 0, i32 6
  %tmp60 = load i8* %tmp59, align 1, !tbaa !3
  %tmp61 = icmp eq i8 %tmp60, 0
  br i1 %tmp61, label %bb65, label %bb62

bb62:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i3
  %tmp63 = getelementptr inbounds %"class.std::ctype"* %tmp56, i64 0, i32 7, i64 10
  %tmp64 = load i8* %tmp63, align 1, !tbaa !3
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit5

bb65:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i3
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp56)
  %tmp66 = bitcast %"class.std::ctype"* %tmp56 to i8 (%"class.std::ctype"*, i8)***
  %tmp67 = load i8 (%"class.std::ctype"*, i8)*** %tmp66, align 8, !tbaa !0
  %tmp68 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp67, i64 6
  %tmp69 = load i8 (%"class.std::ctype"*, i8)** %tmp68, align 8
  %tmp70 = tail call signext i8 %tmp69(%"class.std::ctype"* %tmp56, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit5

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit5: ; preds = %bb65, %bb62
  %.0.i.i.i4 = phi i8 [ %tmp64, %bb62 ], [ %tmp70, %bb65 ]
  %tmp71 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp47, i8 signext %.0.i.i.i4)
  %tmp72 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp71)
  %tmp73 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %os, i8* getelementptr inbounds ([13 x i8]* @.str8, i64 0, i64 0), i64 12)
  %tmp74 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 6
  %tmp75 = load i8* %tmp74, align 1, !tbaa !4, !range !7
  %tmp76 = icmp ne i8 %tmp75, 0
  %tmp77 = tail call %"class.std::basic_ostream"* @_ZNSo9_M_insertIbEERSoT_(%"class.std::basic_ostream"* %os, i1 zeroext %tmp76)
  %tmp78 = bitcast %"class.std::basic_ostream"* %tmp77 to i8**
  %tmp79 = load i8** %tmp78, align 8, !tbaa !0
  %tmp80 = getelementptr i8* %tmp79, i64 -24
  %tmp81 = bitcast i8* %tmp80 to i64*
  %tmp82 = load i64* %tmp81, align 8
  %tmp83 = bitcast %"class.std::basic_ostream"* %tmp77 to i8*
  %.sum.i6 = add i64 %tmp82, 240
  %tmp84 = getelementptr inbounds i8* %tmp83, i64 %.sum.i6
  %tmp85 = bitcast i8* %tmp84 to %"class.std::ctype"**
  %tmp86 = load %"class.std::ctype"** %tmp85, align 8, !tbaa !2
  %tmp87 = icmp eq %"class.std::ctype"* %tmp86, null
  br i1 %tmp87, label %bb88, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i7

bb88:                                             ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit5
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i7: ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit5
  %tmp89 = getelementptr inbounds %"class.std::ctype"* %tmp86, i64 0, i32 6
  %tmp90 = load i8* %tmp89, align 1, !tbaa !3
  %tmp91 = icmp eq i8 %tmp90, 0
  br i1 %tmp91, label %bb95, label %bb92

bb92:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i7
  %tmp93 = getelementptr inbounds %"class.std::ctype"* %tmp86, i64 0, i32 7, i64 10
  %tmp94 = load i8* %tmp93, align 1, !tbaa !3
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit9

bb95:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i7
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp86)
  %tmp96 = bitcast %"class.std::ctype"* %tmp86 to i8 (%"class.std::ctype"*, i8)***
  %tmp97 = load i8 (%"class.std::ctype"*, i8)*** %tmp96, align 8, !tbaa !0
  %tmp98 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp97, i64 6
  %tmp99 = load i8 (%"class.std::ctype"*, i8)** %tmp98, align 8
  %tmp100 = tail call signext i8 %tmp99(%"class.std::ctype"* %tmp86, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit9

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit9: ; preds = %bb95, %bb92
  %.0.i.i.i8 = phi i8 [ %tmp94, %bb92 ], [ %tmp100, %bb95 ]
  %tmp101 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp77, i8 signext %.0.i.i.i8)
  %tmp102 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp101)
  ret void
}

define linkonce_odr i8* @_ZNK7sc_core9sc_signalIbE4kindEv(%"class.sc_core::sc_signal.54"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str3, i64 0, i64 0)
}

define linkonce_odr void @_ZN7sc_core9sc_signalIbE6updateEv(%"class.sc_core::sc_signal.54"* nocapture %this) uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 6
  %tmp1 = load i8* %tmp, align 1, !tbaa !4, !range !7
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 3
  %tmp3 = load i8* %tmp2, align 1, !tbaa !4, !range !7
  %tmp4 = icmp eq i8 %tmp1, %tmp3
  br i1 %tmp4, label %bb126, label %bb5

bb5:                                              ; preds = %bb
  %tmp6 = alloca %"class.sc_core::sc_event"*, align 8
  %tmp7 = alloca %"class.sc_core::sc_event"*, align 8
  %tmp8 = alloca %"class.sc_core::sc_event"*, align 8
  store i8 %tmp1, i8* %tmp2, align 1, !tbaa !4
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 9
  %tmp10 = load %"class.sc_core::sc_reset"** %tmp9, align 8, !tbaa !2
  %tmp11 = icmp eq %"class.sc_core::sc_reset"* %tmp10, null
  br i1 %tmp11, label %bb13, label %bb12

bb12:                                             ; preds = %bb5
  call void @_ZN7sc_core8sc_reset16notify_processesEv(%"class.sc_core::sc_reset"* %tmp10)
  br label %bb13

bb13:                                             ; preds = %bb12, %bb5
  %tmp14 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 2
  %tmp15 = load %"class.sc_core::sc_event"** %tmp14, align 8, !tbaa !2
  %tmp16 = icmp eq %"class.sc_core::sc_event"* %tmp15, null
  br i1 %tmp16, label %._crit_edge, label %bb17

bb17:                                             ; preds = %bb13
  %tmp18 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp15, i64 0, i32 1
  %tmp19 = load i32* %tmp18, align 4, !tbaa !9
  %tmp20 = icmp eq i32 %tmp19, 0
  br i1 %tmp20, label %bb22, label %bb21

bb21:                                             ; preds = %bb17
  call void @_ZN7sc_core17sc_report_handler6reportENS_11sc_severityEPKcS3_S3_i(i32 2, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core21SC_ID_NOTIFY_DELAYED_E, i64 0, i64 0), i8* null, i8* getelementptr inbounds ([42 x i8]* @.str4, i64 0, i64 0), i32 260)
  br label %bb22

bb22:                                             ; preds = %bb21, %bb17
  %tmp23 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp15, i64 0, i32 0
  %tmp24 = load %"class.sc_core::sc_simcontext"** %tmp23, align 8, !tbaa !2
  %tmp25 = bitcast %"class.sc_core::sc_event"** %tmp8 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp25)
  store %"class.sc_core::sc_event"* %tmp15, %"class.sc_core::sc_event"** %tmp8, align 8, !tbaa !2
  %tmp26 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp24, i64 0, i32 12
  %tmp27 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp24, i64 0, i32 12, i32 0, i32 0, i32 1
  %tmp28 = load %"class.sc_core::sc_event"*** %tmp27, align 8, !tbaa !2
  %tmp29 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp24, i64 0, i32 12, i32 0, i32 0, i32 2
  %tmp30 = load %"class.sc_core::sc_event"*** %tmp29, align 8, !tbaa !2
  %tmp31 = icmp eq %"class.sc_core::sc_event"** %tmp28, %tmp30
  br i1 %tmp31, label %bb37, label %bb32

bb32:                                             ; preds = %bb22
  %tmp33 = icmp eq %"class.sc_core::sc_event"** %tmp28, null
  br i1 %tmp33, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i, label %bb34

bb34:                                             ; preds = %bb32
  store %"class.sc_core::sc_event"* %tmp15, %"class.sc_core::sc_event"** %tmp28, align 8, !tbaa !2
  %.pre.i.i.i = load %"class.sc_core::sc_event"*** %tmp27, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i: ; preds = %bb34, %bb32
  %tmp35 = phi %"class.sc_core::sc_event"** [ null, %bb32 ], [ %.pre.i.i.i, %bb34 ]
  %tmp36 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp35, i64 1
  store %"class.sc_core::sc_event"** %tmp36, %"class.sc_core::sc_event"*** %tmp27, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit

bb37:                                             ; preds = %bb22
  call void @_ZNSt6vectorIPN7sc_core8sc_eventESaIS2_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS2_S4_EERKS2_(%"class.std::vector.15"* %tmp26, %"class.sc_core::sc_event"** %tmp28, %"class.sc_core::sc_event"** %tmp8)
  %.pre.i.i = load %"class.sc_core::sc_event"*** %tmp27, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit

_ZN7sc_core8sc_event17notify_next_deltaEv.exit:   ; preds = %bb37, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i
  %tmp38 = phi %"class.sc_core::sc_event"** [ %tmp36, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i ], [ %.pre.i.i, %bb37 ]
  %tmp39 = getelementptr inbounds %"class.std::vector.15"* %tmp26, i64 0, i32 0, i32 0, i32 0
  %tmp40 = load %"class.sc_core::sc_event"*** %tmp39, align 8, !tbaa !2
  %tmp41 = ptrtoint %"class.sc_core::sc_event"** %tmp38 to i64
  %tmp42 = ptrtoint %"class.sc_core::sc_event"** %tmp40 to i64
  %tmp43 = sub i64 %tmp41, %tmp42
  %tmp44 = lshr exact i64 %tmp43, 3
  %tmp45 = add i64 %tmp44, 4294967295
  %tmp46 = trunc i64 %tmp45 to i32
  call void @llvm.lifetime.end(i64 -1, i8* %tmp25)
  %tmp47 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp15, i64 0, i32 2
  store i32 %tmp46, i32* %tmp47, align 4, !tbaa !6
  store i32 1, i32* %tmp18, align 4, !tbaa !9
  br label %._crit_edge

._crit_edge:                                      ; preds = %_ZN7sc_core8sc_event17notify_next_deltaEv.exit, %bb13
  %tmp48 = load i8* %tmp2, align 1, !tbaa !4, !range !7
  %tmp49 = icmp eq i8 %tmp48, 0
  br i1 %tmp49, label %bb85, label %bb50

bb50:                                             ; preds = %._crit_edge
  %tmp51 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 8
  %tmp52 = load %"class.sc_core::sc_event"** %tmp51, align 8, !tbaa !2
  %tmp53 = icmp eq %"class.sc_core::sc_event"* %tmp52, null
  br i1 %tmp53, label %bb120, label %bb54

bb54:                                             ; preds = %bb50
  %tmp55 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp52, i64 0, i32 1
  %tmp56 = load i32* %tmp55, align 4, !tbaa !9
  %tmp57 = icmp eq i32 %tmp56, 0
  br i1 %tmp57, label %bb59, label %bb58

bb58:                                             ; preds = %bb54
  call void @_ZN7sc_core17sc_report_handler6reportENS_11sc_severityEPKcS3_S3_i(i32 2, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core21SC_ID_NOTIFY_DELAYED_E, i64 0, i64 0), i8* null, i8* getelementptr inbounds ([42 x i8]* @.str4, i64 0, i64 0), i32 260)
  br label %bb59

bb59:                                             ; preds = %bb58, %bb54
  %tmp60 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp52, i64 0, i32 0
  %tmp61 = load %"class.sc_core::sc_simcontext"** %tmp60, align 8, !tbaa !2
  %tmp62 = bitcast %"class.sc_core::sc_event"** %tmp7 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp62)
  store %"class.sc_core::sc_event"* %tmp52, %"class.sc_core::sc_event"** %tmp7, align 8, !tbaa !2
  %tmp63 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp61, i64 0, i32 12
  %tmp64 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp61, i64 0, i32 12, i32 0, i32 0, i32 1
  %tmp65 = load %"class.sc_core::sc_event"*** %tmp64, align 8, !tbaa !2
  %tmp66 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp61, i64 0, i32 12, i32 0, i32 0, i32 2
  %tmp67 = load %"class.sc_core::sc_event"*** %tmp66, align 8, !tbaa !2
  %tmp68 = icmp eq %"class.sc_core::sc_event"** %tmp65, %tmp67
  br i1 %tmp68, label %bb74, label %bb69

bb69:                                             ; preds = %bb59
  %tmp70 = icmp eq %"class.sc_core::sc_event"** %tmp65, null
  br i1 %tmp70, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2, label %bb71

bb71:                                             ; preds = %bb69
  store %"class.sc_core::sc_event"* %tmp52, %"class.sc_core::sc_event"** %tmp65, align 8, !tbaa !2
  %.pre.i.i.i1 = load %"class.sc_core::sc_event"*** %tmp64, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2: ; preds = %bb71, %bb69
  %tmp72 = phi %"class.sc_core::sc_event"** [ null, %bb69 ], [ %.pre.i.i.i1, %bb71 ]
  %tmp73 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp72, i64 1
  store %"class.sc_core::sc_event"** %tmp73, %"class.sc_core::sc_event"*** %tmp64, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit4

bb74:                                             ; preds = %bb59
  call void @_ZNSt6vectorIPN7sc_core8sc_eventESaIS2_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS2_S4_EERKS2_(%"class.std::vector.15"* %tmp63, %"class.sc_core::sc_event"** %tmp65, %"class.sc_core::sc_event"** %tmp7)
  %.pre.i.i3 = load %"class.sc_core::sc_event"*** %tmp64, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit4

_ZN7sc_core8sc_event17notify_next_deltaEv.exit4:  ; preds = %bb74, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2
  %tmp75 = phi %"class.sc_core::sc_event"** [ %tmp73, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2 ], [ %.pre.i.i3, %bb74 ]
  %tmp76 = getelementptr inbounds %"class.std::vector.15"* %tmp63, i64 0, i32 0, i32 0, i32 0
  %tmp77 = load %"class.sc_core::sc_event"*** %tmp76, align 8, !tbaa !2
  %tmp78 = ptrtoint %"class.sc_core::sc_event"** %tmp75 to i64
  %tmp79 = ptrtoint %"class.sc_core::sc_event"** %tmp77 to i64
  %tmp80 = sub i64 %tmp78, %tmp79
  %tmp81 = lshr exact i64 %tmp80, 3
  %tmp82 = add i64 %tmp81, 4294967295
  %tmp83 = trunc i64 %tmp82 to i32
  call void @llvm.lifetime.end(i64 -1, i8* %tmp62)
  %tmp84 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp52, i64 0, i32 2
  store i32 %tmp83, i32* %tmp84, align 4, !tbaa !6
  store i32 1, i32* %tmp55, align 4, !tbaa !9
  br label %bb120

bb85:                                             ; preds = %._crit_edge
  %tmp86 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 5
  %tmp87 = load %"class.sc_core::sc_event"** %tmp86, align 8, !tbaa !2
  %tmp88 = icmp eq %"class.sc_core::sc_event"* %tmp87, null
  br i1 %tmp88, label %bb120, label %bb89

bb89:                                             ; preds = %bb85
  %tmp90 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp87, i64 0, i32 1
  %tmp91 = load i32* %tmp90, align 4, !tbaa !9
  %tmp92 = icmp eq i32 %tmp91, 0
  br i1 %tmp92, label %bb94, label %bb93

bb93:                                             ; preds = %bb89
  call void @_ZN7sc_core17sc_report_handler6reportENS_11sc_severityEPKcS3_S3_i(i32 2, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core21SC_ID_NOTIFY_DELAYED_E, i64 0, i64 0), i8* null, i8* getelementptr inbounds ([42 x i8]* @.str4, i64 0, i64 0), i32 260)
  br label %bb94

bb94:                                             ; preds = %bb93, %bb89
  %tmp95 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp87, i64 0, i32 0
  %tmp96 = load %"class.sc_core::sc_simcontext"** %tmp95, align 8, !tbaa !2
  %tmp97 = bitcast %"class.sc_core::sc_event"** %tmp6 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp97)
  store %"class.sc_core::sc_event"* %tmp87, %"class.sc_core::sc_event"** %tmp6, align 8, !tbaa !2
  %tmp98 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp96, i64 0, i32 12
  %tmp99 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp96, i64 0, i32 12, i32 0, i32 0, i32 1
  %tmp100 = load %"class.sc_core::sc_event"*** %tmp99, align 8, !tbaa !2
  %tmp101 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp96, i64 0, i32 12, i32 0, i32 0, i32 2
  %tmp102 = load %"class.sc_core::sc_event"*** %tmp101, align 8, !tbaa !2
  %tmp103 = icmp eq %"class.sc_core::sc_event"** %tmp100, %tmp102
  br i1 %tmp103, label %bb109, label %bb104

bb104:                                            ; preds = %bb94
  %tmp105 = icmp eq %"class.sc_core::sc_event"** %tmp100, null
  br i1 %tmp105, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6, label %bb106

bb106:                                            ; preds = %bb104
  store %"class.sc_core::sc_event"* %tmp87, %"class.sc_core::sc_event"** %tmp100, align 8, !tbaa !2
  %.pre.i.i.i5 = load %"class.sc_core::sc_event"*** %tmp99, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6: ; preds = %bb106, %bb104
  %tmp107 = phi %"class.sc_core::sc_event"** [ null, %bb104 ], [ %.pre.i.i.i5, %bb106 ]
  %tmp108 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp107, i64 1
  store %"class.sc_core::sc_event"** %tmp108, %"class.sc_core::sc_event"*** %tmp99, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit8

bb109:                                            ; preds = %bb94
  call void @_ZNSt6vectorIPN7sc_core8sc_eventESaIS2_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS2_S4_EERKS2_(%"class.std::vector.15"* %tmp98, %"class.sc_core::sc_event"** %tmp100, %"class.sc_core::sc_event"** %tmp6)
  %.pre.i.i7 = load %"class.sc_core::sc_event"*** %tmp99, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit8

_ZN7sc_core8sc_event17notify_next_deltaEv.exit8:  ; preds = %bb109, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6
  %tmp110 = phi %"class.sc_core::sc_event"** [ %tmp108, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6 ], [ %.pre.i.i7, %bb109 ]
  %tmp111 = getelementptr inbounds %"class.std::vector.15"* %tmp98, i64 0, i32 0, i32 0, i32 0
  %tmp112 = load %"class.sc_core::sc_event"*** %tmp111, align 8, !tbaa !2
  %tmp113 = ptrtoint %"class.sc_core::sc_event"** %tmp110 to i64
  %tmp114 = ptrtoint %"class.sc_core::sc_event"** %tmp112 to i64
  %tmp115 = sub i64 %tmp113, %tmp114
  %tmp116 = lshr exact i64 %tmp115, 3
  %tmp117 = add i64 %tmp116, 4294967295
  %tmp118 = trunc i64 %tmp117 to i32
  call void @llvm.lifetime.end(i64 -1, i8* %tmp97)
  %tmp119 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp87, i64 0, i32 2
  store i32 %tmp118, i32* %tmp119, align 4, !tbaa !6
  store i32 1, i32* %tmp90, align 4, !tbaa !9
  br label %bb120

bb120:                                            ; preds = %_ZN7sc_core8sc_event17notify_next_deltaEv.exit8, %bb85, %_ZN7sc_core8sc_event17notify_next_deltaEv.exit4, %bb50
  %tmp121 = getelementptr %"class.sc_core::sc_signal.54"* %this, i64 0, i32 1, i32 0, i32 1
  %tmp122 = load %"class.sc_core::sc_simcontext"** %tmp121, align 8, !tbaa !2
  %tmp123 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp122, i64 0, i32 19
  %tmp124 = load i64* %tmp123, align 8, !tbaa !5
  %tmp125 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 0, i32 4
  store i64 %tmp124, i64* %tmp125, align 8, !tbaa !5
  br label %bb126

bb126:                                            ; preds = %bb120, %bb
  ret void
}

define linkonce_odr zeroext i1 @_ZNK7sc_core9sc_signalIbE8is_clockEv(%"class.sc_core::sc_signal.54"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i1 false
}

define available_externally void @_ZThn8_N7sc_core9sc_signalIbED1Ev(%"class.sc_core::sc_signal.54"* %this) {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 -1, i32 10
  %tmp1 = bitcast %"class.sc_core::sc_object"** %tmp to %"class.sc_core::sc_signal.54"*
  tail call void @_ZN7sc_core9sc_signalIbED1Ev(%"class.sc_core::sc_signal.54"* %tmp1)
  ret void
}

define available_externally void @_ZThn8_N7sc_core9sc_signalIbED0Ev(%"class.sc_core::sc_signal.54"* %this) {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 -1, i32 10
  %tmp1 = bitcast %"class.sc_core::sc_object"** %tmp to %"class.sc_core::sc_signal.54"*
  tail call void @_ZN7sc_core9sc_signalIbED0Ev(%"class.sc_core::sc_signal.54"* %tmp1)
  ret void
}

define linkonce_odr void @_ZThn8_N7sc_core9sc_signalIbE5writeERKb(%"class.sc_core::sc_signal.54"* %this, i8* nocapture %arg) {
bb:
  %tmp = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  %tmp1 = icmp eq %"class.sc_core::sc_simcontext"* %tmp, null
  br i1 %tmp1, label %bb2, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit.i

bb2:                                              ; preds = %bb
  %tmp3 = tail call noalias i8* @_Znwm(i64 248)
  %tmp4 = bitcast i8* %tmp3 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp4)
  store %"class.sc_core::sc_simcontext"* %tmp4, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !2
  store %"class.sc_core::sc_simcontext"* %tmp4, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !2
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit.i

_ZN7sc_core22sc_get_curr_simcontextEv.exit.i:     ; preds = %bb2, %bb
  %tmp5 = phi %"class.sc_core::sc_simcontext"* [ %tmp4, %bb2 ], [ %tmp, %bb ]
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 -1, i32 10
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp5, i64 0, i32 8
  %tmp8 = load %"class.sc_core::sc_object"** %tmp7, align 8, !tbaa !2
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_object"** %tmp6, i64 17
  %tmp10 = load %"class.sc_core::sc_object"** %tmp9, align 8, !tbaa !2
  %tmp11 = icmp eq %"class.sc_core::sc_object"* %tmp10, null
  br i1 %tmp11, label %bb12, label %bb13

bb12:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit.i
  store %"class.sc_core::sc_object"* %tmp8, %"class.sc_core::sc_object"** %tmp9, align 8, !tbaa !2
  br label %bb18

bb13:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit.i
  %tmp14 = icmp eq %"class.sc_core::sc_object"* %tmp10, %tmp8
  br i1 %tmp14, label %bb18, label %bb15

bb15:                                             ; preds = %bb13
  %tmp16 = getelementptr %"class.sc_core::sc_object"** %tmp6, i64 2
  %tmp17 = bitcast %"class.sc_core::sc_object"** %tmp16 to %"class.sc_core::sc_object"*
  tail call void @_ZN7sc_core24sc_signal_invalid_writerEPNS_9sc_objectES1_S1_(%"class.sc_core::sc_object"* %tmp17, %"class.sc_core::sc_object"* %tmp10, %"class.sc_core::sc_object"* %tmp8)
  br label %bb18

bb18:                                             ; preds = %bb15, %bb13, %bb12
  %tmp19 = load i8* %arg, align 1, !tbaa !4, !range !7
  %tmp20 = getelementptr inbounds %"class.sc_core::sc_object"** %tmp6, i64 13
  %tmp21 = bitcast %"class.sc_core::sc_object"** %tmp20 to i8*
  store i8 %tmp19, i8* %tmp21, align 1, !tbaa !4
  %tmp22 = getelementptr inbounds %"class.sc_core::sc_object"** %tmp6, i64 10
  %tmp23 = bitcast %"class.sc_core::sc_object"** %tmp22 to i8*
  %tmp24 = load i8* %tmp23, align 1, !tbaa !4, !range !7
  %tmp25 = icmp eq i8 %tmp19, %tmp24
  br i1 %tmp25, label %_ZN7sc_core9sc_signalIbE5writeERKb.exit, label %bb26

bb26:                                             ; preds = %bb18
  %tmp27 = getelementptr %"class.sc_core::sc_object"** %tmp6, i64 8
  %tmp28 = load %"class.sc_core::sc_object"** %tmp27, align 8
  %tmp29 = icmp eq %"class.sc_core::sc_object"* %tmp28, null
  br i1 %tmp29, label %bb30, label %_ZN7sc_core9sc_signalIbE5writeERKb.exit

bb30:                                             ; preds = %bb26
  %tmp31 = getelementptr %"class.sc_core::sc_object"** %tmp6, i64 2
  %tmp32 = getelementptr %"class.sc_core::sc_object"** %tmp6, i64 7
  %tmp33 = load %"class.sc_core::sc_object"** %tmp32, align 8
  %tmp34 = getelementptr inbounds %"class.sc_core::sc_object"* %tmp33, i64 0, i32 4
  %tmp35 = load %"class.sc_core::sc_object"** %tmp34, align 8
  store %"class.sc_core::sc_object"* %tmp35, %"class.sc_core::sc_object"** %tmp27, align 8, !tbaa !2
  %.c = bitcast %"class.sc_core::sc_object"** %tmp31 to %"class.sc_core::sc_object"*
  store %"class.sc_core::sc_object"* %.c, %"class.sc_core::sc_object"** %tmp34, align 8, !tbaa !2
  br label %_ZN7sc_core9sc_signalIbE5writeERKb.exit

_ZN7sc_core9sc_signalIbE5writeERKb.exit:          ; preds = %bb30, %bb26, %bb18
  ret void
}

define linkonce_odr void @_ZThn16_NK7sc_core9sc_signalIbE5printERSo(%"class.sc_core::sc_signal.54"* nocapture %this, %"class.std::basic_ostream"* %arg) {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 -1, i32 9
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_reset"** %tmp, i64 10
  %tmp2 = bitcast %"class.sc_core::sc_reset"** %tmp1 to i8*
  %tmp3 = load i8* %tmp2, align 1, !tbaa !4, !range !7
  %tmp4 = icmp ne i8 %tmp3, 0
  %tmp5 = tail call %"class.std::basic_ostream"* @_ZNSo9_M_insertIbEERSoT_(%"class.std::basic_ostream"* %arg, i1 zeroext %tmp4)
  ret void
}

define linkonce_odr void @_ZThn16_NK7sc_core9sc_signalIbE4dumpERSo(%"class.sc_core::sc_signal.54"* nocapture %this, %"class.std::basic_ostream"* %arg) {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 -1, i32 9
  %tmp1 = bitcast %"class.sc_core::sc_reset"** %tmp to %"class.sc_core::sc_signal.54"*
  tail call void @_ZNK7sc_core9sc_signalIbE4dumpERSo(%"class.sc_core::sc_signal.54"* %tmp1, %"class.std::basic_ostream"* %arg)
  ret void
}

define linkonce_odr void @_ZThn16_NK7sc_core9sc_signalIbE5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_signal.54"* nocapture %this, %"class.sc_core::sc_trace_file"* nocapture %tf) {
bb:
  tail call void @_ZN7sc_core19sc_deprecated_traceEv()
  ret void
}

define linkonce_odr i8* @_ZThn16_NK7sc_core9sc_signalIbE4kindEv(%"class.sc_core::sc_signal.54"* nocapture %this) nounwind readnone {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str3, i64 0, i64 0)
}

define available_externally void @_ZThn16_N7sc_core9sc_signalIbED1Ev(%"class.sc_core::sc_signal.54"* %this) {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 -1, i32 9
  %tmp1 = bitcast %"class.sc_core::sc_reset"** %tmp to %"class.sc_core::sc_signal.54"*
  tail call void @_ZN7sc_core9sc_signalIbED1Ev(%"class.sc_core::sc_signal.54"* %tmp1)
  ret void
}

define available_externally void @_ZThn16_N7sc_core9sc_signalIbED0Ev(%"class.sc_core::sc_signal.54"* %this) {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 -1, i32 9
  %tmp1 = bitcast %"class.sc_core::sc_reset"** %tmp to %"class.sc_core::sc_signal.54"*
  tail call void @_ZN7sc_core9sc_signalIbED0Ev(%"class.sc_core::sc_signal.54"* %tmp1)
  ret void
}

define linkonce_odr void @_ZThn16_N7sc_core9sc_signalIbE6updateEv(%"class.sc_core::sc_signal.54"* nocapture %this) {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal.54"* %this, i64 -1, i32 9
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_reset"** %tmp, i64 13
  %tmp2 = bitcast %"class.sc_core::sc_reset"** %tmp1 to i8*
  %tmp3 = load i8* %tmp2, align 1, !tbaa !4, !range !7
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_reset"** %tmp, i64 10
  %tmp5 = bitcast %"class.sc_core::sc_reset"** %tmp4 to i8*
  %tmp6 = load i8* %tmp5, align 1, !tbaa !4, !range !7
  %tmp7 = icmp eq i8 %tmp3, %tmp6
  br i1 %tmp7, label %_ZN7sc_core9sc_signalIbE6updateEv1.exit, label %bb8

bb8:                                              ; preds = %bb
  %tmp9 = alloca %"class.sc_core::sc_event"*, align 8
  %tmp10 = alloca %"class.sc_core::sc_event"*, align 8
  %tmp11 = alloca %"class.sc_core::sc_event"*, align 8
  store i8 %tmp3, i8* %tmp5, align 1, !tbaa !4
  %tmp12 = getelementptr inbounds %"class.sc_core::sc_reset"** %tmp, i64 16
  %tmp13 = load %"class.sc_core::sc_reset"** %tmp12, align 8, !tbaa !2
  %tmp14 = icmp eq %"class.sc_core::sc_reset"* %tmp13, null
  br i1 %tmp14, label %bb16, label %bb15

bb15:                                             ; preds = %bb8
  call void @_ZN7sc_core8sc_reset16notify_processesEv(%"class.sc_core::sc_reset"* %tmp13)
  br label %bb16

bb16:                                             ; preds = %bb15, %bb8
  %tmp17 = getelementptr inbounds %"class.sc_core::sc_reset"** %tmp, i64 9
  %tmp18 = load %"class.sc_core::sc_reset"** %tmp17, align 8
  %tmp19 = bitcast %"class.sc_core::sc_reset"* %tmp18 to %"class.sc_core::sc_event"*
  %tmp20 = icmp eq %"class.sc_core::sc_reset"* %tmp18, null
  br i1 %tmp20, label %._crit_edge.i, label %bb21

bb21:                                             ; preds = %bb16
  %tmp22 = getelementptr inbounds %"class.sc_core::sc_reset"* %tmp18, i64 0, i32 1
  %tmp23 = bitcast %"class.std::vector.49"* %tmp22 to i32*
  %tmp24 = load i32* %tmp23, align 4, !tbaa !9
  %tmp25 = icmp eq i32 %tmp24, 0
  br i1 %tmp25, label %bb27, label %bb26

bb26:                                             ; preds = %bb21
  call void @_ZN7sc_core17sc_report_handler6reportENS_11sc_severityEPKcS3_S3_i(i32 2, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core21SC_ID_NOTIFY_DELAYED_E, i64 0, i64 0), i8* null, i8* getelementptr inbounds ([42 x i8]* @.str4, i64 0, i64 0), i32 260)
  br label %bb27

bb27:                                             ; preds = %bb26, %bb21
  %tmp28 = bitcast %"class.sc_core::sc_reset"* %tmp18 to %"class.sc_core::sc_simcontext"**
  %tmp29 = load %"class.sc_core::sc_simcontext"** %tmp28, align 8, !tbaa !2
  %tmp30 = bitcast %"class.sc_core::sc_event"** %tmp11 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp30)
  store %"class.sc_core::sc_event"* %tmp19, %"class.sc_core::sc_event"** %tmp11, align 8, !tbaa !2
  %tmp31 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp29, i64 0, i32 12
  %tmp32 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp29, i64 0, i32 12, i32 0, i32 0, i32 1
  %tmp33 = load %"class.sc_core::sc_event"*** %tmp32, align 8, !tbaa !2
  %tmp34 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp29, i64 0, i32 12, i32 0, i32 0, i32 2
  %tmp35 = load %"class.sc_core::sc_event"*** %tmp34, align 8, !tbaa !2
  %tmp36 = icmp eq %"class.sc_core::sc_event"** %tmp33, %tmp35
  br i1 %tmp36, label %bb42, label %bb37

bb37:                                             ; preds = %bb27
  %tmp38 = icmp eq %"class.sc_core::sc_event"** %tmp33, null
  br i1 %tmp38, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i.i, label %bb39

bb39:                                             ; preds = %bb37
  store %"class.sc_core::sc_event"* %tmp19, %"class.sc_core::sc_event"** %tmp33, align 8, !tbaa !2
  %.pre.i.i.i.i = load %"class.sc_core::sc_event"*** %tmp32, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i.i

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i.i: ; preds = %bb39, %bb37
  %tmp40 = phi %"class.sc_core::sc_event"** [ null, %bb37 ], [ %.pre.i.i.i.i, %bb39 ]
  %tmp41 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp40, i64 1
  store %"class.sc_core::sc_event"** %tmp41, %"class.sc_core::sc_event"*** %tmp32, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit.i

bb42:                                             ; preds = %bb27
  call void @_ZNSt6vectorIPN7sc_core8sc_eventESaIS2_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS2_S4_EERKS2_(%"class.std::vector.15"* %tmp31, %"class.sc_core::sc_event"** %tmp33, %"class.sc_core::sc_event"** %tmp11)
  %.pre.i.i.i = load %"class.sc_core::sc_event"*** %tmp32, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit.i

_ZN7sc_core8sc_event17notify_next_deltaEv.exit.i: ; preds = %bb42, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i.i
  %tmp43 = phi %"class.sc_core::sc_event"** [ %tmp41, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i.i ], [ %.pre.i.i.i, %bb42 ]
  %tmp44 = getelementptr inbounds %"class.std::vector.15"* %tmp31, i64 0, i32 0, i32 0, i32 0
  %tmp45 = load %"class.sc_core::sc_event"*** %tmp44, align 8, !tbaa !2
  %tmp46 = ptrtoint %"class.sc_core::sc_event"** %tmp43 to i64
  %tmp47 = ptrtoint %"class.sc_core::sc_event"** %tmp45 to i64
  %tmp48 = sub i64 %tmp46, %tmp47
  %tmp49 = lshr exact i64 %tmp48, 3
  %tmp50 = add i64 %tmp49, 4294967295
  %tmp51 = trunc i64 %tmp50 to i32
  call void @llvm.lifetime.end(i64 -1, i8* %tmp30)
  %tmp52 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp19, i64 0, i32 2
  store i32 %tmp51, i32* %tmp52, align 4, !tbaa !6
  store i32 1, i32* %tmp23, align 4, !tbaa !9
  br label %._crit_edge.i

._crit_edge.i:                                    ; preds = %_ZN7sc_core8sc_event17notify_next_deltaEv.exit.i, %bb16
  %tmp53 = load i8* %tmp5, align 1, !tbaa !4, !range !7
  %tmp54 = icmp eq i8 %tmp53, 0
  br i1 %tmp54, label %bb92, label %bb55

bb55:                                             ; preds = %._crit_edge.i
  %tmp56 = getelementptr inbounds %"class.sc_core::sc_reset"** %tmp, i64 15
  %tmp57 = load %"class.sc_core::sc_reset"** %tmp56, align 8
  %tmp58 = bitcast %"class.sc_core::sc_reset"* %tmp57 to %"class.sc_core::sc_event"*
  %tmp59 = icmp eq %"class.sc_core::sc_reset"* %tmp57, null
  br i1 %tmp59, label %bb129, label %bb60

bb60:                                             ; preds = %bb55
  %tmp61 = getelementptr inbounds %"class.sc_core::sc_reset"* %tmp57, i64 0, i32 1
  %tmp62 = bitcast %"class.std::vector.49"* %tmp61 to i32*
  %tmp63 = load i32* %tmp62, align 4, !tbaa !9
  %tmp64 = icmp eq i32 %tmp63, 0
  br i1 %tmp64, label %bb66, label %bb65

bb65:                                             ; preds = %bb60
  call void @_ZN7sc_core17sc_report_handler6reportENS_11sc_severityEPKcS3_S3_i(i32 2, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core21SC_ID_NOTIFY_DELAYED_E, i64 0, i64 0), i8* null, i8* getelementptr inbounds ([42 x i8]* @.str4, i64 0, i64 0), i32 260)
  br label %bb66

bb66:                                             ; preds = %bb65, %bb60
  %tmp67 = bitcast %"class.sc_core::sc_reset"* %tmp57 to %"class.sc_core::sc_simcontext"**
  %tmp68 = load %"class.sc_core::sc_simcontext"** %tmp67, align 8, !tbaa !2
  %tmp69 = bitcast %"class.sc_core::sc_event"** %tmp10 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp69)
  store %"class.sc_core::sc_event"* %tmp58, %"class.sc_core::sc_event"** %tmp10, align 8, !tbaa !2
  %tmp70 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp68, i64 0, i32 12
  %tmp71 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp68, i64 0, i32 12, i32 0, i32 0, i32 1
  %tmp72 = load %"class.sc_core::sc_event"*** %tmp71, align 8, !tbaa !2
  %tmp73 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp68, i64 0, i32 12, i32 0, i32 0, i32 2
  %tmp74 = load %"class.sc_core::sc_event"*** %tmp73, align 8, !tbaa !2
  %tmp75 = icmp eq %"class.sc_core::sc_event"** %tmp72, %tmp74
  br i1 %tmp75, label %bb81, label %bb76

bb76:                                             ; preds = %bb66
  %tmp77 = icmp eq %"class.sc_core::sc_event"** %tmp72, null
  br i1 %tmp77, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2.i, label %bb78

bb78:                                             ; preds = %bb76
  store %"class.sc_core::sc_event"* %tmp58, %"class.sc_core::sc_event"** %tmp72, align 8, !tbaa !2
  %.pre.i.i.i1.i = load %"class.sc_core::sc_event"*** %tmp71, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2.i

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2.i: ; preds = %bb78, %bb76
  %tmp79 = phi %"class.sc_core::sc_event"** [ null, %bb76 ], [ %.pre.i.i.i1.i, %bb78 ]
  %tmp80 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp79, i64 1
  store %"class.sc_core::sc_event"** %tmp80, %"class.sc_core::sc_event"*** %tmp71, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit4.i

bb81:                                             ; preds = %bb66
  call void @_ZNSt6vectorIPN7sc_core8sc_eventESaIS2_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS2_S4_EERKS2_(%"class.std::vector.15"* %tmp70, %"class.sc_core::sc_event"** %tmp72, %"class.sc_core::sc_event"** %tmp10)
  %.pre.i.i3.i = load %"class.sc_core::sc_event"*** %tmp71, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit4.i

_ZN7sc_core8sc_event17notify_next_deltaEv.exit4.i: ; preds = %bb81, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2.i
  %tmp82 = phi %"class.sc_core::sc_event"** [ %tmp80, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i2.i ], [ %.pre.i.i3.i, %bb81 ]
  %tmp83 = getelementptr inbounds %"class.std::vector.15"* %tmp70, i64 0, i32 0, i32 0, i32 0
  %tmp84 = load %"class.sc_core::sc_event"*** %tmp83, align 8, !tbaa !2
  %tmp85 = ptrtoint %"class.sc_core::sc_event"** %tmp82 to i64
  %tmp86 = ptrtoint %"class.sc_core::sc_event"** %tmp84 to i64
  %tmp87 = sub i64 %tmp85, %tmp86
  %tmp88 = lshr exact i64 %tmp87, 3
  %tmp89 = add i64 %tmp88, 4294967295
  %tmp90 = trunc i64 %tmp89 to i32
  call void @llvm.lifetime.end(i64 -1, i8* %tmp69)
  %tmp91 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp58, i64 0, i32 2
  store i32 %tmp90, i32* %tmp91, align 4, !tbaa !6
  store i32 1, i32* %tmp62, align 4, !tbaa !9
  br label %bb129

bb92:                                             ; preds = %._crit_edge.i
  %tmp93 = getelementptr inbounds %"class.sc_core::sc_reset"** %tmp, i64 12
  %tmp94 = load %"class.sc_core::sc_reset"** %tmp93, align 8
  %tmp95 = bitcast %"class.sc_core::sc_reset"* %tmp94 to %"class.sc_core::sc_event"*
  %tmp96 = icmp eq %"class.sc_core::sc_reset"* %tmp94, null
  br i1 %tmp96, label %bb129, label %bb97

bb97:                                             ; preds = %bb92
  %tmp98 = getelementptr inbounds %"class.sc_core::sc_reset"* %tmp94, i64 0, i32 1
  %tmp99 = bitcast %"class.std::vector.49"* %tmp98 to i32*
  %tmp100 = load i32* %tmp99, align 4, !tbaa !9
  %tmp101 = icmp eq i32 %tmp100, 0
  br i1 %tmp101, label %bb103, label %bb102

bb102:                                            ; preds = %bb97
  call void @_ZN7sc_core17sc_report_handler6reportENS_11sc_severityEPKcS3_S3_i(i32 2, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core21SC_ID_NOTIFY_DELAYED_E, i64 0, i64 0), i8* null, i8* getelementptr inbounds ([42 x i8]* @.str4, i64 0, i64 0), i32 260)
  br label %bb103

bb103:                                            ; preds = %bb102, %bb97
  %tmp104 = bitcast %"class.sc_core::sc_reset"* %tmp94 to %"class.sc_core::sc_simcontext"**
  %tmp105 = load %"class.sc_core::sc_simcontext"** %tmp104, align 8, !tbaa !2
  %tmp106 = bitcast %"class.sc_core::sc_event"** %tmp9 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp106)
  store %"class.sc_core::sc_event"* %tmp95, %"class.sc_core::sc_event"** %tmp9, align 8, !tbaa !2
  %tmp107 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp105, i64 0, i32 12
  %tmp108 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp105, i64 0, i32 12, i32 0, i32 0, i32 1
  %tmp109 = load %"class.sc_core::sc_event"*** %tmp108, align 8, !tbaa !2
  %tmp110 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp105, i64 0, i32 12, i32 0, i32 0, i32 2
  %tmp111 = load %"class.sc_core::sc_event"*** %tmp110, align 8, !tbaa !2
  %tmp112 = icmp eq %"class.sc_core::sc_event"** %tmp109, %tmp111
  br i1 %tmp112, label %bb118, label %bb113

bb113:                                            ; preds = %bb103
  %tmp114 = icmp eq %"class.sc_core::sc_event"** %tmp109, null
  br i1 %tmp114, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6.i, label %bb115

bb115:                                            ; preds = %bb113
  store %"class.sc_core::sc_event"* %tmp95, %"class.sc_core::sc_event"** %tmp109, align 8, !tbaa !2
  %.pre.i.i.i5.i = load %"class.sc_core::sc_event"*** %tmp108, align 8, !tbaa !2
  br label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6.i

_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6.i: ; preds = %bb115, %bb113
  %tmp116 = phi %"class.sc_core::sc_event"** [ null, %bb113 ], [ %.pre.i.i.i5.i, %bb115 ]
  %tmp117 = getelementptr inbounds %"class.sc_core::sc_event"** %tmp116, i64 1
  store %"class.sc_core::sc_event"** %tmp117, %"class.sc_core::sc_event"*** %tmp108, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit8.i

bb118:                                            ; preds = %bb103
  call void @_ZNSt6vectorIPN7sc_core8sc_eventESaIS2_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS2_S4_EERKS2_(%"class.std::vector.15"* %tmp107, %"class.sc_core::sc_event"** %tmp109, %"class.sc_core::sc_event"** %tmp9)
  %.pre.i.i7.i = load %"class.sc_core::sc_event"*** %tmp108, align 8, !tbaa !2
  br label %_ZN7sc_core8sc_event17notify_next_deltaEv.exit8.i

_ZN7sc_core8sc_event17notify_next_deltaEv.exit8.i: ; preds = %bb118, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6.i
  %tmp119 = phi %"class.sc_core::sc_event"** [ %tmp117, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core8sc_eventEE9constructEPS3_RKS3_.exit.i.i.i6.i ], [ %.pre.i.i7.i, %bb118 ]
  %tmp120 = getelementptr inbounds %"class.std::vector.15"* %tmp107, i64 0, i32 0, i32 0, i32 0
  %tmp121 = load %"class.sc_core::sc_event"*** %tmp120, align 8, !tbaa !2
  %tmp122 = ptrtoint %"class.sc_core::sc_event"** %tmp119 to i64
  %tmp123 = ptrtoint %"class.sc_core::sc_event"** %tmp121 to i64
  %tmp124 = sub i64 %tmp122, %tmp123
  %tmp125 = lshr exact i64 %tmp124, 3
  %tmp126 = add i64 %tmp125, 4294967295
  %tmp127 = trunc i64 %tmp126 to i32
  call void @llvm.lifetime.end(i64 -1, i8* %tmp106)
  %tmp128 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp95, i64 0, i32 2
  store i32 %tmp127, i32* %tmp128, align 4, !tbaa !6
  store i32 1, i32* %tmp99, align 4, !tbaa !9
  br label %bb129

bb129:                                            ; preds = %_ZN7sc_core8sc_event17notify_next_deltaEv.exit8.i, %bb92, %_ZN7sc_core8sc_event17notify_next_deltaEv.exit4.i, %bb55
  %tmp130 = getelementptr %"class.sc_core::sc_reset"** %tmp, i64 3
  %tmp131 = load %"class.sc_core::sc_reset"** %tmp130, align 8
  %tmp132 = getelementptr inbounds %"class.sc_core::sc_reset"* %tmp131, i64 6, i32 1
  %tmp133 = bitcast %"class.std::vector.49"* %tmp132 to i64*
  %tmp134 = load i64* %tmp133, align 8, !tbaa !5
  %tmp135 = getelementptr inbounds %"class.sc_core::sc_reset"** %tmp, i64 11
  %.c = inttoptr i64 %tmp134 to %"class.sc_core::sc_reset"*
  store %"class.sc_core::sc_reset"* %.c, %"class.sc_core::sc_reset"** %tmp135, align 8, !tbaa !5
  br label %_ZN7sc_core9sc_signalIbE6updateEv1.exit

_ZN7sc_core9sc_signalIbE6updateEv1.exit:          ; preds = %bb129, %bb
  ret void
}

define linkonce_odr void @_ZN7sc_core18sc_signal_inout_ifIbED1Ev(%"class.sc_core::sc_signal_inout_if.55"* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"* %this, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  ret void
}

define linkonce_odr void @_ZN7sc_core18sc_signal_inout_ifIbED0Ev(%"class.sc_core::sc_signal_inout_if.55"* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7sc_core18sc_signal_inout_ifIbED1Ev.exit:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"* %this, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  %tmp1 = bitcast %"class.sc_core::sc_signal_inout_if.55"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp1) nounwind
  ret void
}

define linkonce_odr void @_ZThn8_N7sc_core18sc_signal_inout_ifIbED1Ev(%"class.sc_core::sc_signal_inout_if.55"* %this) {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"* %this, i64 -1, i32 1, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  ret void
}

define linkonce_odr void @_ZThn8_N7sc_core18sc_signal_inout_ifIbED0Ev(%"class.sc_core::sc_signal_inout_if.55"* %this) {
_ZN7sc_core18sc_signal_inout_ifIbED0Ev.exit:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal_inout_if.55"* %this, i64 -1, i32 1
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_signal_write_if.56"* %tmp, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp1)
  %tmp2 = bitcast %"class.sc_core::sc_signal_write_if.56"* %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp2) nounwind
  ret void
}

define linkonce_odr void @_ZN7sc_core15sc_signal_in_ifIbED1Ev(%"class.sc_core::sc_signal_in_if.48"* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  ret void
}

define linkonce_odr void @_ZN7sc_core15sc_signal_in_ifIbED0Ev(%"class.sc_core::sc_signal_in_if.48"* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7sc_core15sc_signal_in_ifIbED1Ev.exit:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal_in_if.48"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  %tmp1 = bitcast %"class.sc_core::sc_signal_in_if.48"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp1) nounwind
  ret void
}

define linkonce_odr void @_ZN7sc_core18sc_signal_write_ifIbED1Ev(%"class.sc_core::sc_signal_write_if.56"* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal_write_if.56"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  ret void
}

define linkonce_odr void @_ZN7sc_core18sc_signal_write_ifIbED0Ev(%"class.sc_core::sc_signal_write_if.56"* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7sc_core18sc_signal_write_ifIbED1Ev.exit:
  %tmp = getelementptr inbounds %"class.sc_core::sc_signal_write_if.56"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  %tmp1 = bitcast %"class.sc_core::sc_signal_write_if.56"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp1) nounwind
  ret void
}

define linkonce_odr void @_ZTv0_n40_N7sc_core18sc_signal_write_ifIbED1Ev(%"class.sc_core::sc_signal_write_if.56"* %this) {
bb:
  %tmp = bitcast %"class.sc_core::sc_signal_write_if.56"* %this to i8*
  %tmp1 = bitcast %"class.sc_core::sc_signal_write_if.56"* %this to i8**
  %tmp2 = load i8** %tmp1, align 8
  %tmp3 = getelementptr inbounds i8* %tmp2, i64 -40
  %tmp4 = bitcast i8* %tmp3 to i64*
  %tmp5 = load i64* %tmp4, align 8
  %tmp6 = getelementptr inbounds i8* %tmp, i64 %tmp5
  %tmp7 = bitcast i8* %tmp6 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp7)
  ret void
}

define linkonce_odr void @_ZTv0_n40_N7sc_core18sc_signal_write_ifIbED0Ev(%"class.sc_core::sc_signal_write_if.56"* %this) {
_ZN7sc_core18sc_signal_write_ifIbED0Ev.exit:
  %tmp = bitcast %"class.sc_core::sc_signal_write_if.56"* %this to i8*
  %tmp1 = bitcast %"class.sc_core::sc_signal_write_if.56"* %this to i8**
  %tmp2 = load i8** %tmp1, align 8
  %tmp3 = getelementptr inbounds i8* %tmp2, i64 -40
  %tmp4 = bitcast i8* %tmp3 to i64*
  %tmp5 = load i64* %tmp4, align 8
  %tmp6 = getelementptr inbounds i8* %tmp, i64 %tmp5
  %tmp7 = bitcast i8* %tmp6 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp7)
  tail call void @_ZdlPv(i8* %tmp6) nounwind
  ret void
}

declare void @_ZN7sc_core8sc_reset16notify_processesEv(%"class.sc_core::sc_reset"*)

declare %"class.std::basic_ostream"* @_ZNSo9_M_insertIbEERSoT_(%"class.std::basic_ostream"*, i1 zeroext)

declare void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"*)

define internal void @_GLOBAL__I_a() section ".text.startup" {
bb:
  tail call void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"* @_ZStL8__ioinit)
  %tmp = tail call i32 @__cxa_atexit(void (i8*)* bitcast (void (%"class.std::ios_base::Init"*)* @_ZNSt8ios_base4InitD1Ev to void (i8*)*), i8* getelementptr inbounds (%"class.std::ios_base::Init"* @_ZStL8__ioinit, i64 0, i32 0), i8* @__dso_handle)
  tail call void @_ZN7sc_core20sc_api_version_2_2_0C1Ev(%"class.sc_core::sc_api_version_2_2_0"* @_ZN7sc_coreL17api_version_checkE)
  ret void
}

declare void @llvm.lifetime.start(i64, i8* nocapture) nounwind

declare void @llvm.lifetime.end(i64, i8* nocapture) nounwind

declare void @llvm.memset.p0i8.i64(i8* nocapture, i8, i64, i32, i1) nounwind

declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) nounwind readnone

declare i8* @llvm.stacksave() nounwind

declare void @llvm.stackrestore(i8*) nounwind

!0 = metadata !{metadata !"vtable pointer", metadata !1}
!1 = metadata !{metadata !"Simple C/C++ TBAA"}
!2 = metadata !{metadata !"any pointer", metadata !3}
!3 = metadata !{metadata !"omnipotent char", metadata !1}
!4 = metadata !{metadata !"bool", metadata !3}
!5 = metadata !{metadata !"long long", metadata !3}
!6 = metadata !{metadata !"int", metadata !3}
!7 = metadata !{i8 0, i8 2}                       
!8 = metadata !{metadata !"branch_weights", i32 64, i32 4}
!9 = metadata !{metadata !"_ZTSN7sc_core8sc_event8notify_tE", metadata !3}
!10 = metadata !{metadata !"_ZTSSt12_Ios_Iostate", metadata !3}
