; ModuleID = 'jerome-chaino.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%"class.std::ios_base::Init" = type { i8 }
%"class.sc_core::sc_api_version_2_2_0" = type { i8 }
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
%"class.std::basic_stringstream" = type { %"class.std::basic_iostream.base", %"class.std::basic_stringbuf", %"class.std::basic_ios" }
%"class.std::basic_iostream.base" = type { %"class.std::basic_istream.base", %"class.std::basic_ostream.base" }
%"class.std::basic_istream.base" = type { i32 (...)**, i64 }
%"class.std::basic_ostream.base" = type { i32 (...)** }
%"class.std::basic_stringbuf" = type { %"class.std::basic_streambuf", i32, %"class.std::basic_string" }
%struct.Source = type { %"class.sc_core::sc_module", %struct.MyModule* }
%"class.sc_core::sc_module" = type { %"class.sc_core::sc_object", %"class.sc_core::sc_process_host", %"class.sc_core::sc_sensitive", %"class.sc_core::sc_sensitive_pos", %"class.sc_core::sc_sensitive_neg", i8, %"class.std::vector"*, i32, %"class.sc_core::sc_name_gen"*, %"class.std::vector.10", %"class.sc_core::sc_module_name"* }
%"class.sc_core::sc_sensitive" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_pos" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_neg" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_module_name" = type { i8*, %"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*, %"class.sc_core::sc_simcontext"*, i8 }
%struct.MyModule = type { %"class.sc_core::sc_module", %"class.sc_core::sc_event", %struct.MyModule* }
%"class.std::basic_istream" = type { i32 (...)**, i64, %"class.std::basic_ios" }
%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep" = type { %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep_base" }
%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep_base" = type { i64, i64, i32 }
%"class.sc_core::sc_process_handle" = type { %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_spawn_options" = type opaque

@_ZStL8__ioinit = internal global %"class.std::ios_base::Init" zeroinitializer, align 1
@__dso_handle = external global i8
@_ZN7sc_coreL17api_version_checkE = internal global %"class.sc_core::sc_api_version_2_2_0" zeroinitializer, align 1
@_ZTVN10__cxxabiv121__vmi_class_type_infoE = external global i8*
@_ZTVN10__cxxabiv117__class_type_infoE = external global i8*
@_ZSt4cout = external global %"class.std::basic_ostream"
@.str = private unnamed_addr constant [10 x i8] c"Start : \0A\00", align 1
@_ZSt4cerr = external global %"class.std::basic_ostream"
@.str2 = private unnamed_addr constant [40 x i8] c"Number of modules should be at least 3.\00", align 1
@.str3 = private unnamed_addr constant [21 x i8] c"Number of modules : \00", align 1
@.str4 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str5 = private unnamed_addr constant [31 x i8] c"usage: run.x number_of_modules\00", align 1
@.str6 = private unnamed_addr constant [7 x i8] c"Source\00", align 1
@.str7 = private unnamed_addr constant [9 x i8] c"MyModule\00", align 1
@.str8 = private unnamed_addr constant [25 x i8] c"Address of new module : \00", align 1
@.str9 = private unnamed_addr constant [20 x i8] c"Address of event : \00", align 1
@.str10 = private unnamed_addr constant [5 x i8] c"Sink\00", align 1
@.str11 = private unnamed_addr constant [19 x i8] c"Address of sink : \00", align 1
@.str12 = private unnamed_addr constant [25 x i8] c"Address of sink event : \00", align 1
@_ZTVN10__cxxabiv120__si_class_type_infoE = external global i8*
@_ZTSN7sc_core9sc_objectE = available_externally constant [21 x i8] c"N7sc_core9sc_objectE\00"
@_ZTIN7sc_core9sc_objectE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_objectE, i32 0, i32 0) }
@.str13 = private unnamed_addr constant [42 x i8] c"basic_string::_S_construct null not valid\00", align 1
@_ZNSs4_Rep11_S_terminalE = external constant i8
@_ZNSs4_Rep20_S_empty_rep_storageE = external global [0 x i64]
@_ZTVSt18basic_stringstreamIcSt11char_traitsIcESaIcEE = external unnamed_addr constant [15 x i8*]
@_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE = external unnamed_addr constant [10 x i8*]
@_ZTVSt15basic_stringbufIcSt11char_traitsIcESaIcEE = external unnamed_addr constant [16 x i8*]
@_ZTVSt15basic_streambufIcSt11char_traitsIcEE = external unnamed_addr constant [16 x i8*]
@_ZTVSt9basic_iosIcSt11char_traitsIcEE = external unnamed_addr constant [4 x i8*]
@_ZTV8MyModule = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI8MyModule to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%struct.MyModule*)* @_ZN8MyModuleD1Ev to i8*), i8* bitcast (void (%struct.MyModule*)* @_ZN8MyModuleD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI8MyModule to i8*), i8* bitcast (void (%struct.MyModule*)* @_ZThn40_N8MyModuleD1Ev to i8*), i8* bitcast (void (%struct.MyModule*)* @_ZThn40_N8MyModuleD0Ev to i8*)]
@_ZTS8MyModule = linkonce_odr constant [10 x i8] c"8MyModule\00"
@_ZTSN7sc_core9sc_moduleE = available_externally constant [21 x i8] c"N7sc_core9sc_moduleE\00"
@_ZTSN7sc_core15sc_process_hostE = linkonce_odr constant [28 x i8] c"N7sc_core15sc_process_hostE\00"
@_ZTIN7sc_core15sc_process_hostE = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_process_hostE, i32 0, i32 0) }
@_ZTIN7sc_core9sc_moduleE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_moduleE, i32 0, i32 0), i32 0, i32 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*), i64 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core15sc_process_hostE to i8*), i64 10242 }
@_ZTI8MyModule = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([10 x i8]* @_ZTS8MyModule, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@.str20 = private unnamed_addr constant [10 x i8] c"sc_module\00", align 1
@.str21 = private unnamed_addr constant [8 x i8] c"compute\00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str22 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str23 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str24 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_increment()\00", align 1
@_ZTV6Source = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI6Source to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%struct.Source*)* @_ZN6SourceD1Ev to i8*), i8* bitcast (void (%struct.Source*)* @_ZN6SourceD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI6Source to i8*), i8* bitcast (void (%struct.Source*)* @_ZThn40_N6SourceD1Ev to i8*), i8* bitcast (void (%struct.Source*)* @_ZThn40_N6SourceD0Ev to i8*)]
@_ZTS6Source = linkonce_odr constant [8 x i8] c"6Source\00"
@_ZTI6Source = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([8 x i8]* @_ZTS6Source, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@_ZN7sc_core18sc_curr_simcontextE = external global %"class.sc_core::sc_simcontext"*
@_ZN7sc_core25sc_default_global_contextE = external global %"class.sc_core::sc_simcontext"*
@llvm.global_ctors = appending global [1 x { i32, void ()* }] [{ i32, void ()* } { i32 65535, void ()* @_GLOBAL__I_a }]

declare void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"*)

declare void @_ZNSt8ios_base4InitD1Ev(%"class.std::ios_base::Init"*)

declare i32 @__cxa_atexit(void (i8*)*, i8*, i8*) nounwind

declare void @_ZN7sc_core20sc_api_version_2_2_0C1Ev(%"class.sc_core::sc_api_version_2_2_0"*)

declare i32 @__gxx_personality_v0(...)

declare void @_ZSt9terminatev()

define i32 @sc_main(i32 %argc, i8** nocapture %argv) uwtable {
bb:
  %tmp = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([10 x i8]* @.str, i64 0, i64 0), i64 9)
  %tmp1 = icmp sgt i32 %argc, 1
  br i1 %tmp1, label %bb2, label %bb146

bb2:                                              ; preds = %bb
  %tmp3 = alloca %"class.std::allocator.40", align 1
  %tmp4 = alloca %"class.std::allocator.40", align 1
  %tmp5 = alloca %"class.std::allocator.40", align 1
  %tmp6 = alloca %"class.std::allocator.40", align 1
  %nb_modules = alloca i32, align 4
  %ss = alloca %"class.std::basic_stringstream", align 8
  %source = alloca %struct.Source, align 8
  %tmp7 = alloca %"class.sc_core::sc_module_name", align 8
  %ss1 = alloca %"class.std::basic_stringstream", align 8
  %tmp8 = alloca %"class.sc_core::sc_module_name", align 8
  %tmp9 = alloca %"class.std::basic_string", align 8
  %sink = alloca %struct.MyModule, align 8
  %tmp10 = alloca %"class.sc_core::sc_module_name", align 8
  call void @_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEEC1ESt13_Ios_Openmode(%"class.std::basic_stringstream"* %ss, i32 24)
  %tmp11 = getelementptr %"class.std::basic_stringstream"* %ss, i64 0, i32 0, i32 1
  %tmp12 = getelementptr inbounds i8** %argv, i64 1
  %tmp13 = load i8** %tmp12, align 8, !tbaa !0
  %tmp14 = icmp eq i8* %tmp13, null
  br i1 %tmp14, label %bb15, label %bb28

bb15:                                             ; preds = %bb2
  %tmp16 = bitcast %"class.std::basic_ostream.base"* %tmp11 to i8**
  %tmp17 = load i8** %tmp16, align 8, !tbaa !3
  %tmp18 = getelementptr i8* %tmp17, i64 -24
  %tmp19 = bitcast i8* %tmp18 to i64*
  %tmp20 = load i64* %tmp19, align 8
  %tmp21 = bitcast %"class.std::basic_ostream.base"* %tmp11 to i8*
  %tmp22 = getelementptr i8* %tmp21, i64 %tmp20
  %tmp23 = bitcast i8* %tmp22 to %"class.std::basic_ios"*
  %.sum.i = add i64 %tmp20, 32
  %tmp24 = getelementptr inbounds i8* %tmp21, i64 %.sum.i
  %tmp25 = bitcast i8* %tmp24 to i32*
  %tmp26 = load i32* %tmp25, align 4, !tbaa !4
  %tmp27 = or i32 %tmp26, 1
  call void @_ZNSt9basic_iosIcSt11char_traitsIcEE5clearESt12_Ios_Iostate(%"class.std::basic_ios"* %tmp23, i32 %tmp27)
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit

bb28:                                             ; preds = %bb2
  %tmp29 = bitcast %"class.std::basic_ostream.base"* %tmp11 to %"class.std::basic_ostream"*
  %tmp30 = call i64 @strlen(i8* %tmp13) nounwind
  %tmp31 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp29, i8* %tmp13, i64 %tmp30)
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit: ; preds = %bb28, %bb15
  %tmp32 = bitcast %"class.std::basic_stringstream"* %ss to %"class.std::basic_istream"*
  %tmp33 = call %"class.std::basic_istream"* @_ZNSirsERi(%"class.std::basic_istream"* %tmp32, i32* %nb_modules)
  %tmp34 = load i32* %nb_modules, align 4, !tbaa !5
  %tmp35 = icmp slt i32 %tmp34, 3
  br i1 %tmp35, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit17, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit25

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit17: ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit
  %tmp36 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cerr, i8* getelementptr inbounds ([40 x i8]* @.str2, i64 0, i64 0), i64 39)
  %tmp37 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cerr to i8**), align 8, !tbaa !3
  %tmp38 = getelementptr i8* %tmp37, i64 -24
  %tmp39 = bitcast i8* %tmp38 to i64*
  %tmp40 = load i64* %tmp39, align 8
  %.sum104 = add i64 %tmp40, 240
  %tmp41 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cerr to i8*), i64 %.sum104
  %tmp42 = bitcast i8* %tmp41 to %"class.std::ctype"**
  %tmp43 = load %"class.std::ctype"** %tmp42, align 8, !tbaa !0
  %tmp44 = icmp eq %"class.std::ctype"* %tmp43, null
  br i1 %tmp44, label %bb45, label %.noexc94

bb45:                                             ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit17
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

.noexc94:                                         ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit17
  %tmp46 = getelementptr inbounds %"class.std::ctype"* %tmp43, i64 0, i32 6
  %tmp47 = load i8* %tmp46, align 1, !tbaa !1
  %tmp48 = icmp eq i8 %tmp47, 0
  br i1 %tmp48, label %.noexc96, label %bb49

bb49:                                             ; preds = %.noexc94
  %tmp50 = getelementptr inbounds %"class.std::ctype"* %tmp43, i64 0, i32 7, i64 10
  %tmp51 = load i8* %tmp50, align 1, !tbaa !1
  br label %.noexc19

.noexc96:                                         ; preds = %.noexc94
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp43)
  %tmp52 = bitcast %"class.std::ctype"* %tmp43 to i8 (%"class.std::ctype"*, i8)***
  %tmp53 = load i8 (%"class.std::ctype"*, i8)*** %tmp52, align 8, !tbaa !3
  %tmp54 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp53, i64 6
  %tmp55 = load i8 (%"class.std::ctype"*, i8)** %tmp54, align 8
  %tmp56 = call signext i8 %tmp55(%"class.std::ctype"* %tmp43, i8 signext 10)
  br label %.noexc19

.noexc19:                                         ; preds = %.noexc96, %bb49
  %.0.i = phi i8 [ %tmp51, %bb49 ], [ %tmp56, %.noexc96 ]
  %tmp57 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* @_ZSt4cerr, i8 signext %.0.i)
  %tmp58 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp57)
  %tmp59 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 0), align 8
  %tmp60 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 0, i32 0, i32 0
  %.c.i.i88 = bitcast i8* %tmp59 to i32 (...)**
  store i32 (...)** %.c.i.i88, i32 (...)*** %tmp60, align 8, !tbaa !3
  %tmp61 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 8), align 8
  %tmp62 = getelementptr i8* %tmp59, i64 -24
  %tmp63 = bitcast i8* %tmp62 to i64*
  %tmp64 = load i64* %tmp63, align 8
  %tmp65 = bitcast %"class.std::basic_stringstream"* %ss to i8*
  %tmp66 = getelementptr i8* %tmp65, i64 %tmp64
  %tmp67 = bitcast i8* %tmp66 to i8**
  store i8* %tmp61, i8** %tmp67, align 8, !tbaa !3
  %tmp68 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 9), align 8
  %tmp69 = getelementptr %"class.std::basic_stringstream"* %ss, i64 0, i32 0, i32 1, i32 0
  %.c1.i.i89 = bitcast i8* %tmp68 to i32 (...)**
  store i32 (...)** %.c1.i.i89, i32 (...)*** %tmp69, align 8, !tbaa !3
  %tmp70 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([16 x i8*]* @_ZTVSt15basic_stringbufIcSt11char_traitsIcESaIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp70, align 8, !tbaa !3
  %tmp71 = getelementptr inbounds %"class.std::allocator.40"* %tmp3, i64 0, i32 0
  call void @llvm.lifetime.start(i64 -1, i8* %tmp71) nounwind
  %tmp72 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 1, i32 2, i32 0, i32 0
  %tmp73 = load i8** %tmp72, align 8, !tbaa !0
  %tmp74 = getelementptr inbounds i8* %tmp73, i64 -24
  %tmp75 = icmp eq i8* %tmp74, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp75, label %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit93, label %bb262, !prof !6

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit25: ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit
  %tmp76 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([21 x i8]* @.str3, i64 0, i64 0), i64 20)
  %tmp77 = load i32* %nb_modules, align 4, !tbaa !5
  %tmp78 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp77)
  %tmp79 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp78, i8* getelementptr inbounds ([2 x i8]* @.str4, i64 0, i64 0), i64 1)
  %tmp80 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 0), align 8
  %tmp81 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 0, i32 0, i32 0
  %.c.i.i28 = bitcast i8* %tmp80 to i32 (...)**
  store i32 (...)** %.c.i.i28, i32 (...)*** %tmp81, align 8, !tbaa !3
  %tmp82 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 8), align 8
  %tmp83 = getelementptr i8* %tmp80, i64 -24
  %tmp84 = bitcast i8* %tmp83 to i64*
  %tmp85 = load i64* %tmp84, align 8
  %tmp86 = bitcast %"class.std::basic_stringstream"* %ss to i8*
  %tmp87 = getelementptr i8* %tmp86, i64 %tmp85
  %tmp88 = bitcast i8* %tmp87 to i8**
  store i8* %tmp82, i8** %tmp88, align 8, !tbaa !3
  %tmp89 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 9), align 8
  %tmp90 = getelementptr %"class.std::basic_stringstream"* %ss, i64 0, i32 0, i32 1, i32 0
  %.c1.i.i29 = bitcast i8* %tmp89 to i32 (...)**
  store i32 (...)** %.c1.i.i29, i32 (...)*** %tmp90, align 8, !tbaa !3
  %tmp91 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([16 x i8*]* @_ZTVSt15basic_stringbufIcSt11char_traitsIcESaIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp91, align 8, !tbaa !3
  %tmp92 = getelementptr inbounds %"class.std::allocator.40"* %tmp6, i64 0, i32 0
  call void @llvm.lifetime.start(i64 -1, i8* %tmp92) nounwind
  %tmp93 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 1, i32 2, i32 0, i32 0
  %tmp94 = load i8** %tmp93, align 8, !tbaa !0
  %tmp95 = getelementptr inbounds i8* %tmp94, i64 -24
  %tmp96 = icmp eq i8* %tmp95, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp96, label %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit33, label %bb97, !prof !6

bb97:                                             ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit25
  %tmp98 = getelementptr inbounds i8* %tmp94, i64 -8
  %tmp99 = bitcast i8* %tmp98 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb100, label %bb102

bb100:                                            ; preds = %bb97
  %tmp101 = atomicrmw add i32* %tmp99, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i31

bb102:                                            ; preds = %bb97
  %tmp103 = load i32* %tmp99, align 4, !tbaa !5
  %tmp104 = add nsw i32 %tmp103, -1
  store i32 %tmp104, i32* %tmp99, align 4, !tbaa !5
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i31

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i31: ; preds = %bb102, %bb100
  %.0.i.i.i.i.i.i.i.i30 = phi i32 [ %tmp101, %bb100 ], [ %tmp103, %bb102 ]
  %tmp105 = icmp slt i32 %.0.i.i.i.i.i.i.i.i30, 1
  br i1 %tmp105, label %bb106, label %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit33

bb106:                                            ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i31
  %tmp107 = bitcast i8* %tmp95 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp107, %"class.std::allocator.40"* %tmp6) nounwind
  br label %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit33

_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit33: ; preds = %bb106, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i31, %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit25
  call void @llvm.lifetime.end(i64 -1, i8* %tmp92) nounwind
  store i32 (...)** bitcast (i8** getelementptr inbounds ([16 x i8*]* @_ZTVSt15basic_streambufIcSt11char_traitsIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp91, align 8, !tbaa !3
  %tmp108 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 1, i32 0, i32 7
  call void @_ZNSt6localeD1Ev(%"class.std::locale"* %tmp108) nounwind
  %tmp109 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 2), align 8
  %.c.i.i.i.i32 = bitcast i8* %tmp109 to i32 (...)**
  store i32 (...)** %.c.i.i.i.i32, i32 (...)*** %tmp81, align 8, !tbaa !3
  %tmp110 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 3), align 8
  %tmp111 = getelementptr i8* %tmp109, i64 -24
  %tmp112 = bitcast i8* %tmp111 to i64*
  %tmp113 = load i64* %tmp112, align 8
  %tmp114 = getelementptr i8* %tmp86, i64 %tmp113
  %tmp115 = bitcast i8* %tmp114 to i8**
  store i8* %tmp110, i8** %tmp115, align 8, !tbaa !3
  %tmp116 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 0, i32 0, i32 1
  store i64 0, i64* %tmp116, align 8, !tbaa !7
  %tmp117 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 2, i32 0
  call void @_ZNSt8ios_baseD2Ev(%"class.std::ios_base"* %tmp117)
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp7, i8* getelementptr inbounds ([7 x i8]* @.str6, i64 0, i64 0))
  call void @_ZN6SourceC2EN7sc_core14sc_module_nameEi(%struct.Source* %source, %"class.sc_core::sc_module_name"* %tmp7, i32 undef)
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp7)
  %tmp118 = load i32* %nb_modules, align 4, !tbaa !5
  %tmp119 = add nsw i32 %tmp118, -2
  %tmp120 = sext i32 %tmp119 to i64
  %tmp121 = call { i64, i1 } @llvm.umul.with.overflow.i64(i64 %tmp120, i64 8)
  %tmp122 = extractvalue { i64, i1 } %tmp121, 1
  %tmp123 = extractvalue { i64, i1 } %tmp121, 0
  %tmp124 = select i1 %tmp122, i64 -1, i64 %tmp123
  %tmp125 = call noalias i8* @_Znam(i64 %tmp124)
  %tmp126 = bitcast i8* %tmp125 to %struct.MyModule**
  %tmp127 = getelementptr %"class.std::basic_stringstream"* %ss1, i64 0, i32 0, i32 1
  %tmp128 = bitcast %"class.std::basic_ostream.base"* %tmp127 to %"class.std::basic_ostream"*
  %tmp129 = getelementptr inbounds %"class.std::basic_stringstream"* %ss1, i64 0, i32 1
  %tmp130 = getelementptr inbounds %"class.std::basic_string"* %tmp9, i64 0, i32 0, i32 0
  %tmp131 = getelementptr inbounds %"class.std::allocator.40"* %tmp5, i64 0, i32 0
  %tmp132 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 0), align 8
  %tmp133 = getelementptr inbounds %"class.std::basic_stringstream"* %ss1, i64 0, i32 0, i32 0, i32 0
  %.c.i.i50 = bitcast i8* %tmp132 to i32 (...)**
  %tmp134 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 8), align 8
  %tmp135 = getelementptr i8* %tmp132, i64 -24
  %tmp136 = bitcast i8* %tmp135 to i64*
  %tmp137 = bitcast %"class.std::basic_stringstream"* %ss1 to i8*
  %tmp138 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 9), align 8
  %tmp139 = getelementptr %"class.std::basic_stringstream"* %ss1, i64 0, i32 0, i32 1, i32 0
  %.c1.i.i51 = bitcast i8* %tmp138 to i32 (...)**
  %tmp140 = getelementptr inbounds %"class.std::basic_stringstream"* %ss1, i64 0, i32 1, i32 0, i32 0
  %tmp141 = getelementptr inbounds %"class.std::allocator.40"* %tmp4, i64 0, i32 0
  %tmp142 = getelementptr inbounds %"class.std::basic_stringstream"* %ss1, i64 0, i32 1, i32 2, i32 0, i32 0
  %tmp143 = getelementptr inbounds %"class.std::basic_stringstream"* %ss1, i64 0, i32 1, i32 0, i32 7
  %tmp144 = getelementptr inbounds %"class.std::basic_stringstream"* %ss1, i64 0, i32 0, i32 0, i32 1
  %tmp145 = getelementptr inbounds %"class.std::basic_stringstream"* %ss1, i64 0, i32 2, i32 0
  br label %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit56

bb146:                                            ; preds = %bb
  %tmp147 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cerr, i8* getelementptr inbounds ([31 x i8]* @.str5, i64 0, i64 0), i64 30)
  %tmp148 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cerr to i8**), align 8, !tbaa !3
  %tmp149 = getelementptr i8* %tmp148, i64 -24
  %tmp150 = bitcast i8* %tmp149 to i64*
  %tmp151 = load i64* %tmp150, align 8
  %.sum = add i64 %tmp151, 240
  %tmp152 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cerr to i8*), i64 %.sum
  %tmp153 = bitcast i8* %tmp152 to %"class.std::ctype"**
  %tmp154 = load %"class.std::ctype"** %tmp153, align 8, !tbaa !0
  %tmp155 = icmp eq %"class.std::ctype"* %tmp154, null
  br i1 %tmp155, label %bb156, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit101

bb156:                                            ; preds = %bb146
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit101: ; preds = %bb146
  %tmp157 = getelementptr inbounds %"class.std::ctype"* %tmp154, i64 0, i32 6
  %tmp158 = load i8* %tmp157, align 1, !tbaa !1
  %tmp159 = icmp eq i8 %tmp158, 0
  br i1 %tmp159, label %bb163, label %bb160

bb160:                                            ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit101
  %tmp161 = getelementptr inbounds %"class.std::ctype"* %tmp154, i64 0, i32 7, i64 10
  %tmp162 = load i8* %tmp161, align 1, !tbaa !1
  br label %_ZNKSt5ctypeIcE5widenEc.exit99

bb163:                                            ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit101
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp154)
  %tmp164 = bitcast %"class.std::ctype"* %tmp154 to i8 (%"class.std::ctype"*, i8)***
  %tmp165 = load i8 (%"class.std::ctype"*, i8)*** %tmp164, align 8, !tbaa !3
  %tmp166 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp165, i64 6
  %tmp167 = load i8 (%"class.std::ctype"*, i8)** %tmp166, align 8
  %tmp168 = call signext i8 %tmp167(%"class.std::ctype"* %tmp154, i8 signext 10)
  br label %_ZNKSt5ctypeIcE5widenEc.exit99

_ZNKSt5ctypeIcE5widenEc.exit99:                   ; preds = %bb163, %bb160
  %.0.i98 = phi i8 [ %tmp162, %bb160 ], [ %tmp168, %bb163 ]
  %tmp169 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* @_ZSt4cerr, i8 signext %.0.i98)
  %tmp170 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp169)
  br label %bb283

_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit56: ; preds = %bb223, %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit33
  %indvars.iv112 = phi i64 [ %indvars.iv.next113, %bb223 ], [ 0, %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit33 ]
  %tmp171 = load i32* %nb_modules, align 4, !tbaa !5
  %tmp172 = add nsw i32 %tmp171, -2
  %tmp173 = trunc i64 %indvars.iv112 to i32
  %tmp174 = icmp slt i32 %tmp173, %tmp172
  br i1 %tmp174, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit36, label %_ZN8MyModuleC1EN7sc_core14sc_module_nameE.exit68

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit36: ; preds = %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit56
  call void @_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEEC1ESt13_Ios_Openmode(%"class.std::basic_stringstream"* %ss1, i32 24)
  %tmp175 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp128, i8* getelementptr inbounds ([9 x i8]* @.str7, i64 0, i64 0), i64 8)
  %indvars.iv.next113 = add i64 %indvars.iv112, 1
  %tmp176 = trunc i64 %indvars.iv.next113 to i32
  %tmp177 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp128, i32 %tmp176)
  %tmp178 = call noalias i8* @_Znwm(i64 312)
  %tmp179 = bitcast i8* %tmp178 to %struct.MyModule*
  call void @_ZNKSt15basic_stringbufIcSt11char_traitsIcESaIcEE3strEv(%"class.std::basic_string"* sret %tmp9, %"class.std::basic_stringbuf"* %tmp129)
  %tmp180 = load i8** %tmp130, align 8, !tbaa !0
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp8, i8* %tmp180)
  call void @_ZN8MyModuleC2EN7sc_core14sc_module_nameE(%struct.MyModule* %tmp179, %"class.sc_core::sc_module_name"* %tmp8)
  %tmp181 = getelementptr inbounds %struct.MyModule** %tmp126, i64 %indvars.iv112
  store %struct.MyModule* %tmp179, %struct.MyModule** %tmp181, align 8, !tbaa !0
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp8)
  call void @llvm.lifetime.start(i64 -1, i8* %tmp131) nounwind
  %tmp182 = load i8** %tmp130, align 8, !tbaa !0
  %tmp183 = getelementptr inbounds i8* %tmp182, i64 -24
  %tmp184 = icmp eq i8* %tmp183, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp184, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit40, label %bb185, !prof !6

bb185:                                            ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit36
  %tmp186 = getelementptr inbounds i8* %tmp182, i64 -8
  %tmp187 = bitcast i8* %tmp186 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb188, label %bb190

bb188:                                            ; preds = %bb185
  %tmp189 = atomicrmw add i32* %tmp187, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i

bb190:                                            ; preds = %bb185
  %tmp191 = load i32* %tmp187, align 4, !tbaa !5
  %tmp192 = add nsw i32 %tmp191, -1
  store i32 %tmp192, i32* %tmp187, align 4, !tbaa !5
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i: ; preds = %bb190, %bb188
  %.0.i.i.i.i = phi i32 [ %tmp189, %bb188 ], [ %tmp191, %bb190 ]
  %tmp193 = icmp slt i32 %.0.i.i.i.i, 1
  br i1 %tmp193, label %bb194, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit40

bb194:                                            ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i
  %tmp195 = bitcast i8* %tmp183 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp195, %"class.std::allocator.40"* %tmp5) nounwind
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit40

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit40: ; preds = %bb194, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i, %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit36
  call void @llvm.lifetime.end(i64 -1, i8* %tmp131) nounwind
  %tmp196 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([25 x i8]* @.str8, i64 0, i64 0), i64 24)
  %tmp197 = load %struct.MyModule** %tmp181, align 8, !tbaa !0
  %tmp198 = bitcast %struct.MyModule* %tmp197 to i8*
  %tmp199 = call %"class.std::basic_ostream"* @_ZNSo9_M_insertIPKvEERSoT_(%"class.std::basic_ostream"* @_ZSt4cout, i8* %tmp198)
  %tmp200 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp199, i8* getelementptr inbounds ([2 x i8]* @.str4, i64 0, i64 0), i64 1)
  %tmp201 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([20 x i8]* @.str9, i64 0, i64 0), i64 19)
  %tmp202 = getelementptr inbounds %struct.MyModule* %tmp197, i64 0, i32 1
  %tmp203 = bitcast %"class.sc_core::sc_event"* %tmp202 to i8*
  %tmp204 = call %"class.std::basic_ostream"* @_ZNSo9_M_insertIPKvEERSoT_(%"class.std::basic_ostream"* @_ZSt4cout, i8* %tmp203)
  %tmp205 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp204, i8* getelementptr inbounds ([2 x i8]* @.str4, i64 0, i64 0), i64 1)
  store i32 (...)** %.c.i.i50, i32 (...)*** %tmp133, align 8, !tbaa !3
  %tmp206 = load i64* %tmp136, align 8
  %tmp207 = getelementptr i8* %tmp137, i64 %tmp206
  %tmp208 = bitcast i8* %tmp207 to i8**
  store i8* %tmp134, i8** %tmp208, align 8, !tbaa !3
  store i32 (...)** %.c1.i.i51, i32 (...)*** %tmp139, align 8, !tbaa !3
  store i32 (...)** bitcast (i8** getelementptr inbounds ([16 x i8*]* @_ZTVSt15basic_stringbufIcSt11char_traitsIcESaIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp140, align 8, !tbaa !3
  call void @llvm.lifetime.start(i64 -1, i8* %tmp141) nounwind
  %tmp209 = load i8** %tmp142, align 8, !tbaa !0
  %tmp210 = getelementptr inbounds i8* %tmp209, i64 -24
  %tmp211 = icmp eq i8* %tmp210, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp211, label %bb223, label %bb212, !prof !6

bb212:                                            ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit40
  %tmp213 = getelementptr inbounds i8* %tmp209, i64 -8
  %tmp214 = bitcast i8* %tmp213 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb215, label %bb217

bb215:                                            ; preds = %bb212
  %tmp216 = atomicrmw add i32* %tmp214, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i53

bb217:                                            ; preds = %bb212
  %tmp218 = load i32* %tmp214, align 4, !tbaa !5
  %tmp219 = add nsw i32 %tmp218, -1
  store i32 %tmp219, i32* %tmp214, align 4, !tbaa !5
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i53

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i53: ; preds = %bb217, %bb215
  %.0.i.i.i.i.i.i.i.i52 = phi i32 [ %tmp216, %bb215 ], [ %tmp218, %bb217 ]
  %tmp220 = icmp slt i32 %.0.i.i.i.i.i.i.i.i52, 1
  br i1 %tmp220, label %bb221, label %bb223

bb221:                                            ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i53
  %tmp222 = bitcast i8* %tmp210 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp222, %"class.std::allocator.40"* %tmp4) nounwind
  br label %bb223

bb223:                                            ; preds = %bb221, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i53, %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit40
  call void @llvm.lifetime.end(i64 -1, i8* %tmp141) nounwind
  store i32 (...)** bitcast (i8** getelementptr inbounds ([16 x i8*]* @_ZTVSt15basic_streambufIcSt11char_traitsIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp140, align 8, !tbaa !3
  call void @_ZNSt6localeD1Ev(%"class.std::locale"* %tmp143) nounwind
  store i32 (...)** %.c.i.i.i.i32, i32 (...)*** %tmp133, align 8, !tbaa !3
  %tmp224 = load i64* %tmp112, align 8
  %tmp225 = getelementptr i8* %tmp137, i64 %tmp224
  %tmp226 = bitcast i8* %tmp225 to i8**
  store i8* %tmp110, i8** %tmp226, align 8, !tbaa !3
  store i64 0, i64* %tmp144, align 8, !tbaa !7
  call void @_ZNSt8ios_baseD2Ev(%"class.std::ios_base"* %tmp145)
  br label %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit56

_ZN8MyModuleC1EN7sc_core14sc_module_nameE.exit68: ; preds = %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit56
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp10, i8* getelementptr inbounds ([5 x i8]* @.str10, i64 0, i64 0))
  call void @_ZN8MyModuleC2EN7sc_core14sc_module_nameE(%struct.MyModule* %sink, %"class.sc_core::sc_module_name"* %tmp10)
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp10)
  %tmp227 = getelementptr inbounds %struct.MyModule* %sink, i64 0, i32 2
  store %struct.MyModule* %sink, %struct.MyModule** %tmp227, align 8, !tbaa !0
  %tmp228 = load %struct.MyModule** %tmp126, align 8, !tbaa !0
  %tmp229 = getelementptr inbounds %struct.Source* %source, i64 0, i32 1
  store %struct.MyModule* %tmp228, %struct.MyModule** %tmp229, align 8, !tbaa !0
  %tmp230 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([19 x i8]* @.str11, i64 0, i64 0), i64 18)
  %tmp231 = bitcast %struct.MyModule* %sink to i8*
  %tmp232 = call %"class.std::basic_ostream"* @_ZNSo9_M_insertIPKvEERSoT_(%"class.std::basic_ostream"* @_ZSt4cout, i8* %tmp231)
  %tmp233 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp232, i8* getelementptr inbounds ([2 x i8]* @.str4, i64 0, i64 0), i64 1)
  %tmp234 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([25 x i8]* @.str12, i64 0, i64 0), i64 24)
  %tmp235 = getelementptr inbounds %struct.MyModule* %sink, i64 0, i32 1
  %tmp236 = bitcast %"class.sc_core::sc_event"* %tmp235 to i8*
  %tmp237 = call %"class.std::basic_ostream"* @_ZNSo9_M_insertIPKvEERSoT_(%"class.std::basic_ostream"* @_ZSt4cout, i8* %tmp236)
  %tmp238 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp237, i8* getelementptr inbounds ([2 x i8]* @.str4, i64 0, i64 0), i64 1)
  %tmp239 = load i32* %nb_modules, align 4, !tbaa !5
  %tmp240 = add nsw i32 %tmp239, -2
  %tmp241 = icmp sgt i32 %tmp240, 0
  br i1 %tmp241, label %.lr.ph.preheader, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80._crit_edge

.lr.ph.preheader:                                 ; preds = %_ZN8MyModuleC1EN7sc_core14sc_module_nameE.exit68
  br label %.lr.ph

.lr.ph:                                           ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80, %.lr.ph.preheader
  %indvars.iv = phi i64 [ %indvars.iv.next.pre-phi, %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80 ], [ 0, %.lr.ph.preheader ]
  %tmp242 = add nsw i32 %tmp239, -3
  %tmp243 = trunc i64 %indvars.iv to i32
  %tmp244 = icmp eq i32 %tmp243, %tmp242
  br i1 %tmp244, label %bb252, label %bb245

bb245:                                            ; preds = %.lr.ph
  %tmp246 = add nsw i64 %indvars.iv, 1
  %tmp247 = getelementptr inbounds %struct.MyModule** %tmp126, i64 %tmp246
  %tmp248 = load %struct.MyModule** %tmp247, align 8, !tbaa !0
  %tmp249 = getelementptr inbounds %struct.MyModule** %tmp126, i64 %indvars.iv
  %tmp250 = load %struct.MyModule** %tmp249, align 8, !tbaa !0
  %tmp251 = getelementptr inbounds %struct.MyModule* %tmp250, i64 0, i32 2
  store %struct.MyModule* %tmp248, %struct.MyModule** %tmp251, align 8, !tbaa !0
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80

bb252:                                            ; preds = %.lr.ph
  %tmp253 = getelementptr inbounds %struct.MyModule** %tmp126, i64 %indvars.iv
  %tmp254 = load %struct.MyModule** %tmp253, align 8, !tbaa !0
  %tmp255 = getelementptr inbounds %struct.MyModule* %tmp254, i64 0, i32 2
  store %struct.MyModule* %sink, %struct.MyModule** %tmp255, align 8, !tbaa !0
  %indvars.iv.next.pre = add i64 %indvars.iv, 1
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80: ; preds = %bb252, %bb245
  %indvars.iv.next.pre-phi = phi i64 [ %tmp246, %bb245 ], [ %indvars.iv.next.pre, %bb252 ]
  %tmp256 = trunc i64 %indvars.iv.next.pre-phi to i32
  %tmp257 = icmp slt i32 %tmp256, %tmp240
  br i1 %tmp257, label %.lr.ph, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80._crit_edge.loopexit

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80._crit_edge.loopexit: ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80._crit_edge

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80._crit_edge: ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80._crit_edge.loopexit, %_ZN8MyModuleC1EN7sc_core14sc_module_nameE.exit68
  call void @_ZN7sc_core8sc_startEv()
  %tmp258 = getelementptr inbounds %struct.MyModule* %sink, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp258, align 8, !tbaa !3
  %tmp259 = getelementptr %struct.MyModule* %sink, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp259, align 8, !tbaa !3
  call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp235)
  %tmp260 = getelementptr inbounds %struct.MyModule* %sink, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp260)
  %tmp261 = getelementptr inbounds %struct.Source* %source, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp261)
  br label %bb283

bb262:                                            ; preds = %.noexc19
  %tmp263 = getelementptr inbounds i8* %tmp73, i64 -8
  %tmp264 = bitcast i8* %tmp263 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb265, label %bb267

bb265:                                            ; preds = %bb262
  %tmp266 = atomicrmw add i32* %tmp264, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i91

bb267:                                            ; preds = %bb262
  %tmp268 = load i32* %tmp264, align 4, !tbaa !5
  %tmp269 = add nsw i32 %tmp268, -1
  store i32 %tmp269, i32* %tmp264, align 4, !tbaa !5
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i91

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i91: ; preds = %bb267, %bb265
  %.0.i.i.i.i.i.i.i.i90 = phi i32 [ %tmp266, %bb265 ], [ %tmp268, %bb267 ]
  %tmp270 = icmp slt i32 %.0.i.i.i.i.i.i.i.i90, 1
  br i1 %tmp270, label %bb271, label %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit93

bb271:                                            ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i91
  %tmp272 = bitcast i8* %tmp74 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp272, %"class.std::allocator.40"* %tmp3) nounwind
  br label %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit93

_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit93: ; preds = %bb271, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i.i.i.i.i91, %.noexc19
  call void @llvm.lifetime.end(i64 -1, i8* %tmp71) nounwind
  store i32 (...)** bitcast (i8** getelementptr inbounds ([16 x i8*]* @_ZTVSt15basic_streambufIcSt11char_traitsIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp70, align 8, !tbaa !3
  %tmp273 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 1, i32 0, i32 7
  call void @_ZNSt6localeD1Ev(%"class.std::locale"* %tmp273) nounwind
  %tmp274 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 2), align 8
  %.c.i.i.i.i92 = bitcast i8* %tmp274 to i32 (...)**
  store i32 (...)** %.c.i.i.i.i92, i32 (...)*** %tmp60, align 8, !tbaa !3
  %tmp275 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 3), align 8
  %tmp276 = getelementptr i8* %tmp274, i64 -24
  %tmp277 = bitcast i8* %tmp276 to i64*
  %tmp278 = load i64* %tmp277, align 8
  %tmp279 = getelementptr i8* %tmp65, i64 %tmp278
  %tmp280 = bitcast i8* %tmp279 to i8**
  store i8* %tmp275, i8** %tmp280, align 8, !tbaa !3
  %tmp281 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 0, i32 0, i32 1
  store i64 0, i64* %tmp281, align 8, !tbaa !7
  %tmp282 = getelementptr inbounds %"class.std::basic_stringstream"* %ss, i64 0, i32 2, i32 0
  call void @_ZNSt8ios_baseD2Ev(%"class.std::ios_base"* %tmp282)
  br label %bb283

bb283:                                            ; preds = %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit93, %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80._crit_edge, %_ZNKSt5ctypeIcE5widenEc.exit99
  %.1 = phi i32 [ 0, %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit80._crit_edge ], [ 1, %_ZNKSt5ctypeIcE5widenEc.exit99 ], [ 1, %_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEED1Ev.exit93 ]
  ret i32 %.1
}

define available_externally void @_ZNSt18basic_stringstreamIcSt11char_traitsIcESaIcEEC1ESt13_Ios_Openmode(%"class.std::basic_stringstream"* %this, i32 %__m) unnamed_addr uwtable align 2 {
.noexc:
  %tmp = bitcast %"class.std::basic_stringstream"* %this to i8*
  %tmp1 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 2
  %tmp2 = getelementptr inbounds %"class.std::basic_ios"* %tmp1, i64 0, i32 0
  call void @_ZNSt8ios_baseC2Ev(%"class.std::ios_base"* %tmp2) nounwind
  %tmp3 = getelementptr inbounds %"class.std::basic_ios"* %tmp1, i64 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([4 x i8*]* @_ZTVSt9basic_iosIcSt11char_traitsIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp3, align 8, !tbaa !3
  %tmp4 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 2, i32 1
  store %"class.std::basic_ostream"* null, %"class.std::basic_ostream"** %tmp4, align 8, !tbaa !0
  %tmp5 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 2, i32 2
  store i8 0, i8* %tmp5, align 1, !tbaa !1
  %tmp6 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 2, i32 3
  store i8 0, i8* %tmp6, align 1, !tbaa !8
  %tmp7 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 2, i32 4
  %tmp8 = bitcast %"class.std::basic_streambuf"** %tmp7 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp8, i8 0, i64 32, i32 8, i1 false) nounwind
  %tmp9 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 2), align 8
  %tmp10 = bitcast %"class.std::basic_stringstream"* %this to i8**
  %tmp11 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 0, i32 0, i32 0
  %.c.i.i = bitcast i8* %tmp9 to i32 (...)**
  store i32 (...)** %.c.i.i, i32 (...)*** %tmp11, align 8, !tbaa !3
  %tmp12 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 3), align 8
  %tmp13 = getelementptr i8* %tmp9, i64 -24
  %tmp14 = bitcast i8* %tmp13 to i64*
  %tmp15 = load i64* %tmp14, align 8
  %tmp16 = getelementptr i8* %tmp, i64 %tmp15
  %tmp17 = bitcast i8* %tmp16 to i8**
  store i8* %tmp12, i8** %tmp17, align 8, !tbaa !3
  %tmp18 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 0, i32 0, i32 1
  store i64 0, i64* %tmp18, align 8, !tbaa !7
  %tmp19 = load i8** %tmp10, align 8, !tbaa !3
  %tmp20 = getelementptr i8* %tmp19, i64 -24
  %tmp21 = bitcast i8* %tmp20 to i64*
  %tmp22 = load i64* %tmp21, align 8
  %tmp23 = getelementptr i8* %tmp, i64 %tmp22
  %tmp24 = bitcast i8* %tmp23 to %"class.std::basic_ios"*
  call void @_ZNSt9basic_iosIcSt11char_traitsIcEE4initEPSt15basic_streambufIcS1_E(%"class.std::basic_ios"* %tmp24, %"class.std::basic_streambuf"* null)
  %tmp25 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 0, i32 1
  %tmp26 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 4), align 8
  %tmp27 = bitcast %"class.std::basic_ostream.base"* %tmp25 to i8**
  %tmp28 = getelementptr inbounds %"class.std::basic_ostream.base"* %tmp25, i64 0, i32 0
  %.c.i2.i = bitcast i8* %tmp26 to i32 (...)**
  store i32 (...)** %.c.i2.i, i32 (...)*** %tmp28, align 8, !tbaa !3
  %tmp29 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 5), align 8
  %tmp30 = getelementptr i8* %tmp26, i64 -24
  %tmp31 = bitcast i8* %tmp30 to i64*
  %tmp32 = load i64* %tmp31, align 8
  %tmp33 = bitcast %"class.std::basic_ostream.base"* %tmp25 to i8*
  %tmp34 = getelementptr i8* %tmp33, i64 %tmp32
  %tmp35 = bitcast i8* %tmp34 to i8**
  store i8* %tmp29, i8** %tmp35, align 8, !tbaa !3
  %tmp36 = load i8** %tmp27, align 8, !tbaa !3
  %tmp37 = getelementptr i8* %tmp36, i64 -24
  %tmp38 = bitcast i8* %tmp37 to i64*
  %tmp39 = load i64* %tmp38, align 8
  %tmp40 = getelementptr i8* %tmp33, i64 %tmp39
  %tmp41 = bitcast i8* %tmp40 to %"class.std::basic_ios"*
  call void @_ZNSt9basic_iosIcSt11char_traitsIcEE4initEPSt15basic_streambufIcS1_E(%"class.std::basic_ios"* %tmp41, %"class.std::basic_streambuf"* null)
  %tmp42 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 1), align 8
  %.c.i = bitcast i8* %tmp42 to i32 (...)**
  store i32 (...)** %.c.i, i32 (...)*** %tmp11, align 8, !tbaa !3
  %tmp43 = load i8** getelementptr inbounds ([10 x i8*]* @_ZTTSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 6), align 8
  %tmp44 = getelementptr i8* %tmp42, i64 -24
  %tmp45 = bitcast i8* %tmp44 to i64*
  %tmp46 = load i64* %tmp45, align 8
  %tmp47 = getelementptr i8* %tmp, i64 %tmp46
  %tmp48 = bitcast i8* %tmp47 to i8**
  store i8* %tmp43, i8** %tmp48, align 8, !tbaa !3
  %tmp49 = getelementptr %"class.std::basic_stringstream"* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([15 x i8*]* @_ZTVSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 3) to i32 (...)**), i32 (...)*** %tmp11, align 8, !tbaa !3
  %tmp50 = getelementptr %"class.std::basic_stringstream"* %this, i64 0, i32 2, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([15 x i8*]* @_ZTVSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 13) to i32 (...)**), i32 (...)*** %tmp50, align 8, !tbaa !3
  store i32 (...)** bitcast (i8** getelementptr inbounds ([15 x i8*]* @_ZTVSt18basic_stringstreamIcSt11char_traitsIcESaIcEE, i64 0, i64 8) to i32 (...)**), i32 (...)*** %tmp49, align 8, !tbaa !3
  %tmp51 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 1
  %tmp52 = getelementptr inbounds %"class.std::basic_stringbuf"* %tmp51, i64 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([16 x i8*]* @_ZTVSt15basic_streambufIcSt11char_traitsIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp52, align 8, !tbaa !3
  %tmp53 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 1, i32 0, i32 1
  %tmp54 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 1, i32 0, i32 7
  %tmp55 = bitcast i8** %tmp53 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp55, i8 0, i64 48, i32 8, i1 false) nounwind
  call void @_ZNSt6localeC1Ev(%"class.std::locale"* %tmp54) nounwind
  store i32 (...)** bitcast (i8** getelementptr inbounds ([16 x i8*]* @_ZTVSt15basic_stringbufIcSt11char_traitsIcESaIcEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp52, align 8, !tbaa !3
  %tmp56 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 1, i32 1
  store i32 %__m, i32* %tmp56, align 4, !tbaa !9
  %tmp57 = getelementptr inbounds %"class.std::basic_stringstream"* %this, i64 0, i32 1, i32 2, i32 0, i32 0
  store i8* bitcast (i64* getelementptr inbounds ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE, i64 0, i64 3) to i8*), i8** %tmp57, align 8, !tbaa !0
  %tmp58 = load i8** %tmp10, align 8, !tbaa !3
  %tmp59 = getelementptr i8* %tmp58, i64 -24
  %tmp60 = bitcast i8* %tmp59 to i64*
  %tmp61 = load i64* %tmp60, align 8
  %tmp62 = getelementptr i8* %tmp, i64 %tmp61
  %tmp63 = bitcast i8* %tmp62 to %"class.std::basic_ios"*
  %tmp64 = getelementptr inbounds %"class.std::basic_stringbuf"* %tmp51, i64 0, i32 0
  call void @_ZNSt9basic_iosIcSt11char_traitsIcEE4initEPSt15basic_streambufIcS1_E(%"class.std::basic_ios"* %tmp63, %"class.std::basic_streambuf"* %tmp64)
  ret void
}

declare %"class.std::basic_istream"* @_ZNSirsERi(%"class.std::basic_istream"*, i32*)

declare %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"*, i32)

declare void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"*, i8*)

declare void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"*)

declare { i64, i1 } @llvm.umul.with.overflow.i64(i64, i64) nounwind readnone

declare noalias i8* @_Znam(i64)

declare noalias i8* @_Znwm(i64)

declare void @_ZdlPv(i8*) nounwind

declare void @_ZN7sc_core8sc_startEv()

define linkonce_odr void @_ZN8MyModuleD1Ev(%struct.MyModule* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN8MyModuleD2Ev.exit:
  %tmp = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %struct.MyModule* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 1
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp2)
  %tmp3 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  ret void
}

define linkonce_odr void @_ZN6SourceD1Ev(%struct.Source* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %struct.Source* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp)
  ret void
}

declare %"class.std::basic_ostream"* @_ZNSo9_M_insertIPKvEERSoT_(%"class.std::basic_ostream"*, i8*)

define available_externally void @_ZNKSt15basic_stringbufIcSt11char_traitsIcESaIcEE3strEv(%"class.std::basic_string"* noalias sret %agg.result, %"class.std::basic_stringbuf"* %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.std::basic_string"* %agg.result, i64 0, i32 0, i32 0
  store i8* bitcast (i64* getelementptr inbounds ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE, i64 0, i64 3) to i8*), i8** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.std::basic_stringbuf"* %this, i64 0, i32 0, i32 5
  %tmp2 = load i8** %tmp1, align 8, !tbaa !0
  %tmp3 = icmp eq i8* %tmp2, null
  br i1 %tmp3, label %bb86, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.std::basic_stringbuf"* %this, i64 0, i32 0, i32 3
  %tmp6 = load i8** %tmp5, align 8, !tbaa !0
  %tmp7 = icmp ugt i8* %tmp2, %tmp6
  %tmp8 = getelementptr inbounds %"class.std::basic_stringbuf"* %this, i64 0, i32 0, i32 4
  %tmp9 = load i8** %tmp8, align 8, !tbaa !0
  %tmp10 = alloca %"class.std::allocator.40", align 1
  %tmp11 = alloca %"class.std::basic_string", align 8
  br i1 %tmp7, label %bb12, label %bb49

bb12:                                             ; preds = %bb4
  %tmp13 = icmp eq i8* %tmp9, %tmp2
  br i1 %tmp13, label %_ZNSsaSERKSs.exit20, label %.critedge.i.i.i.i.i13

.critedge.i.i.i.i.i13:                            ; preds = %bb12
  %.not.i.i.i.i.i12 = icmp eq i8* %tmp9, null
  br i1 %.not.i.i.i.i.i12, label %bb14, label %.noexc17

bb14:                                             ; preds = %.critedge.i.i.i.i.i13
  call void @_ZSt19__throw_logic_errorPKc(i8* getelementptr inbounds ([42 x i8]* @.str13, i64 0, i64 0)) noreturn
  unreachable

.noexc17:                                         ; preds = %.critedge.i.i.i.i.i13
  %tmp15 = alloca %"class.std::allocator.40", align 1
  %tmp16 = ptrtoint i8* %tmp2 to i64
  %tmp17 = ptrtoint i8* %tmp9 to i64
  %tmp18 = sub i64 %tmp16, %tmp17
  %tmp19 = call %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* @_ZNSs4_Rep9_S_createEmmRKSaIcE(i64 %tmp18, i64 0, %"class.std::allocator.40"* %tmp15)
  %tmp20 = getelementptr inbounds %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp19, i64 1
  %tmp21 = bitcast %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp20 to i8*
  %tmp22 = icmp eq i64 %tmp18, 1
  br i1 %tmp22, label %bb23, label %bb25

bb23:                                             ; preds = %.noexc17
  %tmp24 = load i8* %tmp9, align 1, !tbaa !1
  store i8 %tmp24, i8* %tmp21, align 1, !tbaa !1
  br label %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i14

bb25:                                             ; preds = %.noexc17
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %tmp21, i8* %tmp9, i64 %tmp18, i32 1, i1 false) nounwind
  br label %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i14

_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i14:     ; preds = %bb25, %bb23
  %tmp26 = icmp eq %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp19, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*)
  br i1 %tmp26, label %_ZNSsaSERKSs.exit20, label %bb27, !prof !6

bb27:                                             ; preds = %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i14
  %tmp28 = getelementptr inbounds %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp19, i64 0, i32 0, i32 2
  store i32 0, i32* %tmp28, align 4, !tbaa !5
  %tmp29 = getelementptr inbounds %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp19, i64 0, i32 0, i32 0
  store i64 %tmp18, i64* %tmp29, align 8, !tbaa !7
  %tmp30 = getelementptr inbounds i8* %tmp21, i64 %tmp18
  %tmp31 = load i8* @_ZNSs4_Rep11_S_terminalE, align 1, !tbaa !1
  store i8 %tmp31, i8* %tmp30, align 1, !tbaa !1
  br label %_ZNSsaSERKSs.exit20

_ZNSsaSERKSs.exit20:                              ; preds = %bb27, %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i14, %bb12
  %.0.i.i.i.i.i15 = phi i8* [ bitcast (i64* getelementptr inbounds ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE, i64 0, i64 3) to i8*), %bb12 ], [ %tmp21, %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i14 ], [ %tmp21, %bb27 ]
  %tmp32 = getelementptr inbounds %"class.std::basic_string"* %tmp11, i64 0, i32 0, i32 0
  store i8* %.0.i.i.i.i.i15, i8** %tmp32, align 8, !tbaa !0
  %tmp33 = call %"class.std::basic_string"* @_ZNSs6assignERKSs(%"class.std::basic_string"* %agg.result, %"class.std::basic_string"* %tmp11)
  %tmp34 = getelementptr inbounds %"class.std::allocator.40"* %tmp10, i64 0, i32 0
  call void @llvm.lifetime.start(i64 -1, i8* %tmp34) nounwind
  %tmp35 = load i8** %tmp32, align 8, !tbaa !0
  %tmp36 = getelementptr inbounds i8* %tmp35, i64 -24
  %tmp37 = icmp eq i8* %tmp36, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp37, label %_ZNSsaSERKSs.exit, label %bb38, !prof !6

bb38:                                             ; preds = %_ZNSsaSERKSs.exit20
  %tmp39 = getelementptr inbounds i8* %tmp35, i64 -8
  %tmp40 = bitcast i8* %tmp39 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb41, label %bb43

bb41:                                             ; preds = %bb38
  %tmp42 = atomicrmw add i32* %tmp40, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i22

bb43:                                             ; preds = %bb38
  %tmp44 = load i32* %tmp40, align 4, !tbaa !5
  %tmp45 = add nsw i32 %tmp44, -1
  store i32 %tmp45, i32* %tmp40, align 4, !tbaa !5
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i22

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i22: ; preds = %bb43, %bb41
  %.0.i.i.i.i21 = phi i32 [ %tmp42, %bb41 ], [ %tmp44, %bb43 ]
  %tmp46 = icmp slt i32 %.0.i.i.i.i21, 1
  br i1 %tmp46, label %bb47, label %_ZNSsaSERKSs.exit

bb47:                                             ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i22
  %tmp48 = bitcast i8* %tmp36 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp48, %"class.std::allocator.40"* %tmp10) nounwind
  br label %_ZNSsaSERKSs.exit

bb49:                                             ; preds = %bb4
  %tmp50 = icmp eq i8* %tmp9, %tmp6
  br i1 %tmp50, label %_ZNSsaSERKSs.exit10, label %.critedge.i.i.i.i.i

.critedge.i.i.i.i.i:                              ; preds = %bb49
  %.not.i.i.i.i.i = icmp eq i8* %tmp9, null
  br i1 %.not.i.i.i.i.i, label %bb51, label %.noexc11

bb51:                                             ; preds = %.critedge.i.i.i.i.i
  call void @_ZSt19__throw_logic_errorPKc(i8* getelementptr inbounds ([42 x i8]* @.str13, i64 0, i64 0)) noreturn
  unreachable

.noexc11:                                         ; preds = %.critedge.i.i.i.i.i
  %tmp52 = alloca %"class.std::allocator.40", align 1
  %tmp53 = ptrtoint i8* %tmp6 to i64
  %tmp54 = ptrtoint i8* %tmp9 to i64
  %tmp55 = sub i64 %tmp53, %tmp54
  %tmp56 = call %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* @_ZNSs4_Rep9_S_createEmmRKSaIcE(i64 %tmp55, i64 0, %"class.std::allocator.40"* %tmp52)
  %tmp57 = getelementptr inbounds %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp56, i64 1
  %tmp58 = bitcast %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp57 to i8*
  %tmp59 = icmp eq i64 %tmp55, 1
  br i1 %tmp59, label %bb60, label %bb62

bb60:                                             ; preds = %.noexc11
  %tmp61 = load i8* %tmp9, align 1, !tbaa !1
  store i8 %tmp61, i8* %tmp58, align 1, !tbaa !1
  br label %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i

bb62:                                             ; preds = %.noexc11
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %tmp58, i8* %tmp9, i64 %tmp55, i32 1, i1 false) nounwind
  br label %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i

_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i:       ; preds = %bb62, %bb60
  %tmp63 = icmp eq %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp56, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*)
  br i1 %tmp63, label %_ZNSsaSERKSs.exit10, label %bb64, !prof !6

bb64:                                             ; preds = %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i
  %tmp65 = getelementptr inbounds %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp56, i64 0, i32 0, i32 2
  store i32 0, i32* %tmp65, align 4, !tbaa !5
  %tmp66 = getelementptr inbounds %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp56, i64 0, i32 0, i32 0
  store i64 %tmp55, i64* %tmp66, align 8, !tbaa !7
  %tmp67 = getelementptr inbounds i8* %tmp58, i64 %tmp55
  %tmp68 = load i8* @_ZNSs4_Rep11_S_terminalE, align 1, !tbaa !1
  store i8 %tmp68, i8* %tmp67, align 1, !tbaa !1
  br label %_ZNSsaSERKSs.exit10

_ZNSsaSERKSs.exit10:                              ; preds = %bb64, %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i, %bb49
  %.0.i.i.i.i.i = phi i8* [ bitcast (i64* getelementptr inbounds ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE, i64 0, i64 3) to i8*), %bb49 ], [ %tmp58, %_ZNSs13_S_copy_charsEPcS_S_.exit.i.i.i.i.i ], [ %tmp58, %bb64 ]
  %tmp69 = getelementptr inbounds %"class.std::basic_string"* %tmp11, i64 0, i32 0, i32 0
  store i8* %.0.i.i.i.i.i, i8** %tmp69, align 8, !tbaa !0
  %tmp70 = call %"class.std::basic_string"* @_ZNSs6assignERKSs(%"class.std::basic_string"* %agg.result, %"class.std::basic_string"* %tmp11)
  %tmp71 = getelementptr inbounds %"class.std::allocator.40"* %tmp10, i64 0, i32 0
  call void @llvm.lifetime.start(i64 -1, i8* %tmp71) nounwind
  %tmp72 = load i8** %tmp69, align 8, !tbaa !0
  %tmp73 = getelementptr inbounds i8* %tmp72, i64 -24
  %tmp74 = icmp eq i8* %tmp73, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp74, label %_ZNSsaSERKSs.exit, label %bb75, !prof !6

bb75:                                             ; preds = %_ZNSsaSERKSs.exit10
  %tmp76 = getelementptr inbounds i8* %tmp72, i64 -8
  %tmp77 = bitcast i8* %tmp76 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb78, label %bb80

bb78:                                             ; preds = %bb75
  %tmp79 = atomicrmw add i32* %tmp77, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8

bb80:                                             ; preds = %bb75
  %tmp81 = load i32* %tmp77, align 4, !tbaa !5
  %tmp82 = add nsw i32 %tmp81, -1
  store i32 %tmp82, i32* %tmp77, align 4, !tbaa !5
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8: ; preds = %bb80, %bb78
  %.0.i.i.i.i7 = phi i32 [ %tmp79, %bb78 ], [ %tmp81, %bb80 ]
  %tmp83 = icmp slt i32 %.0.i.i.i.i7, 1
  br i1 %tmp83, label %bb84, label %_ZNSsaSERKSs.exit

bb84:                                             ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8
  %tmp85 = bitcast i8* %tmp73 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp85, %"class.std::allocator.40"* %tmp10) nounwind
  br label %_ZNSsaSERKSs.exit

bb86:                                             ; preds = %bb
  %tmp87 = getelementptr inbounds %"class.std::basic_stringbuf"* %this, i64 0, i32 2
  %tmp88 = call %"class.std::basic_string"* @_ZNSs6assignERKSs(%"class.std::basic_string"* %agg.result, %"class.std::basic_string"* %tmp87)
  br label %_ZNSsaSERKSs.exit

_ZNSsaSERKSs.exit:                                ; preds = %bb86, %bb84, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8, %_ZNSsaSERKSs.exit10, %bb47, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i22, %_ZNSsaSERKSs.exit20
  ret void
}

declare void @_ZSt19__throw_logic_errorPKc(i8*) noreturn

declare %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* @_ZNSs4_Rep9_S_createEmmRKSaIcE(i64, i64, %"class.std::allocator.40"*)

declare void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*, %"class.std::allocator.40"*) nounwind

declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i32, i1) nounwind

declare %"class.std::basic_string"* @_ZNSs6assignERKSs(%"class.std::basic_string"*, %"class.std::basic_string"*)

declare void @_ZNSt8ios_baseD2Ev(%"class.std::ios_base"*)

declare void @_ZNSt6localeD1Ev(%"class.std::locale"*) nounwind

declare void @_ZNSt9basic_iosIcSt11char_traitsIcEE4initEPSt15basic_streambufIcS1_E(%"class.std::basic_ios"*, %"class.std::basic_streambuf"*)

declare void @_ZNSt6localeC1Ev(%"class.std::locale"*) nounwind

declare void @_ZNSt8ios_baseC2Ev(%"class.std::ios_base"*) nounwind

declare %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"*, i8 signext)

declare void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"*)

declare void @_ZSt16__throw_bad_castv() noreturn

declare %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"*)

declare %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"*, i8*, i64)

declare i64 @strlen(i8* nocapture) nounwind readonly

declare void @_ZNSt9basic_iosIcSt11char_traitsIcEE5clearESt12_Ios_Iostate(%"class.std::basic_ios"*, i32)

declare extern_weak i32 @pthread_cancel(i64)

define linkonce_odr void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  tail call void @_ZN7sc_core8sc_event6cancelEv(%"class.sc_core::sc_event"* %this)
  %tmp = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 7, i32 0, i32 0, i32 0
  %tmp1 = load %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !0
  %tmp2 = icmp eq %"class.sc_core::sc_thread_process"** %tmp1, null
  br i1 %tmp2, label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %"class.sc_core::sc_thread_process"** %tmp1 to i8*
  tail call void @_ZdlPv(i8* %tmp4) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit

_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit: ; preds = %bb3, %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 6, i32 0, i32 0, i32 0
  %tmp6 = load %"class.sc_core::sc_thread_process"*** %tmp5, align 8, !tbaa !0
  %tmp7 = icmp eq %"class.sc_core::sc_thread_process"** %tmp6, null
  br i1 %tmp7, label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4, label %bb8

bb8:                                              ; preds = %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit
  %tmp9 = bitcast %"class.sc_core::sc_thread_process"** %tmp6 to i8*
  tail call void @_ZdlPv(i8* %tmp9) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4

_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4: ; preds = %bb8, %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit
  %tmp10 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 5, i32 0, i32 0, i32 0
  %tmp11 = load %"class.sc_core::sc_method_process"*** %tmp10, align 8, !tbaa !0
  %tmp12 = icmp eq %"class.sc_core::sc_method_process"** %tmp11, null
  br i1 %tmp12, label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit, label %bb13

bb13:                                             ; preds = %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4
  %tmp14 = bitcast %"class.sc_core::sc_method_process"** %tmp11 to i8*
  tail call void @_ZdlPv(i8* %tmp14) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit

_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit: ; preds = %bb13, %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4
  %tmp15 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 4, i32 0, i32 0, i32 0
  %tmp16 = load %"class.sc_core::sc_method_process"*** %tmp15, align 8, !tbaa !0
  %tmp17 = icmp eq %"class.sc_core::sc_method_process"** %tmp16, null
  br i1 %tmp17, label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit5, label %bb18

bb18:                                             ; preds = %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit
  %tmp19 = bitcast %"class.sc_core::sc_method_process"** %tmp16 to i8*
  tail call void @_ZdlPv(i8* %tmp19) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit5

_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit5: ; preds = %bb18, %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit
  ret void
}

declare void @_ZN7sc_core8sc_event6cancelEv(%"class.sc_core::sc_event"*)

define linkonce_odr void @_ZThn40_N6SourceD1Ev(%struct.Source* %this) {
bb:
  %tmp = getelementptr inbounds %struct.Source* %this, i64 -1, i32 0, i32 9
  %tmp1 = bitcast %"class.std::vector.10"* %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp1)
  ret void
}

declare void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZThn40_N8MyModuleD1Ev(%struct.MyModule* %this) {
_ZN8MyModuleD1Ev.exit:
  %tmp = getelementptr inbounds %struct.MyModule* %this, i64 -1, i32 1, i32 6, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 23
  %tmp3 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  ret void
}

declare void @_ZNK7sc_core9sc_object5printERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object4dumpERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)

define linkonce_odr i8* @_ZNK7sc_core9sc_module4kindEv(%"class.sc_core::sc_module"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str20, i64 0, i64 0)
}

declare %"class.std::vector.10"* @_ZNK7sc_core9sc_module17get_child_objectsEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN8MyModuleD0Ev(%struct.MyModule* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN8MyModuleD2Ev.exit.i:
  %tmp = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %struct.MyModule* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 1
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp2)
  %tmp3 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  %tmp4 = bitcast %struct.MyModule* %this to i8*
  tail call void @_ZdlPv(i8* %tmp4) nounwind
  ret void
}

declare void @_ZN7sc_core9sc_module25before_end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module18end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module19start_of_simulationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module17end_of_simulationEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZThn40_N8MyModuleD0Ev(%struct.MyModule* %this) {
_ZN8MyModuleD2Ev.exit.i.i:
  %tmp = getelementptr inbounds %struct.MyModule* %this, i64 -1, i32 1, i32 6, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 23
  %tmp3 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  %tmp5 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  ret void
}

define linkonce_odr void @_ZN8MyModuleC2EN7sc_core14sc_module_nameE(%struct.MyModule* %this, %"class.sc_core::sc_module_name"* %name) unnamed_addr uwtable align 2 {
bb:
  %compute_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp3, %"class.sc_core::sc_module_name"* %name)
  %tmp4 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !3
  %tmp5 = getelementptr %struct.MyModule* %this, i64 0, i32 0, i32 1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp5, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8MyModule, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp6, align 8, !tbaa !3
  %tmp7 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp8 = icmp eq %"class.sc_core::sc_simcontext"* %tmp7, null
  br i1 %tmp8, label %.noexc, label %bb11

.noexc:                                           ; preds = %bb
  %tmp9 = call noalias i8* @_Znwm(i64 248)
  %tmp10 = bitcast i8* %tmp9 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp10)
  store %"class.sc_core::sc_simcontext"* %tmp10, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp10, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %bb11

bb11:                                             ; preds = %.noexc, %bb
  %tmp12 = phi %"class.sc_core::sc_simcontext"* [ %tmp10, %.noexc ], [ %tmp7, %bb ]
  %tmp13 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 1
  %tmp14 = getelementptr inbounds %"class.sc_core::sc_event"* %tmp13, i64 0, i32 0
  store %"class.sc_core::sc_simcontext"* %tmp12, %"class.sc_core::sc_simcontext"** %tmp14, align 8, !tbaa !0
  %tmp15 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 1, i32 1
  store i32 0, i32* %tmp15, align 4, !tbaa !10
  %tmp16 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 1, i32 2
  store i32 -1, i32* %tmp16, align 4, !tbaa !5
  %tmp17 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 1, i32 3
  %tmp18 = bitcast %"class.sc_core::sc_event_timed"** %tmp17 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp18, i8 0, i64 104, i32 8, i1 false)
  %tmp19 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp20 = icmp eq %"class.sc_core::sc_simcontext"* %tmp19, null
  br i1 %tmp20, label %.noexc6, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc6:                                          ; preds = %bb11
  %tmp21 = call noalias i8* @_Znwm(i64 248)
  %tmp22 = bitcast i8* %tmp21 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp22)
  store %"class.sc_core::sc_simcontext"* %tmp22, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp22, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc6, %bb11
  %tmp23 = phi %"class.sc_core::sc_simcontext"* [ %tmp22, %.noexc6 ], [ %tmp19, %bb11 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %compute_handle, %"class.sc_core::sc_simcontext"* %tmp23, i8* getelementptr inbounds ([8 x i8]* @.str21, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%struct.MyModule*)* @_ZN8MyModule7computeEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp5, %"class.sc_core::sc_spawn_options"* null)
  %tmp24 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %compute_handle, i64 0, i32 0
  %tmp25 = load %"class.sc_core::sc_process_b"** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp25, %"class.sc_core::sc_process_b"** %tmp26, align 8, !tbaa !0
  %tmp27 = icmp eq %"class.sc_core::sc_process_b"* %tmp25, null
  br i1 %tmp27, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb28

bb28:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp29 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp25, i64 0, i32 15
  %tmp30 = load i32* %tmp29, align 4, !tbaa !5
  %tmp31 = icmp eq i32 %tmp30, 0
  br i1 %tmp31, label %bb32, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb32:                                             ; preds = %bb28
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str24, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb28
  %tmp33 = add nsw i32 %tmp30, 1
  store i32 %tmp33, i32* %tmp29, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp34 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0, i32 2
  %tmp35 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp34, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp36 = load %"class.sc_core::sc_process_b"** %tmp26, align 8, !tbaa !0
  %tmp37 = icmp eq %"class.sc_core::sc_process_b"* %tmp36, null
  br i1 %tmp37, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb38

bb38:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp39 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp36, i64 0, i32 15
  %tmp40 = load i32* %tmp39, align 4, !tbaa !5
  %tmp41 = add nsw i32 %tmp40, -1
  store i32 %tmp41, i32* %tmp39, align 4, !tbaa !5
  %tmp42 = icmp eq i32 %tmp41, 0
  br i1 %tmp42, label %bb43, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb43:                                             ; preds = %bb38
  %tmp44 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp45 = icmp eq %"class.sc_core::sc_process_b"* %tmp44, null
  br i1 %tmp45, label %bb50, label %.noexc9

.noexc9:                                          ; preds = %bb43
  %tmp46 = bitcast %"class.sc_core::sc_process_b"* %tmp44 to void (%"class.sc_core::sc_process_b"*)***
  %tmp47 = load void (%"class.sc_core::sc_process_b"*)*** %tmp46, align 8, !tbaa !3
  %tmp48 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp47, i64 6
  %tmp49 = load void (%"class.sc_core::sc_process_b"*)** %tmp48, align 8
  call void %tmp49(%"class.sc_core::sc_process_b"* %tmp44)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb50

bb50:                                             ; preds = %.noexc9, %bb43
  %tmp51 = phi %"class.sc_core::sc_process_b"* [ null, %bb43 ], [ %.pre.i.i.i, %.noexc9 ]
  %tmp52 = icmp eq %"class.sc_core::sc_process_b"* %tmp51, %tmp36
  br i1 %tmp52, label %bb53, label %bb54

bb53:                                             ; preds = %bb50
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str22, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb54:                                             ; preds = %bb50
  store %"class.sc_core::sc_process_b"* %tmp36, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb54, %bb38, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp55 = load %"class.sc_core::sc_process_b"** %tmp24, align 8, !tbaa !0
  %tmp56 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp55, %"class.sc_core::sc_process_b"** %tmp56, align 8, !tbaa !0
  %tmp57 = icmp eq %"class.sc_core::sc_process_b"* %tmp55, null
  br i1 %tmp57, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit11, label %bb58

bb58:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp59 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp55, i64 0, i32 15
  %tmp60 = load i32* %tmp59, align 4, !tbaa !5
  %tmp61 = icmp eq i32 %tmp60, 0
  br i1 %tmp61, label %bb62, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i10

bb62:                                             ; preds = %bb58
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str24, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i10: ; preds = %bb58
  %tmp63 = add nsw i32 %tmp60, 1
  store i32 %tmp63, i32* %tmp59, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit11

_ZN7sc_core17sc_process_handleC1ERKS0_.exit11:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i10, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp64 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0, i32 3
  %tmp65 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp64, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp66 = load %"class.sc_core::sc_process_b"** %tmp56, align 8, !tbaa !0
  %tmp67 = icmp eq %"class.sc_core::sc_process_b"* %tmp66, null
  br i1 %tmp67, label %_ZN7sc_core17sc_process_handleD1Ev.exit14, label %bb68

bb68:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit11
  %tmp69 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp66, i64 0, i32 15
  %tmp70 = load i32* %tmp69, align 4, !tbaa !5
  %tmp71 = add nsw i32 %tmp70, -1
  store i32 %tmp71, i32* %tmp69, align 4, !tbaa !5
  %tmp72 = icmp eq i32 %tmp71, 0
  br i1 %tmp72, label %bb73, label %_ZN7sc_core17sc_process_handleD1Ev.exit14

bb73:                                             ; preds = %bb68
  %tmp74 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp75 = icmp eq %"class.sc_core::sc_process_b"* %tmp74, null
  br i1 %tmp75, label %bb80, label %.noexc13

.noexc13:                                         ; preds = %bb73
  %tmp76 = bitcast %"class.sc_core::sc_process_b"* %tmp74 to void (%"class.sc_core::sc_process_b"*)***
  %tmp77 = load void (%"class.sc_core::sc_process_b"*)*** %tmp76, align 8, !tbaa !3
  %tmp78 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp77, i64 6
  %tmp79 = load void (%"class.sc_core::sc_process_b"*)** %tmp78, align 8
  call void %tmp79(%"class.sc_core::sc_process_b"* %tmp74)
  %.pre.i.i.i12 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb80

bb80:                                             ; preds = %.noexc13, %bb73
  %tmp81 = phi %"class.sc_core::sc_process_b"* [ null, %bb73 ], [ %.pre.i.i.i12, %.noexc13 ]
  %tmp82 = icmp eq %"class.sc_core::sc_process_b"* %tmp81, %tmp66
  br i1 %tmp82, label %bb83, label %bb84

bb83:                                             ; preds = %bb80
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str22, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb84:                                             ; preds = %bb80
  store %"class.sc_core::sc_process_b"* %tmp66, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit14

_ZN7sc_core17sc_process_handleD1Ev.exit14:        ; preds = %bb84, %bb68, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit11
  %tmp85 = load %"class.sc_core::sc_process_b"** %tmp24, align 8, !tbaa !0
  %tmp86 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp85, %"class.sc_core::sc_process_b"** %tmp86, align 8, !tbaa !0
  %tmp87 = icmp eq %"class.sc_core::sc_process_b"* %tmp85, null
  br i1 %tmp87, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit16, label %bb88

bb88:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit14
  %tmp89 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp85, i64 0, i32 15
  %tmp90 = load i32* %tmp89, align 4, !tbaa !5
  %tmp91 = icmp eq i32 %tmp90, 0
  br i1 %tmp91, label %bb92, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i15

bb92:                                             ; preds = %bb88
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str24, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i15: ; preds = %bb88
  %tmp93 = add nsw i32 %tmp90, 1
  store i32 %tmp93, i32* %tmp89, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit16

_ZN7sc_core17sc_process_handleC1ERKS0_.exit16:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i15, %_ZN7sc_core17sc_process_handleD1Ev.exit14
  %tmp94 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0, i32 4
  %tmp95 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp94, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp96 = load %"class.sc_core::sc_process_b"** %tmp86, align 8, !tbaa !0
  %tmp97 = icmp eq %"class.sc_core::sc_process_b"* %tmp96, null
  br i1 %tmp97, label %_ZN7sc_core17sc_process_handleD1Ev.exit19, label %bb98

bb98:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit16
  %tmp99 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp96, i64 0, i32 15
  %tmp100 = load i32* %tmp99, align 4, !tbaa !5
  %tmp101 = add nsw i32 %tmp100, -1
  store i32 %tmp101, i32* %tmp99, align 4, !tbaa !5
  %tmp102 = icmp eq i32 %tmp101, 0
  br i1 %tmp102, label %bb103, label %_ZN7sc_core17sc_process_handleD1Ev.exit19

bb103:                                            ; preds = %bb98
  %tmp104 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp105 = icmp eq %"class.sc_core::sc_process_b"* %tmp104, null
  br i1 %tmp105, label %bb110, label %.noexc18

.noexc18:                                         ; preds = %bb103
  %tmp106 = bitcast %"class.sc_core::sc_process_b"* %tmp104 to void (%"class.sc_core::sc_process_b"*)***
  %tmp107 = load void (%"class.sc_core::sc_process_b"*)*** %tmp106, align 8, !tbaa !3
  %tmp108 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp107, i64 6
  %tmp109 = load void (%"class.sc_core::sc_process_b"*)** %tmp108, align 8
  call void %tmp109(%"class.sc_core::sc_process_b"* %tmp104)
  %.pre.i.i.i17 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb110

bb110:                                            ; preds = %.noexc18, %bb103
  %tmp111 = phi %"class.sc_core::sc_process_b"* [ null, %bb103 ], [ %.pre.i.i.i17, %.noexc18 ]
  %tmp112 = icmp eq %"class.sc_core::sc_process_b"* %tmp111, %tmp96
  br i1 %tmp112, label %bb113, label %bb114

bb113:                                            ; preds = %bb110
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str22, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb114:                                            ; preds = %bb110
  store %"class.sc_core::sc_process_b"* %tmp96, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit19

_ZN7sc_core17sc_process_handleD1Ev.exit19:        ; preds = %bb114, %bb98, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit16
  %tmp115 = load %"class.sc_core::sc_process_b"** %tmp24, align 8, !tbaa !0
  %tmp116 = icmp eq %"class.sc_core::sc_process_b"* %tmp115, null
  br i1 %tmp116, label %_ZN7sc_core17sc_process_handleD1Ev.exit22, label %bb117

bb117:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit19
  %tmp118 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp115, i64 0, i32 15
  %tmp119 = load i32* %tmp118, align 4, !tbaa !5
  %tmp120 = add nsw i32 %tmp119, -1
  store i32 %tmp120, i32* %tmp118, align 4, !tbaa !5
  %tmp121 = icmp eq i32 %tmp120, 0
  br i1 %tmp121, label %bb122, label %_ZN7sc_core17sc_process_handleD1Ev.exit22

bb122:                                            ; preds = %bb117
  %tmp123 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp124 = icmp eq %"class.sc_core::sc_process_b"* %tmp123, null
  br i1 %tmp124, label %bb129, label %.noexc21

.noexc21:                                         ; preds = %bb122
  %tmp125 = bitcast %"class.sc_core::sc_process_b"* %tmp123 to void (%"class.sc_core::sc_process_b"*)***
  %tmp126 = load void (%"class.sc_core::sc_process_b"*)*** %tmp125, align 8, !tbaa !3
  %tmp127 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp126, i64 6
  %tmp128 = load void (%"class.sc_core::sc_process_b"*)** %tmp127, align 8
  call void %tmp128(%"class.sc_core::sc_process_b"* %tmp123)
  %.pre.i.i.i20 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb129

bb129:                                            ; preds = %.noexc21, %bb122
  %tmp130 = phi %"class.sc_core::sc_process_b"* [ null, %bb122 ], [ %.pre.i.i.i20, %.noexc21 ]
  %tmp131 = icmp eq %"class.sc_core::sc_process_b"* %tmp130, %tmp115
  br i1 %tmp131, label %bb132, label %bb133

bb132:                                            ; preds = %bb129
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str22, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb133:                                            ; preds = %bb129
  store %"class.sc_core::sc_process_b"* %tmp115, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit22

_ZN7sc_core17sc_process_handleD1Ev.exit22:        ; preds = %bb133, %bb117, %_ZN7sc_core17sc_process_handleD1Ev.exit19
  %tmp134 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsERKNS_8sc_eventE(%"class.sc_core::sc_sensitive"* %tmp34, %"class.sc_core::sc_event"* %tmp13)
  ret void
}

declare void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

define linkonce_odr void @_ZN8MyModule7computeEv(%struct.MyModule* %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 1
  %tmp1 = getelementptr inbounds %struct.MyModule* %this, i64 0, i32 0, i32 0, i32 1
  %tmp2 = load %"class.sc_core::sc_simcontext"** %tmp1, align 8, !tbaa !0
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp, %"class.sc_core::sc_simcontext"* %tmp2)
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp)
  ret void
}

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsERKNS_8sc_eventE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_event"*)

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

declare void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"*)

declare void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"*, %"class.sc_core::sc_simcontext"*)

define linkonce_odr void @_ZN6SourceC2EN7sc_core14sc_module_nameEi(%struct.Source* %this, %"class.sc_core::sc_module_name"* %name, i32 %number_) unnamed_addr uwtable align 2 {
bb:
  %compute_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = getelementptr inbounds %struct.Source* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp3, %"class.sc_core::sc_module_name"* %name)
  %tmp4 = getelementptr inbounds %struct.Source* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !3
  %tmp5 = getelementptr %struct.Source* %this, i64 0, i32 0, i32 1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp5, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV6Source, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp6, align 8, !tbaa !3
  %tmp7 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp8 = icmp eq %"class.sc_core::sc_simcontext"* %tmp7, null
  br i1 %tmp8, label %.noexc, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc:                                           ; preds = %bb
  %tmp9 = call noalias i8* @_Znwm(i64 248)
  %tmp10 = bitcast i8* %tmp9 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp10)
  store %"class.sc_core::sc_simcontext"* %tmp10, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp10, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc, %bb
  %tmp11 = phi %"class.sc_core::sc_simcontext"* [ %tmp10, %.noexc ], [ %tmp7, %bb ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %compute_handle, %"class.sc_core::sc_simcontext"* %tmp11, i8* getelementptr inbounds ([8 x i8]* @.str21, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%struct.Source*)* @_ZN6Source7computeEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp5, %"class.sc_core::sc_spawn_options"* null)
  %tmp12 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %compute_handle, i64 0, i32 0
  %tmp13 = load %"class.sc_core::sc_process_b"** %tmp12, align 8, !tbaa !0
  %tmp14 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp13, %"class.sc_core::sc_process_b"** %tmp14, align 8, !tbaa !0
  %tmp15 = icmp eq %"class.sc_core::sc_process_b"* %tmp13, null
  br i1 %tmp15, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb16

bb16:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp17 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp13, i64 0, i32 15
  %tmp18 = load i32* %tmp17, align 4, !tbaa !5
  %tmp19 = icmp eq i32 %tmp18, 0
  br i1 %tmp19, label %bb20, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb20:                                             ; preds = %bb16
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str24, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb16
  %tmp21 = add nsw i32 %tmp18, 1
  store i32 %tmp21, i32* %tmp17, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp22 = getelementptr inbounds %struct.Source* %this, i64 0, i32 0, i32 2
  %tmp23 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp22, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp24 = load %"class.sc_core::sc_process_b"** %tmp14, align 8, !tbaa !0
  %tmp25 = icmp eq %"class.sc_core::sc_process_b"* %tmp24, null
  br i1 %tmp25, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb26

bb26:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp27 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp24, i64 0, i32 15
  %tmp28 = load i32* %tmp27, align 4, !tbaa !5
  %tmp29 = add nsw i32 %tmp28, -1
  store i32 %tmp29, i32* %tmp27, align 4, !tbaa !5
  %tmp30 = icmp eq i32 %tmp29, 0
  br i1 %tmp30, label %bb31, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb31:                                             ; preds = %bb26
  %tmp32 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp33 = icmp eq %"class.sc_core::sc_process_b"* %tmp32, null
  br i1 %tmp33, label %bb38, label %.noexc5

.noexc5:                                          ; preds = %bb31
  %tmp34 = bitcast %"class.sc_core::sc_process_b"* %tmp32 to void (%"class.sc_core::sc_process_b"*)***
  %tmp35 = load void (%"class.sc_core::sc_process_b"*)*** %tmp34, align 8, !tbaa !3
  %tmp36 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp35, i64 6
  %tmp37 = load void (%"class.sc_core::sc_process_b"*)** %tmp36, align 8
  call void %tmp37(%"class.sc_core::sc_process_b"* %tmp32)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb38

bb38:                                             ; preds = %.noexc5, %bb31
  %tmp39 = phi %"class.sc_core::sc_process_b"* [ null, %bb31 ], [ %.pre.i.i.i, %.noexc5 ]
  %tmp40 = icmp eq %"class.sc_core::sc_process_b"* %tmp39, %tmp24
  br i1 %tmp40, label %bb41, label %bb42

bb41:                                             ; preds = %bb38
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str22, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb42:                                             ; preds = %bb38
  store %"class.sc_core::sc_process_b"* %tmp24, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb42, %bb26, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp43 = load %"class.sc_core::sc_process_b"** %tmp12, align 8, !tbaa !0
  %tmp44 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp43, %"class.sc_core::sc_process_b"** %tmp44, align 8, !tbaa !0
  %tmp45 = icmp eq %"class.sc_core::sc_process_b"* %tmp43, null
  br i1 %tmp45, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit7, label %bb46

bb46:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp47 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp43, i64 0, i32 15
  %tmp48 = load i32* %tmp47, align 4, !tbaa !5
  %tmp49 = icmp eq i32 %tmp48, 0
  br i1 %tmp49, label %bb50, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i6

bb50:                                             ; preds = %bb46
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str24, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i6: ; preds = %bb46
  %tmp51 = add nsw i32 %tmp48, 1
  store i32 %tmp51, i32* %tmp47, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit7

_ZN7sc_core17sc_process_handleC1ERKS0_.exit7:     ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i6, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp52 = getelementptr inbounds %struct.Source* %this, i64 0, i32 0, i32 3
  %tmp53 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp52, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp54 = load %"class.sc_core::sc_process_b"** %tmp44, align 8, !tbaa !0
  %tmp55 = icmp eq %"class.sc_core::sc_process_b"* %tmp54, null
  br i1 %tmp55, label %_ZN7sc_core17sc_process_handleD1Ev.exit10, label %bb56

bb56:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit7
  %tmp57 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp54, i64 0, i32 15
  %tmp58 = load i32* %tmp57, align 4, !tbaa !5
  %tmp59 = add nsw i32 %tmp58, -1
  store i32 %tmp59, i32* %tmp57, align 4, !tbaa !5
  %tmp60 = icmp eq i32 %tmp59, 0
  br i1 %tmp60, label %bb61, label %_ZN7sc_core17sc_process_handleD1Ev.exit10

bb61:                                             ; preds = %bb56
  %tmp62 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp63 = icmp eq %"class.sc_core::sc_process_b"* %tmp62, null
  br i1 %tmp63, label %bb68, label %.noexc9

.noexc9:                                          ; preds = %bb61
  %tmp64 = bitcast %"class.sc_core::sc_process_b"* %tmp62 to void (%"class.sc_core::sc_process_b"*)***
  %tmp65 = load void (%"class.sc_core::sc_process_b"*)*** %tmp64, align 8, !tbaa !3
  %tmp66 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp65, i64 6
  %tmp67 = load void (%"class.sc_core::sc_process_b"*)** %tmp66, align 8
  call void %tmp67(%"class.sc_core::sc_process_b"* %tmp62)
  %.pre.i.i.i8 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb68

bb68:                                             ; preds = %.noexc9, %bb61
  %tmp69 = phi %"class.sc_core::sc_process_b"* [ null, %bb61 ], [ %.pre.i.i.i8, %.noexc9 ]
  %tmp70 = icmp eq %"class.sc_core::sc_process_b"* %tmp69, %tmp54
  br i1 %tmp70, label %bb71, label %bb72

bb71:                                             ; preds = %bb68
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str22, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb72:                                             ; preds = %bb68
  store %"class.sc_core::sc_process_b"* %tmp54, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit10

_ZN7sc_core17sc_process_handleD1Ev.exit10:        ; preds = %bb72, %bb56, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit7
  %tmp73 = load %"class.sc_core::sc_process_b"** %tmp12, align 8, !tbaa !0
  %tmp74 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp73, %"class.sc_core::sc_process_b"** %tmp74, align 8, !tbaa !0
  %tmp75 = icmp eq %"class.sc_core::sc_process_b"* %tmp73, null
  br i1 %tmp75, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit12, label %bb76

bb76:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit10
  %tmp77 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp73, i64 0, i32 15
  %tmp78 = load i32* %tmp77, align 4, !tbaa !5
  %tmp79 = icmp eq i32 %tmp78, 0
  br i1 %tmp79, label %bb80, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i11

bb80:                                             ; preds = %bb76
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str24, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i11: ; preds = %bb76
  %tmp81 = add nsw i32 %tmp78, 1
  store i32 %tmp81, i32* %tmp77, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit12

_ZN7sc_core17sc_process_handleC1ERKS0_.exit12:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i11, %_ZN7sc_core17sc_process_handleD1Ev.exit10
  %tmp82 = getelementptr inbounds %struct.Source* %this, i64 0, i32 0, i32 4
  %tmp83 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp82, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp84 = load %"class.sc_core::sc_process_b"** %tmp74, align 8, !tbaa !0
  %tmp85 = icmp eq %"class.sc_core::sc_process_b"* %tmp84, null
  br i1 %tmp85, label %_ZN7sc_core17sc_process_handleD1Ev.exit15, label %bb86

bb86:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit12
  %tmp87 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp84, i64 0, i32 15
  %tmp88 = load i32* %tmp87, align 4, !tbaa !5
  %tmp89 = add nsw i32 %tmp88, -1
  store i32 %tmp89, i32* %tmp87, align 4, !tbaa !5
  %tmp90 = icmp eq i32 %tmp89, 0
  br i1 %tmp90, label %bb91, label %_ZN7sc_core17sc_process_handleD1Ev.exit15

bb91:                                             ; preds = %bb86
  %tmp92 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp93 = icmp eq %"class.sc_core::sc_process_b"* %tmp92, null
  br i1 %tmp93, label %bb98, label %.noexc14

.noexc14:                                         ; preds = %bb91
  %tmp94 = bitcast %"class.sc_core::sc_process_b"* %tmp92 to void (%"class.sc_core::sc_process_b"*)***
  %tmp95 = load void (%"class.sc_core::sc_process_b"*)*** %tmp94, align 8, !tbaa !3
  %tmp96 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp95, i64 6
  %tmp97 = load void (%"class.sc_core::sc_process_b"*)** %tmp96, align 8
  call void %tmp97(%"class.sc_core::sc_process_b"* %tmp92)
  %.pre.i.i.i13 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb98

bb98:                                             ; preds = %.noexc14, %bb91
  %tmp99 = phi %"class.sc_core::sc_process_b"* [ null, %bb91 ], [ %.pre.i.i.i13, %.noexc14 ]
  %tmp100 = icmp eq %"class.sc_core::sc_process_b"* %tmp99, %tmp84
  br i1 %tmp100, label %bb101, label %bb102

bb101:                                            ; preds = %bb98
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str22, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb102:                                            ; preds = %bb98
  store %"class.sc_core::sc_process_b"* %tmp84, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit15

_ZN7sc_core17sc_process_handleD1Ev.exit15:        ; preds = %bb102, %bb86, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit12
  %tmp103 = load %"class.sc_core::sc_process_b"** %tmp12, align 8, !tbaa !0
  %tmp104 = icmp eq %"class.sc_core::sc_process_b"* %tmp103, null
  br i1 %tmp104, label %_ZN7sc_core17sc_process_handleD1Ev.exit18, label %bb105

bb105:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit15
  %tmp106 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp103, i64 0, i32 15
  %tmp107 = load i32* %tmp106, align 4, !tbaa !5
  %tmp108 = add nsw i32 %tmp107, -1
  store i32 %tmp108, i32* %tmp106, align 4, !tbaa !5
  %tmp109 = icmp eq i32 %tmp108, 0
  br i1 %tmp109, label %bb110, label %_ZN7sc_core17sc_process_handleD1Ev.exit18

bb110:                                            ; preds = %bb105
  %tmp111 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp112 = icmp eq %"class.sc_core::sc_process_b"* %tmp111, null
  br i1 %tmp112, label %bb117, label %.noexc17

.noexc17:                                         ; preds = %bb110
  %tmp113 = bitcast %"class.sc_core::sc_process_b"* %tmp111 to void (%"class.sc_core::sc_process_b"*)***
  %tmp114 = load void (%"class.sc_core::sc_process_b"*)*** %tmp113, align 8, !tbaa !3
  %tmp115 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp114, i64 6
  %tmp116 = load void (%"class.sc_core::sc_process_b"*)** %tmp115, align 8
  call void %tmp116(%"class.sc_core::sc_process_b"* %tmp111)
  %.pre.i.i.i16 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb117

bb117:                                            ; preds = %.noexc17, %bb110
  %tmp118 = phi %"class.sc_core::sc_process_b"* [ null, %bb110 ], [ %.pre.i.i.i16, %.noexc17 ]
  %tmp119 = icmp eq %"class.sc_core::sc_process_b"* %tmp118, %tmp103
  br i1 %tmp119, label %bb120, label %bb121

bb120:                                            ; preds = %bb117
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str22, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str23, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb121:                                            ; preds = %bb117
  store %"class.sc_core::sc_process_b"* %tmp103, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit18

_ZN7sc_core17sc_process_handleD1Ev.exit18:        ; preds = %bb121, %bb105, %_ZN7sc_core17sc_process_handleD1Ev.exit15
  ret void
}

define linkonce_odr void @_ZN6Source7computeEv(%struct.Source* nocapture %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %struct.Source* %this, i64 0, i32 1
  %tmp1 = load %struct.MyModule** %tmp, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %struct.MyModule* %tmp1, i64 0, i32 1
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp2)
  ret void
}

define linkonce_odr void @_ZN6SourceD0Ev(%struct.Source* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN6SourceD1Ev.exit:
  %tmp = getelementptr inbounds %struct.Source* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp)
  %tmp1 = bitcast %struct.Source* %this to i8*
  tail call void @_ZdlPv(i8* %tmp1) nounwind
  ret void
}

define linkonce_odr void @_ZThn40_N6SourceD0Ev(%struct.Source* %this) {
_ZN6SourceD0Ev.exit:
  %tmp = getelementptr inbounds %struct.Source* %this, i64 -1, i32 0, i32 9
  %tmp1 = bitcast %"class.std::vector.10"* %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp1)
  %tmp2 = bitcast %"class.std::vector.10"* %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp2) nounwind
  ret void
}

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

!0 = metadata !{metadata !"any pointer", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA"}
!3 = metadata !{metadata !"vtable pointer", metadata !2}
!4 = metadata !{metadata !"_ZTSSt12_Ios_Iostate", metadata !1}
!5 = metadata !{metadata !"int", metadata !1}
!6 = metadata !{metadata !"branch_weights", i32 64, i32 4}
!7 = metadata !{metadata !"long", metadata !1}
!8 = metadata !{metadata !"bool", metadata !1}
!9 = metadata !{metadata !"_ZTSSt13_Ios_Openmode", metadata !1}
!10 = metadata !{metadata !"_ZTSN7sc_core8sc_event8notify_tE", metadata !1}
