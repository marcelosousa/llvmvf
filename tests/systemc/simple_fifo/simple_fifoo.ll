; ModuleID = 'simple_fifoo.bc'
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
%"class.std::type_info" = type { i32 (...)**, i8* }
%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep" = type { %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep_base" }
%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep_base" = type { i64, i64, i32 }
%class.top = type { %"class.sc_core::sc_module", %class.fifo*, %class.producer*, %class.consumer* }
%"class.sc_core::sc_module" = type { %"class.sc_core::sc_object", %"class.sc_core::sc_process_host", %"class.sc_core::sc_sensitive", %"class.sc_core::sc_sensitive_pos", %"class.sc_core::sc_sensitive_neg", i8, %"class.std::vector"*, i32, %"class.sc_core::sc_name_gen"*, %"class.std::vector.10", %"class.sc_core::sc_module_name"* }
%"class.sc_core::sc_sensitive" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_pos" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_neg" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_module_name" = type { i8*, %"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*, %"class.sc_core::sc_simcontext"*, i8 }
%class.fifo = type { %"class.sc_core::sc_module", %class.write_if, %class.read_if, [10 x i8], i32, i32, %"class.sc_core::sc_event", %"class.sc_core::sc_event" }
%class.write_if = type { %"class.sc_core::sc_interface" }
%class.read_if = type { %"class.sc_core::sc_interface" }
%class.producer = type { %"class.sc_core::sc_module", %"class.sc_core::sc_port" }
%"class.sc_core::sc_port" = type { %"class.sc_core::sc_port_b" }
%"class.sc_core::sc_port_b" = type { %"class.sc_core::sc_port_base", %class.write_if*, %"class.std::vector.54" }
%"class.std::vector.54" = type { %"struct.std::_Vector_base.55" }
%"struct.std::_Vector_base.55" = type { %"struct.std::_Vector_base<write_if *, std::allocator<write_if *> >::_Vector_impl" }
%"struct.std::_Vector_base<write_if *, std::allocator<write_if *> >::_Vector_impl" = type { %class.write_if**, %class.write_if**, %class.write_if** }
%class.consumer = type { %"class.sc_core::sc_module", %"class.sc_core::sc_port.59" }
%"class.sc_core::sc_port.59" = type { %"class.sc_core::sc_port_b.60" }
%"class.sc_core::sc_port_b.60" = type { %"class.sc_core::sc_port_base", %class.read_if*, %"class.std::vector.61" }
%"class.std::vector.61" = type { %"struct.std::_Vector_base.62" }
%"struct.std::_Vector_base.62" = type { %"struct.std::_Vector_base<read_if *, std::allocator<read_if *> >::_Vector_impl" }
%"struct.std::_Vector_base<read_if *, std::allocator<read_if *> >::_Vector_impl" = type { %class.read_if**, %class.read_if**, %class.read_if** }
%"class.sc_core::sc_process_handle" = type { %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_spawn_options" = type opaque
%"class.sc_core::sc_event_finder" = type { i32 (...)**, %"class.sc_core::sc_port_base"* }

@_ZStL8__ioinit = internal global %"class.std::ios_base::Init" zeroinitializer, align 1
@__dso_handle = external global i8
@_ZN7sc_coreL17api_version_checkE = internal global %"class.sc_core::sc_api_version_2_2_0" zeroinitializer, align 1
@_ZTVN10__cxxabiv121__vmi_class_type_infoE = external global i8*
@_ZTVN10__cxxabiv117__class_type_infoE = external global i8*
@_ZTSN7sc_core12sc_interfaceE = available_externally constant [25 x i8] c"N7sc_core12sc_interfaceE\00"
@_ZTIN7sc_core12sc_interfaceE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([25 x i8]* @_ZTSN7sc_core12sc_interfaceE, i32 0, i32 0) }
@_ZSt4cout = external global %"class.std::basic_ostream"
@.str = private unnamed_addr constant [14 x i8] c"obj->kind()= \00", align 1
@.str2 = private unnamed_addr constant [22 x i8] c" typeid(*obj).name()=\00", align 1
@.str3 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@.str4 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str5 = private unnamed_addr constant [7 x i8] c" obj= \00", align 1
@.str6 = private unnamed_addr constant [5 x i8] c"Top1\00", align 1
@_ZTVN10__cxxabiv120__si_class_type_infoE = external global i8*
@_ZTSN7sc_core9sc_objectE = available_externally constant [21 x i8] c"N7sc_core9sc_objectE\00"
@_ZTIN7sc_core9sc_objectE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_objectE, i32 0, i32 0) }
@_ZNSs4_Rep20_S_empty_rep_storageE = external global [0 x i64]
@_ZTV3top = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI3top to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.top*)* @_ZN3topD1Ev to i8*), i8* bitcast (void (%class.top*)* @_ZN3topD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI3top to i8*), i8* bitcast (void (%class.top*)* @_ZThn40_N3topD1Ev to i8*), i8* bitcast (void (%class.top*)* @_ZThn40_N3topD0Ev to i8*)]
@.str13 = private unnamed_addr constant [6 x i8] c"Fifo1\00", align 1
@.str14 = private unnamed_addr constant [10 x i8] c"Producer1\00", align 1
@.str15 = private unnamed_addr constant [10 x i8] c"Consumer1\00", align 1
@_ZTS3top = linkonce_odr constant [5 x i8] c"3top\00"
@_ZTSN7sc_core9sc_moduleE = available_externally constant [21 x i8] c"N7sc_core9sc_moduleE\00"
@_ZTSN7sc_core15sc_process_hostE = linkonce_odr constant [28 x i8] c"N7sc_core15sc_process_hostE\00"
@_ZTIN7sc_core15sc_process_hostE = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_process_hostE, i32 0, i32 0) }
@_ZTIN7sc_core9sc_moduleE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_moduleE, i32 0, i32 0), i32 0, i32 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*), i64 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core15sc_process_hostE to i8*), i64 10242 }
@_ZTI3top = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([5 x i8]* @_ZTS3top, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@.str16 = private unnamed_addr constant [10 x i8] c"sc_module\00", align 1
@_ZTV8consumer = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI8consumer to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.consumer*)* @_ZN8consumerD1Ev to i8*), i8* bitcast (void (%class.consumer*)* @_ZN8consumerD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI8consumer to i8*), i8* bitcast (void (%class.consumer*)* @_ZThn40_N8consumerD1Ev to i8*), i8* bitcast (void (%class.consumer*)* @_ZThn40_N8consumerD0Ev to i8*)]
@.str17 = private unnamed_addr constant [5 x i8] c"main\00", align 1
@_ZTS8consumer = linkonce_odr constant [10 x i8] c"8consumer\00"
@_ZTI8consumer = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([10 x i8]* @_ZTS8consumer, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@_ZTVN7sc_core9sc_port_bI7read_ifEE = linkonce_odr unnamed_addr constant [22 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bI7read_ifEE to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_base"*)* @_ZNK7sc_core12sc_port_base4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_object"*)* @_ZNK7sc_core9sc_object17get_child_objectsEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.60"*)* @_ZN7sc_core9sc_port_bI7read_ifED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.60"*)* @_ZN7sc_core9sc_port_bI7read_ifED0Ev to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b.60"*)* @_ZN7sc_core9sc_port_bI7read_ifE13get_interfaceEv to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b.60"*)* @_ZNK7sc_core9sc_port_bI7read_ifE13get_interfaceEv to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bI7read_ifE5vbindERNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_port_base"*)* @_ZN7sc_core9sc_port_bI7read_ifE5vbindERNS_12sc_port_baseE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bI7read_ifE13add_interfaceEPNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.60"*)* @_ZN7sc_core9sc_port_bI7read_ifE15interface_countEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_b.60"*)* @_ZNK7sc_core9sc_port_bI7read_ifE11if_typenameEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base17end_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE to i8*)]
@_ZTSN7sc_core9sc_port_bI7read_ifEE = linkonce_odr constant [31 x i8] c"N7sc_core9sc_port_bI7read_ifEE\00"
@_ZTSN7sc_core12sc_port_baseE = available_externally constant [25 x i8] c"N7sc_core12sc_port_baseE\00"
@_ZTIN7sc_core12sc_port_baseE = available_externally unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([25 x i8]* @_ZTSN7sc_core12sc_port_baseE, i32 0, i32 0), i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*) }
@_ZTIN7sc_core9sc_port_bI7read_ifEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([31 x i8]* @_ZTSN7sc_core9sc_port_bI7read_ifEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core12sc_port_baseE to i8*) }
@.str18 = private unnamed_addr constant [13 x i8] c"iface_p != 0\00", align 1
@.str19 = private unnamed_addr constant [48 x i8] c"/usr/local/include/sysc/communication/sc_port.h\00", align 1
@__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE = private unnamed_addr constant [124 x i8] c"virtual void sc_core::sc_port_b<read_if>::make_sensitive(sc_method_handle, sc_core::sc_event_finder *) const [IF = read_if]\00", align 1
@__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE = private unnamed_addr constant [124 x i8] c"virtual void sc_core::sc_port_b<read_if>::make_sensitive(sc_thread_handle, sc_core::sc_event_finder *) const [IF = read_if]\00", align 1
@_ZTS7read_if = linkonce_odr constant [9 x i8] c"7read_if\00"
@_ZTI7read_if = linkonce_odr unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([9 x i8]* @_ZTS7read_if, i32 0, i32 0), i32 0, i32 1, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i64 -12285 }
@.str20 = private unnamed_addr constant [11 x i8] c"iface != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core9sc_port_bI7read_ifE13add_interfaceEPNS_12sc_interfaceE = private unnamed_addr constant [96 x i8] c"virtual void sc_core::sc_port_b<read_if>::add_interface(sc_core::sc_interface *) [IF = read_if]\00", align 1
@_ZN7sc_core22SC_ID_BIND_IF_TO_PORT_E = external constant [0 x i8]
@.str21 = private unnamed_addr constant [32 x i8] c"interface already bound to port\00", align 1
@.str22 = private unnamed_addr constant [13 x i8] c"sc_port_base\00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str23 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str24 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str25 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_increment()\00", align 1
@.str26 = private unnamed_addr constant [4 x i8] c"<1>\00", align 1
@.str27 = private unnamed_addr constant [4 x i8] c"<9>\00", align 1
@_ZN7sc_core13SC_ID_GET_IF_E = external constant [0 x i8]
@.str28 = private unnamed_addr constant [18 x i8] c"port is not bound\00", align 1
@_ZTVN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EEE = linkonce_odr unnamed_addr constant [22 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EEE to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port.59"*)* @_ZNK7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EE4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_object"*)* @_ZNK7sc_core9sc_object17get_child_objectsEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port.59"*)* @_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_port.59"*)* @_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED0Ev to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b.60"*)* @_ZN7sc_core9sc_port_bI7read_ifE13get_interfaceEv to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b.60"*)* @_ZNK7sc_core9sc_port_bI7read_ifE13get_interfaceEv to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bI7read_ifE5vbindERNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_port_base"*)* @_ZN7sc_core9sc_port_bI7read_ifE5vbindERNS_12sc_port_baseE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bI7read_ifE13add_interfaceEPNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b.60"*)* @_ZN7sc_core9sc_port_bI7read_ifE15interface_countEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_b.60"*)* @_ZNK7sc_core9sc_port_bI7read_ifE11if_typenameEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base17end_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b.60"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE to i8*)]
@_ZTSN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EEE = linkonce_odr constant [56 x i8] c"N7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EEE\00"
@_ZTIN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([56 x i8]* @_ZTSN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bI7read_ifEE to i8*) }
@.str29 = private unnamed_addr constant [8 x i8] c"sc_port\00", align 1
@_ZTV8producer = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI8producer to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.producer*)* @_ZN8producerD1Ev to i8*), i8* bitcast (void (%class.producer*)* @_ZN8producerD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI8producer to i8*), i8* bitcast (void (%class.producer*)* @_ZThn40_N8producerD1Ev to i8*), i8* bitcast (void (%class.producer*)* @_ZThn40_N8producerD0Ev to i8*)]
@_ZTS8producer = linkonce_odr constant [10 x i8] c"8producer\00"
@_ZTI8producer = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([10 x i8]* @_ZTS8producer, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@_ZTVN7sc_core9sc_port_bI8write_ifEE = linkonce_odr unnamed_addr constant [22 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bI8write_ifEE to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_base"*)* @_ZNK7sc_core12sc_port_base4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_object"*)* @_ZNK7sc_core9sc_object17get_child_objectsEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*)* @_ZN7sc_core9sc_port_bI8write_ifED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*)* @_ZN7sc_core9sc_port_bI8write_ifED0Ev to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b"*)* @_ZN7sc_core9sc_port_bI8write_ifE13get_interfaceEv to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b"*)* @_ZNK7sc_core9sc_port_bI8write_ifE13get_interfaceEv to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bI8write_ifE5vbindERNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_port_base"*)* @_ZN7sc_core9sc_port_bI8write_ifE5vbindERNS_12sc_port_baseE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bI8write_ifE13add_interfaceEPNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*)* @_ZN7sc_core9sc_port_bI8write_ifE15interface_countEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_b"*)* @_ZNK7sc_core9sc_port_bI8write_ifE11if_typenameEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base17end_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE to i8*)]
@_ZTSN7sc_core9sc_port_bI8write_ifEE = linkonce_odr constant [32 x i8] c"N7sc_core9sc_port_bI8write_ifEE\00"
@_ZTIN7sc_core9sc_port_bI8write_ifEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([32 x i8]* @_ZTSN7sc_core9sc_port_bI8write_ifEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core12sc_port_baseE to i8*) }
@__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE = private unnamed_addr constant [126 x i8] c"virtual void sc_core::sc_port_b<write_if>::make_sensitive(sc_method_handle, sc_core::sc_event_finder *) const [IF = write_if]\00", align 1
@__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE = private unnamed_addr constant [126 x i8] c"virtual void sc_core::sc_port_b<write_if>::make_sensitive(sc_thread_handle, sc_core::sc_event_finder *) const [IF = write_if]\00", align 1
@_ZTS8write_if = linkonce_odr constant [10 x i8] c"8write_if\00"
@_ZTI8write_if = linkonce_odr unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([10 x i8]* @_ZTS8write_if, i32 0, i32 0), i32 0, i32 1, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i64 -12285 }
@__PRETTY_FUNCTION__._ZN7sc_core9sc_port_bI8write_ifE13add_interfaceEPNS_12sc_interfaceE = private unnamed_addr constant [98 x i8] c"virtual void sc_core::sc_port_b<write_if>::add_interface(sc_core::sc_interface *) [IF = write_if]\00", align 1
@.str30 = private unnamed_addr constant [66 x i8] c"Visit www.systemc.org and see what SystemC can do for you today!\0A\00", align 1
@_ZTVN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EEE = linkonce_odr unnamed_addr constant [22 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EEE to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port"*)* @_ZNK7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EE4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_object"*)* @_ZNK7sc_core9sc_object17get_child_objectsEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port"*)* @_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED1Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_port"*)* @_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED0Ev to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b"*)* @_ZN7sc_core9sc_port_bI8write_ifE13get_interfaceEv to i8*), i8* bitcast (%"class.sc_core::sc_interface"* (%"class.sc_core::sc_port_b"*)* @_ZNK7sc_core9sc_port_bI8write_ifE13get_interfaceEv to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bI8write_ifE5vbindERNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_port_base"*)* @_ZN7sc_core9sc_port_bI8write_ifE5vbindERNS_12sc_port_baseE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_interface"*)* @_ZN7sc_core9sc_port_bI8write_ifE13add_interfaceEPNS_12sc_interfaceE to i8*), i8* bitcast (i32 (%"class.sc_core::sc_port_b"*)* @_ZN7sc_core9sc_port_bI8write_ifE15interface_countEv to i8*), i8* bitcast (i8* (%"class.sc_core::sc_port_b"*)* @_ZNK7sc_core9sc_port_bI8write_ifE11if_typenameEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_base"*)* @_ZN7sc_core12sc_port_base17end_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_thread_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE to i8*), i8* bitcast (void (%"class.sc_core::sc_port_b"*, %"class.sc_core::sc_method_process"*, %"class.sc_core::sc_event_finder"*)* @_ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE to i8*)]
@_ZTSN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EEE = linkonce_odr constant [57 x i8] c"N7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EEE\00"
@_ZTIN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EEE = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([57 x i8]* @_ZTSN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EEE, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bI8write_ifEE to i8*) }
@_ZTV4fifo = linkonce_odr unnamed_addr constant [46 x i8*] [i8* inttoptr (i64 184 to i8*), i8* null, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64, i8*, i64 }* @_ZTI4fifo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.fifo*)* @_ZN4fifoD1Ev to i8*), i8* bitcast (void (%class.fifo*)* @_ZN4fifoD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* bitcast (void (%class.fifo*, i8)* @_ZN4fifo5writeEc to i8*), i8* bitcast (void (%class.fifo*, i8*)* @_ZN4fifo4readERc to i8*), i8* bitcast (void (%class.fifo*)* @_ZN4fifo5resetEv to i8*), i8* bitcast (i32 (%class.fifo*)* @_ZN4fifo13num_availableEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64, i8*, i64 }* @_ZTI4fifo to i8*), i8* bitcast (void (%class.fifo*)* @_ZThn40_N4fifoD1Ev to i8*), i8* bitcast (void (%class.fifo*)* @_ZThn40_N4fifoD0Ev to i8*), i8* null, i8* inttoptr (i64 -184 to i8*), i8* null, i8* null, i8* inttoptr (i64 -184 to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64, i8*, i64 }* @_ZTI4fifo to i8*), i8* bitcast (void (%"class.sc_core::sc_interface"*, %"class.sc_core::sc_port_base"*, i8*)* @_ZN7sc_core12sc_interface13register_portERNS_12sc_port_baseEPKc to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)* @_ZNK7sc_core12sc_interface13default_eventEv to i8*), i8* bitcast (void (%class.fifo*)* @_ZThn184_N4fifoD1Ev to i8*), i8* bitcast (void (%class.fifo*)* @_ZThn184_N4fifoD0Ev to i8*), i8* bitcast (void (%class.fifo*, i8)* @_ZThn184_N4fifo5writeEc to i8*), i8* bitcast (void (%class.fifo*)* @_ZThn184_N4fifo5resetEv to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -192 to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -192 to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64, i8*, i64 }* @_ZTI4fifo to i8*), i8* null, i8* null, i8* bitcast (void (%class.fifo*)* @_ZThn192_N4fifoD1Ev to i8*), i8* bitcast (void (%class.fifo*)* @_ZThn192_N4fifoD0Ev to i8*), i8* bitcast (void (%class.fifo*, i8*)* @_ZThn192_N4fifo4readERc to i8*), i8* bitcast (i32 (%class.fifo*)* @_ZThn192_N4fifo13num_availableEv to i8*)]
@_ZTS4fifo = linkonce_odr constant [6 x i8] c"4fifo\00"
@_ZTI4fifo = linkonce_odr unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([6 x i8]* @_ZTS4fifo, i32 0, i32 0), i32 2, i32 3, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*), i64 2, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI8write_if to i8*), i64 47106, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI7read_if to i8*), i64 49154 }
@_ZTC4fifo184_8write_if = linkonce_odr unnamed_addr constant [12 x i8*] [i8* null, i8* null, i8* null, i8* null, i8* null, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI8write_if to i8*), i8* bitcast (void (%"class.sc_core::sc_interface"*, %"class.sc_core::sc_port_base"*, i8*)* @_ZN7sc_core12sc_interface13register_portERNS_12sc_port_baseEPKc to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)* @_ZNK7sc_core12sc_interface13default_eventEv to i8*), i8* bitcast (void (%class.write_if*)* @_ZN8write_ifD1Ev to i8*), i8* bitcast (void (%class.write_if*)* @_ZN8write_ifD0Ev to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*)]
@_ZTC4fifo192_7read_if = linkonce_odr unnamed_addr constant [21 x i8*] [i8* inttoptr (i64 -8 to i8*), i8* null, i8* inttoptr (i64 -8 to i8*), i8* inttoptr (i64 -8 to i8*), i8* null, i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI7read_if to i8*), i8* null, i8* null, i8* bitcast (void (%class.read_if*)* @_ZN7read_ifD1Ev to i8*), i8* bitcast (void (%class.read_if*)* @_ZN7read_ifD0Ev to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* inttoptr (i64 8 to i8*), i8* null, i8* null, i8* inttoptr (i64 8 to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI7read_if to i8*), i8* bitcast (void (%"class.sc_core::sc_interface"*, %"class.sc_core::sc_port_base"*, i8*)* @_ZN7sc_core12sc_interface13register_portERNS_12sc_port_baseEPKc to i8*), i8* bitcast (%"class.sc_core::sc_event"* (%"class.sc_core::sc_interface"*)* @_ZNK7sc_core12sc_interface13default_eventEv to i8*), i8* bitcast (void (%class.read_if*)* @_ZTv0_n40_N7read_ifD1Ev to i8*), i8* bitcast (void (%class.read_if*)* @_ZTv0_n40_N7read_ifD0Ev to i8*)]
@_ZN7sc_core18sc_curr_simcontextE = external global %"class.sc_core::sc_simcontext"*
@_ZN7sc_core25sc_default_global_contextE = external global %"class.sc_core::sc_simcontext"*
@llvm.global_ctors = appending global [1 x { i32, void ()* }] [{ i32, void ()* } { i32 65535, void ()* @_GLOBAL__I_a }]

declare void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"*)

declare void @_ZNSt8ios_base4InitD1Ev(%"class.std::ios_base::Init"*)

declare i32 @__cxa_atexit(void (i8*)*, i8*, i8*) nounwind

declare void @_ZN7sc_core20sc_api_version_2_2_0C1Ev(%"class.sc_core::sc_api_version_2_2_0"*)

declare void @_ZNSsC1EPKcRKSaIcE(%"class.std::basic_string"*, i8*, %"class.std::allocator.40"*)

declare i32 @__gxx_personality_v0(...)

declare void @_ZSt9terminatev()

define void @_Z17recursive_descentPN7sc_core9sc_objectE(%"class.sc_core::sc_object"* %obj) uwtable {
bb:
  %tmp = alloca %"class.std::allocator.40", align 1
  %tmp1 = alloca %"class.std::allocator.40", align 1
  %tmp2 = alloca %"class.std::allocator.40", align 1
  %topname = alloca %"class.std::basic_string", align 8
  %tmp3 = alloca %"class.std::allocator.40", align 1
  %tmp4 = alloca %"class.std::basic_string", align 8
  %tmp5 = alloca %"class.std::basic_string", align 8
  %tmp6 = alloca %"class.std::allocator.40", align 1
  %tmp7 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([14 x i8]* @.str, i64 0, i64 0), i64 13)
  %tmp8 = bitcast %"class.sc_core::sc_object"* %obj to i8* (%"class.sc_core::sc_object"*)***
  %tmp9 = load i8* (%"class.sc_core::sc_object"*)*** %tmp8, align 8, !tbaa !0
  %tmp10 = getelementptr inbounds i8* (%"class.sc_core::sc_object"*)** %tmp9, i64 3
  %tmp11 = load i8* (%"class.sc_core::sc_object"*)** %tmp10, align 8
  %tmp12 = call i8* %tmp11(%"class.sc_core::sc_object"* %obj)
  %tmp13 = icmp eq i8* %tmp12, null
  br i1 %tmp13, label %bb14, label %bb25

bb14:                                             ; preds = %bb
  %tmp15 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8**), align 8, !tbaa !0
  %tmp16 = getelementptr i8* %tmp15, i64 -24
  %tmp17 = bitcast i8* %tmp16 to i64*
  %tmp18 = load i64* %tmp17, align 8
  %tmp19 = getelementptr i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %tmp18
  %tmp20 = bitcast i8* %tmp19 to %"class.std::basic_ios"*
  %.sum.i = add i64 %tmp18, 32
  %tmp21 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %.sum.i
  %tmp22 = bitcast i8* %tmp21 to i32*
  %tmp23 = load i32* %tmp22, align 4, !tbaa !2
  %tmp24 = or i32 %tmp23, 1
  call void @_ZNSt9basic_iosIcSt11char_traitsIcEE5clearESt12_Ios_Iostate(%"class.std::basic_ios"* %tmp20, i32 %tmp24)
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit

bb25:                                             ; preds = %bb
  %tmp26 = call i64 @strlen(i8* %tmp12) nounwind
  %tmp27 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* %tmp12, i64 %tmp26)
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit: ; preds = %bb25, %bb14
  %tmp28 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8**), align 8, !tbaa !0
  %tmp29 = getelementptr i8* %tmp28, i64 -24
  %tmp30 = bitcast i8* %tmp29 to i64*
  %tmp31 = load i64* %tmp30, align 8
  %.sum = add i64 %tmp31, 240
  %tmp32 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %.sum
  %tmp33 = bitcast i8* %tmp32 to %"class.std::ctype"**
  %tmp34 = load %"class.std::ctype"** %tmp33, align 8, !tbaa !4
  %tmp35 = icmp eq %"class.std::ctype"* %tmp34, null
  br i1 %tmp35, label %bb36, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit54

bb36:                                             ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit54:  ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit
  %tmp37 = getelementptr inbounds %"class.std::ctype"* %tmp34, i64 0, i32 6
  %tmp38 = load i8* %tmp37, align 1, !tbaa !3
  %tmp39 = icmp eq i8 %tmp38, 0
  br i1 %tmp39, label %bb43, label %bb40

bb40:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit54
  %tmp41 = getelementptr inbounds %"class.std::ctype"* %tmp34, i64 0, i32 7, i64 10
  %tmp42 = load i8* %tmp41, align 1, !tbaa !3
  br label %_ZNKSt5ctypeIcE5widenEc.exit44

bb43:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit54
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp34)
  %tmp44 = bitcast %"class.std::ctype"* %tmp34 to i8 (%"class.std::ctype"*, i8)***
  %tmp45 = load i8 (%"class.std::ctype"*, i8)*** %tmp44, align 8, !tbaa !0
  %tmp46 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp45, i64 6
  %tmp47 = load i8 (%"class.std::ctype"*, i8)** %tmp46, align 8
  %tmp48 = call signext i8 %tmp47(%"class.std::ctype"* %tmp34, i8 signext 10)
  br label %_ZNKSt5ctypeIcE5widenEc.exit44

_ZNKSt5ctypeIcE5widenEc.exit44:                   ; preds = %bb43, %bb40
  %.0.i43 = phi i8 [ %tmp42, %bb40 ], [ %tmp48, %bb43 ]
  %tmp49 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* @_ZSt4cout, i8 signext %.0.i43)
  %tmp50 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp49)
  %tmp51 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([22 x i8]* @.str2, i64 0, i64 0), i64 21)
  %tmp52 = icmp eq %"class.sc_core::sc_object"* %obj, null
  br i1 %tmp52, label %bb53, label %bb54

bb53:                                             ; preds = %_ZNKSt5ctypeIcE5widenEc.exit44
  call void @__cxa_bad_typeid() noreturn
  unreachable

bb54:                                             ; preds = %_ZNKSt5ctypeIcE5widenEc.exit44
  %tmp55 = bitcast %"class.sc_core::sc_object"* %obj to %"class.std::type_info"***
  %tmp56 = load %"class.std::type_info"*** %tmp55, align 8, !tbaa !0
  %tmp57 = getelementptr inbounds %"class.std::type_info"** %tmp56, i64 -1
  %tmp58 = load %"class.std::type_info"** %tmp57, align 8
  %tmp59 = getelementptr inbounds %"class.std::type_info"* %tmp58, i64 0, i32 1
  %tmp60 = load i8** %tmp59, align 8, !tbaa !4
  %tmp61 = load i8* %tmp60, align 1, !tbaa !3
  %tmp62 = icmp eq i8 %tmp61, 42
  %tmp63 = getelementptr inbounds i8* %tmp60, i64 1
  %tmp64 = select i1 %tmp62, i8* %tmp63, i8* %tmp60
  %tmp65 = icmp eq i8* %tmp64, null
  br i1 %tmp65, label %bb66, label %bb77

bb66:                                             ; preds = %bb54
  %tmp67 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8**), align 8, !tbaa !0
  %tmp68 = getelementptr i8* %tmp67, i64 -24
  %tmp69 = bitcast i8* %tmp68 to i64*
  %tmp70 = load i64* %tmp69, align 8
  %tmp71 = getelementptr i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %tmp70
  %tmp72 = bitcast i8* %tmp71 to %"class.std::basic_ios"*
  %.sum.i5 = add i64 %tmp70, 32
  %tmp73 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %.sum.i5
  %tmp74 = bitcast i8* %tmp73 to i32*
  %tmp75 = load i32* %tmp74, align 4, !tbaa !2
  %tmp76 = or i32 %tmp75, 1
  call void @_ZNSt9basic_iosIcSt11char_traitsIcEE5clearESt12_Ios_Iostate(%"class.std::basic_ios"* %tmp72, i32 %tmp76)
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit6

bb77:                                             ; preds = %bb54
  %tmp78 = call i64 @strlen(i8* %tmp64) nounwind
  %tmp79 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* %tmp64, i64 %tmp78)
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit6

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit6: ; preds = %bb77, %bb66
  %tmp80 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8**), align 8, !tbaa !0
  %tmp81 = getelementptr i8* %tmp80, i64 -24
  %tmp82 = bitcast i8* %tmp81 to i64*
  %tmp83 = load i64* %tmp82, align 8
  %.sum62 = add i64 %tmp83, 240
  %tmp84 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %.sum62
  %tmp85 = bitcast i8* %tmp84 to %"class.std::ctype"**
  %tmp86 = load %"class.std::ctype"** %tmp85, align 8, !tbaa !4
  %tmp87 = icmp eq %"class.std::ctype"* %tmp86, null
  br i1 %tmp87, label %bb88, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit

bb88:                                             ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit6
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit:    ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit6
  %tmp89 = getelementptr inbounds %"class.std::ctype"* %tmp86, i64 0, i32 6
  %tmp90 = load i8* %tmp89, align 1, !tbaa !3
  %tmp91 = icmp eq i8 %tmp90, 0
  br i1 %tmp91, label %bb95, label %bb92

bb92:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit
  %tmp93 = getelementptr inbounds %"class.std::ctype"* %tmp86, i64 0, i32 7, i64 10
  %tmp94 = load i8* %tmp93, align 1, !tbaa !3
  br label %_ZNKSt5ctypeIcE5widenEc.exit

bb95:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp86)
  %tmp96 = bitcast %"class.std::ctype"* %tmp86 to i8 (%"class.std::ctype"*, i8)***
  %tmp97 = load i8 (%"class.std::ctype"*, i8)*** %tmp96, align 8, !tbaa !0
  %tmp98 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp97, i64 6
  %tmp99 = load i8 (%"class.std::ctype"*, i8)** %tmp98, align 8
  %tmp100 = call signext i8 %tmp99(%"class.std::ctype"* %tmp86, i8 signext 10)
  br label %_ZNKSt5ctypeIcE5widenEc.exit

_ZNKSt5ctypeIcE5widenEc.exit:                     ; preds = %bb95, %bb92
  %.0.i = phi i8 [ %tmp94, %bb92 ], [ %tmp100, %bb95 ]
  %tmp101 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* @_ZSt4cout, i8 signext %.0.i)
  %tmp102 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp101)
  call void @_ZNSsC1EPKcRKSaIcE(%"class.std::basic_string"* %topname, i8* getelementptr inbounds ([1 x i8]* @.str3, i64 0, i64 0), %"class.std::allocator.40"* %tmp3)
  %tmp103 = bitcast %"class.sc_core::sc_object"* %obj to i64**
  %tmp104 = load i64** %tmp103, align 8, !tbaa !0
  %tmp105 = getelementptr inbounds i64* %tmp104, i64 -2
  %tmp106 = load i64* %tmp105, align 8
  %tmp107 = bitcast %"class.sc_core::sc_object"* %obj to i8*
  %tmp108 = getelementptr inbounds i8* %tmp107, i64 %tmp106
  %phitmp = icmp eq i8* %tmp108, null
  br i1 %phitmp, label %.critedge, label %_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_.exit

_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_.exit: ; preds = %_ZNKSt5ctypeIcE5widenEc.exit
  %tmp109 = getelementptr inbounds %"class.sc_core::sc_object"* %obj, i64 0, i32 2
  %tmp110 = load i8** %tmp109, align 8, !tbaa !4
  call void @_ZNSsC1EPKcRKSaIcE(%"class.std::basic_string"* %tmp5, i8* getelementptr inbounds ([2 x i8]* @.str4, i64 0, i64 0), %"class.std::allocator.40"* %tmp6)
  %tmp111 = call i64 @strlen(i8* %tmp110) nounwind
  %tmp112 = getelementptr inbounds %"class.std::basic_string"* %tmp4, i64 0, i32 0, i32 0
  store i8* bitcast (i64* getelementptr inbounds ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE, i64 0, i64 3) to i8*), i8** %tmp112, align 8, !tbaa !4
  %tmp113 = getelementptr inbounds %"class.std::basic_string"* %tmp5, i64 0, i32 0, i32 0
  %tmp114 = load i8** %tmp113, align 8, !tbaa !4
  %tmp115 = getelementptr inbounds i8* %tmp114, i64 -24
  %tmp116 = bitcast i8* %tmp115 to i64*
  %tmp117 = load i64* %tmp116, align 8, !tbaa !5
  %tmp118 = add i64 %tmp117, %tmp111
  call void @_ZNSs7reserveEm(%"class.std::basic_string"* %tmp4, i64 %tmp118)
  %tmp119 = call %"class.std::basic_string"* @_ZNSs6appendEPKcm(%"class.std::basic_string"* %tmp4, i8* %tmp110, i64 %tmp111)
  %tmp120 = call %"class.std::basic_string"* @_ZNSs6appendERKSs(%"class.std::basic_string"* %tmp4, %"class.std::basic_string"* %tmp5)
  %tmp121 = call %"class.std::basic_string"* @_ZNSs6appendERKSs(%"class.std::basic_string"* %topname, %"class.std::basic_string"* %tmp4)
  %tmp122 = getelementptr inbounds %"class.std::allocator.40"* %tmp2, i64 0, i32 0
  call void @llvm.lifetime.start(i64 -1, i8* %tmp122)
  %tmp123 = load i8** %tmp112, align 8, !tbaa !4
  %tmp124 = getelementptr inbounds i8* %tmp123, i64 -24
  %tmp125 = icmp eq i8* %tmp124, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp125, label %bb137, label %bb126, !prof !6

bb126:                                            ; preds = %_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_.exit
  %tmp127 = getelementptr inbounds i8* %tmp123, i64 -8
  %tmp128 = bitcast i8* %tmp127 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb129, label %bb131

bb129:                                            ; preds = %bb126
  %tmp130 = atomicrmw add i32* %tmp128, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i

bb131:                                            ; preds = %bb126
  %tmp132 = load i32* %tmp128, align 4, !tbaa !7
  %tmp133 = add nsw i32 %tmp132, -1
  store i32 %tmp133, i32* %tmp128, align 4, !tbaa !7
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i: ; preds = %bb131, %bb129
  %.0.i.i.i.i = phi i32 [ %tmp130, %bb129 ], [ %tmp132, %bb131 ]
  %tmp134 = icmp slt i32 %.0.i.i.i.i, 1
  br i1 %tmp134, label %bb135, label %bb137

bb135:                                            ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i
  %tmp136 = bitcast i8* %tmp124 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp136, %"class.std::allocator.40"* %tmp2) nounwind
  br label %bb137

bb137:                                            ; preds = %bb135, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i, %_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_.exit
  call void @llvm.lifetime.end(i64 -1, i8* %tmp122)
  %tmp138 = getelementptr inbounds %"class.std::allocator.40"* %tmp1, i64 0, i32 0
  call void @llvm.lifetime.start(i64 -1, i8* %tmp138)
  %tmp139 = load i8** %tmp113, align 8, !tbaa !4
  %tmp140 = getelementptr inbounds i8* %tmp139, i64 -24
  %tmp141 = icmp eq i8* %tmp140, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp141, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit10, label %bb142, !prof !6

bb142:                                            ; preds = %bb137
  %tmp143 = getelementptr inbounds i8* %tmp139, i64 -8
  %tmp144 = bitcast i8* %tmp143 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb145, label %bb147

bb145:                                            ; preds = %bb142
  %tmp146 = atomicrmw add i32* %tmp144, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8

bb147:                                            ; preds = %bb142
  %tmp148 = load i32* %tmp144, align 4, !tbaa !7
  %tmp149 = add nsw i32 %tmp148, -1
  store i32 %tmp149, i32* %tmp144, align 4, !tbaa !7
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8: ; preds = %bb147, %bb145
  %.0.i.i.i.i7 = phi i32 [ %tmp146, %bb145 ], [ %tmp148, %bb147 ]
  %tmp150 = icmp slt i32 %.0.i.i.i.i7, 1
  br i1 %tmp150, label %bb151, label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit10

bb151:                                            ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8
  %tmp152 = bitcast i8* %tmp140 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp152, %"class.std::allocator.40"* %tmp1) nounwind
  br label %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit10

_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit10: ; preds = %bb151, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i8, %bb137
  call void @llvm.lifetime.end(i64 -1, i8* %tmp138)
  %tmp153 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([7 x i8]* @.str5, i64 0, i64 0), i64 6)
  %tmp154 = getelementptr inbounds %"class.std::basic_string"* %topname, i64 0, i32 0, i32 0
  %tmp155 = load i8** %tmp154, align 8, !tbaa !4
  %tmp156 = getelementptr inbounds i8* %tmp155, i64 -24
  %tmp157 = bitcast i8* %tmp156 to i64*
  %tmp158 = load i64* %tmp157, align 8, !tbaa !5
  %tmp159 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* %tmp155, i64 %tmp158)
  %tmp160 = bitcast %"class.std::basic_ostream"* %tmp159 to i8**
  %tmp161 = load i8** %tmp160, align 8, !tbaa !0
  %tmp162 = getelementptr i8* %tmp161, i64 -24
  %tmp163 = bitcast i8* %tmp162 to i64*
  %tmp164 = load i64* %tmp163, align 8
  %tmp165 = bitcast %"class.std::basic_ostream"* %tmp159 to i8*
  %.sum63 = add i64 %tmp164, 240
  %tmp166 = getelementptr inbounds i8* %tmp165, i64 %.sum63
  %tmp167 = bitcast i8* %tmp166 to %"class.std::ctype"**
  %tmp168 = load %"class.std::ctype"** %tmp167, align 8, !tbaa !4
  %tmp169 = icmp eq %"class.std::ctype"* %tmp168, null
  br i1 %tmp169, label %bb170, label %.noexc45

bb170:                                            ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit10
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

.noexc45:                                         ; preds = %_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc.exit10
  %tmp171 = getelementptr inbounds %"class.std::ctype"* %tmp168, i64 0, i32 6
  %tmp172 = load i8* %tmp171, align 1, !tbaa !3
  %tmp173 = icmp eq i8 %tmp172, 0
  br i1 %tmp173, label %.noexc49, label %bb174

bb174:                                            ; preds = %.noexc45
  %tmp175 = getelementptr inbounds %"class.std::ctype"* %tmp168, i64 0, i32 7, i64 10
  %tmp176 = load i8* %tmp175, align 1, !tbaa !3
  br label %.noexc

.noexc49:                                         ; preds = %.noexc45
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp168)
  %tmp177 = bitcast %"class.std::ctype"* %tmp168 to i8 (%"class.std::ctype"*, i8)***
  %tmp178 = load i8 (%"class.std::ctype"*, i8)*** %tmp177, align 8, !tbaa !0
  %tmp179 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp178, i64 6
  %tmp180 = load i8 (%"class.std::ctype"*, i8)** %tmp179, align 8
  %tmp181 = call signext i8 %tmp180(%"class.std::ctype"* %tmp168, i8 signext 10)
  br label %.noexc

.noexc:                                           ; preds = %.noexc49, %bb174
  %.0.i48 = phi i8 [ %tmp176, %bb174 ], [ %tmp181, %.noexc49 ]
  %tmp182 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp159, i8 signext %.0.i48)
  %tmp183 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp182)
  %tmp184 = bitcast %"class.std::basic_ostream"* %tmp183 to i8**
  %tmp185 = load i8** %tmp184, align 8, !tbaa !0
  %tmp186 = getelementptr i8* %tmp185, i64 -24
  %tmp187 = bitcast i8* %tmp186 to i64*
  %tmp188 = load i64* %tmp187, align 8
  %tmp189 = bitcast %"class.std::basic_ostream"* %tmp183 to i8*
  %.sum64 = add i64 %tmp188, 240
  %tmp190 = getelementptr inbounds i8* %tmp189, i64 %.sum64
  %tmp191 = bitcast i8* %tmp190 to %"class.std::ctype"**
  %tmp192 = load %"class.std::ctype"** %tmp191, align 8, !tbaa !4
  %tmp193 = icmp eq %"class.std::ctype"* %tmp192, null
  br i1 %tmp193, label %bb194, label %.noexc30

bb194:                                            ; preds = %.noexc
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

.noexc30:                                         ; preds = %.noexc
  %tmp195 = getelementptr inbounds %"class.std::ctype"* %tmp192, i64 0, i32 6
  %tmp196 = load i8* %tmp195, align 1, !tbaa !3
  %tmp197 = icmp eq i8 %tmp196, 0
  br i1 %tmp197, label %.noexc33, label %bb198

bb198:                                            ; preds = %.noexc30
  %tmp199 = getelementptr inbounds %"class.std::ctype"* %tmp192, i64 0, i32 7, i64 10
  %tmp200 = load i8* %tmp199, align 1, !tbaa !3
  br label %.noexc16

.noexc33:                                         ; preds = %.noexc30
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp192)
  %tmp201 = bitcast %"class.std::ctype"* %tmp192 to i8 (%"class.std::ctype"*, i8)***
  %tmp202 = load i8 (%"class.std::ctype"*, i8)*** %tmp201, align 8, !tbaa !0
  %tmp203 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp202, i64 6
  %tmp204 = load i8 (%"class.std::ctype"*, i8)** %tmp203, align 8
  %tmp205 = call signext i8 %tmp204(%"class.std::ctype"* %tmp192, i8 signext 10)
  br label %.noexc16

.noexc16:                                         ; preds = %.noexc33, %bb198
  %.0.i32 = phi i8 [ %tmp200, %bb198 ], [ %tmp205, %.noexc33 ]
  %tmp206 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp183, i8 signext %.0.i32)
  %tmp207 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp206)
  br label %.critedge

.critedge:                                        ; preds = %.noexc16, %_ZNKSt5ctypeIcE5widenEc.exit
  %tmp208 = bitcast %"class.sc_core::sc_object"* %obj to %"class.std::vector.10"* (%"class.sc_core::sc_object"*)***
  %tmp209 = load %"class.std::vector.10"* (%"class.sc_core::sc_object"*)*** %tmp208, align 8, !tbaa !0
  %tmp210 = getelementptr inbounds %"class.std::vector.10"* (%"class.sc_core::sc_object"*)** %tmp209, i64 4
  %tmp211 = load %"class.std::vector.10"* (%"class.sc_core::sc_object"*)** %tmp210, align 8
  %tmp212 = call %"class.std::vector.10"* %tmp211(%"class.sc_core::sc_object"* %obj)
  %tmp213 = getelementptr inbounds %"class.std::vector.10"* %tmp212, i64 0, i32 0, i32 0, i32 1
  %tmp214 = load %"class.sc_core::sc_object"*** %tmp213, align 8, !tbaa !4
  %tmp215 = getelementptr inbounds %"class.std::vector.10"* %tmp212, i64 0, i32 0, i32 0, i32 0
  %tmp216 = load %"class.sc_core::sc_object"*** %tmp215, align 8, !tbaa !4
  %tmp217 = ptrtoint %"class.sc_core::sc_object"** %tmp214 to i64
  %tmp218 = ptrtoint %"class.sc_core::sc_object"** %tmp216 to i64
  %tmp219 = sub i64 %tmp217, %tmp218
  %tmp220 = ashr exact i64 %tmp219, 3
  %tmp221 = icmp eq i64 %tmp220, 0
  br i1 %tmp221, label %bb226, label %bb222

bb222:                                            ; preds = %.critedge
  %tmp223 = icmp ugt i64 %tmp220, 2305843009213693951
  br i1 %tmp223, label %.noexc.i.i.i, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i

.noexc.i.i.i:                                     ; preds = %bb222
  call void @_ZSt17__throw_bad_allocv() noreturn
  unreachable

_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i: ; preds = %bb222
  %tmp224 = call noalias i8* @_Znwm(i64 %tmp219)
  %tmp225 = bitcast i8* %tmp224 to %"class.sc_core::sc_object"**
  %.pre = load %"class.sc_core::sc_object"*** %tmp215, align 8, !tbaa !4
  %.pre65 = load %"class.sc_core::sc_object"*** %tmp213, align 8, !tbaa !4
  br label %bb226

bb226:                                            ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i, %.critedge
  %tmp227 = phi %"class.sc_core::sc_object"** [ %.pre65, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i ], [ %tmp214, %.critedge ]
  %tmp228 = phi %"class.sc_core::sc_object"** [ %.pre, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i ], [ %tmp216, %.critedge ]
  %tmp229 = phi %"class.sc_core::sc_object"** [ %tmp225, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i ], [ null, %.critedge ]
  %tmp230 = ptrtoint %"class.sc_core::sc_object"** %tmp227 to i64
  %tmp231 = ptrtoint %"class.sc_core::sc_object"** %tmp228 to i64
  %tmp232 = sub i64 %tmp230, %tmp231
  %tmp233 = icmp eq %"class.sc_core::sc_object"** %tmp227, %tmp228
  br i1 %tmp233, label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader, label %bb234

bb234:                                            ; preds = %bb226
  %tmp235 = bitcast %"class.sc_core::sc_object"** %tmp229 to i8*
  %tmp236 = bitcast %"class.sc_core::sc_object"** %tmp228 to i8*
  call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp235, i8* %tmp236, i64 %tmp232, i32 8, i1 false) nounwind
  br label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader

_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader: ; preds = %bb234, %bb226
  %tmp237 = ashr exact i64 %tmp232, 3
  br label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit

_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit: ; preds = %bb240, %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader
  %i.0 = phi i32 [ %tmp243, %bb240 ], [ 0, %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader ]
  %tmp238 = zext i32 %i.0 to i64
  %tmp239 = icmp ult i64 %tmp238, %tmp237
  br i1 %tmp239, label %bb240, label %bb244

bb240:                                            ; preds = %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit
  %tmp241 = getelementptr inbounds %"class.sc_core::sc_object"** %tmp229, i64 %tmp238
  %tmp242 = load %"class.sc_core::sc_object"** %tmp241, align 8, !tbaa !4
  call void @_Z17recursive_descentPN7sc_core9sc_objectE(%"class.sc_core::sc_object"* %tmp242)
  %tmp243 = add i32 %i.0, 1
  br label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit

bb244:                                            ; preds = %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit
  %tmp245 = icmp eq %"class.sc_core::sc_object"** %tmp229, null
  br i1 %tmp245, label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EED1Ev.exit39, label %bb246

bb246:                                            ; preds = %bb244
  %tmp247 = bitcast %"class.sc_core::sc_object"** %tmp229 to i8*
  call void @_ZdlPv(i8* %tmp247) nounwind
  br label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EED1Ev.exit39

_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EED1Ev.exit39: ; preds = %bb246, %bb244
  %tmp248 = getelementptr inbounds %"class.std::allocator.40"* %tmp, i64 0, i32 0
  call void @llvm.lifetime.start(i64 -1, i8* %tmp248)
  %tmp249 = getelementptr inbounds %"class.std::basic_string"* %topname, i64 0, i32 0, i32 0
  %tmp250 = load i8** %tmp249, align 8, !tbaa !4
  %tmp251 = getelementptr inbounds i8* %tmp250, i64 -24
  %tmp252 = icmp eq i8* %tmp251, bitcast ([0 x i64]* @_ZNSs4_Rep20_S_empty_rep_storageE to i8*)
  br i1 %tmp252, label %_ZNSsD1Ev.exit42, label %bb253, !prof !6

bb253:                                            ; preds = %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EED1Ev.exit39
  %tmp254 = getelementptr inbounds i8* %tmp250, i64 -8
  %tmp255 = bitcast i8* %tmp254 to i32*
  br i1 icmp ne (i8* bitcast (i32 (i64)* @pthread_cancel to i8*), i8* null), label %bb256, label %bb258

bb256:                                            ; preds = %bb253
  %tmp257 = atomicrmw add i32* %tmp255, i32 -1 seq_cst
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i41

bb258:                                            ; preds = %bb253
  %tmp259 = load i32* %tmp255, align 4, !tbaa !7
  %tmp260 = add nsw i32 %tmp259, -1
  store i32 %tmp260, i32* %tmp255, align 4, !tbaa !7
  br label %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i41

_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i41: ; preds = %bb258, %bb256
  %.0.i.i.i.i40 = phi i32 [ %tmp257, %bb256 ], [ %tmp259, %bb258 ]
  %tmp261 = icmp slt i32 %.0.i.i.i.i40, 1
  br i1 %tmp261, label %bb262, label %_ZNSsD1Ev.exit42

bb262:                                            ; preds = %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i41
  %tmp263 = bitcast i8* %tmp251 to %"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*
  call void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"* %tmp263, %"class.std::allocator.40"* %tmp) nounwind
  br label %_ZNSsD1Ev.exit42

_ZNSsD1Ev.exit42:                                 ; preds = %bb262, %_ZN9__gnu_cxxL27__exchange_and_add_dispatchEPii.exit.i.i.i41, %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EED1Ev.exit39
  call void @llvm.lifetime.end(i64 -1, i8* %tmp248)
  ret void
}

declare void @__cxa_bad_typeid()

define void @_Z18traverse_hierarchyv() uwtable {
bb:
  %tmp = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  %tmp1 = icmp eq %"class.sc_core::sc_simcontext"* %tmp, null
  br i1 %tmp1, label %bb2, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

bb2:                                              ; preds = %bb
  %tmp3 = tail call noalias i8* @_Znwm(i64 248)
  %tmp4 = bitcast i8* %tmp3 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp4)
  store %"class.sc_core::sc_simcontext"* %tmp4, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !4
  store %"class.sc_core::sc_simcontext"* %tmp4, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %bb2, %bb
  %tmp5 = phi %"class.sc_core::sc_simcontext"* [ %tmp4, %bb2 ], [ %tmp, %bb ]
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp5, i64 0, i32 11, i32 0, i32 0, i32 1
  %tmp7 = load %"class.sc_core::sc_object"*** %tmp6, align 8, !tbaa !4
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_simcontext"* %tmp5, i64 0, i32 11, i32 0, i32 0, i32 0
  %tmp9 = load %"class.sc_core::sc_object"*** %tmp8, align 8, !tbaa !4
  %tmp10 = ptrtoint %"class.sc_core::sc_object"** %tmp7 to i64
  %tmp11 = ptrtoint %"class.sc_core::sc_object"** %tmp9 to i64
  %tmp12 = sub i64 %tmp10, %tmp11
  %tmp13 = ashr exact i64 %tmp12, 3
  %tmp14 = icmp eq i64 %tmp13, 0
  br i1 %tmp14, label %bb19, label %bb15

bb15:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp16 = icmp ugt i64 %tmp13, 2305843009213693951
  br i1 %tmp16, label %.noexc.i.i.i, label %_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i

.noexc.i.i.i:                                     ; preds = %bb15
  tail call void @_ZSt17__throw_bad_allocv() noreturn
  unreachable

_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i: ; preds = %bb15
  %tmp17 = tail call noalias i8* @_Znwm(i64 %tmp12)
  %tmp18 = bitcast i8* %tmp17 to %"class.sc_core::sc_object"**
  br label %bb19

bb19:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp20 = phi %"class.sc_core::sc_object"** [ %tmp18, %_ZN9__gnu_cxx13new_allocatorIPN7sc_core9sc_objectEE8allocateEmPKv.exit.i.i.i.i ], [ null, %_ZN7sc_core22sc_get_curr_simcontextEv.exit ]
  %tmp21 = icmp eq %"class.sc_core::sc_object"** %tmp7, %tmp9
  br i1 %tmp21, label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader, label %bb22

bb22:                                             ; preds = %bb19
  %tmp23 = bitcast %"class.sc_core::sc_object"** %tmp20 to i8*
  %tmp24 = bitcast %"class.sc_core::sc_object"** %tmp9 to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp23, i8* %tmp24, i64 %tmp12, i32 8, i1 false) nounwind
  br label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader

_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader: ; preds = %bb22, %bb19
  br label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit

_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit: ; preds = %bb27, %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader
  %i.0 = phi i32 [ %tmp30, %bb27 ], [ 0, %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit.preheader ]
  %tmp25 = zext i32 %i.0 to i64
  %tmp26 = icmp ult i64 %tmp25, %tmp13
  br i1 %tmp26, label %bb27, label %bb31

bb27:                                             ; preds = %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit
  %tmp28 = getelementptr inbounds %"class.sc_core::sc_object"** %tmp20, i64 %tmp25
  %tmp29 = load %"class.sc_core::sc_object"** %tmp28, align 8, !tbaa !4
  tail call void @_Z17recursive_descentPN7sc_core9sc_objectE(%"class.sc_core::sc_object"* %tmp29)
  %tmp30 = add i32 %i.0, 1
  br label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit

bb31:                                             ; preds = %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EEC1ERKS4_.exit
  %tmp32 = icmp eq %"class.sc_core::sc_object"** %tmp20, null
  br i1 %tmp32, label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EED1Ev.exit1, label %bb33

bb33:                                             ; preds = %bb31
  %tmp34 = bitcast %"class.sc_core::sc_object"** %tmp20 to i8*
  tail call void @_ZdlPv(i8* %tmp34) nounwind
  br label %_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EED1Ev.exit1

_ZNSt6vectorIPN7sc_core9sc_objectESaIS2_EED1Ev.exit1: ; preds = %bb33, %bb31
  ret void
}

define i32 @sc_main(i32 %argc, i8** nocapture %argv) uwtable {
_ZN3topC1EN7sc_core14sc_module_nameE.exit:
  %top1 = alloca %class.top, align 8
  %tmp = alloca %"class.sc_core::sc_module_name", align 8
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp, i8* getelementptr inbounds ([5 x i8]* @.str6, i64 0, i64 0))
  call void @_ZN3topC2EN7sc_core14sc_module_nameE(%class.top* %top1, %"class.sc_core::sc_module_name"* %tmp)
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp)
  call void @_Z18traverse_hierarchyv()
  call void @_ZN7sc_core8sc_startEv()
  %tmp1 = getelementptr inbounds %class.top* %top1, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp1)
  ret i32 0
}

declare void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"*, i8*)

declare void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core8sc_startEv()

define linkonce_odr void @_ZN3topD1Ev(%class.top* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %class.top* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp)
  ret void
}

define linkonce_odr noalias %"class.std::vector.10"* @_ZNK7sc_core9sc_object17get_child_objectsEv(%"class.sc_core::sc_object"* nocapture %this) uwtable align 2 {
bb:
  %tmp = tail call noalias i8* @_Znwm(i64 24)
  %tmp1 = bitcast i8* %tmp to %"class.std::vector.10"*
  tail call void @llvm.memset.p0i8.i64(i8* %tmp, i8 0, i64 24, i32 8, i1 false) nounwind
  ret %"class.std::vector.10"* %tmp1
}

declare void @_ZN7sc_core12sc_interface13register_portERNS_12sc_port_baseEPKc(%"class.sc_core::sc_interface"*, %"class.sc_core::sc_port_base"*, i8*)

declare %"class.sc_core::sc_event"* @_ZNK7sc_core12sc_interface13default_eventEv(%"class.sc_core::sc_interface"*)

declare void @__cxa_pure_virtual()

declare void @_ZdlPv(i8*) nounwind

declare void @llvm.memmove.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i32, i1) nounwind

declare void @_ZSt17__throw_bad_allocv() noreturn

declare noalias i8* @_Znwm(i64)

declare %"class.std::basic_string"* @_ZNSs6appendERKSs(%"class.std::basic_string"*, %"class.std::basic_string"*)

declare void @_ZNSs7reserveEm(%"class.std::basic_string"*, i64)

declare %"class.std::basic_string"* @_ZNSs6appendEPKcm(%"class.std::basic_string"*, i8*, i64)

declare i64 @strlen(i8* nocapture) nounwind readonly

declare %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"*, i8 signext)

declare void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"*)

declare void @_ZSt16__throw_bad_castv() noreturn

declare %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"*)

declare %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"*, i8*, i64)

declare void @_ZNSt9basic_iosIcSt11char_traitsIcEE5clearESt12_Ios_Iostate(%"class.std::basic_ios"*, i32)

declare void @_ZNSs4_Rep10_M_destroyERKSaIcE(%"struct.std::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Rep"*, %"class.std::allocator.40"*) nounwind

declare extern_weak i32 @pthread_cancel(i64)

declare void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"*)

define linkonce_odr void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  tail call void @_ZN7sc_core8sc_event6cancelEv(%"class.sc_core::sc_event"* %this)
  %tmp = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 7, i32 0, i32 0, i32 0
  %tmp1 = load %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !4
  %tmp2 = icmp eq %"class.sc_core::sc_thread_process"** %tmp1, null
  br i1 %tmp2, label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %"class.sc_core::sc_thread_process"** %tmp1 to i8*
  tail call void @_ZdlPv(i8* %tmp4) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit

_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit: ; preds = %bb3, %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 6, i32 0, i32 0, i32 0
  %tmp6 = load %"class.sc_core::sc_thread_process"*** %tmp5, align 8, !tbaa !4
  %tmp7 = icmp eq %"class.sc_core::sc_thread_process"** %tmp6, null
  br i1 %tmp7, label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4, label %bb8

bb8:                                              ; preds = %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit
  %tmp9 = bitcast %"class.sc_core::sc_thread_process"** %tmp6 to i8*
  tail call void @_ZdlPv(i8* %tmp9) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4

_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4: ; preds = %bb8, %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit
  %tmp10 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 5, i32 0, i32 0, i32 0
  %tmp11 = load %"class.sc_core::sc_method_process"*** %tmp10, align 8, !tbaa !4
  %tmp12 = icmp eq %"class.sc_core::sc_method_process"** %tmp11, null
  br i1 %tmp12, label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit, label %bb13

bb13:                                             ; preds = %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4
  %tmp14 = bitcast %"class.sc_core::sc_method_process"** %tmp11 to i8*
  tail call void @_ZdlPv(i8* %tmp14) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit

_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit: ; preds = %bb13, %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4
  %tmp15 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 4, i32 0, i32 0, i32 0
  %tmp16 = load %"class.sc_core::sc_method_process"*** %tmp15, align 8, !tbaa !4
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

define linkonce_odr void @_ZThn40_N3topD1Ev(%class.top* %this) {
bb:
  %tmp = getelementptr inbounds %class.top* %this, i64 -1, i32 0, i32 9, i32 0, i32 0, i32 2
  %tmp1 = bitcast %"class.sc_core::sc_object"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp1)
  ret void
}

declare void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN3topC2EN7sc_core14sc_module_nameE(%class.top* %this, %"class.sc_core::sc_module_name"* %name) unnamed_addr uwtable align 2 {
_ZN8producerC1EN7sc_core14sc_module_nameE.exit:
  %tmp = alloca %"class.sc_core::sc_module_name", align 8
  %tmp1 = alloca %"class.sc_core::sc_module_name", align 8
  %tmp2 = alloca %"class.sc_core::sc_module_name", align 8
  %tmp3 = getelementptr inbounds %class.top* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp3, %"class.sc_core::sc_module_name"* %name)
  %tmp4 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !0
  %tmp5 = getelementptr %class.top* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp5, align 8, !tbaa !0
  %tmp6 = call noalias i8* @_Znwm(i64 464)
  %tmp7 = bitcast i8* %tmp6 to %class.fifo*
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp, i8* getelementptr inbounds ([6 x i8]* @.str13, i64 0, i64 0))
  call void @_ZN4fifoC1EN7sc_core14sc_module_nameE(%class.fifo* %tmp7, %"class.sc_core::sc_module_name"* %tmp)
  %tmp8 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  store %class.fifo* %tmp7, %class.fifo** %tmp8, align 8, !tbaa !4
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp)
  %tmp9 = call noalias i8* @_Znwm(i64 264)
  %tmp10 = bitcast i8* %tmp9 to %class.producer*
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp1, i8* getelementptr inbounds ([10 x i8]* @.str14, i64 0, i64 0))
  call void @_ZN8producerC2EN7sc_core14sc_module_nameE(%class.producer* %tmp10, %"class.sc_core::sc_module_name"* %tmp1)
  %tmp11 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  store %class.producer* %tmp10, %class.producer** %tmp11, align 8, !tbaa !4
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp1)
  %tmp12 = load %class.producer** %tmp11, align 8, !tbaa !4
  %tmp13 = load %class.fifo** %tmp8, align 8, !tbaa !4
  %tmp14 = getelementptr %class.fifo* %tmp13, i64 0, i32 1
  %tmp15 = getelementptr inbounds %class.producer* %tmp12, i64 0, i32 1, i32 0, i32 0
  %tmp16 = bitcast %class.write_if* %tmp14 to i8**
  %tmp17 = load i8** %tmp16, align 8, !tbaa !0
  %tmp18 = getelementptr i8* %tmp17, i64 -48
  %tmp19 = bitcast i8* %tmp18 to i64*
  %tmp20 = load i64* %tmp19, align 8
  %tmp21 = bitcast %class.write_if* %tmp14 to i8*
  %tmp22 = getelementptr i8* %tmp21, i64 %tmp20
  %tmp23 = bitcast i8* %tmp22 to %"class.sc_core::sc_interface"*
  call void @_ZN7sc_core12sc_port_base4bindERNS_12sc_interfaceE(%"class.sc_core::sc_port_base"* %tmp15, %"class.sc_core::sc_interface"* %tmp23)
  %tmp24 = call noalias i8* @_Znwm(i64 264)
  %tmp25 = bitcast i8* %tmp24 to %class.consumer*
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp2, i8* getelementptr inbounds ([10 x i8]* @.str15, i64 0, i64 0))
  call void @_ZN8consumerC2EN7sc_core14sc_module_nameE(%class.consumer* %tmp25, %"class.sc_core::sc_module_name"* %tmp2)
  %tmp26 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  store %class.consumer* %tmp25, %class.consumer** %tmp26, align 8, !tbaa !4
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp2)
  %tmp27 = load %class.consumer** %tmp26, align 8, !tbaa !4
  %tmp28 = load %class.fifo** %tmp8, align 8, !tbaa !4
  %tmp29 = getelementptr %class.fifo* %tmp28, i64 0, i32 2
  %tmp30 = getelementptr inbounds %class.consumer* %tmp27, i64 0, i32 1, i32 0, i32 0
  %tmp31 = bitcast %class.read_if* %tmp29 to i8**
  %tmp32 = load i8** %tmp31, align 8, !tbaa !0
  %tmp33 = getelementptr i8* %tmp32, i64 -48
  %tmp34 = bitcast i8* %tmp33 to i64*
  %tmp35 = load i64* %tmp34, align 8
  %tmp36 = bitcast %class.read_if* %tmp29 to i8*
  %tmp37 = getelementptr i8* %tmp36, i64 %tmp35
  %tmp38 = bitcast i8* %tmp37 to %"class.sc_core::sc_interface"*
  call void @_ZN7sc_core12sc_port_base4bindERNS_12sc_interfaceE(%"class.sc_core::sc_port_base"* %tmp30, %"class.sc_core::sc_interface"* %tmp38)
  ret void
}

declare void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*)

define linkonce_odr void @_ZN4fifoC1EN7sc_core14sc_module_nameE(%class.fifo* %this, %"class.sc_core::sc_module_name"* %name) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 0, i32 1
  %tmp1 = getelementptr inbounds %class.write_if* %tmp, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceC2Ev(%"class.sc_core::sc_interface"* %tmp1)
  %tmp2 = getelementptr inbounds %class.fifo* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp2, %"class.sc_core::sc_module_name"* %name)
  %tmp3 = getelementptr inbounds %class.write_if* %tmp, i64 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([12 x i8*]* @_ZTC4fifo184_8write_if, i64 0, i64 6) to i32 (...)**), i32 (...)*** %tmp3, align 8, !tbaa !0
  %tmp4 = load i64* bitcast ([12 x i8*]* @_ZTC4fifo184_8write_if to i64*), align 8
  %tmp5 = bitcast %class.write_if* %tmp to i8*
  %tmp6 = getelementptr i8* %tmp5, i64 %tmp4
  %tmp7 = bitcast i8* %tmp6 to i8**
  store i8* bitcast (i8** getelementptr inbounds ([12 x i8*]* @_ZTC4fifo184_8write_if, i64 0, i64 6) to i8*), i8** %tmp7, align 8, !tbaa !0
  %tmp8 = getelementptr inbounds %class.fifo* %this, i64 0, i32 2
  %tmp9 = getelementptr inbounds %class.read_if* %tmp8, i64 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([21 x i8*]* @_ZTC4fifo192_7read_if, i64 0, i64 6) to i32 (...)**), i32 (...)*** %tmp9, align 8, !tbaa !0
  %tmp10 = getelementptr inbounds %class.read_if* %tmp8, i64 -1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([21 x i8*]* @_ZTC4fifo192_7read_if, i64 0, i64 17) to i32 (...)**), i32 (...)*** %tmp10, align 8, !tbaa !0
  %tmp11 = getelementptr inbounds %class.fifo* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to i32 (...)**), i32 (...)*** %tmp11, align 8, !tbaa !0
  %tmp12 = getelementptr %class.fifo* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to i32 (...)**), i32 (...)*** %tmp12, align 8, !tbaa !0
  %tmp13 = getelementptr %class.fifo* %this, i64 0, i32 1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to i32 (...)**), i32 (...)*** %tmp13, align 8, !tbaa !0
  %tmp14 = getelementptr %class.fifo* %this, i64 0, i32 2, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to i32 (...)**), i32 (...)*** %tmp14, align 8, !tbaa !0
  %tmp15 = getelementptr inbounds %class.fifo* %this, i64 0, i32 4
  store i32 0, i32* %tmp15, align 4, !tbaa !7
  %tmp16 = getelementptr inbounds %class.fifo* %this, i64 0, i32 5
  store i32 0, i32* %tmp16, align 4, !tbaa !7
  %tmp17 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  %tmp18 = icmp eq %"class.sc_core::sc_simcontext"* %tmp17, null
  br i1 %tmp18, label %.noexc3, label %bb21

.noexc3:                                          ; preds = %bb
  %tmp19 = tail call noalias i8* @_Znwm(i64 248)
  %tmp20 = bitcast i8* %tmp19 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp20)
  store %"class.sc_core::sc_simcontext"* %tmp20, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !4
  store %"class.sc_core::sc_simcontext"* %tmp20, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  br label %bb21

bb21:                                             ; preds = %.noexc3, %bb
  %tmp22 = phi %"class.sc_core::sc_simcontext"* [ %tmp20, %.noexc3 ], [ %tmp17, %bb ]
  %tmp23 = getelementptr inbounds %class.fifo* %this, i64 0, i32 6, i32 0
  store %"class.sc_core::sc_simcontext"* %tmp22, %"class.sc_core::sc_simcontext"** %tmp23, align 8, !tbaa !4
  %tmp24 = getelementptr inbounds %class.fifo* %this, i64 0, i32 6, i32 1
  store i32 0, i32* %tmp24, align 4, !tbaa !8
  %tmp25 = getelementptr inbounds %class.fifo* %this, i64 0, i32 6, i32 2
  store i32 -1, i32* %tmp25, align 4, !tbaa !7
  %tmp26 = getelementptr inbounds %class.fifo* %this, i64 0, i32 6, i32 3
  %tmp27 = bitcast %"class.sc_core::sc_event_timed"** %tmp26 to i8*
  tail call void @llvm.memset.p0i8.i64(i8* %tmp27, i8 0, i64 104, i32 8, i1 false)
  %tmp28 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  %tmp29 = icmp eq %"class.sc_core::sc_simcontext"* %tmp28, null
  br i1 %tmp29, label %.noexc, label %bb32

.noexc:                                           ; preds = %bb21
  %tmp30 = tail call noalias i8* @_Znwm(i64 248)
  %tmp31 = bitcast i8* %tmp30 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp31)
  store %"class.sc_core::sc_simcontext"* %tmp31, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !4
  store %"class.sc_core::sc_simcontext"* %tmp31, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  br label %bb32

bb32:                                             ; preds = %.noexc, %bb21
  %tmp33 = phi %"class.sc_core::sc_simcontext"* [ %tmp31, %.noexc ], [ %tmp28, %bb21 ]
  %tmp34 = getelementptr inbounds %class.fifo* %this, i64 0, i32 7, i32 0
  store %"class.sc_core::sc_simcontext"* %tmp33, %"class.sc_core::sc_simcontext"** %tmp34, align 8, !tbaa !4
  %tmp35 = getelementptr inbounds %class.fifo* %this, i64 0, i32 7, i32 1
  store i32 0, i32* %tmp35, align 4, !tbaa !8
  %tmp36 = getelementptr inbounds %class.fifo* %this, i64 0, i32 7, i32 2
  store i32 -1, i32* %tmp36, align 4, !tbaa !7
  %tmp37 = getelementptr inbounds %class.fifo* %this, i64 0, i32 7, i32 3
  %tmp38 = bitcast %"class.sc_core::sc_event_timed"** %tmp37 to i8*
  tail call void @llvm.memset.p0i8.i64(i8* %tmp38, i8 0, i64 104, i32 8, i1 false)
  ret void
}

declare void @_ZNK7sc_core9sc_object5printERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object4dumpERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)

define linkonce_odr i8* @_ZNK7sc_core9sc_module4kindEv(%"class.sc_core::sc_module"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str16, i64 0, i64 0)
}

declare %"class.std::vector.10"* @_ZNK7sc_core9sc_module17get_child_objectsEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN3topD0Ev(%class.top* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN3topD1Ev.exit:
  %tmp = getelementptr inbounds %class.top* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp)
  %tmp1 = bitcast %class.top* %this to i8*
  tail call void @_ZdlPv(i8* %tmp1) nounwind
  ret void
}

declare void @_ZN7sc_core9sc_module25before_end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module18end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module19start_of_simulationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module17end_of_simulationEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZThn40_N3topD0Ev(%class.top* %this) {
_ZN3topD0Ev.exit:
  %tmp = getelementptr inbounds %class.top* %this, i64 -1, i32 0, i32 9, i32 0, i32 0, i32 2
  %tmp1 = bitcast %"class.sc_core::sc_object"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp1)
  %tmp2 = bitcast %"class.sc_core::sc_object"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp2) nounwind
  ret void
}

declare void @_ZN7sc_core12sc_port_base4bindERNS_12sc_interfaceE(%"class.sc_core::sc_port_base"*, %"class.sc_core::sc_interface"*)

define linkonce_odr void @_ZN8consumerC2EN7sc_core14sc_module_nameE(%class.consumer* %this, %"class.sc_core::sc_module_name"* %name) unnamed_addr uwtable align 2 {
bb:
  %main_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = getelementptr inbounds %class.consumer* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp3, %"class.sc_core::sc_module_name"* %name)
  %tmp4 = getelementptr inbounds %class.consumer* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !0
  %tmp5 = getelementptr %class.consumer* %this, i64 0, i32 0, i32 1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp5, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp6, align 8, !tbaa !0
  %tmp7 = getelementptr inbounds %class.consumer* %this, i64 0, i32 1
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_port.59"* %tmp7, i64 0, i32 0, i32 0
  call void @_ZN7sc_core12sc_port_baseC2EiNS_14sc_port_policyE(%"class.sc_core::sc_port_base"* %tmp8, i32 1, i32 0)
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port.59"* %tmp7, i64 0, i32 0, i32 0, i32 0, i32 0
  %tmp10 = getelementptr inbounds %class.consumer* %this, i64 0, i32 1, i32 0, i32 1
  %tmp11 = bitcast %class.read_if** %tmp10 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp11, i8 0, i64 32, i32 8, i1 false)
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp9, align 8, !tbaa !0
  %tmp12 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  %tmp13 = icmp eq %"class.sc_core::sc_simcontext"* %tmp12, null
  br i1 %tmp13, label %.noexc, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc:                                           ; preds = %bb
  %tmp14 = call noalias i8* @_Znwm(i64 248)
  %tmp15 = bitcast i8* %tmp14 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp15)
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !4
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc, %bb
  %tmp16 = phi %"class.sc_core::sc_simcontext"* [ %tmp15, %.noexc ], [ %tmp12, %bb ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %main_handle, %"class.sc_core::sc_simcontext"* %tmp16, i8* getelementptr inbounds ([5 x i8]* @.str17, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.consumer*)* @_ZN8consumer4mainEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp5, %"class.sc_core::sc_spawn_options"* null)
  %tmp17 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %main_handle, i64 0, i32 0
  %tmp18 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !4
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp18, %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !4
  %tmp20 = icmp eq %"class.sc_core::sc_process_b"* %tmp18, null
  br i1 %tmp20, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb21

bb21:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp22 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp18, i64 0, i32 15
  %tmp23 = load i32* %tmp22, align 4, !tbaa !7
  %tmp24 = icmp eq i32 %tmp23, 0
  br i1 %tmp24, label %bb25, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb25:                                             ; preds = %bb21
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str25, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb21
  %tmp26 = add nsw i32 %tmp23, 1
  store i32 %tmp26, i32* %tmp22, align 4, !tbaa !7
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp27 = getelementptr inbounds %class.consumer* %this, i64 0, i32 0, i32 2
  %tmp28 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp27, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp29 = load %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !4
  %tmp30 = icmp eq %"class.sc_core::sc_process_b"* %tmp29, null
  br i1 %tmp30, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb31

bb31:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp32 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp29, i64 0, i32 15
  %tmp33 = load i32* %tmp32, align 4, !tbaa !7
  %tmp34 = add nsw i32 %tmp33, -1
  store i32 %tmp34, i32* %tmp32, align 4, !tbaa !7
  %tmp35 = icmp eq i32 %tmp34, 0
  br i1 %tmp35, label %bb36, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb36:                                             ; preds = %bb31
  %tmp37 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp38 = icmp eq %"class.sc_core::sc_process_b"* %tmp37, null
  br i1 %tmp38, label %bb43, label %.noexc6

.noexc6:                                          ; preds = %bb36
  %tmp39 = bitcast %"class.sc_core::sc_process_b"* %tmp37 to void (%"class.sc_core::sc_process_b"*)***
  %tmp40 = load void (%"class.sc_core::sc_process_b"*)*** %tmp39, align 8, !tbaa !0
  %tmp41 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp40, i64 6
  %tmp42 = load void (%"class.sc_core::sc_process_b"*)** %tmp41, align 8
  call void %tmp42(%"class.sc_core::sc_process_b"* %tmp37)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb43

bb43:                                             ; preds = %.noexc6, %bb36
  %tmp44 = phi %"class.sc_core::sc_process_b"* [ null, %bb36 ], [ %.pre.i.i.i, %.noexc6 ]
  %tmp45 = icmp eq %"class.sc_core::sc_process_b"* %tmp44, %tmp29
  br i1 %tmp45, label %bb46, label %bb47

bb46:                                             ; preds = %bb43
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str23, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb47:                                             ; preds = %bb43
  store %"class.sc_core::sc_process_b"* %tmp29, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb47, %bb31, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp48 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !4
  %tmp49 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp48, %"class.sc_core::sc_process_b"** %tmp49, align 8, !tbaa !4
  %tmp50 = icmp eq %"class.sc_core::sc_process_b"* %tmp48, null
  br i1 %tmp50, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8, label %bb51

bb51:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp52 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp48, i64 0, i32 15
  %tmp53 = load i32* %tmp52, align 4, !tbaa !7
  %tmp54 = icmp eq i32 %tmp53, 0
  br i1 %tmp54, label %bb55, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7

bb55:                                             ; preds = %bb51
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str25, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7: ; preds = %bb51
  %tmp56 = add nsw i32 %tmp53, 1
  store i32 %tmp56, i32* %tmp52, align 4, !tbaa !7
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8

_ZN7sc_core17sc_process_handleC1ERKS0_.exit8:     ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp57 = getelementptr inbounds %class.consumer* %this, i64 0, i32 0, i32 3
  %tmp58 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp57, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp59 = load %"class.sc_core::sc_process_b"** %tmp49, align 8, !tbaa !4
  %tmp60 = icmp eq %"class.sc_core::sc_process_b"* %tmp59, null
  br i1 %tmp60, label %_ZN7sc_core17sc_process_handleD1Ev.exit11, label %bb61

bb61:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8
  %tmp62 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp59, i64 0, i32 15
  %tmp63 = load i32* %tmp62, align 4, !tbaa !7
  %tmp64 = add nsw i32 %tmp63, -1
  store i32 %tmp64, i32* %tmp62, align 4, !tbaa !7
  %tmp65 = icmp eq i32 %tmp64, 0
  br i1 %tmp65, label %bb66, label %_ZN7sc_core17sc_process_handleD1Ev.exit11

bb66:                                             ; preds = %bb61
  %tmp67 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp68 = icmp eq %"class.sc_core::sc_process_b"* %tmp67, null
  br i1 %tmp68, label %bb73, label %.noexc10

.noexc10:                                         ; preds = %bb66
  %tmp69 = bitcast %"class.sc_core::sc_process_b"* %tmp67 to void (%"class.sc_core::sc_process_b"*)***
  %tmp70 = load void (%"class.sc_core::sc_process_b"*)*** %tmp69, align 8, !tbaa !0
  %tmp71 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp70, i64 6
  %tmp72 = load void (%"class.sc_core::sc_process_b"*)** %tmp71, align 8
  call void %tmp72(%"class.sc_core::sc_process_b"* %tmp67)
  %.pre.i.i.i9 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb73

bb73:                                             ; preds = %.noexc10, %bb66
  %tmp74 = phi %"class.sc_core::sc_process_b"* [ null, %bb66 ], [ %.pre.i.i.i9, %.noexc10 ]
  %tmp75 = icmp eq %"class.sc_core::sc_process_b"* %tmp74, %tmp59
  br i1 %tmp75, label %bb76, label %bb77

bb76:                                             ; preds = %bb73
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str23, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb77:                                             ; preds = %bb73
  store %"class.sc_core::sc_process_b"* %tmp59, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit11

_ZN7sc_core17sc_process_handleD1Ev.exit11:        ; preds = %bb77, %bb61, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8
  %tmp78 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !4
  %tmp79 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp78, %"class.sc_core::sc_process_b"** %tmp79, align 8, !tbaa !4
  %tmp80 = icmp eq %"class.sc_core::sc_process_b"* %tmp78, null
  br i1 %tmp80, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13, label %bb81

bb81:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit11
  %tmp82 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp78, i64 0, i32 15
  %tmp83 = load i32* %tmp82, align 4, !tbaa !7
  %tmp84 = icmp eq i32 %tmp83, 0
  br i1 %tmp84, label %bb85, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12

bb85:                                             ; preds = %bb81
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str25, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12: ; preds = %bb81
  %tmp86 = add nsw i32 %tmp83, 1
  store i32 %tmp86, i32* %tmp82, align 4, !tbaa !7
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13

_ZN7sc_core17sc_process_handleC1ERKS0_.exit13:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12, %_ZN7sc_core17sc_process_handleD1Ev.exit11
  %tmp87 = getelementptr inbounds %class.consumer* %this, i64 0, i32 0, i32 4
  %tmp88 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp87, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp89 = load %"class.sc_core::sc_process_b"** %tmp79, align 8, !tbaa !4
  %tmp90 = icmp eq %"class.sc_core::sc_process_b"* %tmp89, null
  br i1 %tmp90, label %_ZN7sc_core17sc_process_handleD1Ev.exit16, label %bb91

bb91:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp92 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp89, i64 0, i32 15
  %tmp93 = load i32* %tmp92, align 4, !tbaa !7
  %tmp94 = add nsw i32 %tmp93, -1
  store i32 %tmp94, i32* %tmp92, align 4, !tbaa !7
  %tmp95 = icmp eq i32 %tmp94, 0
  br i1 %tmp95, label %bb96, label %_ZN7sc_core17sc_process_handleD1Ev.exit16

bb96:                                             ; preds = %bb91
  %tmp97 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp98 = icmp eq %"class.sc_core::sc_process_b"* %tmp97, null
  br i1 %tmp98, label %bb103, label %.noexc15

.noexc15:                                         ; preds = %bb96
  %tmp99 = bitcast %"class.sc_core::sc_process_b"* %tmp97 to void (%"class.sc_core::sc_process_b"*)***
  %tmp100 = load void (%"class.sc_core::sc_process_b"*)*** %tmp99, align 8, !tbaa !0
  %tmp101 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp100, i64 6
  %tmp102 = load void (%"class.sc_core::sc_process_b"*)** %tmp101, align 8
  call void %tmp102(%"class.sc_core::sc_process_b"* %tmp97)
  %.pre.i.i.i14 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb103

bb103:                                            ; preds = %.noexc15, %bb96
  %tmp104 = phi %"class.sc_core::sc_process_b"* [ null, %bb96 ], [ %.pre.i.i.i14, %.noexc15 ]
  %tmp105 = icmp eq %"class.sc_core::sc_process_b"* %tmp104, %tmp89
  br i1 %tmp105, label %bb106, label %bb107

bb106:                                            ; preds = %bb103
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str23, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb107:                                            ; preds = %bb103
  store %"class.sc_core::sc_process_b"* %tmp89, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit16

_ZN7sc_core17sc_process_handleD1Ev.exit16:        ; preds = %bb107, %bb91, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp108 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !4
  %tmp109 = icmp eq %"class.sc_core::sc_process_b"* %tmp108, null
  br i1 %tmp109, label %_ZN7sc_core17sc_process_handleD1Ev.exit19, label %bb110

bb110:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit16
  %tmp111 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp108, i64 0, i32 15
  %tmp112 = load i32* %tmp111, align 4, !tbaa !7
  %tmp113 = add nsw i32 %tmp112, -1
  store i32 %tmp113, i32* %tmp111, align 4, !tbaa !7
  %tmp114 = icmp eq i32 %tmp113, 0
  br i1 %tmp114, label %bb115, label %_ZN7sc_core17sc_process_handleD1Ev.exit19

bb115:                                            ; preds = %bb110
  %tmp116 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp117 = icmp eq %"class.sc_core::sc_process_b"* %tmp116, null
  br i1 %tmp117, label %bb122, label %.noexc18

.noexc18:                                         ; preds = %bb115
  %tmp118 = bitcast %"class.sc_core::sc_process_b"* %tmp116 to void (%"class.sc_core::sc_process_b"*)***
  %tmp119 = load void (%"class.sc_core::sc_process_b"*)*** %tmp118, align 8, !tbaa !0
  %tmp120 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp119, i64 6
  %tmp121 = load void (%"class.sc_core::sc_process_b"*)** %tmp120, align 8
  call void %tmp121(%"class.sc_core::sc_process_b"* %tmp116)
  %.pre.i.i.i17 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb122

bb122:                                            ; preds = %.noexc18, %bb115
  %tmp123 = phi %"class.sc_core::sc_process_b"* [ null, %bb115 ], [ %.pre.i.i.i17, %.noexc18 ]
  %tmp124 = icmp eq %"class.sc_core::sc_process_b"* %tmp123, %tmp108
  br i1 %tmp124, label %bb125, label %bb126

bb125:                                            ; preds = %bb122
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str23, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb126:                                            ; preds = %bb122
  store %"class.sc_core::sc_process_b"* %tmp108, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit19

_ZN7sc_core17sc_process_handleD1Ev.exit19:        ; preds = %bb126, %bb110, %_ZN7sc_core17sc_process_handleD1Ev.exit16
  ret void
}

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

define linkonce_odr void @_ZN8consumer4mainEv(%class.consumer* %this) noreturn uwtable align 2 {
bb:
  %tmp = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8**), align 8, !tbaa !0
  %tmp1 = getelementptr i8* %tmp, i64 -24
  %tmp2 = bitcast i8* %tmp1 to i64*
  %tmp3 = load i64* %tmp2, align 8
  %.sum.i = add i64 %tmp3, 240
  %tmp4 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %.sum.i
  %tmp5 = bitcast i8* %tmp4 to %"class.std::ctype"**
  %tmp6 = load %"class.std::ctype"** %tmp5, align 8, !tbaa !4
  %tmp7 = icmp eq %"class.std::ctype"* %tmp6, null
  br i1 %tmp7, label %bb8, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb8:                                              ; preds = %bb
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %bb
  %tmp9 = alloca i8, align 1
  %c = alloca i8, align 1
  %tmp10 = getelementptr inbounds %"class.std::ctype"* %tmp6, i64 0, i32 6
  %tmp11 = load i8* %tmp10, align 1, !tbaa !3
  %tmp12 = icmp eq i8 %tmp11, 0
  br i1 %tmp12, label %bb16, label %bb13

bb13:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp14 = getelementptr inbounds %"class.std::ctype"* %tmp6, i64 0, i32 7, i64 10
  %tmp15 = load i8* %tmp14, align 1, !tbaa !3
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb16:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp6)
  %tmp17 = bitcast %"class.std::ctype"* %tmp6 to i8 (%"class.std::ctype"*, i8)***
  %tmp18 = load i8 (%"class.std::ctype"*, i8)*** %tmp17, align 8, !tbaa !0
  %tmp19 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp18, i64 6
  %tmp20 = load i8 (%"class.std::ctype"*, i8)** %tmp19, align 8
  %tmp21 = call signext i8 %tmp20(%"class.std::ctype"* %tmp6, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb16, %bb13
  %.0.i.i.i = phi i8 [ %tmp15, %bb13 ], [ %tmp21, %bb16 ]
  %tmp22 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* @_ZSt4cout, i8 signext %.0.i.i.i)
  %tmp23 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp22)
  %tmp24 = bitcast %"class.std::basic_ostream"* %tmp23 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -24
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %"class.std::basic_ostream"* %tmp23 to i8*
  %.sum.i1 = add i64 %tmp28, 240
  %tmp30 = getelementptr inbounds i8* %tmp29, i64 %.sum.i1
  %tmp31 = bitcast i8* %tmp30 to %"class.std::ctype"**
  %tmp32 = load %"class.std::ctype"** %tmp31, align 8, !tbaa !4
  %tmp33 = icmp eq %"class.std::ctype"* %tmp32, null
  br i1 %tmp33, label %bb34, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2

bb34:                                             ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2: ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit
  %tmp35 = getelementptr inbounds %"class.std::ctype"* %tmp32, i64 0, i32 6
  %tmp36 = load i8* %tmp35, align 1, !tbaa !3
  %tmp37 = icmp eq i8 %tmp36, 0
  br i1 %tmp37, label %bb41, label %bb38

bb38:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  %tmp39 = getelementptr inbounds %"class.std::ctype"* %tmp32, i64 0, i32 7, i64 10
  %tmp40 = load i8* %tmp39, align 1, !tbaa !3
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

bb41:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp32)
  %tmp42 = bitcast %"class.std::ctype"* %tmp32 to i8 (%"class.std::ctype"*, i8)***
  %tmp43 = load i8 (%"class.std::ctype"*, i8)*** %tmp42, align 8, !tbaa !0
  %tmp44 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp43, i64 6
  %tmp45 = load i8 (%"class.std::ctype"*, i8)** %tmp44, align 8
  %tmp46 = call signext i8 %tmp45(%"class.std::ctype"* %tmp32, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4: ; preds = %bb41, %bb38
  %.0.i.i.i3 = phi i8 [ %tmp40, %bb38 ], [ %tmp46, %bb41 ]
  %tmp47 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp23, i8 signext %.0.i.i.i3)
  %tmp48 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp47)
  %tmp49 = getelementptr inbounds %class.consumer* %this, i64 0, i32 1, i32 0, i32 1
  %tmp50 = getelementptr inbounds %class.consumer* %this, i64 0, i32 1, i32 0, i32 0
  br label %.backedge

.backedge:                                        ; preds = %.backedge.backedge, %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4
  %tmp51 = load %class.read_if** %tmp49, align 8, !tbaa !4
  %tmp52 = icmp eq %class.read_if* %tmp51, null
  br i1 %tmp52, label %bb53, label %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit

bb53:                                             ; preds = %.backedge
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp50, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core13SC_ID_GET_IF_E, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8]* @.str28, i64 0, i64 0))
  %.pre.i = load %class.read_if** %tmp49, align 8, !tbaa !4
  br label %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit

_ZN7sc_core9sc_port_bI7read_ifEptEv.exit:         ; preds = %bb53, %.backedge
  %tmp54 = phi %class.read_if* [ %.pre.i, %bb53 ], [ %tmp51, %.backedge ]
  %tmp55 = bitcast %class.read_if* %tmp54 to void (%class.read_if*, i8*)***
  %tmp56 = load void (%class.read_if*, i8*)*** %tmp55, align 8, !tbaa !0
  %tmp57 = getelementptr inbounds void (%class.read_if*, i8*)** %tmp56, i64 4
  %tmp58 = load void (%class.read_if*, i8*)** %tmp57, align 8
  call void %tmp58(%class.read_if* %tmp54, i8* %c)
  %tmp59 = load i8* %c, align 1, !tbaa !3
  call void @llvm.lifetime.start(i64 -1, i8* %tmp9)
  store i8 %tmp59, i8* %tmp9, align 1, !tbaa !3
  %tmp60 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* %tmp9, i64 1)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp9)
  %tmp61 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp60)
  %tmp62 = load %class.read_if** %tmp49, align 8, !tbaa !4
  %tmp63 = icmp eq %class.read_if* %tmp62, null
  br i1 %tmp63, label %bb64, label %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit6

bb64:                                             ; preds = %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp50, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core13SC_ID_GET_IF_E, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8]* @.str28, i64 0, i64 0))
  %.pre.i5 = load %class.read_if** %tmp49, align 8, !tbaa !4
  br label %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit6

_ZN7sc_core9sc_port_bI7read_ifEptEv.exit6:        ; preds = %bb64, %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit
  %tmp65 = phi %class.read_if* [ %.pre.i5, %bb64 ], [ %tmp62, %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit ]
  %tmp66 = bitcast %class.read_if* %tmp65 to i32 (%class.read_if*)***
  %tmp67 = load i32 (%class.read_if*)*** %tmp66, align 8, !tbaa !0
  %tmp68 = getelementptr inbounds i32 (%class.read_if*)** %tmp67, i64 5
  %tmp69 = load i32 (%class.read_if*)** %tmp68, align 8
  %tmp70 = call i32 %tmp69(%class.read_if* %tmp65)
  %tmp71 = icmp eq i32 %tmp70, 1
  br i1 %tmp71, label %bb72, label %bb75

bb72:                                             ; preds = %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit6
  %tmp73 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([4 x i8]* @.str26, i64 0, i64 0), i64 3)
  %tmp74 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* @_ZSt4cout)
  br label %bb75

bb75:                                             ; preds = %bb72, %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit6
  %tmp76 = load %class.read_if** %tmp49, align 8, !tbaa !4
  %tmp77 = icmp eq %class.read_if* %tmp76, null
  br i1 %tmp77, label %bb78, label %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit8

bb78:                                             ; preds = %bb75
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp50, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core13SC_ID_GET_IF_E, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8]* @.str28, i64 0, i64 0))
  %.pre.i7 = load %class.read_if** %tmp49, align 8, !tbaa !4
  br label %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit8

_ZN7sc_core9sc_port_bI7read_ifEptEv.exit8:        ; preds = %bb78, %bb75
  %tmp79 = phi %class.read_if* [ %.pre.i7, %bb78 ], [ %tmp76, %bb75 ]
  %tmp80 = bitcast %class.read_if* %tmp79 to i32 (%class.read_if*)***
  %tmp81 = load i32 (%class.read_if*)*** %tmp80, align 8, !tbaa !0
  %tmp82 = getelementptr inbounds i32 (%class.read_if*)** %tmp81, i64 5
  %tmp83 = load i32 (%class.read_if*)** %tmp82, align 8
  %tmp84 = call i32 %tmp83(%class.read_if* %tmp79)
  %tmp85 = icmp eq i32 %tmp84, 9
  br i1 %tmp85, label %bb86, label %.backedge.backedge

.backedge.backedge:                               ; preds = %bb86, %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit8
  br label %.backedge

bb86:                                             ; preds = %_ZN7sc_core9sc_port_bI7read_ifEptEv.exit8
  %tmp87 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([4 x i8]* @.str27, i64 0, i64 0), i64 3)
  %tmp88 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* @_ZSt4cout)
  br label %.backedge.backedge
}

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

define linkonce_odr void @_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED1Ev(%"class.sc_core::sc_port.59"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port.59"* %this, i64 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI7read_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port.59"* %this, i64 0, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %class.read_if*** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %class.read_if** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %class.read_if** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit

_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit: ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port.59"* %this, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  ret void
}

define linkonce_odr void @_ZN8consumerD1Ev(%class.consumer* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %class.consumer* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.consumer* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %class.consumer* %this, i64 0, i32 1
  %tmp3 = getelementptr inbounds %"class.sc_core::sc_port.59"* %tmp2, i64 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI7read_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %class.consumer* %this, i64 0, i32 1, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp5 = load %class.read_if*** %tmp4, align 8, !tbaa !4
  %tmp6 = icmp eq %class.read_if** %tmp5, null
  br i1 %tmp6, label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i, label %bb7

bb7:                                              ; preds = %bb
  %tmp8 = bitcast %class.read_if** %tmp5 to i8*
  tail call void @_ZdlPv(i8* %tmp8) nounwind
  br label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i

_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i: ; preds = %bb7, %bb
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port.59"* %tmp2, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp9)
  %tmp10 = getelementptr inbounds %class.consumer* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp10)
  ret void
}

define linkonce_odr void @_ZN8consumerD0Ev(%class.consumer* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %class.consumer* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.consumer* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %class.consumer* %this, i64 0, i32 1
  %tmp3 = getelementptr inbounds %"class.sc_core::sc_port.59"* %tmp2, i64 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI7read_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %class.consumer* %this, i64 0, i32 1, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp5 = load %class.read_if*** %tmp4, align 8, !tbaa !4
  %tmp6 = icmp eq %class.read_if** %tmp5, null
  br i1 %tmp6, label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i, label %bb7

bb7:                                              ; preds = %bb
  %tmp8 = bitcast %class.read_if** %tmp5 to i8*
  tail call void @_ZdlPv(i8* %tmp8) nounwind
  br label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i

_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i: ; preds = %bb7, %bb
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port.59"* %tmp2, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp9)
  %tmp10 = getelementptr inbounds %class.consumer* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp10)
  %tmp11 = bitcast %class.consumer* %this to i8*
  tail call void @_ZdlPv(i8* %tmp11) nounwind
  ret void
}

define linkonce_odr void @_ZThn40_N8consumerD1Ev(%class.consumer* %this) {
bb:
  %tmp = getelementptr inbounds %class.consumer* %this, i64 -1, i32 1, i32 0, i32 0, i32 1
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 2) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"struct.sc_core::sc_bind_info"** %tmp, i64 5
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 15) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %"struct.sc_core::sc_bind_info"** %tmp, i64 23
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI7read_ifEE, i64 0, i64 2) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr inbounds %"struct.sc_core::sc_bind_info"** %tmp, i64 30
  %tmp4 = load %"struct.sc_core::sc_bind_info"** %tmp3, align 8
  %tmp5 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp4, null
  br i1 %tmp5, label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i, label %bb6

bb6:                                              ; preds = %bb
  %tmp7 = bitcast %"struct.sc_core::sc_bind_info"* %tmp4 to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  br label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i

_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i: ; preds = %bb6, %bb
  %tmp8 = bitcast %"struct.sc_core::sc_bind_info"** %tmp2 to %"class.sc_core::sc_port_base"*
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp8)
  %tmp9 = bitcast %"struct.sc_core::sc_bind_info"** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp9)
  ret void
}

define linkonce_odr void @_ZThn40_N8consumerD0Ev(%class.consumer* %this) {
bb:
  %tmp = getelementptr inbounds %class.consumer* %this, i64 -1, i32 1, i32 0, i32 0, i32 1
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 2) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"struct.sc_core::sc_bind_info"** %tmp, i64 5
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8consumer, i64 0, i64 15) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %"struct.sc_core::sc_bind_info"** %tmp, i64 23
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI7read_ifEE, i64 0, i64 2) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr inbounds %"struct.sc_core::sc_bind_info"** %tmp, i64 30
  %tmp4 = load %"struct.sc_core::sc_bind_info"** %tmp3, align 8
  %tmp5 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp4, null
  br i1 %tmp5, label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i.i, label %bb6

bb6:                                              ; preds = %bb
  %tmp7 = bitcast %"struct.sc_core::sc_bind_info"* %tmp4 to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  br label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i.i

_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i.i: ; preds = %bb6, %bb
  %tmp8 = bitcast %"struct.sc_core::sc_bind_info"** %tmp2 to %"class.sc_core::sc_port_base"*
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp8)
  %tmp9 = bitcast %"struct.sc_core::sc_bind_info"** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp9)
  %tmp10 = bitcast %"struct.sc_core::sc_bind_info"** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp10) nounwind
  ret void
}

declare void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"*)

define linkonce_odr i8* @_ZNK7sc_core12sc_port_base4kindEv(%"class.sc_core::sc_port_base"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([13 x i8]* @.str22, i64 0, i64 0)
}

define linkonce_odr void @_ZN7sc_core9sc_port_bI7read_ifED1Ev(%"class.sc_core::sc_port_b.60"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI7read_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %class.read_if*** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %class.read_if** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core9sc_port_bI7read_ifED2Ev.exit, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %class.read_if** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core9sc_port_bI7read_ifED2Ev.exit

_ZN7sc_core9sc_port_bI7read_ifED2Ev.exit:         ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  ret void
}

define linkonce_odr void @_ZN7sc_core9sc_port_bI7read_ifED0Ev(%"class.sc_core::sc_port_b.60"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI7read_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %class.read_if*** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %class.read_if** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core9sc_port_bI7read_ifED2Ev.exit.i, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %class.read_if** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core9sc_port_bI7read_ifED2Ev.exit.i

_ZN7sc_core9sc_port_bI7read_ifED2Ev.exit.i:       ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  %tmp7 = bitcast %"class.sc_core::sc_port_b.60"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  ret void
}

define linkonce_odr %"class.sc_core::sc_interface"* @_ZN7sc_core9sc_port_bI7read_ifE13get_interfaceEv(%"class.sc_core::sc_port_b.60"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 1
  %tmp1 = load %class.read_if** %tmp, align 8, !tbaa !4
  %tmp2 = icmp eq %class.read_if* %tmp1, null
  br i1 %tmp2, label %bb12, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %class.read_if* %tmp1 to i8**
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = getelementptr i8* %tmp5, i64 -48
  %tmp7 = bitcast i8* %tmp6 to i64*
  %tmp8 = load i64* %tmp7, align 8
  %tmp9 = bitcast %class.read_if* %tmp1 to i8*
  %tmp10 = getelementptr i8* %tmp9, i64 %tmp8
  %tmp11 = bitcast i8* %tmp10 to %"class.sc_core::sc_interface"*
  br label %bb12

bb12:                                             ; preds = %bb3, %bb
  %tmp13 = phi %"class.sc_core::sc_interface"* [ %tmp11, %bb3 ], [ null, %bb ]
  ret %"class.sc_core::sc_interface"* %tmp13
}

define linkonce_odr %"class.sc_core::sc_interface"* @_ZNK7sc_core9sc_port_bI7read_ifE13get_interfaceEv(%"class.sc_core::sc_port_b.60"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 1
  %tmp1 = load %class.read_if** %tmp, align 8, !tbaa !4
  %tmp2 = icmp eq %class.read_if* %tmp1, null
  br i1 %tmp2, label %bb12, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %class.read_if* %tmp1 to i8**
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = getelementptr i8* %tmp5, i64 -48
  %tmp7 = bitcast i8* %tmp6 to i64*
  %tmp8 = load i64* %tmp7, align 8
  %tmp9 = bitcast %class.read_if* %tmp1 to i8*
  %tmp10 = getelementptr i8* %tmp9, i64 %tmp8
  %tmp11 = bitcast i8* %tmp10 to %"class.sc_core::sc_interface"*
  br label %bb12

bb12:                                             ; preds = %bb3, %bb
  %tmp13 = phi %"class.sc_core::sc_interface"* [ %tmp11, %bb3 ], [ null, %bb ]
  ret %"class.sc_core::sc_interface"* %tmp13
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bI7read_ifE5vbindERNS_12sc_interfaceE(%"class.sc_core::sc_port_b.60"* %this, %"class.sc_core::sc_interface"* %interface_) uwtable align 2 {
bb:
  %tmp = icmp eq %"class.sc_core::sc_interface"* %interface_, null
  br i1 %tmp, label %.thread, label %bb1

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_interface"* %interface_ to i8*
  %tmp3 = tail call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI7read_if to i8*), i64 -1)
  %tmp4 = icmp eq i8* %tmp3, null
  br i1 %tmp4, label %.thread, label %bb5

bb5:                                              ; preds = %bb1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0
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

define linkonce_odr i32 @_ZN7sc_core9sc_port_bI7read_ifE5vbindERNS_12sc_port_baseE(%"class.sc_core::sc_port_b.60"* %this, %"class.sc_core::sc_port_base"* %parent_) uwtable align 2 {
bb:
  %tmp = icmp eq %"class.sc_core::sc_port_base"* %parent_, null
  br i1 %tmp, label %.thread, label %bb1

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_port_base"* %parent_ to i8*
  %tmp3 = tail call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core12sc_port_baseE to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bI7read_ifEE to i8*), i64 -1)
  %tmp4 = icmp eq i8* %tmp3, null
  br i1 %tmp4, label %.thread, label %bb5

bb5:                                              ; preds = %bb1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0
  %tmp7 = bitcast i8* %tmp3 to %"class.sc_core::sc_port_base"*
  tail call void @_ZN7sc_core12sc_port_base4bindERS0_(%"class.sc_core::sc_port_base"* %tmp6, %"class.sc_core::sc_port_base"* %tmp7)
  br label %.thread

.thread:                                          ; preds = %bb5, %bb1, %bb
  %.0 = phi i32 [ 0, %bb5 ], [ 2, %bb1 ], [ 2, %bb ]
  ret i32 %.0
}

define linkonce_odr void @_ZN7sc_core9sc_port_bI7read_ifE13add_interfaceEPNS_12sc_interfaceE(%"class.sc_core::sc_port_b.60"* %this, %"class.sc_core::sc_interface"* %interface_) uwtable align 2 {
bb:
  %iface = alloca %class.read_if*, align 8
  %tmp = icmp eq %"class.sc_core::sc_interface"* %interface_, null
  br i1 %tmp, label %.thread, label %bb1

.thread:                                          ; preds = %bb
  store %class.read_if* null, %class.read_if** %iface, align 8, !tbaa !4
  br label %bb6

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_interface"* %interface_ to i8*
  %tmp3 = call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI7read_if to i8*), i64 -1)
  %tmp4 = bitcast i8* %tmp3 to %class.read_if*
  store %class.read_if* %tmp4, %class.read_if** %iface, align 8, !tbaa !4
  %tmp5 = icmp eq i8* %tmp3, null
  br i1 %tmp5, label %bb6, label %bb7

bb6:                                              ; preds = %bb1, %.thread
  call void @__assert_fail(i8* getelementptr inbounds ([11 x i8]* @.str20, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str19, i64 0, i64 0), i32 580, i8* getelementptr inbounds ([96 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core9sc_port_bI7read_ifE13add_interfaceEPNS_12sc_interfaceE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb7:                                              ; preds = %bb1
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp10 = load %class.read_if*** %tmp9, align 8, !tbaa !4
  %tmp11 = getelementptr inbounds %"class.std::vector.61"* %tmp8, i64 0, i32 0, i32 0, i32 0
  %tmp12 = load %class.read_if*** %tmp11, align 8, !tbaa !4
  %tmp13 = ptrtoint %class.read_if** %tmp10 to i64
  %tmp14 = ptrtoint %class.read_if** %tmp12 to i64
  %tmp15 = sub i64 %tmp13, %tmp14
  %tmp16 = lshr exact i64 %tmp15, 3
  %tmp17 = trunc i64 %tmp16 to i32
  %tmp18 = icmp sgt i32 %tmp17, 0
  br i1 %tmp18, label %.lr.ph, label %bb27

.lr.ph:                                           ; preds = %bb7
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0
  br label %bb20

bb20:                                             ; preds = %._crit_edge2, %.lr.ph
  %tmp21 = phi %class.read_if** [ %tmp12, %.lr.ph ], [ %.pre3, %._crit_edge2 ]
  %indvars.iv = phi i64 [ 0, %.lr.ph ], [ %indvars.iv.next, %._crit_edge2 ]
  %tmp22 = getelementptr inbounds %class.read_if** %tmp21, i64 %indvars.iv
  %tmp23 = load %class.read_if** %tmp22, align 8, !tbaa !4
  %tmp24 = icmp eq %class.read_if* %tmp4, %tmp23
  br i1 %tmp24, label %bb25, label %bb26

bb25:                                             ; preds = %bb20
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp19, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core22SC_ID_BIND_IF_TO_PORT_E, i64 0, i64 0), i8* getelementptr inbounds ([32 x i8]* @.str21, i64 0, i64 0))
  br label %bb26

bb26:                                             ; preds = %bb25, %bb20
  %indvars.iv.next = add i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %tmp17
  br i1 %exitcond, label %._crit_edge, label %._crit_edge2

._crit_edge2:                                     ; preds = %bb26
  %.pre3 = load %class.read_if*** %tmp11, align 8, !tbaa !4
  br label %bb20

._crit_edge:                                      ; preds = %bb26
  %.pre = load %class.read_if*** %tmp9, align 8, !tbaa !4
  br label %bb27

bb27:                                             ; preds = %._crit_edge, %bb7
  %tmp28 = phi %class.read_if** [ %.pre, %._crit_edge ], [ %tmp10, %bb7 ]
  %tmp29 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 2
  %tmp30 = load %class.read_if*** %tmp29, align 8, !tbaa !4
  %tmp31 = icmp eq %class.read_if** %tmp28, %tmp30
  br i1 %tmp31, label %bb37, label %bb32

bb32:                                             ; preds = %bb27
  %tmp33 = icmp eq %class.read_if** %tmp28, null
  br i1 %tmp33, label %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit.i, label %bb34

bb34:                                             ; preds = %bb32
  store %class.read_if* %tmp4, %class.read_if** %tmp28, align 8, !tbaa !4
  %.pre.i = load %class.read_if*** %tmp9, align 8, !tbaa !4
  br label %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit.i

_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit.i: ; preds = %bb34, %bb32
  %tmp35 = phi %class.read_if** [ null, %bb32 ], [ %.pre.i, %bb34 ]
  %tmp36 = getelementptr inbounds %class.read_if** %tmp35, i64 1
  store %class.read_if** %tmp36, %class.read_if*** %tmp9, align 8, !tbaa !4
  br label %_ZNSt6vectorIP7read_ifSaIS1_EE9push_backERKS1_.exit

bb37:                                             ; preds = %bb27
  call void @_ZNSt6vectorIP7read_ifSaIS1_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS1_S3_EERKS1_(%"class.std::vector.61"* %tmp8, %class.read_if** %tmp28, %class.read_if** %iface)
  br label %_ZNSt6vectorIP7read_ifSaIS1_EE9push_backERKS1_.exit

_ZNSt6vectorIP7read_ifSaIS1_EE9push_backERKS1_.exit: ; preds = %bb37, %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit.i
  %tmp38 = load %class.read_if*** %tmp11, align 8, !tbaa !4
  %tmp39 = load %class.read_if** %tmp38, align 8, !tbaa !4
  %tmp40 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 1
  store %class.read_if* %tmp39, %class.read_if** %tmp40, align 8, !tbaa !4
  ret void
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bI7read_ifE15interface_countEv(%"class.sc_core::sc_port_b.60"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp1 = load %class.read_if*** %tmp, align 8, !tbaa !4
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp3 = load %class.read_if*** %tmp2, align 8, !tbaa !4
  %tmp4 = ptrtoint %class.read_if** %tmp1 to i64
  %tmp5 = ptrtoint %class.read_if** %tmp3 to i64
  %tmp6 = sub i64 %tmp4, %tmp5
  %tmp7 = lshr exact i64 %tmp6, 3
  %tmp8 = trunc i64 %tmp7 to i32
  ret i32 %tmp8
}

define linkonce_odr i8* @_ZNK7sc_core9sc_port_bI7read_ifE11if_typenameEv(%"class.sc_core::sc_port_b.60"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  ret i8* getelementptr inbounds ([9 x i8]* @_ZTS7read_if, i64 0, i64 0)
}

declare void @_ZN7sc_core12sc_port_base25before_end_of_elaborationEv(%"class.sc_core::sc_port_base"*)

declare void @_ZN7sc_core12sc_port_base18end_of_elaborationEv(%"class.sc_core::sc_port_base"*)

declare void @_ZN7sc_core12sc_port_base19start_of_simulationEv(%"class.sc_core::sc_port_base"*)

declare void @_ZN7sc_core12sc_port_base17end_of_simulationEv(%"class.sc_core::sc_port_base"*)

define linkonce_odr void @_ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_b.60"* %this, %"class.sc_core::sc_thread_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0, i32 1
  %tmp2 = load %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb37

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp6 = load %class.read_if*** %tmp5, align 8, !tbaa !4
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp8 = load %class.read_if*** %tmp7, align 8, !tbaa !4
  %tmp9 = ptrtoint %class.read_if** %tmp6 to i64
  %tmp10 = ptrtoint %class.read_if** %tmp8 to i64
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
  %tmp18 = load %class.read_if*** %tmp7, align 8, !tbaa !4
  %tmp19 = getelementptr inbounds %class.read_if** %tmp18, i64 %indvars.iv
  %tmp20 = load %class.read_if** %tmp19, align 8, !tbaa !4
  %tmp21 = icmp eq %class.read_if* %tmp20, null
  br i1 %tmp21, label %bb22, label %bb23

bb22:                                             ; preds = %bb17
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str19, i64 0, i64 0), i32 627, i8* getelementptr inbounds ([124 x i8]* @__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb23:                                             ; preds = %bb17
  %tmp24 = bitcast %class.read_if* %tmp20 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -48
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %class.read_if* %tmp20 to i8*
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

define linkonce_odr void @_ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_b.60"* %this, %"class.sc_core::sc_method_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 0, i32 1
  %tmp2 = load %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb37

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp6 = load %class.read_if*** %tmp5, align 8, !tbaa !4
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_port_b.60"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp8 = load %class.read_if*** %tmp7, align 8, !tbaa !4
  %tmp9 = ptrtoint %class.read_if** %tmp6 to i64
  %tmp10 = ptrtoint %class.read_if** %tmp8 to i64
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
  %tmp18 = load %class.read_if*** %tmp7, align 8, !tbaa !4
  %tmp19 = getelementptr inbounds %class.read_if** %tmp18, i64 %indvars.iv
  %tmp20 = load %class.read_if** %tmp19, align 8, !tbaa !4
  %tmp21 = icmp eq %class.read_if* %tmp20, null
  br i1 %tmp21, label %bb22, label %bb23

bb22:                                             ; preds = %bb17
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str19, i64 0, i64 0), i32 648, i8* getelementptr inbounds ([124 x i8]* @__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bI7read_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb23:                                             ; preds = %bb17
  %tmp24 = bitcast %class.read_if* %tmp20 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -48
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %class.read_if* %tmp20 to i8*
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

define linkonce_odr void @_ZNSt6vectorIP7read_ifSaIS1_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS1_S3_EERKS1_(%"class.std::vector.61"* nocapture %this, %class.read_if** %__position.coerce, %class.read_if** nocapture %__x) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.std::vector.61"* %this, i64 0, i32 0, i32 0, i32 1
  %tmp1 = load %class.read_if*** %tmp, align 8, !tbaa !4
  %tmp2 = getelementptr inbounds %"class.std::vector.61"* %this, i64 0, i32 0, i32 0, i32 2
  %tmp3 = load %class.read_if*** %tmp2, align 8, !tbaa !4
  %tmp4 = icmp eq %class.read_if** %tmp1, %tmp3
  br i1 %tmp4, label %_ZNKSt6vectorIP7read_ifSaIS1_EE12_M_check_lenEmPKc.exit, label %bb5

bb5:                                              ; preds = %bb
  %tmp6 = icmp eq %class.read_if** %tmp1, null
  br i1 %tmp6, label %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit, label %bb7

bb7:                                              ; preds = %bb5
  %tmp8 = getelementptr inbounds %class.read_if** %tmp1, i64 -1
  %tmp9 = load %class.read_if** %tmp8, align 8, !tbaa !4
  store %class.read_if* %tmp9, %class.read_if** %tmp1, align 8, !tbaa !4
  %.pre = load %class.read_if*** %tmp, align 8, !tbaa !4
  br label %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit

_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit: ; preds = %bb7, %bb5
  %tmp10 = phi %class.read_if** [ null, %bb5 ], [ %.pre, %bb7 ]
  %tmp11 = getelementptr inbounds %class.read_if** %tmp10, i64 1
  store %class.read_if** %tmp11, %class.read_if*** %tmp, align 8, !tbaa !4
  %tmp12 = load %class.read_if** %__x, align 8, !tbaa !4
  %tmp13 = getelementptr inbounds %class.read_if** %tmp10, i64 -1
  %tmp14 = ptrtoint %class.read_if** %tmp13 to i64
  %tmp15 = ptrtoint %class.read_if** %__position.coerce to i64
  %tmp16 = sub i64 %tmp14, %tmp15
  %tmp17 = ashr exact i64 %tmp16, 3
  %tmp18 = icmp eq i64 %tmp17, 0
  br i1 %tmp18, label %_ZSt13copy_backwardIPP7read_ifS2_ET0_T_S4_S3_.exit, label %bb19

bb19:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit
  %.pre.i.i.i.i = sub i64 0, %tmp17
  %.pre1.i.i.i.i = getelementptr inbounds %class.read_if** %tmp10, i64 %.pre.i.i.i.i
  %tmp20 = bitcast %class.read_if** %.pre1.i.i.i.i to i8*
  %tmp21 = bitcast %class.read_if** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp20, i8* %tmp21, i64 %tmp16, i32 8, i1 false) nounwind
  br label %_ZSt13copy_backwardIPP7read_ifS2_ET0_T_S4_S3_.exit

_ZSt13copy_backwardIPP7read_ifS2_ET0_T_S4_S3_.exit: ; preds = %bb19, %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit
  store %class.read_if* %tmp12, %class.read_if** %__position.coerce, align 8, !tbaa !4
  br label %bb67

_ZNKSt6vectorIP7read_ifSaIS1_EE12_M_check_lenEmPKc.exit: ; preds = %bb
  %tmp22 = getelementptr inbounds %"class.std::vector.61"* %this, i64 0, i32 0, i32 0, i32 0
  %tmp23 = load %class.read_if*** %tmp22, align 8, !tbaa !4
  %tmp24 = ptrtoint %class.read_if** %tmp1 to i64
  %tmp25 = ptrtoint %class.read_if** %tmp23 to i64
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
  br i1 %tmp34, label %_ZNSt12_Vector_baseIP7read_ifSaIS1_EE11_M_allocateEm.exit, label %bb35

bb35:                                             ; preds = %_ZNKSt6vectorIP7read_ifSaIS1_EE12_M_check_lenEmPKc.exit
  %tmp36 = icmp ugt i64 %tmp33, 2305843009213693951
  br i1 %tmp36, label %bb37, label %_ZN9__gnu_cxx13new_allocatorIP7read_ifE8allocateEmPKv.exit.i

bb37:                                             ; preds = %bb35
  tail call void @_ZSt17__throw_bad_allocv() noreturn
  unreachable

_ZN9__gnu_cxx13new_allocatorIP7read_ifE8allocateEmPKv.exit.i: ; preds = %bb35
  %tmp38 = shl i64 %tmp33, 3
  %tmp39 = tail call noalias i8* @_Znwm(i64 %tmp38)
  %tmp40 = bitcast i8* %tmp39 to %class.read_if**
  br label %_ZNSt12_Vector_baseIP7read_ifSaIS1_EE11_M_allocateEm.exit

_ZNSt12_Vector_baseIP7read_ifSaIS1_EE11_M_allocateEm.exit: ; preds = %_ZN9__gnu_cxx13new_allocatorIP7read_ifE8allocateEmPKv.exit.i, %_ZNKSt6vectorIP7read_ifSaIS1_EE12_M_check_lenEmPKc.exit
  %tmp41 = phi %class.read_if** [ %tmp40, %_ZN9__gnu_cxx13new_allocatorIP7read_ifE8allocateEmPKv.exit.i ], [ null, %_ZNKSt6vectorIP7read_ifSaIS1_EE12_M_check_lenEmPKc.exit ]
  %tmp42 = ptrtoint %class.read_if** %__position.coerce to i64
  %tmp43 = sub i64 %tmp42, %tmp25
  %tmp44 = ashr exact i64 %tmp43, 3
  %tmp45 = getelementptr inbounds %class.read_if** %tmp41, i64 %tmp44
  %tmp46 = icmp eq %class.read_if** %tmp45, null
  br i1 %tmp46, label %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit3, label %bb47

bb47:                                             ; preds = %_ZNSt12_Vector_baseIP7read_ifSaIS1_EE11_M_allocateEm.exit
  %tmp48 = load %class.read_if** %__x, align 8, !tbaa !4
  store %class.read_if* %tmp48, %class.read_if** %tmp45, align 8, !tbaa !4
  br label %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit3

_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit3: ; preds = %bb47, %_ZNSt12_Vector_baseIP7read_ifSaIS1_EE11_M_allocateEm.exit
  %tmp49 = icmp eq i64 %tmp44, 0
  br i1 %tmp49, label %bb53, label %bb50

bb50:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit3
  %tmp51 = bitcast %class.read_if** %tmp41 to i8*
  %tmp52 = bitcast %class.read_if** %tmp23 to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp51, i8* %tmp52, i64 %tmp43, i32 8, i1 false) nounwind
  br label %bb53

bb53:                                             ; preds = %bb50, %_ZN9__gnu_cxx13new_allocatorIP7read_ifE9constructEPS2_RKS2_.exit3
  %.sum = add i64 %tmp44, 1
  %tmp54 = sub i64 %tmp24, %tmp42
  %tmp55 = ashr exact i64 %tmp54, 3
  %tmp56 = icmp eq i64 %tmp55, 0
  br i1 %tmp56, label %bb61, label %bb57

bb57:                                             ; preds = %bb53
  %tmp58 = getelementptr inbounds %class.read_if** %tmp41, i64 %.sum
  %tmp59 = bitcast %class.read_if** %tmp58 to i8*
  %tmp60 = bitcast %class.read_if** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp59, i8* %tmp60, i64 %tmp54, i32 8, i1 false) nounwind
  br label %bb61

bb61:                                             ; preds = %bb57, %bb53
  %tmp62 = icmp eq %class.read_if** %tmp23, null
  br i1 %tmp62, label %_ZNSt12_Vector_baseIP7read_ifSaIS1_EE13_M_deallocateEPS1_m.exit1, label %bb63

bb63:                                             ; preds = %bb61
  %tmp64 = bitcast %class.read_if** %tmp23 to i8*
  tail call void @_ZdlPv(i8* %tmp64) nounwind
  br label %_ZNSt12_Vector_baseIP7read_ifSaIS1_EE13_M_deallocateEPS1_m.exit1

_ZNSt12_Vector_baseIP7read_ifSaIS1_EE13_M_deallocateEPS1_m.exit1: ; preds = %bb63, %bb61
  %.sum4 = add i64 %tmp55, %.sum
  %tmp65 = getelementptr inbounds %class.read_if** %tmp41, i64 %.sum4
  store %class.read_if** %tmp41, %class.read_if*** %tmp22, align 8, !tbaa !4
  store %class.read_if** %tmp65, %class.read_if*** %tmp, align 8, !tbaa !4
  %tmp66 = getelementptr inbounds %class.read_if** %tmp41, i64 %tmp33
  store %class.read_if** %tmp66, %class.read_if*** %tmp2, align 8, !tbaa !4
  br label %bb67

bb67:                                             ; preds = %_ZNSt12_Vector_baseIP7read_ifSaIS1_EE13_M_deallocateEPS1_m.exit1, %_ZSt13copy_backwardIPP7read_ifS2_ET0_T_S4_S3_.exit
  ret void
}

declare void @_ZN7sc_core12sc_port_base4bindERS0_(%"class.sc_core::sc_port_base"*, %"class.sc_core::sc_port_base"*)

define linkonce_odr i8* @_ZNK7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EE4kindEv(%"class.sc_core::sc_port.59"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([8 x i8]* @.str29, i64 0, i64 0)
}

define linkonce_odr void @_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED0Ev(%"class.sc_core::sc_port.59"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port.59"* %this, i64 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI7read_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port.59"* %this, i64 0, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %class.read_if*** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %class.read_if** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %class.read_if** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i

_ZN7sc_core7sc_portI7read_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i: ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port.59"* %this, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  %tmp7 = bitcast %"class.sc_core::sc_port.59"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  ret void
}

declare void @_ZN7sc_core12sc_port_baseC2EiNS_14sc_port_policyE(%"class.sc_core::sc_port_base"*, i32, i32)

define linkonce_odr void @_ZN8producerC2EN7sc_core14sc_module_nameE(%class.producer* %this, %"class.sc_core::sc_module_name"* %name) unnamed_addr uwtable align 2 {
bb:
  %main_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = getelementptr inbounds %class.producer* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp3, %"class.sc_core::sc_module_name"* %name)
  %tmp4 = getelementptr inbounds %class.producer* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp4, align 8, !tbaa !0
  %tmp5 = getelementptr %class.producer* %this, i64 0, i32 0, i32 1
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp5, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp6, align 8, !tbaa !0
  %tmp7 = getelementptr inbounds %class.producer* %this, i64 0, i32 1
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_port"* %tmp7, i64 0, i32 0, i32 0
  call void @_ZN7sc_core12sc_port_baseC2EiNS_14sc_port_policyE(%"class.sc_core::sc_port_base"* %tmp8, i32 1, i32 0)
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port"* %tmp7, i64 0, i32 0, i32 0, i32 0, i32 0
  %tmp10 = getelementptr inbounds %class.producer* %this, i64 0, i32 1, i32 0, i32 1
  %tmp11 = bitcast %class.write_if** %tmp10 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp11, i8 0, i64 32, i32 8, i1 false)
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp9, align 8, !tbaa !0
  %tmp12 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  %tmp13 = icmp eq %"class.sc_core::sc_simcontext"* %tmp12, null
  br i1 %tmp13, label %.noexc, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc:                                           ; preds = %bb
  %tmp14 = call noalias i8* @_Znwm(i64 248)
  %tmp15 = bitcast i8* %tmp14 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp15)
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !4
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc, %bb
  %tmp16 = phi %"class.sc_core::sc_simcontext"* [ %tmp15, %.noexc ], [ %tmp12, %bb ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %main_handle, %"class.sc_core::sc_simcontext"* %tmp16, i8* getelementptr inbounds ([5 x i8]* @.str17, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.producer*)* @_ZN8producer4mainEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp5, %"class.sc_core::sc_spawn_options"* null)
  %tmp17 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %main_handle, i64 0, i32 0
  %tmp18 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !4
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp18, %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !4
  %tmp20 = icmp eq %"class.sc_core::sc_process_b"* %tmp18, null
  br i1 %tmp20, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb21

bb21:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp22 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp18, i64 0, i32 15
  %tmp23 = load i32* %tmp22, align 4, !tbaa !7
  %tmp24 = icmp eq i32 %tmp23, 0
  br i1 %tmp24, label %bb25, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb25:                                             ; preds = %bb21
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str25, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb21
  %tmp26 = add nsw i32 %tmp23, 1
  store i32 %tmp26, i32* %tmp22, align 4, !tbaa !7
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp27 = getelementptr inbounds %class.producer* %this, i64 0, i32 0, i32 2
  %tmp28 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp27, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp29 = load %"class.sc_core::sc_process_b"** %tmp19, align 8, !tbaa !4
  %tmp30 = icmp eq %"class.sc_core::sc_process_b"* %tmp29, null
  br i1 %tmp30, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb31

bb31:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp32 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp29, i64 0, i32 15
  %tmp33 = load i32* %tmp32, align 4, !tbaa !7
  %tmp34 = add nsw i32 %tmp33, -1
  store i32 %tmp34, i32* %tmp32, align 4, !tbaa !7
  %tmp35 = icmp eq i32 %tmp34, 0
  br i1 %tmp35, label %bb36, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb36:                                             ; preds = %bb31
  %tmp37 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp38 = icmp eq %"class.sc_core::sc_process_b"* %tmp37, null
  br i1 %tmp38, label %bb43, label %.noexc6

.noexc6:                                          ; preds = %bb36
  %tmp39 = bitcast %"class.sc_core::sc_process_b"* %tmp37 to void (%"class.sc_core::sc_process_b"*)***
  %tmp40 = load void (%"class.sc_core::sc_process_b"*)*** %tmp39, align 8, !tbaa !0
  %tmp41 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp40, i64 6
  %tmp42 = load void (%"class.sc_core::sc_process_b"*)** %tmp41, align 8
  call void %tmp42(%"class.sc_core::sc_process_b"* %tmp37)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb43

bb43:                                             ; preds = %.noexc6, %bb36
  %tmp44 = phi %"class.sc_core::sc_process_b"* [ null, %bb36 ], [ %.pre.i.i.i, %.noexc6 ]
  %tmp45 = icmp eq %"class.sc_core::sc_process_b"* %tmp44, %tmp29
  br i1 %tmp45, label %bb46, label %bb47

bb46:                                             ; preds = %bb43
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str23, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb47:                                             ; preds = %bb43
  store %"class.sc_core::sc_process_b"* %tmp29, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb47, %bb31, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp48 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !4
  %tmp49 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp48, %"class.sc_core::sc_process_b"** %tmp49, align 8, !tbaa !4
  %tmp50 = icmp eq %"class.sc_core::sc_process_b"* %tmp48, null
  br i1 %tmp50, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8, label %bb51

bb51:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp52 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp48, i64 0, i32 15
  %tmp53 = load i32* %tmp52, align 4, !tbaa !7
  %tmp54 = icmp eq i32 %tmp53, 0
  br i1 %tmp54, label %bb55, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7

bb55:                                             ; preds = %bb51
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str25, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7: ; preds = %bb51
  %tmp56 = add nsw i32 %tmp53, 1
  store i32 %tmp56, i32* %tmp52, align 4, !tbaa !7
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8

_ZN7sc_core17sc_process_handleC1ERKS0_.exit8:     ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i7, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp57 = getelementptr inbounds %class.producer* %this, i64 0, i32 0, i32 3
  %tmp58 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp57, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp59 = load %"class.sc_core::sc_process_b"** %tmp49, align 8, !tbaa !4
  %tmp60 = icmp eq %"class.sc_core::sc_process_b"* %tmp59, null
  br i1 %tmp60, label %_ZN7sc_core17sc_process_handleD1Ev.exit11, label %bb61

bb61:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8
  %tmp62 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp59, i64 0, i32 15
  %tmp63 = load i32* %tmp62, align 4, !tbaa !7
  %tmp64 = add nsw i32 %tmp63, -1
  store i32 %tmp64, i32* %tmp62, align 4, !tbaa !7
  %tmp65 = icmp eq i32 %tmp64, 0
  br i1 %tmp65, label %bb66, label %_ZN7sc_core17sc_process_handleD1Ev.exit11

bb66:                                             ; preds = %bb61
  %tmp67 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp68 = icmp eq %"class.sc_core::sc_process_b"* %tmp67, null
  br i1 %tmp68, label %bb73, label %.noexc10

.noexc10:                                         ; preds = %bb66
  %tmp69 = bitcast %"class.sc_core::sc_process_b"* %tmp67 to void (%"class.sc_core::sc_process_b"*)***
  %tmp70 = load void (%"class.sc_core::sc_process_b"*)*** %tmp69, align 8, !tbaa !0
  %tmp71 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp70, i64 6
  %tmp72 = load void (%"class.sc_core::sc_process_b"*)** %tmp71, align 8
  call void %tmp72(%"class.sc_core::sc_process_b"* %tmp67)
  %.pre.i.i.i9 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb73

bb73:                                             ; preds = %.noexc10, %bb66
  %tmp74 = phi %"class.sc_core::sc_process_b"* [ null, %bb66 ], [ %.pre.i.i.i9, %.noexc10 ]
  %tmp75 = icmp eq %"class.sc_core::sc_process_b"* %tmp74, %tmp59
  br i1 %tmp75, label %bb76, label %bb77

bb76:                                             ; preds = %bb73
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str23, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb77:                                             ; preds = %bb73
  store %"class.sc_core::sc_process_b"* %tmp59, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit11

_ZN7sc_core17sc_process_handleD1Ev.exit11:        ; preds = %bb77, %bb61, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit8
  %tmp78 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !4
  %tmp79 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp78, %"class.sc_core::sc_process_b"** %tmp79, align 8, !tbaa !4
  %tmp80 = icmp eq %"class.sc_core::sc_process_b"* %tmp78, null
  br i1 %tmp80, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13, label %bb81

bb81:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit11
  %tmp82 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp78, i64 0, i32 15
  %tmp83 = load i32* %tmp82, align 4, !tbaa !7
  %tmp84 = icmp eq i32 %tmp83, 0
  br i1 %tmp84, label %bb85, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12

bb85:                                             ; preds = %bb81
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str25, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12: ; preds = %bb81
  %tmp86 = add nsw i32 %tmp83, 1
  store i32 %tmp86, i32* %tmp82, align 4, !tbaa !7
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13

_ZN7sc_core17sc_process_handleC1ERKS0_.exit13:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12, %_ZN7sc_core17sc_process_handleD1Ev.exit11
  %tmp87 = getelementptr inbounds %class.producer* %this, i64 0, i32 0, i32 4
  %tmp88 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp87, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp89 = load %"class.sc_core::sc_process_b"** %tmp79, align 8, !tbaa !4
  %tmp90 = icmp eq %"class.sc_core::sc_process_b"* %tmp89, null
  br i1 %tmp90, label %_ZN7sc_core17sc_process_handleD1Ev.exit16, label %bb91

bb91:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp92 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp89, i64 0, i32 15
  %tmp93 = load i32* %tmp92, align 4, !tbaa !7
  %tmp94 = add nsw i32 %tmp93, -1
  store i32 %tmp94, i32* %tmp92, align 4, !tbaa !7
  %tmp95 = icmp eq i32 %tmp94, 0
  br i1 %tmp95, label %bb96, label %_ZN7sc_core17sc_process_handleD1Ev.exit16

bb96:                                             ; preds = %bb91
  %tmp97 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp98 = icmp eq %"class.sc_core::sc_process_b"* %tmp97, null
  br i1 %tmp98, label %bb103, label %.noexc15

.noexc15:                                         ; preds = %bb96
  %tmp99 = bitcast %"class.sc_core::sc_process_b"* %tmp97 to void (%"class.sc_core::sc_process_b"*)***
  %tmp100 = load void (%"class.sc_core::sc_process_b"*)*** %tmp99, align 8, !tbaa !0
  %tmp101 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp100, i64 6
  %tmp102 = load void (%"class.sc_core::sc_process_b"*)** %tmp101, align 8
  call void %tmp102(%"class.sc_core::sc_process_b"* %tmp97)
  %.pre.i.i.i14 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb103

bb103:                                            ; preds = %.noexc15, %bb96
  %tmp104 = phi %"class.sc_core::sc_process_b"* [ null, %bb96 ], [ %.pre.i.i.i14, %.noexc15 ]
  %tmp105 = icmp eq %"class.sc_core::sc_process_b"* %tmp104, %tmp89
  br i1 %tmp105, label %bb106, label %bb107

bb106:                                            ; preds = %bb103
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str23, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb107:                                            ; preds = %bb103
  store %"class.sc_core::sc_process_b"* %tmp89, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit16

_ZN7sc_core17sc_process_handleD1Ev.exit16:        ; preds = %bb107, %bb91, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp108 = load %"class.sc_core::sc_process_b"** %tmp17, align 8, !tbaa !4
  %tmp109 = icmp eq %"class.sc_core::sc_process_b"* %tmp108, null
  br i1 %tmp109, label %_ZN7sc_core17sc_process_handleD1Ev.exit19, label %bb110

bb110:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit16
  %tmp111 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp108, i64 0, i32 15
  %tmp112 = load i32* %tmp111, align 4, !tbaa !7
  %tmp113 = add nsw i32 %tmp112, -1
  store i32 %tmp113, i32* %tmp111, align 4, !tbaa !7
  %tmp114 = icmp eq i32 %tmp113, 0
  br i1 %tmp114, label %bb115, label %_ZN7sc_core17sc_process_handleD1Ev.exit19

bb115:                                            ; preds = %bb110
  %tmp116 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp117 = icmp eq %"class.sc_core::sc_process_b"* %tmp116, null
  br i1 %tmp117, label %bb122, label %.noexc18

.noexc18:                                         ; preds = %bb115
  %tmp118 = bitcast %"class.sc_core::sc_process_b"* %tmp116 to void (%"class.sc_core::sc_process_b"*)***
  %tmp119 = load void (%"class.sc_core::sc_process_b"*)*** %tmp118, align 8, !tbaa !0
  %tmp120 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp119, i64 6
  %tmp121 = load void (%"class.sc_core::sc_process_b"*)** %tmp120, align 8
  call void %tmp121(%"class.sc_core::sc_process_b"* %tmp116)
  %.pre.i.i.i17 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb122

bb122:                                            ; preds = %.noexc18, %bb115
  %tmp123 = phi %"class.sc_core::sc_process_b"* [ null, %bb115 ], [ %.pre.i.i.i17, %.noexc18 ]
  %tmp124 = icmp eq %"class.sc_core::sc_process_b"* %tmp123, %tmp108
  br i1 %tmp124, label %bb125, label %bb126

bb125:                                            ; preds = %bb122
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str23, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str24, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb126:                                            ; preds = %bb122
  store %"class.sc_core::sc_process_b"* %tmp108, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit19

_ZN7sc_core17sc_process_handleD1Ev.exit19:        ; preds = %bb126, %bb110, %_ZN7sc_core17sc_process_handleD1Ev.exit16
  ret void
}

define linkonce_odr void @_ZN8producer4mainEv(%class.producer* %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.producer* %this, i64 0, i32 1, i32 0, i32 1
  %tmp1 = getelementptr inbounds %class.producer* %this, i64 0, i32 1, i32 0, i32 0
  br label %bb2

bb2:                                              ; preds = %_ZN7sc_core9sc_port_bI8write_ifEptEv.exit, %bb
  %str.01 = phi i8* [ getelementptr inbounds ([66 x i8]* @.str30, i64 0, i64 0), %bb ], [ %tmp11, %_ZN7sc_core9sc_port_bI8write_ifEptEv.exit ]
  %tmp3 = load %class.write_if** %tmp, align 8, !tbaa !4
  %tmp4 = icmp eq %class.write_if* %tmp3, null
  br i1 %tmp4, label %bb5, label %_ZN7sc_core9sc_port_bI8write_ifEptEv.exit

bb5:                                              ; preds = %bb2
  tail call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp1, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core13SC_ID_GET_IF_E, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8]* @.str28, i64 0, i64 0))
  %.pre.i = load %class.write_if** %tmp, align 8, !tbaa !4
  br label %_ZN7sc_core9sc_port_bI8write_ifEptEv.exit

_ZN7sc_core9sc_port_bI8write_ifEptEv.exit:        ; preds = %bb5, %bb2
  %tmp6 = phi %class.write_if* [ %.pre.i, %bb5 ], [ %tmp3, %bb2 ]
  %tmp7 = bitcast %class.write_if* %tmp6 to void (%class.write_if*, i8)***
  %tmp8 = load void (%class.write_if*, i8)*** %tmp7, align 8, !tbaa !0
  %tmp9 = getelementptr inbounds void (%class.write_if*, i8)** %tmp8, i64 4
  %tmp10 = load void (%class.write_if*, i8)** %tmp9, align 8
  %tmp11 = getelementptr inbounds i8* %str.01, i64 1
  %tmp12 = load i8* %str.01, align 1, !tbaa !3
  tail call void %tmp10(%class.write_if* %tmp6, i8 signext %tmp12)
  %exitcond = icmp eq i8* %tmp11, getelementptr inbounds ([66 x i8]* @.str30, i64 0, i64 65)
  br i1 %exitcond, label %bb13, label %bb2

bb13:                                             ; preds = %_ZN7sc_core9sc_port_bI8write_ifEptEv.exit
  ret void
}

define linkonce_odr void @_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED1Ev(%"class.sc_core::sc_port"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port"* %this, i64 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI8write_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port"* %this, i64 0, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %class.write_if*** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %class.write_if** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %class.write_if** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit

_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit: ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port"* %this, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  ret void
}

define linkonce_odr void @_ZN8producerD1Ev(%class.producer* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %class.producer* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.producer* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %class.producer* %this, i64 0, i32 1
  %tmp3 = getelementptr inbounds %"class.sc_core::sc_port"* %tmp2, i64 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI8write_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %class.producer* %this, i64 0, i32 1, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp5 = load %class.write_if*** %tmp4, align 8, !tbaa !4
  %tmp6 = icmp eq %class.write_if** %tmp5, null
  br i1 %tmp6, label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i, label %bb7

bb7:                                              ; preds = %bb
  %tmp8 = bitcast %class.write_if** %tmp5 to i8*
  tail call void @_ZdlPv(i8* %tmp8) nounwind
  br label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i

_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i: ; preds = %bb7, %bb
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port"* %tmp2, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp9)
  %tmp10 = getelementptr inbounds %class.producer* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp10)
  ret void
}

define linkonce_odr void @_ZN8producerD0Ev(%class.producer* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %class.producer* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.producer* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %class.producer* %this, i64 0, i32 1
  %tmp3 = getelementptr inbounds %"class.sc_core::sc_port"* %tmp2, i64 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI8write_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %class.producer* %this, i64 0, i32 1, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp5 = load %class.write_if*** %tmp4, align 8, !tbaa !4
  %tmp6 = icmp eq %class.write_if** %tmp5, null
  br i1 %tmp6, label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i, label %bb7

bb7:                                              ; preds = %bb
  %tmp8 = bitcast %class.write_if** %tmp5 to i8*
  tail call void @_ZdlPv(i8* %tmp8) nounwind
  br label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i

_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i: ; preds = %bb7, %bb
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port"* %tmp2, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp9)
  %tmp10 = getelementptr inbounds %class.producer* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp10)
  %tmp11 = bitcast %class.producer* %this to i8*
  tail call void @_ZdlPv(i8* %tmp11) nounwind
  ret void
}

define linkonce_odr void @_ZThn40_N8producerD1Ev(%class.producer* %this) {
bb:
  %tmp = getelementptr inbounds %class.producer* %this, i64 -1, i32 1, i32 0, i32 0, i32 1
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 2) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"struct.sc_core::sc_bind_info"** %tmp, i64 5
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 15) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %"struct.sc_core::sc_bind_info"** %tmp, i64 23
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI8write_ifEE, i64 0, i64 2) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr inbounds %"struct.sc_core::sc_bind_info"** %tmp, i64 30
  %tmp4 = load %"struct.sc_core::sc_bind_info"** %tmp3, align 8
  %tmp5 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp4, null
  br i1 %tmp5, label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i, label %bb6

bb6:                                              ; preds = %bb
  %tmp7 = bitcast %"struct.sc_core::sc_bind_info"* %tmp4 to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  br label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i

_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i: ; preds = %bb6, %bb
  %tmp8 = bitcast %"struct.sc_core::sc_bind_info"** %tmp2 to %"class.sc_core::sc_port_base"*
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp8)
  %tmp9 = bitcast %"struct.sc_core::sc_bind_info"** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp9)
  ret void
}

define linkonce_odr void @_ZThn40_N8producerD0Ev(%class.producer* %this) {
bb:
  %tmp = getelementptr inbounds %class.producer* %this, i64 -1, i32 1, i32 0, i32 0, i32 1
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 2) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"struct.sc_core::sc_bind_info"** %tmp, i64 5
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV8producer, i64 0, i64 15) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr inbounds %"struct.sc_core::sc_bind_info"** %tmp, i64 23
  store %"struct.sc_core::sc_bind_info"* bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI8write_ifEE, i64 0, i64 2) to %"struct.sc_core::sc_bind_info"*), %"struct.sc_core::sc_bind_info"** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr inbounds %"struct.sc_core::sc_bind_info"** %tmp, i64 30
  %tmp4 = load %"struct.sc_core::sc_bind_info"** %tmp3, align 8
  %tmp5 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp4, null
  br i1 %tmp5, label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i.i, label %bb6

bb6:                                              ; preds = %bb
  %tmp7 = bitcast %"struct.sc_core::sc_bind_info"* %tmp4 to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  br label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i.i

_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i.i.i.i: ; preds = %bb6, %bb
  %tmp8 = bitcast %"struct.sc_core::sc_bind_info"** %tmp2 to %"class.sc_core::sc_port_base"*
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp8)
  %tmp9 = bitcast %"struct.sc_core::sc_bind_info"** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp9)
  %tmp10 = bitcast %"struct.sc_core::sc_bind_info"** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp10) nounwind
  ret void
}

define linkonce_odr void @_ZN7sc_core9sc_port_bI8write_ifED1Ev(%"class.sc_core::sc_port_b"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI8write_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %class.write_if*** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %class.write_if** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core9sc_port_bI8write_ifED2Ev.exit, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %class.write_if** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core9sc_port_bI8write_ifED2Ev.exit

_ZN7sc_core9sc_port_bI8write_ifED2Ev.exit:        ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  ret void
}

define linkonce_odr void @_ZN7sc_core9sc_port_bI8write_ifED0Ev(%"class.sc_core::sc_port_b"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI8write_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %class.write_if*** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %class.write_if** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core9sc_port_bI8write_ifED2Ev.exit.i, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %class.write_if** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core9sc_port_bI8write_ifED2Ev.exit.i

_ZN7sc_core9sc_port_bI8write_ifED2Ev.exit.i:      ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  %tmp7 = bitcast %"class.sc_core::sc_port_b"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  ret void
}

define linkonce_odr %"class.sc_core::sc_interface"* @_ZN7sc_core9sc_port_bI8write_ifE13get_interfaceEv(%"class.sc_core::sc_port_b"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 1
  %tmp1 = load %class.write_if** %tmp, align 8, !tbaa !4
  %tmp2 = icmp eq %class.write_if* %tmp1, null
  br i1 %tmp2, label %bb12, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %class.write_if* %tmp1 to i8**
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = getelementptr i8* %tmp5, i64 -48
  %tmp7 = bitcast i8* %tmp6 to i64*
  %tmp8 = load i64* %tmp7, align 8
  %tmp9 = bitcast %class.write_if* %tmp1 to i8*
  %tmp10 = getelementptr i8* %tmp9, i64 %tmp8
  %tmp11 = bitcast i8* %tmp10 to %"class.sc_core::sc_interface"*
  br label %bb12

bb12:                                             ; preds = %bb3, %bb
  %tmp13 = phi %"class.sc_core::sc_interface"* [ %tmp11, %bb3 ], [ null, %bb ]
  ret %"class.sc_core::sc_interface"* %tmp13
}

define linkonce_odr %"class.sc_core::sc_interface"* @_ZNK7sc_core9sc_port_bI8write_ifE13get_interfaceEv(%"class.sc_core::sc_port_b"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 1
  %tmp1 = load %class.write_if** %tmp, align 8, !tbaa !4
  %tmp2 = icmp eq %class.write_if* %tmp1, null
  br i1 %tmp2, label %bb12, label %bb3

bb3:                                              ; preds = %bb
  %tmp4 = bitcast %class.write_if* %tmp1 to i8**
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = getelementptr i8* %tmp5, i64 -48
  %tmp7 = bitcast i8* %tmp6 to i64*
  %tmp8 = load i64* %tmp7, align 8
  %tmp9 = bitcast %class.write_if* %tmp1 to i8*
  %tmp10 = getelementptr i8* %tmp9, i64 %tmp8
  %tmp11 = bitcast i8* %tmp10 to %"class.sc_core::sc_interface"*
  br label %bb12

bb12:                                             ; preds = %bb3, %bb
  %tmp13 = phi %"class.sc_core::sc_interface"* [ %tmp11, %bb3 ], [ null, %bb ]
  ret %"class.sc_core::sc_interface"* %tmp13
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bI8write_ifE5vbindERNS_12sc_interfaceE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_interface"* %interface_) uwtable align 2 {
bb:
  %tmp = icmp eq %"class.sc_core::sc_interface"* %interface_, null
  br i1 %tmp, label %.thread, label %bb1

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_interface"* %interface_ to i8*
  %tmp3 = tail call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI8write_if to i8*), i64 -1)
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

define linkonce_odr i32 @_ZN7sc_core9sc_port_bI8write_ifE5vbindERNS_12sc_port_baseE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_port_base"* %parent_) uwtable align 2 {
bb:
  %tmp = icmp eq %"class.sc_core::sc_port_base"* %parent_, null
  br i1 %tmp, label %.thread, label %bb1

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_port_base"* %parent_ to i8*
  %tmp3 = tail call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core12sc_port_baseE to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN7sc_core9sc_port_bI8write_ifEE to i8*), i64 -1)
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

define linkonce_odr void @_ZN7sc_core9sc_port_bI8write_ifE13add_interfaceEPNS_12sc_interfaceE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_interface"* %interface_) uwtable align 2 {
bb:
  %iface = alloca %class.write_if*, align 8
  %tmp = icmp eq %"class.sc_core::sc_interface"* %interface_, null
  br i1 %tmp, label %.thread, label %bb1

.thread:                                          ; preds = %bb
  store %class.write_if* null, %class.write_if** %iface, align 8, !tbaa !4
  br label %bb6

bb1:                                              ; preds = %bb
  %tmp2 = bitcast %"class.sc_core::sc_interface"* %interface_ to i8*
  %tmp3 = call i8* @__dynamic_cast(i8* %tmp2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core12sc_interfaceE to i8*), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64 }* @_ZTI8write_if to i8*), i64 -1)
  %tmp4 = bitcast i8* %tmp3 to %class.write_if*
  store %class.write_if* %tmp4, %class.write_if** %iface, align 8, !tbaa !4
  %tmp5 = icmp eq i8* %tmp3, null
  br i1 %tmp5, label %bb6, label %bb7

bb6:                                              ; preds = %bb1, %.thread
  call void @__assert_fail(i8* getelementptr inbounds ([11 x i8]* @.str20, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str19, i64 0, i64 0), i32 580, i8* getelementptr inbounds ([98 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core9sc_port_bI8write_ifE13add_interfaceEPNS_12sc_interfaceE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb7:                                              ; preds = %bb1
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp10 = load %class.write_if*** %tmp9, align 8, !tbaa !4
  %tmp11 = getelementptr inbounds %"class.std::vector.54"* %tmp8, i64 0, i32 0, i32 0, i32 0
  %tmp12 = load %class.write_if*** %tmp11, align 8, !tbaa !4
  %tmp13 = ptrtoint %class.write_if** %tmp10 to i64
  %tmp14 = ptrtoint %class.write_if** %tmp12 to i64
  %tmp15 = sub i64 %tmp13, %tmp14
  %tmp16 = lshr exact i64 %tmp15, 3
  %tmp17 = trunc i64 %tmp16 to i32
  %tmp18 = icmp sgt i32 %tmp17, 0
  br i1 %tmp18, label %.lr.ph, label %bb27

.lr.ph:                                           ; preds = %bb7
  %tmp19 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  br label %bb20

bb20:                                             ; preds = %._crit_edge2, %.lr.ph
  %tmp21 = phi %class.write_if** [ %tmp12, %.lr.ph ], [ %.pre3, %._crit_edge2 ]
  %indvars.iv = phi i64 [ 0, %.lr.ph ], [ %indvars.iv.next, %._crit_edge2 ]
  %tmp22 = getelementptr inbounds %class.write_if** %tmp21, i64 %indvars.iv
  %tmp23 = load %class.write_if** %tmp22, align 8, !tbaa !4
  %tmp24 = icmp eq %class.write_if* %tmp4, %tmp23
  br i1 %tmp24, label %bb25, label %bb26

bb25:                                             ; preds = %bb20
  call void @_ZNK7sc_core12sc_port_base12report_errorEPKcS2_(%"class.sc_core::sc_port_base"* %tmp19, i8* getelementptr inbounds ([0 x i8]* @_ZN7sc_core22SC_ID_BIND_IF_TO_PORT_E, i64 0, i64 0), i8* getelementptr inbounds ([32 x i8]* @.str21, i64 0, i64 0))
  br label %bb26

bb26:                                             ; preds = %bb25, %bb20
  %indvars.iv.next = add i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %tmp17
  br i1 %exitcond, label %._crit_edge, label %._crit_edge2

._crit_edge2:                                     ; preds = %bb26
  %.pre3 = load %class.write_if*** %tmp11, align 8, !tbaa !4
  br label %bb20

._crit_edge:                                      ; preds = %bb26
  %.pre = load %class.write_if*** %tmp9, align 8, !tbaa !4
  br label %bb27

bb27:                                             ; preds = %._crit_edge, %bb7
  %tmp28 = phi %class.write_if** [ %.pre, %._crit_edge ], [ %tmp10, %bb7 ]
  %tmp29 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 2
  %tmp30 = load %class.write_if*** %tmp29, align 8, !tbaa !4
  %tmp31 = icmp eq %class.write_if** %tmp28, %tmp30
  br i1 %tmp31, label %bb37, label %bb32

bb32:                                             ; preds = %bb27
  %tmp33 = icmp eq %class.write_if** %tmp28, null
  br i1 %tmp33, label %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit.i, label %bb34

bb34:                                             ; preds = %bb32
  store %class.write_if* %tmp4, %class.write_if** %tmp28, align 8, !tbaa !4
  %.pre.i = load %class.write_if*** %tmp9, align 8, !tbaa !4
  br label %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit.i

_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit.i: ; preds = %bb34, %bb32
  %tmp35 = phi %class.write_if** [ null, %bb32 ], [ %.pre.i, %bb34 ]
  %tmp36 = getelementptr inbounds %class.write_if** %tmp35, i64 1
  store %class.write_if** %tmp36, %class.write_if*** %tmp9, align 8, !tbaa !4
  br label %_ZNSt6vectorIP8write_ifSaIS1_EE9push_backERKS1_.exit

bb37:                                             ; preds = %bb27
  call void @_ZNSt6vectorIP8write_ifSaIS1_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS1_S3_EERKS1_(%"class.std::vector.54"* %tmp8, %class.write_if** %tmp28, %class.write_if** %iface)
  br label %_ZNSt6vectorIP8write_ifSaIS1_EE9push_backERKS1_.exit

_ZNSt6vectorIP8write_ifSaIS1_EE9push_backERKS1_.exit: ; preds = %bb37, %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit.i
  %tmp38 = load %class.write_if*** %tmp11, align 8, !tbaa !4
  %tmp39 = load %class.write_if** %tmp38, align 8, !tbaa !4
  %tmp40 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 1
  store %class.write_if* %tmp39, %class.write_if** %tmp40, align 8, !tbaa !4
  ret void
}

define linkonce_odr i32 @_ZN7sc_core9sc_port_bI8write_ifE15interface_countEv(%"class.sc_core::sc_port_b"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp1 = load %class.write_if*** %tmp, align 8, !tbaa !4
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp3 = load %class.write_if*** %tmp2, align 8, !tbaa !4
  %tmp4 = ptrtoint %class.write_if** %tmp1 to i64
  %tmp5 = ptrtoint %class.write_if** %tmp3 to i64
  %tmp6 = sub i64 %tmp4, %tmp5
  %tmp7 = lshr exact i64 %tmp6, 3
  %tmp8 = trunc i64 %tmp7 to i32
  ret i32 %tmp8
}

define linkonce_odr i8* @_ZNK7sc_core9sc_port_bI8write_ifE11if_typenameEv(%"class.sc_core::sc_port_b"* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @_ZTS8write_if, i64 0, i64 0)
}

define linkonce_odr void @_ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_thread_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0, i32 1
  %tmp2 = load %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb37

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp6 = load %class.write_if*** %tmp5, align 8, !tbaa !4
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp8 = load %class.write_if*** %tmp7, align 8, !tbaa !4
  %tmp9 = ptrtoint %class.write_if** %tmp6 to i64
  %tmp10 = ptrtoint %class.write_if** %tmp8 to i64
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
  %tmp18 = load %class.write_if*** %tmp7, align 8, !tbaa !4
  %tmp19 = getelementptr inbounds %class.write_if** %tmp18, i64 %indvars.iv
  %tmp20 = load %class.write_if** %tmp19, align 8, !tbaa !4
  %tmp21 = icmp eq %class.write_if* %tmp20, null
  br i1 %tmp21, label %bb22, label %bb23

bb22:                                             ; preds = %bb17
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str19, i64 0, i64 0), i32 627, i8* getelementptr inbounds ([126 x i8]* @__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_thread_processEPNS_15sc_event_finderE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb23:                                             ; preds = %bb17
  %tmp24 = bitcast %class.write_if* %tmp20 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -48
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %class.write_if* %tmp20 to i8*
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

define linkonce_odr void @_ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE(%"class.sc_core::sc_port_b"* %this, %"class.sc_core::sc_method_process"* %handle_p, %"class.sc_core::sc_event_finder"* %event_finder_) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 0, i32 1
  %tmp2 = load %"struct.sc_core::sc_bind_info"** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %"struct.sc_core::sc_bind_info"* %tmp2, null
  br i1 %tmp3, label %bb4, label %bb37

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 1
  %tmp6 = load %class.write_if*** %tmp5, align 8, !tbaa !4
  %tmp7 = getelementptr inbounds %"class.sc_core::sc_port_b"* %this, i64 0, i32 2, i32 0, i32 0, i32 0
  %tmp8 = load %class.write_if*** %tmp7, align 8, !tbaa !4
  %tmp9 = ptrtoint %class.write_if** %tmp6 to i64
  %tmp10 = ptrtoint %class.write_if** %tmp8 to i64
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
  %tmp18 = load %class.write_if*** %tmp7, align 8, !tbaa !4
  %tmp19 = getelementptr inbounds %class.write_if** %tmp18, i64 %indvars.iv
  %tmp20 = load %class.write_if** %tmp19, align 8, !tbaa !4
  %tmp21 = icmp eq %class.write_if* %tmp20, null
  br i1 %tmp21, label %bb22, label %bb23

bb22:                                             ; preds = %bb17
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8]* @.str19, i64 0, i64 0), i32 648, i8* getelementptr inbounds ([126 x i8]* @__PRETTY_FUNCTION__._ZNK7sc_core9sc_port_bI8write_ifE14make_sensitiveEPNS_17sc_method_processEPNS_15sc_event_finderE, i64 0, i64 0)) noreturn nounwind
  unreachable

bb23:                                             ; preds = %bb17
  %tmp24 = bitcast %class.write_if* %tmp20 to i8**
  %tmp25 = load i8** %tmp24, align 8, !tbaa !0
  %tmp26 = getelementptr i8* %tmp25, i64 -48
  %tmp27 = bitcast i8* %tmp26 to i64*
  %tmp28 = load i64* %tmp27, align 8
  %tmp29 = bitcast %class.write_if* %tmp20 to i8*
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

define linkonce_odr void @_ZNSt6vectorIP8write_ifSaIS1_EE13_M_insert_auxEN9__gnu_cxx17__normal_iteratorIPS1_S3_EERKS1_(%"class.std::vector.54"* nocapture %this, %class.write_if** %__position.coerce, %class.write_if** nocapture %__x) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.std::vector.54"* %this, i64 0, i32 0, i32 0, i32 1
  %tmp1 = load %class.write_if*** %tmp, align 8, !tbaa !4
  %tmp2 = getelementptr inbounds %"class.std::vector.54"* %this, i64 0, i32 0, i32 0, i32 2
  %tmp3 = load %class.write_if*** %tmp2, align 8, !tbaa !4
  %tmp4 = icmp eq %class.write_if** %tmp1, %tmp3
  br i1 %tmp4, label %_ZNKSt6vectorIP8write_ifSaIS1_EE12_M_check_lenEmPKc.exit, label %bb5

bb5:                                              ; preds = %bb
  %tmp6 = icmp eq %class.write_if** %tmp1, null
  br i1 %tmp6, label %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit, label %bb7

bb7:                                              ; preds = %bb5
  %tmp8 = getelementptr inbounds %class.write_if** %tmp1, i64 -1
  %tmp9 = load %class.write_if** %tmp8, align 8, !tbaa !4
  store %class.write_if* %tmp9, %class.write_if** %tmp1, align 8, !tbaa !4
  %.pre = load %class.write_if*** %tmp, align 8, !tbaa !4
  br label %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit

_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit: ; preds = %bb7, %bb5
  %tmp10 = phi %class.write_if** [ null, %bb5 ], [ %.pre, %bb7 ]
  %tmp11 = getelementptr inbounds %class.write_if** %tmp10, i64 1
  store %class.write_if** %tmp11, %class.write_if*** %tmp, align 8, !tbaa !4
  %tmp12 = load %class.write_if** %__x, align 8, !tbaa !4
  %tmp13 = getelementptr inbounds %class.write_if** %tmp10, i64 -1
  %tmp14 = ptrtoint %class.write_if** %tmp13 to i64
  %tmp15 = ptrtoint %class.write_if** %__position.coerce to i64
  %tmp16 = sub i64 %tmp14, %tmp15
  %tmp17 = ashr exact i64 %tmp16, 3
  %tmp18 = icmp eq i64 %tmp17, 0
  br i1 %tmp18, label %_ZSt13copy_backwardIPP8write_ifS2_ET0_T_S4_S3_.exit, label %bb19

bb19:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit
  %.pre.i.i.i.i = sub i64 0, %tmp17
  %.pre1.i.i.i.i = getelementptr inbounds %class.write_if** %tmp10, i64 %.pre.i.i.i.i
  %tmp20 = bitcast %class.write_if** %.pre1.i.i.i.i to i8*
  %tmp21 = bitcast %class.write_if** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp20, i8* %tmp21, i64 %tmp16, i32 8, i1 false) nounwind
  br label %_ZSt13copy_backwardIPP8write_ifS2_ET0_T_S4_S3_.exit

_ZSt13copy_backwardIPP8write_ifS2_ET0_T_S4_S3_.exit: ; preds = %bb19, %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit
  store %class.write_if* %tmp12, %class.write_if** %__position.coerce, align 8, !tbaa !4
  br label %bb67

_ZNKSt6vectorIP8write_ifSaIS1_EE12_M_check_lenEmPKc.exit: ; preds = %bb
  %tmp22 = getelementptr inbounds %"class.std::vector.54"* %this, i64 0, i32 0, i32 0, i32 0
  %tmp23 = load %class.write_if*** %tmp22, align 8, !tbaa !4
  %tmp24 = ptrtoint %class.write_if** %tmp1 to i64
  %tmp25 = ptrtoint %class.write_if** %tmp23 to i64
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
  br i1 %tmp34, label %_ZNSt12_Vector_baseIP8write_ifSaIS1_EE11_M_allocateEm.exit, label %bb35

bb35:                                             ; preds = %_ZNKSt6vectorIP8write_ifSaIS1_EE12_M_check_lenEmPKc.exit
  %tmp36 = icmp ugt i64 %tmp33, 2305843009213693951
  br i1 %tmp36, label %bb37, label %_ZN9__gnu_cxx13new_allocatorIP8write_ifE8allocateEmPKv.exit.i

bb37:                                             ; preds = %bb35
  tail call void @_ZSt17__throw_bad_allocv() noreturn
  unreachable

_ZN9__gnu_cxx13new_allocatorIP8write_ifE8allocateEmPKv.exit.i: ; preds = %bb35
  %tmp38 = shl i64 %tmp33, 3
  %tmp39 = tail call noalias i8* @_Znwm(i64 %tmp38)
  %tmp40 = bitcast i8* %tmp39 to %class.write_if**
  br label %_ZNSt12_Vector_baseIP8write_ifSaIS1_EE11_M_allocateEm.exit

_ZNSt12_Vector_baseIP8write_ifSaIS1_EE11_M_allocateEm.exit: ; preds = %_ZN9__gnu_cxx13new_allocatorIP8write_ifE8allocateEmPKv.exit.i, %_ZNKSt6vectorIP8write_ifSaIS1_EE12_M_check_lenEmPKc.exit
  %tmp41 = phi %class.write_if** [ %tmp40, %_ZN9__gnu_cxx13new_allocatorIP8write_ifE8allocateEmPKv.exit.i ], [ null, %_ZNKSt6vectorIP8write_ifSaIS1_EE12_M_check_lenEmPKc.exit ]
  %tmp42 = ptrtoint %class.write_if** %__position.coerce to i64
  %tmp43 = sub i64 %tmp42, %tmp25
  %tmp44 = ashr exact i64 %tmp43, 3
  %tmp45 = getelementptr inbounds %class.write_if** %tmp41, i64 %tmp44
  %tmp46 = icmp eq %class.write_if** %tmp45, null
  br i1 %tmp46, label %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit3, label %bb47

bb47:                                             ; preds = %_ZNSt12_Vector_baseIP8write_ifSaIS1_EE11_M_allocateEm.exit
  %tmp48 = load %class.write_if** %__x, align 8, !tbaa !4
  store %class.write_if* %tmp48, %class.write_if** %tmp45, align 8, !tbaa !4
  br label %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit3

_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit3: ; preds = %bb47, %_ZNSt12_Vector_baseIP8write_ifSaIS1_EE11_M_allocateEm.exit
  %tmp49 = icmp eq i64 %tmp44, 0
  br i1 %tmp49, label %bb53, label %bb50

bb50:                                             ; preds = %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit3
  %tmp51 = bitcast %class.write_if** %tmp41 to i8*
  %tmp52 = bitcast %class.write_if** %tmp23 to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp51, i8* %tmp52, i64 %tmp43, i32 8, i1 false) nounwind
  br label %bb53

bb53:                                             ; preds = %bb50, %_ZN9__gnu_cxx13new_allocatorIP8write_ifE9constructEPS2_RKS2_.exit3
  %.sum = add i64 %tmp44, 1
  %tmp54 = sub i64 %tmp24, %tmp42
  %tmp55 = ashr exact i64 %tmp54, 3
  %tmp56 = icmp eq i64 %tmp55, 0
  br i1 %tmp56, label %bb61, label %bb57

bb57:                                             ; preds = %bb53
  %tmp58 = getelementptr inbounds %class.write_if** %tmp41, i64 %.sum
  %tmp59 = bitcast %class.write_if** %tmp58 to i8*
  %tmp60 = bitcast %class.write_if** %__position.coerce to i8*
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %tmp59, i8* %tmp60, i64 %tmp54, i32 8, i1 false) nounwind
  br label %bb61

bb61:                                             ; preds = %bb57, %bb53
  %tmp62 = icmp eq %class.write_if** %tmp23, null
  br i1 %tmp62, label %_ZNSt12_Vector_baseIP8write_ifSaIS1_EE13_M_deallocateEPS1_m.exit1, label %bb63

bb63:                                             ; preds = %bb61
  %tmp64 = bitcast %class.write_if** %tmp23 to i8*
  tail call void @_ZdlPv(i8* %tmp64) nounwind
  br label %_ZNSt12_Vector_baseIP8write_ifSaIS1_EE13_M_deallocateEPS1_m.exit1

_ZNSt12_Vector_baseIP8write_ifSaIS1_EE13_M_deallocateEPS1_m.exit1: ; preds = %bb63, %bb61
  %.sum4 = add i64 %tmp55, %.sum
  %tmp65 = getelementptr inbounds %class.write_if** %tmp41, i64 %.sum4
  store %class.write_if** %tmp41, %class.write_if*** %tmp22, align 8, !tbaa !4
  store %class.write_if** %tmp65, %class.write_if*** %tmp, align 8, !tbaa !4
  %tmp66 = getelementptr inbounds %class.write_if** %tmp41, i64 %tmp33
  store %class.write_if** %tmp66, %class.write_if*** %tmp2, align 8, !tbaa !4
  br label %bb67

bb67:                                             ; preds = %_ZNSt12_Vector_baseIP8write_ifSaIS1_EE13_M_deallocateEPS1_m.exit1, %_ZSt13copy_backwardIPP8write_ifS2_ET0_T_S4_S3_.exit
  ret void
}

define linkonce_odr i8* @_ZNK7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EE4kindEv(%"class.sc_core::sc_port"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([8 x i8]* @.str29, i64 0, i64 0)
}

define linkonce_odr void @_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED0Ev(%"class.sc_core::sc_port"* %this) unnamed_addr uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %"class.sc_core::sc_port"* %this, i64 0, i32 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([22 x i8*]* @_ZTVN7sc_core9sc_port_bI8write_ifEE, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr inbounds %"class.sc_core::sc_port"* %this, i64 0, i32 0, i32 2, i32 0, i32 0, i32 0
  %tmp2 = load %class.write_if*** %tmp1, align 8, !tbaa !4
  %tmp3 = icmp eq %class.write_if** %tmp2, null
  br i1 %tmp3, label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = bitcast %class.write_if** %tmp2 to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  br label %_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i

_ZN7sc_core7sc_portI8write_ifLi1ELNS_14sc_port_policyE0EED2Ev.exit.i: ; preds = %bb4, %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_port"* %this, i64 0, i32 0, i32 0
  tail call void @_ZN7sc_core12sc_port_baseD2Ev(%"class.sc_core::sc_port_base"* %tmp6)
  %tmp7 = bitcast %"class.sc_core::sc_port"* %this to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  ret void
}

declare void @_ZN7sc_core12sc_interfaceC2Ev(%"class.sc_core::sc_interface"*)

define linkonce_odr void @_ZN4fifoD1Ev(%class.fifo* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7sc_core8sc_eventD1Ev.exit.i:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.fifo* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr %class.fifo* %this, i64 0, i32 1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to i32 (...)**), i32 (...)*** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr %class.fifo* %this, i64 0, i32 2, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to i32 (...)**), i32 (...)*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %class.fifo* %this, i64 0, i32 7
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp4)
  %tmp5 = getelementptr inbounds %class.fifo* %this, i64 0, i32 6
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp5)
  %tmp6 = getelementptr inbounds %class.fifo* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp6)
  %tmp7 = getelementptr inbounds %class.fifo* %this, i64 0, i32 1, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp7)
  ret void
}

define linkonce_odr void @_ZN4fifoD0Ev(%class.fifo* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7sc_core8sc_eventD1Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %class.fifo* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr %class.fifo* %this, i64 0, i32 1, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to i32 (...)**), i32 (...)*** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr %class.fifo* %this, i64 0, i32 2, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to i32 (...)**), i32 (...)*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %class.fifo* %this, i64 0, i32 7
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp4)
  %tmp5 = getelementptr inbounds %class.fifo* %this, i64 0, i32 6
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp5)
  %tmp6 = getelementptr inbounds %class.fifo* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp6)
  %tmp7 = getelementptr inbounds %class.fifo* %this, i64 0, i32 1, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp7)
  %tmp8 = bitcast %class.fifo* %this to i8*
  tail call void @_ZdlPv(i8* %tmp8) nounwind
  ret void
}

define linkonce_odr void @_ZN4fifo5writeEc(%class.fifo* %this, i8 signext %c) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 0, i32 4
  %tmp1 = load i32* %tmp, align 4, !tbaa !7
  %tmp2 = icmp eq i32 %tmp1, 10
  br i1 %tmp2, label %bb3, label %bb7

bb3:                                              ; preds = %bb
  %tmp4 = getelementptr inbounds %class.fifo* %this, i64 0, i32 7
  %tmp5 = getelementptr inbounds %class.fifo* %this, i64 0, i32 0, i32 0, i32 1
  %tmp6 = load %"class.sc_core::sc_simcontext"** %tmp5, align 8, !tbaa !4
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp4, %"class.sc_core::sc_simcontext"* %tmp6)
  %.pre = load i32* %tmp, align 4, !tbaa !7
  br label %bb7

bb7:                                              ; preds = %bb3, %bb
  %tmp8 = phi i32 [ %.pre, %bb3 ], [ %tmp1, %bb ]
  %tmp9 = getelementptr inbounds %class.fifo* %this, i64 0, i32 5
  %tmp10 = load i32* %tmp9, align 4, !tbaa !7
  %tmp11 = add nsw i32 %tmp8, %tmp10
  %tmp12 = srem i32 %tmp11, 10
  %tmp13 = sext i32 %tmp12 to i64
  %tmp14 = getelementptr inbounds %class.fifo* %this, i64 0, i32 3, i64 %tmp13
  store i8 %c, i8* %tmp14, align 1, !tbaa !3
  %tmp15 = load i32* %tmp, align 4, !tbaa !7
  %tmp16 = add nsw i32 %tmp15, 1
  store i32 %tmp16, i32* %tmp, align 4, !tbaa !7
  %tmp17 = getelementptr inbounds %class.fifo* %this, i64 0, i32 6
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp17)
  ret void
}

define linkonce_odr void @_ZN4fifo4readERc(%class.fifo* %this, i8* nocapture %c) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 0, i32 4
  %tmp1 = load i32* %tmp, align 4, !tbaa !7
  %tmp2 = icmp eq i32 %tmp1, 0
  br i1 %tmp2, label %bb3, label %bb7

bb3:                                              ; preds = %bb
  %tmp4 = getelementptr inbounds %class.fifo* %this, i64 0, i32 6
  %tmp5 = getelementptr inbounds %class.fifo* %this, i64 0, i32 0, i32 0, i32 1
  %tmp6 = load %"class.sc_core::sc_simcontext"** %tmp5, align 8, !tbaa !4
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp4, %"class.sc_core::sc_simcontext"* %tmp6)
  br label %bb7

bb7:                                              ; preds = %bb3, %bb
  %tmp8 = getelementptr inbounds %class.fifo* %this, i64 0, i32 5
  %tmp9 = load i32* %tmp8, align 4, !tbaa !7
  %tmp10 = sext i32 %tmp9 to i64
  %tmp11 = getelementptr inbounds %class.fifo* %this, i64 0, i32 3, i64 %tmp10
  %tmp12 = load i8* %tmp11, align 1, !tbaa !3
  store i8 %tmp12, i8* %c, align 1, !tbaa !3
  %tmp13 = load i32* %tmp, align 4, !tbaa !7
  %tmp14 = add nsw i32 %tmp13, -1
  store i32 %tmp14, i32* %tmp, align 4, !tbaa !7
  %tmp15 = load i32* %tmp8, align 4, !tbaa !7
  %tmp16 = add nsw i32 %tmp15, 1
  %tmp17 = srem i32 %tmp16, 10
  store i32 %tmp17, i32* %tmp8, align 4, !tbaa !7
  %tmp18 = getelementptr inbounds %class.fifo* %this, i64 0, i32 7
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp18)
  ret void
}

define linkonce_odr void @_ZN4fifo5resetEv(%class.fifo* nocapture %this) nounwind uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 0, i32 5
  store i32 0, i32* %tmp, align 4, !tbaa !7
  %tmp1 = getelementptr inbounds %class.fifo* %this, i64 0, i32 4
  store i32 0, i32* %tmp1, align 4, !tbaa !7
  ret void
}

define linkonce_odr i32 @_ZN4fifo13num_availableEv(%class.fifo* nocapture %this) nounwind uwtable readonly align 2 {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 0, i32 4
  %tmp1 = load i32* %tmp, align 4, !tbaa !7
  ret i32 %tmp1
}

define linkonce_odr void @_ZThn40_N4fifoD1Ev(%class.fifo* %this) {
_ZN7sc_core8sc_eventD1Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 7, i32 6, i32 0, i32 0, i32 1
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 23
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 24
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 43
  %tmp5 = bitcast %"class.sc_core::sc_thread_process"*** %tmp4 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp5)
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 28
  %tmp7 = bitcast %"class.sc_core::sc_thread_process"*** %tmp6 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp7)
  %tmp8 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp8)
  %tmp9 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp9)
  ret void
}

define linkonce_odr void @_ZThn40_N4fifoD0Ev(%class.fifo* %this) {
_ZN7sc_core8sc_eventD1Ev.exit.i.i.i:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 7, i32 6, i32 0, i32 0, i32 1
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 23
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 24
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 43
  %tmp5 = bitcast %"class.sc_core::sc_thread_process"*** %tmp4 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp5)
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 28
  %tmp7 = bitcast %"class.sc_core::sc_thread_process"*** %tmp6 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp7)
  %tmp8 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp8)
  %tmp9 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp9)
  %tmp10 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp10) nounwind
  ret void
}

define linkonce_odr void @_ZThn184_N4fifoD1Ev(%class.fifo* %this) {
_ZN7sc_core8sc_eventD1Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5, i32 0, i32 0, i32 1
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"class.sc_core::sc_method_process"*** %tmp, i64 5
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr %"class.sc_core::sc_method_process"*** %tmp, i64 23
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr %"class.sc_core::sc_method_process"*** %tmp, i64 24
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 43
  %tmp5 = bitcast %"class.sc_core::sc_method_process"*** %tmp4 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp5)
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 28
  %tmp7 = bitcast %"class.sc_core::sc_method_process"*** %tmp6 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp7)
  %tmp8 = bitcast %"class.sc_core::sc_method_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp8)
  %tmp9 = bitcast %"class.sc_core::sc_method_process"*** %tmp2 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp9)
  ret void
}

define linkonce_odr void @_ZThn184_N4fifoD0Ev(%class.fifo* %this) {
_ZN7sc_core8sc_eventD1Ev.exit.i.i.i:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5, i32 0, i32 0, i32 1
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp, align 8, !tbaa !0
  %tmp1 = getelementptr %"class.sc_core::sc_method_process"*** %tmp, i64 5
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr %"class.sc_core::sc_method_process"*** %tmp, i64 23
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr %"class.sc_core::sc_method_process"*** %tmp, i64 24
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 43
  %tmp5 = bitcast %"class.sc_core::sc_method_process"*** %tmp4 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp5)
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 28
  %tmp7 = bitcast %"class.sc_core::sc_method_process"*** %tmp6 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp7)
  %tmp8 = bitcast %"class.sc_core::sc_method_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp8)
  %tmp9 = bitcast %"class.sc_core::sc_method_process"*** %tmp2 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp9)
  %tmp10 = bitcast %"class.sc_core::sc_method_process"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp10) nounwind
  ret void
}

define linkonce_odr void @_ZThn184_N4fifo5writeEc(%class.fifo* %this, i8 signext %c) {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5, i32 0, i32 0, i32 1
  %tmp1 = bitcast %"class.sc_core::sc_method_process"*** %tmp to %class.fifo*
  %tmp2 = getelementptr inbounds %class.fifo* %tmp1, i64 0, i32 4
  %tmp3 = load i32* %tmp2, align 4, !tbaa !7
  %tmp4 = icmp eq i32 %tmp3, 10
  br i1 %tmp4, label %bb5, label %_ZN4fifo5writeEc.exit

bb5:                                              ; preds = %bb
  %tmp6 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 43
  %tmp7 = bitcast %"class.sc_core::sc_method_process"*** %tmp6 to %"class.sc_core::sc_event"*
  %tmp8 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 1
  %tmp9 = load %"class.sc_core::sc_method_process"*** %tmp8, align 8
  %tmp10 = bitcast %"class.sc_core::sc_method_process"** %tmp9 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp7, %"class.sc_core::sc_simcontext"* %tmp10)
  %.pre.i = load i32* %tmp2, align 4, !tbaa !7
  br label %_ZN4fifo5writeEc.exit

_ZN4fifo5writeEc.exit:                            ; preds = %bb5, %bb
  %tmp11 = phi i32 [ %.pre.i, %bb5 ], [ %tmp3, %bb ]
  %tmp12 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 27
  %tmp13 = bitcast %"class.sc_core::sc_method_process"*** %tmp12 to i32*
  %tmp14 = load i32* %tmp13, align 4, !tbaa !7
  %tmp15 = add nsw i32 %tmp14, %tmp11
  %tmp16 = srem i32 %tmp15, 10
  %tmp17 = sext i32 %tmp16 to i64
  %tmp18 = getelementptr inbounds %class.fifo* %tmp1, i64 0, i32 3, i64 %tmp17
  store i8 %c, i8* %tmp18, align 1, !tbaa !3
  %tmp19 = load i32* %tmp2, align 4, !tbaa !7
  %tmp20 = add nsw i32 %tmp19, 1
  store i32 %tmp20, i32* %tmp2, align 4, !tbaa !7
  %tmp21 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 28
  %tmp22 = bitcast %"class.sc_core::sc_method_process"*** %tmp21 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp22)
  ret void
}

define linkonce_odr void @_ZThn184_N4fifo5resetEv(%class.fifo* nocapture %this) nounwind {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5, i32 0, i32 0, i32 1
  %tmp1 = bitcast %"class.sc_core::sc_method_process"*** %tmp to %class.fifo*
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_method_process"*** %tmp, i64 27
  %tmp3 = bitcast %"class.sc_core::sc_method_process"*** %tmp2 to i32*
  store i32 0, i32* %tmp3, align 4, !tbaa !7
  %tmp4 = getelementptr inbounds %class.fifo* %tmp1, i64 0, i32 4
  store i32 0, i32* %tmp4, align 4, !tbaa !7
  ret void
}

define linkonce_odr void @_ZThn192_N4fifoD1Ev(%class.fifo* %this) {
_ZN7sc_core8sc_eventD1Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5
  %tmp1 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 0, i32 0, i32 0, i32 0
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr %"class.std::vector.20"* %tmp, i64 1, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr %"class.std::vector.20"* %tmp, i64 7, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 8, i32 0, i32 0, i32 0
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp4, align 8, !tbaa !0
  %tmp5 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 14, i32 0, i32 0, i32 1
  %tmp6 = bitcast %"class.sc_core::sc_method_process"*** %tmp5 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp6)
  %tmp7 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 9, i32 0, i32 0, i32 1
  %tmp8 = bitcast %"class.sc_core::sc_method_process"*** %tmp7 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp8)
  %tmp9 = bitcast %"class.std::vector.20"* %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp9)
  %tmp10 = bitcast %"class.sc_core::sc_method_process"*** %tmp3 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp10)
  ret void
}

define linkonce_odr void @_ZThn192_N4fifoD0Ev(%class.fifo* %this) {
_ZN7sc_core8sc_eventD1Ev.exit.i.i.i:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5
  %tmp1 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 0, i32 0, i32 0, i32 0
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 3) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp1, align 8, !tbaa !0
  %tmp2 = getelementptr %"class.std::vector.20"* %tmp, i64 1, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 20) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp2, align 8, !tbaa !0
  %tmp3 = getelementptr %"class.std::vector.20"* %tmp, i64 7, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 28) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp3, align 8, !tbaa !0
  %tmp4 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 8, i32 0, i32 0, i32 0
  store %"class.sc_core::sc_method_process"** bitcast (i8** getelementptr inbounds ([46 x i8*]* @_ZTV4fifo, i64 0, i64 40) to %"class.sc_core::sc_method_process"**), %"class.sc_core::sc_method_process"*** %tmp4, align 8, !tbaa !0
  %tmp5 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 14, i32 0, i32 0, i32 1
  %tmp6 = bitcast %"class.sc_core::sc_method_process"*** %tmp5 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp6)
  %tmp7 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 9, i32 0, i32 0, i32 1
  %tmp8 = bitcast %"class.sc_core::sc_method_process"*** %tmp7 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp8)
  %tmp9 = bitcast %"class.std::vector.20"* %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp9)
  %tmp10 = bitcast %"class.sc_core::sc_method_process"*** %tmp3 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp10)
  %tmp11 = bitcast %"class.std::vector.20"* %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp11) nounwind
  ret void
}

define linkonce_odr void @_ZThn192_N4fifo4readERc(%class.fifo* %this, i8* nocapture %c) {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5
  %tmp1 = bitcast %"class.std::vector.20"* %tmp to %class.fifo*
  %tmp2 = getelementptr inbounds %class.fifo* %tmp1, i64 0, i32 4
  %tmp3 = load i32* %tmp2, align 4, !tbaa !7
  %tmp4 = icmp eq i32 %tmp3, 0
  br i1 %tmp4, label %bb5, label %_ZN4fifo4readERc.exit

bb5:                                              ; preds = %bb
  %tmp6 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 9, i32 0, i32 0, i32 1
  %tmp7 = bitcast %"class.sc_core::sc_method_process"*** %tmp6 to %"class.sc_core::sc_event"*
  %tmp8 = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5, i32 0, i32 0, i32 1
  %tmp9 = load %"class.sc_core::sc_method_process"*** %tmp8, align 8
  %tmp10 = bitcast %"class.sc_core::sc_method_process"** %tmp9 to %"class.sc_core::sc_simcontext"*
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp7, %"class.sc_core::sc_simcontext"* %tmp10)
  br label %_ZN4fifo4readERc.exit

_ZN4fifo4readERc.exit:                            ; preds = %bb5, %bb
  %tmp11 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 9
  %tmp12 = bitcast %"class.std::vector.20"* %tmp11 to i32*
  %tmp13 = load i32* %tmp12, align 4, !tbaa !7
  %tmp14 = sext i32 %tmp13 to i64
  %tmp15 = getelementptr inbounds %class.fifo* %tmp1, i64 0, i32 3, i64 %tmp14
  %tmp16 = load i8* %tmp15, align 1, !tbaa !3
  store i8 %tmp16, i8* %c, align 1, !tbaa !3
  %tmp17 = load i32* %tmp2, align 4, !tbaa !7
  %tmp18 = add nsw i32 %tmp17, -1
  store i32 %tmp18, i32* %tmp2, align 4, !tbaa !7
  %tmp19 = load i32* %tmp12, align 4, !tbaa !7
  %tmp20 = add nsw i32 %tmp19, 1
  %tmp21 = srem i32 %tmp20, 10
  store i32 %tmp21, i32* %tmp12, align 4, !tbaa !7
  %tmp22 = getelementptr inbounds %"class.std::vector.20"* %tmp, i64 14, i32 0, i32 0, i32 1
  %tmp23 = bitcast %"class.sc_core::sc_method_process"*** %tmp22 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp23)
  ret void
}

define linkonce_odr i32 @_ZThn192_N4fifo13num_availableEv(%class.fifo* nocapture %this) nounwind readonly {
bb:
  %tmp = getelementptr inbounds %class.fifo* %this, i64 -1, i32 6, i32 5
  %tmp1 = bitcast %"class.std::vector.20"* %tmp to %class.fifo*
  %tmp2 = getelementptr inbounds %class.fifo* %tmp1, i64 0, i32 4
  %tmp3 = load i32* %tmp2, align 4, !tbaa !7
  ret i32 %tmp3
}

define linkonce_odr void @_ZN8write_ifD1Ev(%class.write_if* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %class.write_if* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  ret void
}

define linkonce_odr void @_ZN8write_ifD0Ev(%class.write_if* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN8write_ifD1Ev.exit:
  %tmp = getelementptr inbounds %class.write_if* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  %tmp1 = bitcast %class.write_if* %this to i8*
  tail call void @_ZdlPv(i8* %tmp1) nounwind
  ret void
}

define linkonce_odr void @_ZN7read_ifD1Ev(%class.read_if* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %class.read_if* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  ret void
}

define linkonce_odr void @_ZN7read_ifD0Ev(%class.read_if* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7read_ifD1Ev.exit:
  %tmp = getelementptr inbounds %class.read_if* %this, i64 0, i32 0
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp)
  %tmp1 = bitcast %class.read_if* %this to i8*
  tail call void @_ZdlPv(i8* %tmp1) nounwind
  ret void
}

define linkonce_odr void @_ZTv0_n40_N7read_ifD1Ev(%class.read_if* %this) {
bb:
  %tmp = bitcast %class.read_if* %this to i8*
  %tmp1 = bitcast %class.read_if* %this to i8**
  %tmp2 = load i8** %tmp1, align 8
  %tmp3 = getelementptr inbounds i8* %tmp2, i64 -40
  %tmp4 = bitcast i8* %tmp3 to i64*
  %tmp5 = load i64* %tmp4, align 8
  %tmp6 = getelementptr inbounds i8* %tmp, i64 %tmp5
  %tmp7 = bitcast i8* %tmp6 to %"class.sc_core::sc_interface"*
  tail call void @_ZN7sc_core12sc_interfaceD2Ev(%"class.sc_core::sc_interface"* %tmp7)
  ret void
}

define linkonce_odr void @_ZTv0_n40_N7read_ifD0Ev(%class.read_if* %this) {
_ZN7read_ifD0Ev.exit:
  %tmp = bitcast %class.read_if* %this to i8*
  %tmp1 = bitcast %class.read_if* %this to i8**
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

declare void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"*)

declare void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"*, %"class.sc_core::sc_simcontext"*)

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

!0 = metadata !{metadata !"vtable pointer", metadata !1}
!1 = metadata !{metadata !"Simple C/C++ TBAA"}
!2 = metadata !{metadata !"_ZTSSt12_Ios_Iostate", metadata !3}
!3 = metadata !{metadata !"omnipotent char", metadata !1}
!4 = metadata !{metadata !"any pointer", metadata !3}
!5 = metadata !{metadata !"long", metadata !3}
!6 = metadata !{metadata !"branch_weights", i32 64, i32 4}
!7 = metadata !{metadata !"int", metadata !3}
!8 = metadata !{metadata !"_ZTSN7sc_core8sc_event8notify_tE", metadata !3}
