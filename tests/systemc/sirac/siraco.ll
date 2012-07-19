; ModuleID = 'siraco.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%"class.std::ios_base::Init" = type { i8 }
%"class.sc_core::sc_api_version_2_2_0" = type { i8 }
%class.element = type { %"class.sc_core::sc_module", %"class.sc_core::sc_event", %"class.sc_core::sc_event", i32, i32 }
%"class.sc_core::sc_module" = type { %"class.sc_core::sc_object", %"class.sc_core::sc_process_host", %"class.sc_core::sc_sensitive", %"class.sc_core::sc_sensitive_pos", %"class.sc_core::sc_sensitive_neg", i8, %"class.std::vector"*, i32, %"class.sc_core::sc_name_gen"*, %"class.std::vector.10", %"class.sc_core::sc_module_name"* }
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
%"class.sc_core::sc_process_b" = type { %"class.sc_core::sc_object", i8*, i32, i32, %"class.std::vector.10", i8, i8, %"class.sc_core::sc_event"*, i32, %"class.sc_core::sc_event_list"*, %"class.sc_core::sc_process_b"*, i8, %"class.sc_core::sc_report"*, %"class.sc_core::sc_name_gen"*, i32, i32, i8, %"class.sc_core::sc_reset"*, %"class.sc_core::sc_process_b"*, %"class.sc_core::sc_process_host"*, { i64, i64 }, %"class.std::vector.43", %"class.sc_core::sc_event"*, i32, i8, %"class.sc_core::sc_event"*, i32, i8 }
%"class.std::vector.10" = type { %"struct.std::_Vector_base.11" }
%"struct.std::_Vector_base.11" = type { %"struct.std::_Vector_base<sc_core::sc_object *, std::allocator<sc_core::sc_object *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_object *, std::allocator<sc_core::sc_object *> >::_Vector_impl" = type { %"class.sc_core::sc_object"**, %"class.sc_core::sc_object"**, %"class.sc_core::sc_object"** }
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
%"class.std::vector.15" = type { %"struct.std::_Vector_base.16" }
%"struct.std::_Vector_base.16" = type { %"struct.std::_Vector_base<sc_core::sc_event *, std::allocator<sc_core::sc_event *> >::_Vector_impl" }
%"struct.std::_Vector_base<sc_core::sc_event *, std::allocator<sc_core::sc_event *> >::_Vector_impl" = type { %"class.sc_core::sc_event"**, %"class.sc_core::sc_event"**, %"class.sc_core::sc_event"** }
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
%"class.sc_core::sc_sensitive" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_pos" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_neg" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_module_name" = type { i8*, %"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*, %"class.sc_core::sc_simcontext"*, i8 }
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
@ELEMENT = global %class.element* null, align 8
@_ZSt4cout = external global %"class.std::basic_ostream"
@.str = private unnamed_addr constant [3 x i8] c"p1\00", align 1
@.str2 = private unnamed_addr constant [9 x i8] c"Element \00", align 1
@.str3 = private unnamed_addr constant [6 x i8] c" OK.\0A\00", align 1
@.str4 = private unnamed_addr constant [3 x i8] c"p2\00", align 1
@.str5 = private unnamed_addr constant [4 x i8] c"p2 \00", align 1
@.str6 = private unnamed_addr constant [8 x i8] c"element\00", align 1
@_ZTVN10__cxxabiv120__si_class_type_infoE = external global i8*
@_ZTSN7sc_core9sc_objectE = available_externally constant [21 x i8] c"N7sc_core9sc_objectE\00"
@_ZTIN7sc_core9sc_objectE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_objectE, i32 0, i32 0) }
@_ZTV7element = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI7element to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.element*)* @_ZN7elementD1Ev to i8*), i8* bitcast (void (%class.element*)* @_ZN7elementD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI7element to i8*), i8* bitcast (void (%class.element*)* @_ZThn40_N7elementD1Ev to i8*), i8* bitcast (void (%class.element*)* @_ZThn40_N7elementD0Ev to i8*)]
@.str13 = private unnamed_addr constant [3 x i8] c"P1\00", align 1
@.str14 = private unnamed_addr constant [3 x i8] c"P2\00", align 1
@_ZTS7element = linkonce_odr constant [9 x i8] c"7element\00"
@_ZTSN7sc_core9sc_moduleE = available_externally constant [21 x i8] c"N7sc_core9sc_moduleE\00"
@_ZTSN7sc_core15sc_process_hostE = linkonce_odr constant [28 x i8] c"N7sc_core15sc_process_hostE\00"
@_ZTIN7sc_core15sc_process_hostE = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_process_hostE, i32 0, i32 0) }
@_ZTIN7sc_core9sc_moduleE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_moduleE, i32 0, i32 0), i32 0, i32 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*), i64 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core15sc_process_hostE to i8*), i64 10242 }
@_ZTI7element = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([9 x i8]* @_ZTS7element, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@.str15 = private unnamed_addr constant [10 x i8] c"sc_module\00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str16 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str17 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str18 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_increment()\00", align 1
@_ZN7sc_core18sc_curr_simcontextE = external global %"class.sc_core::sc_simcontext"*
@_ZN7sc_core25sc_default_global_contextE = external global %"class.sc_core::sc_simcontext"*
@llvm.global_ctors = appending global [1 x { i32, void ()* }] [{ i32, void ()* } { i32 65535, void ()* @_GLOBAL__I_a }]

declare void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"*)

declare void @_ZNSt8ios_base4InitD1Ev(%"class.std::ios_base::Init"*)

declare i32 @__cxa_atexit(void (i8*)*, i8*, i8*) nounwind

declare void @_ZN7sc_core20sc_api_version_2_2_0C1Ev(%"class.sc_core::sc_api_version_2_2_0"*)

declare i32 @__gxx_personality_v0(...)

declare void @_ZSt9terminatev()

define void @_ZN7element2P1Ev(%class.element* %this) uwtable align 2 {
bb:
  %tmp = alloca %"class.sc_core::sc_time", align 8
  %tmp1 = bitcast %"class.sc_core::sc_time"* %tmp to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp1)
  %tmp2 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 1
  %tmp3 = load %"class.sc_core::sc_simcontext"** %tmp2, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, double 1.000000e+00, i32 2, %"class.sc_core::sc_simcontext"* %tmp3)
  %tmp4 = load %"class.sc_core::sc_simcontext"** %tmp2, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, %"class.sc_core::sc_simcontext"* %tmp4)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp1)
  %tmp5 = getelementptr inbounds %class.element* %this, i64 0, i32 3
  %tmp6 = load i32* %tmp5, align 4, !tbaa !3
  %tmp7 = getelementptr inbounds %class.element* %this, i64 0, i32 4
  %tmp8 = load i32* %tmp7, align 4, !tbaa !3
  %tmp9 = icmp sle i32 %tmp6, %tmp8
  %tmp10 = icmp sgt i32 %tmp6, 0
  %or.cond = and i1 %tmp9, %tmp10
  %tmp11 = icmp sgt i32 %tmp8, 0
  %or.cond1 = and i1 %or.cond, %tmp11
  br i1 %or.cond1, label %bb12, label %bb15

bb12:                                             ; preds = %bb
  %tmp13 = getelementptr inbounds %class.element* %this, i64 0, i32 1
  call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp13)
  %tmp14 = load %"class.sc_core::sc_simcontext"** %tmp2, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp13, %"class.sc_core::sc_simcontext"* %tmp14)
  br label %bb15

bb15:                                             ; preds = %bb12, %bb
  %tmp16 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str, i64 0, i64 0), i64 2)
  %tmp17 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8**), align 8, !tbaa !4
  %tmp18 = getelementptr i8* %tmp17, i64 -24
  %tmp19 = bitcast i8* %tmp18 to i64*
  %tmp20 = load i64* %tmp19, align 8
  %.sum = add i64 %tmp20, 240
  %tmp21 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %.sum
  %tmp22 = bitcast i8* %tmp21 to %"class.std::ctype"**
  %tmp23 = load %"class.std::ctype"** %tmp22, align 8, !tbaa !0
  %tmp24 = icmp eq %"class.std::ctype"* %tmp23, null
  br i1 %tmp24, label %bb25, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit

bb25:                                             ; preds = %bb15
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit:    ; preds = %bb15
  %tmp26 = getelementptr inbounds %"class.std::ctype"* %tmp23, i64 0, i32 6
  %tmp27 = load i8* %tmp26, align 1, !tbaa !1
  %tmp28 = icmp eq i8 %tmp27, 0
  br i1 %tmp28, label %bb32, label %bb29

bb29:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit
  %tmp30 = getelementptr inbounds %"class.std::ctype"* %tmp23, i64 0, i32 7, i64 10
  %tmp31 = load i8* %tmp30, align 1, !tbaa !1
  br label %_ZNKSt5ctypeIcE5widenEc.exit

bb32:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp23)
  %tmp33 = bitcast %"class.std::ctype"* %tmp23 to i8 (%"class.std::ctype"*, i8)***
  %tmp34 = load i8 (%"class.std::ctype"*, i8)*** %tmp33, align 8, !tbaa !4
  %tmp35 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp34, i64 6
  %tmp36 = load i8 (%"class.std::ctype"*, i8)** %tmp35, align 8
  %tmp37 = call signext i8 %tmp36(%"class.std::ctype"* %tmp23, i8 signext 10)
  br label %_ZNKSt5ctypeIcE5widenEc.exit

_ZNKSt5ctypeIcE5widenEc.exit:                     ; preds = %bb32, %bb29
  %.0.i = phi i8 [ %tmp31, %bb29 ], [ %tmp37, %bb32 ]
  %tmp38 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* @_ZSt4cout, i8 signext %.0.i)
  %tmp39 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp38)
  %tmp40 = getelementptr inbounds %class.element* %this, i64 0, i32 1
  call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp40)
  %tmp41 = getelementptr inbounds %class.element* %this, i64 0, i32 2
  %tmp42 = load %"class.sc_core::sc_simcontext"** %tmp2, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp41, %"class.sc_core::sc_simcontext"* %tmp42)
  %tmp43 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([9 x i8]* @.str2, i64 0, i64 0), i64 8)
  %tmp44 = load i32* %tmp7, align 4, !tbaa !3
  %tmp45 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp44)
  %tmp46 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp45, i8* getelementptr inbounds ([6 x i8]* @.str3, i64 0, i64 0), i64 5)
  ret void
}

declare void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"*)

declare %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"*, i32)

define void @_ZN7element2P2Ev(%class.element* %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.element* %this, i64 0, i32 3
  %tmp1 = load i32* %tmp, align 4, !tbaa !3
  %tmp2 = getelementptr inbounds %class.element* %this, i64 0, i32 4
  %tmp3 = load i32* %tmp2, align 4, !tbaa !3
  %tmp4 = icmp sgt i32 %tmp1, %tmp3
  br i1 %tmp4, label %bb63, label %bb5

bb5:                                              ; preds = %bb
  %tmp6 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str4, i64 0, i64 0), i64 2)
  %tmp7 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8**), align 8, !tbaa !4
  %tmp8 = getelementptr i8* %tmp7, i64 -24
  %tmp9 = bitcast i8* %tmp8 to i64*
  %tmp10 = load i64* %tmp9, align 8
  %.sum.i = add i64 %tmp10, 240
  %tmp11 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %.sum.i
  %tmp12 = bitcast i8* %tmp11 to %"class.std::ctype"**
  %tmp13 = load %"class.std::ctype"** %tmp12, align 8, !tbaa !0
  %tmp14 = icmp eq %"class.std::ctype"* %tmp13, null
  br i1 %tmp14, label %bb15, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb15:                                             ; preds = %bb5
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %bb5
  %tmp16 = getelementptr inbounds %"class.std::ctype"* %tmp13, i64 0, i32 6
  %tmp17 = load i8* %tmp16, align 1, !tbaa !1
  %tmp18 = icmp eq i8 %tmp17, 0
  br i1 %tmp18, label %bb22, label %bb19

bb19:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp20 = getelementptr inbounds %"class.std::ctype"* %tmp13, i64 0, i32 7, i64 10
  %tmp21 = load i8* %tmp20, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb22:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp13)
  %tmp23 = bitcast %"class.std::ctype"* %tmp13 to i8 (%"class.std::ctype"*, i8)***
  %tmp24 = load i8 (%"class.std::ctype"*, i8)*** %tmp23, align 8, !tbaa !4
  %tmp25 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp24, i64 6
  %tmp26 = load i8 (%"class.std::ctype"*, i8)** %tmp25, align 8
  %tmp27 = tail call signext i8 %tmp26(%"class.std::ctype"* %tmp13, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb22, %bb19
  %.0.i.i.i = phi i8 [ %tmp21, %bb19 ], [ %tmp27, %bb22 ]
  %tmp28 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* @_ZSt4cout, i8 signext %.0.i.i.i)
  %tmp29 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp28)
  %tmp30 = getelementptr inbounds %class.element* %this, i64 0, i32 1
  %tmp31 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 1
  %tmp32 = load %"class.sc_core::sc_simcontext"** %tmp31, align 8, !tbaa !0
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp30, %"class.sc_core::sc_simcontext"* %tmp32)
  %tmp33 = load i32* %tmp, align 4, !tbaa !3
  %tmp34 = add nsw i32 %tmp33, 1
  store i32 %tmp34, i32* %tmp, align 4, !tbaa !3
  %tmp35 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([4 x i8]* @.str5, i64 0, i64 0), i64 3)
  %tmp36 = load i32* %tmp, align 4, !tbaa !3
  %tmp37 = tail call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp36)
  %tmp38 = bitcast %"class.std::basic_ostream"* %tmp37 to i8**
  %tmp39 = load i8** %tmp38, align 8, !tbaa !4
  %tmp40 = getelementptr i8* %tmp39, i64 -24
  %tmp41 = bitcast i8* %tmp40 to i64*
  %tmp42 = load i64* %tmp41, align 8
  %tmp43 = bitcast %"class.std::basic_ostream"* %tmp37 to i8*
  %.sum.i1 = add i64 %tmp42, 240
  %tmp44 = getelementptr inbounds i8* %tmp43, i64 %.sum.i1
  %tmp45 = bitcast i8* %tmp44 to %"class.std::ctype"**
  %tmp46 = load %"class.std::ctype"** %tmp45, align 8, !tbaa !0
  %tmp47 = icmp eq %"class.std::ctype"* %tmp46, null
  br i1 %tmp47, label %bb48, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2

bb48:                                             ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2: ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit
  %tmp49 = getelementptr inbounds %"class.std::ctype"* %tmp46, i64 0, i32 6
  %tmp50 = load i8* %tmp49, align 1, !tbaa !1
  %tmp51 = icmp eq i8 %tmp50, 0
  br i1 %tmp51, label %bb55, label %bb52

bb52:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  %tmp53 = getelementptr inbounds %"class.std::ctype"* %tmp46, i64 0, i32 7, i64 10
  %tmp54 = load i8* %tmp53, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

bb55:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp46)
  %tmp56 = bitcast %"class.std::ctype"* %tmp46 to i8 (%"class.std::ctype"*, i8)***
  %tmp57 = load i8 (%"class.std::ctype"*, i8)*** %tmp56, align 8, !tbaa !4
  %tmp58 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp57, i64 6
  %tmp59 = load i8 (%"class.std::ctype"*, i8)** %tmp58, align 8
  %tmp60 = tail call signext i8 %tmp59(%"class.std::ctype"* %tmp46, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4: ; preds = %bb55, %bb52
  %.0.i.i.i3 = phi i8 [ %tmp54, %bb52 ], [ %tmp60, %bb55 ]
  %tmp61 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp37, i8 signext %.0.i.i.i3)
  %tmp62 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp61)
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp30)
  %.pr = load i32* %tmp2, align 4, !tbaa !3
  br label %bb63

bb63:                                             ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4, %bb
  %tmp64 = phi i32 [ %tmp3, %bb ], [ %.pr, %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4 ]
  %tmp65 = icmp sgt i32 %tmp64, 1
  br i1 %tmp65, label %bb66, label %bb70

bb66:                                             ; preds = %bb63
  %tmp67 = getelementptr inbounds %class.element* %this, i64 0, i32 1
  %tmp68 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 1
  %tmp69 = load %"class.sc_core::sc_simcontext"** %tmp68, align 8, !tbaa !0
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp67, %"class.sc_core::sc_simcontext"* %tmp69)
  br label %bb70

bb70:                                             ; preds = %bb66, %bb63
  ret void
}

define i32 @sc_main(i32 %argc, i8** nocapture %argv) uwtable {
bb:
  %tmp = alloca %"class.sc_core::sc_module_name", align 8
  %tmp1 = icmp sgt i32 %argc, 1
  br i1 %tmp1, label %bb2, label %_ZN7elementC1EN7sc_core14sc_module_nameEii.exit

bb2:                                              ; preds = %bb
  %tmp3 = getelementptr inbounds i8** %argv, i64 1
  %tmp4 = load i8** %tmp3, align 8, !tbaa !0
  %tmp5 = call i32 @atoi(i8* %tmp4) nounwind readonly
  %tmp6 = getelementptr inbounds i8** %argv, i64 2
  %tmp7 = load i8** %tmp6, align 8, !tbaa !0
  %tmp8 = call i32 @atoi(i8* %tmp7) nounwind readonly
  br label %_ZN7elementC1EN7sc_core14sc_module_nameEii.exit

_ZN7elementC1EN7sc_core14sc_module_nameEii.exit:  ; preds = %bb2, %bb
  %a.0 = phi i32 [ %tmp8, %bb2 ], [ 0, %bb ]
  %num.0 = phi i32 [ %tmp5, %bb2 ], [ 0, %bb ]
  %tmp9 = call noalias i8* @_Znwm(i64 432)
  %tmp10 = bitcast i8* %tmp9 to %class.element*
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp, i8* getelementptr inbounds ([8 x i8]* @.str6, i64 0, i64 0))
  call void @_ZN7elementC2EN7sc_core14sc_module_nameEii(%class.element* %tmp10, %"class.sc_core::sc_module_name"* %tmp, i32 %a.0, i32 %num.0)
  store %class.element* %tmp10, %class.element** @ELEMENT, align 8, !tbaa !0
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp)
  call void @_ZN7sc_core8sc_startEv()
  ret i32 0
}

declare i32 @atoi(i8* nocapture) nounwind readonly

declare noalias i8* @_Znwm(i64)

declare void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"*, i8*)

declare void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"*)

declare void @_ZdlPv(i8*) nounwind

declare void @_ZN7sc_core8sc_startEv()

declare %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"*, i8 signext)

declare void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"*)

declare void @_ZSt16__throw_bad_castv() noreturn

declare %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"*)

declare %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"*, i8*, i64)

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

define linkonce_odr void @_ZN7elementC2EN7sc_core14sc_module_nameEii(%class.element* %this, %"class.sc_core::sc_module_name"* %name, i32 %a, i32 %n) unnamed_addr uwtable align 2 {
bb:
  %P1_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %P2_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp4 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp5 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp6 = getelementptr inbounds %class.element* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp6, %"class.sc_core::sc_module_name"* %name)
  %tmp7 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp7, align 8, !tbaa !4
  %tmp8 = getelementptr %class.element* %this, i64 0, i32 0, i32 1
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp8, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp9, align 8, !tbaa !4
  %tmp10 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp11 = icmp eq %"class.sc_core::sc_simcontext"* %tmp10, null
  br i1 %tmp11, label %.noexc, label %bb14

.noexc:                                           ; preds = %bb
  %tmp12 = call noalias i8* @_Znwm(i64 248)
  %tmp13 = bitcast i8* %tmp12 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp13)
  store %"class.sc_core::sc_simcontext"* %tmp13, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp13, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %bb14

bb14:                                             ; preds = %.noexc, %bb
  %tmp15 = phi %"class.sc_core::sc_simcontext"* [ %tmp13, %.noexc ], [ %tmp10, %bb ]
  %tmp16 = getelementptr inbounds %class.element* %this, i64 0, i32 1, i32 0
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** %tmp16, align 8, !tbaa !0
  %tmp17 = getelementptr inbounds %class.element* %this, i64 0, i32 1, i32 1
  store i32 0, i32* %tmp17, align 4, !tbaa !5
  %tmp18 = getelementptr inbounds %class.element* %this, i64 0, i32 1, i32 2
  store i32 -1, i32* %tmp18, align 4, !tbaa !3
  %tmp19 = getelementptr inbounds %class.element* %this, i64 0, i32 1, i32 3
  %tmp20 = bitcast %"class.sc_core::sc_event_timed"** %tmp19 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp20, i8 0, i64 104, i32 8, i1 false)
  %tmp21 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp22 = icmp eq %"class.sc_core::sc_simcontext"* %tmp21, null
  br i1 %tmp22, label %.noexc10, label %bb25

.noexc10:                                         ; preds = %bb14
  %tmp23 = call noalias i8* @_Znwm(i64 248)
  %tmp24 = bitcast i8* %tmp23 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp24)
  store %"class.sc_core::sc_simcontext"* %tmp24, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp24, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %bb25

bb25:                                             ; preds = %.noexc10, %bb14
  %tmp26 = phi %"class.sc_core::sc_simcontext"* [ %tmp24, %.noexc10 ], [ %tmp21, %bb14 ]
  %tmp27 = getelementptr inbounds %class.element* %this, i64 0, i32 2, i32 0
  store %"class.sc_core::sc_simcontext"* %tmp26, %"class.sc_core::sc_simcontext"** %tmp27, align 8, !tbaa !0
  %tmp28 = getelementptr inbounds %class.element* %this, i64 0, i32 2, i32 1
  store i32 0, i32* %tmp28, align 4, !tbaa !5
  %tmp29 = getelementptr inbounds %class.element* %this, i64 0, i32 2, i32 2
  store i32 -1, i32* %tmp29, align 4, !tbaa !3
  %tmp30 = getelementptr inbounds %class.element* %this, i64 0, i32 2, i32 3
  %tmp31 = bitcast %"class.sc_core::sc_event_timed"** %tmp30 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp31, i8 0, i64 104, i32 8, i1 false)
  %tmp32 = getelementptr inbounds %class.element* %this, i64 0, i32 3
  store i32 %a, i32* %tmp32, align 4, !tbaa !3
  %tmp33 = getelementptr inbounds %class.element* %this, i64 0, i32 4
  store i32 %n, i32* %tmp33, align 4, !tbaa !3
  %tmp34 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp35 = icmp eq %"class.sc_core::sc_simcontext"* %tmp34, null
  br i1 %tmp35, label %.noexc14, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc14:                                         ; preds = %bb25
  %tmp36 = call noalias i8* @_Znwm(i64 248)
  %tmp37 = bitcast i8* %tmp36 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp37)
  store %"class.sc_core::sc_simcontext"* %tmp37, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp37, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc14, %bb25
  %tmp38 = phi %"class.sc_core::sc_simcontext"* [ %tmp37, %.noexc14 ], [ %tmp34, %bb25 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %P1_handle, %"class.sc_core::sc_simcontext"* %tmp38, i8* getelementptr inbounds ([3 x i8]* @.str13, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.element*)* @_ZN7element2P1Ev to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp8, %"class.sc_core::sc_spawn_options"* null)
  %tmp39 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %P1_handle, i64 0, i32 0
  %tmp40 = load %"class.sc_core::sc_process_b"** %tmp39, align 8, !tbaa !0
  %tmp41 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp40, %"class.sc_core::sc_process_b"** %tmp41, align 8, !tbaa !0
  %tmp42 = icmp eq %"class.sc_core::sc_process_b"* %tmp40, null
  br i1 %tmp42, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb43

bb43:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp44 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp40, i64 0, i32 15
  %tmp45 = load i32* %tmp44, align 4, !tbaa !3
  %tmp46 = icmp eq i32 %tmp45, 0
  br i1 %tmp46, label %bb47, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb47:                                             ; preds = %bb43
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb43
  %tmp48 = add nsw i32 %tmp45, 1
  store i32 %tmp48, i32* %tmp44, align 4, !tbaa !3
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp49 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 2
  %tmp50 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp49, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp51 = load %"class.sc_core::sc_process_b"** %tmp41, align 8, !tbaa !0
  %tmp52 = icmp eq %"class.sc_core::sc_process_b"* %tmp51, null
  br i1 %tmp52, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb53

bb53:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp54 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp51, i64 0, i32 15
  %tmp55 = load i32* %tmp54, align 4, !tbaa !3
  %tmp56 = add nsw i32 %tmp55, -1
  store i32 %tmp56, i32* %tmp54, align 4, !tbaa !3
  %tmp57 = icmp eq i32 %tmp56, 0
  br i1 %tmp57, label %bb58, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb58:                                             ; preds = %bb53
  %tmp59 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp60 = icmp eq %"class.sc_core::sc_process_b"* %tmp59, null
  br i1 %tmp60, label %bb65, label %.noexc17

.noexc17:                                         ; preds = %bb58
  %tmp61 = bitcast %"class.sc_core::sc_process_b"* %tmp59 to void (%"class.sc_core::sc_process_b"*)***
  %tmp62 = load void (%"class.sc_core::sc_process_b"*)*** %tmp61, align 8, !tbaa !4
  %tmp63 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp62, i64 6
  %tmp64 = load void (%"class.sc_core::sc_process_b"*)** %tmp63, align 8
  call void %tmp64(%"class.sc_core::sc_process_b"* %tmp59)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb65

bb65:                                             ; preds = %.noexc17, %bb58
  %tmp66 = phi %"class.sc_core::sc_process_b"* [ null, %bb58 ], [ %.pre.i.i.i, %.noexc17 ]
  %tmp67 = icmp eq %"class.sc_core::sc_process_b"* %tmp66, %tmp51
  br i1 %tmp67, label %bb68, label %bb69

bb68:                                             ; preds = %bb65
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb69:                                             ; preds = %bb65
  store %"class.sc_core::sc_process_b"* %tmp51, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb69, %bb53, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp70 = load %"class.sc_core::sc_process_b"** %tmp39, align 8, !tbaa !0
  %tmp71 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp70, %"class.sc_core::sc_process_b"** %tmp71, align 8, !tbaa !0
  %tmp72 = icmp eq %"class.sc_core::sc_process_b"* %tmp70, null
  br i1 %tmp72, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19, label %bb73

bb73:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp74 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp70, i64 0, i32 15
  %tmp75 = load i32* %tmp74, align 4, !tbaa !3
  %tmp76 = icmp eq i32 %tmp75, 0
  br i1 %tmp76, label %bb77, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18

bb77:                                             ; preds = %bb73
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18: ; preds = %bb73
  %tmp78 = add nsw i32 %tmp75, 1
  store i32 %tmp78, i32* %tmp74, align 4, !tbaa !3
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19

_ZN7sc_core17sc_process_handleC1ERKS0_.exit19:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp79 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 3
  %tmp80 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp79, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp81 = load %"class.sc_core::sc_process_b"** %tmp71, align 8, !tbaa !0
  %tmp82 = icmp eq %"class.sc_core::sc_process_b"* %tmp81, null
  br i1 %tmp82, label %_ZN7sc_core17sc_process_handleD1Ev.exit22, label %bb83

bb83:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19
  %tmp84 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp81, i64 0, i32 15
  %tmp85 = load i32* %tmp84, align 4, !tbaa !3
  %tmp86 = add nsw i32 %tmp85, -1
  store i32 %tmp86, i32* %tmp84, align 4, !tbaa !3
  %tmp87 = icmp eq i32 %tmp86, 0
  br i1 %tmp87, label %bb88, label %_ZN7sc_core17sc_process_handleD1Ev.exit22

bb88:                                             ; preds = %bb83
  %tmp89 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp90 = icmp eq %"class.sc_core::sc_process_b"* %tmp89, null
  br i1 %tmp90, label %bb95, label %.noexc21

.noexc21:                                         ; preds = %bb88
  %tmp91 = bitcast %"class.sc_core::sc_process_b"* %tmp89 to void (%"class.sc_core::sc_process_b"*)***
  %tmp92 = load void (%"class.sc_core::sc_process_b"*)*** %tmp91, align 8, !tbaa !4
  %tmp93 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp92, i64 6
  %tmp94 = load void (%"class.sc_core::sc_process_b"*)** %tmp93, align 8
  call void %tmp94(%"class.sc_core::sc_process_b"* %tmp89)
  %.pre.i.i.i20 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb95

bb95:                                             ; preds = %.noexc21, %bb88
  %tmp96 = phi %"class.sc_core::sc_process_b"* [ null, %bb88 ], [ %.pre.i.i.i20, %.noexc21 ]
  %tmp97 = icmp eq %"class.sc_core::sc_process_b"* %tmp96, %tmp81
  br i1 %tmp97, label %bb98, label %bb99

bb98:                                             ; preds = %bb95
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb99:                                             ; preds = %bb95
  store %"class.sc_core::sc_process_b"* %tmp81, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit22

_ZN7sc_core17sc_process_handleD1Ev.exit22:        ; preds = %bb99, %bb83, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19
  %tmp100 = load %"class.sc_core::sc_process_b"** %tmp39, align 8, !tbaa !0
  %tmp101 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp100, %"class.sc_core::sc_process_b"** %tmp101, align 8, !tbaa !0
  %tmp102 = icmp eq %"class.sc_core::sc_process_b"* %tmp100, null
  br i1 %tmp102, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit24, label %bb103

bb103:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit22
  %tmp104 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp100, i64 0, i32 15
  %tmp105 = load i32* %tmp104, align 4, !tbaa !3
  %tmp106 = icmp eq i32 %tmp105, 0
  br i1 %tmp106, label %bb107, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i23

bb107:                                            ; preds = %bb103
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i23: ; preds = %bb103
  %tmp108 = add nsw i32 %tmp105, 1
  store i32 %tmp108, i32* %tmp104, align 4, !tbaa !3
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit24

_ZN7sc_core17sc_process_handleC1ERKS0_.exit24:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i23, %_ZN7sc_core17sc_process_handleD1Ev.exit22
  %tmp109 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 4
  %tmp110 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp109, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp111 = load %"class.sc_core::sc_process_b"** %tmp101, align 8, !tbaa !0
  %tmp112 = icmp eq %"class.sc_core::sc_process_b"* %tmp111, null
  br i1 %tmp112, label %_ZN7sc_core17sc_process_handleD1Ev.exit27, label %bb113

bb113:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit24
  %tmp114 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp111, i64 0, i32 15
  %tmp115 = load i32* %tmp114, align 4, !tbaa !3
  %tmp116 = add nsw i32 %tmp115, -1
  store i32 %tmp116, i32* %tmp114, align 4, !tbaa !3
  %tmp117 = icmp eq i32 %tmp116, 0
  br i1 %tmp117, label %bb118, label %_ZN7sc_core17sc_process_handleD1Ev.exit27

bb118:                                            ; preds = %bb113
  %tmp119 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp120 = icmp eq %"class.sc_core::sc_process_b"* %tmp119, null
  br i1 %tmp120, label %bb125, label %.noexc26

.noexc26:                                         ; preds = %bb118
  %tmp121 = bitcast %"class.sc_core::sc_process_b"* %tmp119 to void (%"class.sc_core::sc_process_b"*)***
  %tmp122 = load void (%"class.sc_core::sc_process_b"*)*** %tmp121, align 8, !tbaa !4
  %tmp123 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp122, i64 6
  %tmp124 = load void (%"class.sc_core::sc_process_b"*)** %tmp123, align 8
  call void %tmp124(%"class.sc_core::sc_process_b"* %tmp119)
  %.pre.i.i.i25 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb125

bb125:                                            ; preds = %.noexc26, %bb118
  %tmp126 = phi %"class.sc_core::sc_process_b"* [ null, %bb118 ], [ %.pre.i.i.i25, %.noexc26 ]
  %tmp127 = icmp eq %"class.sc_core::sc_process_b"* %tmp126, %tmp111
  br i1 %tmp127, label %bb128, label %bb129

bb128:                                            ; preds = %bb125
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb129:                                            ; preds = %bb125
  store %"class.sc_core::sc_process_b"* %tmp111, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit27

_ZN7sc_core17sc_process_handleD1Ev.exit27:        ; preds = %bb129, %bb113, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit24
  %tmp130 = load %"class.sc_core::sc_process_b"** %tmp39, align 8, !tbaa !0
  %tmp131 = icmp eq %"class.sc_core::sc_process_b"* %tmp130, null
  br i1 %tmp131, label %_ZN7sc_core17sc_process_handleD1Ev.exit30, label %bb132

bb132:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit27
  %tmp133 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp130, i64 0, i32 15
  %tmp134 = load i32* %tmp133, align 4, !tbaa !3
  %tmp135 = add nsw i32 %tmp134, -1
  store i32 %tmp135, i32* %tmp133, align 4, !tbaa !3
  %tmp136 = icmp eq i32 %tmp135, 0
  br i1 %tmp136, label %bb137, label %_ZN7sc_core17sc_process_handleD1Ev.exit30

bb137:                                            ; preds = %bb132
  %tmp138 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp139 = icmp eq %"class.sc_core::sc_process_b"* %tmp138, null
  br i1 %tmp139, label %bb144, label %.noexc29

.noexc29:                                         ; preds = %bb137
  %tmp140 = bitcast %"class.sc_core::sc_process_b"* %tmp138 to void (%"class.sc_core::sc_process_b"*)***
  %tmp141 = load void (%"class.sc_core::sc_process_b"*)*** %tmp140, align 8, !tbaa !4
  %tmp142 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp141, i64 6
  %tmp143 = load void (%"class.sc_core::sc_process_b"*)** %tmp142, align 8
  call void %tmp143(%"class.sc_core::sc_process_b"* %tmp138)
  %.pre.i.i.i28 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb144

bb144:                                            ; preds = %.noexc29, %bb137
  %tmp145 = phi %"class.sc_core::sc_process_b"* [ null, %bb137 ], [ %.pre.i.i.i28, %.noexc29 ]
  %tmp146 = icmp eq %"class.sc_core::sc_process_b"* %tmp145, %tmp130
  br i1 %tmp146, label %bb147, label %bb148

bb147:                                            ; preds = %bb144
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb148:                                            ; preds = %bb144
  store %"class.sc_core::sc_process_b"* %tmp130, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit30

_ZN7sc_core17sc_process_handleD1Ev.exit30:        ; preds = %bb148, %bb132, %_ZN7sc_core17sc_process_handleD1Ev.exit27
  %tmp149 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp150 = icmp eq %"class.sc_core::sc_simcontext"* %tmp149, null
  br i1 %tmp150, label %.noexc31, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit34

.noexc31:                                         ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit30
  %tmp151 = call noalias i8* @_Znwm(i64 248)
  %tmp152 = bitcast i8* %tmp151 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp152)
  store %"class.sc_core::sc_simcontext"* %tmp152, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp152, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit34

_ZN7sc_core22sc_get_curr_simcontextEv.exit34:     ; preds = %.noexc31, %_ZN7sc_core17sc_process_handleD1Ev.exit30
  %tmp153 = phi %"class.sc_core::sc_simcontext"* [ %tmp152, %.noexc31 ], [ %tmp149, %_ZN7sc_core17sc_process_handleD1Ev.exit30 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %P2_handle, %"class.sc_core::sc_simcontext"* %tmp153, i8* getelementptr inbounds ([3 x i8]* @.str14, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.element*)* @_ZN7element2P2Ev to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp8, %"class.sc_core::sc_spawn_options"* null)
  %tmp154 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %P2_handle, i64 0, i32 0
  %tmp155 = load %"class.sc_core::sc_process_b"** %tmp154, align 8, !tbaa !0
  %tmp156 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp3, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp155, %"class.sc_core::sc_process_b"** %tmp156, align 8, !tbaa !0
  %tmp157 = icmp eq %"class.sc_core::sc_process_b"* %tmp155, null
  br i1 %tmp157, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36, label %bb158

bb158:                                            ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit34
  %tmp159 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp155, i64 0, i32 15
  %tmp160 = load i32* %tmp159, align 4, !tbaa !3
  %tmp161 = icmp eq i32 %tmp160, 0
  br i1 %tmp161, label %bb162, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35

bb162:                                            ; preds = %bb158
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35: ; preds = %bb158
  %tmp163 = add nsw i32 %tmp160, 1
  store i32 %tmp163, i32* %tmp159, align 4, !tbaa !3
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36

_ZN7sc_core17sc_process_handleC1ERKS0_.exit36:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35, %_ZN7sc_core22sc_get_curr_simcontextEv.exit34
  %tmp164 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp49, %"class.sc_core::sc_process_handle"* %tmp3)
  %tmp165 = load %"class.sc_core::sc_process_b"** %tmp156, align 8, !tbaa !0
  %tmp166 = icmp eq %"class.sc_core::sc_process_b"* %tmp165, null
  br i1 %tmp166, label %_ZN7sc_core17sc_process_handleD1Ev.exit39, label %bb167

bb167:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36
  %tmp168 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp165, i64 0, i32 15
  %tmp169 = load i32* %tmp168, align 4, !tbaa !3
  %tmp170 = add nsw i32 %tmp169, -1
  store i32 %tmp170, i32* %tmp168, align 4, !tbaa !3
  %tmp171 = icmp eq i32 %tmp170, 0
  br i1 %tmp171, label %bb172, label %_ZN7sc_core17sc_process_handleD1Ev.exit39

bb172:                                            ; preds = %bb167
  %tmp173 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp174 = icmp eq %"class.sc_core::sc_process_b"* %tmp173, null
  br i1 %tmp174, label %bb179, label %.noexc38

.noexc38:                                         ; preds = %bb172
  %tmp175 = bitcast %"class.sc_core::sc_process_b"* %tmp173 to void (%"class.sc_core::sc_process_b"*)***
  %tmp176 = load void (%"class.sc_core::sc_process_b"*)*** %tmp175, align 8, !tbaa !4
  %tmp177 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp176, i64 6
  %tmp178 = load void (%"class.sc_core::sc_process_b"*)** %tmp177, align 8
  call void %tmp178(%"class.sc_core::sc_process_b"* %tmp173)
  %.pre.i.i.i37 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb179

bb179:                                            ; preds = %.noexc38, %bb172
  %tmp180 = phi %"class.sc_core::sc_process_b"* [ null, %bb172 ], [ %.pre.i.i.i37, %.noexc38 ]
  %tmp181 = icmp eq %"class.sc_core::sc_process_b"* %tmp180, %tmp165
  br i1 %tmp181, label %bb182, label %bb183

bb182:                                            ; preds = %bb179
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb183:                                            ; preds = %bb179
  store %"class.sc_core::sc_process_b"* %tmp165, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit39

_ZN7sc_core17sc_process_handleD1Ev.exit39:        ; preds = %bb183, %bb167, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36
  %tmp184 = load %"class.sc_core::sc_process_b"** %tmp154, align 8, !tbaa !0
  %tmp185 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp4, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp184, %"class.sc_core::sc_process_b"** %tmp185, align 8, !tbaa !0
  %tmp186 = icmp eq %"class.sc_core::sc_process_b"* %tmp184, null
  br i1 %tmp186, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41, label %bb187

bb187:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit39
  %tmp188 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp184, i64 0, i32 15
  %tmp189 = load i32* %tmp188, align 4, !tbaa !3
  %tmp190 = icmp eq i32 %tmp189, 0
  br i1 %tmp190, label %bb191, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40

bb191:                                            ; preds = %bb187
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40: ; preds = %bb187
  %tmp192 = add nsw i32 %tmp189, 1
  store i32 %tmp192, i32* %tmp188, align 4, !tbaa !3
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41

_ZN7sc_core17sc_process_handleC1ERKS0_.exit41:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40, %_ZN7sc_core17sc_process_handleD1Ev.exit39
  %tmp193 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp79, %"class.sc_core::sc_process_handle"* %tmp4)
  %tmp194 = load %"class.sc_core::sc_process_b"** %tmp185, align 8, !tbaa !0
  %tmp195 = icmp eq %"class.sc_core::sc_process_b"* %tmp194, null
  br i1 %tmp195, label %_ZN7sc_core17sc_process_handleD1Ev.exit44, label %bb196

bb196:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41
  %tmp197 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp194, i64 0, i32 15
  %tmp198 = load i32* %tmp197, align 4, !tbaa !3
  %tmp199 = add nsw i32 %tmp198, -1
  store i32 %tmp199, i32* %tmp197, align 4, !tbaa !3
  %tmp200 = icmp eq i32 %tmp199, 0
  br i1 %tmp200, label %bb201, label %_ZN7sc_core17sc_process_handleD1Ev.exit44

bb201:                                            ; preds = %bb196
  %tmp202 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp203 = icmp eq %"class.sc_core::sc_process_b"* %tmp202, null
  br i1 %tmp203, label %bb208, label %.noexc43

.noexc43:                                         ; preds = %bb201
  %tmp204 = bitcast %"class.sc_core::sc_process_b"* %tmp202 to void (%"class.sc_core::sc_process_b"*)***
  %tmp205 = load void (%"class.sc_core::sc_process_b"*)*** %tmp204, align 8, !tbaa !4
  %tmp206 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp205, i64 6
  %tmp207 = load void (%"class.sc_core::sc_process_b"*)** %tmp206, align 8
  call void %tmp207(%"class.sc_core::sc_process_b"* %tmp202)
  %.pre.i.i.i42 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb208

bb208:                                            ; preds = %.noexc43, %bb201
  %tmp209 = phi %"class.sc_core::sc_process_b"* [ null, %bb201 ], [ %.pre.i.i.i42, %.noexc43 ]
  %tmp210 = icmp eq %"class.sc_core::sc_process_b"* %tmp209, %tmp194
  br i1 %tmp210, label %bb211, label %bb212

bb211:                                            ; preds = %bb208
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb212:                                            ; preds = %bb208
  store %"class.sc_core::sc_process_b"* %tmp194, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit44

_ZN7sc_core17sc_process_handleD1Ev.exit44:        ; preds = %bb212, %bb196, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41
  %tmp213 = load %"class.sc_core::sc_process_b"** %tmp154, align 8, !tbaa !0
  %tmp214 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp5, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp213, %"class.sc_core::sc_process_b"** %tmp214, align 8, !tbaa !0
  %tmp215 = icmp eq %"class.sc_core::sc_process_b"* %tmp213, null
  br i1 %tmp215, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit46, label %bb216

bb216:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit44
  %tmp217 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp213, i64 0, i32 15
  %tmp218 = load i32* %tmp217, align 4, !tbaa !3
  %tmp219 = icmp eq i32 %tmp218, 0
  br i1 %tmp219, label %bb220, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i45

bb220:                                            ; preds = %bb216
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str18, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i45: ; preds = %bb216
  %tmp221 = add nsw i32 %tmp218, 1
  store i32 %tmp221, i32* %tmp217, align 4, !tbaa !3
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit46

_ZN7sc_core17sc_process_handleC1ERKS0_.exit46:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i45, %_ZN7sc_core17sc_process_handleD1Ev.exit44
  %tmp222 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp109, %"class.sc_core::sc_process_handle"* %tmp5)
  %tmp223 = load %"class.sc_core::sc_process_b"** %tmp214, align 8, !tbaa !0
  %tmp224 = icmp eq %"class.sc_core::sc_process_b"* %tmp223, null
  br i1 %tmp224, label %_ZN7sc_core17sc_process_handleD1Ev.exit49, label %bb225

bb225:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit46
  %tmp226 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp223, i64 0, i32 15
  %tmp227 = load i32* %tmp226, align 4, !tbaa !3
  %tmp228 = add nsw i32 %tmp227, -1
  store i32 %tmp228, i32* %tmp226, align 4, !tbaa !3
  %tmp229 = icmp eq i32 %tmp228, 0
  br i1 %tmp229, label %bb230, label %_ZN7sc_core17sc_process_handleD1Ev.exit49

bb230:                                            ; preds = %bb225
  %tmp231 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp232 = icmp eq %"class.sc_core::sc_process_b"* %tmp231, null
  br i1 %tmp232, label %bb237, label %.noexc48

.noexc48:                                         ; preds = %bb230
  %tmp233 = bitcast %"class.sc_core::sc_process_b"* %tmp231 to void (%"class.sc_core::sc_process_b"*)***
  %tmp234 = load void (%"class.sc_core::sc_process_b"*)*** %tmp233, align 8, !tbaa !4
  %tmp235 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp234, i64 6
  %tmp236 = load void (%"class.sc_core::sc_process_b"*)** %tmp235, align 8
  call void %tmp236(%"class.sc_core::sc_process_b"* %tmp231)
  %.pre.i.i.i47 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb237

bb237:                                            ; preds = %.noexc48, %bb230
  %tmp238 = phi %"class.sc_core::sc_process_b"* [ null, %bb230 ], [ %.pre.i.i.i47, %.noexc48 ]
  %tmp239 = icmp eq %"class.sc_core::sc_process_b"* %tmp238, %tmp223
  br i1 %tmp239, label %bb240, label %bb241

bb240:                                            ; preds = %bb237
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb241:                                            ; preds = %bb237
  store %"class.sc_core::sc_process_b"* %tmp223, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit49

_ZN7sc_core17sc_process_handleD1Ev.exit49:        ; preds = %bb241, %bb225, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit46
  %tmp242 = load %"class.sc_core::sc_process_b"** %tmp154, align 8, !tbaa !0
  %tmp243 = icmp eq %"class.sc_core::sc_process_b"* %tmp242, null
  br i1 %tmp243, label %_ZN7sc_core17sc_process_handleD1Ev.exit52, label %bb244

bb244:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit49
  %tmp245 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp242, i64 0, i32 15
  %tmp246 = load i32* %tmp245, align 4, !tbaa !3
  %tmp247 = add nsw i32 %tmp246, -1
  store i32 %tmp247, i32* %tmp245, align 4, !tbaa !3
  %tmp248 = icmp eq i32 %tmp247, 0
  br i1 %tmp248, label %bb249, label %_ZN7sc_core17sc_process_handleD1Ev.exit52

bb249:                                            ; preds = %bb244
  %tmp250 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp251 = icmp eq %"class.sc_core::sc_process_b"* %tmp250, null
  br i1 %tmp251, label %bb256, label %.noexc51

.noexc51:                                         ; preds = %bb249
  %tmp252 = bitcast %"class.sc_core::sc_process_b"* %tmp250 to void (%"class.sc_core::sc_process_b"*)***
  %tmp253 = load void (%"class.sc_core::sc_process_b"*)*** %tmp252, align 8, !tbaa !4
  %tmp254 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp253, i64 6
  %tmp255 = load void (%"class.sc_core::sc_process_b"*)** %tmp254, align 8
  call void %tmp255(%"class.sc_core::sc_process_b"* %tmp250)
  %.pre.i.i.i50 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb256

bb256:                                            ; preds = %.noexc51, %bb249
  %tmp257 = phi %"class.sc_core::sc_process_b"* [ null, %bb249 ], [ %.pre.i.i.i50, %.noexc51 ]
  %tmp258 = icmp eq %"class.sc_core::sc_process_b"* %tmp257, %tmp242
  br i1 %tmp258, label %bb259, label %bb260

bb259:                                            ; preds = %bb256
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str17, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb260:                                            ; preds = %bb256
  store %"class.sc_core::sc_process_b"* %tmp242, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit52

_ZN7sc_core17sc_process_handleD1Ev.exit52:        ; preds = %bb260, %bb244, %_ZN7sc_core17sc_process_handleD1Ev.exit49
  ret void
}

declare void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

declare void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"*)

declare void @_ZNK7sc_core9sc_object5printERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object4dumpERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)

define linkonce_odr i8* @_ZNK7sc_core9sc_module4kindEv(%"class.sc_core::sc_module"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str15, i64 0, i64 0)
}

declare %"class.std::vector.10"* @_ZNK7sc_core9sc_module17get_child_objectsEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN7elementD1Ev(%class.element* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7sc_core8sc_eventD1Ev.exit.i:
  %tmp = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !4
  %tmp1 = getelementptr %class.element* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !4
  %tmp2 = getelementptr inbounds %class.element* %this, i64 0, i32 2
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp2)
  %tmp3 = getelementptr inbounds %class.element* %this, i64 0, i32 1
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = getelementptr inbounds %class.element* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  ret void
}

define linkonce_odr void @_ZN7elementD0Ev(%class.element* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7sc_core8sc_eventD1Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !4
  %tmp1 = getelementptr %class.element* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !4
  %tmp2 = getelementptr inbounds %class.element* %this, i64 0, i32 2
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp2)
  %tmp3 = getelementptr inbounds %class.element* %this, i64 0, i32 1
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = getelementptr inbounds %class.element* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  %tmp5 = bitcast %class.element* %this to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  ret void
}

declare void @_ZN7sc_core9sc_module25before_end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module18end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module19start_of_simulationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module17end_of_simulationEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZThn40_N7elementD1Ev(%class.element* %this) {
_ZN7sc_core8sc_eventD1Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.element* %this, i64 -1, i32 2, i32 6, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !4
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !4
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 38
  %tmp3 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 23
  %tmp5 = bitcast %"class.sc_core::sc_thread_process"*** %tmp4 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp5)
  %tmp6 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp6)
  ret void
}

define linkonce_odr void @_ZThn40_N7elementD0Ev(%class.element* %this) {
_ZN7sc_core8sc_eventD1Ev.exit.i.i.i:
  %tmp = getelementptr inbounds %class.element* %this, i64 -1, i32 2, i32 6, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !4
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !4
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 38
  %tmp3 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 23
  %tmp5 = bitcast %"class.sc_core::sc_thread_process"*** %tmp4 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp5)
  %tmp6 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp6)
  %tmp7 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp7) nounwind
  ret void
}

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

declare void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"*, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, double, i32, %"class.sc_core::sc_simcontext"*)

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
!3 = metadata !{metadata !"int", metadata !1}
!4 = metadata !{metadata !"vtable pointer", metadata !2}
!5 = metadata !{metadata !"_ZTSN7sc_core8sc_event8notify_tE", metadata !1}
