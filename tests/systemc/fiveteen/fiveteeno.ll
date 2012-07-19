; ModuleID = 'fiveteeno.bc'
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
%class.top = type { %"class.sc_core::sc_module", i32, i32, i32 }
%"class.sc_core::sc_module" = type { %"class.sc_core::sc_object", %"class.sc_core::sc_process_host", %"class.sc_core::sc_sensitive", %"class.sc_core::sc_sensitive_pos", %"class.sc_core::sc_sensitive_neg", i8, %"class.std::vector"*, i32, %"class.sc_core::sc_name_gen"*, %"class.std::vector.10", %"class.sc_core::sc_module_name"* }
%"class.sc_core::sc_sensitive" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_pos" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_neg" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_module_name" = type { i8*, %"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*, %"class.sc_core::sc_simcontext"*, i8 }
%"class.sc_core::sc_process_handle" = type { %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_spawn_options" = type opaque

@_ZStL8__ioinit = internal global %"class.std::ios_base::Init" zeroinitializer, align 1
@__dso_handle = external global i8
@_ZN7sc_coreL17api_version_checkE = internal global %"class.sc_core::sc_api_version_2_2_0" zeroinitializer, align 1
@_ZTVN10__cxxabiv121__vmi_class_type_infoE = external global i8*
@_ZTVN10__cxxabiv117__class_type_infoE = external global i8*
@.str = private unnamed_addr constant [4 x i8] c"TOP\00", align 1
@_ZTVN10__cxxabiv120__si_class_type_infoE = external global i8*
@_ZTSN7sc_core9sc_objectE = available_externally constant [21 x i8] c"N7sc_core9sc_objectE\00"
@_ZTIN7sc_core9sc_objectE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_objectE, i32 0, i32 0) }
@_ZTV3top = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI3top to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.top*)* @_ZN3topD1Ev to i8*), i8* bitcast (void (%class.top*)* @_ZN3topD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI3top to i8*), i8* bitcast (void (%class.top*)* @_ZThn40_N3topD1Ev to i8*), i8* bitcast (void (%class.top*)* @_ZThn40_N3topD0Ev to i8*)]
@.str8 = private unnamed_addr constant [2 x i8] c"P\00", align 1
@.str9 = private unnamed_addr constant [2 x i8] c"Q\00", align 1
@.str10 = private unnamed_addr constant [2 x i8] c"R\00", align 1
@_ZTS3top = linkonce_odr constant [5 x i8] c"3top\00"
@_ZTSN7sc_core9sc_moduleE = available_externally constant [21 x i8] c"N7sc_core9sc_moduleE\00"
@_ZTSN7sc_core15sc_process_hostE = linkonce_odr constant [28 x i8] c"N7sc_core15sc_process_hostE\00"
@_ZTIN7sc_core15sc_process_hostE = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_process_hostE, i32 0, i32 0) }
@_ZTIN7sc_core9sc_moduleE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_moduleE, i32 0, i32 0), i32 0, i32 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*), i64 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core15sc_process_hostE to i8*), i64 10242 }
@_ZTI3top = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([5 x i8]* @_ZTS3top, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@.str11 = private unnamed_addr constant [10 x i8] c"sc_module\00", align 1
@_ZSt4cout = external global %"class.std::basic_ostream"
@.str12 = private unnamed_addr constant [2 x i8] c"r\00", align 1
@.str13 = private unnamed_addr constant [2 x i8] c"q\00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str14 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str15 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str16 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_increment()\00", align 1
@.str17 = private unnamed_addr constant [2 x i8] c"p\00", align 1
@.str18 = private unnamed_addr constant [7 x i8] c" xyz: \00", align 1
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
_ZN3topC1EN7sc_core14sc_module_nameEiii.exit:
  %TOP = alloca %class.top, align 8
  %tmp = alloca %"class.sc_core::sc_module_name", align 8
  %tmp1 = getelementptr inbounds i8** %argv, i64 1
  %tmp2 = load i8** %tmp1, align 8, !tbaa !0
  %tmp3 = call i32 @atoi(i8* %tmp2) nounwind readonly
  %tmp4 = getelementptr inbounds i8** %argv, i64 2
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = call i32 @atoi(i8* %tmp5) nounwind readonly
  %tmp7 = getelementptr inbounds i8** %argv, i64 3
  %tmp8 = load i8** %tmp7, align 8, !tbaa !0
  %tmp9 = call i32 @atoi(i8* %tmp8) nounwind readonly
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp, i8* getelementptr inbounds ([4 x i8]* @.str, i64 0, i64 0))
  call void @_ZN3topC2EN7sc_core14sc_module_nameEiii(%class.top* %TOP, %"class.sc_core::sc_module_name"* %tmp, i32 %tmp3, i32 %tmp6, i32 %tmp9)
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp)
  call void @_ZN7sc_core8sc_startEv()
  %tmp10 = getelementptr inbounds %class.top* %TOP, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp10)
  ret i32 0
}

declare i32 @atoi(i8* nocapture) nounwind readonly

declare void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"*, i8*)

declare void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core8sc_startEv()

define linkonce_odr void @_ZN3topD1Ev(%class.top* %this) unnamed_addr uwtable inlinehint align 2 {
bb:
  %tmp = getelementptr inbounds %class.top* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp)
  ret void
}

declare void @_ZdlPv(i8*) nounwind

declare noalias i8* @_Znwm(i64)

declare %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"*, i8*, i64)

declare %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"*, i8 signext)

declare void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"*)

declare void @_ZSt16__throw_bad_castv() noreturn

declare %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"*)

define linkonce_odr void @_ZThn40_N3topD1Ev(%class.top* %this) {
bb:
  %tmp = getelementptr inbounds %class.top* %this, i64 -1, i32 0, i32 9, i32 0, i32 0, i32 1
  %tmp1 = bitcast %"class.sc_core::sc_object"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp1)
  ret void
}

declare void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN3topC2EN7sc_core14sc_module_nameEiii(%class.top* %this, %"class.sc_core::sc_module_name"* %name, i32 %a, i32 %b, i32 %c) unnamed_addr uwtable align 2 {
bb:
  %P_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %Q_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp4 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp5 = alloca %"class.sc_core::sc_process_handle", align 8
  %R_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp6 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp7 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp8 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp9 = getelementptr inbounds %class.top* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp9, %"class.sc_core::sc_module_name"* %name)
  %tmp10 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp10, align 8, !tbaa !3
  %tmp11 = getelementptr %class.top* %this, i64 0, i32 0, i32 1
  %tmp12 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp11, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp12, align 8, !tbaa !3
  %tmp13 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  store i32 %a, i32* %tmp13, align 4, !tbaa !4
  %tmp14 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  store i32 %b, i32* %tmp14, align 4, !tbaa !4
  %tmp15 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  store i32 %c, i32* %tmp15, align 4, !tbaa !4
  %tmp16 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp17 = icmp eq %"class.sc_core::sc_simcontext"* %tmp16, null
  br i1 %tmp17, label %.noexc, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc:                                           ; preds = %bb
  %tmp18 = call noalias i8* @_Znwm(i64 248)
  %tmp19 = bitcast i8* %tmp18 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp19)
  store %"class.sc_core::sc_simcontext"* %tmp19, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp19, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc, %bb
  %tmp20 = phi %"class.sc_core::sc_simcontext"* [ %tmp19, %.noexc ], [ %tmp16, %bb ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %P_handle, %"class.sc_core::sc_simcontext"* %tmp20, i8* getelementptr inbounds ([2 x i8]* @.str8, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1PEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp11, %"class.sc_core::sc_spawn_options"* null)
  %tmp21 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %P_handle, i64 0, i32 0
  %tmp22 = load %"class.sc_core::sc_process_b"** %tmp21, align 8, !tbaa !0
  %tmp23 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp22, %"class.sc_core::sc_process_b"** %tmp23, align 8, !tbaa !0
  %tmp24 = icmp eq %"class.sc_core::sc_process_b"* %tmp22, null
  br i1 %tmp24, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb25

bb25:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp26 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp22, i64 0, i32 15
  %tmp27 = load i32* %tmp26, align 4, !tbaa !4
  %tmp28 = icmp eq i32 %tmp27, 0
  br i1 %tmp28, label %bb29, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb29:                                             ; preds = %bb25
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb25
  %tmp30 = add nsw i32 %tmp27, 1
  store i32 %tmp30, i32* %tmp26, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp31 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 2
  %tmp32 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp31, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp33 = load %"class.sc_core::sc_process_b"** %tmp23, align 8, !tbaa !0
  %tmp34 = icmp eq %"class.sc_core::sc_process_b"* %tmp33, null
  br i1 %tmp34, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb35

bb35:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp36 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp33, i64 0, i32 15
  %tmp37 = load i32* %tmp36, align 4, !tbaa !4
  %tmp38 = add nsw i32 %tmp37, -1
  store i32 %tmp38, i32* %tmp36, align 4, !tbaa !4
  %tmp39 = icmp eq i32 %tmp38, 0
  br i1 %tmp39, label %bb40, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb40:                                             ; preds = %bb35
  %tmp41 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp42 = icmp eq %"class.sc_core::sc_process_b"* %tmp41, null
  br i1 %tmp42, label %bb47, label %.noexc11

.noexc11:                                         ; preds = %bb40
  %tmp43 = bitcast %"class.sc_core::sc_process_b"* %tmp41 to void (%"class.sc_core::sc_process_b"*)***
  %tmp44 = load void (%"class.sc_core::sc_process_b"*)*** %tmp43, align 8, !tbaa !3
  %tmp45 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp44, i64 6
  %tmp46 = load void (%"class.sc_core::sc_process_b"*)** %tmp45, align 8
  call void %tmp46(%"class.sc_core::sc_process_b"* %tmp41)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb47

bb47:                                             ; preds = %.noexc11, %bb40
  %tmp48 = phi %"class.sc_core::sc_process_b"* [ null, %bb40 ], [ %.pre.i.i.i, %.noexc11 ]
  %tmp49 = icmp eq %"class.sc_core::sc_process_b"* %tmp48, %tmp33
  br i1 %tmp49, label %bb50, label %bb51

bb50:                                             ; preds = %bb47
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb51:                                             ; preds = %bb47
  store %"class.sc_core::sc_process_b"* %tmp33, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb51, %bb35, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp52 = load %"class.sc_core::sc_process_b"** %tmp21, align 8, !tbaa !0
  %tmp53 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp52, %"class.sc_core::sc_process_b"** %tmp53, align 8, !tbaa !0
  %tmp54 = icmp eq %"class.sc_core::sc_process_b"* %tmp52, null
  br i1 %tmp54, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13, label %bb55

bb55:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp56 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp52, i64 0, i32 15
  %tmp57 = load i32* %tmp56, align 4, !tbaa !4
  %tmp58 = icmp eq i32 %tmp57, 0
  br i1 %tmp58, label %bb59, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12

bb59:                                             ; preds = %bb55
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12: ; preds = %bb55
  %tmp60 = add nsw i32 %tmp57, 1
  store i32 %tmp60, i32* %tmp56, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13

_ZN7sc_core17sc_process_handleC1ERKS0_.exit13:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i12, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp61 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 3
  %tmp62 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp61, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp63 = load %"class.sc_core::sc_process_b"** %tmp53, align 8, !tbaa !0
  %tmp64 = icmp eq %"class.sc_core::sc_process_b"* %tmp63, null
  br i1 %tmp64, label %_ZN7sc_core17sc_process_handleD1Ev.exit16, label %bb65

bb65:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp66 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp63, i64 0, i32 15
  %tmp67 = load i32* %tmp66, align 4, !tbaa !4
  %tmp68 = add nsw i32 %tmp67, -1
  store i32 %tmp68, i32* %tmp66, align 4, !tbaa !4
  %tmp69 = icmp eq i32 %tmp68, 0
  br i1 %tmp69, label %bb70, label %_ZN7sc_core17sc_process_handleD1Ev.exit16

bb70:                                             ; preds = %bb65
  %tmp71 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp72 = icmp eq %"class.sc_core::sc_process_b"* %tmp71, null
  br i1 %tmp72, label %bb77, label %.noexc15

.noexc15:                                         ; preds = %bb70
  %tmp73 = bitcast %"class.sc_core::sc_process_b"* %tmp71 to void (%"class.sc_core::sc_process_b"*)***
  %tmp74 = load void (%"class.sc_core::sc_process_b"*)*** %tmp73, align 8, !tbaa !3
  %tmp75 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp74, i64 6
  %tmp76 = load void (%"class.sc_core::sc_process_b"*)** %tmp75, align 8
  call void %tmp76(%"class.sc_core::sc_process_b"* %tmp71)
  %.pre.i.i.i14 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb77

bb77:                                             ; preds = %.noexc15, %bb70
  %tmp78 = phi %"class.sc_core::sc_process_b"* [ null, %bb70 ], [ %.pre.i.i.i14, %.noexc15 ]
  %tmp79 = icmp eq %"class.sc_core::sc_process_b"* %tmp78, %tmp63
  br i1 %tmp79, label %bb80, label %bb81

bb80:                                             ; preds = %bb77
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb81:                                             ; preds = %bb77
  store %"class.sc_core::sc_process_b"* %tmp63, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit16

_ZN7sc_core17sc_process_handleD1Ev.exit16:        ; preds = %bb81, %bb65, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit13
  %tmp82 = load %"class.sc_core::sc_process_b"** %tmp21, align 8, !tbaa !0
  %tmp83 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp82, %"class.sc_core::sc_process_b"** %tmp83, align 8, !tbaa !0
  %tmp84 = icmp eq %"class.sc_core::sc_process_b"* %tmp82, null
  br i1 %tmp84, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit18, label %bb85

bb85:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit16
  %tmp86 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp82, i64 0, i32 15
  %tmp87 = load i32* %tmp86, align 4, !tbaa !4
  %tmp88 = icmp eq i32 %tmp87, 0
  br i1 %tmp88, label %bb89, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i17

bb89:                                             ; preds = %bb85
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i17: ; preds = %bb85
  %tmp90 = add nsw i32 %tmp87, 1
  store i32 %tmp90, i32* %tmp86, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit18

_ZN7sc_core17sc_process_handleC1ERKS0_.exit18:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i17, %_ZN7sc_core17sc_process_handleD1Ev.exit16
  %tmp91 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 4
  %tmp92 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp91, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp93 = load %"class.sc_core::sc_process_b"** %tmp83, align 8, !tbaa !0
  %tmp94 = icmp eq %"class.sc_core::sc_process_b"* %tmp93, null
  br i1 %tmp94, label %_ZN7sc_core17sc_process_handleD1Ev.exit21, label %bb95

bb95:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit18
  %tmp96 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp93, i64 0, i32 15
  %tmp97 = load i32* %tmp96, align 4, !tbaa !4
  %tmp98 = add nsw i32 %tmp97, -1
  store i32 %tmp98, i32* %tmp96, align 4, !tbaa !4
  %tmp99 = icmp eq i32 %tmp98, 0
  br i1 %tmp99, label %bb100, label %_ZN7sc_core17sc_process_handleD1Ev.exit21

bb100:                                            ; preds = %bb95
  %tmp101 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp102 = icmp eq %"class.sc_core::sc_process_b"* %tmp101, null
  br i1 %tmp102, label %bb107, label %.noexc20

.noexc20:                                         ; preds = %bb100
  %tmp103 = bitcast %"class.sc_core::sc_process_b"* %tmp101 to void (%"class.sc_core::sc_process_b"*)***
  %tmp104 = load void (%"class.sc_core::sc_process_b"*)*** %tmp103, align 8, !tbaa !3
  %tmp105 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp104, i64 6
  %tmp106 = load void (%"class.sc_core::sc_process_b"*)** %tmp105, align 8
  call void %tmp106(%"class.sc_core::sc_process_b"* %tmp101)
  %.pre.i.i.i19 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb107

bb107:                                            ; preds = %.noexc20, %bb100
  %tmp108 = phi %"class.sc_core::sc_process_b"* [ null, %bb100 ], [ %.pre.i.i.i19, %.noexc20 ]
  %tmp109 = icmp eq %"class.sc_core::sc_process_b"* %tmp108, %tmp93
  br i1 %tmp109, label %bb110, label %bb111

bb110:                                            ; preds = %bb107
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb111:                                            ; preds = %bb107
  store %"class.sc_core::sc_process_b"* %tmp93, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit21

_ZN7sc_core17sc_process_handleD1Ev.exit21:        ; preds = %bb111, %bb95, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit18
  %tmp112 = load %"class.sc_core::sc_process_b"** %tmp21, align 8, !tbaa !0
  %tmp113 = icmp eq %"class.sc_core::sc_process_b"* %tmp112, null
  br i1 %tmp113, label %_ZN7sc_core17sc_process_handleD1Ev.exit24, label %bb114

bb114:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit21
  %tmp115 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp112, i64 0, i32 15
  %tmp116 = load i32* %tmp115, align 4, !tbaa !4
  %tmp117 = add nsw i32 %tmp116, -1
  store i32 %tmp117, i32* %tmp115, align 4, !tbaa !4
  %tmp118 = icmp eq i32 %tmp117, 0
  br i1 %tmp118, label %bb119, label %_ZN7sc_core17sc_process_handleD1Ev.exit24

bb119:                                            ; preds = %bb114
  %tmp120 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp121 = icmp eq %"class.sc_core::sc_process_b"* %tmp120, null
  br i1 %tmp121, label %bb126, label %.noexc23

.noexc23:                                         ; preds = %bb119
  %tmp122 = bitcast %"class.sc_core::sc_process_b"* %tmp120 to void (%"class.sc_core::sc_process_b"*)***
  %tmp123 = load void (%"class.sc_core::sc_process_b"*)*** %tmp122, align 8, !tbaa !3
  %tmp124 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp123, i64 6
  %tmp125 = load void (%"class.sc_core::sc_process_b"*)** %tmp124, align 8
  call void %tmp125(%"class.sc_core::sc_process_b"* %tmp120)
  %.pre.i.i.i22 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb126

bb126:                                            ; preds = %.noexc23, %bb119
  %tmp127 = phi %"class.sc_core::sc_process_b"* [ null, %bb119 ], [ %.pre.i.i.i22, %.noexc23 ]
  %tmp128 = icmp eq %"class.sc_core::sc_process_b"* %tmp127, %tmp112
  br i1 %tmp128, label %bb129, label %bb130

bb129:                                            ; preds = %bb126
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb130:                                            ; preds = %bb126
  store %"class.sc_core::sc_process_b"* %tmp112, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit24

_ZN7sc_core17sc_process_handleD1Ev.exit24:        ; preds = %bb130, %bb114, %_ZN7sc_core17sc_process_handleD1Ev.exit21
  %tmp131 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp132 = icmp eq %"class.sc_core::sc_simcontext"* %tmp131, null
  br i1 %tmp132, label %.noexc25, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit28

.noexc25:                                         ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit24
  %tmp133 = call noalias i8* @_Znwm(i64 248)
  %tmp134 = bitcast i8* %tmp133 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp134)
  store %"class.sc_core::sc_simcontext"* %tmp134, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp134, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit28

_ZN7sc_core22sc_get_curr_simcontextEv.exit28:     ; preds = %.noexc25, %_ZN7sc_core17sc_process_handleD1Ev.exit24
  %tmp135 = phi %"class.sc_core::sc_simcontext"* [ %tmp134, %.noexc25 ], [ %tmp131, %_ZN7sc_core17sc_process_handleD1Ev.exit24 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %Q_handle, %"class.sc_core::sc_simcontext"* %tmp135, i8* getelementptr inbounds ([2 x i8]* @.str9, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1QEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp11, %"class.sc_core::sc_spawn_options"* null)
  %tmp136 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %Q_handle, i64 0, i32 0
  %tmp137 = load %"class.sc_core::sc_process_b"** %tmp136, align 8, !tbaa !0
  %tmp138 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp3, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp137, %"class.sc_core::sc_process_b"** %tmp138, align 8, !tbaa !0
  %tmp139 = icmp eq %"class.sc_core::sc_process_b"* %tmp137, null
  br i1 %tmp139, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit30, label %bb140

bb140:                                            ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit28
  %tmp141 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp137, i64 0, i32 15
  %tmp142 = load i32* %tmp141, align 4, !tbaa !4
  %tmp143 = icmp eq i32 %tmp142, 0
  br i1 %tmp143, label %bb144, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i29

bb144:                                            ; preds = %bb140
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i29: ; preds = %bb140
  %tmp145 = add nsw i32 %tmp142, 1
  store i32 %tmp145, i32* %tmp141, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit30

_ZN7sc_core17sc_process_handleC1ERKS0_.exit30:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i29, %_ZN7sc_core22sc_get_curr_simcontextEv.exit28
  %tmp146 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp31, %"class.sc_core::sc_process_handle"* %tmp3)
  %tmp147 = load %"class.sc_core::sc_process_b"** %tmp138, align 8, !tbaa !0
  %tmp148 = icmp eq %"class.sc_core::sc_process_b"* %tmp147, null
  br i1 %tmp148, label %_ZN7sc_core17sc_process_handleD1Ev.exit33, label %bb149

bb149:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit30
  %tmp150 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp147, i64 0, i32 15
  %tmp151 = load i32* %tmp150, align 4, !tbaa !4
  %tmp152 = add nsw i32 %tmp151, -1
  store i32 %tmp152, i32* %tmp150, align 4, !tbaa !4
  %tmp153 = icmp eq i32 %tmp152, 0
  br i1 %tmp153, label %bb154, label %_ZN7sc_core17sc_process_handleD1Ev.exit33

bb154:                                            ; preds = %bb149
  %tmp155 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp156 = icmp eq %"class.sc_core::sc_process_b"* %tmp155, null
  br i1 %tmp156, label %bb161, label %.noexc32

.noexc32:                                         ; preds = %bb154
  %tmp157 = bitcast %"class.sc_core::sc_process_b"* %tmp155 to void (%"class.sc_core::sc_process_b"*)***
  %tmp158 = load void (%"class.sc_core::sc_process_b"*)*** %tmp157, align 8, !tbaa !3
  %tmp159 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp158, i64 6
  %tmp160 = load void (%"class.sc_core::sc_process_b"*)** %tmp159, align 8
  call void %tmp160(%"class.sc_core::sc_process_b"* %tmp155)
  %.pre.i.i.i31 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb161

bb161:                                            ; preds = %.noexc32, %bb154
  %tmp162 = phi %"class.sc_core::sc_process_b"* [ null, %bb154 ], [ %.pre.i.i.i31, %.noexc32 ]
  %tmp163 = icmp eq %"class.sc_core::sc_process_b"* %tmp162, %tmp147
  br i1 %tmp163, label %bb164, label %bb165

bb164:                                            ; preds = %bb161
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb165:                                            ; preds = %bb161
  store %"class.sc_core::sc_process_b"* %tmp147, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit33

_ZN7sc_core17sc_process_handleD1Ev.exit33:        ; preds = %bb165, %bb149, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit30
  %tmp166 = load %"class.sc_core::sc_process_b"** %tmp136, align 8, !tbaa !0
  %tmp167 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp4, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp166, %"class.sc_core::sc_process_b"** %tmp167, align 8, !tbaa !0
  %tmp168 = icmp eq %"class.sc_core::sc_process_b"* %tmp166, null
  br i1 %tmp168, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit35, label %bb169

bb169:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit33
  %tmp170 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp166, i64 0, i32 15
  %tmp171 = load i32* %tmp170, align 4, !tbaa !4
  %tmp172 = icmp eq i32 %tmp171, 0
  br i1 %tmp172, label %bb173, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i34

bb173:                                            ; preds = %bb169
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i34: ; preds = %bb169
  %tmp174 = add nsw i32 %tmp171, 1
  store i32 %tmp174, i32* %tmp170, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit35

_ZN7sc_core17sc_process_handleC1ERKS0_.exit35:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i34, %_ZN7sc_core17sc_process_handleD1Ev.exit33
  %tmp175 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp61, %"class.sc_core::sc_process_handle"* %tmp4)
  %tmp176 = load %"class.sc_core::sc_process_b"** %tmp167, align 8, !tbaa !0
  %tmp177 = icmp eq %"class.sc_core::sc_process_b"* %tmp176, null
  br i1 %tmp177, label %_ZN7sc_core17sc_process_handleD1Ev.exit38, label %bb178

bb178:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit35
  %tmp179 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp176, i64 0, i32 15
  %tmp180 = load i32* %tmp179, align 4, !tbaa !4
  %tmp181 = add nsw i32 %tmp180, -1
  store i32 %tmp181, i32* %tmp179, align 4, !tbaa !4
  %tmp182 = icmp eq i32 %tmp181, 0
  br i1 %tmp182, label %bb183, label %_ZN7sc_core17sc_process_handleD1Ev.exit38

bb183:                                            ; preds = %bb178
  %tmp184 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp185 = icmp eq %"class.sc_core::sc_process_b"* %tmp184, null
  br i1 %tmp185, label %bb190, label %.noexc37

.noexc37:                                         ; preds = %bb183
  %tmp186 = bitcast %"class.sc_core::sc_process_b"* %tmp184 to void (%"class.sc_core::sc_process_b"*)***
  %tmp187 = load void (%"class.sc_core::sc_process_b"*)*** %tmp186, align 8, !tbaa !3
  %tmp188 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp187, i64 6
  %tmp189 = load void (%"class.sc_core::sc_process_b"*)** %tmp188, align 8
  call void %tmp189(%"class.sc_core::sc_process_b"* %tmp184)
  %.pre.i.i.i36 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb190

bb190:                                            ; preds = %.noexc37, %bb183
  %tmp191 = phi %"class.sc_core::sc_process_b"* [ null, %bb183 ], [ %.pre.i.i.i36, %.noexc37 ]
  %tmp192 = icmp eq %"class.sc_core::sc_process_b"* %tmp191, %tmp176
  br i1 %tmp192, label %bb193, label %bb194

bb193:                                            ; preds = %bb190
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb194:                                            ; preds = %bb190
  store %"class.sc_core::sc_process_b"* %tmp176, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit38

_ZN7sc_core17sc_process_handleD1Ev.exit38:        ; preds = %bb194, %bb178, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit35
  %tmp195 = load %"class.sc_core::sc_process_b"** %tmp136, align 8, !tbaa !0
  %tmp196 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp5, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp195, %"class.sc_core::sc_process_b"** %tmp196, align 8, !tbaa !0
  %tmp197 = icmp eq %"class.sc_core::sc_process_b"* %tmp195, null
  br i1 %tmp197, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit40, label %bb198

bb198:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit38
  %tmp199 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp195, i64 0, i32 15
  %tmp200 = load i32* %tmp199, align 4, !tbaa !4
  %tmp201 = icmp eq i32 %tmp200, 0
  br i1 %tmp201, label %bb202, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i39

bb202:                                            ; preds = %bb198
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i39: ; preds = %bb198
  %tmp203 = add nsw i32 %tmp200, 1
  store i32 %tmp203, i32* %tmp199, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit40

_ZN7sc_core17sc_process_handleC1ERKS0_.exit40:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i39, %_ZN7sc_core17sc_process_handleD1Ev.exit38
  %tmp204 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp91, %"class.sc_core::sc_process_handle"* %tmp5)
  %tmp205 = load %"class.sc_core::sc_process_b"** %tmp196, align 8, !tbaa !0
  %tmp206 = icmp eq %"class.sc_core::sc_process_b"* %tmp205, null
  br i1 %tmp206, label %_ZN7sc_core17sc_process_handleD1Ev.exit43, label %bb207

bb207:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit40
  %tmp208 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp205, i64 0, i32 15
  %tmp209 = load i32* %tmp208, align 4, !tbaa !4
  %tmp210 = add nsw i32 %tmp209, -1
  store i32 %tmp210, i32* %tmp208, align 4, !tbaa !4
  %tmp211 = icmp eq i32 %tmp210, 0
  br i1 %tmp211, label %bb212, label %_ZN7sc_core17sc_process_handleD1Ev.exit43

bb212:                                            ; preds = %bb207
  %tmp213 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp214 = icmp eq %"class.sc_core::sc_process_b"* %tmp213, null
  br i1 %tmp214, label %bb219, label %.noexc42

.noexc42:                                         ; preds = %bb212
  %tmp215 = bitcast %"class.sc_core::sc_process_b"* %tmp213 to void (%"class.sc_core::sc_process_b"*)***
  %tmp216 = load void (%"class.sc_core::sc_process_b"*)*** %tmp215, align 8, !tbaa !3
  %tmp217 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp216, i64 6
  %tmp218 = load void (%"class.sc_core::sc_process_b"*)** %tmp217, align 8
  call void %tmp218(%"class.sc_core::sc_process_b"* %tmp213)
  %.pre.i.i.i41 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb219

bb219:                                            ; preds = %.noexc42, %bb212
  %tmp220 = phi %"class.sc_core::sc_process_b"* [ null, %bb212 ], [ %.pre.i.i.i41, %.noexc42 ]
  %tmp221 = icmp eq %"class.sc_core::sc_process_b"* %tmp220, %tmp205
  br i1 %tmp221, label %bb222, label %bb223

bb222:                                            ; preds = %bb219
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb223:                                            ; preds = %bb219
  store %"class.sc_core::sc_process_b"* %tmp205, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit43

_ZN7sc_core17sc_process_handleD1Ev.exit43:        ; preds = %bb223, %bb207, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit40
  %tmp224 = load %"class.sc_core::sc_process_b"** %tmp136, align 8, !tbaa !0
  %tmp225 = icmp eq %"class.sc_core::sc_process_b"* %tmp224, null
  br i1 %tmp225, label %_ZN7sc_core17sc_process_handleD1Ev.exit46, label %bb226

bb226:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit43
  %tmp227 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp224, i64 0, i32 15
  %tmp228 = load i32* %tmp227, align 4, !tbaa !4
  %tmp229 = add nsw i32 %tmp228, -1
  store i32 %tmp229, i32* %tmp227, align 4, !tbaa !4
  %tmp230 = icmp eq i32 %tmp229, 0
  br i1 %tmp230, label %bb231, label %_ZN7sc_core17sc_process_handleD1Ev.exit46

bb231:                                            ; preds = %bb226
  %tmp232 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp233 = icmp eq %"class.sc_core::sc_process_b"* %tmp232, null
  br i1 %tmp233, label %bb238, label %.noexc45

.noexc45:                                         ; preds = %bb231
  %tmp234 = bitcast %"class.sc_core::sc_process_b"* %tmp232 to void (%"class.sc_core::sc_process_b"*)***
  %tmp235 = load void (%"class.sc_core::sc_process_b"*)*** %tmp234, align 8, !tbaa !3
  %tmp236 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp235, i64 6
  %tmp237 = load void (%"class.sc_core::sc_process_b"*)** %tmp236, align 8
  call void %tmp237(%"class.sc_core::sc_process_b"* %tmp232)
  %.pre.i.i.i44 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb238

bb238:                                            ; preds = %.noexc45, %bb231
  %tmp239 = phi %"class.sc_core::sc_process_b"* [ null, %bb231 ], [ %.pre.i.i.i44, %.noexc45 ]
  %tmp240 = icmp eq %"class.sc_core::sc_process_b"* %tmp239, %tmp224
  br i1 %tmp240, label %bb241, label %bb242

bb241:                                            ; preds = %bb238
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb242:                                            ; preds = %bb238
  store %"class.sc_core::sc_process_b"* %tmp224, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit46

_ZN7sc_core17sc_process_handleD1Ev.exit46:        ; preds = %bb242, %bb226, %_ZN7sc_core17sc_process_handleD1Ev.exit43
  %tmp243 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp244 = icmp eq %"class.sc_core::sc_simcontext"* %tmp243, null
  br i1 %tmp244, label %.noexc47, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit50

.noexc47:                                         ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit46
  %tmp245 = call noalias i8* @_Znwm(i64 248)
  %tmp246 = bitcast i8* %tmp245 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp246)
  store %"class.sc_core::sc_simcontext"* %tmp246, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp246, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit50

_ZN7sc_core22sc_get_curr_simcontextEv.exit50:     ; preds = %.noexc47, %_ZN7sc_core17sc_process_handleD1Ev.exit46
  %tmp247 = phi %"class.sc_core::sc_simcontext"* [ %tmp246, %.noexc47 ], [ %tmp243, %_ZN7sc_core17sc_process_handleD1Ev.exit46 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %R_handle, %"class.sc_core::sc_simcontext"* %tmp247, i8* getelementptr inbounds ([2 x i8]* @.str10, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1REv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp11, %"class.sc_core::sc_spawn_options"* null)
  %tmp248 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %R_handle, i64 0, i32 0
  %tmp249 = load %"class.sc_core::sc_process_b"** %tmp248, align 8, !tbaa !0
  %tmp250 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp6, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp249, %"class.sc_core::sc_process_b"** %tmp250, align 8, !tbaa !0
  %tmp251 = icmp eq %"class.sc_core::sc_process_b"* %tmp249, null
  br i1 %tmp251, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit52, label %bb252

bb252:                                            ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit50
  %tmp253 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp249, i64 0, i32 15
  %tmp254 = load i32* %tmp253, align 4, !tbaa !4
  %tmp255 = icmp eq i32 %tmp254, 0
  br i1 %tmp255, label %bb256, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i51

bb256:                                            ; preds = %bb252
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i51: ; preds = %bb252
  %tmp257 = add nsw i32 %tmp254, 1
  store i32 %tmp257, i32* %tmp253, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit52

_ZN7sc_core17sc_process_handleC1ERKS0_.exit52:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i51, %_ZN7sc_core22sc_get_curr_simcontextEv.exit50
  %tmp258 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp31, %"class.sc_core::sc_process_handle"* %tmp6)
  %tmp259 = load %"class.sc_core::sc_process_b"** %tmp250, align 8, !tbaa !0
  %tmp260 = icmp eq %"class.sc_core::sc_process_b"* %tmp259, null
  br i1 %tmp260, label %_ZN7sc_core17sc_process_handleD1Ev.exit55, label %bb261

bb261:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit52
  %tmp262 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp259, i64 0, i32 15
  %tmp263 = load i32* %tmp262, align 4, !tbaa !4
  %tmp264 = add nsw i32 %tmp263, -1
  store i32 %tmp264, i32* %tmp262, align 4, !tbaa !4
  %tmp265 = icmp eq i32 %tmp264, 0
  br i1 %tmp265, label %bb266, label %_ZN7sc_core17sc_process_handleD1Ev.exit55

bb266:                                            ; preds = %bb261
  %tmp267 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp268 = icmp eq %"class.sc_core::sc_process_b"* %tmp267, null
  br i1 %tmp268, label %bb273, label %.noexc54

.noexc54:                                         ; preds = %bb266
  %tmp269 = bitcast %"class.sc_core::sc_process_b"* %tmp267 to void (%"class.sc_core::sc_process_b"*)***
  %tmp270 = load void (%"class.sc_core::sc_process_b"*)*** %tmp269, align 8, !tbaa !3
  %tmp271 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp270, i64 6
  %tmp272 = load void (%"class.sc_core::sc_process_b"*)** %tmp271, align 8
  call void %tmp272(%"class.sc_core::sc_process_b"* %tmp267)
  %.pre.i.i.i53 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb273

bb273:                                            ; preds = %.noexc54, %bb266
  %tmp274 = phi %"class.sc_core::sc_process_b"* [ null, %bb266 ], [ %.pre.i.i.i53, %.noexc54 ]
  %tmp275 = icmp eq %"class.sc_core::sc_process_b"* %tmp274, %tmp259
  br i1 %tmp275, label %bb276, label %bb277

bb276:                                            ; preds = %bb273
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb277:                                            ; preds = %bb273
  store %"class.sc_core::sc_process_b"* %tmp259, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit55

_ZN7sc_core17sc_process_handleD1Ev.exit55:        ; preds = %bb277, %bb261, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit52
  %tmp278 = load %"class.sc_core::sc_process_b"** %tmp248, align 8, !tbaa !0
  %tmp279 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp7, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp278, %"class.sc_core::sc_process_b"** %tmp279, align 8, !tbaa !0
  %tmp280 = icmp eq %"class.sc_core::sc_process_b"* %tmp278, null
  br i1 %tmp280, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit57, label %bb281

bb281:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit55
  %tmp282 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp278, i64 0, i32 15
  %tmp283 = load i32* %tmp282, align 4, !tbaa !4
  %tmp284 = icmp eq i32 %tmp283, 0
  br i1 %tmp284, label %bb285, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i56

bb285:                                            ; preds = %bb281
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i56: ; preds = %bb281
  %tmp286 = add nsw i32 %tmp283, 1
  store i32 %tmp286, i32* %tmp282, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit57

_ZN7sc_core17sc_process_handleC1ERKS0_.exit57:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i56, %_ZN7sc_core17sc_process_handleD1Ev.exit55
  %tmp287 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp61, %"class.sc_core::sc_process_handle"* %tmp7)
  %tmp288 = load %"class.sc_core::sc_process_b"** %tmp279, align 8, !tbaa !0
  %tmp289 = icmp eq %"class.sc_core::sc_process_b"* %tmp288, null
  br i1 %tmp289, label %_ZN7sc_core17sc_process_handleD1Ev.exit60, label %bb290

bb290:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit57
  %tmp291 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp288, i64 0, i32 15
  %tmp292 = load i32* %tmp291, align 4, !tbaa !4
  %tmp293 = add nsw i32 %tmp292, -1
  store i32 %tmp293, i32* %tmp291, align 4, !tbaa !4
  %tmp294 = icmp eq i32 %tmp293, 0
  br i1 %tmp294, label %bb295, label %_ZN7sc_core17sc_process_handleD1Ev.exit60

bb295:                                            ; preds = %bb290
  %tmp296 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp297 = icmp eq %"class.sc_core::sc_process_b"* %tmp296, null
  br i1 %tmp297, label %bb302, label %.noexc59

.noexc59:                                         ; preds = %bb295
  %tmp298 = bitcast %"class.sc_core::sc_process_b"* %tmp296 to void (%"class.sc_core::sc_process_b"*)***
  %tmp299 = load void (%"class.sc_core::sc_process_b"*)*** %tmp298, align 8, !tbaa !3
  %tmp300 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp299, i64 6
  %tmp301 = load void (%"class.sc_core::sc_process_b"*)** %tmp300, align 8
  call void %tmp301(%"class.sc_core::sc_process_b"* %tmp296)
  %.pre.i.i.i58 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb302

bb302:                                            ; preds = %.noexc59, %bb295
  %tmp303 = phi %"class.sc_core::sc_process_b"* [ null, %bb295 ], [ %.pre.i.i.i58, %.noexc59 ]
  %tmp304 = icmp eq %"class.sc_core::sc_process_b"* %tmp303, %tmp288
  br i1 %tmp304, label %bb305, label %bb306

bb305:                                            ; preds = %bb302
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb306:                                            ; preds = %bb302
  store %"class.sc_core::sc_process_b"* %tmp288, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit60

_ZN7sc_core17sc_process_handleD1Ev.exit60:        ; preds = %bb306, %bb290, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit57
  %tmp307 = load %"class.sc_core::sc_process_b"** %tmp248, align 8, !tbaa !0
  %tmp308 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp8, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp307, %"class.sc_core::sc_process_b"** %tmp308, align 8, !tbaa !0
  %tmp309 = icmp eq %"class.sc_core::sc_process_b"* %tmp307, null
  br i1 %tmp309, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit62, label %bb310

bb310:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit60
  %tmp311 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp307, i64 0, i32 15
  %tmp312 = load i32* %tmp311, align 4, !tbaa !4
  %tmp313 = icmp eq i32 %tmp312, 0
  br i1 %tmp313, label %bb314, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i61

bb314:                                            ; preds = %bb310
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i61: ; preds = %bb310
  %tmp315 = add nsw i32 %tmp312, 1
  store i32 %tmp315, i32* %tmp311, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit62

_ZN7sc_core17sc_process_handleC1ERKS0_.exit62:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i61, %_ZN7sc_core17sc_process_handleD1Ev.exit60
  %tmp316 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp91, %"class.sc_core::sc_process_handle"* %tmp8)
  %tmp317 = load %"class.sc_core::sc_process_b"** %tmp308, align 8, !tbaa !0
  %tmp318 = icmp eq %"class.sc_core::sc_process_b"* %tmp317, null
  br i1 %tmp318, label %_ZN7sc_core17sc_process_handleD1Ev.exit65, label %bb319

bb319:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit62
  %tmp320 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp317, i64 0, i32 15
  %tmp321 = load i32* %tmp320, align 4, !tbaa !4
  %tmp322 = add nsw i32 %tmp321, -1
  store i32 %tmp322, i32* %tmp320, align 4, !tbaa !4
  %tmp323 = icmp eq i32 %tmp322, 0
  br i1 %tmp323, label %bb324, label %_ZN7sc_core17sc_process_handleD1Ev.exit65

bb324:                                            ; preds = %bb319
  %tmp325 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp326 = icmp eq %"class.sc_core::sc_process_b"* %tmp325, null
  br i1 %tmp326, label %bb331, label %.noexc64

.noexc64:                                         ; preds = %bb324
  %tmp327 = bitcast %"class.sc_core::sc_process_b"* %tmp325 to void (%"class.sc_core::sc_process_b"*)***
  %tmp328 = load void (%"class.sc_core::sc_process_b"*)*** %tmp327, align 8, !tbaa !3
  %tmp329 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp328, i64 6
  %tmp330 = load void (%"class.sc_core::sc_process_b"*)** %tmp329, align 8
  call void %tmp330(%"class.sc_core::sc_process_b"* %tmp325)
  %.pre.i.i.i63 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb331

bb331:                                            ; preds = %.noexc64, %bb324
  %tmp332 = phi %"class.sc_core::sc_process_b"* [ null, %bb324 ], [ %.pre.i.i.i63, %.noexc64 ]
  %tmp333 = icmp eq %"class.sc_core::sc_process_b"* %tmp332, %tmp317
  br i1 %tmp333, label %bb334, label %bb335

bb334:                                            ; preds = %bb331
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb335:                                            ; preds = %bb331
  store %"class.sc_core::sc_process_b"* %tmp317, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit65

_ZN7sc_core17sc_process_handleD1Ev.exit65:        ; preds = %bb335, %bb319, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit62
  %tmp336 = load %"class.sc_core::sc_process_b"** %tmp248, align 8, !tbaa !0
  %tmp337 = icmp eq %"class.sc_core::sc_process_b"* %tmp336, null
  br i1 %tmp337, label %_ZN7sc_core17sc_process_handleD1Ev.exit68, label %bb338

bb338:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit65
  %tmp339 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp336, i64 0, i32 15
  %tmp340 = load i32* %tmp339, align 4, !tbaa !4
  %tmp341 = add nsw i32 %tmp340, -1
  store i32 %tmp341, i32* %tmp339, align 4, !tbaa !4
  %tmp342 = icmp eq i32 %tmp341, 0
  br i1 %tmp342, label %bb343, label %_ZN7sc_core17sc_process_handleD1Ev.exit68

bb343:                                            ; preds = %bb338
  %tmp344 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp345 = icmp eq %"class.sc_core::sc_process_b"* %tmp344, null
  br i1 %tmp345, label %bb350, label %.noexc67

.noexc67:                                         ; preds = %bb343
  %tmp346 = bitcast %"class.sc_core::sc_process_b"* %tmp344 to void (%"class.sc_core::sc_process_b"*)***
  %tmp347 = load void (%"class.sc_core::sc_process_b"*)*** %tmp346, align 8, !tbaa !3
  %tmp348 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp347, i64 6
  %tmp349 = load void (%"class.sc_core::sc_process_b"*)** %tmp348, align 8
  call void %tmp349(%"class.sc_core::sc_process_b"* %tmp344)
  %.pre.i.i.i66 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb350

bb350:                                            ; preds = %.noexc67, %bb343
  %tmp351 = phi %"class.sc_core::sc_process_b"* [ null, %bb343 ], [ %.pre.i.i.i66, %.noexc67 ]
  %tmp352 = icmp eq %"class.sc_core::sc_process_b"* %tmp351, %tmp336
  br i1 %tmp352, label %bb353, label %bb354

bb353:                                            ; preds = %bb350
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb354:                                            ; preds = %bb350
  store %"class.sc_core::sc_process_b"* %tmp336, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit68

_ZN7sc_core17sc_process_handleD1Ev.exit68:        ; preds = %bb354, %bb338, %_ZN7sc_core17sc_process_handleD1Ev.exit65
  ret void
}

declare void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

define linkonce_odr void @_ZN3top1PEv(%class.top* nocapture %this) uwtable align 2 {
bb:
  %tmp = alloca %"class.sc_core::sc_time", align 8
  %tmp1 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([2 x i8]* @.str17, i64 0, i64 0), i64 1)
  %tmp2 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  %tmp3 = load i32* %tmp2, align 4, !tbaa !4
  %tmp4 = icmp eq i32 %tmp3, 0
  %tmp5 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  br i1 %tmp4, label %bb6, label %._crit_edge

bb6:                                              ; preds = %bb
  %tmp7 = alloca %"class.sc_core::sc_time", align 8
  %tmp8 = load i32* %tmp5, align 4, !tbaa !4
  %tmp9 = icmp sgt i32 %tmp8, 0
  br i1 %tmp9, label %bb14, label %bb10

bb10:                                             ; preds = %bb6
  %tmp11 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  %tmp12 = load i32* %tmp11, align 4, !tbaa !4
  %tmp13 = icmp sgt i32 %tmp12, 0
  br i1 %tmp13, label %bb14, label %._crit_edge

bb14:                                             ; preds = %bb10, %bb6
  %tmp15 = bitcast %"class.sc_core::sc_time"* %tmp7 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp15)
  %tmp16 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp17 = load %"class.sc_core::sc_simcontext"** %tmp16, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp7, double 1.000000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp17)
  %tmp18 = load %"class.sc_core::sc_simcontext"** %tmp16, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp7, %"class.sc_core::sc_simcontext"* %tmp18)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp15)
  store i32 1, i32* %tmp2, align 4, !tbaa !4
  br label %._crit_edge

._crit_edge:                                      ; preds = %bb14, %bb10, %bb
  %tmp19 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([2 x i8]* @.str17, i64 0, i64 0), i64 1)
  %tmp20 = bitcast %"class.sc_core::sc_time"* %tmp to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp20)
  %tmp21 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp22 = load %"class.sc_core::sc_simcontext"** %tmp21, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, double 2.000000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp22)
  %tmp23 = load %"class.sc_core::sc_simcontext"** %tmp21, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, %"class.sc_core::sc_simcontext"* %tmp23)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp20)
  store i32 0, i32* %tmp2, align 4, !tbaa !4
  %tmp24 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([7 x i8]* @.str18, i64 0, i64 0), i64 6)
  %tmp25 = load i32* %tmp2, align 4, !tbaa !4
  %tmp26 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp25)
  %tmp27 = load i32* %tmp5, align 4, !tbaa !4
  %tmp28 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp26, i32 %tmp27)
  %tmp29 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  %tmp30 = load i32* %tmp29, align 4, !tbaa !4
  %tmp31 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp28, i32 %tmp30)
  %tmp32 = bitcast %"class.std::basic_ostream"* %tmp31 to i8**
  %tmp33 = load i8** %tmp32, align 8, !tbaa !3
  %tmp34 = getelementptr i8* %tmp33, i64 -24
  %tmp35 = bitcast i8* %tmp34 to i64*
  %tmp36 = load i64* %tmp35, align 8
  %tmp37 = bitcast %"class.std::basic_ostream"* %tmp31 to i8*
  %.sum.i = add i64 %tmp36, 240
  %tmp38 = getelementptr inbounds i8* %tmp37, i64 %.sum.i
  %tmp39 = bitcast i8* %tmp38 to %"class.std::ctype"**
  %tmp40 = load %"class.std::ctype"** %tmp39, align 8, !tbaa !0
  %tmp41 = icmp eq %"class.std::ctype"* %tmp40, null
  br i1 %tmp41, label %bb42, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb42:                                             ; preds = %._crit_edge
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %._crit_edge
  %tmp43 = getelementptr inbounds %"class.std::ctype"* %tmp40, i64 0, i32 6
  %tmp44 = load i8* %tmp43, align 1, !tbaa !1
  %tmp45 = icmp eq i8 %tmp44, 0
  br i1 %tmp45, label %bb49, label %bb46

bb46:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp47 = getelementptr inbounds %"class.std::ctype"* %tmp40, i64 0, i32 7, i64 10
  %tmp48 = load i8* %tmp47, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb49:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp40)
  %tmp50 = bitcast %"class.std::ctype"* %tmp40 to i8 (%"class.std::ctype"*, i8)***
  %tmp51 = load i8 (%"class.std::ctype"*, i8)*** %tmp50, align 8, !tbaa !3
  %tmp52 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp51, i64 6
  %tmp53 = load i8 (%"class.std::ctype"*, i8)** %tmp52, align 8
  %tmp54 = call signext i8 %tmp53(%"class.std::ctype"* %tmp40, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb49, %bb46
  %.0.i.i.i = phi i8 [ %tmp48, %bb46 ], [ %tmp54, %bb49 ]
  %tmp55 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp31, i8 signext %.0.i.i.i)
  %tmp56 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp55)
  ret void
}

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

define linkonce_odr void @_ZN3top1QEv(%class.top* nocapture %this) uwtable align 2 {
bb:
  %tmp = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([2 x i8]* @.str13, i64 0, i64 0), i64 1)
  %tmp1 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  %tmp2 = load i32* %tmp1, align 4, !tbaa !4
  %tmp3 = icmp eq i32 %tmp2, 0
  %tmp4 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  br i1 %tmp3, label %bb5, label %._crit_edge

bb5:                                              ; preds = %bb
  %tmp6 = alloca %"class.sc_core::sc_time", align 8
  %tmp7 = load i32* %tmp4, align 4, !tbaa !4
  %tmp8 = icmp sgt i32 %tmp7, 0
  br i1 %tmp8, label %bb13, label %bb9

bb9:                                              ; preds = %bb5
  %tmp10 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  %tmp11 = load i32* %tmp10, align 4, !tbaa !4
  %tmp12 = icmp sgt i32 %tmp11, 0
  br i1 %tmp12, label %bb13, label %._crit_edge

bb13:                                             ; preds = %bb9, %bb5
  %tmp14 = bitcast %"class.sc_core::sc_time"* %tmp6 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp14)
  %tmp15 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp16 = load %"class.sc_core::sc_simcontext"** %tmp15, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp6, double 1.000000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp16)
  %tmp17 = load %"class.sc_core::sc_simcontext"** %tmp15, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp6, %"class.sc_core::sc_simcontext"* %tmp17)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp14)
  store i32 1, i32* %tmp1, align 4, !tbaa !4
  br label %._crit_edge

._crit_edge:                                      ; preds = %bb13, %bb9, %bb
  %tmp18 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  store i32 1, i32* %tmp18, align 4, !tbaa !4
  store i32 2, i32* %tmp4, align 4, !tbaa !4
  ret void
}

define linkonce_odr void @_ZN3top1REv(%class.top* nocapture %this) uwtable align 2 {
bb:
  %tmp = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([2 x i8]* @.str12, i64 0, i64 0), i64 1)
  %tmp1 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  %tmp2 = load i32* %tmp1, align 4, !tbaa !4
  %tmp3 = icmp eq i32 %tmp2, 0
  br i1 %tmp3, label %bb4, label %bb14

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  %tmp6 = load i32* %tmp5, align 4, !tbaa !4
  %tmp7 = icmp sgt i32 %tmp6, 0
  br i1 %tmp7, label %bb8, label %bb14

bb8:                                              ; preds = %bb4
  %tmp9 = alloca %"class.sc_core::sc_time", align 8
  %tmp10 = bitcast %"class.sc_core::sc_time"* %tmp9 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp10)
  %tmp11 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp12 = load %"class.sc_core::sc_simcontext"** %tmp11, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp9, double 1.000000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp12)
  %tmp13 = load %"class.sc_core::sc_simcontext"** %tmp11, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp9, %"class.sc_core::sc_simcontext"* %tmp13)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp10)
  store i32 1, i32* %tmp1, align 4, !tbaa !4
  br label %bb14

bb14:                                             ; preds = %bb8, %bb4, %bb
  store i32 2, i32* %tmp1, align 4, !tbaa !4
  %tmp15 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  store i32 2, i32* %tmp15, align 4, !tbaa !4
  ret void
}

declare void @_ZNK7sc_core9sc_object5printERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object4dumpERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)

define linkonce_odr i8* @_ZNK7sc_core9sc_module4kindEv(%"class.sc_core::sc_module"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str11, i64 0, i64 0)
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
  %tmp = getelementptr inbounds %class.top* %this, i64 -1, i32 0, i32 9, i32 0, i32 0, i32 1
  %tmp1 = bitcast %"class.sc_core::sc_object"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp1)
  %tmp2 = bitcast %"class.sc_core::sc_object"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp2) nounwind
  ret void
}

declare void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, double, i32, %"class.sc_core::sc_simcontext"*)

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

declare %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"*, i32)

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

!0 = metadata !{metadata !"any pointer", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA"}
!3 = metadata !{metadata !"vtable pointer", metadata !2}
!4 = metadata !{metadata !"int", metadata !1}
