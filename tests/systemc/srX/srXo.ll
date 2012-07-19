; ModuleID = 'srXo.bc'
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
%class.top = type { %"class.sc_core::sc_module", i32, i32, %"class.sc_core::sc_event" }
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
@_ZTS3top = linkonce_odr constant [5 x i8] c"3top\00"
@_ZTSN7sc_core9sc_moduleE = available_externally constant [21 x i8] c"N7sc_core9sc_moduleE\00"
@_ZTSN7sc_core15sc_process_hostE = linkonce_odr constant [28 x i8] c"N7sc_core15sc_process_hostE\00"
@_ZTIN7sc_core15sc_process_hostE = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_process_hostE, i32 0, i32 0) }
@_ZTIN7sc_core9sc_moduleE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_moduleE, i32 0, i32 0), i32 0, i32 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*), i64 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core15sc_process_hostE to i8*), i64 10242 }
@_ZTI3top = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([5 x i8]* @_ZTS3top, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@.str8 = private unnamed_addr constant [10 x i8] c"sc_module\00", align 1
@.str9 = private unnamed_addr constant [2 x i8] c"P\00", align 1
@.str10 = private unnamed_addr constant [2 x i8] c"Q\00", align 1
@.str11 = private unnamed_addr constant [2 x i8] c"R\00", align 1
@.str12 = private unnamed_addr constant [2 x i8] c"S\00", align 1
@_ZSt4cout = external global %"class.std::basic_ostream"
@.str13 = private unnamed_addr constant [3 x i8] c"s \00", align 1
@.str14 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str15 = private unnamed_addr constant [3 x i8] c"r \00", align 1
@.str16 = private unnamed_addr constant [3 x i8] c"q \00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str17 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str18 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str19 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_increment()\00", align 1
@.str20 = private unnamed_addr constant [3 x i8] c"p \00", align 1
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
_ZN3topC1EN7sc_core14sc_module_nameEii.exit:
  %TOP = alloca %class.top, align 8
  %tmp = alloca %"class.sc_core::sc_module_name", align 8
  %tmp1 = getelementptr inbounds i8** %argv, i64 1
  %tmp2 = load i8** %tmp1, align 8, !tbaa !0
  %tmp3 = call i32 @atoi(i8* %tmp2) nounwind readonly
  %tmp4 = getelementptr inbounds i8** %argv, i64 2
  %tmp5 = load i8** %tmp4, align 8, !tbaa !0
  %tmp6 = call i32 @atoi(i8* %tmp5) nounwind readonly
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp, i8* getelementptr inbounds ([4 x i8]* @.str, i64 0, i64 0))
  call void @_ZN3topC2EN7sc_core14sc_module_nameEii(%class.top* %TOP, %"class.sc_core::sc_module_name"* %tmp, i32 %tmp3, i32 %tmp6)
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp)
  call void @_ZN7sc_core8sc_startEv()
  %tmp7 = getelementptr inbounds %class.top* %TOP, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp7, align 8, !tbaa !3
  %tmp8 = getelementptr %class.top* %TOP, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp8, align 8, !tbaa !3
  %tmp9 = getelementptr inbounds %class.top* %TOP, i64 0, i32 3
  call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp9)
  %tmp10 = getelementptr inbounds %class.top* %TOP, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp10)
  ret i32 0
}

declare i32 @atoi(i8* nocapture) nounwind readonly

declare void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"*, i8*)

declare void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core8sc_startEv()

define linkonce_odr void @_ZN3topD1Ev(%class.top* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN3topD2Ev.exit:
  %tmp = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %class.top* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp2)
  %tmp3 = getelementptr inbounds %class.top* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  ret void
}

declare void @_ZdlPv(i8*) nounwind

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

declare noalias i8* @_Znwm(i64)

declare %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"*, i8*, i64)

declare %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"*, i8 signext)

declare void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"*)

declare void @_ZSt16__throw_bad_castv() noreturn

declare %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"*)

define linkonce_odr void @_ZThn40_N3topD1Ev(%class.top* %this) {
_ZN3topD1Ev.exit:
  %tmp = getelementptr inbounds %class.top* %this, i64 -1, i32 3, i32 6, i32 0, i32 0, i32 1
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 24
  %tmp3 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  ret void
}

declare void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"*)

declare void @_ZNK7sc_core9sc_object5printERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object4dumpERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)

define linkonce_odr i8* @_ZNK7sc_core9sc_module4kindEv(%"class.sc_core::sc_module"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str8, i64 0, i64 0)
}

declare %"class.std::vector.10"* @_ZNK7sc_core9sc_module17get_child_objectsEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN3topD0Ev(%class.top* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN3topD2Ev.exit.i:
  %tmp = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %class.top* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp2)
  %tmp3 = getelementptr inbounds %class.top* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  %tmp4 = bitcast %class.top* %this to i8*
  tail call void @_ZdlPv(i8* %tmp4) nounwind
  ret void
}

declare void @_ZN7sc_core9sc_module25before_end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module18end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module19start_of_simulationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module17end_of_simulationEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZThn40_N3topD0Ev(%class.top* %this) {
_ZN3topD2Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.top* %this, i64 -1, i32 3, i32 6, i32 0, i32 0, i32 1
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 24
  %tmp3 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  %tmp5 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  ret void
}

define linkonce_odr void @_ZN3topC2EN7sc_core14sc_module_nameEii(%class.top* %this, %"class.sc_core::sc_module_name"* %name, i32 %a, i32 %b) unnamed_addr uwtable align 2 {
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
  %S_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp9 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp10 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp11 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp12 = getelementptr inbounds %class.top* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp12, %"class.sc_core::sc_module_name"* %name)
  %tmp13 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp13, align 8, !tbaa !3
  %tmp14 = getelementptr %class.top* %this, i64 0, i32 0, i32 1
  %tmp15 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp14, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp15, align 8, !tbaa !3
  %tmp16 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  store i32 %a, i32* %tmp16, align 4, !tbaa !4
  %tmp17 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  store i32 %b, i32* %tmp17, align 4, !tbaa !4
  %tmp18 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp19 = icmp eq %"class.sc_core::sc_simcontext"* %tmp18, null
  br i1 %tmp19, label %.noexc, label %bb22

.noexc:                                           ; preds = %bb
  %tmp20 = call noalias i8* @_Znwm(i64 248)
  %tmp21 = bitcast i8* %tmp20 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp21)
  store %"class.sc_core::sc_simcontext"* %tmp21, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp21, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %bb22

bb22:                                             ; preds = %.noexc, %bb
  %tmp23 = phi %"class.sc_core::sc_simcontext"* [ %tmp21, %.noexc ], [ %tmp18, %bb ]
  %tmp24 = getelementptr inbounds %class.top* %this, i64 0, i32 3, i32 0
  store %"class.sc_core::sc_simcontext"* %tmp23, %"class.sc_core::sc_simcontext"** %tmp24, align 8, !tbaa !0
  %tmp25 = getelementptr inbounds %class.top* %this, i64 0, i32 3, i32 1
  store i32 0, i32* %tmp25, align 4, !tbaa !5
  %tmp26 = getelementptr inbounds %class.top* %this, i64 0, i32 3, i32 2
  store i32 -1, i32* %tmp26, align 4, !tbaa !4
  %tmp27 = getelementptr inbounds %class.top* %this, i64 0, i32 3, i32 3
  %tmp28 = bitcast %"class.sc_core::sc_event_timed"** %tmp27 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp28, i8 0, i64 104, i32 8, i1 false)
  %tmp29 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp30 = icmp eq %"class.sc_core::sc_simcontext"* %tmp29, null
  br i1 %tmp30, label %.noexc15, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc15:                                         ; preds = %bb22
  %tmp31 = call noalias i8* @_Znwm(i64 248)
  %tmp32 = bitcast i8* %tmp31 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp32)
  store %"class.sc_core::sc_simcontext"* %tmp32, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp32, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc15, %bb22
  %tmp33 = phi %"class.sc_core::sc_simcontext"* [ %tmp32, %.noexc15 ], [ %tmp29, %bb22 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %P_handle, %"class.sc_core::sc_simcontext"* %tmp33, i8* getelementptr inbounds ([2 x i8]* @.str9, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1PEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp14, %"class.sc_core::sc_spawn_options"* null)
  %tmp34 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %P_handle, i64 0, i32 0
  %tmp35 = load %"class.sc_core::sc_process_b"** %tmp34, align 8, !tbaa !0
  %tmp36 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp35, %"class.sc_core::sc_process_b"** %tmp36, align 8, !tbaa !0
  %tmp37 = icmp eq %"class.sc_core::sc_process_b"* %tmp35, null
  br i1 %tmp37, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb38

bb38:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp39 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp35, i64 0, i32 15
  %tmp40 = load i32* %tmp39, align 4, !tbaa !4
  %tmp41 = icmp eq i32 %tmp40, 0
  br i1 %tmp41, label %bb42, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb42:                                             ; preds = %bb38
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb38
  %tmp43 = add nsw i32 %tmp40, 1
  store i32 %tmp43, i32* %tmp39, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp44 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 2
  %tmp45 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp44, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp46 = load %"class.sc_core::sc_process_b"** %tmp36, align 8, !tbaa !0
  %tmp47 = icmp eq %"class.sc_core::sc_process_b"* %tmp46, null
  br i1 %tmp47, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb48

bb48:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp49 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp46, i64 0, i32 15
  %tmp50 = load i32* %tmp49, align 4, !tbaa !4
  %tmp51 = add nsw i32 %tmp50, -1
  store i32 %tmp51, i32* %tmp49, align 4, !tbaa !4
  %tmp52 = icmp eq i32 %tmp51, 0
  br i1 %tmp52, label %bb53, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb53:                                             ; preds = %bb48
  %tmp54 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp55 = icmp eq %"class.sc_core::sc_process_b"* %tmp54, null
  br i1 %tmp55, label %bb60, label %.noexc18

.noexc18:                                         ; preds = %bb53
  %tmp56 = bitcast %"class.sc_core::sc_process_b"* %tmp54 to void (%"class.sc_core::sc_process_b"*)***
  %tmp57 = load void (%"class.sc_core::sc_process_b"*)*** %tmp56, align 8, !tbaa !3
  %tmp58 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp57, i64 6
  %tmp59 = load void (%"class.sc_core::sc_process_b"*)** %tmp58, align 8
  call void %tmp59(%"class.sc_core::sc_process_b"* %tmp54)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb60

bb60:                                             ; preds = %.noexc18, %bb53
  %tmp61 = phi %"class.sc_core::sc_process_b"* [ null, %bb53 ], [ %.pre.i.i.i, %.noexc18 ]
  %tmp62 = icmp eq %"class.sc_core::sc_process_b"* %tmp61, %tmp46
  br i1 %tmp62, label %bb63, label %bb64

bb63:                                             ; preds = %bb60
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb64:                                             ; preds = %bb60
  store %"class.sc_core::sc_process_b"* %tmp46, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb64, %bb48, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp65 = load %"class.sc_core::sc_process_b"** %tmp34, align 8, !tbaa !0
  %tmp66 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp65, %"class.sc_core::sc_process_b"** %tmp66, align 8, !tbaa !0
  %tmp67 = icmp eq %"class.sc_core::sc_process_b"* %tmp65, null
  br i1 %tmp67, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit20, label %bb68

bb68:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp69 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp65, i64 0, i32 15
  %tmp70 = load i32* %tmp69, align 4, !tbaa !4
  %tmp71 = icmp eq i32 %tmp70, 0
  br i1 %tmp71, label %bb72, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i19

bb72:                                             ; preds = %bb68
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i19: ; preds = %bb68
  %tmp73 = add nsw i32 %tmp70, 1
  store i32 %tmp73, i32* %tmp69, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit20

_ZN7sc_core17sc_process_handleC1ERKS0_.exit20:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i19, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp74 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 3
  %tmp75 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp74, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp76 = load %"class.sc_core::sc_process_b"** %tmp66, align 8, !tbaa !0
  %tmp77 = icmp eq %"class.sc_core::sc_process_b"* %tmp76, null
  br i1 %tmp77, label %_ZN7sc_core17sc_process_handleD1Ev.exit23, label %bb78

bb78:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit20
  %tmp79 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp76, i64 0, i32 15
  %tmp80 = load i32* %tmp79, align 4, !tbaa !4
  %tmp81 = add nsw i32 %tmp80, -1
  store i32 %tmp81, i32* %tmp79, align 4, !tbaa !4
  %tmp82 = icmp eq i32 %tmp81, 0
  br i1 %tmp82, label %bb83, label %_ZN7sc_core17sc_process_handleD1Ev.exit23

bb83:                                             ; preds = %bb78
  %tmp84 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp85 = icmp eq %"class.sc_core::sc_process_b"* %tmp84, null
  br i1 %tmp85, label %bb90, label %.noexc22

.noexc22:                                         ; preds = %bb83
  %tmp86 = bitcast %"class.sc_core::sc_process_b"* %tmp84 to void (%"class.sc_core::sc_process_b"*)***
  %tmp87 = load void (%"class.sc_core::sc_process_b"*)*** %tmp86, align 8, !tbaa !3
  %tmp88 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp87, i64 6
  %tmp89 = load void (%"class.sc_core::sc_process_b"*)** %tmp88, align 8
  call void %tmp89(%"class.sc_core::sc_process_b"* %tmp84)
  %.pre.i.i.i21 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb90

bb90:                                             ; preds = %.noexc22, %bb83
  %tmp91 = phi %"class.sc_core::sc_process_b"* [ null, %bb83 ], [ %.pre.i.i.i21, %.noexc22 ]
  %tmp92 = icmp eq %"class.sc_core::sc_process_b"* %tmp91, %tmp76
  br i1 %tmp92, label %bb93, label %bb94

bb93:                                             ; preds = %bb90
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb94:                                             ; preds = %bb90
  store %"class.sc_core::sc_process_b"* %tmp76, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit23

_ZN7sc_core17sc_process_handleD1Ev.exit23:        ; preds = %bb94, %bb78, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit20
  %tmp95 = load %"class.sc_core::sc_process_b"** %tmp34, align 8, !tbaa !0
  %tmp96 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp95, %"class.sc_core::sc_process_b"** %tmp96, align 8, !tbaa !0
  %tmp97 = icmp eq %"class.sc_core::sc_process_b"* %tmp95, null
  br i1 %tmp97, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit25, label %bb98

bb98:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit23
  %tmp99 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp95, i64 0, i32 15
  %tmp100 = load i32* %tmp99, align 4, !tbaa !4
  %tmp101 = icmp eq i32 %tmp100, 0
  br i1 %tmp101, label %bb102, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i24

bb102:                                            ; preds = %bb98
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i24: ; preds = %bb98
  %tmp103 = add nsw i32 %tmp100, 1
  store i32 %tmp103, i32* %tmp99, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit25

_ZN7sc_core17sc_process_handleC1ERKS0_.exit25:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i24, %_ZN7sc_core17sc_process_handleD1Ev.exit23
  %tmp104 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 4
  %tmp105 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp104, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp106 = load %"class.sc_core::sc_process_b"** %tmp96, align 8, !tbaa !0
  %tmp107 = icmp eq %"class.sc_core::sc_process_b"* %tmp106, null
  br i1 %tmp107, label %_ZN7sc_core17sc_process_handleD1Ev.exit28, label %bb108

bb108:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit25
  %tmp109 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp106, i64 0, i32 15
  %tmp110 = load i32* %tmp109, align 4, !tbaa !4
  %tmp111 = add nsw i32 %tmp110, -1
  store i32 %tmp111, i32* %tmp109, align 4, !tbaa !4
  %tmp112 = icmp eq i32 %tmp111, 0
  br i1 %tmp112, label %bb113, label %_ZN7sc_core17sc_process_handleD1Ev.exit28

bb113:                                            ; preds = %bb108
  %tmp114 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp115 = icmp eq %"class.sc_core::sc_process_b"* %tmp114, null
  br i1 %tmp115, label %bb120, label %.noexc27

.noexc27:                                         ; preds = %bb113
  %tmp116 = bitcast %"class.sc_core::sc_process_b"* %tmp114 to void (%"class.sc_core::sc_process_b"*)***
  %tmp117 = load void (%"class.sc_core::sc_process_b"*)*** %tmp116, align 8, !tbaa !3
  %tmp118 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp117, i64 6
  %tmp119 = load void (%"class.sc_core::sc_process_b"*)** %tmp118, align 8
  call void %tmp119(%"class.sc_core::sc_process_b"* %tmp114)
  %.pre.i.i.i26 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb120

bb120:                                            ; preds = %.noexc27, %bb113
  %tmp121 = phi %"class.sc_core::sc_process_b"* [ null, %bb113 ], [ %.pre.i.i.i26, %.noexc27 ]
  %tmp122 = icmp eq %"class.sc_core::sc_process_b"* %tmp121, %tmp106
  br i1 %tmp122, label %bb123, label %bb124

bb123:                                            ; preds = %bb120
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb124:                                            ; preds = %bb120
  store %"class.sc_core::sc_process_b"* %tmp106, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit28

_ZN7sc_core17sc_process_handleD1Ev.exit28:        ; preds = %bb124, %bb108, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit25
  %tmp125 = load %"class.sc_core::sc_process_b"** %tmp34, align 8, !tbaa !0
  %tmp126 = icmp eq %"class.sc_core::sc_process_b"* %tmp125, null
  br i1 %tmp126, label %_ZN7sc_core17sc_process_handleD1Ev.exit31, label %bb127

bb127:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit28
  %tmp128 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp125, i64 0, i32 15
  %tmp129 = load i32* %tmp128, align 4, !tbaa !4
  %tmp130 = add nsw i32 %tmp129, -1
  store i32 %tmp130, i32* %tmp128, align 4, !tbaa !4
  %tmp131 = icmp eq i32 %tmp130, 0
  br i1 %tmp131, label %bb132, label %_ZN7sc_core17sc_process_handleD1Ev.exit31

bb132:                                            ; preds = %bb127
  %tmp133 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp134 = icmp eq %"class.sc_core::sc_process_b"* %tmp133, null
  br i1 %tmp134, label %bb139, label %.noexc30

.noexc30:                                         ; preds = %bb132
  %tmp135 = bitcast %"class.sc_core::sc_process_b"* %tmp133 to void (%"class.sc_core::sc_process_b"*)***
  %tmp136 = load void (%"class.sc_core::sc_process_b"*)*** %tmp135, align 8, !tbaa !3
  %tmp137 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp136, i64 6
  %tmp138 = load void (%"class.sc_core::sc_process_b"*)** %tmp137, align 8
  call void %tmp138(%"class.sc_core::sc_process_b"* %tmp133)
  %.pre.i.i.i29 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb139

bb139:                                            ; preds = %.noexc30, %bb132
  %tmp140 = phi %"class.sc_core::sc_process_b"* [ null, %bb132 ], [ %.pre.i.i.i29, %.noexc30 ]
  %tmp141 = icmp eq %"class.sc_core::sc_process_b"* %tmp140, %tmp125
  br i1 %tmp141, label %bb142, label %bb143

bb142:                                            ; preds = %bb139
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb143:                                            ; preds = %bb139
  store %"class.sc_core::sc_process_b"* %tmp125, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit31

_ZN7sc_core17sc_process_handleD1Ev.exit31:        ; preds = %bb143, %bb127, %_ZN7sc_core17sc_process_handleD1Ev.exit28
  %tmp144 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp145 = icmp eq %"class.sc_core::sc_simcontext"* %tmp144, null
  br i1 %tmp145, label %.noexc32, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit35

.noexc32:                                         ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit31
  %tmp146 = call noalias i8* @_Znwm(i64 248)
  %tmp147 = bitcast i8* %tmp146 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp147)
  store %"class.sc_core::sc_simcontext"* %tmp147, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp147, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit35

_ZN7sc_core22sc_get_curr_simcontextEv.exit35:     ; preds = %.noexc32, %_ZN7sc_core17sc_process_handleD1Ev.exit31
  %tmp148 = phi %"class.sc_core::sc_simcontext"* [ %tmp147, %.noexc32 ], [ %tmp144, %_ZN7sc_core17sc_process_handleD1Ev.exit31 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %Q_handle, %"class.sc_core::sc_simcontext"* %tmp148, i8* getelementptr inbounds ([2 x i8]* @.str10, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1QEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp14, %"class.sc_core::sc_spawn_options"* null)
  %tmp149 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %Q_handle, i64 0, i32 0
  %tmp150 = load %"class.sc_core::sc_process_b"** %tmp149, align 8, !tbaa !0
  %tmp151 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp3, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp150, %"class.sc_core::sc_process_b"** %tmp151, align 8, !tbaa !0
  %tmp152 = icmp eq %"class.sc_core::sc_process_b"* %tmp150, null
  br i1 %tmp152, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit37, label %bb153

bb153:                                            ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit35
  %tmp154 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp150, i64 0, i32 15
  %tmp155 = load i32* %tmp154, align 4, !tbaa !4
  %tmp156 = icmp eq i32 %tmp155, 0
  br i1 %tmp156, label %bb157, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i36

bb157:                                            ; preds = %bb153
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i36: ; preds = %bb153
  %tmp158 = add nsw i32 %tmp155, 1
  store i32 %tmp158, i32* %tmp154, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit37

_ZN7sc_core17sc_process_handleC1ERKS0_.exit37:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i36, %_ZN7sc_core22sc_get_curr_simcontextEv.exit35
  %tmp159 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp44, %"class.sc_core::sc_process_handle"* %tmp3)
  %tmp160 = load %"class.sc_core::sc_process_b"** %tmp151, align 8, !tbaa !0
  %tmp161 = icmp eq %"class.sc_core::sc_process_b"* %tmp160, null
  br i1 %tmp161, label %_ZN7sc_core17sc_process_handleD1Ev.exit40, label %bb162

bb162:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit37
  %tmp163 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp160, i64 0, i32 15
  %tmp164 = load i32* %tmp163, align 4, !tbaa !4
  %tmp165 = add nsw i32 %tmp164, -1
  store i32 %tmp165, i32* %tmp163, align 4, !tbaa !4
  %tmp166 = icmp eq i32 %tmp165, 0
  br i1 %tmp166, label %bb167, label %_ZN7sc_core17sc_process_handleD1Ev.exit40

bb167:                                            ; preds = %bb162
  %tmp168 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp169 = icmp eq %"class.sc_core::sc_process_b"* %tmp168, null
  br i1 %tmp169, label %bb174, label %.noexc39

.noexc39:                                         ; preds = %bb167
  %tmp170 = bitcast %"class.sc_core::sc_process_b"* %tmp168 to void (%"class.sc_core::sc_process_b"*)***
  %tmp171 = load void (%"class.sc_core::sc_process_b"*)*** %tmp170, align 8, !tbaa !3
  %tmp172 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp171, i64 6
  %tmp173 = load void (%"class.sc_core::sc_process_b"*)** %tmp172, align 8
  call void %tmp173(%"class.sc_core::sc_process_b"* %tmp168)
  %.pre.i.i.i38 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb174

bb174:                                            ; preds = %.noexc39, %bb167
  %tmp175 = phi %"class.sc_core::sc_process_b"* [ null, %bb167 ], [ %.pre.i.i.i38, %.noexc39 ]
  %tmp176 = icmp eq %"class.sc_core::sc_process_b"* %tmp175, %tmp160
  br i1 %tmp176, label %bb177, label %bb178

bb177:                                            ; preds = %bb174
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb178:                                            ; preds = %bb174
  store %"class.sc_core::sc_process_b"* %tmp160, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit40

_ZN7sc_core17sc_process_handleD1Ev.exit40:        ; preds = %bb178, %bb162, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit37
  %tmp179 = load %"class.sc_core::sc_process_b"** %tmp149, align 8, !tbaa !0
  %tmp180 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp4, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp179, %"class.sc_core::sc_process_b"** %tmp180, align 8, !tbaa !0
  %tmp181 = icmp eq %"class.sc_core::sc_process_b"* %tmp179, null
  br i1 %tmp181, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit42, label %bb182

bb182:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit40
  %tmp183 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp179, i64 0, i32 15
  %tmp184 = load i32* %tmp183, align 4, !tbaa !4
  %tmp185 = icmp eq i32 %tmp184, 0
  br i1 %tmp185, label %bb186, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i41

bb186:                                            ; preds = %bb182
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i41: ; preds = %bb182
  %tmp187 = add nsw i32 %tmp184, 1
  store i32 %tmp187, i32* %tmp183, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit42

_ZN7sc_core17sc_process_handleC1ERKS0_.exit42:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i41, %_ZN7sc_core17sc_process_handleD1Ev.exit40
  %tmp188 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp74, %"class.sc_core::sc_process_handle"* %tmp4)
  %tmp189 = load %"class.sc_core::sc_process_b"** %tmp180, align 8, !tbaa !0
  %tmp190 = icmp eq %"class.sc_core::sc_process_b"* %tmp189, null
  br i1 %tmp190, label %_ZN7sc_core17sc_process_handleD1Ev.exit45, label %bb191

bb191:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit42
  %tmp192 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp189, i64 0, i32 15
  %tmp193 = load i32* %tmp192, align 4, !tbaa !4
  %tmp194 = add nsw i32 %tmp193, -1
  store i32 %tmp194, i32* %tmp192, align 4, !tbaa !4
  %tmp195 = icmp eq i32 %tmp194, 0
  br i1 %tmp195, label %bb196, label %_ZN7sc_core17sc_process_handleD1Ev.exit45

bb196:                                            ; preds = %bb191
  %tmp197 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp198 = icmp eq %"class.sc_core::sc_process_b"* %tmp197, null
  br i1 %tmp198, label %bb203, label %.noexc44

.noexc44:                                         ; preds = %bb196
  %tmp199 = bitcast %"class.sc_core::sc_process_b"* %tmp197 to void (%"class.sc_core::sc_process_b"*)***
  %tmp200 = load void (%"class.sc_core::sc_process_b"*)*** %tmp199, align 8, !tbaa !3
  %tmp201 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp200, i64 6
  %tmp202 = load void (%"class.sc_core::sc_process_b"*)** %tmp201, align 8
  call void %tmp202(%"class.sc_core::sc_process_b"* %tmp197)
  %.pre.i.i.i43 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb203

bb203:                                            ; preds = %.noexc44, %bb196
  %tmp204 = phi %"class.sc_core::sc_process_b"* [ null, %bb196 ], [ %.pre.i.i.i43, %.noexc44 ]
  %tmp205 = icmp eq %"class.sc_core::sc_process_b"* %tmp204, %tmp189
  br i1 %tmp205, label %bb206, label %bb207

bb206:                                            ; preds = %bb203
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb207:                                            ; preds = %bb203
  store %"class.sc_core::sc_process_b"* %tmp189, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit45

_ZN7sc_core17sc_process_handleD1Ev.exit45:        ; preds = %bb207, %bb191, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit42
  %tmp208 = load %"class.sc_core::sc_process_b"** %tmp149, align 8, !tbaa !0
  %tmp209 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp5, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp208, %"class.sc_core::sc_process_b"** %tmp209, align 8, !tbaa !0
  %tmp210 = icmp eq %"class.sc_core::sc_process_b"* %tmp208, null
  br i1 %tmp210, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit47, label %bb211

bb211:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit45
  %tmp212 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp208, i64 0, i32 15
  %tmp213 = load i32* %tmp212, align 4, !tbaa !4
  %tmp214 = icmp eq i32 %tmp213, 0
  br i1 %tmp214, label %bb215, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i46

bb215:                                            ; preds = %bb211
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i46: ; preds = %bb211
  %tmp216 = add nsw i32 %tmp213, 1
  store i32 %tmp216, i32* %tmp212, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit47

_ZN7sc_core17sc_process_handleC1ERKS0_.exit47:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i46, %_ZN7sc_core17sc_process_handleD1Ev.exit45
  %tmp217 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp104, %"class.sc_core::sc_process_handle"* %tmp5)
  %tmp218 = load %"class.sc_core::sc_process_b"** %tmp209, align 8, !tbaa !0
  %tmp219 = icmp eq %"class.sc_core::sc_process_b"* %tmp218, null
  br i1 %tmp219, label %_ZN7sc_core17sc_process_handleD1Ev.exit50, label %bb220

bb220:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit47
  %tmp221 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp218, i64 0, i32 15
  %tmp222 = load i32* %tmp221, align 4, !tbaa !4
  %tmp223 = add nsw i32 %tmp222, -1
  store i32 %tmp223, i32* %tmp221, align 4, !tbaa !4
  %tmp224 = icmp eq i32 %tmp223, 0
  br i1 %tmp224, label %bb225, label %_ZN7sc_core17sc_process_handleD1Ev.exit50

bb225:                                            ; preds = %bb220
  %tmp226 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp227 = icmp eq %"class.sc_core::sc_process_b"* %tmp226, null
  br i1 %tmp227, label %bb232, label %.noexc49

.noexc49:                                         ; preds = %bb225
  %tmp228 = bitcast %"class.sc_core::sc_process_b"* %tmp226 to void (%"class.sc_core::sc_process_b"*)***
  %tmp229 = load void (%"class.sc_core::sc_process_b"*)*** %tmp228, align 8, !tbaa !3
  %tmp230 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp229, i64 6
  %tmp231 = load void (%"class.sc_core::sc_process_b"*)** %tmp230, align 8
  call void %tmp231(%"class.sc_core::sc_process_b"* %tmp226)
  %.pre.i.i.i48 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb232

bb232:                                            ; preds = %.noexc49, %bb225
  %tmp233 = phi %"class.sc_core::sc_process_b"* [ null, %bb225 ], [ %.pre.i.i.i48, %.noexc49 ]
  %tmp234 = icmp eq %"class.sc_core::sc_process_b"* %tmp233, %tmp218
  br i1 %tmp234, label %bb235, label %bb236

bb235:                                            ; preds = %bb232
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb236:                                            ; preds = %bb232
  store %"class.sc_core::sc_process_b"* %tmp218, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit50

_ZN7sc_core17sc_process_handleD1Ev.exit50:        ; preds = %bb236, %bb220, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit47
  %tmp237 = load %"class.sc_core::sc_process_b"** %tmp149, align 8, !tbaa !0
  %tmp238 = icmp eq %"class.sc_core::sc_process_b"* %tmp237, null
  br i1 %tmp238, label %_ZN7sc_core17sc_process_handleD1Ev.exit53, label %bb239

bb239:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit50
  %tmp240 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp237, i64 0, i32 15
  %tmp241 = load i32* %tmp240, align 4, !tbaa !4
  %tmp242 = add nsw i32 %tmp241, -1
  store i32 %tmp242, i32* %tmp240, align 4, !tbaa !4
  %tmp243 = icmp eq i32 %tmp242, 0
  br i1 %tmp243, label %bb244, label %_ZN7sc_core17sc_process_handleD1Ev.exit53

bb244:                                            ; preds = %bb239
  %tmp245 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp246 = icmp eq %"class.sc_core::sc_process_b"* %tmp245, null
  br i1 %tmp246, label %bb251, label %.noexc52

.noexc52:                                         ; preds = %bb244
  %tmp247 = bitcast %"class.sc_core::sc_process_b"* %tmp245 to void (%"class.sc_core::sc_process_b"*)***
  %tmp248 = load void (%"class.sc_core::sc_process_b"*)*** %tmp247, align 8, !tbaa !3
  %tmp249 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp248, i64 6
  %tmp250 = load void (%"class.sc_core::sc_process_b"*)** %tmp249, align 8
  call void %tmp250(%"class.sc_core::sc_process_b"* %tmp245)
  %.pre.i.i.i51 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb251

bb251:                                            ; preds = %.noexc52, %bb244
  %tmp252 = phi %"class.sc_core::sc_process_b"* [ null, %bb244 ], [ %.pre.i.i.i51, %.noexc52 ]
  %tmp253 = icmp eq %"class.sc_core::sc_process_b"* %tmp252, %tmp237
  br i1 %tmp253, label %bb254, label %bb255

bb254:                                            ; preds = %bb251
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb255:                                            ; preds = %bb251
  store %"class.sc_core::sc_process_b"* %tmp237, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit53

_ZN7sc_core17sc_process_handleD1Ev.exit53:        ; preds = %bb255, %bb239, %_ZN7sc_core17sc_process_handleD1Ev.exit50
  %tmp256 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp257 = icmp eq %"class.sc_core::sc_simcontext"* %tmp256, null
  br i1 %tmp257, label %.noexc54, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit57

.noexc54:                                         ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit53
  %tmp258 = call noalias i8* @_Znwm(i64 248)
  %tmp259 = bitcast i8* %tmp258 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp259)
  store %"class.sc_core::sc_simcontext"* %tmp259, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp259, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit57

_ZN7sc_core22sc_get_curr_simcontextEv.exit57:     ; preds = %.noexc54, %_ZN7sc_core17sc_process_handleD1Ev.exit53
  %tmp260 = phi %"class.sc_core::sc_simcontext"* [ %tmp259, %.noexc54 ], [ %tmp256, %_ZN7sc_core17sc_process_handleD1Ev.exit53 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %R_handle, %"class.sc_core::sc_simcontext"* %tmp260, i8* getelementptr inbounds ([2 x i8]* @.str11, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1REv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp14, %"class.sc_core::sc_spawn_options"* null)
  %tmp261 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %R_handle, i64 0, i32 0
  %tmp262 = load %"class.sc_core::sc_process_b"** %tmp261, align 8, !tbaa !0
  %tmp263 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp6, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp262, %"class.sc_core::sc_process_b"** %tmp263, align 8, !tbaa !0
  %tmp264 = icmp eq %"class.sc_core::sc_process_b"* %tmp262, null
  br i1 %tmp264, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit59, label %bb265

bb265:                                            ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit57
  %tmp266 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp262, i64 0, i32 15
  %tmp267 = load i32* %tmp266, align 4, !tbaa !4
  %tmp268 = icmp eq i32 %tmp267, 0
  br i1 %tmp268, label %bb269, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i58

bb269:                                            ; preds = %bb265
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i58: ; preds = %bb265
  %tmp270 = add nsw i32 %tmp267, 1
  store i32 %tmp270, i32* %tmp266, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit59

_ZN7sc_core17sc_process_handleC1ERKS0_.exit59:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i58, %_ZN7sc_core22sc_get_curr_simcontextEv.exit57
  %tmp271 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp44, %"class.sc_core::sc_process_handle"* %tmp6)
  %tmp272 = load %"class.sc_core::sc_process_b"** %tmp263, align 8, !tbaa !0
  %tmp273 = icmp eq %"class.sc_core::sc_process_b"* %tmp272, null
  br i1 %tmp273, label %_ZN7sc_core17sc_process_handleD1Ev.exit62, label %bb274

bb274:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit59
  %tmp275 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp272, i64 0, i32 15
  %tmp276 = load i32* %tmp275, align 4, !tbaa !4
  %tmp277 = add nsw i32 %tmp276, -1
  store i32 %tmp277, i32* %tmp275, align 4, !tbaa !4
  %tmp278 = icmp eq i32 %tmp277, 0
  br i1 %tmp278, label %bb279, label %_ZN7sc_core17sc_process_handleD1Ev.exit62

bb279:                                            ; preds = %bb274
  %tmp280 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp281 = icmp eq %"class.sc_core::sc_process_b"* %tmp280, null
  br i1 %tmp281, label %bb286, label %.noexc61

.noexc61:                                         ; preds = %bb279
  %tmp282 = bitcast %"class.sc_core::sc_process_b"* %tmp280 to void (%"class.sc_core::sc_process_b"*)***
  %tmp283 = load void (%"class.sc_core::sc_process_b"*)*** %tmp282, align 8, !tbaa !3
  %tmp284 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp283, i64 6
  %tmp285 = load void (%"class.sc_core::sc_process_b"*)** %tmp284, align 8
  call void %tmp285(%"class.sc_core::sc_process_b"* %tmp280)
  %.pre.i.i.i60 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb286

bb286:                                            ; preds = %.noexc61, %bb279
  %tmp287 = phi %"class.sc_core::sc_process_b"* [ null, %bb279 ], [ %.pre.i.i.i60, %.noexc61 ]
  %tmp288 = icmp eq %"class.sc_core::sc_process_b"* %tmp287, %tmp272
  br i1 %tmp288, label %bb289, label %bb290

bb289:                                            ; preds = %bb286
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb290:                                            ; preds = %bb286
  store %"class.sc_core::sc_process_b"* %tmp272, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit62

_ZN7sc_core17sc_process_handleD1Ev.exit62:        ; preds = %bb290, %bb274, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit59
  %tmp291 = load %"class.sc_core::sc_process_b"** %tmp261, align 8, !tbaa !0
  %tmp292 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp7, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp291, %"class.sc_core::sc_process_b"** %tmp292, align 8, !tbaa !0
  %tmp293 = icmp eq %"class.sc_core::sc_process_b"* %tmp291, null
  br i1 %tmp293, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit64, label %bb294

bb294:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit62
  %tmp295 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp291, i64 0, i32 15
  %tmp296 = load i32* %tmp295, align 4, !tbaa !4
  %tmp297 = icmp eq i32 %tmp296, 0
  br i1 %tmp297, label %bb298, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i63

bb298:                                            ; preds = %bb294
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i63: ; preds = %bb294
  %tmp299 = add nsw i32 %tmp296, 1
  store i32 %tmp299, i32* %tmp295, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit64

_ZN7sc_core17sc_process_handleC1ERKS0_.exit64:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i63, %_ZN7sc_core17sc_process_handleD1Ev.exit62
  %tmp300 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp74, %"class.sc_core::sc_process_handle"* %tmp7)
  %tmp301 = load %"class.sc_core::sc_process_b"** %tmp292, align 8, !tbaa !0
  %tmp302 = icmp eq %"class.sc_core::sc_process_b"* %tmp301, null
  br i1 %tmp302, label %_ZN7sc_core17sc_process_handleD1Ev.exit67, label %bb303

bb303:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit64
  %tmp304 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp301, i64 0, i32 15
  %tmp305 = load i32* %tmp304, align 4, !tbaa !4
  %tmp306 = add nsw i32 %tmp305, -1
  store i32 %tmp306, i32* %tmp304, align 4, !tbaa !4
  %tmp307 = icmp eq i32 %tmp306, 0
  br i1 %tmp307, label %bb308, label %_ZN7sc_core17sc_process_handleD1Ev.exit67

bb308:                                            ; preds = %bb303
  %tmp309 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp310 = icmp eq %"class.sc_core::sc_process_b"* %tmp309, null
  br i1 %tmp310, label %bb315, label %.noexc66

.noexc66:                                         ; preds = %bb308
  %tmp311 = bitcast %"class.sc_core::sc_process_b"* %tmp309 to void (%"class.sc_core::sc_process_b"*)***
  %tmp312 = load void (%"class.sc_core::sc_process_b"*)*** %tmp311, align 8, !tbaa !3
  %tmp313 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp312, i64 6
  %tmp314 = load void (%"class.sc_core::sc_process_b"*)** %tmp313, align 8
  call void %tmp314(%"class.sc_core::sc_process_b"* %tmp309)
  %.pre.i.i.i65 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb315

bb315:                                            ; preds = %.noexc66, %bb308
  %tmp316 = phi %"class.sc_core::sc_process_b"* [ null, %bb308 ], [ %.pre.i.i.i65, %.noexc66 ]
  %tmp317 = icmp eq %"class.sc_core::sc_process_b"* %tmp316, %tmp301
  br i1 %tmp317, label %bb318, label %bb319

bb318:                                            ; preds = %bb315
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb319:                                            ; preds = %bb315
  store %"class.sc_core::sc_process_b"* %tmp301, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit67

_ZN7sc_core17sc_process_handleD1Ev.exit67:        ; preds = %bb319, %bb303, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit64
  %tmp320 = load %"class.sc_core::sc_process_b"** %tmp261, align 8, !tbaa !0
  %tmp321 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp8, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp320, %"class.sc_core::sc_process_b"** %tmp321, align 8, !tbaa !0
  %tmp322 = icmp eq %"class.sc_core::sc_process_b"* %tmp320, null
  br i1 %tmp322, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit69, label %bb323

bb323:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit67
  %tmp324 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp320, i64 0, i32 15
  %tmp325 = load i32* %tmp324, align 4, !tbaa !4
  %tmp326 = icmp eq i32 %tmp325, 0
  br i1 %tmp326, label %bb327, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i68

bb327:                                            ; preds = %bb323
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i68: ; preds = %bb323
  %tmp328 = add nsw i32 %tmp325, 1
  store i32 %tmp328, i32* %tmp324, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit69

_ZN7sc_core17sc_process_handleC1ERKS0_.exit69:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i68, %_ZN7sc_core17sc_process_handleD1Ev.exit67
  %tmp329 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp104, %"class.sc_core::sc_process_handle"* %tmp8)
  %tmp330 = load %"class.sc_core::sc_process_b"** %tmp321, align 8, !tbaa !0
  %tmp331 = icmp eq %"class.sc_core::sc_process_b"* %tmp330, null
  br i1 %tmp331, label %_ZN7sc_core17sc_process_handleD1Ev.exit72, label %bb332

bb332:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit69
  %tmp333 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp330, i64 0, i32 15
  %tmp334 = load i32* %tmp333, align 4, !tbaa !4
  %tmp335 = add nsw i32 %tmp334, -1
  store i32 %tmp335, i32* %tmp333, align 4, !tbaa !4
  %tmp336 = icmp eq i32 %tmp335, 0
  br i1 %tmp336, label %bb337, label %_ZN7sc_core17sc_process_handleD1Ev.exit72

bb337:                                            ; preds = %bb332
  %tmp338 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp339 = icmp eq %"class.sc_core::sc_process_b"* %tmp338, null
  br i1 %tmp339, label %bb344, label %.noexc71

.noexc71:                                         ; preds = %bb337
  %tmp340 = bitcast %"class.sc_core::sc_process_b"* %tmp338 to void (%"class.sc_core::sc_process_b"*)***
  %tmp341 = load void (%"class.sc_core::sc_process_b"*)*** %tmp340, align 8, !tbaa !3
  %tmp342 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp341, i64 6
  %tmp343 = load void (%"class.sc_core::sc_process_b"*)** %tmp342, align 8
  call void %tmp343(%"class.sc_core::sc_process_b"* %tmp338)
  %.pre.i.i.i70 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb344

bb344:                                            ; preds = %.noexc71, %bb337
  %tmp345 = phi %"class.sc_core::sc_process_b"* [ null, %bb337 ], [ %.pre.i.i.i70, %.noexc71 ]
  %tmp346 = icmp eq %"class.sc_core::sc_process_b"* %tmp345, %tmp330
  br i1 %tmp346, label %bb347, label %bb348

bb347:                                            ; preds = %bb344
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb348:                                            ; preds = %bb344
  store %"class.sc_core::sc_process_b"* %tmp330, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit72

_ZN7sc_core17sc_process_handleD1Ev.exit72:        ; preds = %bb348, %bb332, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit69
  %tmp349 = load %"class.sc_core::sc_process_b"** %tmp261, align 8, !tbaa !0
  %tmp350 = icmp eq %"class.sc_core::sc_process_b"* %tmp349, null
  br i1 %tmp350, label %_ZN7sc_core17sc_process_handleD1Ev.exit75, label %bb351

bb351:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit72
  %tmp352 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp349, i64 0, i32 15
  %tmp353 = load i32* %tmp352, align 4, !tbaa !4
  %tmp354 = add nsw i32 %tmp353, -1
  store i32 %tmp354, i32* %tmp352, align 4, !tbaa !4
  %tmp355 = icmp eq i32 %tmp354, 0
  br i1 %tmp355, label %bb356, label %_ZN7sc_core17sc_process_handleD1Ev.exit75

bb356:                                            ; preds = %bb351
  %tmp357 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp358 = icmp eq %"class.sc_core::sc_process_b"* %tmp357, null
  br i1 %tmp358, label %bb363, label %.noexc74

.noexc74:                                         ; preds = %bb356
  %tmp359 = bitcast %"class.sc_core::sc_process_b"* %tmp357 to void (%"class.sc_core::sc_process_b"*)***
  %tmp360 = load void (%"class.sc_core::sc_process_b"*)*** %tmp359, align 8, !tbaa !3
  %tmp361 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp360, i64 6
  %tmp362 = load void (%"class.sc_core::sc_process_b"*)** %tmp361, align 8
  call void %tmp362(%"class.sc_core::sc_process_b"* %tmp357)
  %.pre.i.i.i73 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb363

bb363:                                            ; preds = %.noexc74, %bb356
  %tmp364 = phi %"class.sc_core::sc_process_b"* [ null, %bb356 ], [ %.pre.i.i.i73, %.noexc74 ]
  %tmp365 = icmp eq %"class.sc_core::sc_process_b"* %tmp364, %tmp349
  br i1 %tmp365, label %bb366, label %bb367

bb366:                                            ; preds = %bb363
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb367:                                            ; preds = %bb363
  store %"class.sc_core::sc_process_b"* %tmp349, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit75

_ZN7sc_core17sc_process_handleD1Ev.exit75:        ; preds = %bb367, %bb351, %_ZN7sc_core17sc_process_handleD1Ev.exit72
  %tmp368 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp369 = icmp eq %"class.sc_core::sc_simcontext"* %tmp368, null
  br i1 %tmp369, label %.noexc76, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit79

.noexc76:                                         ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit75
  %tmp370 = call noalias i8* @_Znwm(i64 248)
  %tmp371 = bitcast i8* %tmp370 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp371)
  store %"class.sc_core::sc_simcontext"* %tmp371, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp371, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit79

_ZN7sc_core22sc_get_curr_simcontextEv.exit79:     ; preds = %.noexc76, %_ZN7sc_core17sc_process_handleD1Ev.exit75
  %tmp372 = phi %"class.sc_core::sc_simcontext"* [ %tmp371, %.noexc76 ], [ %tmp368, %_ZN7sc_core17sc_process_handleD1Ev.exit75 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %S_handle, %"class.sc_core::sc_simcontext"* %tmp372, i8* getelementptr inbounds ([2 x i8]* @.str12, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1SEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp14, %"class.sc_core::sc_spawn_options"* null)
  %tmp373 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %S_handle, i64 0, i32 0
  %tmp374 = load %"class.sc_core::sc_process_b"** %tmp373, align 8, !tbaa !0
  %tmp375 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp9, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp374, %"class.sc_core::sc_process_b"** %tmp375, align 8, !tbaa !0
  %tmp376 = icmp eq %"class.sc_core::sc_process_b"* %tmp374, null
  br i1 %tmp376, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit81, label %bb377

bb377:                                            ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit79
  %tmp378 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp374, i64 0, i32 15
  %tmp379 = load i32* %tmp378, align 4, !tbaa !4
  %tmp380 = icmp eq i32 %tmp379, 0
  br i1 %tmp380, label %bb381, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i80

bb381:                                            ; preds = %bb377
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i80: ; preds = %bb377
  %tmp382 = add nsw i32 %tmp379, 1
  store i32 %tmp382, i32* %tmp378, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit81

_ZN7sc_core17sc_process_handleC1ERKS0_.exit81:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i80, %_ZN7sc_core22sc_get_curr_simcontextEv.exit79
  %tmp383 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp44, %"class.sc_core::sc_process_handle"* %tmp9)
  %tmp384 = load %"class.sc_core::sc_process_b"** %tmp375, align 8, !tbaa !0
  %tmp385 = icmp eq %"class.sc_core::sc_process_b"* %tmp384, null
  br i1 %tmp385, label %_ZN7sc_core17sc_process_handleD1Ev.exit84, label %bb386

bb386:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit81
  %tmp387 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp384, i64 0, i32 15
  %tmp388 = load i32* %tmp387, align 4, !tbaa !4
  %tmp389 = add nsw i32 %tmp388, -1
  store i32 %tmp389, i32* %tmp387, align 4, !tbaa !4
  %tmp390 = icmp eq i32 %tmp389, 0
  br i1 %tmp390, label %bb391, label %_ZN7sc_core17sc_process_handleD1Ev.exit84

bb391:                                            ; preds = %bb386
  %tmp392 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp393 = icmp eq %"class.sc_core::sc_process_b"* %tmp392, null
  br i1 %tmp393, label %bb398, label %.noexc83

.noexc83:                                         ; preds = %bb391
  %tmp394 = bitcast %"class.sc_core::sc_process_b"* %tmp392 to void (%"class.sc_core::sc_process_b"*)***
  %tmp395 = load void (%"class.sc_core::sc_process_b"*)*** %tmp394, align 8, !tbaa !3
  %tmp396 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp395, i64 6
  %tmp397 = load void (%"class.sc_core::sc_process_b"*)** %tmp396, align 8
  call void %tmp397(%"class.sc_core::sc_process_b"* %tmp392)
  %.pre.i.i.i82 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb398

bb398:                                            ; preds = %.noexc83, %bb391
  %tmp399 = phi %"class.sc_core::sc_process_b"* [ null, %bb391 ], [ %.pre.i.i.i82, %.noexc83 ]
  %tmp400 = icmp eq %"class.sc_core::sc_process_b"* %tmp399, %tmp384
  br i1 %tmp400, label %bb401, label %bb402

bb401:                                            ; preds = %bb398
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb402:                                            ; preds = %bb398
  store %"class.sc_core::sc_process_b"* %tmp384, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit84

_ZN7sc_core17sc_process_handleD1Ev.exit84:        ; preds = %bb402, %bb386, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit81
  %tmp403 = load %"class.sc_core::sc_process_b"** %tmp373, align 8, !tbaa !0
  %tmp404 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp10, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp403, %"class.sc_core::sc_process_b"** %tmp404, align 8, !tbaa !0
  %tmp405 = icmp eq %"class.sc_core::sc_process_b"* %tmp403, null
  br i1 %tmp405, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit86, label %bb406

bb406:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit84
  %tmp407 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp403, i64 0, i32 15
  %tmp408 = load i32* %tmp407, align 4, !tbaa !4
  %tmp409 = icmp eq i32 %tmp408, 0
  br i1 %tmp409, label %bb410, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i85

bb410:                                            ; preds = %bb406
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i85: ; preds = %bb406
  %tmp411 = add nsw i32 %tmp408, 1
  store i32 %tmp411, i32* %tmp407, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit86

_ZN7sc_core17sc_process_handleC1ERKS0_.exit86:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i85, %_ZN7sc_core17sc_process_handleD1Ev.exit84
  %tmp412 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp74, %"class.sc_core::sc_process_handle"* %tmp10)
  %tmp413 = load %"class.sc_core::sc_process_b"** %tmp404, align 8, !tbaa !0
  %tmp414 = icmp eq %"class.sc_core::sc_process_b"* %tmp413, null
  br i1 %tmp414, label %_ZN7sc_core17sc_process_handleD1Ev.exit89, label %bb415

bb415:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit86
  %tmp416 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp413, i64 0, i32 15
  %tmp417 = load i32* %tmp416, align 4, !tbaa !4
  %tmp418 = add nsw i32 %tmp417, -1
  store i32 %tmp418, i32* %tmp416, align 4, !tbaa !4
  %tmp419 = icmp eq i32 %tmp418, 0
  br i1 %tmp419, label %bb420, label %_ZN7sc_core17sc_process_handleD1Ev.exit89

bb420:                                            ; preds = %bb415
  %tmp421 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp422 = icmp eq %"class.sc_core::sc_process_b"* %tmp421, null
  br i1 %tmp422, label %bb427, label %.noexc88

.noexc88:                                         ; preds = %bb420
  %tmp423 = bitcast %"class.sc_core::sc_process_b"* %tmp421 to void (%"class.sc_core::sc_process_b"*)***
  %tmp424 = load void (%"class.sc_core::sc_process_b"*)*** %tmp423, align 8, !tbaa !3
  %tmp425 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp424, i64 6
  %tmp426 = load void (%"class.sc_core::sc_process_b"*)** %tmp425, align 8
  call void %tmp426(%"class.sc_core::sc_process_b"* %tmp421)
  %.pre.i.i.i87 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb427

bb427:                                            ; preds = %.noexc88, %bb420
  %tmp428 = phi %"class.sc_core::sc_process_b"* [ null, %bb420 ], [ %.pre.i.i.i87, %.noexc88 ]
  %tmp429 = icmp eq %"class.sc_core::sc_process_b"* %tmp428, %tmp413
  br i1 %tmp429, label %bb430, label %bb431

bb430:                                            ; preds = %bb427
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb431:                                            ; preds = %bb427
  store %"class.sc_core::sc_process_b"* %tmp413, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit89

_ZN7sc_core17sc_process_handleD1Ev.exit89:        ; preds = %bb431, %bb415, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit86
  %tmp432 = load %"class.sc_core::sc_process_b"** %tmp373, align 8, !tbaa !0
  %tmp433 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp11, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp432, %"class.sc_core::sc_process_b"** %tmp433, align 8, !tbaa !0
  %tmp434 = icmp eq %"class.sc_core::sc_process_b"* %tmp432, null
  br i1 %tmp434, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit91, label %bb435

bb435:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit89
  %tmp436 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp432, i64 0, i32 15
  %tmp437 = load i32* %tmp436, align 4, !tbaa !4
  %tmp438 = icmp eq i32 %tmp437, 0
  br i1 %tmp438, label %bb439, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i90

bb439:                                            ; preds = %bb435
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str19, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i90: ; preds = %bb435
  %tmp440 = add nsw i32 %tmp437, 1
  store i32 %tmp440, i32* %tmp436, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit91

_ZN7sc_core17sc_process_handleC1ERKS0_.exit91:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i90, %_ZN7sc_core17sc_process_handleD1Ev.exit89
  %tmp441 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp104, %"class.sc_core::sc_process_handle"* %tmp11)
  %tmp442 = load %"class.sc_core::sc_process_b"** %tmp433, align 8, !tbaa !0
  %tmp443 = icmp eq %"class.sc_core::sc_process_b"* %tmp442, null
  br i1 %tmp443, label %_ZN7sc_core17sc_process_handleD1Ev.exit94, label %bb444

bb444:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit91
  %tmp445 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp442, i64 0, i32 15
  %tmp446 = load i32* %tmp445, align 4, !tbaa !4
  %tmp447 = add nsw i32 %tmp446, -1
  store i32 %tmp447, i32* %tmp445, align 4, !tbaa !4
  %tmp448 = icmp eq i32 %tmp447, 0
  br i1 %tmp448, label %bb449, label %_ZN7sc_core17sc_process_handleD1Ev.exit94

bb449:                                            ; preds = %bb444
  %tmp450 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp451 = icmp eq %"class.sc_core::sc_process_b"* %tmp450, null
  br i1 %tmp451, label %bb456, label %.noexc93

.noexc93:                                         ; preds = %bb449
  %tmp452 = bitcast %"class.sc_core::sc_process_b"* %tmp450 to void (%"class.sc_core::sc_process_b"*)***
  %tmp453 = load void (%"class.sc_core::sc_process_b"*)*** %tmp452, align 8, !tbaa !3
  %tmp454 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp453, i64 6
  %tmp455 = load void (%"class.sc_core::sc_process_b"*)** %tmp454, align 8
  call void %tmp455(%"class.sc_core::sc_process_b"* %tmp450)
  %.pre.i.i.i92 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb456

bb456:                                            ; preds = %.noexc93, %bb449
  %tmp457 = phi %"class.sc_core::sc_process_b"* [ null, %bb449 ], [ %.pre.i.i.i92, %.noexc93 ]
  %tmp458 = icmp eq %"class.sc_core::sc_process_b"* %tmp457, %tmp442
  br i1 %tmp458, label %bb459, label %bb460

bb459:                                            ; preds = %bb456
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb460:                                            ; preds = %bb456
  store %"class.sc_core::sc_process_b"* %tmp442, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit94

_ZN7sc_core17sc_process_handleD1Ev.exit94:        ; preds = %bb460, %bb444, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit91
  %tmp461 = load %"class.sc_core::sc_process_b"** %tmp373, align 8, !tbaa !0
  %tmp462 = icmp eq %"class.sc_core::sc_process_b"* %tmp461, null
  br i1 %tmp462, label %_ZN7sc_core17sc_process_handleD1Ev.exit97, label %bb463

bb463:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit94
  %tmp464 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp461, i64 0, i32 15
  %tmp465 = load i32* %tmp464, align 4, !tbaa !4
  %tmp466 = add nsw i32 %tmp465, -1
  store i32 %tmp466, i32* %tmp464, align 4, !tbaa !4
  %tmp467 = icmp eq i32 %tmp466, 0
  br i1 %tmp467, label %bb468, label %_ZN7sc_core17sc_process_handleD1Ev.exit97

bb468:                                            ; preds = %bb463
  %tmp469 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp470 = icmp eq %"class.sc_core::sc_process_b"* %tmp469, null
  br i1 %tmp470, label %bb475, label %.noexc96

.noexc96:                                         ; preds = %bb468
  %tmp471 = bitcast %"class.sc_core::sc_process_b"* %tmp469 to void (%"class.sc_core::sc_process_b"*)***
  %tmp472 = load void (%"class.sc_core::sc_process_b"*)*** %tmp471, align 8, !tbaa !3
  %tmp473 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp472, i64 6
  %tmp474 = load void (%"class.sc_core::sc_process_b"*)** %tmp473, align 8
  call void %tmp474(%"class.sc_core::sc_process_b"* %tmp469)
  %.pre.i.i.i95 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb475

bb475:                                            ; preds = %.noexc96, %bb468
  %tmp476 = phi %"class.sc_core::sc_process_b"* [ null, %bb468 ], [ %.pre.i.i.i95, %.noexc96 ]
  %tmp477 = icmp eq %"class.sc_core::sc_process_b"* %tmp476, %tmp461
  br i1 %tmp477, label %bb478, label %bb479

bb478:                                            ; preds = %bb475
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str17, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str18, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb479:                                            ; preds = %bb475
  store %"class.sc_core::sc_process_b"* %tmp461, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit97

_ZN7sc_core17sc_process_handleD1Ev.exit97:        ; preds = %bb479, %bb463, %_ZN7sc_core17sc_process_handleD1Ev.exit94
  ret void
}

declare void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

define linkonce_odr void @_ZN3top1PEv(%class.top* nocapture %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.top* %this, i64 0, i32 1
  %tmp1 = load i32* %tmp, align 4, !tbaa !4
  %tmp2 = icmp eq i32 %tmp1, 0
  %.pre = getelementptr inbounds %class.top* %this, i64 0, i32 2
  br i1 %tmp2, label %._crit_edge, label %bb3

bb3:                                              ; preds = %bb
  store i32 0, i32* %.pre, align 4, !tbaa !4
  %tmp4 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str20, i64 0, i64 0), i64 2)
  %tmp5 = load i32* %tmp, align 4, !tbaa !4
  %tmp6 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp5)
  %tmp7 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp6, i8* getelementptr inbounds ([2 x i8]* @.str14, i64 0, i64 0), i64 1)
  %tmp8 = load i32* %.pre, align 4, !tbaa !4
  %tmp9 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp6, i32 %tmp8)
  %tmp10 = bitcast %"class.std::basic_ostream"* %tmp9 to i8**
  %tmp11 = load i8** %tmp10, align 8, !tbaa !3
  %tmp12 = getelementptr i8* %tmp11, i64 -24
  %tmp13 = bitcast i8* %tmp12 to i64*
  %tmp14 = load i64* %tmp13, align 8
  %tmp15 = bitcast %"class.std::basic_ostream"* %tmp9 to i8*
  %.sum.i = add i64 %tmp14, 240
  %tmp16 = getelementptr inbounds i8* %tmp15, i64 %.sum.i
  %tmp17 = bitcast i8* %tmp16 to %"class.std::ctype"**
  %tmp18 = load %"class.std::ctype"** %tmp17, align 8, !tbaa !0
  %tmp19 = icmp eq %"class.std::ctype"* %tmp18, null
  br i1 %tmp19, label %bb20, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb20:                                             ; preds = %bb3
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %bb3
  %tmp21 = alloca %"class.sc_core::sc_time", align 8
  %tmp22 = getelementptr inbounds %"class.std::ctype"* %tmp18, i64 0, i32 6
  %tmp23 = load i8* %tmp22, align 1, !tbaa !1
  %tmp24 = icmp eq i8 %tmp23, 0
  br i1 %tmp24, label %bb28, label %bb25

bb25:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp26 = getelementptr inbounds %"class.std::ctype"* %tmp18, i64 0, i32 7, i64 10
  %tmp27 = load i8* %tmp26, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb28:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp18)
  %tmp29 = bitcast %"class.std::ctype"* %tmp18 to i8 (%"class.std::ctype"*, i8)***
  %tmp30 = load i8 (%"class.std::ctype"*, i8)*** %tmp29, align 8, !tbaa !3
  %tmp31 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp30, i64 6
  %tmp32 = load i8 (%"class.std::ctype"*, i8)** %tmp31, align 8
  %tmp33 = call signext i8 %tmp32(%"class.std::ctype"* %tmp18, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb28, %bb25
  %.0.i.i.i = phi i8 [ %tmp27, %bb25 ], [ %tmp33, %bb28 ]
  %tmp34 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp9, i8 signext %.0.i.i.i)
  %tmp35 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp34)
  %tmp36 = bitcast %"class.sc_core::sc_time"* %tmp21 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp36)
  %tmp37 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp38 = load %"class.sc_core::sc_simcontext"** %tmp37, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp21, double 1.200000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp38)
  %tmp39 = load %"class.sc_core::sc_simcontext"** %tmp37, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp21, %"class.sc_core::sc_simcontext"* %tmp39)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp36)
  br label %._crit_edge

._crit_edge:                                      ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit, %bb
  %tmp40 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str20, i64 0, i64 0), i64 2)
  %tmp41 = load i32* %tmp, align 4, !tbaa !4
  %tmp42 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp41)
  %tmp43 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp42, i8* getelementptr inbounds ([2 x i8]* @.str14, i64 0, i64 0), i64 1)
  %tmp44 = load i32* %.pre, align 4, !tbaa !4
  %tmp45 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp42, i32 %tmp44)
  %tmp46 = bitcast %"class.std::basic_ostream"* %tmp45 to i8**
  %tmp47 = load i8** %tmp46, align 8, !tbaa !3
  %tmp48 = getelementptr i8* %tmp47, i64 -24
  %tmp49 = bitcast i8* %tmp48 to i64*
  %tmp50 = load i64* %tmp49, align 8
  %tmp51 = bitcast %"class.std::basic_ostream"* %tmp45 to i8*
  %.sum.i1 = add i64 %tmp50, 240
  %tmp52 = getelementptr inbounds i8* %tmp51, i64 %.sum.i1
  %tmp53 = bitcast i8* %tmp52 to %"class.std::ctype"**
  %tmp54 = load %"class.std::ctype"** %tmp53, align 8, !tbaa !0
  %tmp55 = icmp eq %"class.std::ctype"* %tmp54, null
  br i1 %tmp55, label %bb56, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2

bb56:                                             ; preds = %._crit_edge
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2: ; preds = %._crit_edge
  %tmp57 = getelementptr inbounds %"class.std::ctype"* %tmp54, i64 0, i32 6
  %tmp58 = load i8* %tmp57, align 1, !tbaa !1
  %tmp59 = icmp eq i8 %tmp58, 0
  br i1 %tmp59, label %bb63, label %bb60

bb60:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  %tmp61 = getelementptr inbounds %"class.std::ctype"* %tmp54, i64 0, i32 7, i64 10
  %tmp62 = load i8* %tmp61, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

bb63:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp54)
  %tmp64 = bitcast %"class.std::ctype"* %tmp54 to i8 (%"class.std::ctype"*, i8)***
  %tmp65 = load i8 (%"class.std::ctype"*, i8)*** %tmp64, align 8, !tbaa !3
  %tmp66 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp65, i64 6
  %tmp67 = load i8 (%"class.std::ctype"*, i8)** %tmp66, align 8
  %tmp68 = call signext i8 %tmp67(%"class.std::ctype"* %tmp54, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4: ; preds = %bb63, %bb60
  %.0.i.i.i3 = phi i8 [ %tmp62, %bb60 ], [ %tmp68, %bb63 ]
  %tmp69 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp45, i8 signext %.0.i.i.i3)
  %tmp70 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp69)
  ret void
}

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

define linkonce_odr void @_ZN3top1QEv(%class.top* %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.top* %this, i64 0, i32 3
  %tmp1 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp2 = load %"class.sc_core::sc_simcontext"** %tmp1, align 8, !tbaa !0
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp, %"class.sc_core::sc_simcontext"* %tmp2)
  %tmp3 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  store i32 19, i32* %tmp3, align 4, !tbaa !4
  %tmp4 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str16, i64 0, i64 0), i64 2)
  %tmp5 = load i32* %tmp3, align 4, !tbaa !4
  %tmp6 = tail call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp5)
  %tmp7 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp6, i8* getelementptr inbounds ([2 x i8]* @.str14, i64 0, i64 0), i64 1)
  %tmp8 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  %tmp9 = load i32* %tmp8, align 4, !tbaa !4
  %tmp10 = tail call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp6, i32 %tmp9)
  %tmp11 = bitcast %"class.std::basic_ostream"* %tmp10 to i8**
  %tmp12 = load i8** %tmp11, align 8, !tbaa !3
  %tmp13 = getelementptr i8* %tmp12, i64 -24
  %tmp14 = bitcast i8* %tmp13 to i64*
  %tmp15 = load i64* %tmp14, align 8
  %tmp16 = bitcast %"class.std::basic_ostream"* %tmp10 to i8*
  %.sum.i = add i64 %tmp15, 240
  %tmp17 = getelementptr inbounds i8* %tmp16, i64 %.sum.i
  %tmp18 = bitcast i8* %tmp17 to %"class.std::ctype"**
  %tmp19 = load %"class.std::ctype"** %tmp18, align 8, !tbaa !0
  %tmp20 = icmp eq %"class.std::ctype"* %tmp19, null
  br i1 %tmp20, label %bb21, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb21:                                             ; preds = %bb
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %bb
  %tmp22 = getelementptr inbounds %"class.std::ctype"* %tmp19, i64 0, i32 6
  %tmp23 = load i8* %tmp22, align 1, !tbaa !1
  %tmp24 = icmp eq i8 %tmp23, 0
  br i1 %tmp24, label %bb28, label %bb25

bb25:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp26 = getelementptr inbounds %"class.std::ctype"* %tmp19, i64 0, i32 7, i64 10
  %tmp27 = load i8* %tmp26, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb28:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp19)
  %tmp29 = bitcast %"class.std::ctype"* %tmp19 to i8 (%"class.std::ctype"*, i8)***
  %tmp30 = load i8 (%"class.std::ctype"*, i8)*** %tmp29, align 8, !tbaa !3
  %tmp31 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp30, i64 6
  %tmp32 = load i8 (%"class.std::ctype"*, i8)** %tmp31, align 8
  %tmp33 = tail call signext i8 %tmp32(%"class.std::ctype"* %tmp19, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb28, %bb25
  %.0.i.i.i = phi i8 [ %tmp27, %bb25 ], [ %tmp33, %bb28 ]
  %tmp34 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp10, i8 signext %.0.i.i.i)
  %tmp35 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp34)
  ret void
}

define linkonce_odr void @_ZN3top1REv(%class.top* %this) uwtable align 2 {
bb:
  %tmp = alloca %"class.sc_core::sc_time", align 8
  %tmp1 = bitcast %"class.sc_core::sc_time"* %tmp to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp1)
  %tmp2 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp3 = load %"class.sc_core::sc_simcontext"** %tmp2, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, double 1.200000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp3)
  %tmp4 = load %"class.sc_core::sc_simcontext"** %tmp2, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, %"class.sc_core::sc_simcontext"* %tmp4)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp1)
  %tmp5 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  %tmp6 = load i32* %tmp5, align 4, !tbaa !4
  %tmp7 = icmp eq i32 %tmp6, 0
  br i1 %tmp7, label %bb8, label %._crit_edge

._crit_edge:                                      ; preds = %bb
  %.pre = getelementptr inbounds %class.top* %this, i64 0, i32 1
  br label %bb42

bb8:                                              ; preds = %bb
  %tmp9 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp9)
  %tmp10 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  store i32 10, i32* %tmp10, align 4, !tbaa !4
  %tmp11 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str15, i64 0, i64 0), i64 2)
  %tmp12 = load i32* %tmp10, align 4, !tbaa !4
  %tmp13 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp12)
  %tmp14 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp13, i8* getelementptr inbounds ([2 x i8]* @.str14, i64 0, i64 0), i64 1)
  %tmp15 = load i32* %tmp5, align 4, !tbaa !4
  %tmp16 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp13, i32 %tmp15)
  %tmp17 = bitcast %"class.std::basic_ostream"* %tmp16 to i8**
  %tmp18 = load i8** %tmp17, align 8, !tbaa !3
  %tmp19 = getelementptr i8* %tmp18, i64 -24
  %tmp20 = bitcast i8* %tmp19 to i64*
  %tmp21 = load i64* %tmp20, align 8
  %tmp22 = bitcast %"class.std::basic_ostream"* %tmp16 to i8*
  %.sum.i = add i64 %tmp21, 240
  %tmp23 = getelementptr inbounds i8* %tmp22, i64 %.sum.i
  %tmp24 = bitcast i8* %tmp23 to %"class.std::ctype"**
  %tmp25 = load %"class.std::ctype"** %tmp24, align 8, !tbaa !0
  %tmp26 = icmp eq %"class.std::ctype"* %tmp25, null
  br i1 %tmp26, label %bb27, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb27:                                             ; preds = %bb8
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %bb8
  %tmp28 = getelementptr inbounds %"class.std::ctype"* %tmp25, i64 0, i32 6
  %tmp29 = load i8* %tmp28, align 1, !tbaa !1
  %tmp30 = icmp eq i8 %tmp29, 0
  br i1 %tmp30, label %bb34, label %bb31

bb31:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp32 = getelementptr inbounds %"class.std::ctype"* %tmp25, i64 0, i32 7, i64 10
  %tmp33 = load i8* %tmp32, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb34:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp25)
  %tmp35 = bitcast %"class.std::ctype"* %tmp25 to i8 (%"class.std::ctype"*, i8)***
  %tmp36 = load i8 (%"class.std::ctype"*, i8)*** %tmp35, align 8, !tbaa !3
  %tmp37 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp36, i64 6
  %tmp38 = load i8 (%"class.std::ctype"*, i8)** %tmp37, align 8
  %tmp39 = call signext i8 %tmp38(%"class.std::ctype"* %tmp25, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb34, %bb31
  %.0.i.i.i = phi i8 [ %tmp33, %bb31 ], [ %tmp39, %bb34 ]
  %tmp40 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp16, i8 signext %.0.i.i.i)
  %tmp41 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp40)
  br label %bb42

bb42:                                             ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit, %._crit_edge
  %.pre-phi = phi i32* [ %.pre, %._crit_edge ], [ %tmp10, %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit ]
  %tmp43 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str15, i64 0, i64 0), i64 2)
  %tmp44 = load i32* %.pre-phi, align 4, !tbaa !4
  %tmp45 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp44)
  %tmp46 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp45, i8* getelementptr inbounds ([2 x i8]* @.str14, i64 0, i64 0), i64 1)
  %tmp47 = load i32* %tmp5, align 4, !tbaa !4
  %tmp48 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp45, i32 %tmp47)
  %tmp49 = bitcast %"class.std::basic_ostream"* %tmp48 to i8**
  %tmp50 = load i8** %tmp49, align 8, !tbaa !3
  %tmp51 = getelementptr i8* %tmp50, i64 -24
  %tmp52 = bitcast i8* %tmp51 to i64*
  %tmp53 = load i64* %tmp52, align 8
  %tmp54 = bitcast %"class.std::basic_ostream"* %tmp48 to i8*
  %.sum.i1 = add i64 %tmp53, 240
  %tmp55 = getelementptr inbounds i8* %tmp54, i64 %.sum.i1
  %tmp56 = bitcast i8* %tmp55 to %"class.std::ctype"**
  %tmp57 = load %"class.std::ctype"** %tmp56, align 8, !tbaa !0
  %tmp58 = icmp eq %"class.std::ctype"* %tmp57, null
  br i1 %tmp58, label %bb59, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2

bb59:                                             ; preds = %bb42
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2: ; preds = %bb42
  %tmp60 = getelementptr inbounds %"class.std::ctype"* %tmp57, i64 0, i32 6
  %tmp61 = load i8* %tmp60, align 1, !tbaa !1
  %tmp62 = icmp eq i8 %tmp61, 0
  br i1 %tmp62, label %bb66, label %bb63

bb63:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  %tmp64 = getelementptr inbounds %"class.std::ctype"* %tmp57, i64 0, i32 7, i64 10
  %tmp65 = load i8* %tmp64, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

bb66:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp57)
  %tmp67 = bitcast %"class.std::ctype"* %tmp57 to i8 (%"class.std::ctype"*, i8)***
  %tmp68 = load i8 (%"class.std::ctype"*, i8)*** %tmp67, align 8, !tbaa !3
  %tmp69 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp68, i64 6
  %tmp70 = load i8 (%"class.std::ctype"*, i8)** %tmp69, align 8
  %tmp71 = call signext i8 %tmp70(%"class.std::ctype"* %tmp57, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4: ; preds = %bb66, %bb63
  %.0.i.i.i3 = phi i8 [ %tmp65, %bb63 ], [ %tmp71, %bb66 ]
  %tmp72 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp48, i8 signext %.0.i.i.i3)
  %tmp73 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp72)
  ret void
}

define linkonce_odr void @_ZN3top1SEv(%class.top* nocapture %this) uwtable align 2 {
bb:
  %tmp = alloca %"class.sc_core::sc_time", align 8
  %tmp1 = bitcast %"class.sc_core::sc_time"* %tmp to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp1)
  %tmp2 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp3 = load %"class.sc_core::sc_simcontext"** %tmp2, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, double 1.200000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp3)
  %tmp4 = load %"class.sc_core::sc_simcontext"** %tmp2, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, %"class.sc_core::sc_simcontext"* %tmp4)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp1)
  %tmp5 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  store i32 1, i32* %tmp5, align 4, !tbaa !4
  %tmp6 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str13, i64 0, i64 0), i64 2)
  %tmp7 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  %tmp8 = load i32* %tmp7, align 4, !tbaa !4
  %tmp9 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp8)
  %tmp10 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp9, i8* getelementptr inbounds ([2 x i8]* @.str14, i64 0, i64 0), i64 1)
  %tmp11 = load i32* %tmp5, align 4, !tbaa !4
  %tmp12 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp9, i32 %tmp11)
  %tmp13 = bitcast %"class.std::basic_ostream"* %tmp12 to i8**
  %tmp14 = load i8** %tmp13, align 8, !tbaa !3
  %tmp15 = getelementptr i8* %tmp14, i64 -24
  %tmp16 = bitcast i8* %tmp15 to i64*
  %tmp17 = load i64* %tmp16, align 8
  %tmp18 = bitcast %"class.std::basic_ostream"* %tmp12 to i8*
  %.sum.i = add i64 %tmp17, 240
  %tmp19 = getelementptr inbounds i8* %tmp18, i64 %.sum.i
  %tmp20 = bitcast i8* %tmp19 to %"class.std::ctype"**
  %tmp21 = load %"class.std::ctype"** %tmp20, align 8, !tbaa !0
  %tmp22 = icmp eq %"class.std::ctype"* %tmp21, null
  br i1 %tmp22, label %bb23, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb23:                                             ; preds = %bb
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %bb
  %tmp24 = getelementptr inbounds %"class.std::ctype"* %tmp21, i64 0, i32 6
  %tmp25 = load i8* %tmp24, align 1, !tbaa !1
  %tmp26 = icmp eq i8 %tmp25, 0
  br i1 %tmp26, label %bb30, label %bb27

bb27:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp28 = getelementptr inbounds %"class.std::ctype"* %tmp21, i64 0, i32 7, i64 10
  %tmp29 = load i8* %tmp28, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb30:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp21)
  %tmp31 = bitcast %"class.std::ctype"* %tmp21 to i8 (%"class.std::ctype"*, i8)***
  %tmp32 = load i8 (%"class.std::ctype"*, i8)*** %tmp31, align 8, !tbaa !3
  %tmp33 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp32, i64 6
  %tmp34 = load i8 (%"class.std::ctype"*, i8)** %tmp33, align 8
  %tmp35 = call signext i8 %tmp34(%"class.std::ctype"* %tmp21, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb30, %bb27
  %.0.i.i.i = phi i8 [ %tmp29, %bb27 ], [ %tmp35, %bb30 ]
  %tmp36 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp12, i8 signext %.0.i.i.i)
  %tmp37 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp36)
  ret void
}

declare %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"*, i32)

declare void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, double, i32, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"*)

declare void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"*, %"class.sc_core::sc_simcontext"*)

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

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
!4 = metadata !{metadata !"int", metadata !1}
!5 = metadata !{metadata !"_ZTSN7sc_core8sc_event8notify_tE", metadata !1}
