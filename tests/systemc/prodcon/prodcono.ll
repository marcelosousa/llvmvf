; ModuleID = 'prodcono.bc'
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
@.str10 = private unnamed_addr constant [2 x i8] c"C\00", align 1
@_ZSt4cout = external global %"class.std::basic_ostream"
@.str11 = private unnamed_addr constant [3 x i8] c"c \00", align 1
@.str12 = private unnamed_addr constant [2 x i8] c" \00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str13 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str14 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str15 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_increment()\00", align 1
@.str16 = private unnamed_addr constant [3 x i8] c"p \00", align 1
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
  %C_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp4 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp5 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp6 = getelementptr inbounds %class.top* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp6, %"class.sc_core::sc_module_name"* %name)
  %tmp7 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp7, align 8, !tbaa !3
  %tmp8 = getelementptr %class.top* %this, i64 0, i32 0, i32 1
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp8, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV3top, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp9, align 8, !tbaa !3
  %tmp10 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  store i32 %a, i32* %tmp10, align 4, !tbaa !4
  %tmp11 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  store i32 %b, i32* %tmp11, align 4, !tbaa !4
  %tmp12 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp13 = icmp eq %"class.sc_core::sc_simcontext"* %tmp12, null
  br i1 %tmp13, label %.noexc, label %bb16

.noexc:                                           ; preds = %bb
  %tmp14 = call noalias i8* @_Znwm(i64 248)
  %tmp15 = bitcast i8* %tmp14 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp15)
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp15, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %bb16

bb16:                                             ; preds = %.noexc, %bb
  %tmp17 = phi %"class.sc_core::sc_simcontext"* [ %tmp15, %.noexc ], [ %tmp12, %bb ]
  %tmp18 = getelementptr inbounds %class.top* %this, i64 0, i32 3, i32 0
  store %"class.sc_core::sc_simcontext"* %tmp17, %"class.sc_core::sc_simcontext"** %tmp18, align 8, !tbaa !0
  %tmp19 = getelementptr inbounds %class.top* %this, i64 0, i32 3, i32 1
  store i32 0, i32* %tmp19, align 4, !tbaa !5
  %tmp20 = getelementptr inbounds %class.top* %this, i64 0, i32 3, i32 2
  store i32 -1, i32* %tmp20, align 4, !tbaa !4
  %tmp21 = getelementptr inbounds %class.top* %this, i64 0, i32 3, i32 3
  %tmp22 = bitcast %"class.sc_core::sc_event_timed"** %tmp21 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp22, i8 0, i64 104, i32 8, i1 false)
  %tmp23 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp24 = icmp eq %"class.sc_core::sc_simcontext"* %tmp23, null
  br i1 %tmp24, label %.noexc9, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc9:                                          ; preds = %bb16
  %tmp25 = call noalias i8* @_Znwm(i64 248)
  %tmp26 = bitcast i8* %tmp25 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp26)
  store %"class.sc_core::sc_simcontext"* %tmp26, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp26, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc9, %bb16
  %tmp27 = phi %"class.sc_core::sc_simcontext"* [ %tmp26, %.noexc9 ], [ %tmp23, %bb16 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %P_handle, %"class.sc_core::sc_simcontext"* %tmp27, i8* getelementptr inbounds ([2 x i8]* @.str9, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1PEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp8, %"class.sc_core::sc_spawn_options"* null)
  %tmp28 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %P_handle, i64 0, i32 0
  %tmp29 = load %"class.sc_core::sc_process_b"** %tmp28, align 8, !tbaa !0
  %tmp30 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp29, %"class.sc_core::sc_process_b"** %tmp30, align 8, !tbaa !0
  %tmp31 = icmp eq %"class.sc_core::sc_process_b"* %tmp29, null
  br i1 %tmp31, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb32

bb32:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp33 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp29, i64 0, i32 15
  %tmp34 = load i32* %tmp33, align 4, !tbaa !4
  %tmp35 = icmp eq i32 %tmp34, 0
  br i1 %tmp35, label %bb36, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb36:                                             ; preds = %bb32
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str15, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb32
  %tmp37 = add nsw i32 %tmp34, 1
  store i32 %tmp37, i32* %tmp33, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp38 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 2
  %tmp39 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp38, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp40 = load %"class.sc_core::sc_process_b"** %tmp30, align 8, !tbaa !0
  %tmp41 = icmp eq %"class.sc_core::sc_process_b"* %tmp40, null
  br i1 %tmp41, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb42

bb42:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp43 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp40, i64 0, i32 15
  %tmp44 = load i32* %tmp43, align 4, !tbaa !4
  %tmp45 = add nsw i32 %tmp44, -1
  store i32 %tmp45, i32* %tmp43, align 4, !tbaa !4
  %tmp46 = icmp eq i32 %tmp45, 0
  br i1 %tmp46, label %bb47, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb47:                                             ; preds = %bb42
  %tmp48 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp49 = icmp eq %"class.sc_core::sc_process_b"* %tmp48, null
  br i1 %tmp49, label %bb54, label %.noexc12

.noexc12:                                         ; preds = %bb47
  %tmp50 = bitcast %"class.sc_core::sc_process_b"* %tmp48 to void (%"class.sc_core::sc_process_b"*)***
  %tmp51 = load void (%"class.sc_core::sc_process_b"*)*** %tmp50, align 8, !tbaa !3
  %tmp52 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp51, i64 6
  %tmp53 = load void (%"class.sc_core::sc_process_b"*)** %tmp52, align 8
  call void %tmp53(%"class.sc_core::sc_process_b"* %tmp48)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb54

bb54:                                             ; preds = %.noexc12, %bb47
  %tmp55 = phi %"class.sc_core::sc_process_b"* [ null, %bb47 ], [ %.pre.i.i.i, %.noexc12 ]
  %tmp56 = icmp eq %"class.sc_core::sc_process_b"* %tmp55, %tmp40
  br i1 %tmp56, label %bb57, label %bb58

bb57:                                             ; preds = %bb54
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb58:                                             ; preds = %bb54
  store %"class.sc_core::sc_process_b"* %tmp40, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb58, %bb42, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp59 = load %"class.sc_core::sc_process_b"** %tmp28, align 8, !tbaa !0
  %tmp60 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp59, %"class.sc_core::sc_process_b"** %tmp60, align 8, !tbaa !0
  %tmp61 = icmp eq %"class.sc_core::sc_process_b"* %tmp59, null
  br i1 %tmp61, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14, label %bb62

bb62:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp63 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp59, i64 0, i32 15
  %tmp64 = load i32* %tmp63, align 4, !tbaa !4
  %tmp65 = icmp eq i32 %tmp64, 0
  br i1 %tmp65, label %bb66, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13

bb66:                                             ; preds = %bb62
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str15, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13: ; preds = %bb62
  %tmp67 = add nsw i32 %tmp64, 1
  store i32 %tmp67, i32* %tmp63, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14

_ZN7sc_core17sc_process_handleC1ERKS0_.exit14:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp68 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 3
  %tmp69 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp68, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp70 = load %"class.sc_core::sc_process_b"** %tmp60, align 8, !tbaa !0
  %tmp71 = icmp eq %"class.sc_core::sc_process_b"* %tmp70, null
  br i1 %tmp71, label %_ZN7sc_core17sc_process_handleD1Ev.exit17, label %bb72

bb72:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14
  %tmp73 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp70, i64 0, i32 15
  %tmp74 = load i32* %tmp73, align 4, !tbaa !4
  %tmp75 = add nsw i32 %tmp74, -1
  store i32 %tmp75, i32* %tmp73, align 4, !tbaa !4
  %tmp76 = icmp eq i32 %tmp75, 0
  br i1 %tmp76, label %bb77, label %_ZN7sc_core17sc_process_handleD1Ev.exit17

bb77:                                             ; preds = %bb72
  %tmp78 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp79 = icmp eq %"class.sc_core::sc_process_b"* %tmp78, null
  br i1 %tmp79, label %bb84, label %.noexc16

.noexc16:                                         ; preds = %bb77
  %tmp80 = bitcast %"class.sc_core::sc_process_b"* %tmp78 to void (%"class.sc_core::sc_process_b"*)***
  %tmp81 = load void (%"class.sc_core::sc_process_b"*)*** %tmp80, align 8, !tbaa !3
  %tmp82 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp81, i64 6
  %tmp83 = load void (%"class.sc_core::sc_process_b"*)** %tmp82, align 8
  call void %tmp83(%"class.sc_core::sc_process_b"* %tmp78)
  %.pre.i.i.i15 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb84

bb84:                                             ; preds = %.noexc16, %bb77
  %tmp85 = phi %"class.sc_core::sc_process_b"* [ null, %bb77 ], [ %.pre.i.i.i15, %.noexc16 ]
  %tmp86 = icmp eq %"class.sc_core::sc_process_b"* %tmp85, %tmp70
  br i1 %tmp86, label %bb87, label %bb88

bb87:                                             ; preds = %bb84
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb88:                                             ; preds = %bb84
  store %"class.sc_core::sc_process_b"* %tmp70, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit17

_ZN7sc_core17sc_process_handleD1Ev.exit17:        ; preds = %bb88, %bb72, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14
  %tmp89 = load %"class.sc_core::sc_process_b"** %tmp28, align 8, !tbaa !0
  %tmp90 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp89, %"class.sc_core::sc_process_b"** %tmp90, align 8, !tbaa !0
  %tmp91 = icmp eq %"class.sc_core::sc_process_b"* %tmp89, null
  br i1 %tmp91, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19, label %bb92

bb92:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit17
  %tmp93 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp89, i64 0, i32 15
  %tmp94 = load i32* %tmp93, align 4, !tbaa !4
  %tmp95 = icmp eq i32 %tmp94, 0
  br i1 %tmp95, label %bb96, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18

bb96:                                             ; preds = %bb92
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str15, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18: ; preds = %bb92
  %tmp97 = add nsw i32 %tmp94, 1
  store i32 %tmp97, i32* %tmp93, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19

_ZN7sc_core17sc_process_handleC1ERKS0_.exit19:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18, %_ZN7sc_core17sc_process_handleD1Ev.exit17
  %tmp98 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 4
  %tmp99 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp98, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp100 = load %"class.sc_core::sc_process_b"** %tmp90, align 8, !tbaa !0
  %tmp101 = icmp eq %"class.sc_core::sc_process_b"* %tmp100, null
  br i1 %tmp101, label %_ZN7sc_core17sc_process_handleD1Ev.exit22, label %bb102

bb102:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19
  %tmp103 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp100, i64 0, i32 15
  %tmp104 = load i32* %tmp103, align 4, !tbaa !4
  %tmp105 = add nsw i32 %tmp104, -1
  store i32 %tmp105, i32* %tmp103, align 4, !tbaa !4
  %tmp106 = icmp eq i32 %tmp105, 0
  br i1 %tmp106, label %bb107, label %_ZN7sc_core17sc_process_handleD1Ev.exit22

bb107:                                            ; preds = %bb102
  %tmp108 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp109 = icmp eq %"class.sc_core::sc_process_b"* %tmp108, null
  br i1 %tmp109, label %bb114, label %.noexc21

.noexc21:                                         ; preds = %bb107
  %tmp110 = bitcast %"class.sc_core::sc_process_b"* %tmp108 to void (%"class.sc_core::sc_process_b"*)***
  %tmp111 = load void (%"class.sc_core::sc_process_b"*)*** %tmp110, align 8, !tbaa !3
  %tmp112 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp111, i64 6
  %tmp113 = load void (%"class.sc_core::sc_process_b"*)** %tmp112, align 8
  call void %tmp113(%"class.sc_core::sc_process_b"* %tmp108)
  %.pre.i.i.i20 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb114

bb114:                                            ; preds = %.noexc21, %bb107
  %tmp115 = phi %"class.sc_core::sc_process_b"* [ null, %bb107 ], [ %.pre.i.i.i20, %.noexc21 ]
  %tmp116 = icmp eq %"class.sc_core::sc_process_b"* %tmp115, %tmp100
  br i1 %tmp116, label %bb117, label %bb118

bb117:                                            ; preds = %bb114
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb118:                                            ; preds = %bb114
  store %"class.sc_core::sc_process_b"* %tmp100, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit22

_ZN7sc_core17sc_process_handleD1Ev.exit22:        ; preds = %bb118, %bb102, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19
  %tmp119 = load %"class.sc_core::sc_process_b"** %tmp28, align 8, !tbaa !0
  %tmp120 = icmp eq %"class.sc_core::sc_process_b"* %tmp119, null
  br i1 %tmp120, label %_ZN7sc_core17sc_process_handleD1Ev.exit25, label %bb121

bb121:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit22
  %tmp122 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp119, i64 0, i32 15
  %tmp123 = load i32* %tmp122, align 4, !tbaa !4
  %tmp124 = add nsw i32 %tmp123, -1
  store i32 %tmp124, i32* %tmp122, align 4, !tbaa !4
  %tmp125 = icmp eq i32 %tmp124, 0
  br i1 %tmp125, label %bb126, label %_ZN7sc_core17sc_process_handleD1Ev.exit25

bb126:                                            ; preds = %bb121
  %tmp127 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp128 = icmp eq %"class.sc_core::sc_process_b"* %tmp127, null
  br i1 %tmp128, label %bb133, label %.noexc24

.noexc24:                                         ; preds = %bb126
  %tmp129 = bitcast %"class.sc_core::sc_process_b"* %tmp127 to void (%"class.sc_core::sc_process_b"*)***
  %tmp130 = load void (%"class.sc_core::sc_process_b"*)*** %tmp129, align 8, !tbaa !3
  %tmp131 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp130, i64 6
  %tmp132 = load void (%"class.sc_core::sc_process_b"*)** %tmp131, align 8
  call void %tmp132(%"class.sc_core::sc_process_b"* %tmp127)
  %.pre.i.i.i23 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb133

bb133:                                            ; preds = %.noexc24, %bb126
  %tmp134 = phi %"class.sc_core::sc_process_b"* [ null, %bb126 ], [ %.pre.i.i.i23, %.noexc24 ]
  %tmp135 = icmp eq %"class.sc_core::sc_process_b"* %tmp134, %tmp119
  br i1 %tmp135, label %bb136, label %bb137

bb136:                                            ; preds = %bb133
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb137:                                            ; preds = %bb133
  store %"class.sc_core::sc_process_b"* %tmp119, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit25

_ZN7sc_core17sc_process_handleD1Ev.exit25:        ; preds = %bb137, %bb121, %_ZN7sc_core17sc_process_handleD1Ev.exit22
  %tmp138 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %tmp139 = icmp eq %"class.sc_core::sc_simcontext"* %tmp138, null
  br i1 %tmp139, label %.noexc26, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit29

.noexc26:                                         ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit25
  %tmp140 = call noalias i8* @_Znwm(i64 248)
  %tmp141 = bitcast i8* %tmp140 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp141)
  store %"class.sc_core::sc_simcontext"* %tmp141, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %tmp141, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit29

_ZN7sc_core22sc_get_curr_simcontextEv.exit29:     ; preds = %.noexc26, %_ZN7sc_core17sc_process_handleD1Ev.exit25
  %tmp142 = phi %"class.sc_core::sc_simcontext"* [ %tmp141, %.noexc26 ], [ %tmp138, %_ZN7sc_core17sc_process_handleD1Ev.exit25 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %C_handle, %"class.sc_core::sc_simcontext"* %tmp142, i8* getelementptr inbounds ([2 x i8]* @.str10, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.top*)* @_ZN3top1CEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp8, %"class.sc_core::sc_spawn_options"* null)
  %tmp143 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %C_handle, i64 0, i32 0
  %tmp144 = load %"class.sc_core::sc_process_b"** %tmp143, align 8, !tbaa !0
  %tmp145 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp3, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp144, %"class.sc_core::sc_process_b"** %tmp145, align 8, !tbaa !0
  %tmp146 = icmp eq %"class.sc_core::sc_process_b"* %tmp144, null
  br i1 %tmp146, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31, label %bb147

bb147:                                            ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit29
  %tmp148 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp144, i64 0, i32 15
  %tmp149 = load i32* %tmp148, align 4, !tbaa !4
  %tmp150 = icmp eq i32 %tmp149, 0
  br i1 %tmp150, label %bb151, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30

bb151:                                            ; preds = %bb147
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str15, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30: ; preds = %bb147
  %tmp152 = add nsw i32 %tmp149, 1
  store i32 %tmp152, i32* %tmp148, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31

_ZN7sc_core17sc_process_handleC1ERKS0_.exit31:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30, %_ZN7sc_core22sc_get_curr_simcontextEv.exit29
  %tmp153 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp38, %"class.sc_core::sc_process_handle"* %tmp3)
  %tmp154 = load %"class.sc_core::sc_process_b"** %tmp145, align 8, !tbaa !0
  %tmp155 = icmp eq %"class.sc_core::sc_process_b"* %tmp154, null
  br i1 %tmp155, label %_ZN7sc_core17sc_process_handleD1Ev.exit34, label %bb156

bb156:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31
  %tmp157 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp154, i64 0, i32 15
  %tmp158 = load i32* %tmp157, align 4, !tbaa !4
  %tmp159 = add nsw i32 %tmp158, -1
  store i32 %tmp159, i32* %tmp157, align 4, !tbaa !4
  %tmp160 = icmp eq i32 %tmp159, 0
  br i1 %tmp160, label %bb161, label %_ZN7sc_core17sc_process_handleD1Ev.exit34

bb161:                                            ; preds = %bb156
  %tmp162 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp163 = icmp eq %"class.sc_core::sc_process_b"* %tmp162, null
  br i1 %tmp163, label %bb168, label %.noexc33

.noexc33:                                         ; preds = %bb161
  %tmp164 = bitcast %"class.sc_core::sc_process_b"* %tmp162 to void (%"class.sc_core::sc_process_b"*)***
  %tmp165 = load void (%"class.sc_core::sc_process_b"*)*** %tmp164, align 8, !tbaa !3
  %tmp166 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp165, i64 6
  %tmp167 = load void (%"class.sc_core::sc_process_b"*)** %tmp166, align 8
  call void %tmp167(%"class.sc_core::sc_process_b"* %tmp162)
  %.pre.i.i.i32 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb168

bb168:                                            ; preds = %.noexc33, %bb161
  %tmp169 = phi %"class.sc_core::sc_process_b"* [ null, %bb161 ], [ %.pre.i.i.i32, %.noexc33 ]
  %tmp170 = icmp eq %"class.sc_core::sc_process_b"* %tmp169, %tmp154
  br i1 %tmp170, label %bb171, label %bb172

bb171:                                            ; preds = %bb168
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb172:                                            ; preds = %bb168
  store %"class.sc_core::sc_process_b"* %tmp154, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit34

_ZN7sc_core17sc_process_handleD1Ev.exit34:        ; preds = %bb172, %bb156, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31
  %tmp173 = load %"class.sc_core::sc_process_b"** %tmp143, align 8, !tbaa !0
  %tmp174 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp4, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp173, %"class.sc_core::sc_process_b"** %tmp174, align 8, !tbaa !0
  %tmp175 = icmp eq %"class.sc_core::sc_process_b"* %tmp173, null
  br i1 %tmp175, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36, label %bb176

bb176:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit34
  %tmp177 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp173, i64 0, i32 15
  %tmp178 = load i32* %tmp177, align 4, !tbaa !4
  %tmp179 = icmp eq i32 %tmp178, 0
  br i1 %tmp179, label %bb180, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35

bb180:                                            ; preds = %bb176
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str15, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35: ; preds = %bb176
  %tmp181 = add nsw i32 %tmp178, 1
  store i32 %tmp181, i32* %tmp177, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36

_ZN7sc_core17sc_process_handleC1ERKS0_.exit36:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35, %_ZN7sc_core17sc_process_handleD1Ev.exit34
  %tmp182 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp68, %"class.sc_core::sc_process_handle"* %tmp4)
  %tmp183 = load %"class.sc_core::sc_process_b"** %tmp174, align 8, !tbaa !0
  %tmp184 = icmp eq %"class.sc_core::sc_process_b"* %tmp183, null
  br i1 %tmp184, label %_ZN7sc_core17sc_process_handleD1Ev.exit39, label %bb185

bb185:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36
  %tmp186 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp183, i64 0, i32 15
  %tmp187 = load i32* %tmp186, align 4, !tbaa !4
  %tmp188 = add nsw i32 %tmp187, -1
  store i32 %tmp188, i32* %tmp186, align 4, !tbaa !4
  %tmp189 = icmp eq i32 %tmp188, 0
  br i1 %tmp189, label %bb190, label %_ZN7sc_core17sc_process_handleD1Ev.exit39

bb190:                                            ; preds = %bb185
  %tmp191 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp192 = icmp eq %"class.sc_core::sc_process_b"* %tmp191, null
  br i1 %tmp192, label %bb197, label %.noexc38

.noexc38:                                         ; preds = %bb190
  %tmp193 = bitcast %"class.sc_core::sc_process_b"* %tmp191 to void (%"class.sc_core::sc_process_b"*)***
  %tmp194 = load void (%"class.sc_core::sc_process_b"*)*** %tmp193, align 8, !tbaa !3
  %tmp195 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp194, i64 6
  %tmp196 = load void (%"class.sc_core::sc_process_b"*)** %tmp195, align 8
  call void %tmp196(%"class.sc_core::sc_process_b"* %tmp191)
  %.pre.i.i.i37 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb197

bb197:                                            ; preds = %.noexc38, %bb190
  %tmp198 = phi %"class.sc_core::sc_process_b"* [ null, %bb190 ], [ %.pre.i.i.i37, %.noexc38 ]
  %tmp199 = icmp eq %"class.sc_core::sc_process_b"* %tmp198, %tmp183
  br i1 %tmp199, label %bb200, label %bb201

bb200:                                            ; preds = %bb197
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb201:                                            ; preds = %bb197
  store %"class.sc_core::sc_process_b"* %tmp183, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit39

_ZN7sc_core17sc_process_handleD1Ev.exit39:        ; preds = %bb201, %bb185, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36
  %tmp202 = load %"class.sc_core::sc_process_b"** %tmp143, align 8, !tbaa !0
  %tmp203 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp5, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp202, %"class.sc_core::sc_process_b"** %tmp203, align 8, !tbaa !0
  %tmp204 = icmp eq %"class.sc_core::sc_process_b"* %tmp202, null
  br i1 %tmp204, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41, label %bb205

bb205:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit39
  %tmp206 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp202, i64 0, i32 15
  %tmp207 = load i32* %tmp206, align 4, !tbaa !4
  %tmp208 = icmp eq i32 %tmp207, 0
  br i1 %tmp208, label %bb209, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40

bb209:                                            ; preds = %bb205
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str15, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40: ; preds = %bb205
  %tmp210 = add nsw i32 %tmp207, 1
  store i32 %tmp210, i32* %tmp206, align 4, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41

_ZN7sc_core17sc_process_handleC1ERKS0_.exit41:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40, %_ZN7sc_core17sc_process_handleD1Ev.exit39
  %tmp211 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp98, %"class.sc_core::sc_process_handle"* %tmp5)
  %tmp212 = load %"class.sc_core::sc_process_b"** %tmp203, align 8, !tbaa !0
  %tmp213 = icmp eq %"class.sc_core::sc_process_b"* %tmp212, null
  br i1 %tmp213, label %_ZN7sc_core17sc_process_handleD1Ev.exit44, label %bb214

bb214:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41
  %tmp215 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp212, i64 0, i32 15
  %tmp216 = load i32* %tmp215, align 4, !tbaa !4
  %tmp217 = add nsw i32 %tmp216, -1
  store i32 %tmp217, i32* %tmp215, align 4, !tbaa !4
  %tmp218 = icmp eq i32 %tmp217, 0
  br i1 %tmp218, label %bb219, label %_ZN7sc_core17sc_process_handleD1Ev.exit44

bb219:                                            ; preds = %bb214
  %tmp220 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp221 = icmp eq %"class.sc_core::sc_process_b"* %tmp220, null
  br i1 %tmp221, label %bb226, label %.noexc43

.noexc43:                                         ; preds = %bb219
  %tmp222 = bitcast %"class.sc_core::sc_process_b"* %tmp220 to void (%"class.sc_core::sc_process_b"*)***
  %tmp223 = load void (%"class.sc_core::sc_process_b"*)*** %tmp222, align 8, !tbaa !3
  %tmp224 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp223, i64 6
  %tmp225 = load void (%"class.sc_core::sc_process_b"*)** %tmp224, align 8
  call void %tmp225(%"class.sc_core::sc_process_b"* %tmp220)
  %.pre.i.i.i42 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb226

bb226:                                            ; preds = %.noexc43, %bb219
  %tmp227 = phi %"class.sc_core::sc_process_b"* [ null, %bb219 ], [ %.pre.i.i.i42, %.noexc43 ]
  %tmp228 = icmp eq %"class.sc_core::sc_process_b"* %tmp227, %tmp212
  br i1 %tmp228, label %bb229, label %bb230

bb229:                                            ; preds = %bb226
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb230:                                            ; preds = %bb226
  store %"class.sc_core::sc_process_b"* %tmp212, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit44

_ZN7sc_core17sc_process_handleD1Ev.exit44:        ; preds = %bb230, %bb214, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41
  %tmp231 = load %"class.sc_core::sc_process_b"** %tmp143, align 8, !tbaa !0
  %tmp232 = icmp eq %"class.sc_core::sc_process_b"* %tmp231, null
  br i1 %tmp232, label %_ZN7sc_core17sc_process_handleD1Ev.exit47, label %bb233

bb233:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit44
  %tmp234 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp231, i64 0, i32 15
  %tmp235 = load i32* %tmp234, align 4, !tbaa !4
  %tmp236 = add nsw i32 %tmp235, -1
  store i32 %tmp236, i32* %tmp234, align 4, !tbaa !4
  %tmp237 = icmp eq i32 %tmp236, 0
  br i1 %tmp237, label %bb238, label %_ZN7sc_core17sc_process_handleD1Ev.exit47

bb238:                                            ; preds = %bb233
  %tmp239 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %tmp240 = icmp eq %"class.sc_core::sc_process_b"* %tmp239, null
  br i1 %tmp240, label %bb245, label %.noexc46

.noexc46:                                         ; preds = %bb238
  %tmp241 = bitcast %"class.sc_core::sc_process_b"* %tmp239 to void (%"class.sc_core::sc_process_b"*)***
  %tmp242 = load void (%"class.sc_core::sc_process_b"*)*** %tmp241, align 8, !tbaa !3
  %tmp243 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp242, i64 6
  %tmp244 = load void (%"class.sc_core::sc_process_b"*)** %tmp243, align 8
  call void %tmp244(%"class.sc_core::sc_process_b"* %tmp239)
  %.pre.i.i.i45 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %bb245

bb245:                                            ; preds = %.noexc46, %bb238
  %tmp246 = phi %"class.sc_core::sc_process_b"* [ null, %bb238 ], [ %.pre.i.i.i45, %.noexc46 ]
  %tmp247 = icmp eq %"class.sc_core::sc_process_b"* %tmp246, %tmp231
  br i1 %tmp247, label %bb248, label %bb249

bb248:                                            ; preds = %bb245
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str14, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb249:                                            ; preds = %bb245
  store %"class.sc_core::sc_process_b"* %tmp231, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit47

_ZN7sc_core17sc_process_handleD1Ev.exit47:        ; preds = %bb249, %bb233, %_ZN7sc_core17sc_process_handleD1Ev.exit44
  ret void
}

declare void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

define linkonce_odr void @_ZN3top1PEv(%class.top* %this) uwtable align 2 {
bb:
  %tmp = alloca %"class.sc_core::sc_time", align 8
  %tmp1 = getelementptr inbounds %class.top* %this, i64 0, i32 1
  %tmp2 = load i32* %tmp1, align 4, !tbaa !4
  %tmp3 = icmp eq i32 %tmp2, 0
  br i1 %tmp3, label %.thread, label %bb4

bb4:                                              ; preds = %bb
  %tmp5 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  %tmp6 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp7 = load %"class.sc_core::sc_simcontext"** %tmp6, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp5, %"class.sc_core::sc_simcontext"* %tmp7)
  %.pr = load i32* %tmp1, align 4, !tbaa !4
  %tmp8 = icmp eq i32 %.pr, 0
  br i1 %tmp8, label %.thread, label %._crit_edge

._crit_edge:                                      ; preds = %bb4
  %.pre = getelementptr inbounds %class.top* %this, i64 0, i32 2
  br label %bb49

.thread:                                          ; preds = %bb4, %bb
  %tmp9 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  %tmp10 = load i32* %tmp9, align 4, !tbaa !4
  %tmp11 = icmp sgt i32 %tmp10, 0
  br i1 %tmp11, label %bb12, label %bb49

bb12:                                             ; preds = %.thread
  store i32 %tmp10, i32* %tmp1, align 4, !tbaa !4
  %tmp13 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str16, i64 0, i64 0), i64 2)
  %tmp14 = load i32* %tmp1, align 4, !tbaa !4
  %tmp15 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp14)
  %tmp16 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp15, i8* getelementptr inbounds ([2 x i8]* @.str12, i64 0, i64 0), i64 1)
  %tmp17 = load i32* %tmp9, align 4, !tbaa !4
  %tmp18 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp15, i32 %tmp17)
  %tmp19 = bitcast %"class.std::basic_ostream"* %tmp18 to i8**
  %tmp20 = load i8** %tmp19, align 8, !tbaa !3
  %tmp21 = getelementptr i8* %tmp20, i64 -24
  %tmp22 = bitcast i8* %tmp21 to i64*
  %tmp23 = load i64* %tmp22, align 8
  %tmp24 = bitcast %"class.std::basic_ostream"* %tmp18 to i8*
  %.sum.i = add i64 %tmp23, 240
  %tmp25 = getelementptr inbounds i8* %tmp24, i64 %.sum.i
  %tmp26 = bitcast i8* %tmp25 to %"class.std::ctype"**
  %tmp27 = load %"class.std::ctype"** %tmp26, align 8, !tbaa !0
  %tmp28 = icmp eq %"class.std::ctype"* %tmp27, null
  br i1 %tmp28, label %bb29, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i

bb29:                                             ; preds = %bb12
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %bb12
  %tmp30 = getelementptr inbounds %"class.std::ctype"* %tmp27, i64 0, i32 6
  %tmp31 = load i8* %tmp30, align 1, !tbaa !1
  %tmp32 = icmp eq i8 %tmp31, 0
  br i1 %tmp32, label %bb36, label %bb33

bb33:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp34 = getelementptr inbounds %"class.std::ctype"* %tmp27, i64 0, i32 7, i64 10
  %tmp35 = load i8* %tmp34, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb36:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp27)
  %tmp37 = bitcast %"class.std::ctype"* %tmp27 to i8 (%"class.std::ctype"*, i8)***
  %tmp38 = load i8 (%"class.std::ctype"*, i8)*** %tmp37, align 8, !tbaa !3
  %tmp39 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp38, i64 6
  %tmp40 = load i8 (%"class.std::ctype"*, i8)** %tmp39, align 8
  %tmp41 = call signext i8 %tmp40(%"class.std::ctype"* %tmp27, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb36, %bb33
  %.0.i.i.i = phi i8 [ %tmp35, %bb33 ], [ %tmp41, %bb36 ]
  %tmp42 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp18, i8 signext %.0.i.i.i)
  %tmp43 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp42)
  %tmp44 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp44)
  %tmp45 = bitcast %"class.sc_core::sc_time"* %tmp to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp45)
  %tmp46 = getelementptr inbounds %class.top* %this, i64 0, i32 0, i32 0, i32 1
  %tmp47 = load %"class.sc_core::sc_simcontext"** %tmp46, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, double 1.000000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp47)
  %tmp48 = load %"class.sc_core::sc_simcontext"** %tmp46, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp, %"class.sc_core::sc_simcontext"* %tmp48)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp45)
  br label %bb81

bb49:                                             ; preds = %.thread, %._crit_edge
  %.pre-phi = phi i32* [ %.pre, %._crit_edge ], [ %tmp9, %.thread ]
  store i32 -1, i32* %tmp1, align 4, !tbaa !4
  %tmp50 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str16, i64 0, i64 0), i64 2)
  %tmp51 = load i32* %tmp1, align 4, !tbaa !4
  %tmp52 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp51)
  %tmp53 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp52, i8* getelementptr inbounds ([2 x i8]* @.str12, i64 0, i64 0), i64 1)
  %tmp54 = load i32* %.pre-phi, align 4, !tbaa !4
  %tmp55 = call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp52, i32 %tmp54)
  %tmp56 = bitcast %"class.std::basic_ostream"* %tmp55 to i8**
  %tmp57 = load i8** %tmp56, align 8, !tbaa !3
  %tmp58 = getelementptr i8* %tmp57, i64 -24
  %tmp59 = bitcast i8* %tmp58 to i64*
  %tmp60 = load i64* %tmp59, align 8
  %tmp61 = bitcast %"class.std::basic_ostream"* %tmp55 to i8*
  %.sum.i1 = add i64 %tmp60, 240
  %tmp62 = getelementptr inbounds i8* %tmp61, i64 %.sum.i1
  %tmp63 = bitcast i8* %tmp62 to %"class.std::ctype"**
  %tmp64 = load %"class.std::ctype"** %tmp63, align 8, !tbaa !0
  %tmp65 = icmp eq %"class.std::ctype"* %tmp64, null
  br i1 %tmp65, label %bb66, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2

bb66:                                             ; preds = %bb49
  call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2: ; preds = %bb49
  %tmp67 = getelementptr inbounds %"class.std::ctype"* %tmp64, i64 0, i32 6
  %tmp68 = load i8* %tmp67, align 1, !tbaa !1
  %tmp69 = icmp eq i8 %tmp68, 0
  br i1 %tmp69, label %bb73, label %bb70

bb70:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  %tmp71 = getelementptr inbounds %"class.std::ctype"* %tmp64, i64 0, i32 7, i64 10
  %tmp72 = load i8* %tmp71, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

bb73:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp64)
  %tmp74 = bitcast %"class.std::ctype"* %tmp64 to i8 (%"class.std::ctype"*, i8)***
  %tmp75 = load i8 (%"class.std::ctype"*, i8)*** %tmp74, align 8, !tbaa !3
  %tmp76 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp75, i64 6
  %tmp77 = load i8 (%"class.std::ctype"*, i8)** %tmp76, align 8
  %tmp78 = call signext i8 %tmp77(%"class.std::ctype"* %tmp64, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4: ; preds = %bb73, %bb70
  %.0.i.i.i3 = phi i8 [ %tmp72, %bb70 ], [ %tmp78, %bb73 ]
  %tmp79 = call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp55, i8 signext %.0.i.i.i3)
  %tmp80 = call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp79)
  br label %bb81

bb81:                                             ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4, %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit
  ret void
}

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

define linkonce_odr void @_ZN3top1CEv(%class.top* %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.top* %this, i64 0, i32 1
  %tmp1 = load i32* %tmp, align 4, !tbaa !4
  %tmp2 = icmp eq i32 %tmp1, 0
  %tmp3 = add nsw i32 %tmp1, 1
  %tmp4 = getelementptr inbounds %class.top* %this, i64 0, i32 2
  store i32 %tmp3, i32* %tmp4, align 4, !tbaa !4
  store i32 0, i32* %tmp, align 4, !tbaa !4
  br i1 %tmp2, label %bb38, label %bb5

bb5:                                              ; preds = %bb
  %tmp6 = getelementptr inbounds %class.top* %this, i64 0, i32 3
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp6)
  %tmp7 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str11, i64 0, i64 0), i64 2)
  %tmp8 = load i32* %tmp, align 4, !tbaa !4
  %tmp9 = tail call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp8)
  %tmp10 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp9, i8* getelementptr inbounds ([2 x i8]* @.str12, i64 0, i64 0), i64 1)
  %tmp11 = load i32* %tmp4, align 4, !tbaa !4
  %tmp12 = tail call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp9, i32 %tmp11)
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

bb23:                                             ; preds = %bb5
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i: ; preds = %bb5
  %tmp24 = getelementptr inbounds %"class.std::ctype"* %tmp21, i64 0, i32 6
  %tmp25 = load i8* %tmp24, align 1, !tbaa !1
  %tmp26 = icmp eq i8 %tmp25, 0
  br i1 %tmp26, label %bb30, label %bb27

bb27:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  %tmp28 = getelementptr inbounds %"class.std::ctype"* %tmp21, i64 0, i32 7, i64 10
  %tmp29 = load i8* %tmp28, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

bb30:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp21)
  %tmp31 = bitcast %"class.std::ctype"* %tmp21 to i8 (%"class.std::ctype"*, i8)***
  %tmp32 = load i8 (%"class.std::ctype"*, i8)*** %tmp31, align 8, !tbaa !3
  %tmp33 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp32, i64 6
  %tmp34 = load i8 (%"class.std::ctype"*, i8)** %tmp33, align 8
  %tmp35 = tail call signext i8 %tmp34(%"class.std::ctype"* %tmp21, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit: ; preds = %bb30, %bb27
  %.0.i.i.i = phi i8 [ %tmp29, %bb27 ], [ %tmp35, %bb30 ]
  %tmp36 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp12, i8 signext %.0.i.i.i)
  %tmp37 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp36)
  br label %bb70

bb38:                                             ; preds = %bb
  %tmp39 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([3 x i8]* @.str11, i64 0, i64 0), i64 2)
  %tmp40 = load i32* %tmp, align 4, !tbaa !4
  %tmp41 = tail call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp40)
  %tmp42 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp41, i8* getelementptr inbounds ([2 x i8]* @.str12, i64 0, i64 0), i64 1)
  %tmp43 = load i32* %tmp4, align 4, !tbaa !4
  %tmp44 = tail call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* %tmp41, i32 %tmp43)
  %tmp45 = bitcast %"class.std::basic_ostream"* %tmp44 to i8**
  %tmp46 = load i8** %tmp45, align 8, !tbaa !3
  %tmp47 = getelementptr i8* %tmp46, i64 -24
  %tmp48 = bitcast i8* %tmp47 to i64*
  %tmp49 = load i64* %tmp48, align 8
  %tmp50 = bitcast %"class.std::basic_ostream"* %tmp44 to i8*
  %.sum.i1 = add i64 %tmp49, 240
  %tmp51 = getelementptr inbounds i8* %tmp50, i64 %.sum.i1
  %tmp52 = bitcast i8* %tmp51 to %"class.std::ctype"**
  %tmp53 = load %"class.std::ctype"** %tmp52, align 8, !tbaa !0
  %tmp54 = icmp eq %"class.std::ctype"* %tmp53, null
  br i1 %tmp54, label %bb55, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2

bb55:                                             ; preds = %bb38
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2: ; preds = %bb38
  %tmp56 = getelementptr inbounds %"class.std::ctype"* %tmp53, i64 0, i32 6
  %tmp57 = load i8* %tmp56, align 1, !tbaa !1
  %tmp58 = icmp eq i8 %tmp57, 0
  br i1 %tmp58, label %bb62, label %bb59

bb59:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  %tmp60 = getelementptr inbounds %"class.std::ctype"* %tmp53, i64 0, i32 7, i64 10
  %tmp61 = load i8* %tmp60, align 1, !tbaa !1
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

bb62:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit.i.i2
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp53)
  %tmp63 = bitcast %"class.std::ctype"* %tmp53 to i8 (%"class.std::ctype"*, i8)***
  %tmp64 = load i8 (%"class.std::ctype"*, i8)*** %tmp63, align 8, !tbaa !3
  %tmp65 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp64, i64 6
  %tmp66 = load i8 (%"class.std::ctype"*, i8)** %tmp65, align 8
  %tmp67 = tail call signext i8 %tmp66(%"class.std::ctype"* %tmp53, i8 signext 10)
  br label %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4

_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4: ; preds = %bb62, %bb59
  %.0.i.i.i3 = phi i8 [ %tmp61, %bb59 ], [ %tmp67, %bb62 ]
  %tmp68 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* %tmp44, i8 signext %.0.i.i.i3)
  %tmp69 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp68)
  br label %bb70

bb70:                                             ; preds = %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit4, %_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_.exit
  ret void
}

declare void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"*)

declare %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"*, i32)

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

declare void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, double, i32, %"class.sc_core::sc_simcontext"*)

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

!0 = metadata !{metadata !"any pointer", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA"}
!3 = metadata !{metadata !"vtable pointer", metadata !2}
!4 = metadata !{metadata !"int", metadata !1}
!5 = metadata !{metadata !"_ZTSN7sc_core8sc_event8notify_tE", metadata !1}
