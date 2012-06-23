; ModuleID = 'paper.bc'
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
%struct.M1 = type { %"class.sc_core::sc_module", %"class.sc_core::sc_event", i8, i8 }
%"class.sc_core::sc_module" = type { %"class.sc_core::sc_object", %"class.sc_core::sc_process_host", %"class.sc_core::sc_sensitive", %"class.sc_core::sc_sensitive_pos", %"class.sc_core::sc_sensitive_neg", i8, %"class.std::vector"*, i32, %"class.sc_core::sc_name_gen"*, %"class.std::vector.10", %"class.sc_core::sc_module_name"* }
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
@.str = private unnamed_addr constant [3 x i8] c"M1\00", align 1
@_ZTVN10__cxxabiv120__si_class_type_infoE = external global i8*
@_ZTSN7sc_core9sc_objectE = available_externally constant [21 x i8] c"N7sc_core9sc_objectE\00"
@_ZTIN7sc_core9sc_objectE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_objectE, i32 0, i32 0) }
@_ZTV2M1 = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI2M1 to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%struct.M1*)* @_ZN2M1D1Ev to i8*), i8* bitcast (void (%struct.M1*)* @_ZN2M1D0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI2M1 to i8*), i8* bitcast (void (%struct.M1*)* @_ZThn40_N2M1D1Ev to i8*), i8* bitcast (void (%struct.M1*)* @_ZThn40_N2M1D0Ev to i8*)]
@_ZTS2M1 = linkonce_odr constant [4 x i8] c"2M1\00"
@_ZTSN7sc_core9sc_moduleE = available_externally constant [21 x i8] c"N7sc_core9sc_moduleE\00"
@_ZTSN7sc_core15sc_process_hostE = linkonce_odr constant [28 x i8] c"N7sc_core15sc_process_hostE\00"
@_ZTIN7sc_core15sc_process_hostE = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_process_hostE, i32 0, i32 0) }
@_ZTIN7sc_core9sc_moduleE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_moduleE, i32 0, i32 0), i32 0, i32 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*), i64 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core15sc_process_hostE to i8*), i64 10242 }
@_ZTI2M1 = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([4 x i8]* @_ZTS2M1, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@.str8 = private unnamed_addr constant [10 x i8] c"sc_module\00", align 1
@.str9 = private unnamed_addr constant [3 x i8] c"T1\00", align 1
@.str10 = private unnamed_addr constant [3 x i8] c"T2\00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str11 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str12 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str13 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
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

define i32 @sc_main(i32 %argc, i8** nocapture %argv) uwtable {
  %M1 = alloca %struct.M1, align 8
  %1 = alloca %"class.sc_core::sc_module_name", align 8
  %2 = icmp eq i32 %argc, 3
  br i1 %2, label %3, label %40

; <label>:3                                       ; preds = %0
  %4 = getelementptr inbounds i8** %argv, i64 1
  %5 = load i8** %4, align 8, !tbaa !0
  %6 = call i32 @atoi(i8* %5) nounwind readonly
  %7 = getelementptr inbounds i8** %argv, i64 2
  %8 = load i8** %7, align 8, !tbaa !0
  %9 = call i32 @atoi(i8* %8) nounwind readonly
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %1, i8* getelementptr inbounds ([3 x i8]* @.str, i64 0, i64 0))
  %10 = icmp ne i32 %6, 0
  %11 = icmp ne i32 %9, 0
  invoke void @_ZN2M1C2EN7sc_core14sc_module_nameEbb(%struct.M1* %M1, %"class.sc_core::sc_module_name"* undef, i1 zeroext %10, i1 zeroext %11)
          to label %_ZN2M1C1EN7sc_core14sc_module_nameEbb.exit unwind label %23

_ZN2M1C1EN7sc_core14sc_module_nameEbb.exit:       ; preds = %3
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %1)
  invoke void @_ZN7sc_core8sc_startEv()
          to label %12 unwind label %27

; <label>:12                                      ; preds = %_ZN2M1C1EN7sc_core14sc_module_nameEbb.exit
  %13 = getelementptr inbounds %struct.M1* %M1, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 2) to i32 (...)**), i32 (...)*** %13, align 8, !tbaa !3
  %14 = getelementptr %struct.M1* %M1, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 15) to i32 (...)**), i32 (...)*** %14, align 8, !tbaa !3
  %15 = getelementptr inbounds %struct.M1* %M1, i64 0, i32 1
  invoke void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %15)
          to label %_ZN2M1D1Ev.exit unwind label %16

; <label>:16                                      ; preds = %12
  %17 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %18 = getelementptr inbounds %struct.M1* %M1, i64 0, i32 0
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %18)
          to label %19 unwind label %20

; <label>:19                                      ; preds = %16
  resume { i8*, i32 } %17

; <label>:20                                      ; preds = %16
  %21 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  call void @_ZSt9terminatev() noreturn nounwind
  unreachable

_ZN2M1D1Ev.exit:                                  ; preds = %12
  %22 = getelementptr inbounds %struct.M1* %M1, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %22)
  br label %40

; <label>:23                                      ; preds = %3
  %24 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %25 = extractvalue { i8*, i32 } %24, 0
  %26 = extractvalue { i8*, i32 } %24, 1
  invoke void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %1)
          to label %_ZN2M1D1Ev.exit3 unwind label %43

; <label>:27                                      ; preds = %_ZN2M1C1EN7sc_core14sc_module_nameEbb.exit
  %28 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %29 = getelementptr inbounds %struct.M1* %M1, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 2) to i32 (...)**), i32 (...)*** %29, align 8, !tbaa !3
  %30 = getelementptr %struct.M1* %M1, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 15) to i32 (...)**), i32 (...)*** %30, align 8, !tbaa !3
  %31 = getelementptr inbounds %struct.M1* %M1, i64 0, i32 1
  invoke void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %31)
          to label %_ZN2M1D2Ev.exit.i unwind label %32

; <label>:32                                      ; preds = %27
  %33 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  %34 = getelementptr inbounds %struct.M1* %M1, i64 0, i32 0
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %34)
          to label %.body unwind label %35

; <label>:35                                      ; preds = %32
  %36 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  call void @_ZSt9terminatev() noreturn nounwind
  unreachable

_ZN2M1D2Ev.exit.i:                                ; preds = %27
  %37 = extractvalue { i8*, i32 } %28, 1
  %38 = extractvalue { i8*, i32 } %28, 0
  %39 = getelementptr inbounds %struct.M1* %M1, i64 0, i32 0
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %39)
          to label %_ZN2M1D1Ev.exit3 unwind label %43

; <label>:40                                      ; preds = %_ZN2M1D1Ev.exit, %0
  %.0 = phi i32 [ 0, %_ZN2M1D1Ev.exit ], [ -1, %0 ]
  ret i32 %.0

_ZN2M1D1Ev.exit3:                                 ; preds = %_ZN2M1D2Ev.exit.i, %23
  %.02 = phi i8* [ %25, %23 ], [ %38, %_ZN2M1D2Ev.exit.i ]
  %.01 = phi i32 [ %26, %23 ], [ %37, %_ZN2M1D2Ev.exit.i ]
  %41 = insertvalue { i8*, i32 } undef, i8* %.02, 0
  %42 = insertvalue { i8*, i32 } %41, i32 %.01, 1
  resume { i8*, i32 } %42

; <label>:43                                      ; preds = %_ZN2M1D2Ev.exit.i, %23
  %44 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  br label %.body

.body:                                            ; preds = %43, %32
  call void @_ZSt9terminatev() noreturn nounwind
  unreachable
}

declare i32 @atoi(i8* nocapture) nounwind readonly

declare void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"*, i8*)

declare void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core8sc_startEv()

define linkonce_odr void @_ZN2M1D1Ev(%struct.M1* %this) unnamed_addr uwtable inlinehint align 2 {
  %1 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 2) to i32 (...)**), i32 (...)*** %1, align 8, !tbaa !3
  %2 = getelementptr %struct.M1* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 15) to i32 (...)**), i32 (...)*** %2, align 8, !tbaa !3
  %3 = getelementptr inbounds %struct.M1* %this, i64 0, i32 1
  invoke void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %3)
          to label %_ZN2M1D2Ev.exit unwind label %4

; <label>:4                                       ; preds = %0
  %5 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %6 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %6)
          to label %7 unwind label %8

; <label>:7                                       ; preds = %4
  resume { i8*, i32 } %5

; <label>:8                                       ; preds = %4
  %9 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  tail call void @_ZSt9terminatev() noreturn nounwind
  unreachable

_ZN2M1D2Ev.exit:                                  ; preds = %0
  %10 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %10)
  ret void
}

declare void @_ZdlPv(i8*) nounwind

define linkonce_odr void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %this) unnamed_addr uwtable inlinehint align 2 {
  invoke void @_ZN7sc_core8sc_event6cancelEv(%"class.sc_core::sc_event"* %this)
          to label %1 unwind label %22

; <label>:1                                       ; preds = %0
  %2 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 7, i32 0, i32 0, i32 0
  %3 = load %"class.sc_core::sc_thread_process"*** %2, align 8, !tbaa !0
  %4 = icmp eq %"class.sc_core::sc_thread_process"** %3, null
  br i1 %4, label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit, label %5

; <label>:5                                       ; preds = %1
  %6 = bitcast %"class.sc_core::sc_thread_process"** %3 to i8*
  tail call void @_ZdlPv(i8* %6) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit

_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit: ; preds = %5, %1
  %7 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 6, i32 0, i32 0, i32 0
  %8 = load %"class.sc_core::sc_thread_process"*** %7, align 8, !tbaa !0
  %9 = icmp eq %"class.sc_core::sc_thread_process"** %8, null
  br i1 %9, label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4, label %10

; <label>:10                                      ; preds = %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit
  %11 = bitcast %"class.sc_core::sc_thread_process"** %8 to i8*
  tail call void @_ZdlPv(i8* %11) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4

_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4: ; preds = %10, %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit
  %12 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 5, i32 0, i32 0, i32 0
  %13 = load %"class.sc_core::sc_method_process"*** %12, align 8, !tbaa !0
  %14 = icmp eq %"class.sc_core::sc_method_process"** %13, null
  br i1 %14, label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit, label %15

; <label>:15                                      ; preds = %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4
  %16 = bitcast %"class.sc_core::sc_method_process"** %13 to i8*
  tail call void @_ZdlPv(i8* %16) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit

_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit: ; preds = %15, %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit4
  %17 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 4, i32 0, i32 0, i32 0
  %18 = load %"class.sc_core::sc_method_process"*** %17, align 8, !tbaa !0
  %19 = icmp eq %"class.sc_core::sc_method_process"** %18, null
  br i1 %19, label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit5, label %20

; <label>:20                                      ; preds = %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit
  %21 = bitcast %"class.sc_core::sc_method_process"** %18 to i8*
  tail call void @_ZdlPv(i8* %21) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit5

_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit5: ; preds = %20, %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit
  ret void

; <label>:22                                      ; preds = %0
  %23 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %24 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 7, i32 0, i32 0, i32 0
  %25 = load %"class.sc_core::sc_thread_process"*** %24, align 8, !tbaa !0
  %26 = icmp eq %"class.sc_core::sc_thread_process"** %25, null
  br i1 %26, label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit6, label %27

; <label>:27                                      ; preds = %22
  %28 = bitcast %"class.sc_core::sc_thread_process"** %25 to i8*
  tail call void @_ZdlPv(i8* %28) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit6

_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit6: ; preds = %27, %22
  %29 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 6, i32 0, i32 0, i32 0
  %30 = load %"class.sc_core::sc_thread_process"*** %29, align 8, !tbaa !0
  %31 = icmp eq %"class.sc_core::sc_thread_process"** %30, null
  br i1 %31, label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit7, label %32

; <label>:32                                      ; preds = %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit6
  %33 = bitcast %"class.sc_core::sc_thread_process"** %30 to i8*
  tail call void @_ZdlPv(i8* %33) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit7

_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit7: ; preds = %32, %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit6
  %34 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 5, i32 0, i32 0, i32 0
  %35 = load %"class.sc_core::sc_method_process"*** %34, align 8, !tbaa !0
  %36 = icmp eq %"class.sc_core::sc_method_process"** %35, null
  br i1 %36, label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit8, label %37

; <label>:37                                      ; preds = %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit7
  %38 = bitcast %"class.sc_core::sc_method_process"** %35 to i8*
  tail call void @_ZdlPv(i8* %38) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit8

_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit8: ; preds = %37, %_ZNSt6vectorIPN7sc_core17sc_thread_processESaIS2_EED1Ev.exit7
  %39 = getelementptr inbounds %"class.sc_core::sc_event"* %this, i64 0, i32 4, i32 0, i32 0, i32 0
  %40 = load %"class.sc_core::sc_method_process"*** %39, align 8, !tbaa !0
  %41 = icmp eq %"class.sc_core::sc_method_process"** %40, null
  br i1 %41, label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit9, label %42

; <label>:42                                      ; preds = %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit8
  %43 = bitcast %"class.sc_core::sc_method_process"** %40 to i8*
  tail call void @_ZdlPv(i8* %43) nounwind
  br label %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit9

_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit9: ; preds = %42, %_ZNSt6vectorIPN7sc_core17sc_method_processESaIS2_EED1Ev.exit8
  resume { i8*, i32 } %23
}

declare void @_ZN7sc_core8sc_event6cancelEv(%"class.sc_core::sc_event"*)

declare noalias i8* @_Znwm(i64)

define linkonce_odr void @_ZThn40_N2M1D1Ev(%struct.M1* %this) {
  %1 = getelementptr inbounds %struct.M1* %this, i64 -1, i32 1, i32 6, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %1, align 8, !tbaa !3
  %2 = getelementptr %"class.sc_core::sc_thread_process"*** %1, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %2, align 8, !tbaa !3
  %3 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %1, i64 23
  %4 = bitcast %"class.sc_core::sc_thread_process"*** %3 to %"class.sc_core::sc_event"*
  invoke void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %4)
          to label %_ZN2M1D1Ev.exit unwind label %5

; <label>:5                                       ; preds = %0
  %6 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %7 = bitcast %"class.sc_core::sc_thread_process"*** %1 to %"class.sc_core::sc_module"*
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %7)
          to label %8 unwind label %9

; <label>:8                                       ; preds = %5
  resume { i8*, i32 } %6

; <label>:9                                       ; preds = %5
  %10 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  tail call void @_ZSt9terminatev() noreturn nounwind
  unreachable

_ZN2M1D1Ev.exit:                                  ; preds = %0
  %11 = bitcast %"class.sc_core::sc_thread_process"*** %1 to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %11)
  ret void
}

declare void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"*)

declare void @_ZNK7sc_core9sc_object5printERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object4dumpERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)

define linkonce_odr i8* @_ZNK7sc_core9sc_module4kindEv(%"class.sc_core::sc_module"* nocapture %this) nounwind uwtable readnone align 2 {
  ret i8* getelementptr inbounds ([10 x i8]* @.str8, i64 0, i64 0)
}

declare %"class.std::vector.10"* @_ZNK7sc_core9sc_module17get_child_objectsEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN2M1D0Ev(%struct.M1* %this) unnamed_addr uwtable inlinehint align 2 {
  %1 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 2) to i32 (...)**), i32 (...)*** %1, align 8, !tbaa !3
  %2 = getelementptr %struct.M1* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 15) to i32 (...)**), i32 (...)*** %2, align 8, !tbaa !3
  %3 = getelementptr inbounds %struct.M1* %this, i64 0, i32 1
  invoke void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %3)
          to label %_ZN2M1D2Ev.exit.i unwind label %4

; <label>:4                                       ; preds = %0
  %5 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %6 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %6)
          to label %.body unwind label %7

; <label>:7                                       ; preds = %4
  %8 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  tail call void @_ZSt9terminatev() noreturn nounwind
  unreachable

_ZN2M1D2Ev.exit.i:                                ; preds = %0
  %9 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %9)
          to label %_ZN2M1D1Ev.exit unwind label %11

_ZN2M1D1Ev.exit:                                  ; preds = %_ZN2M1D2Ev.exit.i
  %10 = bitcast %struct.M1* %this to i8*
  tail call void @_ZdlPv(i8* %10) nounwind
  ret void

; <label>:11                                      ; preds = %_ZN2M1D2Ev.exit.i
  %12 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  br label %.body

.body:                                            ; preds = %11, %4
  %eh.lpad-body = phi { i8*, i32 } [ %12, %11 ], [ %5, %4 ]
  %13 = bitcast %struct.M1* %this to i8*
  tail call void @_ZdlPv(i8* %13) nounwind
  resume { i8*, i32 } %eh.lpad-body
}

declare void @_ZN7sc_core9sc_module25before_end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module18end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module19start_of_simulationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module17end_of_simulationEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZThn40_N2M1D0Ev(%struct.M1* %this) {
  %1 = getelementptr inbounds %struct.M1* %this, i64 -1, i32 1, i32 6, i32 0, i32 0, i32 2
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %1, align 8, !tbaa !3
  %2 = getelementptr %"class.sc_core::sc_thread_process"*** %1, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %2, align 8, !tbaa !3
  %3 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %1, i64 23
  %4 = bitcast %"class.sc_core::sc_thread_process"*** %3 to %"class.sc_core::sc_event"*
  invoke void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %4)
          to label %_ZN2M1D2Ev.exit.i.i unwind label %5

; <label>:5                                       ; preds = %0
  %6 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %7 = bitcast %"class.sc_core::sc_thread_process"*** %1 to %"class.sc_core::sc_module"*
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %7)
          to label %.body.i unwind label %8

; <label>:8                                       ; preds = %5
  %9 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  tail call void @_ZSt9terminatev() noreturn nounwind
  unreachable

_ZN2M1D2Ev.exit.i.i:                              ; preds = %0
  %10 = bitcast %"class.sc_core::sc_thread_process"*** %1 to %"class.sc_core::sc_module"*
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %10)
          to label %_ZN2M1D0Ev.exit unwind label %11

; <label>:11                                      ; preds = %_ZN2M1D2Ev.exit.i.i
  %12 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  br label %.body.i

.body.i:                                          ; preds = %11, %5
  %eh.lpad-body.i = phi { i8*, i32 } [ %12, %11 ], [ %6, %5 ]
  %13 = bitcast %"class.sc_core::sc_thread_process"*** %1 to i8*
  tail call void @_ZdlPv(i8* %13) nounwind
  resume { i8*, i32 } %eh.lpad-body.i

_ZN2M1D0Ev.exit:                                  ; preds = %_ZN2M1D2Ev.exit.i.i
  %14 = bitcast %"class.sc_core::sc_thread_process"*** %1 to i8*
  tail call void @_ZdlPv(i8* %14) nounwind
  ret void
}

define linkonce_odr void @_ZN2M1C2EN7sc_core14sc_module_nameEbb(%struct.M1* %this, %"class.sc_core::sc_module_name"* nocapture %name, i1 zeroext %x, i1 zeroext %y) unnamed_addr uwtable align 2 {
  %T1_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %1 = alloca %"class.sc_core::sc_process_handle", align 8
  %2 = alloca %"class.sc_core::sc_process_handle", align 8
  %3 = alloca %"class.sc_core::sc_process_handle", align 8
  %T2_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %4 = alloca %"class.sc_core::sc_process_handle", align 8
  %5 = alloca %"class.sc_core::sc_process_handle", align 8
  %6 = alloca %"class.sc_core::sc_process_handle", align 8
  %7 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2Ev(%"class.sc_core::sc_module"* %7)
  %8 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 2) to i32 (...)**), i32 (...)*** %8, align 8, !tbaa !3
  %9 = getelementptr %struct.M1* %this, i64 0, i32 0, i32 1
  %10 = getelementptr inbounds %"class.sc_core::sc_process_host"* %9, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV2M1, i64 0, i64 15) to i32 (...)**), i32 (...)*** %10, align 8, !tbaa !3
  %11 = getelementptr inbounds %struct.M1* %this, i64 0, i32 1
  %12 = getelementptr inbounds %"class.sc_core::sc_event"* %11, i64 0, i32 0
  %13 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %14 = icmp eq %"class.sc_core::sc_simcontext"* %13, null
  br i1 %14, label %15, label %21

; <label>:15                                      ; preds = %0
  %16 = invoke noalias i8* @_Znwm(i64 248)
          to label %.noexc unwind label %282

.noexc:                                           ; preds = %15
  %17 = bitcast i8* %16 to %"class.sc_core::sc_simcontext"*
  invoke void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %17)
          to label %18 unwind label %19

; <label>:18                                      ; preds = %.noexc
  store %"class.sc_core::sc_simcontext"* %17, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %17, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %21

; <label>:19                                      ; preds = %.noexc
  %20 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  call void @_ZdlPv(i8* %16) nounwind
  br label %.body

; <label>:21                                      ; preds = %18, %0
  %22 = phi %"class.sc_core::sc_simcontext"* [ %17, %18 ], [ %13, %0 ]
  store %"class.sc_core::sc_simcontext"* %22, %"class.sc_core::sc_simcontext"** %12, align 8, !tbaa !0
  %23 = getelementptr inbounds %struct.M1* %this, i64 0, i32 1, i32 1
  store i32 0, i32* %23, align 4, !tbaa !4
  %24 = getelementptr inbounds %struct.M1* %this, i64 0, i32 1, i32 2
  store i32 -1, i32* %24, align 4, !tbaa !5
  %25 = getelementptr inbounds %struct.M1* %this, i64 0, i32 1, i32 3
  %26 = bitcast %"class.sc_core::sc_event_timed"** %25 to i8*
  call void @llvm.memset.p0i8.i64(i8* %26, i8 0, i64 104, i32 8, i1 false)
  %27 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %28 = icmp eq %"class.sc_core::sc_simcontext"* %27, null
  br i1 %28, label %29, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

; <label>:29                                      ; preds = %21
  %30 = invoke noalias i8* @_Znwm(i64 248)
          to label %.noexc9 unwind label %286

.noexc9:                                          ; preds = %29
  %31 = bitcast i8* %30 to %"class.sc_core::sc_simcontext"*
  invoke void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %31)
          to label %32 unwind label %33

; <label>:32                                      ; preds = %.noexc9
  store %"class.sc_core::sc_simcontext"* %31, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %31, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

; <label>:33                                      ; preds = %.noexc9
  %34 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  call void @_ZdlPv(i8* %30) nounwind
  br label %.body10

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %32, %21
  %35 = phi %"class.sc_core::sc_simcontext"* [ %31, %32 ], [ %27, %21 ]
  invoke void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %T1_handle, %"class.sc_core::sc_simcontext"* %35, i8* getelementptr inbounds ([3 x i8]* @.str9, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%struct.M1*)* @_ZN2M12T1Ev to i64), i64 -40, %"class.sc_core::sc_process_host"* %9, %"class.sc_core::sc_spawn_options"* null)
          to label %36 unwind label %286

; <label>:36                                      ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %37 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 2
  %38 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %T1_handle, i64 0, i32 0
  %39 = load %"class.sc_core::sc_process_b"** %38, align 8, !tbaa !0
  %40 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %39, %"class.sc_core::sc_process_b"** %40, align 8, !tbaa !0
  %41 = icmp eq %"class.sc_core::sc_process_b"* %39, null
  br i1 %41, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %42

; <label>:42                                      ; preds = %36
  %43 = getelementptr inbounds %"class.sc_core::sc_process_b"* %39, i64 0, i32 15
  %44 = load i32* %43, align 4, !tbaa !5
  %45 = icmp eq i32 %44, 0
  br i1 %45, label %46, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

; <label>:46                                      ; preds = %42
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %42
  %47 = add nsw i32 %44, 1
  store i32 %47, i32* %43, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %36
  %48 = invoke %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %37, %"class.sc_core::sc_process_handle"* %1)
          to label %49 unwind label %294

; <label>:49                                      ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %50 = load %"class.sc_core::sc_process_b"** %40, align 8, !tbaa !0
  %51 = icmp eq %"class.sc_core::sc_process_b"* %50, null
  br i1 %51, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %52

; <label>:52                                      ; preds = %49
  %53 = getelementptr inbounds %"class.sc_core::sc_process_b"* %50, i64 0, i32 15
  %54 = load i32* %53, align 4, !tbaa !5
  %55 = add nsw i32 %54, -1
  store i32 %55, i32* %53, align 4, !tbaa !5
  %56 = icmp eq i32 %55, 0
  br i1 %56, label %57, label %_ZN7sc_core17sc_process_handleD1Ev.exit

; <label>:57                                      ; preds = %52
  %58 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %59 = icmp eq %"class.sc_core::sc_process_b"* %58, null
  br i1 %59, label %65, label %60

; <label>:60                                      ; preds = %57
  %61 = bitcast %"class.sc_core::sc_process_b"* %58 to void (%"class.sc_core::sc_process_b"*)***
  %62 = load void (%"class.sc_core::sc_process_b"*)*** %61, align 8, !tbaa !3
  %63 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %62, i64 6
  %64 = load void (%"class.sc_core::sc_process_b"*)** %63, align 8
  invoke void %64(%"class.sc_core::sc_process_b"* %58)
          to label %.noexc12 unwind label %290

.noexc12:                                         ; preds = %60
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %65

; <label>:65                                      ; preds = %.noexc12, %57
  %66 = phi %"class.sc_core::sc_process_b"* [ null, %57 ], [ %.pre.i.i.i, %.noexc12 ]
  %67 = icmp eq %"class.sc_core::sc_process_b"* %66, %50
  br i1 %67, label %68, label %69

; <label>:68                                      ; preds = %65
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:69                                      ; preds = %65
  store %"class.sc_core::sc_process_b"* %50, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %69, %52, %49
  %70 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 3
  %71 = load %"class.sc_core::sc_process_b"** %38, align 8, !tbaa !0
  %72 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %71, %"class.sc_core::sc_process_b"** %72, align 8, !tbaa !0
  %73 = icmp eq %"class.sc_core::sc_process_b"* %71, null
  br i1 %73, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14, label %74

; <label>:74                                      ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %75 = getelementptr inbounds %"class.sc_core::sc_process_b"* %71, i64 0, i32 15
  %76 = load i32* %75, align 4, !tbaa !5
  %77 = icmp eq i32 %76, 0
  br i1 %77, label %78, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13

; <label>:78                                      ; preds = %74
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13: ; preds = %74
  %79 = add nsw i32 %76, 1
  store i32 %79, i32* %75, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14

_ZN7sc_core17sc_process_handleC1ERKS0_.exit14:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %80 = invoke %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %70, %"class.sc_core::sc_process_handle"* %2)
          to label %81 unwind label %318

; <label>:81                                      ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14
  %82 = load %"class.sc_core::sc_process_b"** %72, align 8, !tbaa !0
  %83 = icmp eq %"class.sc_core::sc_process_b"* %82, null
  br i1 %83, label %_ZN7sc_core17sc_process_handleD1Ev.exit17, label %84

; <label>:84                                      ; preds = %81
  %85 = getelementptr inbounds %"class.sc_core::sc_process_b"* %82, i64 0, i32 15
  %86 = load i32* %85, align 4, !tbaa !5
  %87 = add nsw i32 %86, -1
  store i32 %87, i32* %85, align 4, !tbaa !5
  %88 = icmp eq i32 %87, 0
  br i1 %88, label %89, label %_ZN7sc_core17sc_process_handleD1Ev.exit17

; <label>:89                                      ; preds = %84
  %90 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %91 = icmp eq %"class.sc_core::sc_process_b"* %90, null
  br i1 %91, label %97, label %92

; <label>:92                                      ; preds = %89
  %93 = bitcast %"class.sc_core::sc_process_b"* %90 to void (%"class.sc_core::sc_process_b"*)***
  %94 = load void (%"class.sc_core::sc_process_b"*)*** %93, align 8, !tbaa !3
  %95 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %94, i64 6
  %96 = load void (%"class.sc_core::sc_process_b"*)** %95, align 8
  invoke void %96(%"class.sc_core::sc_process_b"* %90)
          to label %.noexc16 unwind label %290

.noexc16:                                         ; preds = %92
  %.pre.i.i.i15 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %97

; <label>:97                                      ; preds = %.noexc16, %89
  %98 = phi %"class.sc_core::sc_process_b"* [ null, %89 ], [ %.pre.i.i.i15, %.noexc16 ]
  %99 = icmp eq %"class.sc_core::sc_process_b"* %98, %82
  br i1 %99, label %100, label %101

; <label>:100                                     ; preds = %97
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:101                                     ; preds = %97
  store %"class.sc_core::sc_process_b"* %82, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit17

_ZN7sc_core17sc_process_handleD1Ev.exit17:        ; preds = %101, %84, %81
  %102 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 4
  %103 = load %"class.sc_core::sc_process_b"** %38, align 8, !tbaa !0
  %104 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %3, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %103, %"class.sc_core::sc_process_b"** %104, align 8, !tbaa !0
  %105 = icmp eq %"class.sc_core::sc_process_b"* %103, null
  br i1 %105, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19, label %106

; <label>:106                                     ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit17
  %107 = getelementptr inbounds %"class.sc_core::sc_process_b"* %103, i64 0, i32 15
  %108 = load i32* %107, align 4, !tbaa !5
  %109 = icmp eq i32 %108, 0
  br i1 %109, label %110, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18

; <label>:110                                     ; preds = %106
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18: ; preds = %106
  %111 = add nsw i32 %108, 1
  store i32 %111, i32* %107, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19

_ZN7sc_core17sc_process_handleC1ERKS0_.exit19:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18, %_ZN7sc_core17sc_process_handleD1Ev.exit17
  %112 = invoke %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %102, %"class.sc_core::sc_process_handle"* %3)
          to label %113 unwind label %342

; <label>:113                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19
  %114 = load %"class.sc_core::sc_process_b"** %104, align 8, !tbaa !0
  %115 = icmp eq %"class.sc_core::sc_process_b"* %114, null
  br i1 %115, label %_ZN7sc_core17sc_process_handleD1Ev.exit22, label %116

; <label>:116                                     ; preds = %113
  %117 = getelementptr inbounds %"class.sc_core::sc_process_b"* %114, i64 0, i32 15
  %118 = load i32* %117, align 4, !tbaa !5
  %119 = add nsw i32 %118, -1
  store i32 %119, i32* %117, align 4, !tbaa !5
  %120 = icmp eq i32 %119, 0
  br i1 %120, label %121, label %_ZN7sc_core17sc_process_handleD1Ev.exit22

; <label>:121                                     ; preds = %116
  %122 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %123 = icmp eq %"class.sc_core::sc_process_b"* %122, null
  br i1 %123, label %129, label %124

; <label>:124                                     ; preds = %121
  %125 = bitcast %"class.sc_core::sc_process_b"* %122 to void (%"class.sc_core::sc_process_b"*)***
  %126 = load void (%"class.sc_core::sc_process_b"*)*** %125, align 8, !tbaa !3
  %127 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %126, i64 6
  %128 = load void (%"class.sc_core::sc_process_b"*)** %127, align 8
  invoke void %128(%"class.sc_core::sc_process_b"* %122)
          to label %.noexc21 unwind label %290

.noexc21:                                         ; preds = %124
  %.pre.i.i.i20 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %129

; <label>:129                                     ; preds = %.noexc21, %121
  %130 = phi %"class.sc_core::sc_process_b"* [ null, %121 ], [ %.pre.i.i.i20, %.noexc21 ]
  %131 = icmp eq %"class.sc_core::sc_process_b"* %130, %114
  br i1 %131, label %132, label %133

; <label>:132                                     ; preds = %129
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:133                                     ; preds = %129
  store %"class.sc_core::sc_process_b"* %114, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit22

_ZN7sc_core17sc_process_handleD1Ev.exit22:        ; preds = %133, %116, %113
  %134 = load %"class.sc_core::sc_process_b"** %38, align 8, !tbaa !0
  %135 = icmp eq %"class.sc_core::sc_process_b"* %134, null
  br i1 %135, label %_ZN7sc_core17sc_process_handleD1Ev.exit25, label %136

; <label>:136                                     ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit22
  %137 = getelementptr inbounds %"class.sc_core::sc_process_b"* %134, i64 0, i32 15
  %138 = load i32* %137, align 4, !tbaa !5
  %139 = add nsw i32 %138, -1
  store i32 %139, i32* %137, align 4, !tbaa !5
  %140 = icmp eq i32 %139, 0
  br i1 %140, label %141, label %_ZN7sc_core17sc_process_handleD1Ev.exit25

; <label>:141                                     ; preds = %136
  %142 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %143 = icmp eq %"class.sc_core::sc_process_b"* %142, null
  br i1 %143, label %149, label %144

; <label>:144                                     ; preds = %141
  %145 = bitcast %"class.sc_core::sc_process_b"* %142 to void (%"class.sc_core::sc_process_b"*)***
  %146 = load void (%"class.sc_core::sc_process_b"*)*** %145, align 8, !tbaa !3
  %147 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %146, i64 6
  %148 = load void (%"class.sc_core::sc_process_b"*)** %147, align 8
  invoke void %148(%"class.sc_core::sc_process_b"* %142)
          to label %.noexc24 unwind label %286

.noexc24:                                         ; preds = %144
  %.pre.i.i.i23 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %149

; <label>:149                                     ; preds = %.noexc24, %141
  %150 = phi %"class.sc_core::sc_process_b"* [ null, %141 ], [ %.pre.i.i.i23, %.noexc24 ]
  %151 = icmp eq %"class.sc_core::sc_process_b"* %150, %134
  br i1 %151, label %152, label %153

; <label>:152                                     ; preds = %149
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:153                                     ; preds = %149
  store %"class.sc_core::sc_process_b"* %134, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit25

_ZN7sc_core17sc_process_handleD1Ev.exit25:        ; preds = %153, %136, %_ZN7sc_core17sc_process_handleD1Ev.exit22
  %154 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  %155 = icmp eq %"class.sc_core::sc_simcontext"* %154, null
  br i1 %155, label %156, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit29

; <label>:156                                     ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit25
  %157 = invoke noalias i8* @_Znwm(i64 248)
          to label %.noexc26 unwind label %286

.noexc26:                                         ; preds = %156
  %158 = bitcast i8* %157 to %"class.sc_core::sc_simcontext"*
  invoke void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %158)
          to label %159 unwind label %160

; <label>:159                                     ; preds = %.noexc26
  store %"class.sc_core::sc_simcontext"* %158, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !0
  store %"class.sc_core::sc_simcontext"* %158, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !0
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit29

; <label>:160                                     ; preds = %.noexc26
  %161 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  call void @_ZdlPv(i8* %157) nounwind
  br label %.body10

_ZN7sc_core22sc_get_curr_simcontextEv.exit29:     ; preds = %159, %_ZN7sc_core17sc_process_handleD1Ev.exit25
  %162 = phi %"class.sc_core::sc_simcontext"* [ %158, %159 ], [ %154, %_ZN7sc_core17sc_process_handleD1Ev.exit25 ]
  invoke void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %T2_handle, %"class.sc_core::sc_simcontext"* %162, i8* getelementptr inbounds ([3 x i8]* @.str10, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%struct.M1*)* @_ZN2M12T2Ev to i64), i64 -40, %"class.sc_core::sc_process_host"* %9, %"class.sc_core::sc_spawn_options"* null)
          to label %163 unwind label %286

; <label>:163                                     ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit29
  %164 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %T2_handle, i64 0, i32 0
  %165 = load %"class.sc_core::sc_process_b"** %164, align 8, !tbaa !0
  %166 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %4, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %165, %"class.sc_core::sc_process_b"** %166, align 8, !tbaa !0
  %167 = icmp eq %"class.sc_core::sc_process_b"* %165, null
  br i1 %167, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31, label %168

; <label>:168                                     ; preds = %163
  %169 = getelementptr inbounds %"class.sc_core::sc_process_b"* %165, i64 0, i32 15
  %170 = load i32* %169, align 4, !tbaa !5
  %171 = icmp eq i32 %170, 0
  br i1 %171, label %172, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30

; <label>:172                                     ; preds = %168
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30: ; preds = %168
  %173 = add nsw i32 %170, 1
  store i32 %173, i32* %169, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31

_ZN7sc_core17sc_process_handleC1ERKS0_.exit31:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30, %163
  %174 = invoke %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %37, %"class.sc_core::sc_process_handle"* %4)
          to label %175 unwind label %390

; <label>:175                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31
  %176 = load %"class.sc_core::sc_process_b"** %166, align 8, !tbaa !0
  %177 = icmp eq %"class.sc_core::sc_process_b"* %176, null
  br i1 %177, label %_ZN7sc_core17sc_process_handleD1Ev.exit34, label %178

; <label>:178                                     ; preds = %175
  %179 = getelementptr inbounds %"class.sc_core::sc_process_b"* %176, i64 0, i32 15
  %180 = load i32* %179, align 4, !tbaa !5
  %181 = add nsw i32 %180, -1
  store i32 %181, i32* %179, align 4, !tbaa !5
  %182 = icmp eq i32 %181, 0
  br i1 %182, label %183, label %_ZN7sc_core17sc_process_handleD1Ev.exit34

; <label>:183                                     ; preds = %178
  %184 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %185 = icmp eq %"class.sc_core::sc_process_b"* %184, null
  br i1 %185, label %191, label %186

; <label>:186                                     ; preds = %183
  %187 = bitcast %"class.sc_core::sc_process_b"* %184 to void (%"class.sc_core::sc_process_b"*)***
  %188 = load void (%"class.sc_core::sc_process_b"*)*** %187, align 8, !tbaa !3
  %189 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %188, i64 6
  %190 = load void (%"class.sc_core::sc_process_b"*)** %189, align 8
  invoke void %190(%"class.sc_core::sc_process_b"* %184)
          to label %.noexc33 unwind label %386

.noexc33:                                         ; preds = %186
  %.pre.i.i.i32 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %191

; <label>:191                                     ; preds = %.noexc33, %183
  %192 = phi %"class.sc_core::sc_process_b"* [ null, %183 ], [ %.pre.i.i.i32, %.noexc33 ]
  %193 = icmp eq %"class.sc_core::sc_process_b"* %192, %176
  br i1 %193, label %194, label %195

; <label>:194                                     ; preds = %191
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:195                                     ; preds = %191
  store %"class.sc_core::sc_process_b"* %176, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit34

_ZN7sc_core17sc_process_handleD1Ev.exit34:        ; preds = %195, %178, %175
  %196 = load %"class.sc_core::sc_process_b"** %164, align 8, !tbaa !0
  %197 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %5, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %196, %"class.sc_core::sc_process_b"** %197, align 8, !tbaa !0
  %198 = icmp eq %"class.sc_core::sc_process_b"* %196, null
  br i1 %198, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36, label %199

; <label>:199                                     ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit34
  %200 = getelementptr inbounds %"class.sc_core::sc_process_b"* %196, i64 0, i32 15
  %201 = load i32* %200, align 4, !tbaa !5
  %202 = icmp eq i32 %201, 0
  br i1 %202, label %203, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35

; <label>:203                                     ; preds = %199
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35: ; preds = %199
  %204 = add nsw i32 %201, 1
  store i32 %204, i32* %200, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36

_ZN7sc_core17sc_process_handleC1ERKS0_.exit36:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35, %_ZN7sc_core17sc_process_handleD1Ev.exit34
  %205 = invoke %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %70, %"class.sc_core::sc_process_handle"* %5)
          to label %206 unwind label %414

; <label>:206                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36
  %207 = load %"class.sc_core::sc_process_b"** %197, align 8, !tbaa !0
  %208 = icmp eq %"class.sc_core::sc_process_b"* %207, null
  br i1 %208, label %_ZN7sc_core17sc_process_handleD1Ev.exit39, label %209

; <label>:209                                     ; preds = %206
  %210 = getelementptr inbounds %"class.sc_core::sc_process_b"* %207, i64 0, i32 15
  %211 = load i32* %210, align 4, !tbaa !5
  %212 = add nsw i32 %211, -1
  store i32 %212, i32* %210, align 4, !tbaa !5
  %213 = icmp eq i32 %212, 0
  br i1 %213, label %214, label %_ZN7sc_core17sc_process_handleD1Ev.exit39

; <label>:214                                     ; preds = %209
  %215 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %216 = icmp eq %"class.sc_core::sc_process_b"* %215, null
  br i1 %216, label %222, label %217

; <label>:217                                     ; preds = %214
  %218 = bitcast %"class.sc_core::sc_process_b"* %215 to void (%"class.sc_core::sc_process_b"*)***
  %219 = load void (%"class.sc_core::sc_process_b"*)*** %218, align 8, !tbaa !3
  %220 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %219, i64 6
  %221 = load void (%"class.sc_core::sc_process_b"*)** %220, align 8
  invoke void %221(%"class.sc_core::sc_process_b"* %215)
          to label %.noexc38 unwind label %386

.noexc38:                                         ; preds = %217
  %.pre.i.i.i37 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %222

; <label>:222                                     ; preds = %.noexc38, %214
  %223 = phi %"class.sc_core::sc_process_b"* [ null, %214 ], [ %.pre.i.i.i37, %.noexc38 ]
  %224 = icmp eq %"class.sc_core::sc_process_b"* %223, %207
  br i1 %224, label %225, label %226

; <label>:225                                     ; preds = %222
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:226                                     ; preds = %222
  store %"class.sc_core::sc_process_b"* %207, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit39

_ZN7sc_core17sc_process_handleD1Ev.exit39:        ; preds = %226, %209, %206
  %227 = load %"class.sc_core::sc_process_b"** %164, align 8, !tbaa !0
  %228 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %6, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %227, %"class.sc_core::sc_process_b"** %228, align 8, !tbaa !0
  %229 = icmp eq %"class.sc_core::sc_process_b"* %227, null
  br i1 %229, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41, label %230

; <label>:230                                     ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit39
  %231 = getelementptr inbounds %"class.sc_core::sc_process_b"* %227, i64 0, i32 15
  %232 = load i32* %231, align 4, !tbaa !5
  %233 = icmp eq i32 %232, 0
  br i1 %233, label %234, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40

; <label>:234                                     ; preds = %230
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str13, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40: ; preds = %230
  %235 = add nsw i32 %232, 1
  store i32 %235, i32* %231, align 4, !tbaa !5
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41

_ZN7sc_core17sc_process_handleC1ERKS0_.exit41:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40, %_ZN7sc_core17sc_process_handleD1Ev.exit39
  %236 = invoke %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %102, %"class.sc_core::sc_process_handle"* %6)
          to label %237 unwind label %438

; <label>:237                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41
  %238 = load %"class.sc_core::sc_process_b"** %228, align 8, !tbaa !0
  %239 = icmp eq %"class.sc_core::sc_process_b"* %238, null
  br i1 %239, label %_ZN7sc_core17sc_process_handleD1Ev.exit44, label %240

; <label>:240                                     ; preds = %237
  %241 = getelementptr inbounds %"class.sc_core::sc_process_b"* %238, i64 0, i32 15
  %242 = load i32* %241, align 4, !tbaa !5
  %243 = add nsw i32 %242, -1
  store i32 %243, i32* %241, align 4, !tbaa !5
  %244 = icmp eq i32 %243, 0
  br i1 %244, label %245, label %_ZN7sc_core17sc_process_handleD1Ev.exit44

; <label>:245                                     ; preds = %240
  %246 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %247 = icmp eq %"class.sc_core::sc_process_b"* %246, null
  br i1 %247, label %253, label %248

; <label>:248                                     ; preds = %245
  %249 = bitcast %"class.sc_core::sc_process_b"* %246 to void (%"class.sc_core::sc_process_b"*)***
  %250 = load void (%"class.sc_core::sc_process_b"*)*** %249, align 8, !tbaa !3
  %251 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %250, i64 6
  %252 = load void (%"class.sc_core::sc_process_b"*)** %251, align 8
  invoke void %252(%"class.sc_core::sc_process_b"* %246)
          to label %.noexc43 unwind label %386

.noexc43:                                         ; preds = %248
  %.pre.i.i.i42 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %253

; <label>:253                                     ; preds = %.noexc43, %245
  %254 = phi %"class.sc_core::sc_process_b"* [ null, %245 ], [ %.pre.i.i.i42, %.noexc43 ]
  %255 = icmp eq %"class.sc_core::sc_process_b"* %254, %238
  br i1 %255, label %256, label %257

; <label>:256                                     ; preds = %253
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:257                                     ; preds = %253
  store %"class.sc_core::sc_process_b"* %238, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit44

_ZN7sc_core17sc_process_handleD1Ev.exit44:        ; preds = %257, %240, %237
  %258 = load %"class.sc_core::sc_process_b"** %164, align 8, !tbaa !0
  %259 = icmp eq %"class.sc_core::sc_process_b"* %258, null
  br i1 %259, label %_ZN7sc_core17sc_process_handleD1Ev.exit47, label %260

; <label>:260                                     ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit44
  %261 = getelementptr inbounds %"class.sc_core::sc_process_b"* %258, i64 0, i32 15
  %262 = load i32* %261, align 4, !tbaa !5
  %263 = add nsw i32 %262, -1
  store i32 %263, i32* %261, align 4, !tbaa !5
  %264 = icmp eq i32 %263, 0
  br i1 %264, label %265, label %_ZN7sc_core17sc_process_handleD1Ev.exit47

; <label>:265                                     ; preds = %260
  %266 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %267 = icmp eq %"class.sc_core::sc_process_b"* %266, null
  br i1 %267, label %273, label %268

; <label>:268                                     ; preds = %265
  %269 = bitcast %"class.sc_core::sc_process_b"* %266 to void (%"class.sc_core::sc_process_b"*)***
  %270 = load void (%"class.sc_core::sc_process_b"*)*** %269, align 8, !tbaa !3
  %271 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %270, i64 6
  %272 = load void (%"class.sc_core::sc_process_b"*)** %271, align 8
  invoke void %272(%"class.sc_core::sc_process_b"* %266)
          to label %.noexc46 unwind label %286

.noexc46:                                         ; preds = %268
  %.pre.i.i.i45 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %273

; <label>:273                                     ; preds = %.noexc46, %265
  %274 = phi %"class.sc_core::sc_process_b"* [ null, %265 ], [ %.pre.i.i.i45, %.noexc46 ]
  %275 = icmp eq %"class.sc_core::sc_process_b"* %274, %258
  br i1 %275, label %276, label %277

; <label>:276                                     ; preds = %273
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:277                                     ; preds = %273
  store %"class.sc_core::sc_process_b"* %258, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit47

_ZN7sc_core17sc_process_handleD1Ev.exit47:        ; preds = %277, %260, %_ZN7sc_core17sc_process_handleD1Ev.exit44
  %278 = getelementptr inbounds %struct.M1* %this, i64 0, i32 2
  %279 = zext i1 %x to i8
  store i8 %279, i8* %278, align 1, !tbaa !6
  %280 = getelementptr inbounds %struct.M1* %this, i64 0, i32 3
  %281 = zext i1 %y to i8
  store i8 %281, i8* %280, align 1, !tbaa !6
  ret void

; <label>:282                                     ; preds = %15
  %283 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  br label %.body

.body:                                            ; preds = %282, %19
  %eh.lpad-body = phi { i8*, i32 } [ %283, %282 ], [ %20, %19 ]
  %284 = extractvalue { i8*, i32 } %eh.lpad-body, 0
  %285 = extractvalue { i8*, i32 } %eh.lpad-body, 1
  br label %_ZN7sc_core8sc_eventD1Ev.exit

; <label>:286                                     ; preds = %268, %_ZN7sc_core22sc_get_curr_simcontextEv.exit29, %156, %144, %_ZN7sc_core22sc_get_curr_simcontextEv.exit, %29
  %287 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  br label %.body10

.body10:                                          ; preds = %286, %160, %33
  %eh.lpad-body11 = phi { i8*, i32 } [ %34, %33 ], [ %287, %286 ], [ %161, %160 ]
  %288 = extractvalue { i8*, i32 } %eh.lpad-body11, 0
  %289 = extractvalue { i8*, i32 } %eh.lpad-body11, 1
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit59

; <label>:290                                     ; preds = %124, %92, %60
  %291 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %292 = extractvalue { i8*, i32 } %291, 0
  %293 = extractvalue { i8*, i32 } %291, 1
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit50

; <label>:294                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %295 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %296 = extractvalue { i8*, i32 } %295, 0
  %297 = extractvalue { i8*, i32 } %295, 1
  %298 = load %"class.sc_core::sc_process_b"** %40, align 8, !tbaa !0
  %299 = icmp eq %"class.sc_core::sc_process_b"* %298, null
  br i1 %299, label %_ZN7sc_core17sc_process_handleD1Ev.exit50, label %300

; <label>:300                                     ; preds = %294
  %301 = getelementptr inbounds %"class.sc_core::sc_process_b"* %298, i64 0, i32 15
  %302 = load i32* %301, align 4, !tbaa !5
  %303 = add nsw i32 %302, -1
  store i32 %303, i32* %301, align 4, !tbaa !5
  %304 = icmp eq i32 %303, 0
  br i1 %304, label %305, label %_ZN7sc_core17sc_process_handleD1Ev.exit50

; <label>:305                                     ; preds = %300
  %306 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %307 = icmp eq %"class.sc_core::sc_process_b"* %306, null
  br i1 %307, label %313, label %308

; <label>:308                                     ; preds = %305
  %309 = bitcast %"class.sc_core::sc_process_b"* %306 to void (%"class.sc_core::sc_process_b"*)***
  %310 = load void (%"class.sc_core::sc_process_b"*)*** %309, align 8, !tbaa !3
  %311 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %310, i64 6
  %312 = load void (%"class.sc_core::sc_process_b"*)** %311, align 8
  invoke void %312(%"class.sc_core::sc_process_b"* %306)
          to label %.noexc49 unwind label %485

.noexc49:                                         ; preds = %308
  %.pre.i.i.i48 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %313

; <label>:313                                     ; preds = %.noexc49, %305
  %314 = phi %"class.sc_core::sc_process_b"* [ null, %305 ], [ %.pre.i.i.i48, %.noexc49 ]
  %315 = icmp eq %"class.sc_core::sc_process_b"* %314, %298
  br i1 %315, label %316, label %317

; <label>:316                                     ; preds = %313
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:317                                     ; preds = %313
  store %"class.sc_core::sc_process_b"* %298, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit50

; <label>:318                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14
  %319 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %320 = extractvalue { i8*, i32 } %319, 0
  %321 = extractvalue { i8*, i32 } %319, 1
  %322 = load %"class.sc_core::sc_process_b"** %72, align 8, !tbaa !0
  %323 = icmp eq %"class.sc_core::sc_process_b"* %322, null
  br i1 %323, label %_ZN7sc_core17sc_process_handleD1Ev.exit50, label %324

; <label>:324                                     ; preds = %318
  %325 = getelementptr inbounds %"class.sc_core::sc_process_b"* %322, i64 0, i32 15
  %326 = load i32* %325, align 4, !tbaa !5
  %327 = add nsw i32 %326, -1
  store i32 %327, i32* %325, align 4, !tbaa !5
  %328 = icmp eq i32 %327, 0
  br i1 %328, label %329, label %_ZN7sc_core17sc_process_handleD1Ev.exit50

; <label>:329                                     ; preds = %324
  %330 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %331 = icmp eq %"class.sc_core::sc_process_b"* %330, null
  br i1 %331, label %337, label %332

; <label>:332                                     ; preds = %329
  %333 = bitcast %"class.sc_core::sc_process_b"* %330 to void (%"class.sc_core::sc_process_b"*)***
  %334 = load void (%"class.sc_core::sc_process_b"*)*** %333, align 8, !tbaa !3
  %335 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %334, i64 6
  %336 = load void (%"class.sc_core::sc_process_b"*)** %335, align 8
  invoke void %336(%"class.sc_core::sc_process_b"* %330)
          to label %.noexc52 unwind label %485

.noexc52:                                         ; preds = %332
  %.pre.i.i.i51 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %337

; <label>:337                                     ; preds = %.noexc52, %329
  %338 = phi %"class.sc_core::sc_process_b"* [ null, %329 ], [ %.pre.i.i.i51, %.noexc52 ]
  %339 = icmp eq %"class.sc_core::sc_process_b"* %338, %322
  br i1 %339, label %340, label %341

; <label>:340                                     ; preds = %337
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:341                                     ; preds = %337
  store %"class.sc_core::sc_process_b"* %322, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit50

; <label>:342                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19
  %343 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %344 = extractvalue { i8*, i32 } %343, 0
  %345 = extractvalue { i8*, i32 } %343, 1
  %346 = load %"class.sc_core::sc_process_b"** %104, align 8, !tbaa !0
  %347 = icmp eq %"class.sc_core::sc_process_b"* %346, null
  br i1 %347, label %_ZN7sc_core17sc_process_handleD1Ev.exit50, label %348

; <label>:348                                     ; preds = %342
  %349 = getelementptr inbounds %"class.sc_core::sc_process_b"* %346, i64 0, i32 15
  %350 = load i32* %349, align 4, !tbaa !5
  %351 = add nsw i32 %350, -1
  store i32 %351, i32* %349, align 4, !tbaa !5
  %352 = icmp eq i32 %351, 0
  br i1 %352, label %353, label %_ZN7sc_core17sc_process_handleD1Ev.exit50

; <label>:353                                     ; preds = %348
  %354 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %355 = icmp eq %"class.sc_core::sc_process_b"* %354, null
  br i1 %355, label %361, label %356

; <label>:356                                     ; preds = %353
  %357 = bitcast %"class.sc_core::sc_process_b"* %354 to void (%"class.sc_core::sc_process_b"*)***
  %358 = load void (%"class.sc_core::sc_process_b"*)*** %357, align 8, !tbaa !3
  %359 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %358, i64 6
  %360 = load void (%"class.sc_core::sc_process_b"*)** %359, align 8
  invoke void %360(%"class.sc_core::sc_process_b"* %354)
          to label %.noexc55 unwind label %485

.noexc55:                                         ; preds = %356
  %.pre.i.i.i54 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %361

; <label>:361                                     ; preds = %.noexc55, %353
  %362 = phi %"class.sc_core::sc_process_b"* [ null, %353 ], [ %.pre.i.i.i54, %.noexc55 ]
  %363 = icmp eq %"class.sc_core::sc_process_b"* %362, %346
  br i1 %363, label %364, label %365

; <label>:364                                     ; preds = %361
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:365                                     ; preds = %361
  store %"class.sc_core::sc_process_b"* %346, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit50

_ZN7sc_core17sc_process_handleD1Ev.exit50:        ; preds = %365, %348, %342, %341, %324, %318, %317, %300, %294, %290
  %.01 = phi i8* [ %292, %290 ], [ %296, %294 ], [ %296, %300 ], [ %296, %317 ], [ %320, %318 ], [ %320, %324 ], [ %320, %341 ], [ %344, %342 ], [ %344, %348 ], [ %344, %365 ]
  %.0 = phi i32 [ %293, %290 ], [ %297, %294 ], [ %297, %300 ], [ %297, %317 ], [ %321, %318 ], [ %321, %324 ], [ %321, %341 ], [ %345, %342 ], [ %345, %348 ], [ %345, %365 ]
  %366 = load %"class.sc_core::sc_process_b"** %38, align 8, !tbaa !0
  %367 = icmp eq %"class.sc_core::sc_process_b"* %366, null
  br i1 %367, label %_ZN7sc_core17sc_process_handleD1Ev.exit59, label %368

; <label>:368                                     ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit50
  %369 = getelementptr inbounds %"class.sc_core::sc_process_b"* %366, i64 0, i32 15
  %370 = load i32* %369, align 4, !tbaa !5
  %371 = add nsw i32 %370, -1
  store i32 %371, i32* %369, align 4, !tbaa !5
  %372 = icmp eq i32 %371, 0
  br i1 %372, label %373, label %_ZN7sc_core17sc_process_handleD1Ev.exit59

; <label>:373                                     ; preds = %368
  %374 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %375 = icmp eq %"class.sc_core::sc_process_b"* %374, null
  br i1 %375, label %381, label %376

; <label>:376                                     ; preds = %373
  %377 = bitcast %"class.sc_core::sc_process_b"* %374 to void (%"class.sc_core::sc_process_b"*)***
  %378 = load void (%"class.sc_core::sc_process_b"*)*** %377, align 8, !tbaa !3
  %379 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %378, i64 6
  %380 = load void (%"class.sc_core::sc_process_b"*)** %379, align 8
  invoke void %380(%"class.sc_core::sc_process_b"* %374)
          to label %.noexc58 unwind label %485

.noexc58:                                         ; preds = %376
  %.pre.i.i.i57 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %381

; <label>:381                                     ; preds = %.noexc58, %373
  %382 = phi %"class.sc_core::sc_process_b"* [ null, %373 ], [ %.pre.i.i.i57, %.noexc58 ]
  %383 = icmp eq %"class.sc_core::sc_process_b"* %382, %366
  br i1 %383, label %384, label %385

; <label>:384                                     ; preds = %381
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:385                                     ; preds = %381
  store %"class.sc_core::sc_process_b"* %366, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit59

; <label>:386                                     ; preds = %248, %217, %186
  %387 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %388 = extractvalue { i8*, i32 } %387, 0
  %389 = extractvalue { i8*, i32 } %387, 1
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit62

; <label>:390                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31
  %391 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %392 = extractvalue { i8*, i32 } %391, 0
  %393 = extractvalue { i8*, i32 } %391, 1
  %394 = load %"class.sc_core::sc_process_b"** %166, align 8, !tbaa !0
  %395 = icmp eq %"class.sc_core::sc_process_b"* %394, null
  br i1 %395, label %_ZN7sc_core17sc_process_handleD1Ev.exit62, label %396

; <label>:396                                     ; preds = %390
  %397 = getelementptr inbounds %"class.sc_core::sc_process_b"* %394, i64 0, i32 15
  %398 = load i32* %397, align 4, !tbaa !5
  %399 = add nsw i32 %398, -1
  store i32 %399, i32* %397, align 4, !tbaa !5
  %400 = icmp eq i32 %399, 0
  br i1 %400, label %401, label %_ZN7sc_core17sc_process_handleD1Ev.exit62

; <label>:401                                     ; preds = %396
  %402 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %403 = icmp eq %"class.sc_core::sc_process_b"* %402, null
  br i1 %403, label %409, label %404

; <label>:404                                     ; preds = %401
  %405 = bitcast %"class.sc_core::sc_process_b"* %402 to void (%"class.sc_core::sc_process_b"*)***
  %406 = load void (%"class.sc_core::sc_process_b"*)*** %405, align 8, !tbaa !3
  %407 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %406, i64 6
  %408 = load void (%"class.sc_core::sc_process_b"*)** %407, align 8
  invoke void %408(%"class.sc_core::sc_process_b"* %402)
          to label %.noexc61 unwind label %485

.noexc61:                                         ; preds = %404
  %.pre.i.i.i60 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %409

; <label>:409                                     ; preds = %.noexc61, %401
  %410 = phi %"class.sc_core::sc_process_b"* [ null, %401 ], [ %.pre.i.i.i60, %.noexc61 ]
  %411 = icmp eq %"class.sc_core::sc_process_b"* %410, %394
  br i1 %411, label %412, label %413

; <label>:412                                     ; preds = %409
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:413                                     ; preds = %409
  store %"class.sc_core::sc_process_b"* %394, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit62

; <label>:414                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36
  %415 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %416 = extractvalue { i8*, i32 } %415, 0
  %417 = extractvalue { i8*, i32 } %415, 1
  %418 = load %"class.sc_core::sc_process_b"** %197, align 8, !tbaa !0
  %419 = icmp eq %"class.sc_core::sc_process_b"* %418, null
  br i1 %419, label %_ZN7sc_core17sc_process_handleD1Ev.exit62, label %420

; <label>:420                                     ; preds = %414
  %421 = getelementptr inbounds %"class.sc_core::sc_process_b"* %418, i64 0, i32 15
  %422 = load i32* %421, align 4, !tbaa !5
  %423 = add nsw i32 %422, -1
  store i32 %423, i32* %421, align 4, !tbaa !5
  %424 = icmp eq i32 %423, 0
  br i1 %424, label %425, label %_ZN7sc_core17sc_process_handleD1Ev.exit62

; <label>:425                                     ; preds = %420
  %426 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %427 = icmp eq %"class.sc_core::sc_process_b"* %426, null
  br i1 %427, label %433, label %428

; <label>:428                                     ; preds = %425
  %429 = bitcast %"class.sc_core::sc_process_b"* %426 to void (%"class.sc_core::sc_process_b"*)***
  %430 = load void (%"class.sc_core::sc_process_b"*)*** %429, align 8, !tbaa !3
  %431 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %430, i64 6
  %432 = load void (%"class.sc_core::sc_process_b"*)** %431, align 8
  invoke void %432(%"class.sc_core::sc_process_b"* %426)
          to label %.noexc64 unwind label %485

.noexc64:                                         ; preds = %428
  %.pre.i.i.i63 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %433

; <label>:433                                     ; preds = %.noexc64, %425
  %434 = phi %"class.sc_core::sc_process_b"* [ null, %425 ], [ %.pre.i.i.i63, %.noexc64 ]
  %435 = icmp eq %"class.sc_core::sc_process_b"* %434, %418
  br i1 %435, label %436, label %437

; <label>:436                                     ; preds = %433
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:437                                     ; preds = %433
  store %"class.sc_core::sc_process_b"* %418, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit62

; <label>:438                                     ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41
  %439 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %440 = extractvalue { i8*, i32 } %439, 0
  %441 = extractvalue { i8*, i32 } %439, 1
  %442 = load %"class.sc_core::sc_process_b"** %228, align 8, !tbaa !0
  %443 = icmp eq %"class.sc_core::sc_process_b"* %442, null
  br i1 %443, label %_ZN7sc_core17sc_process_handleD1Ev.exit62, label %444

; <label>:444                                     ; preds = %438
  %445 = getelementptr inbounds %"class.sc_core::sc_process_b"* %442, i64 0, i32 15
  %446 = load i32* %445, align 4, !tbaa !5
  %447 = add nsw i32 %446, -1
  store i32 %447, i32* %445, align 4, !tbaa !5
  %448 = icmp eq i32 %447, 0
  br i1 %448, label %449, label %_ZN7sc_core17sc_process_handleD1Ev.exit62

; <label>:449                                     ; preds = %444
  %450 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %451 = icmp eq %"class.sc_core::sc_process_b"* %450, null
  br i1 %451, label %457, label %452

; <label>:452                                     ; preds = %449
  %453 = bitcast %"class.sc_core::sc_process_b"* %450 to void (%"class.sc_core::sc_process_b"*)***
  %454 = load void (%"class.sc_core::sc_process_b"*)*** %453, align 8, !tbaa !3
  %455 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %454, i64 6
  %456 = load void (%"class.sc_core::sc_process_b"*)** %455, align 8
  invoke void %456(%"class.sc_core::sc_process_b"* %450)
          to label %.noexc67 unwind label %485

.noexc67:                                         ; preds = %452
  %.pre.i.i.i66 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %457

; <label>:457                                     ; preds = %.noexc67, %449
  %458 = phi %"class.sc_core::sc_process_b"* [ null, %449 ], [ %.pre.i.i.i66, %.noexc67 ]
  %459 = icmp eq %"class.sc_core::sc_process_b"* %458, %442
  br i1 %459, label %460, label %461

; <label>:460                                     ; preds = %457
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:461                                     ; preds = %457
  store %"class.sc_core::sc_process_b"* %442, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit62

_ZN7sc_core17sc_process_handleD1Ev.exit62:        ; preds = %461, %444, %438, %437, %420, %414, %413, %396, %390, %386
  %.12 = phi i8* [ %388, %386 ], [ %392, %390 ], [ %392, %396 ], [ %392, %413 ], [ %416, %414 ], [ %416, %420 ], [ %416, %437 ], [ %440, %438 ], [ %440, %444 ], [ %440, %461 ]
  %.1 = phi i32 [ %389, %386 ], [ %393, %390 ], [ %393, %396 ], [ %393, %413 ], [ %417, %414 ], [ %417, %420 ], [ %417, %437 ], [ %441, %438 ], [ %441, %444 ], [ %441, %461 ]
  %462 = load %"class.sc_core::sc_process_b"** %164, align 8, !tbaa !0
  %463 = icmp eq %"class.sc_core::sc_process_b"* %462, null
  br i1 %463, label %_ZN7sc_core17sc_process_handleD1Ev.exit59, label %464

; <label>:464                                     ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit62
  %465 = getelementptr inbounds %"class.sc_core::sc_process_b"* %462, i64 0, i32 15
  %466 = load i32* %465, align 4, !tbaa !5
  %467 = add nsw i32 %466, -1
  store i32 %467, i32* %465, align 4, !tbaa !5
  %468 = icmp eq i32 %467, 0
  br i1 %468, label %469, label %_ZN7sc_core17sc_process_handleD1Ev.exit59

; <label>:469                                     ; preds = %464
  %470 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  %471 = icmp eq %"class.sc_core::sc_process_b"* %470, null
  br i1 %471, label %477, label %472

; <label>:472                                     ; preds = %469
  %473 = bitcast %"class.sc_core::sc_process_b"* %470 to void (%"class.sc_core::sc_process_b"*)***
  %474 = load void (%"class.sc_core::sc_process_b"*)*** %473, align 8, !tbaa !3
  %475 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %474, i64 6
  %476 = load void (%"class.sc_core::sc_process_b"*)** %475, align 8
  invoke void %476(%"class.sc_core::sc_process_b"* %470)
          to label %.noexc70 unwind label %485

.noexc70:                                         ; preds = %472
  %.pre.i.i.i69 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %477

; <label>:477                                     ; preds = %.noexc70, %469
  %478 = phi %"class.sc_core::sc_process_b"* [ null, %469 ], [ %.pre.i.i.i69, %.noexc70 ]
  %479 = icmp eq %"class.sc_core::sc_process_b"* %478, %462
  br i1 %479, label %480, label %481

; <label>:480                                     ; preds = %477
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str11, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str12, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

; <label>:481                                     ; preds = %477
  store %"class.sc_core::sc_process_b"* %462, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit59

_ZN7sc_core17sc_process_handleD1Ev.exit59:        ; preds = %481, %464, %_ZN7sc_core17sc_process_handleD1Ev.exit62, %385, %368, %_ZN7sc_core17sc_process_handleD1Ev.exit50, %.body10
  %.23 = phi i8* [ %288, %.body10 ], [ %.01, %_ZN7sc_core17sc_process_handleD1Ev.exit50 ], [ %.01, %368 ], [ %.01, %385 ], [ %.12, %_ZN7sc_core17sc_process_handleD1Ev.exit62 ], [ %.12, %464 ], [ %.12, %481 ]
  %.2 = phi i32 [ %289, %.body10 ], [ %.0, %_ZN7sc_core17sc_process_handleD1Ev.exit50 ], [ %.0, %368 ], [ %.0, %385 ], [ %.1, %_ZN7sc_core17sc_process_handleD1Ev.exit62 ], [ %.1, %464 ], [ %.1, %481 ]
  invoke void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %11)
          to label %_ZN7sc_core8sc_eventD1Ev.exit unwind label %485

_ZN7sc_core8sc_eventD1Ev.exit:                    ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit59, %.body
  %.34 = phi i8* [ %284, %.body ], [ %.23, %_ZN7sc_core17sc_process_handleD1Ev.exit59 ]
  %.3 = phi i32 [ %285, %.body ], [ %.2, %_ZN7sc_core17sc_process_handleD1Ev.exit59 ]
  invoke void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %7)
          to label %482 unwind label %485

; <label>:482                                     ; preds = %_ZN7sc_core8sc_eventD1Ev.exit
  %483 = insertvalue { i8*, i32 } undef, i8* %.34, 0
  %484 = insertvalue { i8*, i32 } %483, i32 %.3, 1
  resume { i8*, i32 } %484

; <label>:485                                     ; preds = %_ZN7sc_core8sc_eventD1Ev.exit, %_ZN7sc_core17sc_process_handleD1Ev.exit59, %472, %452, %428, %404, %376, %356, %332, %308
  %486 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  call void @_ZSt9terminatev() noreturn nounwind
  unreachable
}

declare void @_ZN7sc_core9sc_moduleC2Ev(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

define linkonce_odr void @_ZN2M12T1Ev(%struct.M1* %this) uwtable align 2 {
  %1 = alloca %"class.sc_core::sc_time", align 8
  %2 = getelementptr inbounds %struct.M1* %this, i64 0, i32 3
  %3 = load i8* %2, align 1, !tbaa !6, !range !7
  %4 = icmp eq i8 %3, 0
  br i1 %4, label %._crit_edge, label %5

._crit_edge:                                      ; preds = %0
  %.pre = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 0, i32 1
  %.pre1 = getelementptr inbounds %struct.M1* %this, i64 0, i32 2
  br label %10

; <label>:5                                       ; preds = %0
  %6 = getelementptr inbounds %struct.M1* %this, i64 0, i32 1
  %7 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 0, i32 1
  %8 = load %"class.sc_core::sc_simcontext"** %7, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %6, %"class.sc_core::sc_simcontext"* %8)
  %9 = getelementptr inbounds %struct.M1* %this, i64 0, i32 2
  store i8 0, i8* %9, align 1, !tbaa !6
  br label %10

; <label>:10                                      ; preds = %5, %._crit_edge
  %.pre-phi2 = phi i8* [ %.pre1, %._crit_edge ], [ %9, %5 ]
  %.pre-phi = phi %"class.sc_core::sc_simcontext"** [ %.pre, %._crit_edge ], [ %7, %5 ]
  %11 = bitcast %"class.sc_core::sc_time"* %1 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %11)
  %12 = load %"class.sc_core::sc_simcontext"** %.pre-phi, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %1, double 1.000000e+01, i32 2, %"class.sc_core::sc_simcontext"* %12)
  %13 = load %"class.sc_core::sc_simcontext"** %.pre-phi, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %1, %"class.sc_core::sc_simcontext"* %13)
  call void @llvm.lifetime.end(i64 -1, i8* %11)
  store i8 1, i8* %.pre-phi2, align 1, !tbaa !6
  ret void
}

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

define linkonce_odr void @_ZN2M12T2Ev(%struct.M1* %this) uwtable align 2 {
  %1 = alloca %"class.sc_core::sc_time", align 8
  %2 = getelementptr inbounds %struct.M1* %this, i64 0, i32 2
  %3 = load i8* %2, align 1, !tbaa !6, !range !7
  %4 = icmp eq i8 %3, 0
  %.pre = getelementptr inbounds %struct.M1* %this, i64 0, i32 3
  br i1 %4, label %._crit_edge, label %5

; <label>:5                                       ; preds = %0
  store i8 0, i8* %.pre, align 1, !tbaa !6
  %6 = getelementptr inbounds %struct.M1* %this, i64 0, i32 1
  call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %6)
  br label %._crit_edge

._crit_edge:                                      ; preds = %5, %0
  %7 = bitcast %"class.sc_core::sc_time"* %1 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %7)
  %8 = getelementptr inbounds %struct.M1* %this, i64 0, i32 0, i32 0, i32 1
  %9 = load %"class.sc_core::sc_simcontext"** %8, align 8, !tbaa !0
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %1, double 1.000000e+01, i32 2, %"class.sc_core::sc_simcontext"* %9)
  %10 = load %"class.sc_core::sc_simcontext"** %8, align 8, !tbaa !0
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %1, %"class.sc_core::sc_simcontext"* %10)
  call void @llvm.lifetime.end(i64 -1, i8* %7)
  store i8 1, i8* %.pre, align 1, !tbaa !6
  ret void
}

declare void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"*)

declare void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"*, double, i32, %"class.sc_core::sc_simcontext"*)

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

declare void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"*, %"class.sc_core::sc_simcontext"*)

declare void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"*)

define internal void @_GLOBAL__I_a() section ".text.startup" {
  tail call void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"* @_ZStL8__ioinit)
  %1 = tail call i32 @__cxa_atexit(void (i8*)* bitcast (void (%"class.std::ios_base::Init"*)* @_ZNSt8ios_base4InitD1Ev to void (i8*)*), i8* getelementptr inbounds (%"class.std::ios_base::Init"* @_ZStL8__ioinit, i64 0, i32 0), i8* @__dso_handle)
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
!4 = metadata !{metadata !"_ZTSN7sc_core8sc_event8notify_tE", metadata !1}
!5 = metadata !{metadata !"int", metadata !1}
!6 = metadata !{metadata !"bool", metadata !1}
!7 = metadata !{i8 0, i8 2}                       
