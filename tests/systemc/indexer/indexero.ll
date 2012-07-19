; ModuleID = 'indexero.bc'
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
%"class.sc_core::sc_module_name" = type { i8*, %"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*, %"class.sc_core::sc_simcontext"*, i8 }
%"class.sc_core::sc_module" = type { %"class.sc_core::sc_object", %"class.sc_core::sc_process_host", %"class.sc_core::sc_sensitive", %"class.sc_core::sc_sensitive_pos", %"class.sc_core::sc_sensitive_neg", i8, %"class.std::vector"*, i32, %"class.sc_core::sc_name_gen"*, %"class.std::vector.10", %"class.sc_core::sc_module_name"* }
%"class.sc_core::sc_sensitive" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_pos" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_sensitive_neg" = type { %"class.sc_core::sc_module"*, i32, %"class.sc_core::sc_process_b"* }
%class.element = type { %"class.sc_core::sc_module", i32, i32, i32, i32, %"class.sc_core::sc_event" }
%"class.sc_core::sc_process_handle" = type { %"class.sc_core::sc_process_b"* }
%"class.sc_core::sc_spawn_options" = type opaque

@_ZStL8__ioinit = internal global %"class.std::ios_base::Init" zeroinitializer, align 1
@__dso_handle = external global i8
@_ZN7sc_coreL17api_version_checkE = internal global %"class.sc_core::sc_api_version_2_2_0" zeroinitializer, align 1
@_ZTVN10__cxxabiv121__vmi_class_type_infoE = external global i8*
@_ZTVN10__cxxabiv117__class_type_infoE = external global i8*
@table = global [128 x i32] zeroinitializer, align 16
@_ZSt4cout = external global %"class.std::basic_ostream"
@.str = private unnamed_addr constant [6 x i8] c"DUMP \00", align 1
@.str2 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str3 = private unnamed_addr constant [8 x i8] c"element\00", align 1
@.str4 = private unnamed_addr constant [5 x i8] c"END \00", align 1
@_ZTVN10__cxxabiv120__si_class_type_infoE = external global i8*
@_ZTSN7sc_core9sc_objectE = available_externally constant [21 x i8] c"N7sc_core9sc_objectE\00"
@_ZTIN7sc_core9sc_objectE = available_externally unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_objectE, i32 0, i32 0) }
@_ZTV7element = linkonce_odr unnamed_addr constant [17 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI7element to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object5printERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)* @_ZNK7sc_core9sc_object4dumpERSo to i8*), i8* bitcast (void (%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)* @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE to i8*), i8* bitcast (i8* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module4kindEv to i8*), i8* bitcast (%"class.std::vector.10"* (%"class.sc_core::sc_module"*)* @_ZNK7sc_core9sc_module17get_child_objectsEv to i8*), i8* bitcast (void (%class.element*)* @_ZN7elementD1Ev to i8*), i8* bitcast (void (%class.element*)* @_ZN7elementD0Ev to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module25before_end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module18end_of_elaborationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module19start_of_simulationEv to i8*), i8* bitcast (void (%"class.sc_core::sc_module"*)* @_ZN7sc_core9sc_module17end_of_simulationEv to i8*), i8* inttoptr (i64 -40 to i8*), i8* bitcast ({ i8*, i8*, i8* }* @_ZTI7element to i8*), i8* bitcast (void (%class.element*)* @_ZThn40_N7elementD1Ev to i8*), i8* bitcast (void (%class.element*)* @_ZThn40_N7elementD0Ev to i8*)]
@.str11 = private unnamed_addr constant [2 x i8] c"T\00", align 1
@.str12 = private unnamed_addr constant [7 x i8] c"getmsg\00", align 1
@_ZTS7element = linkonce_odr constant [9 x i8] c"7element\00"
@_ZTSN7sc_core9sc_moduleE = available_externally constant [21 x i8] c"N7sc_core9sc_moduleE\00"
@_ZTSN7sc_core15sc_process_hostE = linkonce_odr constant [28 x i8] c"N7sc_core15sc_process_hostE\00"
@_ZTIN7sc_core15sc_process_hostE = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([28 x i8]* @_ZTSN7sc_core15sc_process_hostE, i32 0, i32 0) }
@_ZTIN7sc_core9sc_moduleE = available_externally unnamed_addr constant { i8*, i8*, i32, i32, i8*, i64, i8*, i64 } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv121__vmi_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([21 x i8]* @_ZTSN7sc_core9sc_moduleE, i32 0, i32 0), i32 0, i32 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core9sc_objectE to i8*), i64 2, i8* bitcast ({ i8*, i8* }* @_ZTIN7sc_core15sc_process_hostE to i8*), i64 10242 }
@_ZTI7element = linkonce_odr unnamed_addr constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([9 x i8]* @_ZTS7element, i32 0, i32 0), i8* bitcast ({ i8*, i8*, i32, i32, i8*, i64, i8*, i64 }* @_ZTIN7sc_core9sc_moduleE to i8*) }
@.str13 = private unnamed_addr constant [10 x i8] c"sc_module\00", align 1
@_ZN7sc_core12sc_process_b15m_delete_next_pE = external global %"class.sc_core::sc_process_b"*
@.str14 = private unnamed_addr constant [24 x i8] c"m_delete_next_p != this\00", align 1
@.str15 = private unnamed_addr constant [44 x i8] c"/usr/local/include/sysc/kernel/sc_process.h\00", align 1
@__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv = private unnamed_addr constant [50 x i8] c"void sc_core::sc_process_b::reference_decrement()\00", align 1
@.str16 = private unnamed_addr constant [20 x i8] c"m_references_n != 0\00", align 1
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

define i32 @_Z4hashi(i32 %w) nounwind uwtable readnone {
bb:
  %tmp = mul nsw i32 %w, 7
  %tmp1 = srem i32 %tmp, 128
  ret i32 %tmp1
}

define void @_Z10dump_tablev() uwtable {
bb:
  %tmp = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([6 x i8]* @.str, i64 0, i64 0), i64 5)
  br label %bb3

bb3:                                              ; preds = %bb3, %bb
  %indvars.iv = phi i64 [ 0, %bb ], [ %indvars.iv.next, %bb3 ]
  %tmp4 = getelementptr inbounds [128 x i32]* @table, i64 0, i64 %indvars.iv
  %tmp5 = load i32* %tmp4, align 4, !tbaa !0
  %tmp6 = tail call %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* @_ZSt4cout, i32 %tmp5)
  %tmp7 = tail call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* %tmp6, i8* getelementptr inbounds ([2 x i8]* @.str2, i64 0, i64 0), i64 1)
  %indvars.iv.next = add i64 %indvars.iv, 1
  %lftr.wideiv1 = trunc i64 %indvars.iv.next to i32
  %exitcond2 = icmp eq i32 %lftr.wideiv1, 128
  br i1 %exitcond2, label %bb8, label %bb3

bb8:                                              ; preds = %bb3
  %tmp9 = load i8** bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8**), align 8, !tbaa !3
  %tmp10 = getelementptr i8* %tmp9, i64 -24
  %tmp11 = bitcast i8* %tmp10 to i64*
  %tmp12 = load i64* %tmp11, align 8
  %.sum = add i64 %tmp12, 240
  %tmp13 = getelementptr inbounds i8* bitcast (%"class.std::basic_ostream"* @_ZSt4cout to i8*), i64 %.sum
  %tmp14 = bitcast i8* %tmp13 to %"class.std::ctype"**
  %tmp15 = load %"class.std::ctype"** %tmp14, align 8, !tbaa !4
  %tmp16 = icmp eq %"class.std::ctype"* %tmp15, null
  br i1 %tmp16, label %bb17, label %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit

bb17:                                             ; preds = %bb8
  tail call void @_ZSt16__throw_bad_castv() noreturn
  unreachable

_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit:    ; preds = %bb8
  %tmp18 = getelementptr inbounds %"class.std::ctype"* %tmp15, i64 0, i32 6
  %tmp19 = load i8* %tmp18, align 1, !tbaa !1
  %tmp20 = icmp eq i8 %tmp19, 0
  br i1 %tmp20, label %bb24, label %bb21

bb21:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit
  %tmp22 = getelementptr inbounds %"class.std::ctype"* %tmp15, i64 0, i32 7, i64 10
  %tmp23 = load i8* %tmp22, align 1, !tbaa !1
  br label %_ZNKSt5ctypeIcE5widenEc.exit

bb24:                                             ; preds = %_ZSt13__check_facetISt5ctypeIcEERKT_PS3_.exit
  tail call void @_ZNKSt5ctypeIcE13_M_widen_initEv(%"class.std::ctype"* %tmp15)
  %tmp25 = bitcast %"class.std::ctype"* %tmp15 to i8 (%"class.std::ctype"*, i8)***
  %tmp26 = load i8 (%"class.std::ctype"*, i8)*** %tmp25, align 8, !tbaa !3
  %tmp27 = getelementptr inbounds i8 (%"class.std::ctype"*, i8)** %tmp26, i64 6
  %tmp28 = load i8 (%"class.std::ctype"*, i8)** %tmp27, align 8
  %tmp29 = tail call signext i8 %tmp28(%"class.std::ctype"* %tmp15, i8 signext 10)
  br label %_ZNKSt5ctypeIcE5widenEc.exit

_ZNKSt5ctypeIcE5widenEc.exit:                     ; preds = %bb24, %bb21
  %.0.i = phi i8 [ %tmp23, %bb21 ], [ %tmp29, %bb24 ]
  %tmp30 = tail call %"class.std::basic_ostream"* @_ZNSo3putEc(%"class.std::basic_ostream"* @_ZSt4cout, i8 signext %.0.i)
  %tmp31 = tail call %"class.std::basic_ostream"* @_ZNSo5flushEv(%"class.std::basic_ostream"* %tmp30)
  ret void
}

declare %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"*, i32)

define i32 @sc_main(i32 %argc, i8** nocapture %argv) uwtable {
bb:
  %tmp = alloca %"class.sc_core::sc_module_name", align 8
  %tmp1 = icmp sgt i32 %argc, 2
  br i1 %tmp1, label %bb2, label %_ZN7elementC1EN7sc_core14sc_module_nameEii.exit

bb2:                                              ; preds = %bb
  %tmp3 = getelementptr inbounds i8** %argv, i64 1
  %tmp4 = load i8** %tmp3, align 8, !tbaa !4
  %tmp5 = call i32 @atoi(i8* %tmp4) nounwind readonly
  %tmp6 = getelementptr inbounds i8** %argv, i64 2
  %tmp7 = load i8** %tmp6, align 8, !tbaa !4
  %tmp8 = call i32 @atoi(i8* %tmp7) nounwind readonly
  br label %_ZN7elementC1EN7sc_core14sc_module_nameEii.exit

_ZN7elementC1EN7sc_core14sc_module_nameEii.exit:  ; preds = %bb2, %bb
  %max.0 = phi i32 [ %tmp8, %bb2 ], [ 0, %bb ]
  %num.0 = phi i32 [ %tmp5, %bb2 ], [ 12, %bb ]
  %tmp9 = icmp slt i32 %num.0, 1
  %.num.0 = select i1 %tmp9, i32 12, i32 %num.0
  %tmp10 = call noalias i8* @_Znwm(i64 320)
  call void @_ZN7sc_core14sc_module_nameC1EPKc(%"class.sc_core::sc_module_name"* %tmp, i8* getelementptr inbounds ([8 x i8]* @.str3, i64 0, i64 0))
  %tmp11 = icmp sgt i32 %.num.0, 100
  %num.2 = select i1 %tmp11, i32 100, i32 %.num.0
  %tmp12 = bitcast i8* %tmp10 to %class.element*
  call void @_ZN7elementC2EN7sc_core14sc_module_nameEii(%class.element* %tmp12, %"class.sc_core::sc_module_name"* %tmp, i32 %num.2, i32 %max.0)
  call void @_ZN7sc_core14sc_module_nameD1Ev(%"class.sc_core::sc_module_name"* %tmp)
  call void @_ZN7sc_core8sc_startEv()
  %tmp13 = call %"class.std::basic_ostream"* @_ZSt16__ostream_insertIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_PKS3_l(%"class.std::basic_ostream"* @_ZSt4cout, i8* getelementptr inbounds ([5 x i8]* @.str4, i64 0, i64 0), i64 4)
  call void @_Z10dump_tablev()
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

define linkonce_odr void @_ZN7elementC2EN7sc_core14sc_module_nameEii(%class.element* %this, %"class.sc_core::sc_module_name"* %name, i32 %mm, i32 %mmax) unnamed_addr uwtable align 2 {
bb:
  %T_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp1 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp2 = alloca %"class.sc_core::sc_process_handle", align 8
  %getmsg_handle = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp3 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp4 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp5 = alloca %"class.sc_core::sc_process_handle", align 8
  %tmp6 = getelementptr inbounds %class.element* %this, i64 0, i32 0
  call void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"* %tmp6, %"class.sc_core::sc_module_name"* %name)
  %tmp7 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp7, align 8, !tbaa !3
  %tmp8 = getelementptr %class.element* %this, i64 0, i32 0, i32 1
  %tmp9 = getelementptr inbounds %"class.sc_core::sc_process_host"* %tmp8, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp9, align 8, !tbaa !3
  %tmp10 = getelementptr inbounds %class.element* %this, i64 0, i32 1
  store i32 %mm, i32* %tmp10, align 4, !tbaa !0
  %tmp11 = getelementptr inbounds %class.element* %this, i64 0, i32 2
  store i32 %mmax, i32* %tmp11, align 4, !tbaa !0
  %tmp12 = getelementptr inbounds %class.element* %this, i64 0, i32 3
  store i32 0, i32* %tmp12, align 4, !tbaa !0
  %tmp13 = getelementptr inbounds %class.element* %this, i64 0, i32 4
  store i32 1, i32* %tmp13, align 4, !tbaa !0
  %tmp14 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  %tmp15 = icmp eq %"class.sc_core::sc_simcontext"* %tmp14, null
  br i1 %tmp15, label %.noexc, label %bb18

.noexc:                                           ; preds = %bb
  %tmp16 = call noalias i8* @_Znwm(i64 248)
  %tmp17 = bitcast i8* %tmp16 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp17)
  store %"class.sc_core::sc_simcontext"* %tmp17, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !4
  store %"class.sc_core::sc_simcontext"* %tmp17, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  br label %bb18

bb18:                                             ; preds = %.noexc, %bb
  %tmp19 = phi %"class.sc_core::sc_simcontext"* [ %tmp17, %.noexc ], [ %tmp14, %bb ]
  %tmp20 = getelementptr inbounds %class.element* %this, i64 0, i32 5, i32 0
  store %"class.sc_core::sc_simcontext"* %tmp19, %"class.sc_core::sc_simcontext"** %tmp20, align 8, !tbaa !4
  %tmp21 = getelementptr inbounds %class.element* %this, i64 0, i32 5, i32 1
  store i32 0, i32* %tmp21, align 4, !tbaa !5
  %tmp22 = getelementptr inbounds %class.element* %this, i64 0, i32 5, i32 2
  store i32 -1, i32* %tmp22, align 4, !tbaa !0
  %tmp23 = getelementptr inbounds %class.element* %this, i64 0, i32 5, i32 3
  %tmp24 = bitcast %"class.sc_core::sc_event_timed"** %tmp23 to i8*
  call void @llvm.memset.p0i8.i64(i8* %tmp24, i8 0, i64 104, i32 8, i1 false)
  %tmp25 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  %tmp26 = icmp eq %"class.sc_core::sc_simcontext"* %tmp25, null
  br i1 %tmp26, label %.noexc9, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

.noexc9:                                          ; preds = %bb18
  %tmp27 = call noalias i8* @_Znwm(i64 248)
  %tmp28 = bitcast i8* %tmp27 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp28)
  store %"class.sc_core::sc_simcontext"* %tmp28, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !4
  store %"class.sc_core::sc_simcontext"* %tmp28, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit

_ZN7sc_core22sc_get_curr_simcontextEv.exit:       ; preds = %.noexc9, %bb18
  %tmp29 = phi %"class.sc_core::sc_simcontext"* [ %tmp28, %.noexc9 ], [ %tmp25, %bb18 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %T_handle, %"class.sc_core::sc_simcontext"* %tmp29, i8* getelementptr inbounds ([2 x i8]* @.str11, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.element*)* @_ZN7element1TEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp8, %"class.sc_core::sc_spawn_options"* null)
  %tmp30 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %T_handle, i64 0, i32 0
  %tmp31 = load %"class.sc_core::sc_process_b"** %tmp30, align 8, !tbaa !4
  %tmp32 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp31, %"class.sc_core::sc_process_b"** %tmp32, align 8, !tbaa !4
  %tmp33 = icmp eq %"class.sc_core::sc_process_b"* %tmp31, null
  br i1 %tmp33, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit, label %bb34

bb34:                                             ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp35 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp31, i64 0, i32 15
  %tmp36 = load i32* %tmp35, align 4, !tbaa !0
  %tmp37 = icmp eq i32 %tmp36, 0
  br i1 %tmp37, label %bb38, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i

bb38:                                             ; preds = %bb34
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i: ; preds = %bb34
  %tmp39 = add nsw i32 %tmp36, 1
  store i32 %tmp39, i32* %tmp35, align 4, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit

_ZN7sc_core17sc_process_handleC1ERKS0_.exit:      ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i, %_ZN7sc_core22sc_get_curr_simcontextEv.exit
  %tmp40 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 2
  %tmp41 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp40, %"class.sc_core::sc_process_handle"* %tmp)
  %tmp42 = load %"class.sc_core::sc_process_b"** %tmp32, align 8, !tbaa !4
  %tmp43 = icmp eq %"class.sc_core::sc_process_b"* %tmp42, null
  br i1 %tmp43, label %_ZN7sc_core17sc_process_handleD1Ev.exit, label %bb44

bb44:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp45 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp42, i64 0, i32 15
  %tmp46 = load i32* %tmp45, align 4, !tbaa !0
  %tmp47 = add nsw i32 %tmp46, -1
  store i32 %tmp47, i32* %tmp45, align 4, !tbaa !0
  %tmp48 = icmp eq i32 %tmp47, 0
  br i1 %tmp48, label %bb49, label %_ZN7sc_core17sc_process_handleD1Ev.exit

bb49:                                             ; preds = %bb44
  %tmp50 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp51 = icmp eq %"class.sc_core::sc_process_b"* %tmp50, null
  br i1 %tmp51, label %bb56, label %.noexc12

.noexc12:                                         ; preds = %bb49
  %tmp52 = bitcast %"class.sc_core::sc_process_b"* %tmp50 to void (%"class.sc_core::sc_process_b"*)***
  %tmp53 = load void (%"class.sc_core::sc_process_b"*)*** %tmp52, align 8, !tbaa !3
  %tmp54 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp53, i64 6
  %tmp55 = load void (%"class.sc_core::sc_process_b"*)** %tmp54, align 8
  call void %tmp55(%"class.sc_core::sc_process_b"* %tmp50)
  %.pre.i.i.i = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb56

bb56:                                             ; preds = %.noexc12, %bb49
  %tmp57 = phi %"class.sc_core::sc_process_b"* [ null, %bb49 ], [ %.pre.i.i.i, %.noexc12 ]
  %tmp58 = icmp eq %"class.sc_core::sc_process_b"* %tmp57, %tmp42
  br i1 %tmp58, label %bb59, label %bb60

bb59:                                             ; preds = %bb56
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb60:                                             ; preds = %bb56
  store %"class.sc_core::sc_process_b"* %tmp42, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit

_ZN7sc_core17sc_process_handleD1Ev.exit:          ; preds = %bb60, %bb44, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit
  %tmp61 = load %"class.sc_core::sc_process_b"** %tmp30, align 8, !tbaa !4
  %tmp62 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp1, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp61, %"class.sc_core::sc_process_b"** %tmp62, align 8, !tbaa !4
  %tmp63 = icmp eq %"class.sc_core::sc_process_b"* %tmp61, null
  br i1 %tmp63, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14, label %bb64

bb64:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp65 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp61, i64 0, i32 15
  %tmp66 = load i32* %tmp65, align 4, !tbaa !0
  %tmp67 = icmp eq i32 %tmp66, 0
  br i1 %tmp67, label %bb68, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13

bb68:                                             ; preds = %bb64
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13: ; preds = %bb64
  %tmp69 = add nsw i32 %tmp66, 1
  store i32 %tmp69, i32* %tmp65, align 4, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14

_ZN7sc_core17sc_process_handleC1ERKS0_.exit14:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i13, %_ZN7sc_core17sc_process_handleD1Ev.exit
  %tmp70 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 3
  %tmp71 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp70, %"class.sc_core::sc_process_handle"* %tmp1)
  %tmp72 = load %"class.sc_core::sc_process_b"** %tmp62, align 8, !tbaa !4
  %tmp73 = icmp eq %"class.sc_core::sc_process_b"* %tmp72, null
  br i1 %tmp73, label %_ZN7sc_core17sc_process_handleD1Ev.exit17, label %bb74

bb74:                                             ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14
  %tmp75 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp72, i64 0, i32 15
  %tmp76 = load i32* %tmp75, align 4, !tbaa !0
  %tmp77 = add nsw i32 %tmp76, -1
  store i32 %tmp77, i32* %tmp75, align 4, !tbaa !0
  %tmp78 = icmp eq i32 %tmp77, 0
  br i1 %tmp78, label %bb79, label %_ZN7sc_core17sc_process_handleD1Ev.exit17

bb79:                                             ; preds = %bb74
  %tmp80 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp81 = icmp eq %"class.sc_core::sc_process_b"* %tmp80, null
  br i1 %tmp81, label %bb86, label %.noexc16

.noexc16:                                         ; preds = %bb79
  %tmp82 = bitcast %"class.sc_core::sc_process_b"* %tmp80 to void (%"class.sc_core::sc_process_b"*)***
  %tmp83 = load void (%"class.sc_core::sc_process_b"*)*** %tmp82, align 8, !tbaa !3
  %tmp84 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp83, i64 6
  %tmp85 = load void (%"class.sc_core::sc_process_b"*)** %tmp84, align 8
  call void %tmp85(%"class.sc_core::sc_process_b"* %tmp80)
  %.pre.i.i.i15 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb86

bb86:                                             ; preds = %.noexc16, %bb79
  %tmp87 = phi %"class.sc_core::sc_process_b"* [ null, %bb79 ], [ %.pre.i.i.i15, %.noexc16 ]
  %tmp88 = icmp eq %"class.sc_core::sc_process_b"* %tmp87, %tmp72
  br i1 %tmp88, label %bb89, label %bb90

bb89:                                             ; preds = %bb86
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb90:                                             ; preds = %bb86
  store %"class.sc_core::sc_process_b"* %tmp72, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit17

_ZN7sc_core17sc_process_handleD1Ev.exit17:        ; preds = %bb90, %bb74, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit14
  %tmp91 = load %"class.sc_core::sc_process_b"** %tmp30, align 8, !tbaa !4
  %tmp92 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp2, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp91, %"class.sc_core::sc_process_b"** %tmp92, align 8, !tbaa !4
  %tmp93 = icmp eq %"class.sc_core::sc_process_b"* %tmp91, null
  br i1 %tmp93, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19, label %bb94

bb94:                                             ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit17
  %tmp95 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp91, i64 0, i32 15
  %tmp96 = load i32* %tmp95, align 4, !tbaa !0
  %tmp97 = icmp eq i32 %tmp96, 0
  br i1 %tmp97, label %bb98, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18

bb98:                                             ; preds = %bb94
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18: ; preds = %bb94
  %tmp99 = add nsw i32 %tmp96, 1
  store i32 %tmp99, i32* %tmp95, align 4, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19

_ZN7sc_core17sc_process_handleC1ERKS0_.exit19:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i18, %_ZN7sc_core17sc_process_handleD1Ev.exit17
  %tmp100 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 4
  %tmp101 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp100, %"class.sc_core::sc_process_handle"* %tmp2)
  %tmp102 = load %"class.sc_core::sc_process_b"** %tmp92, align 8, !tbaa !4
  %tmp103 = icmp eq %"class.sc_core::sc_process_b"* %tmp102, null
  br i1 %tmp103, label %_ZN7sc_core17sc_process_handleD1Ev.exit22, label %bb104

bb104:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19
  %tmp105 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp102, i64 0, i32 15
  %tmp106 = load i32* %tmp105, align 4, !tbaa !0
  %tmp107 = add nsw i32 %tmp106, -1
  store i32 %tmp107, i32* %tmp105, align 4, !tbaa !0
  %tmp108 = icmp eq i32 %tmp107, 0
  br i1 %tmp108, label %bb109, label %_ZN7sc_core17sc_process_handleD1Ev.exit22

bb109:                                            ; preds = %bb104
  %tmp110 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp111 = icmp eq %"class.sc_core::sc_process_b"* %tmp110, null
  br i1 %tmp111, label %bb116, label %.noexc21

.noexc21:                                         ; preds = %bb109
  %tmp112 = bitcast %"class.sc_core::sc_process_b"* %tmp110 to void (%"class.sc_core::sc_process_b"*)***
  %tmp113 = load void (%"class.sc_core::sc_process_b"*)*** %tmp112, align 8, !tbaa !3
  %tmp114 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp113, i64 6
  %tmp115 = load void (%"class.sc_core::sc_process_b"*)** %tmp114, align 8
  call void %tmp115(%"class.sc_core::sc_process_b"* %tmp110)
  %.pre.i.i.i20 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb116

bb116:                                            ; preds = %.noexc21, %bb109
  %tmp117 = phi %"class.sc_core::sc_process_b"* [ null, %bb109 ], [ %.pre.i.i.i20, %.noexc21 ]
  %tmp118 = icmp eq %"class.sc_core::sc_process_b"* %tmp117, %tmp102
  br i1 %tmp118, label %bb119, label %bb120

bb119:                                            ; preds = %bb116
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb120:                                            ; preds = %bb116
  store %"class.sc_core::sc_process_b"* %tmp102, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit22

_ZN7sc_core17sc_process_handleD1Ev.exit22:        ; preds = %bb120, %bb104, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit19
  %tmp121 = load %"class.sc_core::sc_process_b"** %tmp30, align 8, !tbaa !4
  %tmp122 = icmp eq %"class.sc_core::sc_process_b"* %tmp121, null
  br i1 %tmp122, label %_ZN7sc_core17sc_process_handleD1Ev.exit25, label %bb123

bb123:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit22
  %tmp124 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp121, i64 0, i32 15
  %tmp125 = load i32* %tmp124, align 4, !tbaa !0
  %tmp126 = add nsw i32 %tmp125, -1
  store i32 %tmp126, i32* %tmp124, align 4, !tbaa !0
  %tmp127 = icmp eq i32 %tmp126, 0
  br i1 %tmp127, label %bb128, label %_ZN7sc_core17sc_process_handleD1Ev.exit25

bb128:                                            ; preds = %bb123
  %tmp129 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp130 = icmp eq %"class.sc_core::sc_process_b"* %tmp129, null
  br i1 %tmp130, label %bb135, label %.noexc24

.noexc24:                                         ; preds = %bb128
  %tmp131 = bitcast %"class.sc_core::sc_process_b"* %tmp129 to void (%"class.sc_core::sc_process_b"*)***
  %tmp132 = load void (%"class.sc_core::sc_process_b"*)*** %tmp131, align 8, !tbaa !3
  %tmp133 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp132, i64 6
  %tmp134 = load void (%"class.sc_core::sc_process_b"*)** %tmp133, align 8
  call void %tmp134(%"class.sc_core::sc_process_b"* %tmp129)
  %.pre.i.i.i23 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb135

bb135:                                            ; preds = %.noexc24, %bb128
  %tmp136 = phi %"class.sc_core::sc_process_b"* [ null, %bb128 ], [ %.pre.i.i.i23, %.noexc24 ]
  %tmp137 = icmp eq %"class.sc_core::sc_process_b"* %tmp136, %tmp121
  br i1 %tmp137, label %bb138, label %bb139

bb138:                                            ; preds = %bb135
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb139:                                            ; preds = %bb135
  store %"class.sc_core::sc_process_b"* %tmp121, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit25

_ZN7sc_core17sc_process_handleD1Ev.exit25:        ; preds = %bb139, %bb123, %_ZN7sc_core17sc_process_handleD1Ev.exit22
  %tmp140 = load %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  %tmp141 = icmp eq %"class.sc_core::sc_simcontext"* %tmp140, null
  br i1 %tmp141, label %.noexc26, label %_ZN7sc_core22sc_get_curr_simcontextEv.exit29

.noexc26:                                         ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit25
  %tmp142 = call noalias i8* @_Znwm(i64 248)
  %tmp143 = bitcast i8* %tmp142 to %"class.sc_core::sc_simcontext"*
  call void @_ZN7sc_core13sc_simcontextC1Ev(%"class.sc_core::sc_simcontext"* %tmp143)
  store %"class.sc_core::sc_simcontext"* %tmp143, %"class.sc_core::sc_simcontext"** @_ZN7sc_core25sc_default_global_contextE, align 8, !tbaa !4
  store %"class.sc_core::sc_simcontext"* %tmp143, %"class.sc_core::sc_simcontext"** @_ZN7sc_core18sc_curr_simcontextE, align 8, !tbaa !4
  br label %_ZN7sc_core22sc_get_curr_simcontextEv.exit29

_ZN7sc_core22sc_get_curr_simcontextEv.exit29:     ; preds = %.noexc26, %_ZN7sc_core17sc_process_handleD1Ev.exit25
  %tmp144 = phi %"class.sc_core::sc_simcontext"* [ %tmp143, %.noexc26 ], [ %tmp140, %_ZN7sc_core17sc_process_handleD1Ev.exit25 ]
  call void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret %getmsg_handle, %"class.sc_core::sc_simcontext"* %tmp144, i8* getelementptr inbounds ([7 x i8]* @.str12, i64 0, i64 0), i1 zeroext false, i64 ptrtoint (void (%class.element*)* @_ZN7element6getmsgEv to i64), i64 -40, %"class.sc_core::sc_process_host"* %tmp8, %"class.sc_core::sc_spawn_options"* null)
  %tmp145 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %getmsg_handle, i64 0, i32 0
  %tmp146 = load %"class.sc_core::sc_process_b"** %tmp145, align 8, !tbaa !4
  %tmp147 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp3, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp146, %"class.sc_core::sc_process_b"** %tmp147, align 8, !tbaa !4
  %tmp148 = icmp eq %"class.sc_core::sc_process_b"* %tmp146, null
  br i1 %tmp148, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31, label %bb149

bb149:                                            ; preds = %_ZN7sc_core22sc_get_curr_simcontextEv.exit29
  %tmp150 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp146, i64 0, i32 15
  %tmp151 = load i32* %tmp150, align 4, !tbaa !0
  %tmp152 = icmp eq i32 %tmp151, 0
  br i1 %tmp152, label %bb153, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30

bb153:                                            ; preds = %bb149
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30: ; preds = %bb149
  %tmp154 = add nsw i32 %tmp151, 1
  store i32 %tmp154, i32* %tmp150, align 4, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31

_ZN7sc_core17sc_process_handleC1ERKS0_.exit31:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i30, %_ZN7sc_core22sc_get_curr_simcontextEv.exit29
  %tmp155 = call %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"* %tmp40, %"class.sc_core::sc_process_handle"* %tmp3)
  %tmp156 = load %"class.sc_core::sc_process_b"** %tmp147, align 8, !tbaa !4
  %tmp157 = icmp eq %"class.sc_core::sc_process_b"* %tmp156, null
  br i1 %tmp157, label %_ZN7sc_core17sc_process_handleD1Ev.exit34, label %bb158

bb158:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31
  %tmp159 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp156, i64 0, i32 15
  %tmp160 = load i32* %tmp159, align 4, !tbaa !0
  %tmp161 = add nsw i32 %tmp160, -1
  store i32 %tmp161, i32* %tmp159, align 4, !tbaa !0
  %tmp162 = icmp eq i32 %tmp161, 0
  br i1 %tmp162, label %bb163, label %_ZN7sc_core17sc_process_handleD1Ev.exit34

bb163:                                            ; preds = %bb158
  %tmp164 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp165 = icmp eq %"class.sc_core::sc_process_b"* %tmp164, null
  br i1 %tmp165, label %bb170, label %.noexc33

.noexc33:                                         ; preds = %bb163
  %tmp166 = bitcast %"class.sc_core::sc_process_b"* %tmp164 to void (%"class.sc_core::sc_process_b"*)***
  %tmp167 = load void (%"class.sc_core::sc_process_b"*)*** %tmp166, align 8, !tbaa !3
  %tmp168 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp167, i64 6
  %tmp169 = load void (%"class.sc_core::sc_process_b"*)** %tmp168, align 8
  call void %tmp169(%"class.sc_core::sc_process_b"* %tmp164)
  %.pre.i.i.i32 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb170

bb170:                                            ; preds = %.noexc33, %bb163
  %tmp171 = phi %"class.sc_core::sc_process_b"* [ null, %bb163 ], [ %.pre.i.i.i32, %.noexc33 ]
  %tmp172 = icmp eq %"class.sc_core::sc_process_b"* %tmp171, %tmp156
  br i1 %tmp172, label %bb173, label %bb174

bb173:                                            ; preds = %bb170
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb174:                                            ; preds = %bb170
  store %"class.sc_core::sc_process_b"* %tmp156, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit34

_ZN7sc_core17sc_process_handleD1Ev.exit34:        ; preds = %bb174, %bb158, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit31
  %tmp175 = load %"class.sc_core::sc_process_b"** %tmp145, align 8, !tbaa !4
  %tmp176 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp4, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp175, %"class.sc_core::sc_process_b"** %tmp176, align 8, !tbaa !4
  %tmp177 = icmp eq %"class.sc_core::sc_process_b"* %tmp175, null
  br i1 %tmp177, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36, label %bb178

bb178:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit34
  %tmp179 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp175, i64 0, i32 15
  %tmp180 = load i32* %tmp179, align 4, !tbaa !0
  %tmp181 = icmp eq i32 %tmp180, 0
  br i1 %tmp181, label %bb182, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35

bb182:                                            ; preds = %bb178
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35: ; preds = %bb178
  %tmp183 = add nsw i32 %tmp180, 1
  store i32 %tmp183, i32* %tmp179, align 4, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36

_ZN7sc_core17sc_process_handleC1ERKS0_.exit36:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i35, %_ZN7sc_core17sc_process_handleD1Ev.exit34
  %tmp184 = call %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"* %tmp70, %"class.sc_core::sc_process_handle"* %tmp4)
  %tmp185 = load %"class.sc_core::sc_process_b"** %tmp176, align 8, !tbaa !4
  %tmp186 = icmp eq %"class.sc_core::sc_process_b"* %tmp185, null
  br i1 %tmp186, label %_ZN7sc_core17sc_process_handleD1Ev.exit39, label %bb187

bb187:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36
  %tmp188 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp185, i64 0, i32 15
  %tmp189 = load i32* %tmp188, align 4, !tbaa !0
  %tmp190 = add nsw i32 %tmp189, -1
  store i32 %tmp190, i32* %tmp188, align 4, !tbaa !0
  %tmp191 = icmp eq i32 %tmp190, 0
  br i1 %tmp191, label %bb192, label %_ZN7sc_core17sc_process_handleD1Ev.exit39

bb192:                                            ; preds = %bb187
  %tmp193 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp194 = icmp eq %"class.sc_core::sc_process_b"* %tmp193, null
  br i1 %tmp194, label %bb199, label %.noexc38

.noexc38:                                         ; preds = %bb192
  %tmp195 = bitcast %"class.sc_core::sc_process_b"* %tmp193 to void (%"class.sc_core::sc_process_b"*)***
  %tmp196 = load void (%"class.sc_core::sc_process_b"*)*** %tmp195, align 8, !tbaa !3
  %tmp197 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp196, i64 6
  %tmp198 = load void (%"class.sc_core::sc_process_b"*)** %tmp197, align 8
  call void %tmp198(%"class.sc_core::sc_process_b"* %tmp193)
  %.pre.i.i.i37 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb199

bb199:                                            ; preds = %.noexc38, %bb192
  %tmp200 = phi %"class.sc_core::sc_process_b"* [ null, %bb192 ], [ %.pre.i.i.i37, %.noexc38 ]
  %tmp201 = icmp eq %"class.sc_core::sc_process_b"* %tmp200, %tmp185
  br i1 %tmp201, label %bb202, label %bb203

bb202:                                            ; preds = %bb199
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb203:                                            ; preds = %bb199
  store %"class.sc_core::sc_process_b"* %tmp185, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit39

_ZN7sc_core17sc_process_handleD1Ev.exit39:        ; preds = %bb203, %bb187, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit36
  %tmp204 = load %"class.sc_core::sc_process_b"** %tmp145, align 8, !tbaa !4
  %tmp205 = getelementptr inbounds %"class.sc_core::sc_process_handle"* %tmp5, i64 0, i32 0
  store %"class.sc_core::sc_process_b"* %tmp204, %"class.sc_core::sc_process_b"** %tmp205, align 8, !tbaa !4
  %tmp206 = icmp eq %"class.sc_core::sc_process_b"* %tmp204, null
  br i1 %tmp206, label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41, label %bb207

bb207:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit39
  %tmp208 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp204, i64 0, i32 15
  %tmp209 = load i32* %tmp208, align 4, !tbaa !0
  %tmp210 = icmp eq i32 %tmp209, 0
  br i1 %tmp210, label %bb211, label %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40

bb211:                                            ; preds = %bb207
  call void @__assert_fail(i8* getelementptr inbounds ([20 x i8]* @.str16, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 481, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_incrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40: ; preds = %bb207
  %tmp212 = add nsw i32 %tmp209, 1
  store i32 %tmp212, i32* %tmp208, align 4, !tbaa !0
  br label %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41

_ZN7sc_core17sc_process_handleC1ERKS0_.exit41:    ; preds = %_ZN7sc_core12sc_process_b19reference_incrementEv.exit.i.i40, %_ZN7sc_core17sc_process_handleD1Ev.exit39
  %tmp213 = call %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"* %tmp100, %"class.sc_core::sc_process_handle"* %tmp5)
  %tmp214 = load %"class.sc_core::sc_process_b"** %tmp205, align 8, !tbaa !4
  %tmp215 = icmp eq %"class.sc_core::sc_process_b"* %tmp214, null
  br i1 %tmp215, label %_ZN7sc_core17sc_process_handleD1Ev.exit44, label %bb216

bb216:                                            ; preds = %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41
  %tmp217 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp214, i64 0, i32 15
  %tmp218 = load i32* %tmp217, align 4, !tbaa !0
  %tmp219 = add nsw i32 %tmp218, -1
  store i32 %tmp219, i32* %tmp217, align 4, !tbaa !0
  %tmp220 = icmp eq i32 %tmp219, 0
  br i1 %tmp220, label %bb221, label %_ZN7sc_core17sc_process_handleD1Ev.exit44

bb221:                                            ; preds = %bb216
  %tmp222 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp223 = icmp eq %"class.sc_core::sc_process_b"* %tmp222, null
  br i1 %tmp223, label %bb228, label %.noexc43

.noexc43:                                         ; preds = %bb221
  %tmp224 = bitcast %"class.sc_core::sc_process_b"* %tmp222 to void (%"class.sc_core::sc_process_b"*)***
  %tmp225 = load void (%"class.sc_core::sc_process_b"*)*** %tmp224, align 8, !tbaa !3
  %tmp226 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp225, i64 6
  %tmp227 = load void (%"class.sc_core::sc_process_b"*)** %tmp226, align 8
  call void %tmp227(%"class.sc_core::sc_process_b"* %tmp222)
  %.pre.i.i.i42 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb228

bb228:                                            ; preds = %.noexc43, %bb221
  %tmp229 = phi %"class.sc_core::sc_process_b"* [ null, %bb221 ], [ %.pre.i.i.i42, %.noexc43 ]
  %tmp230 = icmp eq %"class.sc_core::sc_process_b"* %tmp229, %tmp214
  br i1 %tmp230, label %bb231, label %bb232

bb231:                                            ; preds = %bb228
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb232:                                            ; preds = %bb228
  store %"class.sc_core::sc_process_b"* %tmp214, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit44

_ZN7sc_core17sc_process_handleD1Ev.exit44:        ; preds = %bb232, %bb216, %_ZN7sc_core17sc_process_handleC1ERKS0_.exit41
  %tmp233 = load %"class.sc_core::sc_process_b"** %tmp145, align 8, !tbaa !4
  %tmp234 = icmp eq %"class.sc_core::sc_process_b"* %tmp233, null
  br i1 %tmp234, label %_ZN7sc_core17sc_process_handleD1Ev.exit47, label %bb235

bb235:                                            ; preds = %_ZN7sc_core17sc_process_handleD1Ev.exit44
  %tmp236 = getelementptr inbounds %"class.sc_core::sc_process_b"* %tmp233, i64 0, i32 15
  %tmp237 = load i32* %tmp236, align 4, !tbaa !0
  %tmp238 = add nsw i32 %tmp237, -1
  store i32 %tmp238, i32* %tmp236, align 4, !tbaa !0
  %tmp239 = icmp eq i32 %tmp238, 0
  br i1 %tmp239, label %bb240, label %_ZN7sc_core17sc_process_handleD1Ev.exit47

bb240:                                            ; preds = %bb235
  %tmp241 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  %tmp242 = icmp eq %"class.sc_core::sc_process_b"* %tmp241, null
  br i1 %tmp242, label %bb247, label %.noexc46

.noexc46:                                         ; preds = %bb240
  %tmp243 = bitcast %"class.sc_core::sc_process_b"* %tmp241 to void (%"class.sc_core::sc_process_b"*)***
  %tmp244 = load void (%"class.sc_core::sc_process_b"*)*** %tmp243, align 8, !tbaa !3
  %tmp245 = getelementptr inbounds void (%"class.sc_core::sc_process_b"*)** %tmp244, i64 6
  %tmp246 = load void (%"class.sc_core::sc_process_b"*)** %tmp245, align 8
  call void %tmp246(%"class.sc_core::sc_process_b"* %tmp241)
  %.pre.i.i.i45 = load %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %bb247

bb247:                                            ; preds = %.noexc46, %bb240
  %tmp248 = phi %"class.sc_core::sc_process_b"* [ null, %bb240 ], [ %.pre.i.i.i45, %.noexc46 ]
  %tmp249 = icmp eq %"class.sc_core::sc_process_b"* %tmp248, %tmp233
  br i1 %tmp249, label %bb250, label %bb251

bb250:                                            ; preds = %bb247
  call void @__assert_fail(i8* getelementptr inbounds ([24 x i8]* @.str14, i64 0, i64 0), i8* getelementptr inbounds ([44 x i8]* @.str15, i64 0, i64 0), i32 467, i8* getelementptr inbounds ([50 x i8]* @__PRETTY_FUNCTION__._ZN7sc_core12sc_process_b19reference_decrementEv, i64 0, i64 0)) noreturn nounwind
  unreachable

bb251:                                            ; preds = %bb247
  store %"class.sc_core::sc_process_b"* %tmp233, %"class.sc_core::sc_process_b"** @_ZN7sc_core12sc_process_b15m_delete_next_pE, align 8, !tbaa !4
  br label %_ZN7sc_core17sc_process_handleD1Ev.exit47

_ZN7sc_core17sc_process_handleD1Ev.exit47:        ; preds = %bb251, %bb235, %_ZN7sc_core17sc_process_handleD1Ev.exit44
  ret void
}

declare void @_ZN7sc_core9sc_moduleC2ERKNS_14sc_module_nameE(%"class.sc_core::sc_module"*, %"class.sc_core::sc_module_name"*)

declare void @_ZN7sc_core13sc_simcontext21create_thread_processEPKcbMNS_15sc_process_hostEFvvEPS3_PKNS_16sc_spawn_optionsE(%"class.sc_core::sc_process_handle"* sret, %"class.sc_core::sc_simcontext"*, i8*, i1 zeroext, i64, i64, %"class.sc_core::sc_process_host"*, %"class.sc_core::sc_spawn_options"*)

define linkonce_odr void @_ZN7element1TEv(%class.element* %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.element* %this, i64 0, i32 1
  %tmp1 = load i32* %tmp, align 4, !tbaa !0
  %tmp2 = icmp sgt i32 %tmp1, 0
  br i1 %tmp2, label %bb3, label %bb9

bb3:                                              ; preds = %bb
  %tmp4 = alloca %"class.sc_core::sc_time", align 8
  %tmp5 = bitcast %"class.sc_core::sc_time"* %tmp4 to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %tmp5)
  %tmp6 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 1
  %tmp7 = load %"class.sc_core::sc_simcontext"** %tmp6, align 8, !tbaa !4
  call void @_ZN7sc_core7sc_timeC1EdNS_12sc_time_unitEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp4, double 2.000000e+01, i32 2, %"class.sc_core::sc_simcontext"* %tmp7)
  %tmp8 = load %"class.sc_core::sc_simcontext"** %tmp6, align 8, !tbaa !4
  call void @_ZN7sc_core4waitERKNS_7sc_timeEPNS_13sc_simcontextE(%"class.sc_core::sc_time"* %tmp4, %"class.sc_core::sc_simcontext"* %tmp8)
  call void @llvm.lifetime.end(i64 -1, i8* %tmp5)
  %.pre = load i32* %tmp, align 4, !tbaa !0
  br label %bb9

bb9:                                              ; preds = %bb3, %bb
  %tmp10 = phi i32 [ %.pre, %bb3 ], [ %tmp1, %bb ]
  %tmp11 = getelementptr inbounds %class.element* %this, i64 0, i32 2
  %tmp12 = load i32* %tmp11, align 4, !tbaa !0
  %tmp13 = add nsw i32 %tmp12, 1
  %tmp14 = icmp slt i32 %tmp10, %tmp13
  br i1 %tmp14, label %bb15, label %bb30

bb15:                                             ; preds = %bb9
  %tmp16 = getelementptr inbounds %class.element* %this, i64 0, i32 5
  call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp16)
  %tmp17 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 1
  %tmp18 = load %"class.sc_core::sc_simcontext"** %tmp17, align 8, !tbaa !4
  call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp16, %"class.sc_core::sc_simcontext"* %tmp18)
  %tmp19 = getelementptr inbounds %class.element* %this, i64 0, i32 3
  %tmp20 = load i32* %tmp19, align 4, !tbaa !0
  %tmp21 = mul nsw i32 %tmp20, 7
  %tmp22 = getelementptr inbounds %class.element* %this, i64 0, i32 4
  br label %bb23

bb23:                                             ; preds = %bb23, %bb15
  %storemerge.in = phi i32 [ %tmp21, %bb15 ], [ %tmp28, %bb23 ]
  %storemerge = srem i32 %storemerge.in, 128
  store i32 %storemerge, i32* %tmp22, align 4
  %tmp24 = sext i32 %storemerge to i64
  %tmp25 = getelementptr inbounds [128 x i32]* @table, i64 0, i64 %tmp24
  %tmp26 = load i32* %tmp25, align 4, !tbaa !0
  %tmp27 = icmp eq i32 %tmp26, 0
  %tmp28 = add nsw i32 %storemerge, 1
  br i1 %tmp27, label %bb29, label %bb23

bb29:                                             ; preds = %bb23
  store i32 %tmp20, i32* %tmp25, align 4, !tbaa !0
  br label %bb30

bb30:                                             ; preds = %bb29, %bb9
  ret void
}

declare %"class.sc_core::sc_sensitive"* @_ZN7sc_core12sc_sensitivelsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_pos"* @_ZN7sc_core16sc_sensitive_poslsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_pos"*, %"class.sc_core::sc_process_handle"*)

declare %"class.sc_core::sc_sensitive_neg"* @_ZN7sc_core16sc_sensitive_neglsENS_17sc_process_handleE(%"class.sc_core::sc_sensitive_neg"*, %"class.sc_core::sc_process_handle"*)

define linkonce_odr void @_ZN7element6getmsgEv(%class.element* %this) uwtable align 2 {
bb:
  %tmp = getelementptr inbounds %class.element* %this, i64 0, i32 1
  %tmp1 = load i32* %tmp, align 4, !tbaa !0
  %tmp2 = getelementptr inbounds %class.element* %this, i64 0, i32 2
  %tmp3 = load i32* %tmp2, align 4, !tbaa !0
  %tmp4 = icmp slt i32 %tmp1, %tmp3
  br i1 %tmp4, label %bb5, label %bb14

bb5:                                              ; preds = %bb
  %tmp6 = getelementptr inbounds %class.element* %this, i64 0, i32 5
  %tmp7 = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 1
  %tmp8 = load %"class.sc_core::sc_simcontext"** %tmp7, align 8, !tbaa !4
  tail call void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"* %tmp6, %"class.sc_core::sc_simcontext"* %tmp8)
  %tmp9 = load i32* %tmp, align 4, !tbaa !0
  %tmp10 = add nsw i32 %tmp9, 1
  store i32 %tmp10, i32* %tmp, align 4, !tbaa !0
  %tmp11 = mul nsw i32 %tmp10, 11
  %tmp12 = add nsw i32 %tmp11, 5
  %tmp13 = getelementptr inbounds %class.element* %this, i64 0, i32 3
  store i32 %tmp12, i32* %tmp13, align 4, !tbaa !0
  tail call void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"* %tmp6)
  br label %bb14

bb14:                                             ; preds = %bb5, %bb
  ret void
}

declare void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"*)

declare void @_ZNK7sc_core9sc_object5printERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object4dumpERSo(%"class.sc_core::sc_object"*, %"class.std::basic_ostream"*)

declare void @_ZNK7sc_core9sc_object5traceEPNS_13sc_trace_fileE(%"class.sc_core::sc_object"*, %"class.sc_core::sc_trace_file"*)

define linkonce_odr i8* @_ZNK7sc_core9sc_module4kindEv(%"class.sc_core::sc_module"* nocapture %this) nounwind uwtable readnone align 2 {
bb:
  ret i8* getelementptr inbounds ([10 x i8]* @.str13, i64 0, i64 0)
}

declare %"class.std::vector.10"* @_ZNK7sc_core9sc_module17get_child_objectsEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZN7elementD1Ev(%class.element* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7elementD2Ev.exit:
  %tmp = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %class.element* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %class.element* %this, i64 0, i32 5
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp2)
  %tmp3 = getelementptr inbounds %class.element* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  ret void
}

define linkonce_odr void @_ZN7elementD0Ev(%class.element* %this) unnamed_addr uwtable inlinehint align 2 {
_ZN7elementD2Ev.exit.i:
  %tmp = getelementptr inbounds %class.element* %this, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to i32 (...)**), i32 (...)*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %class.element* %this, i64 0, i32 0, i32 1, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to i32 (...)**), i32 (...)*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %class.element* %this, i64 0, i32 5
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp2)
  %tmp3 = getelementptr inbounds %class.element* %this, i64 0, i32 0
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp3)
  %tmp4 = bitcast %class.element* %this to i8*
  tail call void @_ZdlPv(i8* %tmp4) nounwind
  ret void
}

declare void @_ZN7sc_core9sc_module25before_end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module18end_of_elaborationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module19start_of_simulationEv(%"class.sc_core::sc_module"*)

declare void @_ZN7sc_core9sc_module17end_of_simulationEv(%"class.sc_core::sc_module"*)

define linkonce_odr void @_ZThn40_N7elementD1Ev(%class.element* %this) {
_ZN7elementD1Ev.exit:
  %tmp = getelementptr inbounds %class.element* %this, i64 -1, i32 5, i32 6, i32 0, i32 0, i32 1
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 25
  %tmp3 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  ret void
}

define linkonce_odr void @_ZThn40_N7elementD0Ev(%class.element* %this) {
_ZN7elementD2Ev.exit.i.i:
  %tmp = getelementptr inbounds %class.element* %this, i64 -1, i32 5, i32 6, i32 0, i32 0, i32 1
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 2) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp, align 8, !tbaa !3
  %tmp1 = getelementptr %"class.sc_core::sc_thread_process"*** %tmp, i64 5
  store %"class.sc_core::sc_thread_process"** bitcast (i8** getelementptr inbounds ([17 x i8*]* @_ZTV7element, i64 0, i64 15) to %"class.sc_core::sc_thread_process"**), %"class.sc_core::sc_thread_process"*** %tmp1, align 8, !tbaa !3
  %tmp2 = getelementptr inbounds %"class.sc_core::sc_thread_process"*** %tmp, i64 25
  %tmp3 = bitcast %"class.sc_core::sc_thread_process"*** %tmp2 to %"class.sc_core::sc_event"*
  tail call void @_ZN7sc_core8sc_eventD2Ev(%"class.sc_core::sc_event"* %tmp3)
  %tmp4 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to %"class.sc_core::sc_module"*
  tail call void @_ZN7sc_core9sc_moduleD2Ev(%"class.sc_core::sc_module"* %tmp4)
  %tmp5 = bitcast %"class.sc_core::sc_thread_process"*** %tmp to i8*
  tail call void @_ZdlPv(i8* %tmp5) nounwind
  ret void
}

declare void @_ZN7sc_core8sc_event6notifyEv(%"class.sc_core::sc_event"*)

declare void @_ZN7sc_core4waitERKNS_8sc_eventEPNS_13sc_simcontextE(%"class.sc_core::sc_event"*, %"class.sc_core::sc_simcontext"*)

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

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

!0 = metadata !{metadata !"int", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA"}
!3 = metadata !{metadata !"vtable pointer", metadata !2}
!4 = metadata !{metadata !"any pointer", metadata !1}
!5 = metadata !{metadata !"_ZTSN7sc_core8sc_event8notify_tE", metadata !1}
