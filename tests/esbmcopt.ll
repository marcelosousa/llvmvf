; ModuleID = 'esbmcopt.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%union.pthread_attr_t = type { i64, [48 x i8] }

@j = global i32 1, align 4
@x = global i32 2, align 4
@i = common global i32 0, align 4
@.str = private unnamed_addr constant [13 x i8] c"i>=0 && i<10\00", align 1
@.str1 = private unnamed_addr constant [8 x i8] c"esbmc.c\00", align 1
@__PRETTY_FUNCTION__.Tx = private unnamed_addr constant [17 x i8] c"void *Tx(void *)\00", align 1
@a = common global [10 x i32] zeroinitializer, align 16

define noalias i8* @Tx(i8* nocapture %arg) noreturn nounwind uwtable {
bb:
  %tmp = load i32* @x, align 4, !tbaa !0
  %tmp1 = icmp sgt i32 %tmp, 2
  br i1 %tmp1, label %bb2, label %bb11

bb2:                                              ; preds = %bb
  %tmp3 = load i32* @i, align 4, !tbaa !0
  %tmp4 = icmp ult i32 %tmp3, 10
  br i1 %tmp4, label %bb6, label %bb5

bb5:                                              ; preds = %bb2
  tail call void @__assert_fail(i8* getelementptr inbounds ([13 x i8]* @.str, i64 0, i64 0), i8* getelementptr inbounds ([8 x i8]* @.str1, i64 0, i64 0), i32 21, i8* getelementptr inbounds ([17 x i8]* @__PRETTY_FUNCTION__.Tx, i64 0, i64 0)) noreturn nounwind
  unreachable

bb6:                                              ; preds = %bb2
  %tmp7 = bitcast i8* %arg to i32*
  %tmp8 = load i32* %tmp7, align 4, !tbaa !0
  %tmp9 = sext i32 %tmp3 to i64
  %tmp10 = getelementptr inbounds [10 x i32]* @a, i64 0, i64 %tmp9
  store i32 %tmp8, i32* %tmp10, align 4, !tbaa !0
  br label %bb11

bb11:                                             ; preds = %bb6, %bb
  tail call void @pthread_exit(i8* null) noreturn nounwind
  unreachable
}

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind

declare void @pthread_exit(i8*) noreturn

define noalias i8* @Ty(i8* nocapture %arg) noreturn nounwind uwtable {
bb:
  %tmp = load i32* @x, align 4, !tbaa !0
  %tmp1 = icmp sgt i32 %tmp, 3
  br i1 %tmp1, label %bb2, label %bb8

bb2:                                              ; preds = %bb
  %tmp3 = bitcast i8* %arg to i32*
  %tmp4 = load i32* %tmp3, align 4, !tbaa !0
  %tmp5 = load i32* @j, align 4, !tbaa !0
  %tmp6 = sext i32 %tmp5 to i64
  %tmp7 = getelementptr inbounds [10 x i32]* @a, i64 0, i64 %tmp6
  store i32 %tmp4, i32* %tmp7, align 4, !tbaa !0
  br label %bb9

bb8:                                              ; preds = %bb
  store i32 3, i32* @x, align 4, !tbaa !0
  br label %bb9

bb9:                                              ; preds = %bb8, %bb2
  tail call void @pthread_exit(i8* null) noreturn nounwind
  unreachable
}

define i32 @main() nounwind uwtable {
bb:
  %id1 = alloca i64, align 8
  %id2 = alloca i64, align 8
  %arg1 = alloca i32, align 4
  %arg2 = alloca i32, align 4
  store i32 10, i32* %arg1, align 4, !tbaa !0
  store i32 20, i32* %arg2, align 4, !tbaa !0
  %tmp = call i32 (...)* @nondet_uint() nounwind
  %tmp1 = add nsw i32 %tmp, 1
  store i32 %tmp1, i32* @i, align 4, !tbaa !0
  %tmp2 = bitcast i32* %arg1 to i8*
  %tmp3 = call i32 @pthread_create(i64* %id1, %union.pthread_attr_t* null, i8* (i8*)* @Tx, i8* %tmp2) nounwind
  %tmp4 = bitcast i32* %arg2 to i8*
  %tmp5 = call i32 @pthread_create(i64* %id2, %union.pthread_attr_t* null, i8* (i8*)* @Ty, i8* %tmp4) nounwind
  %tmp6 = load i64* %id1, align 8, !tbaa !3
  %tmp7 = call i32 @pthread_join(i64 %tmp6, i8** null) nounwind
  %tmp8 = load i64* %id2, align 8, !tbaa !3
  %tmp9 = call i32 @pthread_join(i64 %tmp8, i8** null) nounwind
  ret i32 0
}

declare i32 @nondet_uint(...)

declare i32 @pthread_create(i64*, %union.pthread_attr_t*, i8* (i8*)*, i8*) nounwind

declare i32 @pthread_join(i64, i8**)

!0 = metadata !{metadata !"int", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA"}
!3 = metadata !{metadata !"long", metadata !1}
