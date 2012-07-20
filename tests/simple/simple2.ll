; ModuleID = 'simple2.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [6 x i8] c"c < 1\00", align 1
@.str1 = private unnamed_addr constant [9 x i8] c"simple.c\00", align 1
@__PRETTY_FUNCTION__.main = private unnamed_addr constant [23 x i8] c"int main(int, char **)\00", align 1

define i32 @main(i32 %argc, i8** %argv) nounwind uwtable {
bb:
  %tmp = alloca i32, align 4
  %tmp1 = alloca i32, align 4
  %tmp2 = alloca i8**, align 8
  %c = alloca i32, align 4
  store i32 0, i32* %tmp
  store i32 %argc, i32* %tmp1, align 4
  store i8** %argv, i8*** %tmp2, align 8
  store i32 1, i32* %c, align 4
  %tmp3 = load i32* %c, align 4
  %tmp4 = icmp slt i32 %tmp3, 1
  br i1 %tmp4, label %bb5, label %bb6

bb5:                                              ; preds = %bb
  br label %bb8

bb6:                                              ; preds = %bb
  call void @__assert_fail(i8* getelementptr inbounds ([6 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8]* @.str1, i32 0, i32 0), i32 5, i8* getelementptr inbounds ([23 x i8]* @__PRETTY_FUNCTION__.main, i32 0, i32 0)) noreturn nounwind
  unreachable

bb7:                                              ; No predecessors!
  br label %bb8

bb8:                                              ; preds = %bb7, %bb5
  ret i32 0
}

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind
