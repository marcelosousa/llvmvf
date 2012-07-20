; ModuleID = 'simple.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [6 x i8] c"c < 1\00", align 1
@.str1 = private unnamed_addr constant [9 x i8] c"simple.c\00", align 1
@__PRETTY_FUNCTION__.main = private unnamed_addr constant [23 x i8] c"int main(int, char **)\00", align 1

define i32 @main(i32 %argc, i8** %argv) nounwind uwtable {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %c = alloca i32, align 4
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  store i32 1, i32* %c, align 4
  %4 = load i32* %c, align 4
  %5 = icmp slt i32 %4, 1
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  br label %9

; <label>:7                                       ; preds = %0
  call void @__assert_fail(i8* getelementptr inbounds ([6 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8]* @.str1, i32 0, i32 0), i32 5, i8* getelementptr inbounds ([23 x i8]* @__PRETTY_FUNCTION__.main, i32 0, i32 0)) noreturn nounwind
  unreachable
                                                  ; No predecessors!
  br label %9

; <label>:9                                       ; preds = %8, %6
  ret i32 0
}

declare void @__assert_fail(i8*, i8*, i32, i8*) noreturn nounwind
; ModuleID ='simple.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant , align 1
@.str1 = private unnamed_addr constant , align 1
@__PRETTY_FUNCTION__.main = private unnamed_addr constant , align 1

define external TyUnsupported @main(TyUnsupported %argc, opaque %argv){
; <label>:0x00000000038eb050
% = alloca TyUnsupported, align 4
% = alloca TyUnsupported, align 4
% = alloca opaque, align 8
%c = alloca TyUnsupported, align 4
store
store
store
store
load
call
br i1 , label , label 

; <label>:0x00000000038eb510
br 

; <label>:0x00000000038eb570
userop2
unreachable

; <label>:0x00000000038ebe00
br 

; <label>:0x00000000038ebe60
ret undef


}
declare external void @__assert_fail(opaque, opaque, TyUnsupported, opaque)

