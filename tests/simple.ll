; ModuleID = 'simple.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin11.4"

@__func__.1469 = internal constant [5 x i8] c"main\00", align 8
@.str = private constant [9 x i8] c"simple.c\00", align 1
@.str1 = private constant [6 x i8] c"c < 1\00", align 1

define i32 @main(i32 %argc, i8** %argv) nounwind ssp {
entry:
  %argc_addr = alloca i32, align 4
  %argv_addr = alloca i8**, align 8
  %retval = alloca i32
  %0 = alloca i32
  %c = alloca i32
  %"alloca point" = bitcast i32 0 to i32
  store i32 %argc, i32* %argc_addr
  store i8** %argv, i8*** %argv_addr
  store i32 1, i32* %c, align 4
  %1 = load i32* %c, align 4
  %2 = icmp sgt i32 %1, 0
  %3 = zext i1 %2 to i64
  %4 = icmp ne i64 %3, 0
  br i1 %4, label %bb, label %bb1

bb:                                               ; preds = %entry
  call void @__assert_rtn(i8* getelementptr inbounds ([5 x i8]* @__func__.1469, i64 0, i64 0), i8* getelementptr inbounds ([9 x i8]* @.str, i64 0, i64 0), i32 5, i8* getelementptr inbounds ([6 x i8]* @.str1, i64 0, i64 0)) noreturn nounwind
  unreachable

bb1:                                              ; preds = %entry
  store i32 0, i32* %0, align 4
  %5 = load i32* %0, align 4
  store i32 %5, i32* %retval, align 4
  br label %return

return:                                           ; preds = %bb1
  %retval2 = load i32* %retval
  ret i32 %retval2
}

declare void @__assert_rtn(i8*, i8*, i32, i8*) noreturn
