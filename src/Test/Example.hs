module Test.Example where

import Language.LLVMIR 
import Data.Map


s :: Module
s = Module {id_Module_Module = "tn.bc", 
            layout_Module_Module = DataLayout {s_DataLayout_DataLayout = 
            	   ["e","p:64:64:64","i1:8:8","i8:8:8","i16:16:16","i32:32:32","i64:64:64","f32:32:32","f64:64:64","v64:64:64","v128:128:128","a0:0:64","s0:64:64","f80:128:128","n8:16:32:64","S128"]}, target_Module_Module = TargetData {s_TargetData_TargetData = "x86_64-unknown-linux-gnu", t_TargetData_TargetData = Linux}, 
            	   gvars_Module_Module = [], 
            	   funs_Module_Module = fromList [(Global {name_Identifier_Global = "main"},
            	   	FunctionDef {name_Function_FunctionDef = Global {name_Identifier_Global = "main"}, 
            	   	linkage_Function_FunctionDef = ExternalLinkage, 
            	   	retty_Function_FunctionDef = TyInt {p_Type_TyInt = 32}, 
            	   	isVar_Function_FunctionDef = False, 
            	   	params_Function_FunctionDef = [], 
            	   	body_Function_FunctionDef = [BasicBlock {label_BasicBlock_BasicBlock = Local {name_Identifier_Local = "bb"}, 
            	   									phis_BasicBlock_BasicBlock = [], 
            	   									instrs_BasicBlock_BasicBlock = [], 
            	   									tmn_BasicBlock_BasicBlock = 
            	   										Ret {pc_Terminator_Ret = 1, r_Terminator_Ret = ValueRet {v_RetInst_ValueRet = Constant {c_Value_Constant = SmpConst {sc_Constant_SmpConst = ConstantInt {iv_SimpleConstant_ConstantInt = 0, ty_SimpleConstant_ConstantInt = TyInt {p_Type_TyInt = 32}}}}}}}]})], 
            	    nmdtys_Module_Module = fromList []}