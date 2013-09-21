{-# LANGUAGE UnicodeSyntax #-}
module Test.Example where

import Language.LLVMIR 
import Data.Map

fieldsensitive ∷ Module
fieldsensitive = Module {id_Module_Module = "field-sensitiveo.bc", layout_Module_Module = DataLayout {s_DataLayout_DataLayout = ["e","p:64:64:64","i1:8:8","i8:8:8","i16:16:16","i32:32:32","i64:64:64","f32:32:32","f64:64:64","v64:64:64","v128:128:128","a0:0:64","s0:64:64","f80:128:128","n8:16:32:64","S128"]}, target_Module_Module = TargetData {s_TargetData_TargetData = "x86_64-apple-macosx10.7.4", t_TargetData_TargetData = MacOs}, gvars_Module_Module = [], funs_Module_Module = fromList [(Global {name_Identifier_Global = "f"},FunctionDef {name_Function_FunctionDef = Global {name_Identifier_Global = "f"}, linkage_Function_FunctionDef = ExternalLinkage, retty_Function_FunctionDef = TyPointer {ty_Type_TyPointer = TyStruct {name_Type_TyStruct = "struct.Device", numEl_Type_TyStruct = 1, tys_Type_TyStruct = []}}, isVar_Function_FunctionDef = False, params_Function_FunctionDef = [Parameter {var_Parameter_Parameter = Local {name_Identifier_Local = "x"}, ty_Parameter_Parameter = TyPointer {ty_Type_TyPointer = TyStruct {name_Type_TyStruct = "struct.Device", numEl_Type_TyStruct = 1, tys_Type_TyStruct = []}}}], body_Function_FunctionDef = [BasicBlock {label_BasicBlock_BasicBlock = Local {name_Identifier_Local = "bb"}, phis_BasicBlock_BasicBlock = [], instrs_BasicBlock_BasicBlock = [Call {pc_Instruction_Call = 1, mres_Instruction_Call = Local {name_Identifier_Local = "tmp"}, ty_Instruction_Call = TyPointer {ty_Type_TyPointer = TyInt {p_Type_TyInt = 8}}, callee_Instruction_Call = Global {name_Identifier_Global = "ioremap"}, args_Instruction_Call = [Constant {c_Value_Constant = SmpConst {sc_Constant_SmpConst = ConstantInt {iv_SimpleConstant_ConstantInt = 1024, ty_SimpleConstant_ConstantInt = TyInt {p_Type_TyInt = 64}}}},Constant {c_Value_Constant = SmpConst {sc_Constant_SmpConst = ConstantInt {iv_SimpleConstant_ConstantInt = 512, ty_SimpleConstant_ConstantInt = TyInt {p_Type_TyInt = 64}}}}]},GetElementPtr {pc_Instruction_GetElementPtr = 2, id_Instruction_GetElementPtr = Local {name_Identifier_Local = "tmp1"}, ty_Instruction_GetElementPtr = TyPointer {ty_Type_TyPointer = TyPointer {ty_Type_TyPointer = TyInt {p_Type_TyInt = 8}}}, struct_Instruction_GetElementPtr = Id {v_Value_Id = Local {name_Identifier_Local = "x"}, ty_Value_Id = TyPointer {ty_Type_TyPointer = TyStruct {name_Type_TyStruct = "struct.Device", numEl_Type_TyStruct = 1, tys_Type_TyStruct = []}}}, idxs_Instruction_GetElementPtr = [Constant {c_Value_Constant = SmpConst {sc_Constant_SmpConst = ConstantInt {iv_SimpleConstant_ConstantInt = 0, ty_SimpleConstant_ConstantInt = TyInt {p_Type_TyInt = 64}}}},Constant {c_Value_Constant = SmpConst {sc_Constant_SmpConst = ConstantInt {iv_SimpleConstant_ConstantInt = 0, ty_SimpleConstant_ConstantInt = TyInt {p_Type_TyInt = 32}}}}]},Store {pc_Instruction_Store = 3, ty_Instruction_Store = TyVoid, v1_Instruction_Store = Id {v_Value_Id = Local {name_Identifier_Local = "tmp"}, ty_Value_Id = TyPointer {ty_Type_TyPointer = TyInt {p_Type_TyInt = 8}}}, v2_Instruction_Store = Id {v_Value_Id = Local {name_Identifier_Local = "tmp1"}, ty_Value_Id = TyPointer {ty_Type_TyPointer = TyPointer {ty_Type_TyPointer = TyInt {p_Type_TyInt = 8}}}}, align_Instruction_Store = Align {n_Align_Align = 8}}], tmn_BasicBlock_BasicBlock = Ret {pc_Terminator_Ret = 4, r_Terminator_Ret = ValueRet {v_RetInst_ValueRet = Id {v_Value_Id = Local {name_Identifier_Local = "x"}, ty_Value_Id = TyPointer {ty_Type_TyPointer = TyStruct {name_Type_TyStruct = "struct.Device", numEl_Type_TyStruct = 1, tys_Type_TyStruct = []}}}}}}]}),(Global {name_Identifier_Global = "ioremap"},FunctionDecl {name_Function_FunctionDecl = Global {name_Identifier_Global = "ioremap"}, linkage_Function_FunctionDecl = ExternalLinkage, retty_Function_FunctionDecl = TyPointer {ty_Type_TyPointer = TyInt {p_Type_TyInt = 8}}, isVar_Function_FunctionDecl = False, params_Function_FunctionDecl = [Parameter {var_Parameter_Parameter = Local {name_Identifier_Local = "0x0000000107604f70"}, ty_Parameter_Parameter = TyInt {p_Type_TyInt = 64}},Parameter {var_Parameter_Parameter = Local {name_Identifier_Local = "0x0000000107604780"}, ty_Parameter_Parameter = TyInt {p_Type_TyInt = 64}}]})], nmdtys_Module_Module = fromList [("struct.Device",TyStruct {name_Type_TyStruct = "struct.Device", numEl_Type_TyStruct = 1, tys_Type_TyStruct = [TyPointer {ty_Type_TyPointer = TyInt {p_Type_TyInt = 8}}]})]}
