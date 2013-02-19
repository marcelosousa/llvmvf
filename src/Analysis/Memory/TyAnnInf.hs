-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TyAnnInf
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.TyAnnInf where

import Analysis.Memory.TyAnn (TyAnn)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.TypingAnn
import Language.LLVMIR
import qualified Data.Map as M

-- Module TyAnn Inference
modTyInf :: Module -> ([TyAnn], M.Map String (TyAnn, TyAnnEnv))
modTyInf (Module i l t gvs fns nmdtys) = let (vtys, tye) = gLstTyInf M.empty gvarTyInf gvs
                                         in (vtys, M.map (fnTyInf tye) fns)


-- Global TyAnn Inference
gvarTyInf :: TyAnnEnv -> Global -> (TyAnn, TyAnnEnv)
gvarTyInf tyenv (GlobalVar i l isConst isUAddr ty iconst align) = 
  let ta = liftTy ty
  in case iconst of
      Nothing -> (ta, tyenv)
      Just c  -> let (t,te) = constTyInf tyenv c
                 in  (castTy t ta, te) 
 
-- Function TyAnn Inference
fnTyInf :: TyAnnEnv -> Function -> (TyAnn, TyAnnEnv)
fnTyInf tye (FunctionDef  n l rty pms bbs) = let (ptys, tyex) = gLstTyInf tye parTyInf pms
                                                 rtyr = liftTy rty
                                                 (bbtyr, tyenv) = bbsTyInf tyex bbs
                                                 tyr = T.TyDer $ T.TyFun ptys bbtyr -- TODO 
                                                 tyey = M.insert n tyr tyenv
                                             in (tyr, M.insert n tyr tyex)
fnTyInf tye (FunctionDecl n l rty pms)     = let (ptys, tyex) = gLstTyInf tye parTyInf pms
                                                 tyr = T.TyDer $ T.TyFun ptys [liftTy rty]
                                             in (tyr, M.insert n tyr tyex)

bbsTyInf :: TyAnnEnv -> BasicBlocks -> ([TyAnn], TyAnnEnv)
bbsTyInf tye bbs = undefined

-- Basic Block TyAnn Inference
bbTyInf :: TyAnnEnv -> BasicBlock -> (TyAnn, TyAnnEnv)
bbTyInf tyenv (BasicBlock l instr) = undefined

ityinf :: TyAnnEnv -> Instruction -> (TyAnn, TyAnnEnv)
ityinf tyenv (Ret _ VoidRet)      = (T.TyPri T.TyVoid, tyenv)
ityinf tyenv (Ret _ (ValueRet v)) = vtyinf tyenv v
ityinf tyenv (Unreachable _)      = (T.TyBot, tyenv) -- Unreachable has no defined semantics 
ityinf tyenv (Add _ i ty op1 op2) = undefined
-- Conversion Operations
ityinf tyenv (SExt    _ i v ty)   = convTyInf tyenv i v ty
ityinf tyenv (BitCast _ i v ty)   = convTyInf tyenv i v ty
-- Memory Operations
-- The pointer of a load must a first class type.
ityinf tyenv (Load _ i v a)       = let (vty, tye) = vtyinf tyenv v
                                    in (vty, M.insert i vty tye)

-- Auxiliar Function
convTyInf :: TyAnnEnv -> Identifier -> Value -> Type -> (TyAnn, TyAnnEnv)
convTyInf tyenv i v ty = let ity = liftTy ty
                             (vty, tye) = vtyinf tyenv v
                             nty = castTy vty ity
                         in (nty, M.insert i nty tye)


-- Value TyAnn Inference
vtyinf :: TyAnnEnv -> Value -> (TyAnn, TyAnnEnv)
vtyinf tyenv val = case val of
   Id v ty -> let vty = liftTy ty
              in case M.lookup v tyenv of
                   Nothing  -> error $ "vtyinf"  -- (vty, M.insert v vty tyenv)
                   Just tya -> let nty = unify vty tya
                               in (nty, M.adjust (const nty) v tyenv)
   Constant c -> constTyInf tyenv c

-- Constant TyAnn Inference
constTyInf :: TyAnnEnv -> Constant -> (TyAnn, TyAnnEnv)
constTyInf tye c = case c of
   UndefValue      -> (T.TyUndef, tye)
   PoisonValue     -> error "constTyInf: PoisonValue not supported"
   BlockAddr       -> error "constTyInf: BlockAddr not supported"
   SmpConst sc     -> sconstTyInf tye sc
   CmpConst cc     -> cconstTyInf tye cc
   GlobalValue gv  -> gvTyInf tye gv 
   ConstantExpr ec -> econstTyInf tye ec 

sconstTyInf :: TyAnnEnv -> SimpleConstant -> (TyAnn, TyAnnEnv)
sconstTyInf tye c = case c of
   ConstantInt _ ty -> case ty of
        TyInt s -> (T.TyPri $ T.TyInt s, tye)
        err     -> error "constTyInf (1)" 
   ConstantFP fp -> (T.TyPri T.TyFloat, tye)
   ConstantPointerNull ty -> undefined 

cconstTyInf :: TyAnnEnv -> ComplexConstant -> (TyAnn, TyAnnEnv)
cconstTyInf = undefined

gvTyInf :: TyAnnEnv -> GlobalValue -> (TyAnn, TyAnnEnv)
gvTyInf = undefined

econstTyInf :: TyAnnEnv -> ConstantExpr -> (TyAnn, TyAnnEnv)
econstTyInf = undefined

-- Parameter TyAnn Inference
parTyInf :: TyAnnEnv -> Parameter -> (TyAnn, TyAnnEnv)
parTyInf tye (Parameter i ty) = let tyr = liftTy ty
                                in case M.lookup i tye of
                                  Nothing -> (tyr, M.insert i tyr tye)
                                  Just tya -> error "parTyInf"

gLstTyInf tyenv f [] = ([], tyenv)
gLstTyInf tyenv f (x:xs) = let (ty,tye) = f tyenv x
                               (tys,tyef) = gLstTyInf tye f xs
                           in (ty:tys, tyef)

-- Lift a LLVM IR Type to the most generic Type Annotation
liftTy :: Type -> TyAnn
liftTy TyUndefined       = T.TyUndef
liftTy TyVoid            = T.TyPri T.TyVoid
liftTy Tyx86MMX          = error "liftTy: Tyx86MMX not supported"
liftTy TyMetadata        = error "liftTy: TyMetadata not supported"
liftTy (TyInt s)         = T.TyPri $ T.TyInt s
liftTy (TyFloatPoint f)  = T.TyPri T.TyFloat
liftTy TyOpaque          = T.TyDer $ T.TyOpa ""
liftTy (TyArray s ty)    = T.TyDer $ T.TyAgg $ T.TyArr s $ liftTy ty
liftTy (TyStruct n s ty) = T.TyDer $ T.TyAgg $ T.TyStr n s $ map liftTy ty
liftTy TyLabel           = T.TyDer $ T.TyLab [] T.TyBot
liftTy (TyFunction a r)  = T.TyDer $ T.TyFun (map liftTy a) [liftTy r]
liftTy (TyPointer ty)    = T.TyDer $ T.TyPtr (liftTy ty) T.TyAny
liftTy (TyVector s ty)   = T.TyDer $ T.TyVec s $ liftTy ty

unify :: TyAnn -> TyAnn -> TyAnn
unify = undefined

castTy :: TyAnn -> TyAnn -> TyAnn
castTy = undefined
