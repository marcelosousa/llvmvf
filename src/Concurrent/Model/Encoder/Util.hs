-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Util
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Encoder.Util where

import Language.LLVMIR
import Numeric
import Data.Char

errormessage = error "enc type not supported"

fstu  (a,b,c) = a
sndu (a,b,c) = b
trdu (a,b,c) = c

getIdxN :: Type -> [Int]
getIdxN (TyArray  ne ty) = (getBSize ne):(getIdxN ty)
getIdxN (TyVector ne ty) = (getBSize ne):(getIdxN ty)
getIdxN (TyPointer ty)   = getIdxN ty
getIdxN _ = []

getBSize :: Int -> Int
getBSize n =length $  showIntAtBase 2 intToDigit n ""

getIdxSize :: Type -> Int
getIdxSize (TyArray  n _) = getBSize n
getIdxSize (TyVector n _) = getBSize n
getIdxSize (TyPointer ty) = getIdxSize ty
getIdxSize (TyStruct _ n _) = getBSize n
getIdxSize _ = error "getIdxSize"

getISize :: Type -> Int
getISize (TyInt p) = p
getISize (TyPointer t) = getISize t
getISize _ = 0


