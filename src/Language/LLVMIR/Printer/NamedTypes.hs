{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Printer.NamedTypes
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
module Language.LLVMIR.Printer.NamedTypes where

import Language.LLVMIR (NamedTypes, Id, Type)
import Data.Map hiding ((!))
import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

index :: NamedTypes -> Html
index nmdty = html $ do 
    H.head $ do
        H.title "Named Types"
    body $ do
        namedRefs nmdty

namedRefs :: NamedTypes -> Html
namedRefs nmdty = ul $ forM_ (toList nmdty) namedRef

namedRef :: (Id,Type) -> Html
namedRef (i,_) = li $ H.a ! A.href (toValue $ "types.html#"++i) $ toHtml i  
        
