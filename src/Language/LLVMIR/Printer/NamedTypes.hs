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
import UU.PPrint
import Language.LLVMIR.Printer

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
        
types :: NamedTypes -> Html
types nmdty = html $ do 
	H.head $ do
        H.title "Named Types" 
	body $ do
        types' nmdty

types' :: NamedTypes -> Html
types' nmdty = forM_ (toList nmdty) elDiv 

elDiv :: (Id, Type) -> Html 
elDiv (i,t) = H.div ! A.id (toValue i) $ do
	                  H.a ! A.name (toValue i) $ do 
						p $ toHtml i
						p $ toHtml $ show $ pretty t