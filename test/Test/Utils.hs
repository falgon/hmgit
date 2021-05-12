{-# LANGUAGE CPP, TemplateHaskell #-}
module Test.Utils (
    relativeProjRoot
) where

import           Control.Monad              ((>=>))
import           Data.FileEmbed             (makeRelativeToProject)
import           Data.String                (fromString)
import           Language.Haskell.TH.Syntax

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 0
#endif

strToExp :: String -> Q Exp
#if MIN_VERSION_template_haskell(2, 5, 0)
strToExp s = return $ VarE 'fromString `AppE` LitE (StringL s)
#else
strToExp s = do
    helper <- [| fromString |]
    return $! AppE helper $! LitE $! StringL s
#endif

relativeProjRoot :: FilePath -> Q Exp
relativeProjRoot = makeRelativeToProject >=> strToExp
