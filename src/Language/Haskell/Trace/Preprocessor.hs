{-# LANGUAGE RankNTypes #-}

-- | Add trace calls to a module.

module Language.Haskell.Trace.Preprocessor (addTracing) where

import Data.Data
import Data.Maybe
import Language.Haskell.Exts.Annotated

-- | Add tracing wrappers to all expressions in the module.
addTracing :: FilePath -> [(Int,Int,Int,Int)] -> Module SrcSpanInfo -> Module SrcSpanInfo
addTracing fp filters (Module an h ps imports ds) =
  Module an
         h
         ps
         (i : imports)
         (gtraverseT wrap ds)
  where i =
          ImportDecl an
                     (ModuleName an "Language.Haskell.Trace")
                     True
                     False
                     False
                     Nothing
                     Nothing
                     Nothing
        wrap :: Exp SrcSpanInfo -> Exp SrcSpanInfo
        wrap e | null filters || any (inside (srcInfoSpan (ann e))) filters =
          case e of
            Paren a e' -> Paren a (wrap e')
            _ ->
              Paren a
                    (Case a
                          (App a
                               (App a
                                    (Var a (UnQual a (Ident a "Language.Haskell.Trace.trace")))
                                    (Lit a (String a fp fp)))
                               (Tuple a
                                      Boxed
                                      [num srcSpanStartLine
                                      ,num srcSpanStartColumn
                                      ,num srcSpanEndLine
                                      ,num srcSpanEndColumn]))
                          [Alt a
                               (PTuple a Boxed [])
                               (UnGuardedRhs a
                                             (gtraverseT wrap e))
                               Nothing])
              where num f =
                      Lit a
                          (Int a
                               (fromIntegral (f s))
                               (show (f s)))
                    a = ann e
                    s = srcInfoSpan a
        wrap e = gtraverseT wrap e
addTracing _ _ x = x

-- | Is the given src span inside the given range?
inside :: SrcSpan -> (Int,Int,Int,Int) -> Bool
inside (SrcSpan _ sl sc el ec) (sl',sc',el',ec') =
  ((sl,sc) >= (sl',sc')) &&
  ((el,ec) <= (el',ec'))

-- | Traverse a data type, left to stop, right to keep going after
-- updating the value.
gtraverseT :: (Data a,Typeable b)
           => (Typeable b => b -> b) -> a -> a
gtraverseT f =
  gmapT (\x ->
           case cast x of
             Nothing -> gtraverseT f x
             Just b -> fromMaybe x (cast (f b)))
