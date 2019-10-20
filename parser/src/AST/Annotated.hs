{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AST.Annotated where

import AST.Declaration hiding (TopLevelStructure)
import qualified AST.Declaration
import AST.Expression hiding (Expression)
import Data.Fix
import ElmFormat.Mapping


type Expression ns ann =
    Fix (AnnotatedExpression ns ann)


type TopLevelStructure ns ann =
    AST.Declaration.TopLevelStructure (Declaration ns (Expression ns ann))

instance MapNamespace a b (TopLevelStructure a ann) (TopLevelStructure b ann) where
    mapNamespace f tls =
        let
            x d =
              d''
              where
                  d' :: Declaration a (Expression b ann)
                  d' = fmap (\x -> mapNamespace f x) d

                  d'' :: Declaration b (Expression b ann)
                  d'' = mapNamespace f d'
        in
        fmap x tls
    {-# INLINE mapNamespace #-}

instance MapReferences a b (TopLevelStructure a ann) (TopLevelStructure b ann) where
    mapReferences fu fl tls =
        let
            x d =
              d''
              where
                  d' :: Declaration a (Expression b ann)
                  d' = fmap (\x -> mapReferences fu fl x) d

                  d'' :: Declaration b (Expression b ann)
                  d'' = mapReferences fu fl d'
        in
        fmap x tls
    {-# INLINE mapReferences #-}
