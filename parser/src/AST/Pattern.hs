{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AST.Pattern where

import AST.V0_16
import ElmFormat.Mapping

import qualified Reporting.Annotation as A


type Pattern ns =
    A.Located (Pattern' ns)


data Pattern' ns
    = Anything
    | UnitPattern Comments
    | Literal Literal
    | VarPattern LowercaseIdentifier
    | OpPattern SymbolIdentifier
    | Data (ns, UppercaseIdentifier) [C1 BeforeTerm (Pattern ns)]
    | PatternParens (C2 Before After (Pattern ns))
    | Tuple [C2 BeforeTerm AfterTerm (Pattern ns)]
    | EmptyListPattern Comments
    | List [C2 BeforeTerm AfterTerm (Pattern ns)]
    | ConsPattern
        { first :: C0Eol (Pattern ns)
        , rest :: Sequence (Pattern ns)
        }
    | EmptyRecordPattern Comments
    | Record [C2 BeforeTerm AfterTerm LowercaseIdentifier]
    | Alias (Pattern ns, Comments) (Comments, LowercaseIdentifier)
    deriving (Eq, Show, Functor)


instance MapNamespace a b (Pattern' a) (Pattern' b) where
    mapNamespace = fmap


instance MapReferences a b (Pattern' a) (Pattern' b) where
    mapReferences fu fl = \case
        Anything -> Anything
        UnitPattern c -> UnitPattern c
        Literal l -> Literal l
        VarPattern l -> VarPattern l
        OpPattern o -> OpPattern o
        Data ctor args -> Data (fu ctor) (mapReferences fu fl args)
        PatternParens p -> PatternParens (mapReferences fu fl p)
        Tuple ps -> Tuple (mapReferences fu fl ps)
        EmptyListPattern c -> EmptyListPattern c
        List ps -> List (mapReferences fu fl ps)
        ConsPattern first rest -> ConsPattern (mapReferences fu fl first) (mapReferences fu fl rest)
        EmptyRecordPattern c -> EmptyRecordPattern c
        Record fs -> Record fs
        Alias (p, c) as -> Alias (mapReferences fu fl p, c) as
