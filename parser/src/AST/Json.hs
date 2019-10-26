{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module AST.Json where

import Elm.Utils ((|>))

import AST.Annotated ()
import AST.Declaration
import AST.Expression
import AST.Module
import AST.Pattern
import AST.Variable
import AST.V0_16
import Data.Fix
import Data.Maybe (mapMaybe)
import ElmFormat.Mapping
import Reporting.Annotation hiding (map)
import Text.JSON hiding (showJSON)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified ElmFormat.Version
import qualified Reporting.Region as Region
import qualified ReversedList
import ReversedList (Reversed)


pleaseReport :: String -> String -> a
pleaseReport what details =
    error ("<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >")


showModule :: Module -> JSValue
showModule (Module _ maybeHeader _ (C _ imports) body) =
    let
        header =
            maybeHeader
                |> Maybe.fromMaybe AST.Module.defaultHeader

        (Header _ (C _ name) _ _) = header

        getAlias :: ImportMethod -> Maybe UppercaseIdentifier
        getAlias importMethod =
            fmap dropComments (alias importMethod)

        importAliases =
            Map.fromList $ map (\(k, v) -> (v, k)) $ Map.toList $ Map.mapMaybe (getAlias . dropComments) imports

        normalizeNamespace :: [UppercaseIdentifier] -> [UppercaseIdentifier]
        normalizeNamespace namespace =
            case namespace of
                [alias] ->
                    Map.findWithDefault namespace alias importAliases

                _ ->
                    namespace

        importJson (moduleName, C _ (ImportMethod alias (C _ exposing))) =
            let
                name = List.intercalate "." $ fmap (\(UppercaseIdentifier n) -> n) moduleName
            in
            ( name
            , makeObj
                [ ( "as", JSString $ toJSString $ maybe name (\(C _ (UppercaseIdentifier n)) -> n) alias)
                , ( "exposing", showImportListingJSON exposing )
                ]
            )
    in
    makeObj
        [ ( "moduleName", showJSON name )
        , ( "imports", makeObj $ fmap importJson $ Map.toList imports )
        , ( "body" , JSArray $ fmap showJSON $ mergeDeclarations $ (fmap (mapNamespace normalizeNamespace) body :: List (TopLevelStructure (Declaration [UppercaseIdentifier] (Fix (AnnotatedExpression [UppercaseIdentifier] Region.Region))))) )
        ]


data MergedTopLevelStructure ns e
    = MergedDefinition
        { _definitionLocation :: Region.Region
        , _name :: LowercaseIdentifier
        , _args :: List (C1 Before (Pattern ns))
        , _preEquals :: Comments
        , _expression :: e
        , _doc :: Maybe Comment
        , _annotation :: Maybe (Comments, Comments, Type ns)
        }
    | TodoTopLevelStructure String


mergeDeclarations :: Show e => List (TopLevelStructure (Declaration [UppercaseIdentifier] e)) -> List (MergedTopLevelStructure [UppercaseIdentifier] e)
mergeDeclarations decls =
    let
        collectAnnotation decl =
            case decl of
                Entry (A _ (TypeAnnotation (C preColon (VarRef () name)) (C postColon typ))) -> Just (name, (preColon, postColon, typ))
                _ -> Nothing

        annotations :: Map.Map LowercaseIdentifier (Comments, Comments, Type [UppercaseIdentifier])
        annotations =
            Map.fromList $ mapMaybe collectAnnotation decls

        merge decl =
            case decl of
                Entry (A region (Definition (A _ (VarPattern name)) args preEquals expr)) ->
                    Just $ MergedDefinition
                        { _definitionLocation = region
                        , _name = name
                        , _args = args
                        , _preEquals = preEquals
                        , _expression = expr
                        , _doc = Nothing -- TODO: merge docs
                        , _annotation = Map.lookup name annotations
                        }

                Entry (A _ (TypeAnnotation _ _)) ->
                    -- TODO: retain annotations that don't have a matching definition
                    Nothing

                _ ->
                    Just $ TodoTopLevelStructure (show decl)
    in
    mapMaybe merge decls


class ToJSON a where
  showJSON :: a -> JSValue


instance ToJSON Region.Region where
    showJSON region =
        makeObj
            [ ( "start", showJSON $ Region.start region )
            , ( "end", showJSON $ Region.end region )
            ]


instance ToJSON Region.Position where
    showJSON pos =
        makeObj
            [ ( "line", JSRational False $ toRational $ Region.line pos )
            , ( "col", JSRational False $ toRational $ Region.column pos )
            ]


instance ToJSON (List UppercaseIdentifier) where
    showJSON [] = JSNull
    showJSON namespace = (JSString . toJSString . List.intercalate "." . fmap (\(UppercaseIdentifier v) -> v)) namespace


instance ToJSON UppercaseIdentifier where
    showJSON (UppercaseIdentifier name) = JSString $ toJSString name


showImportListingJSON :: Listing DetailedListing -> JSValue
showImportListingJSON (ExplicitListing a _) = showJSON a
showImportListingJSON (OpenListing (C _ ())) = JSString $ toJSString "Everything"
showImportListingJSON ClosedListing = JSNull


showTagListingJSON :: Listing (CommentedMap UppercaseIdentifier ()) -> JSValue
showTagListingJSON (ExplicitListing tags _) =
    makeObj $ fmap (\(UppercaseIdentifier k, C _ ()) -> (k, JSBool True)) $ Map.toList tags
showTagListingJSON (OpenListing (C _ ())) = JSString $ toJSString "AllTags"
showTagListingJSON ClosedListing = JSString $ toJSString "NoTags"


instance ToJSON DetailedListing where
    showJSON (DetailedListing values _operators types) =
        makeObj
            [ ( "values", makeObj $ fmap (\(LowercaseIdentifier k) -> (k, JSBool True)) $ Map.keys values )
            , ( "types", makeObj $ fmap (\(UppercaseIdentifier k, (C _ listing)) -> (k, showTagListingJSON listing)) $ Map.toList types )
            ]


instance ToJSON e => ToJSON (MergedTopLevelStructure [UppercaseIdentifier] e) where
    showJSON (TodoTopLevelStructure what) =
        JSString $ toJSString ("TODO: " ++ what)
    showJSON (MergedDefinition region (LowercaseIdentifier name) args _ expression doc annotation) =
        makeObj
            [ type_ "Definition"
            , ( "name" , JSString $ toJSString name )
            , ( "returnType", maybe JSNull (\(_, _, t) -> showJSON t) annotation )
            , ( "expression" , showJSON expression )
            , sourceLocation region
            ]

-- instance ToJSON (TopLevelStructure Declaration) where
--   showJSON (Entry (A region (Definition (A _ (VarPattern (LowercaseIdentifier var))) _ _ expr))) =
--     makeObj
--       [ type_ "Definition"
--       , ( "name" , JSString $ toJSString var )
--       , ( "returnType", JSNull )
--       , ( "expression" , showJSON expr )
--       , sourceLocation region
--       ]
--   showJSON _ = JSString $ toJSString "TODO: Decl"


instance ToJSON Expr where
  showJSON (Fix (AE (A region expr))) =
      case expr of
          Unit _ ->
              makeObj [ type_ "UnitLiteral" ]

          AST.Expression.Literal (IntNum value repr) ->
              makeObj
                  [ type_ "IntLiteral"
                  , ("value", JSRational False $ toRational value)
                  , ("display"
                    , makeObj
                        [ ("representation", showJSON repr)
                        ]
                    )
                  ]

          AST.Expression.Literal (FloatNum value repr) ->
              makeObj
                  [ type_ "FloatLiteral"
                  , ("value", JSRational False $ toRational value)
                  , ("display"
                    , makeObj
                        [ ("representation", showJSON repr)
                        ]
                    )
                  ]

          AST.Expression.Literal (Boolean value) ->
            makeObj
                [ type_ "ExternalReference"
                , ("module", JSString $ toJSString "Basics")
                , ("identifier", JSString $ toJSString $ show value)
                , sourceLocation region
                ]

          AST.Expression.Literal (Chr chr) ->
              makeObj
                  [ type_ "CharLiteral"
                  , ("module", JSString $ toJSString "Char")
                  , ("value", JSString $ toJSString [chr])
                  , sourceLocation region
                  ]

          AST.Expression.Literal (Str str _) ->
              makeObj
                  [ type_ "StringLiteral"
                  , ("module", JSString $ toJSString "String")
                  , ("value", JSString $ toJSString str)
                  , sourceLocation region
                  ]

          VarExpr (VarRef [] (LowercaseIdentifier var)) ->
            variableReference region var

          VarExpr (VarRef namespace (LowercaseIdentifier var)) ->
              makeObj
                  [ type_ "ExternalReference"
                  , ("module"
                    , showJSON namespace)
                  , ("identifier", JSString $ toJSString var)
                  , sourceLocation region
                  ]

          VarExpr (TagRef [] (UppercaseIdentifier tag)) ->
            variableReference region tag

          VarExpr (OpRef (SymbolIdentifier sym)) ->
            variableReference region sym

          VarExpr _ ->
              JSString $ toJSString "TODO: VarExpr"

          App expr args _ ->
              makeObj
                  [ type_ "FunctionApplication"
                  , ("function", showJSON expr)
                  , ("arguments", JSArray $ showJSON <$> map dropComments args)
                  ]

          Binops first rest _ ->
              makeObj
                  [ type_ "BinaryOperatorList"
                  , ("first", showJSON first)
                  , ("operations"
                    , JSArray $ map
                        (\(BinopsClause _ op _ expr) ->
                           makeObj
                               [ ("operator", showJSON $ Fix $ AE $ noRegion $ VarExpr op)
                               , ("term", showJSON expr)
                               ]
                        )
                        rest
                    )
                  ]

          Unary Negative expr ->
            makeObj
                [ type_ "UnaryOperator"
                , ("operator", JSString $ toJSString "-")
                , ("term", showJSON expr)
                ]

          Parens (C _ expr) ->
              showJSON expr

          ExplicitList terms _ _ ->
              makeObj
                  [ type_ "ListLiteral"
                  , ("terms", JSArray $ fmap showJSON (map dropComments terms))
                  ]

          AST.Expression.Tuple exprs _ ->
              makeObj
                  [ type_ "TupleLiteral"
                  , ("terms", JSArray $ fmap showJSON (map dropComments exprs))
                  ]

          TupleFunction n | n <= 1 ->
            pleaseReport "INVALID TUPLE CONSTRUCTOR" ("n=" ++ show n)

          TupleFunction n ->
            variableReference region $ replicate (n-1) ','

          AST.Expression.Record base fields _ _ ->
              let
                  fieldsJSON =
                      ( "fields"
                      , makeObj $ fmap
                          (\(C _ (Pair (C _ (LowercaseIdentifier key)) (C _ value) _)) ->
                             (key, showJSON value)
                          )
                          fields
                      )

                  fieldOrder =
                      ( "fieldOrder"
                      , JSArray $
                            fmap (JSString . toJSString) $
                            fmap (\(C _ (Pair (C _ (LowercaseIdentifier key)) _ _)) -> key) fields
                      )
              in
              case base of
                  Just (C _ (LowercaseIdentifier identifier)) ->
                      makeObj
                          [ type_ "RecordUpdate"
                          , ("base", JSString $ toJSString identifier)
                          , fieldsJSON
                          , ( "display"
                            , makeObj
                                [ fieldOrder
                                ]
                            )
                          ]

                  Nothing ->
                      makeObj
                          [ type_ "RecordLiteral"
                          , fieldsJSON
                          , ( "display"
                            , makeObj
                                [ fieldOrder
                                ]
                            )
                          ]

          Access base (LowercaseIdentifier field) ->
            makeObj
                [ type_ "RecordAccess"
                , ( "record", showJSON base )
                , ( "field", JSString $ toJSString field )
                ]

          Lambda parameters _ body _ ->
              makeObj
                  [ type_ "AnonymousFunction"
                  , ("parameters", JSArray $ map (\(C _ (A _ pat)) -> showJSON pat) parameters)
                  , ("body", showJSON body)
                  ]

          If (IfClause (C _ cond') (C _ thenBody')) rest' (C _ elseBody) ->
              let
                  ifThenElse :: Expr -> Expr -> [C1 Before (IfClause Expr)] -> JSValue
                  ifThenElse cond thenBody rest =
                      makeObj
                          [ type_ "IfExpression"
                          , ( "if", showJSON cond )
                          , ( "then", showJSON thenBody )
                          , ( "else"
                            , case rest of
                                [] ->
                                    showJSON elseBody

                                C _ (IfClause (C _ nextCond) (C _ nextBody)) : nextRest ->
                                    ifThenElse nextCond nextBody nextRest
                            )
                          ]
              in
              ifThenElse cond' thenBody' rest'

          Let decls _ body ->
              makeObj
                  [ type_ "LetExpression"
                  , ( "declarations", JSArray $ map showJSON decls)
                  , ("body", showJSON body)
                  ]

          Case (C _ subject, _) branches ->
              makeObj
                  [ type_ "CaseExpression"
                  , ( "subject", showJSON subject )
                  , ( "branches"
                    , JSArray $ map
                        (\(CaseBranch _ _ _ (A _ pat) body) ->
                           makeObj
                               [ ("pattern", showJSON pat)
                               , ("body", showJSON body)
                               ]
                        )
                        branches
                    )
                  ]

          Range _ _ _ ->
              JSString $ toJSString "TODO: Range"

          AccessFunction _ ->
              JSString $ toJSString "TODO: AccessFunction"

          GLShader _ ->
              JSString $ toJSString "TODO: GLShader"


variableReference :: Region.Region -> String -> JSValue
variableReference region name =
    makeObj
        [ type_ "VariableReference"
        , ( "name" , JSString $ toJSString name )
        , sourceLocation region
        ]


sourceLocation :: Region.Region -> (String, JSValue)
sourceLocation region =
    ( "sourceLocation", showJSON region )


instance (ToJSON e, Show e, Show ns) => ToJSON (LetDeclaration [ns] e) where
  showJSON letDeclaration =
      case letDeclaration of
          LetDefinition (A _ (VarPattern (LowercaseIdentifier var))) [] _ expr ->
              makeObj
                  [ type_ "Definition"
                  , ("name" , JSString $ toJSString var)
                  , ("expression" , showJSON expr)
                  ]

          _ ->
              JSString $ toJSString $ "TODO: LetDeclaration (" ++ show letDeclaration ++ ")"


instance ToJSON IntRepresentation where
    showJSON DecimalInt = JSString $ toJSString $ "DecimalInt"
    showJSON HexadecimalInt = JSString $ toJSString $ "HexadecimalInt"


instance ToJSON FloatRepresentation where
    showJSON DecimalFloat = JSString $ toJSString "DecimalFloat"
    showJSON ExponentFloat = JSString $ toJSString "ExponentFloat"


instance ToJSON (Pattern' [UppercaseIdentifier]) where
  showJSON pattern' =
      JSString $ toJSString $ "TODO: Pattern (" ++ show pattern' ++ ")"


instance ToJSON (Type [UppercaseIdentifier]) where
    showJSON (A _ type') =
        case type' of
            TypeConstruction (NamedConstructor (namespace, name)) args ->
                makeObj
                    [ type_ "TypeReference"
                    , ( "name", showJSON name )
                    , ( "module", showJSON namespace )
                    , ( "arguments", JSArray $ fmap (showJSON . dropComments) args )
                    ]

            TypeVariable (LowercaseIdentifier name) ->
                makeObj
                    [ type_ "TypeVariable"
                    , ( "name", JSString $ toJSString name )
                    ]

            FunctionType first rest _ ->
                case firstRestToRestLast first rest of
                    (args, C _ last) ->
                        makeObj
                            [ type_ "FunctionType"
                            , ( "returnType", showJSON last)
                            , ( "argumentTypes", JSArray $ fmap (\(C _ t) -> showJSON t) $ args )
                            ]

            _ ->
                JSString $ toJSString $ "TODO: Type (" ++ show type' ++ ")"
        where
            firstRestToRestLast :: C0Eol x -> List (C2Eol a b x) -> (List (C2Eol a b x), C0Eol x)
            firstRestToRestLast first rest =
                done $ foldl (flip step) (ReversedList.empty, first) rest
                where
                    step :: C2Eol a b x -> (Reversed (C2Eol a b x), C0Eol x) -> (Reversed (C2Eol a b x), C0Eol x)
                    step (C (a, b, dn) next) (acc, C dn' last) =
                        (ReversedList.push (C (a, b, dn') last) acc, C dn next)

                    done :: (Reversed (C2Eol a b x), C0Eol x) -> (List (C2Eol a b x), C0Eol x)
                    done (acc, last) =
                        (ReversedList.toList acc, last)


type_ :: String -> (String, JSValue)
type_ t =
    ("type", JSString $ toJSString t)


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Located a
noRegion =
    at nowhere nowhere
