module Data.Generator where

import Prelude

import Control.Monad.Writer (Writer, tell)
import Data.AbiParser (Abi(..), AbiType(..), FunctionInput(..), IndexedSolidityValue(..), SolidityEvent(..), SolidityFunction(..), SolidityConstructor(..), SolidityType(..), format)
import Data.Array (filter, length, mapWithIndex, null, replicate, uncons, unsnoc, zip, zipWith, (:))
import Data.Foldable (fold)
import Data.Identity (Identity(..))
import Data.List (uncons) as List
import Data.List.Types (NonEmptyList(..)) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)
import Data.String (drop, joinWith, take, toLower, toUpper)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), uncurry)
import Network.Ethereum.Core.HexString (fromByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Web3.Types (HexString, unHex)
import Partial.Unsafe (unsafeCrashWith)

--------------------------------------------------------------------------------
type ModuleName = String
type ModuleImports = Array ModuleImport

data ModuleImport
  = IType String
  | ITypeCtr String
  | ITypeOp String
  | IClass String
  | IVal String
  | IOp String

type Imports = Array (Tuple ModuleName ModuleImports)
type Imported = Writer Imports

type CodeOptions =
  { exprPrefix :: String
  , indentationLevel :: Int
  }

class Code a where
  genCode :: a -> CodeOptions -> Imported String

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

paren :: String -> String
paren s = "(" <> s <> ")"

toSignature :: SolidityFunction -> String
toSignature (SolidityFunction f) =
  let args = map (\i -> format i) f.inputs
  in f.name <> paren (joinWith "," args)

capitalize :: String -> String
capitalize s =
  let h = toUpper $ take 1 s
      rest = drop 1 s
  in h <> rest

isValidType :: String -> Boolean
isValidType s =
  let startChar = take 1 s
  -- if the first character is the same when both lowercase and uppercase it cannot be a valid type name (e.g. underscores)
  in toUpper startChar /= toLower startChar

lowerCase :: String -> String
lowerCase s =
  let h = toLower $ take 1 s
      rest = drop 1 s
  in h <> rest

makeDigits :: Int -> Imported String
makeDigits n = do
  let digits = map singleton <<< toCharArray <<< show $ n
  ddigits <- for digits \d -> do
    let d' = "D" <> d
    import' "Network.Ethereum.Web3.Solidity" [IType d']
    pure d'
  import' "Network.Ethereum.Web3.Solidity" [IType "DOne"]
  map paren case unsnoc ddigits of
    Nothing -> unsafeCrashWith "imposible case reached in makeDigits"
    Just { init, last }
      | null init -> pure $ "DOne " <> last
      | otherwise -> do
        import' "Network.Ethereum.Web3.Solidity.Size" [ITypeOp ":&"]
        pure $ joinWith " :& " init <> " :& " <> "DOne " <> last

import' :: ModuleName -> ModuleImports -> Imported Unit
import' mName mImports = tell [ Tuple mName mImports ]

toPSType :: SolidityType -> Imported String
toPSType s = case s of
    SolidityBool -> do
      pure "Boolean"
    SolidityAddress -> do
      import' "Network.Ethereum.Web3.Types" [IType "Address"]
      pure "Address"
    SolidityUint n -> do
      import' "Network.Ethereum.Web3.Solidity" [IType "UIntN"]
      digits <- makeDigits n
      pure $ paren $ "UIntN " <> digits
    SolidityInt n -> do
      import' "Network.Ethereum.Web3.Solidity" [IType "IntN"]
      digits <- makeDigits n
      pure $ paren $ "IntN " <> digits
    SolidityString -> do
      pure "String"
    SolidityBytesN n -> do
      import' "Network.Ethereum.Web3.Solidity" [IType "BytesN"]
      digits <- makeDigits n
      pure $ paren $ "BytesN " <> digits
    SolidityBytesD -> do
      import' "Network.Ethereum.Web3.Solidity" [IType "ByteString"]
      pure "ByteString"
    SolidityVector ns a -> do
      expandVector ns a
    SolidityArray a -> do
      t <- toPSType a
      pure $ paren $ "Array " <> t
  where
  expandVector (List.NonEmptyList (n :| ns)) a' = do
      l <- makeDigits n
      import' "Network.Ethereum.Web3" [IType "Vector"]
      case List.uncons ns of
        Nothing -> do
          x <- toPSType a'
          pure $ paren $ "Vector " <> l <> " " <> x
        Just {head, tail} -> do
          x <- expandVector (List.NonEmptyList $ head :| tail) a'
          pure $ paren $ "Vector " <> l <> " " <> x

--------------------------------------------------------------------------------
-- | Data decleration, instances, and helpers
--------------------------------------------------------------------------------

-- | Data declaration
data FunTypeDecl =
  FunTypeDecl { signature :: String
              , factorTypes :: Array String
              , typeName :: String
              }

funToTypeDecl :: SolidityFunction -> CodeOptions -> Imported FunTypeDecl
funToTypeDecl fun@(SolidityFunction f) opts = do
  factorTypes <-
    if f.isUnCurried
      then for f.inputs tagInput
      else for f.inputs $ \(FunctionInput fi) -> toPSType fi.type
  pure $
    FunTypeDecl
      { typeName: if isValidType f.name then capitalize $ f.name <> "Fn" else "FnT" <> f.name <> "Fn"
      , factorTypes
      , signature: toSignature fun
      }

instance codeDataDecl :: Code FunTypeDecl where
  genCode (FunTypeDecl decl) _ = do
    let
      nArgs = length decl.factorTypes
      tupleType = "Tuple" <> show nArgs
    import' "Data.Functor.Tagged" [IType "Tagged"]
    import' "Data.Symbol" [IType "SProxy"]
    import' "Network.Ethereum.Web3.Solidity" [ITypeCtr tupleType]
    pure $
      fold
        ["type "
        , decl.typeName
        , " = "
        , "Tagged (SProxy \"" <> decl.signature <> "\") (" <> tupleType <> " " <> joinWith " " decl.factorTypes <> ")"
        ]

--------------------------------------------------------------------------------
-- | Helper functions (asynchronous call/send)
--------------------------------------------------------------------------------

type CurriedHelperFunctionR =
  { signature :: Array String
  , unpackExpr :: { name :: String, stockArgs :: Array String, payloadArgs :: Array String }
  , payload :: String
  , transport :: String
  , constraints :: Array String
  , quantifiedVars :: Array String
  }

data HelperFunction
  = CurriedHelperFunction CurriedHelperFunctionR
  | UnCurriedHelperFunction
      { signature :: Array String
      , unpackExpr :: { name :: String, stockArgs :: Array String, stockArgsR :: Array String }
      , constraints :: Array String
      , quantifiedVars :: Array String
      , whereClause :: String
      }

funToHelperFunction :: Boolean -> SolidityFunction -> CodeOptions -> Imported CurriedHelperFunctionR
funToHelperFunction isWhereClause fun@(SolidityFunction f) opts = do
  (FunTypeDecl decl) <- funToTypeDecl fun opts
  import' "Network.Ethereum.Web3.Types" [IType "TransactionOptions"]
  sigPrefix <-
    if f.isConstructor
      then do
        import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
        import' "Network.Ethereum.Web3.Types" [IType "HexString"]
        pure ["TransactionOptions NoPay", "HexString"]
      else if f.constant
        then do
          import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
          import' "Network.Ethereum.Web3.Types" [IType "ChainCursor"]
          pure ["TransactionOptions NoPay", "ChainCursor"]
        else if f.payable
               then do
                   import' "Network.Ethereum.Web3.Types.TokenUnit" [IType "MinorUnit"]
                   pure ["TransactionOptions MinorUnit"]
               else do
                   import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
                   pure ["TransactionOptions NoPay"]
  let
    var = if isWhereClause then "y" else "x"
    constraints = []
    quantifiedVars = []
    stockVars =
      if f.isConstructor
         then [var <> "0", if isWhereClause then "bc'" else "bc"]
         else if f.constant
              then [var <> "0", if isWhereClause then "cm'" else "cm"]
              else [var <> "0"]
    offset = length stockVars
    inputs' = map (\(FunctionInput fi) -> fi.type) f.inputs
    conVars = mapWithIndex (\i _ -> var <> show (offset + i)) inputs'
  helperTransport <- toTransportPrefix f.isConstructor f.constant $ length f.outputs
  helperPayload <- toPayload isWhereClause decl.typeName conVars
  returnType <- toReturnType f.constant f.outputs
  ins <-
    if f.isUnCurried
      then for f.inputs tagInput
      else for f.inputs $ \(FunctionInput fi) -> toPSType fi.type
  pure
    { signature: sigPrefix <> ins <> [returnType]
    , unpackExpr:
        { name: lowerCase $ opts.exprPrefix <> f.name
        , stockArgs: stockVars
        , payloadArgs: conVars
        }
    , payload: helperPayload
    , transport: helperTransport
    , constraints: constraints
    , quantifiedVars: quantifiedVars
    }

funToHelperFunction' :: SolidityFunction -> CodeOptions -> Imported HelperFunction
funToHelperFunction' fun@(SolidityFunction f) opts = do
    (FunTypeDecl decl) <- funToTypeDecl fun opts
    import' "Network.Ethereum.Web3.Types" [IType "TransactionOptions"]
    sigPrefix <-
      if f.isConstructor
        then do
          import' "Network.Ethereum.Web3.Types" [IType "HexString"]
          import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
          pure ["TransactionOptions NoPay", "HexString"]
        else if f.constant
          then do
            import' "Network.Ethereum.Web3.Types" [IType "ChainCursor"]
            import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
            pure ["TransactionOptions NoPay", "ChainCursor"]
          else if f.payable
               then do
                 import' "Network.Ethereum.Web3.Types.TokenUnit" [IType "MinorUnit"]
                 pure ["TransactionOptions MinorUnit"]
               else do
                 import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
                 pure ["TransactionOptions NoPay"]
    let
      constraints = []
      quantifiedVars = []
      stockVars = if f.isConstructor
                    then ["x0", "bc"]
                    else if f.constant
                      then ["x0", "cm"]
                      else ["x0"]
    returnType <- toReturnType f.constant f.outputs
    recIn <- recordInput f.inputs
    whereC <- whereHelper decl sigPrefix f.inputs returnType >>= \h -> genCode h opts {indentationLevel = opts.indentationLevel + 4}
    pure $
      UnCurriedHelperFunction
        { signature: sigPrefix <> [recIn, returnType]
        , unpackExpr:
            { name: lowerCase $ opts.exprPrefix <> f.name
            , stockArgs: stockVars <> ["r"]
            , stockArgsR: stockVars
            }
        , constraints: constraints
        , quantifiedVars: quantifiedVars
        , whereClause: whereC
        }
  where
    recordInput fis = do
      rowElems <- for fis $ \(FunctionInput fi) -> do
        ty <- toPSType fi.type
        pure $ fi.name <> " :: " <> ty
      pure $ "{ " <> joinWith ", " rowElems <> " }"
    whereHelper d pre is ret = do
      helper <- funToHelperFunction true fun opts
      tys <-
        if f.isUnCurried
          then for f.inputs tagInput
          else for f.inputs $ \(FunctionInput fi) -> toPSType fi.type
      pure $ CurriedHelperFunction helper
        { constraints = []
        , quantifiedVars = []
        , unpackExpr = helper.unpackExpr {name = helper.unpackExpr.name <> "'"}
        , signature = pre <> tys <> [ret]
        }

tagInput
  :: FunctionInput
  -> Imported String
tagInput (FunctionInput fi) = do
  ty <- toPSType fi.type
  import' "Data.Functor.Tagged" [IType "Tagged"]
  import' "Data.Symbol" [IType "SProxy"]
  pure $ "(Tagged (SProxy " <> "\"" <> fi.name <> "\") " <> ty <> ")"

toTransportPrefix :: Boolean -> Boolean -> Int -> Imported String
toTransportPrefix isConstructor isCall outputCount = do
  fun <- if isConstructor
    then do
      import' "Network.Ethereum.Web3" [IVal "deployContract"]
      pure "deployContract"
    else if isCall
      then do
        import' "Network.Ethereum.Web3" [IVal "call"]
        pure "call"
      else do
        import' "Network.Ethereum.Web3" [IVal "sendTx"]
        pure "sendTx"
  modifier <- if isCall && outputCount == 1
    then do
      import' "Network.Ethereum.Web3.Solidity" [IVal "unTuple1"]
      pure $ if isCall
               then "map unTuple1 <$> "
               else "unTuple1 <$> "
    else
      pure ""
  pure $ modifier <> fun

toPayload :: Boolean -> String -> Array String -> Imported String
toPayload isWhereClause typeName args = do
  import' "Data.Functor.Tagged" [IVal "tagged"]
  let tupleType = "Tuple" <> show (length args)
  import' "Network.Ethereum.Web3.Solidity" [ITypeCtr tupleType]
  pure $ "((tagged $ " <> tupleType <> " " <> joinWith " " args <> ") :: " <> typeName <> ")"

toReturnType :: Boolean -> Array SolidityType -> Imported String
toReturnType constant outputs' = do
  import' "Network.Ethereum.Web3.Types" [IType "Web3"]
  if not constant
    then do
      import' "Network.Ethereum.Web3.Types" [IType "HexString"]
      pure "Web3 HexString"
    else do
      import' "Network.Ethereum.Web3.Types" [IType "CallError"]
      import' "Data.Either" [IType "Either"]
      outputs <- for outputs' toPSType
      out <- case uncons outputs of
        Nothing -> pure "Unit"
        Just { head, tail: []} -> pure head
        Just _ -> do
          let tupleType = "Tuple" <> show (length outputs)
          import' "Network.Ethereum.Web3.Solidity" [IType tupleType]
          pure $ paren $ tupleType <> " " <> joinWith " " outputs
      pure $ "Web3 " <> paren ("Either CallError " <> out)

instance codeHelperFunction :: Code HelperFunction where
  genCode (CurriedHelperFunction h) opts =
    let constraints = fold $ map (\c -> c <> " => ") h.constraints
        quantification = if h.quantifiedVars == [] then "" else "forall " <> joinWith " " h.quantifiedVars <> ". "
        decl = h.unpackExpr.name <> " :: " <> quantification <> constraints <> joinWith " -> " h.signature
        defL = h.unpackExpr.name <> " " <> joinWith " " (h.unpackExpr.stockArgs <> h.unpackExpr.payloadArgs)
        defR = h.transport <> " " <> joinWith " " h.unpackExpr.stockArgs <> " " <> h.payload
    in pure <<< fold $ map (\s -> indentation <> s) [decl <> "\n", defL <> " = " <> defR]
    where
      indentation = fold $ replicate opts.indentationLevel " "
  genCode (UnCurriedHelperFunction h) _ = do
    import' "Network.Ethereum.Web3.Contract.Internal" [IVal "uncurryFields"]
    let constraints = fold $ map (\c -> c <> " => ") h.constraints
        quantification = if h.quantifiedVars == [] then "" else "forall " <> joinWith " " h.quantifiedVars <> ". "
        decl = h.unpackExpr.name <> " :: " <> quantification <> constraints <> joinWith " -> " h.signature
        defL = h.unpackExpr.name <> " " <> joinWith " " h.unpackExpr.stockArgs
        defR = "uncurryFields " <> " r $ " <> h.unpackExpr.name <> "'" <> " " <> joinWith " " h.unpackExpr.stockArgsR
    pure <<< fold $ [decl <> "\n", defL <> " = " <> defR <> "\n", "   where\n", h.whereClause]


--------------------------------------------------------------------------------

data EventDataDecl =
  EventDataDecl { constructor :: String
                , indexedTypes :: Array (Tuple String String)
                , nonIndexedTypes :: Array (Tuple String String)
                , recordType :: Array (Tuple String String)
                }

eventToDataDecl :: SolidityEvent -> Imported EventDataDecl
eventToDataDecl (SolidityEvent ev) = do
  let is = filter (\(IndexedSolidityValue sv) -> sv.indexed) ev.inputs
      nis = filter (\(IndexedSolidityValue sv) -> not sv.indexed) ev.inputs
  indexedTypes <- for is \(IndexedSolidityValue sv) -> do
    t <- toPSType sv.type
    pure $ Tuple sv.name t
  nonIndexedTypes <- for nis \(IndexedSolidityValue sv) -> do
    t <- toPSType sv.type
    pure $ Tuple sv.name t
  recordType <- for ev.inputs \(IndexedSolidityValue sv) -> do
    t <- toPSType sv.type
    pure $ Tuple sv.name t
  pure $ EventDataDecl
    { constructor: if isValidType (capitalize ev.name) then capitalize ev.name else "EvT" <> ev.name
    , indexedTypes
    , nonIndexedTypes
    , recordType
    }


instance codeEventDataDecl :: Code EventDataDecl where
  genCode (EventDataDecl decl) _ = do
    import' "Data.Newtype" [IClass "Newtype"]
    let recordField (Tuple label val) = label <> " :: " <> val
        newtypeDef = "newtype " <> decl.constructor <> " = " <> decl.constructor <> " {" <> joinWith "," (map recordField decl.recordType) <> "}"
        newtypeInstanceDecl = "derive instance newtype" <> decl.constructor <> " :: Newtype " <> decl.constructor <> " _"
    pure $
      newLine2
        [ newtypeDef
        , newtypeInstanceDecl
        ]


data EventGenericInstance =
  EventGenericInstance { instanceNames :: Array String
                       , instanceTypes :: Array String
                       , genericDefs :: Array String
                       , genericDeriving :: String
                       }

instance codeEventGenericInstance :: Code EventGenericInstance where
  genCode (EventGenericInstance i) _ =
    let headers = uncurry (\n t -> "instance " <> n <> " :: " <> t <> " where") <$> (zip i.instanceNames i.instanceTypes)
        eventGenerics = (\d -> "  " <> d) <$> i.genericDefs
        instances = zipWith (\h g -> h <> "\n" <> g) headers eventGenerics
    in pure $ newLine2 $ i.genericDeriving : instances

eventToEventGenericInstance :: SolidityEvent -> Imported EventGenericInstance
eventToEventGenericInstance ev@(SolidityEvent e) = do
  (EventDataDecl decl) <- eventToDataDecl ev
  let capConst = capitalize decl.constructor
  import' "Data.Generic.Rep.Eq" [IVal "genericEq"]
  import' "Data.Generic.Rep.Show" [IVal "genericShow"]
  import' "Data.Generic.Rep" [IClass "Generic"]
  pure $
    EventGenericInstance
      { instanceNames: (\n -> "eventGeneric" <> capConst <> n) <$> ["Show", "eq"]
      , instanceTypes: (\t -> t <> " " <> capConst) <$> ["Show", "Eq"]
      , genericDefs: ["show = genericShow", "eq = genericEq"]
      , genericDeriving: "derive instance generic" <> capConst <> " :: Generic " <> capConst <> " _"
      }

data EventDecodeInstance =
  EventDecodeInstance { indexedTuple :: String
                      , nonIndexedTuple :: String
                      , combinedType :: String
                      , anonymous :: Boolean
                      }

instance codeEventDecodeInstance :: Code EventDecodeInstance where
  genCode (EventDecodeInstance ev) _ = do
    import' "Network.Ethereum.Web3.Solidity" [IClass "IndexedEvent"]
    let indexedEventDecl = "instance indexedEvent" <> ev.combinedType <> " :: IndexedEvent " <> ev.indexedTuple <> " " <> ev.nonIndexedTuple <> " " <> ev.combinedType <> " where"
        indexedEventBody = "isAnonymous _ = " <> show ev.anonymous
    pure $
      newLine1
        [ indexedEventDecl
        , "  " <> indexedEventBody
        ]

eventToDecodeEventInstance :: SolidityEvent -> Imported EventDecodeInstance
eventToDecodeEventInstance event@(SolidityEvent ev) = do
  (EventDataDecl decl) <- eventToDataDecl event
  indexedTypesTagged <- for decl.indexedTypes taggedFactor
  nonIndexedTypesTagged <- for decl.nonIndexedTypes taggedFactor
  let
    indexedTupleType = "Tuple" <> show (length decl.indexedTypes)
    nonIndexedTupleType = "Tuple" <> show (length decl.nonIndexedTypes)
    indexedTuple = paren $ indexedTupleType <> " " <> joinWith " " indexedTypesTagged
    nonIndexedTuple = paren $ nonIndexedTupleType <> " " <> joinWith " " nonIndexedTypesTagged
  import' "Network.Ethereum.Web3.Solidity" [IType indexedTupleType, IType nonIndexedTupleType]
  pure $ EventDecodeInstance {indexedTuple, nonIndexedTuple, combinedType: decl.constructor, anonymous: ev.anonymous}
  where
  taggedFactor (Tuple label value) = do
    import' "Data.Functor.Tagged" [IType "Tagged"]
    import' "Data.Symbol" [IType "SProxy"]
    pure $ "(Tagged (SProxy \"" <> label <> "\") " <> value <> ")"


data EventFilterInstance =
  EventFilterInstance { instanceName :: String
                      , instanceType :: String
                      , filterDef :: String
                      }

instance codeEventFilterInstance :: Code EventFilterInstance where
  genCode (EventFilterInstance i) _ = do
    import' "Network.Ethereum.Web3" [IClass "EventFilter"]
    let
      header = "instance " <> i.instanceName <> " :: EventFilter " <> i.instanceType <> " where"
      eventFilter = "  " <> i.filterDef
    pure $ newLine1 [header, eventFilter]

eventId :: SolidityEvent -> HexString
eventId (SolidityEvent e) =
  let eventArgs = map (\a -> format a) e.inputs
  in fromByteString $ keccak256 $ e.name <> paren (joinWith "," eventArgs)

eventToEventFilterInstance :: SolidityEvent -> Imported EventFilterInstance
eventToEventFilterInstance ev@(SolidityEvent e) = do
  (EventDataDecl decl) <- eventToDataDecl ev
  filterExpr <- mkFilterExpr "addr"
  pure $
    EventFilterInstance
      { instanceName: "eventFilter" <> capitalize decl.constructor
      , instanceType: decl.constructor
      , filterDef: "eventFilter _ addr = " <> filterExpr
      }
  where
  mkFilterExpr :: String -> Imported String
  mkFilterExpr addr = do
    import' "Network.Ethereum.Web3.Types" [IVal "mkHexString"]
    import' "Data.Maybe" [ITypeCtr "Maybe", IVal "fromJust"]
    import' "Data.Lens" [IOp ".~"]
    import' "Network.Ethereum.Web3" [IVal "_address", IVal "_topics"]
    import' "Network.Ethereum.Web3.Types" [IVal "defaultFilter"]
    import' "Partial.Unsafe" [IVal "unsafePartial"]
    let
      nIndexedArgs = length $ filter (\(IndexedSolidityValue v) -> v.indexed) e.inputs
      indexedVals =
        if nIndexedArgs == 0
          then ""
          else "," <> joinWith "," (replicate nIndexedArgs "Nothing")
      eventIdStr = "Just ( unsafePartial $ fromJust $ mkHexString " <> "\"" <> (unHex $ eventId ev) <> "\"" <> ")"
    pure $
      fold
        ["defaultFilter"
        , "\n    "
        , joinWith "\n    "
          [ "# _address .~ Just " <> addr
          , "# _topics .~ Just [" <> eventIdStr <> indexedVals <> "]"
          ]
        ]


eventToEventCodeBlock :: SolidityEvent -> Imported CodeBlock
eventToEventCodeBlock ev@(SolidityEvent e) = do
  eventDec <- eventToDataDecl ev
  eventFilterInstance <- eventToEventFilterInstance ev
  decodeEventInstance <- eventToDecodeEventInstance ev
  eventGenericInstance <- eventToEventGenericInstance ev
  pure $ EventCodeBlock eventDec eventFilterInstance decodeEventInstance eventGenericInstance

--------------------------------------------------------------------------------

mkComment :: Array String -> String
mkComment cs = let sep = (fromCharArray $ replicate 80 '-') <> "\n"
               in  sep <> (newLine1 $ map (\s -> "-- | " <> s) cs) <> "\n" <> sep

data CodeBlock =
    FunctionCodeBlock FunTypeDecl HelperFunction
  | EventCodeBlock EventDataDecl  EventFilterInstance EventDecodeInstance EventGenericInstance

funToFunctionCodeBlock :: SolidityFunction -> CodeOptions -> Imported CodeBlock
funToFunctionCodeBlock fun@(SolidityFunction f) opts = do
  typeDecl <- funToTypeDecl fun opts
  helperFunction <- if f.isUnCurried
                      then funToHelperFunction' fun opts
                      else funToHelperFunction false fun opts <#> CurriedHelperFunction
  pure $ FunctionCodeBlock typeDecl helperFunction

newLine1 :: Array String -> String
newLine1 = joinWith "\n"

newLine2 :: Array String -> String
newLine2 = joinWith "\n\n"

instance codeFunctionCodeBlock :: Code CodeBlock where
  genCode (FunctionCodeBlock decl@(FunTypeDecl d) helper) opts = do
    let header = mkComment [d.typeName]
    declCode <- genCode decl opts
    helperCode <- genCode helper opts
    pure $
      newLine2
        [ header
        , declCode
        , helperCode
        ]
  genCode (EventCodeBlock decl@(EventDataDecl d) filterInst eventInst genericInst) opts = do
    let header = mkComment [d.constructor]
    declCode <- genCode decl opts
    filterInstCode <- genCode filterInst opts
    eventInstCode <- genCode eventInst opts
    genericInstCode <- genCode genericInst opts
    pure $
      newLine2
        [ header
        , declCode
        , filterInstCode
        , eventInstCode
        , genericInstCode
        ]

instance codeAbi :: Code (Abi Identity) where
  genCode (Abi abi) opts = do
    codes <- for abi $ un Identity >>> case _ of
      AbiFunction f -> do
        functionCodeBlock <- funToFunctionCodeBlock f opts
        genCode functionCodeBlock opts
      AbiEvent e -> do
        eventCodeBlock <- eventToEventCodeBlock e
        genCode eventCodeBlock opts
      AbiConstructor (SolidityConstructor c) -> do
        let f = SolidityFunction { name : "constructor"
                                 , inputs : c.inputs
                                 , outputs : []
                                 , constant : false
                                 , payable : false
                                 , isConstructor : true
                                 , isUnCurried: c.isUnCurried
                                 }
        functionCodeBlock <- funToFunctionCodeBlock f opts
        genCode functionCodeBlock opts
      AbiFallback _ ->
        -- Fallback is a function that gets called in case someone
        -- sends ether to the contract with no function specified
        -- so it's like, you would never call it on purpose, so we ignore it.
        pure ""
    pure $ newLine2 codes
