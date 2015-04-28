{-# LANGUAGE RecordWildCards #-}

module Conjure.Language.Parser
    ( runLexerAndParser
    , parseIO
    , parseModel
    , parseTopLevels
    , parseExpr
    , parseDomain
    , parseDomainWithRepr
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Domain.AddAttributes
import Conjure.Language.Expression.Op
import Conjure.Language.Pretty
import Conjure.Language.Lexer ( Lexeme(..), LexemePos, lexemeFace, lexemeText, runLexer )

-- parsec
import Text.Parsec ( ParsecT, tokenPrim, try, (<?>), errorPos, sourceLine, sourceColumn, getState, modifyState )
import Text.Parsec.Combinator ( between, optionMaybe, sepBy, sepBy1, sepEndBy1, eof )
import qualified Text.Parsec as P ( runParser )

-- text
import qualified Data.Text as T

-- containers
import qualified Data.Set as S ( null, fromList, toList )


parseModel :: Parser Model
parseModel = inCompleteFile $ do
    let
        pLanguage :: Parser LanguageVersion
        pLanguage = do
            l  <- lexeme L_language *> identifierText
            is <- sepBy1 integer dot
            return (LanguageVersion (Name l) (map fromInteger is))
    l  <- pLanguage
    xs <- many parseTopLevels
    return Model
        { mLanguage = l
        , mStatements = concat xs
        , mInfo = def
        }


parseIO :: Parser a -> String -> IO a
parseIO p s =
    case runLexerAndParser (inCompleteFile p) "" (T.pack s) of
        Left err -> error (show err)
        Right x  -> return x


translateQnName :: Text -> Text
translateQnName qnName = case qnName of
    "forAll" -> "and"
    "exists" -> "or"
    _        -> qnName





--------------------------------------------------------------------------------
-- Actual parsers --------------------------------------------------------------
--------------------------------------------------------------------------------

parseTopLevels :: Parser [Statement]
parseTopLevels = do
    let one = msum $ map try
                [ do
                    lexeme L_find
                    decls <- flip sepEndBy1 comma $ do
                        is <- parseName `sepEndBy1` comma
                        j  <- colon >> parseDomain
                        return [ Declaration (FindOrGiven Find i j)
                               | i <- is ]
                    return $ concat decls
                    <?> "find statement"
                , do
                    lexeme L_given
                    decls <- flip sepEndBy1 comma $ do
                        is <- parseName `sepEndBy1` comma
                        msum
                            [ do
                                colon
                                j <- parseDomain
                                return [ Declaration (FindOrGiven Given i j)
                                       | i <- is ]
                            , do
                                lexeme L_new
                                msum
                                    [ do
                                        lexeme L_type
                                        lexeme L_enum
                                        modifyState (\ st -> st { enumDomains = is ++ enumDomains st } )
                                        return [ Declaration (GivenDomainDefnEnum i)
                                               | i <- is ]
                                    ]
                            ]
                    return $ concat decls
                    <?> "given statement"
                , do
                    lexeme L_letting
                    decls <- flip sepEndBy1 comma $ do
                        is <- parseName `sepEndBy1` comma
                        lexeme L_be
                        msum
                            [ do
                                lexeme L_new
                                lexeme L_type
                                msum
                                    [ do
                                        lexeme L_of
                                        lexeme $ LIdentifier "size"
                                        j <- parseExpr
                                        return [ Declaration (LettingDomainDefnUnnamed i j)
                                               | i <- is
                                               ]
                                    , do
                                        lexeme L_enum
                                        ys <- braces (parseName `sepBy` comma) <|> return []
                                        modifyState (\ st -> st { enumDomains = is ++ enumDomains st } )
                                        return [ Declaration (LettingDomainDefnEnum i ys)
                                               | i <- is
                                               ]
                                    ]
                            , do
                                lexeme L_domain
                                j <- parseDomain
                                return [ Declaration (Letting i (Domain j))
                                       | i <- is
                                       ]
                            , do
                                j <- parseExpr
                                return [ Declaration (Letting i j)
                                       | i <- is
                                       ]
                            ]
                    return $ concat decls
                    <?> "letting statement"
                , do
                    lexeme L_where
                    xs <- parseExpr `sepEndBy1` comma
                    return [Where xs]
                    <?> "where statement"
                , do
                    lexeme L_such
                    lexeme L_that
                    xs <- parseExpr `sepEndBy1` comma
                    return [SuchThat xs]
                    <?> "such that statement"
                , do
                    lexeme L_minimising
                    x <- parseExpr
                    return [ Objective Minimising x ]
                    <?> "objective"
                , do
                    lexeme L_maximising
                    x <- parseExpr
                    return [ Objective Maximising x ]
                    <?> "objective"
                , do
                    lexeme L_branching
                    lexeme L_on
                    xs <- brackets $ parseSearchOrder `sepBy` comma
                    return [ SearchOrder xs ]
                    <?> "branching on"
                ]
    concat <$> some one

parseSearchOrder :: Parser SearchOrder
parseSearchOrder = msum [try pBranchingOn, pCut]
    where
        pBranchingOn = BranchingOn <$> parseName
        pCut = Cut <$> parseExpr

parseRange :: Parser a -> Parser (Range a)
parseRange p = msum [try pRange, pSingle]
    where
        pRange = do
            fr <- optionMaybe p
            dot; dot
            to <- optionMaybe p
            return $ case (fr,to) of
                (Nothing, Nothing) -> RangeOpen
                (Just x , Nothing) -> RangeLowerBounded x
                (Nothing, Just y ) -> RangeUpperBounded y
                (Just x , Just y ) -> RangeBounded x y
        pSingle = do
            x <- p
            return (RangeSingle x)

parseDomain :: Parser (Domain () Expression)
parseDomain = forgetRepr <$> parseDomainWithRepr

parseDomainWithRepr :: Parser (Domain HasRepresentation Expression)
parseDomainWithRepr
    = shuntingYardDomain
    $ some
    $ msum [ Right <$> try pDomainAtom
           , Left  <$> parseOp'
           ]
    where
        parseOp' = msum [ do lexeme x; return x | x <- [L_Minus, L_union, L_intersect] ] <?> "operator"
        pDomainAtom = msum $ map try
            [ pBool, pInt, pEnum, pReference
            , pMatrix, pTupleWithout, pTupleWith
            , pRecord, pVariant
            , pSet, pMSet, pFunction, pFunction'
            , pSequence
            , pRelation
            , pPartition
            , pGraph
            , DomainMetaVar <$> parseMetaVariable, parens parseDomainWithRepr
            ]

        parseRepr = msum [ HasRepresentation <$> braces parseName
                         , return NoRepresentation
                         ]

        pBool = do
            lexeme L_bool
            return DomainBool

        pInt = do
            lexeme L_int
            mxs <- optionMaybe $ parens $ parseRange parseExpr `sepBy` comma
            let xs = fromMaybe [] mxs
            return $ DomainInt xs

        pReference = do
            r  <- identifierText
            return $ DomainReference (Name r) Nothing

        pEnum = do
            r  <- identifierText
            xs <- optionMaybe $ parens $ parseRange parseExpr `sepBy` comma
            st <- getState
            guard (Name r `elem` enumDomains st)
            return $ DomainEnum (Name r) xs Nothing

        pMatrix = do
            lexeme L_matrix
            lexeme L_indexed
            lexeme L_by
            xs <- brackets (parseDomain `sepBy1` comma)
            lexeme L_of
            y  <- parseDomainWithRepr
            return $ foldr DomainMatrix y xs

        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ parseDomainWithRepr `sepBy` comma
            return $ DomainTuple xs

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseDomainWithRepr comma
            return $ DomainTuple xs

        pRecord = do
            lexeme L_record
            let one = do n <- parseName
                         lexeme L_Colon
                         d <- parseDomainWithRepr
                         return (n,d)
            xs <- braces $ one `sepBy` comma
            return $ DomainRecord xs

        pVariant = do
            lexeme L_variant
            let one = do n <- parseName
                         lexeme L_Colon
                         d <- parseDomainWithRepr
                         return (n,d)
            xs <- braces $ one `sepBy` comma
            return $ DomainVariant xs

        pSet = do
            lexeme L_set
            r <- parseRepr
            x <- parseSetAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainSet r x y

        pMSet = do
            lexeme L_mset
            r <- parseRepr
            x <- parseMSetAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainMSet r x y

        pFunction' = do
            lexeme L_function
            r <- parseRepr
            (y,z) <- arrowedPair parseDomainWithRepr
            return $ DomainFunction r def y z

        pFunction = do
            lexeme L_function
            r <- parseRepr
            x <- parseFunctionAttr
            (y,z) <- arrowedPair parseDomainWithRepr
            return $ DomainFunction r x y z

        pSequence = do
            lexeme L_sequence
            r <- parseRepr
            x <- parseSequenceAttr
            y <- lexeme L_of >> parseDomainWithRepr
            return $ DomainSequence r x y

        pRelation = do
            lexeme L_relation
            r  <- parseRepr
            x  <- parseRelationAttr
            lexeme L_of
            ys <- parens (parseDomainWithRepr `sepBy` lexeme L_Times)
            let RelationAttr _ (BinaryRelationAttrs binAttrs) = x
            when (length ys /= 2 && not (S.null binAttrs)) $
                fail $ "Only binary relations can have these attributes:" <+>
                            prettyList id "," (S.toList binAttrs)
            return $ DomainRelation r x ys

        pPartition = do
            lexeme L_partition
            r <- parseRepr
            x <- parsePartitionAttr
            lexeme L_from
            y <- parseDomainWithRepr
            return $ DomainPartition r x y

        pGraph = do
            lexeme L_graph
            r <- parseRepr
            x <- parseGraphAttr
            lexeme L_of
            y <- parseDomainWithRepr
            return $ DomainGraph r x y

parseAttributes :: Parser (DomainAttributes Expression)
parseAttributes = do
    xs <- parens (parseAttribute `sepBy` comma) <|> return []
    return $ DomainAttributes xs
    where
        parseAttribute = msum [try parseNameValue, try parseDAName, parseDontCare]
        parseNameValue = DANameValue <$> (Name <$> identifierText) <*> parseExpr
        parseDAName = DAName <$> (Name <$> identifierText)
        parseDontCare = do dot; dot ; return DADotDot

parseSetAttr :: Parser (SetAttr Expression)
parseSetAttr = do
    DomainAttributes attrs <- parseAttributes
    SetAttr <$> case filterSizey attrs of
        [] -> return SizeAttr_None
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))

parseMSetAttr :: Parser (MSetAttr Expression)
parseMSetAttr = do
    DomainAttributes attrs <- parseAttributes
    size <- case filterSizey attrs of
        [] -> return SizeAttr_None
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    occur <- case filterAttrName ["minOccur", "maxOccur"] attrs of
        [] -> return OccurAttr_None
        [DANameValue "minOccur" a] -> return (OccurAttr_MinOccur a)
        [DANameValue "maxOccur" a] -> return (OccurAttr_MaxOccur a)
        [DANameValue "maxOccur" b, DANameValue "minOccur" a] -> return (OccurAttr_MinMaxOccur a b)
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    return (MSetAttr size occur)

parseFunctionAttr :: Parser (FunctionAttr Expression)
parseFunctionAttr = do
    DomainAttributes attrs <- parseAttributes
    size <- case filterSizey attrs of
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        [] -> return SizeAttr_None
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    let partiality = if DAName "total" `elem` attrs
                        then PartialityAttr_Total
                        else PartialityAttr_Partial
    jectivity  <- case filterJectivity attrs of
        [] -> return JectivityAttr_None
        [DAName "bijective" ] -> return JectivityAttr_Bijective
        [DAName "injective" ] -> return JectivityAttr_Injective
        [DAName "surjective"] -> return JectivityAttr_Surjective
        [DAName "injective", DAName "surjective"] -> return JectivityAttr_Bijective
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    return (FunctionAttr size partiality jectivity)

parseSequenceAttr :: Parser (SequenceAttr Expression)
parseSequenceAttr = do
    DomainAttributes attrs <- parseAttributes
    size <- case filterSizey attrs of
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        [] -> return SizeAttr_None
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    jectivity  <- case filterJectivity attrs of
        [] -> return JectivityAttr_None
        [DAName "bijective" ] -> return JectivityAttr_Bijective
        [DAName "injective" ] -> return JectivityAttr_Injective
        [DAName "surjective"] -> return JectivityAttr_Surjective
        [DAName "injective", DAName "surjective"] -> return JectivityAttr_Bijective
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    return (SequenceAttr size jectivity)

parseRelationAttr :: Parser (RelationAttr Expression)
parseRelationAttr = do
    DomainAttributes attrs <- parseAttributes
    size <- case filterSizey attrs of
        [] -> return SizeAttr_None
        [DANameValue "size"    a] -> return (SizeAttr_Size a)
        [DANameValue "minSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxSize" b, DANameValue "minSize" a] -> return (SizeAttr_MinMaxSize a b)
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    let readBinRel' (DAName (Name a)) = readBinRel (fromString (textToString a))
        readBinRel' a = fail $ "Not a binary relation attribute:" <+> pretty a
    binRels <- mapM readBinRel' (filterBinRel attrs)
    return (RelationAttr size (BinaryRelationAttrs (S.fromList binRels)))

parsePartitionAttr :: Parser (PartitionAttr Expression)
parsePartitionAttr = do
    DomainAttributes attrs <- parseAttributes
    unless (null $ filterAttrName ["complete"] attrs) $
        fail $ vcat [ "Partitions do not support the 'complete' attribute."
                    , "They are complete by default."
                    ]
    unless (null $ filterSizey attrs) $
        fail $ vcat [ "Partitions do not support these attributes:" <+> prettyList id "," (filterSizey attrs)
                    , "This is because partitions are complete by default."
                    ]
    partsNum         <- case filterAttrName ["numParts", "minNumParts", "maxNumParts"] attrs of
        [] -> return SizeAttr_None
        [DANameValue "numParts"    a] -> return (SizeAttr_Size a)
        [DANameValue "minNumParts" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxNumParts" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxNumParts" b, DANameValue "minNumParts" a] -> return (SizeAttr_MinMaxSize a b)
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    partsSize        <- case filterAttrName ["partSize", "minPartSize", "maxPartSize"] attrs of
        [] -> return SizeAttr_None
        [DANameValue "partSize"    a] -> return (SizeAttr_Size a)
        [DANameValue "minPartSize" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxPartSize" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxPartSize" b, DANameValue "minPartSize" a] -> return (SizeAttr_MinMaxSize a b)
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    let isRegular  = DAName "regular"  `elem` attrs
    return PartitionAttr {..}

parseGraphAttr :: Parser (GraphAttr Expression)
parseGraphAttr = do
    DomainAttributes attrs <- parseAttributes
    numVerts <- case filterAttrName ["numVerts", "minNumVerts", "maxNumVerts"] attrs of
        [] -> return SizeAttr_None
        [DANameValue "numVerts"    a] -> return (SizeAttr_Size a)
        [DANameValue "minNumVerts" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxNumVerts" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxNumVerts" b, DANameValue "minNumVerts" a] -> return (SizeAttr_MinMaxSize a b)
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    numEdges <- case filterAttrName ["numEdges", "minNumEdges", "maxNumEdges"] attrs of
        [] -> return SizeAttr_None
        [DANameValue "numEdges"    a] -> return (SizeAttr_Size a)
        [DANameValue "minNumEdges" a] -> return (SizeAttr_MinSize a)
        [DANameValue "maxNumEdges" a] -> return (SizeAttr_MaxSize a)
        [DANameValue "maxNumEdges" b, DANameValue "minNumEdges" a] -> return (SizeAttr_MinMaxSize a b)
        as -> fail ("incompatible attributes:" <+> stringToDoc (show as))
    let isComplete  = DAName "complete"  `elem` attrs
    return GraphAttr {..}

filterAttrName :: Ord a => [Name] -> [DomainAttribute a] -> [DomainAttribute a]
filterAttrName keep = sort . filter f
    where
        f (DANameValue nm _) | nm `elem` keep = True
        f (DAName      nm  ) | nm `elem` keep = True
        f _ = False

filterSizey :: Ord a => [DomainAttribute a] -> [DomainAttribute a]
filterSizey = filterAttrName ["size", "minSize", "maxSize"]

filterJectivity :: Ord a => [DomainAttribute a] -> [DomainAttribute a]
filterJectivity = filterAttrName ["injective", "surjective", "bijective"]

filterBinRel :: Ord a => [DomainAttribute a] -> [DomainAttribute a]
filterBinRel = filterAttrName
    [ "reflexive"
    , "irreflexive"
    , "coreflexive"
    , "symmetric"
    , "antiSymmetric"
    , "aSymmetric"
    , "transitive"
    , "total"
    , "connex"
    , "Euclidean"
    , "serial"
    , "equivalence"
    , "partialOrder"
    ]

parseMetaVariable :: Parser String
parseMetaVariable = do
    let isMeta LMetaVar {} = True
        isMeta _           = False
    LMetaVar iden <- satisfyT isMeta
    return (T.unpack iden)

metaVarInE :: String -> Expression
metaVarInE = ExpressionMetaVar

parseExpr :: Parser Expression
parseExpr = shuntingYardExpr parseBeforeShunt
    where
        parseBeforeShunt :: Parser [Either Lexeme Expression]
        parseBeforeShunt = some $ msum
            [ Right <$> try parseAtomicExpr
            , Left  <$> parseOp
            ]

parseAtomicExpr :: Parser Expression
parseAtomicExpr = do
    let
        prefixes = do
            fs <- some $ msum parsePrefixes
            return $ foldr1 (.) fs
        postfixes = do
            fs <- some $ msum parsePostfixes
            return $ foldr1 (.) (reverse fs)
        withPrefix  x = try x <|> do f <- prefixes; i <- x; return $ f i
        withPostfix x = do i <- x; mf <- optionMaybe postfixes; return $ case mf of Nothing -> i
                                                                                    Just f  -> f i
    withPrefix (withPostfix parseAtomicExprNoPrePost) <?> "expression"

parseAtomicExprNoPrePost :: Parser Expression
parseAtomicExprNoPrePost = msum $ map try $ concat
    [ [parseQuantifiedExpr]
    , parseOthers
    , [metaVarInE <$> parseMetaVariable]
    , [parseAAC]
    , [parseReference]
    , [parseLiteral]
    , [parseDomainAsExpr]
    , [parseWithLocals]
    , [parseComprehension]
    , [parens parseExpr]
    ]

parseComprehension :: Parser Expression
parseComprehension = brackets $ do
    x   <- parseExpr
    lexeme L_Bar
    gens <- sepBy1 (try generator <|> condition) comma
    return (Comprehension x (concat gens))
    where
        generator = do
            pats <- parseAbstractPattern `sepBy1` comma
            msum
                [ do
                    lexeme L_Colon
                    domain <- parseDomain
                    return [Generator (GenDomainNoRepr pat domain) | pat <- pats]
                , do
                    lexeme L_LeftArrow
                    expr <- parseExpr
                    return [Generator (GenInExpr       pat expr)   | pat <- pats]
                ]
        condition = return . Condition <$> parseExpr

parseDomainAsExpr :: Parser Expression
parseDomainAsExpr = Domain <$> betweenTicks parseDomain

parsePrefixes :: [Parser (Expression -> Expression)]
parsePrefixes = [parseUnaryMinus, parseUnaryNot]
    where
        parseUnaryMinus = do
            lexeme L_Minus
            return $ \ x -> mkOp "negate" [x]
        parseUnaryNot = do
            lexeme L_ExclamationMark
            return $ \ x -> mkOp "not" [x]

parsePostfixes :: [Parser (Expression -> Expression)]
parsePostfixes = [parseIndexed,parseFactorial,parseFuncApply]
    where
        parseIndexed :: Parser (Expression -> Expression)
        parseIndexed = do
            let
                pIndexer = try pRList <|> (do i <- parseExpr ; return $ \ m -> Op (MkOpIndexing (OpIndexing m i)))
                pRList   = do
                    i <- optionMaybe parseExpr
                    dot; dot
                    j <- optionMaybe parseExpr
                    return $ \ m -> Op (MkOpSlicing (OpSlicing m i j))
            is <- brackets $ pIndexer `sepBy1` comma
            return $ \ x -> foldl (\ m f -> f m ) x is
        parseFactorial :: Parser (Expression -> Expression)
        parseFactorial = do
            lexeme L_ExclamationMark            
            return $ \ x -> mkOp "factorial" [x]
        parseFuncApply :: Parser (Expression -> Expression)
        parseFuncApply = parens $ do
            xs <- parseExpr `sepBy1` comma
            let underscore = Reference "_" Nothing
            let ys = [ if underscore == x then Nothing else Just x | x <- xs ]
            return $ \ x -> Op $ MkOpRelationProj $ OpRelationProj x ys

parseAAC :: Parser Expression
parseAAC = do
    let
        isAttr (LIdentifier txt) | Just _ <- Name txt `lookup` allSupportedAttributes = True
        isAttr _ = False
    LIdentifier attr <- satisfyT isAttr
    let n = fromMaybe (bug "parseAAC") (lookup (Name attr) allSupportedAttributes)
    args <- parens $ countSep (n+1) parseExpr comma
    case (n, args) of
        (0, [e  ]) -> return $ Op $ MkOpAttributeAsConstraint $ OpAttributeAsConstraint e
                                        (fromString (textToString attr)) Nothing
        (1, [e,v]) -> return $ Op $ MkOpAttributeAsConstraint $ OpAttributeAsConstraint e
                                        (fromString (textToString attr)) (Just v)
        _ -> fail "parseAAC"

parseOthers :: [Parser Expression]
parseOthers = [ parseFunctional l
              | l <- functionals
              ] ++ [parseTyped, parseTwoBars]
    where

        parseTwoBars :: Parser Expression
        parseTwoBars = do
            x <- between (lexeme L_Bar) (lexeme L_Bar) parseExpr
            return (mkOp "twoBars" [x])

        parseTyped :: Parser Expression
        parseTyped = parens $ do
            x  <- parseExpr
            lexeme L_Colon
            d  <- betweenTicks parseDomain
            ty <- typeOfDomain d
            return (Typed x ty)

        parseFunctional :: Lexeme -> Parser Expression
        parseFunctional l = do
            lexeme l
            xs <- parens $ parseExpr `sepBy1` comma
            return $ case (l,xs) of
                (L_image, [y,z]) -> Op $ MkOpImage $ OpImage y z
                _ -> mkOp (fromString $ show $ lexemeFace l) xs

parseWithLocals :: Parser Expression
parseWithLocals = braces $ do
    i  <- parseExpr
    lexeme L_At
    js <- parseTopLevels
    let decls =
            [ Declaration (FindOrGiven LocalFind nm dom)
            | Declaration (FindOrGiven Find nm dom) <- js ]
    let cons = concat
            [ xs
            | SuchThat xs <- js
            ]
    let locals = if null decls
                    then Right cons
                    else Left (decls ++ [SuchThat cons])
    return (WithLocals i locals)

parseName :: Parser Name
parseName = Name <$> identifierText

parseReference :: Parser Expression
parseReference = Reference <$> parseName <*> pure Nothing

parseOp :: Parser Lexeme
parseOp = msum [ do lexeme x; return x | (x,_,_) <- operators ]
    <?> "operator"

parseQuantifiedExpr :: Parser Expression
parseQuantifiedExpr = do
    Name qnName <- parseName
    qnPats      <- parseAbstractPattern `sepBy1` comma
    qnOver      <- msum [ Left  <$> (colon *> parseDomain)
                        , Right <$> do
                            lexeme L_in
                            over <- parseExpr
                            return (\ pat -> GenInExpr pat over )
                        , Right <$> do
                            lexeme L_subsetEq
                            over <- parseExpr
                            return (\ pat -> GenInExpr pat (Op $ MkOpPowerSet $ OpPowerSet over) )
                        ]
    qnGuard     <- optionMaybe (comma *> parseExpr)
    qnBody      <- dot *> parseExpr <?> "expecting body of a quantified expression"

    let qnMap pat = case qnOver of
            Left dom -> GenDomainNoRepr pat dom
            Right op -> op pat

    return $ mkOp (translateQnName qnName)
           $ return
           $ Comprehension qnBody
           $ [ Generator (qnMap pat) | pat    <- qnPats    ] ++
             [ Condition g           | Just g <- [qnGuard] ]


parseAbstractPattern :: Parser AbstractPattern
parseAbstractPattern = msum $ map try
    [ AbstractPatternMetaVar <$> parseMetaVariable
    , Single <$> parseName
    , do
        void $ optionMaybe $ lexeme L_tuple
        xs <- parens $ parseAbstractPattern `sepBy1` comma
        return (AbsPatTuple xs)
    , do
        xs <- brackets $ parseAbstractPattern `sepBy1` comma
        return (AbsPatMatrix xs)
    , do
        xs <- braces $ parseAbstractPattern `sepBy1` comma
        return (AbsPatSet xs)
    ]

parseLiteral :: Parser Expression
parseLiteral = msum ( map try
    [ Constant <$> pBool
    , Constant <$> pInt
    , AbstractLiteral <$> pMatrix
    , AbstractLiteral <$> pMatrix'
    , AbstractLiteral <$> pTupleWith
    , AbstractLiteral <$> pTupleWithout
    , AbstractLiteral <$> pRecord
    , AbstractLiteral <$> pVariant
    , AbstractLiteral <$> pSet
    , AbstractLiteral <$> pMSet
    , AbstractLiteral <$> pFunction
    , AbstractLiteral <$> pSequence
    , AbstractLiteral <$> pRelation
    , AbstractLiteral <$> pPartition
    , AbstractLiteral <$> pGraph
    ] ) <?> "value"
    where
        pBool = do
            x <- False <$ lexeme L_false
                 <|>
                 True  <$ lexeme L_true
            return (ConstantBool x)

        pInt = ConstantInt . fromInteger <$> integer

        pMatrix = do
            xs <- brackets (sepBy parseExpr comma)
            let r = mkDomainIntB 1 (fromInt (genericLength xs))
            return (AbsLitMatrix r xs)

        pMatrix' = brackets $ do
            xs <- sepBy parseExpr comma
            lexeme L_SemiColon
            r <- parseDomain
            return (AbsLitMatrix r xs)

        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ sepBy parseExpr comma
            return (AbsLitTuple xs)

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseExpr comma
            return (AbsLitTuple xs)

        pRecord = do
            lexeme L_record
            let one = do n <- parseName
                         lexeme L_Eq
                         x <- parseExpr
                         return (n,x)
            xs <- braces $ one `sepBy` comma
            return $ AbsLitRecord xs

        pVariant = do
            lexeme L_variant
            let one = do n <- parseName
                         lexeme L_Eq
                         x <- parseExpr
                         return (n,x)
            (n,x) <- braces one
            return $ AbsLitVariant Nothing n x

        pSet = do
            xs <- braces (sepBy parseExpr comma)
            return (AbsLitSet xs)

        pMSet = do
            lexeme L_mset
            xs <- parens (sepBy parseExpr comma)
            return (AbsLitMSet xs)

        pFunction = do
            lexeme L_function
            xs <- parens (sepBy inner comma)
            return (AbsLitFunction xs)
            where
                inner = arrowedPair parseExpr

        pSequence = do
            lexeme L_sequence
            xs <- parens (sepBy parseExpr comma)
            return (AbsLitSequence xs)

        pRelation = do
            lexeme L_relation
            xs <- parens (sepBy (try pTupleWith <|> pTupleWithout) comma)
            return (AbsLitRelation [is | AbsLitTuple is <- xs])
            -- return [xMake| value.relation.values := xs |]

        pPartition = do
            lexeme L_partition
            xs <- parens (sepBy inner comma)
            return (AbsLitPartition xs)
            where
                inner = braces (sepBy parseExpr comma)

        pGraph = do
            lexeme L_graph
            xs <- parens (sepBy lFunc comma)
            return $ AbsLitGraph xs
            where
                lFunc = labelSet parseExpr aList
                aList = braces (sepBy parseExpr comma)

shuntingYardExpr :: Parser [Either Lexeme Expression] -> Parser Expression
shuntingYardExpr p = do
    let mergeOp = mkBinOp . lexemeText
    beforeShunt <- fixNegate <$> p
    if not $ checkAlternating beforeShunt
        then fail "Malformed expression, Shunting Yard failed."
        else shunt mergeOp beforeShunt

shuntingYardDomain :: (Eq a, Eq r) => Parser [Either Lexeme (Domain r a)] -> Parser (Domain r a)
shuntingYardDomain p = do
    let mergeOp op before after = DomainOp (Name (lexemeText op)) [before,after]
    beforeShunt <- p
    if not $ checkAlternating beforeShunt
        then fail "Malformed expression, Shunting Yard failed."
        else shunt mergeOp beforeShunt

fixNegate :: [Either Lexeme Expression] -> [Either Lexeme Expression]
fixNegate ( Right a
          : Right (Op (MkOpNegate (OpNegate b)))
          : cs
          ) = fixNegate $ Right a : Left L_Minus : Right b : cs
fixNegate (a:bs) = a : fixNegate bs
fixNegate [] = []

checkAlternating :: [Either a b] -> Bool
checkAlternating [Right _] = True
checkAlternating (Right _:Left _:rest) = checkAlternating rest
checkAlternating _ = False

shunt :: Eq a => (Lexeme -> a -> a -> a) -> [Either Lexeme a] -> Parser a
shunt mergeOp xs = do
    result <- findPivotOp xs
    case result of
        Left x -> return x
        Right (before, op, after) -> do
            b <- shunt mergeOp before
            a <- shunt mergeOp after
            return (mergeOp op b a)

findPivotOp :: Eq a => [Either Lexeme a] -> Parser (Either a ([Either Lexeme a], Lexeme, [Either Lexeme a]))
findPivotOp [Right x] = return $ Left x
findPivotOp xs = do
    let
        pivotPrec :: Int
        pivotFixity :: Fixity
        (pivotPrec,pivotFixity) = minimumBy (comparing fst)
                        [ (p, f) | Left l <- xs, (l',f,p) <- operators, l == l' ]

        chck op = case [ p | (l,_,p) <- operators, l == op ] of
                    [p] -> p == pivotPrec
                    _ -> False

        findFirst :: Eq a => [Either Lexeme a] -> Parser ([Either Lexeme a], Lexeme, [Either Lexeme a])
        findFirst [] = fail "findPivotOp.findFirst"
        findFirst (Left i:is) | chck i = return ([], i, is)
        findFirst (i:is) = do
            (before, op, after) <- findFirst is
            return (i:before, op, after)

        findLast :: Eq a => [Either Lexeme a] -> Parser ([Either Lexeme a], Lexeme, [Either Lexeme a])
        findLast is = do
            (before, op, after) <- findFirst (reverse is)
            return (reverse after, op, reverse before)

        findOnly :: Eq a => [Either Lexeme a] -> Parser ([Either Lexeme a], Lexeme, [Either Lexeme a])
        findOnly is = do
            f <- findFirst is
            l <- findLast  is
            if f == l
                then return f
                else fail "Ambiguous use of non-associative operator."

    let
        finder = case pivotFixity of
                    FLeft  -> findLast
                    FNone  -> findOnly
                    FRight -> findFirst
    Right <$> finder xs




data ParserState = ParserState { enumDomains :: [Name] }
type Parser a = ParsecT [LexemePos] ParserState Identity a

runLexerAndParser :: MonadFail m => Parser a -> String -> T.Text -> m a
runLexerAndParser p s inp = do
    ls <- runLexer inp
    case runParser p s ls of
        Left (msg, line, col) ->
            let theLine = T.lines inp `at` (line - 1)
            in  fail $ vcat
                    [ msg
                    , pretty theLine
                    , pretty $ replicate (col-1) ' ' ++ "^"
                    ]
        Right x -> return x

runParser :: Parser a -> String -> [LexemePos] -> Either (Doc, Int, Int) a
runParser p s ls = either modifyErr Right (P.runParser p (ParserState []) s ls)
    where
        modifyErr e = Left $
            let pos  = errorPos e
                eDoc = pretty $ show e
            in  ( vcat [ pretty s <+> eDoc
                       , pretty (show pos)
                       ]
                , sourceLine   pos
                , sourceColumn pos
                )

identifierText :: Parser T.Text
identifierText = do
    LIdentifier i <- satisfyT isIdentifier
    return i
    where isIdentifier LIdentifier {} = True
          isIdentifier _ = False

satisfyT :: (Lexeme -> Bool) -> Parser Lexeme
satisfyT predicate = tokenPrim showTok nextPos testTok
    where
        showTok              = show . lexemeFace . fst
        testTok (tok, _)     = if predicate tok then Just tok else Nothing
        nextPos _ (_, pos) _ = pos

integer :: Parser Integer
integer = do
    LIntLiteral i <- satisfyT isInt
    return i
    where isInt LIntLiteral {} = True
          isInt _ = False

comma :: Parser ()
comma = lexeme L_Comma <?> "comma"

dot :: Parser ()
dot = lexeme L_Dot <?> "dot"

colon :: Parser ()
colon = lexeme L_Colon <?> "colon"


-- parses a specified number of elements separated by the given separator
countSep :: Int -> Parser a -> Parser sep -> Parser [a]
countSep 1 p _ = (:[]) <$> p
countSep i p separator | i > 1 = (:) <$> (p <* separator) <*> countSep (i-1) p separator
countSep _ _ _ = return []

-- parses at least a given number of elements separated by the given separator
countSepAtLeast :: Int -> Parser a -> Parser sep -> Parser [a]
countSepAtLeast i p separator = (++) <$> countSep i p separator <*> many (separator *> p)

betweenTicks :: Parser a -> Parser a
betweenTicks = between (lexeme L_BackTick) (lexeme L_BackTick)

parens :: Parser a -> Parser a
parens = between (lexeme L_OpenParen) (lexeme L_CloseParen)

braces :: Parser a -> Parser a
braces = between (lexeme L_OpenCurly) (lexeme L_CloseCurly)

brackets :: Parser a -> Parser a
brackets = between (lexeme L_OpenBracket) (lexeme L_CloseBracket)

lexeme :: Lexeme -> Parser ()
lexeme l = void (satisfyT (l==)) <?> show (lexemeFace l)

arrowedPair :: Parser a -> Parser (a,a)
arrowedPair p = do
    i <- p
    lexeme L_LongArrow
    j <- p
    return (i,j)

labelSet :: Parser a -> Parser b -> Parser (a,b)
labelSet p1 p2 = do
    i <- p1
    lexeme L_LongArrow
    j <- p2
    return (i,j)

inCompleteFile :: Parser a -> Parser a
inCompleteFile parser = do
    result <- parser
    eof
    return result

