{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conjure.Representations
    ( down, down_, up
    , down1, down1_, up1, dispatch
    ) where

-- conjure
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty
import Conjure.Language.DomainSize ( domainSizeConstant )
import Conjure.Language.ZeroVal ( zeroVal )

-- text
import Data.Text ( pack )


type D = Domain HasRepresentation Constant

type Representation m
    =  Text
    -> D
    -> m (RepresentationResult m)

type RepresentationResult m =
    ( m (Maybe [(Text, D)])
    , Constant -> m (Maybe [(Text, D, Constant)])
    , [(Text, Constant)] -> m (Text, Constant)
    )


down1
    :: (Applicative m, MonadError Doc m)
    => Representation m
    ->           (Text, D)
    -> m (Maybe [(Text, D)])
down1 repr (name, domain) = repr name domain >>= fst3

down1_
    :: (Applicative m, MonadError Doc m)
    => Representation m
    ->           (Text, D, Constant)
    -> m (Maybe [(Text, D, Constant)])
down1_ repr (name, domain, constant) = repr name domain >>= \ r -> snd3 r constant

up1
    :: (Applicative m, MonadError Doc m)
    => Representation m
    ->   (Text, D)
    ->  [(Text, Constant)]
    -> m (Text, Constant)
up1 repr (name, domain) ctxt = repr name domain >>= \ r -> thd3 r ctxt


down
    :: (Applicative m, MonadError Doc m)
    =>    (Text, D)
    -> m [(Text, D)]
down inp = do
    mout <- down1 dispatch inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM down outs

down_
    :: (Applicative m, MonadError Doc m)
    =>    (Text, D, Constant)
    -> m [(Text, D, Constant)]
down_ inp = do
    mout <- down1_ dispatch inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM down_ outs

up
    :: (Applicative m, MonadError Doc m)
    =>   (Text, D)
    ->  [(Text, Constant)]
    -> m (Text, Constant)
up (name, highDomain) ctxt = do
    toDescend'
        :: Maybe [(Text, D)]
        <- down1 dispatch (name, highDomain)
    case toDescend' of
        Nothing ->
            case lookup name ctxt of
                Nothing -> throwError $ vcat
                    $ ("No value for:" <+> pretty name)
                    : "Bindings in context:"
                    : prettyContext ctxt
                Just val -> return (name, val)
        Just toDescend -> do
            midConstants
                 :: [(Text, Constant)]
                 <- sequence [ up (n,d) ctxt | (n,d) <- toDescend ]
            up1 dispatch (name, highDomain) midConstants


dispatch :: (Applicative m, MonadError Doc m) => Representation m
dispatch name domain =
    case domain of
        DomainBool{} -> return $ primitive name
        DomainInt {} -> return $ primitive name
        DomainTuple ds -> return $ tuple name ds
        DomainMatrix index inner -> return $ matrix name index inner
        DomainSet "Explicit" (SetAttrSize size) inner -> return $ setExplicit name size inner
        DomainSet "ExplicitVarSizeWithMarker" attrs inner -> return $ setExplicitVarSizeWithMarker name attrs inner
        DomainSet "ExplicitVarSizeWithFlags" attrs inner -> return $ setExplicitVarSizeWithFlags name attrs inner
        _ -> throwError $ vcat
                [ "No representation for the domain of:" <+> pretty name
                , "The domain:" <+> pretty domain
                ]


primitive :: MonadError Doc m => Text -> RepresentationResult m
primitive name =
    ( return Nothing
    , const $ return Nothing
    , \ ctxt ->
        case lookup name ctxt of
            Nothing -> throwError $ vcat
                $ ("No value for:" <+> pretty name)
                : "Bindings in context:"
                : prettyContext ctxt
            Just c  -> return (name, c)
    )


tuple :: MonadError Doc m => Text -> [D] -> RepresentationResult m
tuple name ds = 
    ( tupleDown
    , tupleDown_
    , tupleUp
    )

    where

        mkName i = mconcat [name, "_", pack (show (i :: Int))]

        tupleDown = return $ Just
            [ (mkName i, d)
            | i <- [1..]
            | d <- ds
            ]

        -- TODO: check if (length ds == length cs)
        tupleDown_ (ConstantTuple cs) = return $ Just
            [ (mkName i, d, c)
            | i <- [1..]
            | d <- ds
            | c <- cs
            ]
        tupleDown_ _ = return Nothing

        tupleUp ctxt = do
            let names = map mkName [1 .. length ds]
            vals <- forM names $ \ n ->
                case lookup n ctxt of
                    Nothing -> throwError $ vcat $
                        [ "No value for:" <+> pretty n
                        , "When working on:" <+> pretty name
                        , "With domain:" <+> pretty (DomainTuple ds)
                        ] ++
                        ("Bindings in context:" : prettyContext ctxt)
                    Just val -> return val
            -- TODO: check if (length ds == length vals)
            return (name, ConstantTuple vals)


matrix :: (Applicative m, MonadError Doc m) => Text -> Domain () Constant -> D -> RepresentationResult m
matrix name indexDomain innerDomain = 
    ( matrixDown
    , matrixDown_
    , matrixUp
    )

    where

        matrixDown = do
            mres <- down1 dispatch (name, innerDomain)
            case mres of
                Nothing -> return Nothing
                Just mids -> return $ Just [ (n, DomainMatrix indexDomain d) | (n, d) <- mids ]

        -- TODO: check if indices are the same
        matrixDown_ (ConstantMatrix _indexDomain2 constants) = do
            mids1
                :: [Maybe [(Text, D, Constant)]]
                <- sequence [ down1_ dispatch (name, innerDomain, c) | c <- constants ]
            let mids2 = catMaybes mids1
            if null mids2                                       -- if all were `Nothing`s
                then return Nothing
                else
                    if length mids2 == length mids1             -- if all were `Just`s
                        then do
                            let
                                mids3 :: [(Text, D, [Constant])]
                                mids3 = [ ( head [ n | (n,_,_) <- line ]
                                          , head [ d | (_,d,_) <- line ]
                                          ,      [ c | (_,_,c) <- line ]
                                          )
                                        | line <- transpose mids2
                                        ]
                            return $ Just
                                [ ( n
                                  , DomainMatrix indexDomain d
                                  , ConstantMatrix indexDomain cs
                                  )
                                | (n, d, cs) <- mids3
                                ]
                        else
                            throwError $ vcat
                                [ "This is weird. Heterogeneous matrix literal?"
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                ]
        matrixDown_ _ = return Nothing

        matrixUp ctxt = do

            mid1
                :: Maybe [(Text, D)]
                <- down1 dispatch (name, innerDomain)

            case mid1 of
                Nothing ->
                    -- the inner domain doesn't require refinement
                    -- there needs to be a binding with "name"
                    -- and we just pass it through
                    case lookup name ctxt of
                        Nothing -> throwError $ vcat $
                            [ "No value for:" <+> pretty name
                            , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                            ] ++
                            ("Bindings in context:" : prettyContext ctxt)
                        Just constant -> return (name, constant)
                Just mid2 -> do
                    -- the inner domain needs refinement
                    -- there needs to be bindings for each name in (map fst mid2)
                    -- we find those bindings, call (up1 dispatch name inner) on them, then lift
                    mid3
                        :: [(Text, [Constant])]
                        <- forM mid2 $ \ (n, _) -> do
                            case lookup n ctxt of
                                Nothing -> throwError $ vcat $
                                    [ "No value for:" <+> pretty n
                                    , "When working on:" <+> pretty name
                                    , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                    ] ++
                                    ("Bindings in context:" : prettyContext ctxt)
                                Just constant ->
                                    -- this constant is a ConstantMatrix, containing one component of the things to go into up1
                                    case constant of
                                        ConstantMatrix _ c -> return (n,c)
                                        _ -> throwError $ vcat
                                            [ "Expecting a matrix literal for:" <+> pretty n
                                            , "But got:" <+> pretty constant
                                            , "When working on:" <+> pretty name
                                            , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                            ]

                    let midNames     = map fst mid3
                    let midConstants = map snd mid3

                    mid4
                        :: [(Text, Constant)]
                        <- sequence
                            [ up1 dispatch (name, innerDomain) (zip midNames cs)
                            | cs <- transpose midConstants
                            ]
                    let values = map snd mid4
                    return (name, ConstantMatrix indexDomain values)


setExplicit :: MonadError Doc m => Text -> Constant -> D -> RepresentationResult m
setExplicit name size innerDomain = 
    ( setDown
    , setDown_
    , setUp
    )

    where

        outName = mconcat [name, "_Explicit"] 
        outIndexDomain = DomainInt [RangeBounded (ConstantInt 1) size]

        setDown = return $ Just
            [ ( outName
              , DomainMatrix outIndexDomain innerDomain
              ) ]

        setDown_ (ConstantSet constants) = return $ Just
            [ ( outName
              , DomainMatrix   outIndexDomain innerDomain
              , ConstantMatrix outIndexDomain constants
              ) ]
        setDown_ _ = return Nothing

        setUp ctxt =
            case lookup outName ctxt of
                Nothing -> throwError $ vcat $
                    [ "No value for:" <+> pretty outName
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty (DomainSet "Explicit" (SetAttrSize size) innerDomain)
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant ->
                    case constant of
                        ConstantMatrix _ vals ->
                            return (name, ConstantSet vals)
                        _ -> throwError $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty outName
                                , "But got:" <+> pretty constant
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty (DomainSet "Explicit" (SetAttrSize size) innerDomain)
                                ]


setExplicitVarSizeWithMarker :: (Applicative m, MonadError Doc m) => Text -> SetAttr Constant -> D -> RepresentationResult m
setExplicitVarSizeWithMarker name attrs innerDomain = 
    ( setDown
    , setDown_
    , setUp
    )

    where

        thisRepr = "ExplicitVarSizeWithMarker"
        thisFullDomain = DomainSet (HasRepresentation (Name thisRepr)) attrs innerDomain

        nameMarker = mconcat [name, "_", thisRepr, "_Marker"]
        nameMain   = mconcat [name, "_", thisRepr, "_Main"  ]

        maxSize = case attrs of
            SetAttrMaxSize x -> return x
            SetAttrMinMaxSize _ x -> return x
            _ -> ConstantInt <$> domainSizeConstant innerDomain

        genIndexDomain = maxSize >>= \ x -> return $ DomainInt [RangeBounded (ConstantInt 1) x]

        setDown = do
            indexDomain :: D <- genIndexDomain
            return $ Just
                [ ( nameMarker
                  , indexDomain
                  )
                , ( nameMain
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]

        setDown_ (ConstantSet constants) = do
            indexDomain :: D <- genIndexDomain

            z <- zeroVal innerDomain
            maxSizeInt <- do
                x <- maxSize
                case x of
                    ConstantInt i -> return i
                    _ -> throwError $ "Attribute expected to be an int in:" <+> pretty thisFullDomain
            let zeroes = replicate (maxSizeInt - length constants) z

            return $ Just
                [ ( nameMarker
                  , indexDomain
                  , ConstantInt (length constants)
                  )
                , ( nameMain
                  , DomainMatrix   (forgetRepr indexDomain) innerDomain
                  , ConstantMatrix (forgetRepr indexDomain) (constants ++ zeroes)
                  )
                ]
        setDown_ _ = return Nothing

        setUp ctxt =
            case (lookup nameMarker ctxt, lookup nameMain ctxt) of
                (Just marker, Just constantMatrix) ->
                    case marker of
                        ConstantInt card ->
                            case constantMatrix of
                                ConstantMatrix _ vals ->
                                    return (name, ConstantSet (take card vals))
                                _ -> throwError $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty nameMain
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty thisFullDomain
                                        ]
                        _ -> throwError $ vcat
                                [ "Expecting an integer literal for:" <+> pretty nameMarker
                                , "But got:" <+> pretty marker
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty thisFullDomain
                                ]
                (Nothing, _) -> throwError $ vcat $
                    [ "No value for:" <+> pretty nameMarker
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty thisFullDomain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> throwError $ vcat $
                    [ "No value for:" <+> pretty nameMain
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty thisFullDomain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)


setExplicitVarSizeWithFlags :: (Applicative m, MonadError Doc m) => Text -> SetAttr Constant -> D -> RepresentationResult m
setExplicitVarSizeWithFlags name attrs innerDomain = 
    ( setDown
    , setDown_
    , setUp
    )

    where

        thisRepr = "ExplicitVarSizeWithFlags"
        thisFullDomain = DomainSet (HasRepresentation (Name thisRepr)) attrs innerDomain

        nameFlag = mconcat [name, "_", thisRepr, "_Flags"]
        nameMain = mconcat [name, "_", thisRepr, "_Main"]

        maxSize = case attrs of
            SetAttrMaxSize x -> return x
            SetAttrMinMaxSize _ x -> return x
            _ -> ConstantInt <$> domainSizeConstant innerDomain

        genIndexDomain = maxSize >>= \ x -> return $ DomainInt [RangeBounded (ConstantInt 1) x]

        setDown = do
            indexDomain :: D <- genIndexDomain
            return $ Just
                [ ( nameFlag
                  , DomainMatrix (forgetRepr indexDomain) DomainBool
                  )
                , ( nameMain
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]

        setDown_ (ConstantSet constants) = do
            indexDomain :: D <- genIndexDomain

            z <- zeroVal innerDomain
            maxSizeInt <- do
                x <- maxSize
                case x of
                    ConstantInt i -> return i
                    _ -> throwError $ "Attribute expected to be an int in:" <+> pretty thisFullDomain
            let zeroes = replicate (maxSizeInt - length constants) z

            let trues  = replicate (length constants)              (ConstantBool True)
            let falses = replicate (maxSizeInt - length constants) (ConstantBool False)

            return $ Just
                [ ( nameFlag
                  , DomainMatrix   (forgetRepr indexDomain) DomainBool
                  , ConstantMatrix (forgetRepr indexDomain) (trues ++ falses)
                  )
                , ( nameMain
                  , DomainMatrix   (forgetRepr indexDomain) innerDomain
                  , ConstantMatrix (forgetRepr indexDomain) (constants ++ zeroes)
                  )
                ]
        setDown_ _ = return Nothing

        setUp ctxt =
            case (lookup nameFlag ctxt, lookup nameMain ctxt) of
                (Just flagMatrix, Just constantMatrix) ->
                    case flagMatrix of
                        -- TODO: check if indices match
                        ConstantMatrix _ flags ->
                            case constantMatrix of
                                ConstantMatrix _ vals ->
                                    return (name, ConstantSet [ v
                                                              | (i,v) <- zip flags vals
                                                              , i == ConstantBool True
                                                              ] )
                                _ -> throwError $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty nameMain
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty thisFullDomain
                                        ]
                        _ -> throwError $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty nameFlag
                                , "But got:" <+> pretty flagMatrix
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty thisFullDomain
                                ]
                (Nothing, _) -> throwError $ vcat $
                    [ "No value for:" <+> pretty nameFlag
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty thisFullDomain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> throwError $ vcat $
                    [ "No value for:" <+> pretty nameMain
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty thisFullDomain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)

