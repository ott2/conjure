module Conjure.UI.ValidateSolution ( validateSolution ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.Instantiate
import Conjure.Process.Enumerate ( EnumerateDomain )


validateSolution
    :: ( MonadFail m
       , EnumerateDomain m
       )
    => Model      -- essence model
    -> Model      -- essence param
    -> Model      -- essence solution
    -> m ()
validateSolution essenceModel essenceParam essenceSolution = flip evalStateT [] $
  forM_ (mStatements essenceModel) $ \ st -> do
    mapM_ introduceRecordFields (universeBi st :: [Domain () Expression])
    case st of
        Declaration (FindOrGiven Given nm dom) ->
            case [ val | Declaration (Letting nm2 val) <- mStatements essenceParam, nm == nm2 ] of
                [val] -> do
                    valC                  <- gets id >>= flip instantiateExpression val
                    DomainInConstant domC <- gets id >>= flip instantiateExpression (Domain dom)
                    either userErr1 return (validateConstantForDomain nm valC domC)
                    modify ((nm, Constant valC) :)
                []    -> userErr1 $ vcat [ "No value for" <+> pretty nm <+> "in the parameter file."
                                         , "Its domain:" <++> pretty dom
                                         ]
                vals  -> userErr1 $ vcat [ "Multiple values for" <+> pretty nm <+> "in the parameter file."
                                         , "Its domain:" <++> pretty dom
                                         , "Values:" <++> vcat (map pretty vals)
                                         ]
        Declaration (FindOrGiven Find nm dom) ->
            case [ val | Declaration (Letting nm2 val) <- mStatements essenceSolution, nm == nm2 ] of
                [val] -> do
                    valC                  <- gets id >>= flip instantiateExpression val
                    DomainInConstant domC <- gets id >>= flip instantiateExpression (Domain dom)
                    either userErr1 return (validateConstantForDomain nm valC domC)
                    modify ((nm, Constant valC) :)
                []    -> userErr1 $ vcat [ "No value for" <+> pretty nm <+> "in the solution file."
                                         , "Its domain:" <++> pretty dom
                                         ]
                vals  -> userErr1 $ vcat [ "Multiple values for" <+> pretty nm <+> "in the solution file."
                                         , "Its domain:" <++> pretty dom
                                         , "Values:" <++> vcat (map pretty vals)
                                         ]
        Declaration (FindOrGiven Quantified _ _) ->
            userErr1 $ vcat
                [ "A quantified declaration at the top level."
                , "This should never happen."
                , "Statement:" <+> pretty st
                ]
        Declaration (FindOrGiven LocalFind _ _) ->
            userErr1 $ vcat
                [ "A local decision variable at the top level."
                , "This should never happen."
                , "Statement:" <+> pretty st
                ]
        Declaration (FindOrGiven CutFind _ _) ->
            userErr1 $ vcat
                [ "A 'cut' decision variable at the top level."
                , "This should never happen."
                , "Statement:" <+> pretty st
                ]
        Declaration (Letting nm val) -> modify ((nm, val) :)
        Declaration (GivenDomainDefnEnum nm) ->
            case [ val | Declaration (LettingDomainDefnEnum nm2 val) <- mStatements essenceParam, nm == nm2 ] of
                [val] -> do
                    let domain = mkDomainIntB 1 (fromInt (genericLength val))
                    let values = [ (n, Constant (ConstantInt i))
                                 | (n, i) <- zip val allNats
                                 ]
                    modify (((nm, Domain domain) : values) ++)
                []    -> userErr1 $ vcat [ "No value for enum domain" <+> pretty nm <+> "in the parameter file."
                                         ]
                vals  -> userErr1 $ vcat [ "Multiple values for enum domain" <+> pretty nm <+> "in the parameter file."
                                         , "Values:" <++> vcat (map (prettyList prBraces ",") vals)
                                         ]
        Declaration (LettingDomainDefnEnum nm val) -> do
                    let domain = mkDomainIntB 1 (fromInt (genericLength val))
                    let values = [ (n, Constant (ConstantInt i))
                                 | (n, i) <- zip val allNats
                                 ]
                    modify (((nm, Domain domain) : values) ++)
        Declaration (LettingDomainDefnUnnamed nm _) ->
            case [ nms | Declaration (LettingDomainDefnEnum nm2 nms) <- mStatements essenceSolution , nm == nm2 ] of
                [nms] -> do
                    let domain = mkDomainIntB 1 (fromInt (genericLength nms))
                    let values = [ (n, Constant (ConstantInt i))
                                 | (i,n) <- zip allNats nms
                                 ]
                    modify (((nm, Domain domain) : values) ++)
                []    -> userErr1 $ vcat [ "No value for unnamed domain" <+> pretty nm <+> "in the solution file."
                                         ]
                vals  -> userErr1 $ vcat [ "Multiple values for unnamed domain" <+> pretty nm <+> "in the solution file."
                                         , "Values:" <++> vcat (map (prettyList prBraces ",") vals)
                                         ]
        SearchOrder{} -> return ()
        SearchHeuristic{} -> return ()
        Where xs -> do
            vals     <- gets id
            forM_ xs $ \ x -> do
                constant <- instantiateExpression vals x
                case constant of
                    ConstantBool True -> return ()
                    _ -> userErr1 $ "Invalid." <++> vcat [ "Statement evaluates to:" <+> pretty constant
                                                         , "Original statement was:" <+> pretty x
                                                         , "Relevant values:" <++> vcat
                                                             [ "letting" <+> pretty nm <+> "be" <+> pretty val
                                                             | (nm, val) <- vals
                                                             , nm `elem` (universeBi x :: [Name])
                                                             ]
                                                         ]
        Objective{} -> return ()
        SuchThat xs -> do
            vals     <- gets id
            forM_ xs $ \ x -> do
                constant <- instantiateExpression vals x
                case constant of
                    ConstantBool True -> return ()
                    _ -> userErr1 $ "Invalid." <++> vcat [ "Statement evaluates to:" <+> pretty constant
                                                         , "Original statement was:" <+> pretty x
                                                         , "Relevant values:" <++> vcat
                                                             [ "letting" <+> pretty nm <+> "be" <+> pretty val
                                                             | (nm, val) <- vals
                                                             , nm `elem` (universeBi x :: [Name])
                                                             ]
                                                         ]


introduceRecordFields
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , Pretty r
       , Pretty x
       , TypeOf x
       )
    => Domain r x -> m ()
introduceRecordFields (DomainRecord inners) =
    forM_ inners $ \ (n, d) -> do
        t <- typeOf d
        modify ((n, Constant (ConstantField n t)) :)
introduceRecordFields (DomainVariant inners) =
    forM_ inners $ \ (n, d) -> do
        t <- typeOf d
        modify ((n, Constant (ConstantField n t)) :)
introduceRecordFields _ = return ()
