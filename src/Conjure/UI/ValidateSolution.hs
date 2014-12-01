module Conjure.UI.ValidateSolution ( validateSolution ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Instantiate ( instantiateExpression )


validateSolution
    :: ( MonadFail m
       , MonadLog m
       )
    => Model      -- essence model
    -> Model      -- essence param
    -> Model      -- essence solution
    -> m ()
validateSolution essenceModel essenceParam essenceSolution = flip evalStateT [] $
    forM_ (mStatements essenceModel) $ \ st -> case st of
        Declaration (FindOrGiven Given nm dom) ->
            case [ val | Declaration (Letting nm2 val) <- mStatements essenceParam, nm == nm2 ] of
                [val] -> modify ((nm, val) :)
                []    -> fail $ vcat [ "No value for" <+> pretty nm <+> "in the parameter file."
                                     , "Its domain:" <++> pretty dom
                                     ]
                vals  -> fail $ vcat [ "Multiple values for" <+> pretty nm <+> "in the parameter file."
                                     , "Its domain:" <++> pretty dom
                                     , "Values:" <++> vcat (map pretty vals)
                                     ]
        Declaration (FindOrGiven Find nm dom) ->
            case [ val | Declaration (Letting nm2 val) <- mStatements essenceSolution, nm == nm2 ] of
                [val] -> modify ((nm,val) :)
                []    -> fail $ vcat [ "No value for" <+> pretty nm <+> "in the solution file."
                                     , "Its domain:" <++> pretty dom
                                     ]
                vals  -> fail $ vcat [ "Multiple values for" <+> pretty nm <+> "in the solution file."
                                     , "Its domain:" <++> pretty dom
                                     , "Values:" <++> vcat (map pretty vals)
                                     ]
        Declaration (Letting nm val) -> modify ((nm, val) :)
        Declaration (GivenDomainDefnEnum nm) ->
            case [ val | Declaration (LettingDomainDefnEnum nm2 val) <- mStatements essenceParam, nm == nm2 ] of
                [val] -> do
                    let domain = DomainInt [RangeBounded (fromInt 1) (fromInt (length val))]
                    let values = [ (n, Constant (ConstantInt i))
                                 | (n, i) <- zip val allNats
                                 ]
                    modify (((nm, Domain domain) : values) ++)
                []    -> fail $ vcat [ "No value for enum domain" <+> pretty nm <+> "in the parameter file."
                                     ]
                vals  -> fail $ vcat [ "Multiple values for enum domain" <+> pretty nm <+> "in the parameter file."
                                     , "Values:" <++> vcat (map (prettyList prBraces ",") vals)
                                     ]
        Declaration (LettingDomainDefnEnum nm val) -> do
                    let domain = DomainInt [RangeBounded (fromInt 1) (fromInt (length val))]
                    let values = [ (n, Constant (ConstantInt i))
                                 | (n, i) <- zip val allNats
                                 ]
                    modify (((nm, Domain domain) : values) ++)
        Declaration (LettingDomainDefnUnnamed nm _) -> do
            case [ nms | Declaration (LettingDomainDefnEnum nm2 nms) <- mStatements essenceSolution , nm == nm2 ] of
                [nms] -> modify ( [ (n, Constant (ConstantInt i))
                                  | (i,n) <- zip allNats nms
                                  ] ++ )
                []    -> fail $ vcat [ "No value for unnamed domain" <+> pretty nm <+> "in the solution file."
                                     ]
                vals  -> fail $ vcat [ "Multiple values for unnamed domain" <+> pretty nm <+> "in the solution file."
                                     , "Values:" <++> vcat (map (prettyList prBraces ",") vals)
                                     ]
        SearchOrder{} -> return ()
        Where xs -> do
            vals     <- gets id
            forM_ xs $ \ x -> do
                constant <- instantiateExpression vals x
                case constant of
                    ConstantBool True -> return ()
                    _ -> fail $ "Invalid." <++> vcat [ "Statement evaluates to:" <+> pretty constant
                                                     , "Original statement was:" <+> pretty x
                                                     , hang "Relevant values:" 4 $ vcat
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
                    _ -> fail $ "Invalid." <++> vcat [ "Statement evaluates to:" <+> pretty constant
                                                     , "Original statement was:" <+> pretty x
                                                     , hang "Relevant values:" 4 $ vcat
                                                         [ "letting" <+> pretty nm <+> "be" <+> pretty val
                                                         | (nm, val) <- vals
                                                         , nm `elem` (universeBi x :: [Name])
                                                         ]
                                                     ]