module Plugin (plugin) where

import CoreSyn
import GhcPlugins

import Control.Applicative

import Data.List
import Control.Monad

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    reinitializeGlobals
    putMsgS "Hello SrcLoc!"
    return $ srcloc_pass : todo
  where
    srcloc_pass = CoreDoPluginPass "SrcLoc" (bindsOnlyPass srcLocPass)

srcLocPass :: [CoreBind] -> CoreM [CoreBind]
srcLocPass binds = do
--    dflags <- getDynFlags
    liftIO $ putStrLn (showSDoc (ppr binds))
--     mapM_ (\ (v,e) -> do
--         liftIO $ putStrLn $ " == " ++ shw v ++ " == "
--         printSrcLocs e) (flattenBinds binds)
    binds' <- addBndrs <=< addLocs $ binds
    liftIO $ putStrLn (showSDoc (ppr binds'))
    return binds'

addLocs :: [CoreBind] -> CoreM [CoreBind]
addLocs binds =
    case find (\ (v,_) -> "addLoc" == varString v)
              (flattenBinds binds) of
        Nothing -> do
            putMsgS $ "Did not find addLoc!"
            return binds
        Just (add_loc_id,_e) -> do
            putMsgS $ "Found " ++ shw add_loc_id ++ " :: " ++ shw (varType add_loc_id)
            case splitFunTys (varType add_loc_id) of
                ([_string,_x_,_y,ex_ty],ex_ty') | ex_ty `eqType` ex_ty' -> do
                    putMsgS $ "Expression type is " ++ shw ex_ty
                    let add_loc = addLoc add_loc_id ex_ty
                        add_loc_to_binds :: CoreBind -> CoreM CoreBind
                        add_loc_to_binds (NonRec v e) = NonRec v <$> add_loc e
                        add_loc_to_binds (Rec vses) = Rec <$> sequence [ (,) v <$> add_loc e | (v,e) <- vses ]
                    mapM add_loc_to_binds binds
                _ -> do
                    putMsgS "Wrong type!"
                    return binds

addLoc :: Id -> Type -> CoreExpr -> CoreM CoreExpr
addLoc add_loc_id expr_type = go where
    go :: CoreExpr -> CoreM CoreExpr
    go t = case t of
        Var x -> tryAdd (getSrcLoc x)
        App e1 e2 -> App <$> go e1 <*> go e2
        Lam x e -> Lam x <$> go e
        _ -> return t
        -- ^ Just give up for now
      where
        tryAdd :: SrcLoc -> CoreM CoreExpr
        tryAdd (RealSrcLoc src_loc) | exprType t `eqType` expr_type = do
            fs <- mkStringExprFS (srcLocFile src_loc)
            let xi = mkIntExprInt (srcLocLine src_loc)
                yi = mkIntExprInt (srcLocCol src_loc)
                res = mkCoreApps
                        (Var add_loc_id)
                        [ fs
                        , xi
                        , yi
                        , t
                        ]
            putMsgS $ shw t ++ " ~> " ++ shw res
            return res
        tryAdd _ = return t

varString :: Var -> String
varString = occNameString . nameOccName . varName

addBndrs :: [CoreBind] -> CoreM [CoreBind]
addBndrs binds =
    case find (\ (v,_) -> "addBndr" == varString v)
              (flattenBinds binds) of
        Nothing -> do
            putMsgS $ "Did not find addLoc!"
            return binds
        Just (add_bndr_id,_e) -> do
            putMsgS $ "Found " ++ shw add_bndr_id ++ " :: " ++ shw (varType add_bndr_id)
            case splitFunTys (varType add_bndr_id) of
                ([_string,ex_ty],ex_ty') | ex_ty `eqType` ex_ty' -> do
                    putMsgS $ "Expression type is " ++ shw ex_ty
                    let add_bndr = addBndr add_bndr_id ex_ty
                        add_bndr_to_binds :: CoreBind -> CoreM CoreBind
                        add_bndr_to_binds (NonRec v e) = NonRec v <$> add_bndr e
                        add_bndr_to_binds (Rec vses) = Rec <$> sequence [ (,) v <$> add_bndr e | (v,e) <- vses ]
                    mapM add_bndr_to_binds binds
                _ -> do
                    putMsgS "Wrong type!"
                    return binds

addBndr :: Id -> Type -> CoreExpr -> CoreM CoreExpr
addBndr add_bndr_id expr_type = go where
    go :: CoreExpr -> CoreM CoreExpr
    go t = case t of
        App e1 (Lam x e2) -> do
             e1' <- go e1
             e2' <- go e2
             tryAdd (varString x) (exprType e1') (App e1' (Lam x e2'))
        App e1 e2 -> App <$> go e1 <*> go e2
        Lam x e -> Lam x <$> go e
        _ -> return t
        -- ^ Just give up for now
      where
        tryAdd :: String -> Type -> CoreExpr -> CoreM CoreExpr
        tryAdd bndr ty e
            | ty `eqType` ((expr_type `mkFunTy` expr_type) `mkFunTy` expr_type) = do
                bs <- mkStringExpr bndr
                let res = mkCoreApps
                            (Var add_bndr_id)
                            [ bs
                            , e
                            ]
                putMsgS $ shw t ++ " ~> " ++ shw res
                return res
        tryAdd _ _ _ = return t

shw :: Outputable a => a -> String
shw = showSDoc . ppr

{-

printSrcLocs :: CoreExpr -> CoreM ()
printSrcLocs = go where
    go :: CoreExpr -> CoreM ()
    go t = case t of
        Var x -> liftIO $ putStrLn (shw x ++ " at " ++ shw (getSrcLoc x))
        App e1 e2 -> go e1 >> go e2
        Lam x e -> do
            liftIO $ putStrLn $ "Lambda: " ++ shw x ++ " at " ++ shw (getSrcLoc x)
            go e
        Let _ e -> go e
        Case s _ _ alts -> go s >> mapM_ (\(_,_,e) -> go e) alts
        _ -> return ()

-}
