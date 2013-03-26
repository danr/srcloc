module SrcLoc (plugin) where

import CoreSyn
import GhcPlugins

import Control.Applicative
import Data.Traversable (sequenceA)

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
    liftIO $ putStrLn (showSDoc (ppr binds))
    binds' <- addBndrs <=< addLocs $ binds
    liftIO $ putStrLn (showSDoc (ppr binds'))
    return binds'

addLocs :: [CoreBind] -> CoreM [CoreBind]
addLocs = findBind "addLoc" (f . splitFunTys) addLoc
  where
    f ([_string,_x_,_y,ex_ty],ex_ty') | ex_ty `eqType` ex_ty' = Just ex_ty
    f _ = Nothing

addBndrs :: [CoreBind] -> CoreM [CoreBind]
addBndrs = findBind "addBndr" (f . splitFunTys) addBndr
  where
    f ([_string,ex_ty],ex_ty') | ex_ty `eqType` ex_ty' = Just ex_ty
    f _ = Nothing

addLoc :: Id -> Type -> CoreExpr -> CoreM CoreExpr
addLoc add_loc_id expr_type = go
  where
    go :: CoreExpr -> CoreM CoreExpr
    go t = case t of
        Var x -> try_add (getSrcLoc x)
        App e1 e2 -> App <$> go e1 <*> go e2
        Lam x e -> Lam x <$> go e
        _ -> boringCases go t
      where
        try_add :: SrcLoc -> CoreM CoreExpr
        try_add (RealSrcLoc src_loc) | exprType t `eqType` expr_type = do
            fs <- mkStringExprFS (srcLocFile src_loc)
            let xi = mkIntExprInt (srcLocLine src_loc)
                yi = mkIntExprInt (srcLocCol src_loc)
                res = mkCoreApps (Var add_loc_id) [fs, xi, yi, t]
            putMsgS $ shw t ++ " ~> " ++ shw res
            return res
        try_add _ = return t

addBndr :: Id -> Type -> CoreExpr -> CoreM CoreExpr
addBndr add_bndr_id expr_type = go
  where
    go :: CoreExpr -> CoreM CoreExpr
    go t = case t of
        App e1 (Lam x e2) -> do
             e1' <- go e1
             e2' <- go e2
             try_add (varString x) (exprType e1') (App e1' (Lam x e2'))
        App e1 e2 -> App <$> go e1 <*> go e2
        Lam x e -> Lam x <$> go e
        Var{} -> return t
        _ -> boringCases go t
      where
        try_add :: String -> Type -> CoreExpr -> CoreM CoreExpr
        try_add bndr ty e
            | ty `eqType` ((expr_type `mkFunTy` expr_type) `mkFunTy` expr_type) = do
                bs <- mkStringExpr bndr
                let res = mkCoreApps (Var add_bndr_id) [bs, e]
                putMsgS $ shw t ++ " ~> " ++ shw res
                return res
        try_add _ _ _ = return t

-- | Maps an expression over binds
exprMap :: Applicative f => (CoreExpr -> f CoreExpr) -> CoreBind -> f CoreBind
exprMap f (NonRec v e) = NonRec v <$> f e
exprMap f (Rec vses)   = Rec <$> sequenceA [ (,) v <$> f e | (v,e) <- vses ]

-- | Fills in all cases but Var,App,Lam for you
boringCases :: Applicative f => (CoreExpr -> f CoreExpr) -> CoreExpr -> f CoreExpr
boringCases f = go
  where
    go t = case t of
        Var{} -> f t
        Lit{} -> pure t
        App{} -> f t
        Lam{} -> f t
        Let bs e -> Let <$> exprMap go bs <*> f e
        Case s ty w alts ->
            (\s' alts' -> Case s' ty w alts')
                <$> go s
                <*> sequenceA [ (,,) p bs <$> go e | (p,bs,e) <- alts ]
        Cast e co -> (`Cast` co) <$> go e
        Tick tk e -> Tick tk <$> go e
        Type{} -> pure t
        Coercion{} -> pure t

findBind :: String -> (Type -> Maybe Type) -> (Id -> Type -> CoreExpr -> CoreM CoreExpr)
         -> [CoreBind] -> CoreM [CoreBind]
findBind s f k binds =
    case find (\ (v,_) -> s == varString v) (flattenBinds binds) of
        Nothing -> do
            putMsgS $ "Did not find " ++ s ++ "!"
            return binds
        Just (i,_e) -> do
            let ty = varType i
            putMsgS $ "Found " ++ shw i ++ " :: " ++ shw ty
            case f ty of
                Just e_ty -> do
                    putMsgS $ "Expression type is " ++ shw e_ty
                    forM binds $ \ b -> case b of
                        NonRec v _ | varString v == s -> return b
                        _ -> exprMap (k i e_ty) b
                Nothing -> do
                    putMsgS "Wrong type!"
                    return binds


varString :: Var -> String
varString = occNameString . nameOccName . varName

shw :: Outputable a => a -> String
#if __GLASGOW_HASKELL__ >= 706
shw = showSDoc tracingDynFlags . ppr
#else
shw = showSDoc . ppr
#endif

