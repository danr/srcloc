module Expr where

data SrcLoc = SrcLoc String Int Int

instance Show SrcLoc where
    show (SrcLoc file x y) = file ++ ":" ++ show x ++ "," ++ show y

data HExpr
    = HLam (Maybe SrcLoc) (Maybe String) (HExpr -> HExpr)
    | HApp (Maybe SrcLoc) HExpr HExpr
    | HVar (Maybe SrcLoc) String

getLoc :: HExpr -> Maybe SrcLoc
getLoc e = case e of
    HLam sl _ _ -> sl
    HApp sl _ _ -> sl
    HVar sl _   -> sl

instance Show HExpr where
    show e = showLoc (getLoc e) $ case e of
        HLam _ (Just n) k -> "\\ " ++ n ++ " -> " ++ show (k (HVar Nothing n))
        HLam _ Nothing  k -> "\\ ?? -> " ++ show (k (HVar Nothing "??"))
        HApp _ e1 e2 -> "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
        HVar _ s -> s
      where
        showLoc (Just sl) r = "(" ++ r ++ " [" ++ show sl ++ "])"
        showLoc Nothing   r = r
        -- showLoc _ r = r

hlam :: (HExpr -> HExpr) -> HExpr
hlam = HLam Nothing Nothing

($$) :: HExpr -> HExpr -> HExpr
($$) = HApp Nothing

