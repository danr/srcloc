module Main where

import Control.Monad
import Expr

addLoc :: String -> Int -> Int -> HExpr -> HExpr
addLoc s x y  e = case e of
    HLam _ m k   -> HLam sl m k
    HApp _ e1 e2 -> HApp sl e1 e2
    HVar _ s     -> HVar sl s
  where
    sl = Just (SrcLoc s x y)

addBndr :: String -> HExpr -> HExpr
addBndr b (HLam sl _ k) = HLam sl (Just b) k
addBndr _ e             = e

identity :: HExpr
identity = hlam $ \ x -> x

delta :: HExpr
delta = hlam $ \ x -> x $$ x

omega :: HExpr
omega = delta $$ delta

k :: HExpr
k = hlam $ \ x -> hlam $ \ y -> x

s :: HExpr
s = hlam $ \ f -> hlam $ \ g -> hlam $ \ z -> (f $$ z) $$ (g $$ z)

main :: IO ()
main = mapM_ print [identity,delta,omega,k,s]

