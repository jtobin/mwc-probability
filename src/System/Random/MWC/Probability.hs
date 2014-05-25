{-# OPTIONS_GHC -Wall #-}

module System.Random.MWC.Probability (
    module MWC
  , Prob(..)
  , uniform
  , uniformR
  , categorical
  , standard
  , normal
  , logNormal
  , exponential
  , gamma
  , inverseGamma
  , chiSquare
  , beta
  , dirichlet
  , bernoulli
  , binomial
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import System.Random.MWC as MWC hiding (uniform, uniformR)
import qualified System.Random.MWC as QMWC
import qualified System.Random.MWC.Distributions as MWC.Dist

newtype Prob m a = Prob { sample :: Gen (PrimState m) -> m a }

instance Monad m => Functor (Prob m) where
  fmap h (Prob f) = Prob $ liftM h . f
  {-# INLINE fmap #-}

instance Monad m => Applicative (Prob m) where
  pure  = return
  (<*>) = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad m => Monad (Prob m) where
  return  = Prob . const . return
  m >>= h = Prob $ \g -> do
    z <- sample m g
    sample (h z) g
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance MonadTrans Prob where
  lift m = Prob $ const m
  {-# INLINE lift #-}

uniform :: (PrimMonad m, Variate a) => Prob m a
uniform = Prob QMWC.uniform
{-# INLINE uniform #-}

uniformR :: (PrimMonad m, Variate a) => (a, a) -> Prob m a
uniformR r = Prob $ QMWC.uniformR r
{-# INLINE uniformR #-}

categorical :: PrimMonad m => [a] -> Prob m a
categorical cs = do
  j <- uniformR (0, length cs - 1)
  return $ cs !! j
{-# INLINE categorical #-}

standard :: PrimMonad m => Prob m Double
standard = Prob MWC.Dist.standard
{-# INLINE standard #-}

normal :: PrimMonad m => Double -> Double -> Prob m Double
normal m sd = Prob $ MWC.Dist.normal m sd
{-# INLINE normal #-}

logNormal :: PrimMonad m => Double -> Double -> Prob m Double
logNormal m sd = exp <$> normal m sd
{-# INLINE logNormal #-}

exponential :: PrimMonad m => Double -> Prob m Double
exponential r = Prob $ MWC.Dist.exponential r
{-# INLINE exponential #-}

gamma :: PrimMonad m => Double -> Double -> Prob m Double
gamma a b = Prob $ MWC.Dist.gamma a b
{-# INLINE gamma #-}

inverseGamma :: PrimMonad m => Double -> Double -> Prob m Double
inverseGamma a b = recip <$> gamma a b
{-# INLINE inverseGamma #-}

chiSquare :: PrimMonad m => Int -> Prob m Double
chiSquare k = Prob $ MWC.Dist.chiSquare k
{-# INLINE chiSquare #-}

beta :: PrimMonad m => Double -> Double -> Prob m Double
beta a b = do
  u <- gamma a 1
  w <- gamma b 1
  return $ u / (u + w)
{-# INLINE beta #-}

dirichlet :: PrimMonad m => [Double] -> Prob m [Double]
dirichlet as = do
  zs <- mapM (`gamma` 1) as
  return $ map (/ sum zs) zs
{-# INLINE dirichlet #-}

bernoulli :: PrimMonad m => Double -> Prob m Bool
bernoulli p = (< p) <$> uniform
{-# INLINE bernoulli #-}

binomial :: PrimMonad m => Int -> Double -> Prob m Int
binomial n p = liftM (length . filter id) $ replicateM n (bernoulli p)
{-# INLINE binomial #-}

