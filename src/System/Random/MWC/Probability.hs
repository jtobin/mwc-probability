{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- |
-- Module: System.Random.MWC.Probability
-- Copyright: (c) 2015 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- A probability monad based on sampling functions.
--
-- Probability distributions are abstract constructs that can be represented in
-- a variety of ways.  The sampling function representation is particularly
-- useful - it's computationally efficient, and collections of samples are
-- amenable to much practical work.
--
-- Probability monads propagate uncertainty under the hood.  An expression like
-- @'beta' 1 8 >>= 'binomial' 10@ corresponds to a
-- <https://en.wikipedia.org/wiki/Beta-binomial_distribution beta-binomial>
-- distribution in which the uncertainty captured by @'beta' 1 8@ has been
-- marginalized out.
--
-- The distribution resulting from a series of effects is called the
-- /predictive distribution/ of the model described by the corresponding
-- expression.  The monadic structure lets one piece together a hierarchical
-- structure from simpler, local conditionals:
--
-- > hierarchicalModel = do
-- >   [c, d, e, f] <- replicateM 4 $ uniformR (1, 10)
-- >   a <- gamma c d
-- >   b <- gamma e f
-- >   p <- beta a b
-- >   n <- uniformR (5, 10)
-- >   binomial n p
--
-- The functor instance for a probability monad transforms the support of the
-- distribution while leaving its density structure invariant in some sense.
-- For example, @'uniform'@ is a distribution over the 0-1 interval, but @fmap
-- (+ 1) uniform@ is the translated distribution over the 1-2 interval.
--
-- >>> sample (fmap (+ 1) uniform) gen
-- 1.5480073474340754

module System.Random.MWC.Probability (
    module MWC
  , Prob(..)
  , samples

  , uniform
  , uniformR
  , discreteUniform
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
  , symmetricDirichlet
  , bernoulli
  , binomial
  , multinomial
  , student
  , isoGauss
  , poisson
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List (findIndex)
import System.Random.MWC as MWC hiding (uniform, uniformR)
import qualified System.Random.MWC as QMWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import System.Random.MWC.CondensedTable

-- | A probability distribution characterized by a sampling function.
--
-- >>> gen <- create
-- >>> sample uniform gen
-- 0.4208881170464097
newtype Prob m a = Prob { sample :: Gen (PrimState m) -> m a }

-- | Sample from a model 'n' times.
--
-- >>> samples 2 uniform gen
-- [0.6738707766845254,0.9730405951541817]
samples :: PrimMonad m => Int -> Prob m a -> Gen (PrimState m) -> m [a]
samples n model gen = replicateM n (sample model gen)
{-# INLINABLE samples #-}

instance Monad m => Functor (Prob m) where
  fmap h (Prob f) = Prob $ liftM h . f

instance Monad m => Applicative (Prob m) where
  pure  = return
  (<*>) = ap

instance (Applicative m, Monad m, Num a) => Num (Prob m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance Monad m => Monad (Prob m) where
  return  = Prob . const . return
  m >>= h = Prob $ \g -> do
    z <- sample m g
    sample (h z) g
  {-# INLINABLE (>>=) #-}

instance MonadTrans Prob where
  lift m = Prob $ const m

instance MonadIO m => MonadIO (Prob m) where
  liftIO m = Prob $ const (liftIO m)

instance PrimMonad m => PrimMonad (Prob m) where
    type PrimState (Prob m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}

-- | The uniform distribution.
uniform :: (PrimMonad m, Variate a) => Prob m a
uniform = Prob QMWC.uniform
{-# INLINABLE uniform #-}

-- | The uniform distribution over the provided interval.
uniformR :: (PrimMonad m, Variate a) => (a, a) -> Prob m a
uniformR r = Prob $ QMWC.uniformR r
{-# INLINABLE uniformR #-}

-- | The discrete uniform distribution.
discreteUniform :: PrimMonad m => [a] -> Prob m a
discreteUniform cs = do
  j <- uniformR (0, length cs - 1)
  return $ cs !! j
{-# INLINABLE discreteUniform #-}

-- | The standard normal distribution (a Gaussian with mean 0 and variance 1).
standard :: PrimMonad m => Prob m Double
standard = Prob MWC.Dist.standard
{-# INLINABLE standard #-}

-- | The normal or Gaussian distribution with a specified mean and standard
--   deviation.
normal :: PrimMonad m => Double -> Double -> Prob m Double
normal m sd = Prob $ MWC.Dist.normal m sd
{-# INLINABLE normal #-}

-- | The log-normal distribution with specified mean and standard deviation.
logNormal :: PrimMonad m => Double -> Double -> Prob m Double
logNormal m sd = exp <$> normal m sd
{-# INLINABLE logNormal #-}

-- | The exponential distribution.
exponential :: PrimMonad m => Double -> Prob m Double
exponential r = Prob $ MWC.Dist.exponential r
{-# INLINABLE exponential #-}

-- | The gamma distribution.
gamma :: PrimMonad m => Double -> Double -> Prob m Double
gamma a b = Prob $ MWC.Dist.gamma a b
{-# INLINABLE gamma #-}

-- | The inverse-gamma distribution.
inverseGamma :: PrimMonad m => Double -> Double -> Prob m Double
inverseGamma a b = recip <$> gamma a b
{-# INLINABLE inverseGamma #-}

-- | The chi-square distribution.
chiSquare :: PrimMonad m => Int -> Prob m Double
chiSquare k = Prob $ MWC.Dist.chiSquare k
{-# INLINABLE chiSquare #-}

-- | The beta distribution.
beta :: PrimMonad m => Double -> Double -> Prob m Double
beta a b = do
  u <- gamma a 1
  w <- gamma b 1
  return $ u / (u + w)
{-# INLINABLE beta #-}

-- | The Dirichlet distribution.
dirichlet :: PrimMonad m => [Double] -> Prob m [Double]
dirichlet as = do
  zs <- mapM (`gamma` 1) as
  return $ map (/ sum zs) zs
{-# INLINABLE dirichlet #-}

-- | The symmetric Dirichlet distribution (with equal concentration
--   parameters).
symmetricDirichlet :: PrimMonad m => Int -> Double -> Prob m [Double]
symmetricDirichlet n a = dirichlet (replicate n a)
{-# INLINABLE symmetricDirichlet #-}

-- | The Bernoulli distribution.
bernoulli :: PrimMonad m => Double -> Prob m Bool
bernoulli p = (< p) <$> uniform
{-# INLINABLE bernoulli #-}

-- | The binomial distribution.
binomial :: PrimMonad m => Int -> Double -> Prob m Int
binomial n p = liftM (length . filter id) $ replicateM n (bernoulli p)
{-# INLINABLE binomial #-}

-- | The multinomial distribution.
multinomial :: PrimMonad m => Int -> [Double] -> Prob m [Int]
multinomial n ps = do
  let cumulative = scanl1 (+) ps
  replicateM n $ do
    z <- uniform
    let Just g = findIndex (> z) cumulative
    return g
{-# INLINABLE multinomial #-}

-- | Student's t distribution.
student :: PrimMonad m => Double -> Double -> Double -> Prob m Double
student m s k = do
  sd <- sqrt <$> inverseGamma (k / 2) (s * 2 / k)
  normal m sd
{-# INLINABLE student #-}

-- | An isotropic or spherical Gaussian distribution.
isoGauss :: PrimMonad m => [Double] -> Double -> Prob m [Double]
isoGauss ms sd = mapM (`normal` sd) ms
{-# INLINABLE isoGauss #-}

-- | The Poisson distribution.
poisson :: PrimMonad m => Double -> Prob m Int
poisson l = Prob $ genFromTable table where
  table = tablePoisson l
{-# INLINABLE poisson #-}

-- | A categorical distribution defined by the supplied list of probabilities.
categorical :: PrimMonad m => [Double] -> Prob m Int
categorical ps = do
  xs <- multinomial 1 ps
  case xs of
    [x] -> return x
    _   -> error "categorical: invalid return value"
{-# INLINABLE categorical #-}

