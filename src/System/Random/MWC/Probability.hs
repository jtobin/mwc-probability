{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- |
-- Module: System.Random.MWC.Probability
-- Copyright: (c) 2015-2017 Jared Tobin, Marco Zocca
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>, Marco Zocca <zocca.marco gmail>
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
-- >>> create >>= sample (fmap (+ 1) uniform)
-- 1.5480073474340754
--
-- == Running the examples
--
-- In the following we will assume an interactive GHCi session; the user should first declare a random number generator: 
--
-- >>> gen <- create
--
-- which will be reused throughout all examples.
-- Note: creating a random generator is an expensive operation, so it should be only performed once in the code (usually in the top-level IO action, e.g `main`).


module System.Random.MWC.Probability (
    module MWC
  , Prob(..)
  , samples

  -- * Distributions
  -- ** Continuous-valued
  , uniform
  , uniformR
  , normal
  , standardNormal
  , isoNormal    
  , logNormal
  , exponential
  , laplace
  , gamma
  , inverseGamma
  , normalGamma
  , weibull
  , chiSquare
  , beta
  , student
  -- *** Dirichlet process
  , dirichlet
  , symmetricDirichlet  
  -- ** Discrete-valued
  , discreteUniform
  , zipf
  , categorical
  , bernoulli
  , binomial
  , multinomial
  , poisson  


  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
#endif
import qualified Data.Foldable as F
import Data.List (findIndex)
import System.Random.MWC as MWC hiding (uniform, uniformR)
import qualified System.Random.MWC as QMWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import System.Random.MWC.CondensedTable

-- | A probability distribution characterized by a sampling function.
--
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

instance Functor m => Functor (Prob m) where
  fmap h (Prob f) = Prob (fmap h . f)

instance Monad m => Applicative (Prob m) where
  pure  = Prob . const . pure
  (<*>) = ap

instance Monad m => Monad (Prob m) where
  return = pure
  m >>= h = Prob $ \g -> do
    z <- sample m g
    sample (h z) g
  {-# INLINABLE (>>=) #-}

instance (Monad m, Num a) => Num (Prob m a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger



instance MonadTrans Prob where
  lift m = Prob $ const m

instance MonadIO m => MonadIO (Prob m) where
  liftIO m = Prob $ const (liftIO m)

instance PrimMonad m => PrimMonad (Prob m) where
  type PrimState (Prob m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

-- | The uniform distribution over a type.
--
--   >>> sample uniform gen :: IO Double
--   0.29308497534914946
--   >>> sample uniform gen :: IO Bool
--   False
uniform :: (PrimMonad m, Variate a) => Prob m a
uniform = Prob QMWC.uniform
{-# INLINABLE uniform #-}

-- | The uniform distribution over the provided interval.
--
--   >>> sample (uniformR (0, 1)) gen
--   0.44984153252922365
uniformR :: (PrimMonad m, Variate a) => (a, a) -> Prob m a
uniformR r = Prob $ QMWC.uniformR r
{-# INLINABLE uniformR #-}

-- | The discrete uniform distribution.
--
--   >>> sample (discreteUniform [0..10]) gen
--   6
--   >>> sample (discreteUniform "abcdefghijklmnopqrstuvwxyz") gen
--   'a'
discreteUniform :: (PrimMonad m, Foldable f) => f a -> Prob m a
discreteUniform cs = do
  j <- uniformR (0, length cs - 1)
  return $ F.toList cs !! j
{-# INLINABLE discreteUniform #-}

-- | The standard normal or Gaussian distribution (with mean 0 and standard
--   deviation 1).
standardNormal :: PrimMonad m => Prob m Double
standardNormal = Prob MWC.Dist.standard
{-# INLINABLE standardNormal #-}

-- | The normal or Gaussian distribution with a specified mean and standard
--   deviation.
normal :: PrimMonad m => Double -> Double -> Prob m Double
normal m sd = Prob $ MWC.Dist.normal m sd
{-# INLINABLE normal #-}

-- | The log-normal distribution with specified mean and standard deviation.
logNormal :: PrimMonad m => Double -> Double -> Prob m Double
logNormal m sd = exp <$> normal m sd
{-# INLINABLE logNormal #-}

-- | The exponential distribution with provided rate parameter.
exponential :: PrimMonad m => Double -> Prob m Double
exponential r = Prob $ MWC.Dist.exponential r
{-# INLINABLE exponential #-}

-- | The Laplace distribution with provided location and scale parameters.
laplace :: (Floating a, Variate a, PrimMonad m) => a -> a -> Prob m a
laplace mu sigma = do
  u <- uniformR (-0.5, 0.5)
  let b = sigma / sqrt 2
  return $ mu - b * signum u * log (1 - 2 * abs u)
{-# INLINABLE laplace #-}  


-- | The Weibull distribution with provided shape and scale parameters.
weibull :: (Floating a, Variate a, PrimMonad m) => a -> a -> Prob m a
weibull a b = do
  x <- uniform
  return $ (- 1/a * log (1 - x)) ** 1/b
{-# INLINABLE weibull #-}


-- | The gamma distribution with shape parameter a and scale parameter b.
--
--   This is the parameterization used more traditionally in frequentist
--   statistics.  It has the following corresponding probability density
--   function:
--
--   f(x; a, b) = 1 / (Gamma(a) * b ^ a) x ^ (a - 1) e ^ (- x / b)
gamma :: PrimMonad m => Double -> Double -> Prob m Double
gamma a b = Prob $ MWC.Dist.gamma a b
{-# INLINABLE gamma #-}

-- | The inverse-gamma distribution.
inverseGamma :: PrimMonad m => Double -> Double -> Prob m Double
inverseGamma a b = recip <$> gamma a b
{-# INLINABLE inverseGamma #-}

-- | The Normal-Gamma distribution of parameters mu, lambda, a, b
normalGamma :: PrimMonad m => Double -> Double -> Double -> Double -> Prob m Double
normalGamma mu lambda a b = do
  tau <- gamma a b
  let xsd = sqrt $ 1 / (lambda * tau)
  normal mu xsd
{-# INLINABLE normalGamma #-}

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
dirichlet
  :: (Traversable f, PrimMonad m) => f Double -> Prob m (f Double)
dirichlet as = do
  zs <- traverse (`gamma` 1) as
  return $ fmap (/ sum zs) zs
{-# INLINABLE dirichlet #-}

-- | The symmetric Dirichlet distribution of dimension n.
symmetricDirichlet :: PrimMonad m => Int -> Double -> Prob m [Double]
symmetricDirichlet n a = dirichlet (replicate n a)
{-# INLINABLE symmetricDirichlet #-}

-- | The Bernoulli distribution.
bernoulli :: PrimMonad m => Double -> Prob m Bool
bernoulli p = (< p) <$> uniform
{-# INLINABLE bernoulli #-}

-- | The binomial distribution.
binomial :: PrimMonad m => Int -> Double -> Prob m Int
binomial n p = fmap (length . filter id) $ replicateM n (bernoulli p)
{-# INLINABLE binomial #-}

-- | The multinomial distribution.
multinomial :: (Foldable f, PrimMonad m) => Int -> f Double -> Prob m [Int]
multinomial n ps = do
  let cumulative = scanl1 (+) (F.toList ps)
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

-- | An isotropic or spherical Gaussian distribution with specified mean
-- vector and scalar standard deviation parameter.
isoNormal
  :: (Traversable f, PrimMonad m) => f Double -> Double -> Prob m (f Double)
isoNormal ms sd = traverse (`normal` sd) ms
{-# INLINABLE isoNormal #-}

-- | The Poisson distribution.
poisson :: PrimMonad m => Double -> Prob m Int
poisson l = Prob $ genFromTable table where
  table = tablePoisson l
{-# INLINABLE poisson #-}

-- | A categorical distribution defined by the supplied list of probabilities.
categorical :: (Foldable f, PrimMonad m) => f Double -> Prob m Int
categorical ps = do
  xs <- multinomial 1 ps
  case xs of
    [x] -> return x
    _   -> error "categorical: invalid return value"
{-# INLINABLE categorical #-}


-- | The Zipf-Mandelbrot distribution, generated with the rejection
-- sampling algorithm X.6.1 shown in
-- L.Devroye, Non-Uniform Random Variate Generation.
--
-- The parameter should be positive, but values close to 1 should be
-- avoided as they are very computationally intensive. The following
-- code illustrates this behaviour.
-- 
-- >>> samples 10 (zipf 1.1) gen
-- [11315371987423520,2746946,653,609,2,13,85,4,256184577853,50]
-- 
-- >>> samples 10 (zipf 1.5) gen
-- [19,3,3,1,1,2,1,191,2,1]
zipf :: (PrimMonad m, Integral b) => Double -> Prob m b
zipf a = do
  let
    b = 2 ** (a - 1)
    go = do
        u <- uniform
        v <- uniform
        let xInt = floor (u ** (- 1 / (a - 1))) 
            x = fromIntegral xInt
            t = (1 + 1 / x) ** (a - 1)
        if v * x * (t - 1) / (b - 1) <= t / b
          then return xInt
          else go
  go
{-# INLINABLE zipf #-}  
