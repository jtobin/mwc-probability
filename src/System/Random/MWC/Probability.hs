{-# OPTIONS_GHC -Wall #-}

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
  , t
  , isoGauss
  , poisson
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Data.List (findIndex)
import System.Random.MWC as MWC hiding (uniform, uniformR)
import qualified System.Random.MWC as QMWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import System.Random.MWC.CondensedTable

-- | A probability distribution characterized by a sampling function.
newtype Prob m a = Prob { sample :: Gen (PrimState m) -> m a }

samples :: PrimMonad m => Prob m a -> Int -> Gen (PrimState m) -> m [a]
samples model n gen = replicateM n (sample model gen)

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

instance MonadTrans Prob where
  lift m = Prob $ const m

uniform :: (PrimMonad m, Variate a) => Prob m a
uniform = Prob QMWC.uniform

uniformR :: (PrimMonad m, Variate a) => (a, a) -> Prob m a
uniformR r = Prob $ QMWC.uniformR r

discreteUniform :: PrimMonad m => [a] -> Prob m a
discreteUniform cs = do
  j <- uniformR (0, length cs - 1)
  return $ cs !! j

standard :: PrimMonad m => Prob m Double
standard = Prob MWC.Dist.standard

normal :: PrimMonad m => Double -> Double -> Prob m Double
normal m sd = Prob $ MWC.Dist.normal m sd

logNormal :: PrimMonad m => Double -> Double -> Prob m Double
logNormal m sd = exp <$> normal m sd

exponential :: PrimMonad m => Double -> Prob m Double
exponential r = Prob $ MWC.Dist.exponential r

gamma :: PrimMonad m => Double -> Double -> Prob m Double
gamma a b = Prob $ MWC.Dist.gamma a b

inverseGamma :: PrimMonad m => Double -> Double -> Prob m Double
inverseGamma a b = recip <$> gamma a b

chiSquare :: PrimMonad m => Int -> Prob m Double
chiSquare k = Prob $ MWC.Dist.chiSquare k

beta :: PrimMonad m => Double -> Double -> Prob m Double
beta a b = do
  u <- gamma a 1
  w <- gamma b 1
  return $ u / (u + w)

dirichlet :: PrimMonad m => [Double] -> Prob m [Double]
dirichlet as = do
  zs <- mapM (`gamma` 1) as
  return $ map (/ sum zs) zs

symmetricDirichlet :: PrimMonad m => Int -> Double -> Prob m [Double]
symmetricDirichlet n a = dirichlet (replicate n a)

bernoulli :: PrimMonad m => Double -> Prob m Bool
bernoulli p = (< p) <$> uniform

binomial :: PrimMonad m => Int -> Double -> Prob m Int
binomial n p = liftM (length . filter id) $ replicateM n (bernoulli p)

multinomial :: PrimMonad m => Int -> [Double] -> Prob m [Int]
multinomial n ps = do
  let cumulative = scanl1 (+) ps
  replicateM n $ do
    z <- uniform
    let Just g = findIndex (> z) cumulative
    return g

t :: PrimMonad m => Double -> Double -> Double -> Prob m Double
t m s k = do
  sd <- sqrt <$> inverseGamma (k / 2) (s * 2 / k)
  normal m sd

isoGauss :: PrimMonad m => [Double] -> Double -> Prob m [Double]
isoGauss ms sd = mapM (\m -> normal m sd) ms

poisson :: PrimMonad m => Double -> Prob m Int
poisson l = Prob $ genFromTable table where
  table = tablePoisson l

categorical :: PrimMonad m => [Double] -> Prob m Int
categorical ps = do
  xs <- multinomial 1 ps
  case xs of
    [x] -> return x
    _   -> error "categorical: invalid return value"

