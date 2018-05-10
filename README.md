# mwc-probability

[![Build Status](https://secure.travis-ci.org/jtobin/mwc-probability.png)](http://travis-ci.org/jtobin/mwc-probability)
[![Hackage Version](https://img.shields.io/hackage/v/mwc-probability.svg)](http://hackage.haskell.org/package/mwc-probability)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jtobin/mwc-probability/blob/master/LICENSE)

Sampling function-based probability distributions.

A simple probability distribution type, where distributions are characterized
by sampling functions.

This implementation is a thin layer over `mwc-random`, which handles RNG
state-passing automatically by using a `PrimMonad` like `IO` or `ST s` under
the hood.


Examples
--------

1. Transform a distribution's support while leaving its density structure
invariant:

      -- uniform over [0, 1] transformed to uniform over [1, 2]
      succ <$> uniform

2. Sequence distributions together using bind:

      -- a beta-binomial composite distribution
      beta 1 10 >>= binomial 10

3. Use do-notation to build complex joint distributions from composable,
local conditionals:

      hierarchicalModel = do
        [c, d, e, f] <- replicateM 4 $ uniformR (1, 10)
        a <- gamma c d
        b <- gamma e f
        p <- beta a b
        n <- uniformR (5, 10)
        binomial n p



Included probability distributions
-------------

* Continuous

  * Uniform
  * Normal
  * Log-Normal
  * Exponential
  * Inverse Gaussian
  * Laplace
  * Gamma
  * Inverse Gamma
  * Weibull
  * Chi-squared
  * Beta
  * Student t
  * Pareto
  * Dirichlet process
  * Symmetric Dirichlet process  

* Discrete

  * Discrete uniform
  * Zipf-Mandelbrot
  * Categorical
  * Bernoulli
  * Binomial
  * Negative Binomial
  * Multinomial
  * Poisson