{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Lenses and traversals for rational numbers.
--
----------------------------------------------------------------------------
module Data.Ratio.Lens
  (
    _numerator
  , _denominator
  ) where

import Control.Lens
import Data.Ratio

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- $setup
-- >>> import Debug.SimpleReflect
-- >>> let { a ≈ b = abs (a - b) < 1e-6; infix 4 ≈ }

-- | Access the 'numerator' of a rational number.
--
-- >>> (a % b)^._numerator
-- a
--
-- >>> a % b & _numerator *~ 2
-- a * 2 % b
--
-- @'_numerator' :: ('Integral' a, 'Functor' f) => (a -> f a) -> 'Ratio' a -> f ('Ratio' a)@
_numerator :: Integral a => Lens' (Ratio a) a
_numerator f r =
  let n = numerator r
      d = denominator r
   in (% d) <$> f n
{-# INLINE _numerator #-}

-- | Access the '_denominator' of a rational number.
--
-- >>> a % b^._denominator
-- b
--
-- >>> a % b & _denominator *~ 2
-- a % (b * 2)
--
-- @'_denominator' :: ('Integral' a, 'Functor' f) => (a -> f a) -> 'Ratio' a -> f ('Ratio' a)@
_denominator :: Integral a => Lens' (Ratio a) a
_denominator f r =
  let n = numerator r
      d = denominator r
   in (n %) <$> f d
{-# INLINE _denominator #-}

