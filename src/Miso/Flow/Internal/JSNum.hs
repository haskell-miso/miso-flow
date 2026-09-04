-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Internal.JSNum
-- License     :  BSD3-style (see the file LICENSE)
--
-- JavaScript-compatible rendering of 'Double' values, following the
-- ECMA-262 @Number::toString@ algorithm. The SVG path strings produced by
-- @\@xyflow\/system@ interpolate JS numbers, so the Haskell port has to
-- print numbers identically (e.g. @150@ rather than @150.0@, @0.01@
-- rather than @1.0e-2@) for paths to be byte-for-byte compatible.
----------------------------------------------------------------------------
module Miso.Flow.Internal.JSNum
  ( jsShow
  ) where
-----------------------------------------------------------------------------
import           Numeric (floatToDigits)
import           Prelude
-----------------------------------------------------------------------------
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
-- | Render a 'Double' exactly like JavaScript string interpolation does.
jsShow :: Double -> MisoString
jsShow x
  | isNaN x = "NaN"
  | isInfinite x = if x > 0 then "Infinity" else "-Infinity"
  | x == 0 = "0" -- both zeroes print as "0" in JS
  | x < 0 = "-" <> jsShow (negate x)
  | otherwise = ms (positive x)
-----------------------------------------------------------------------------
-- | ECMA-262 6.1.6.1.20 Number::toString for a positive, finite double.
-- @floatToDigits@ yields the same shortest uniquely-identifying digit
-- string that the spec requires.
positive :: Double -> String
positive x =
  let (digits, n) = floatToDigits 10 x
      ds = concatMap show digits
      k = length ds
  in if k <= n && n <= 21
       then ds <> replicate (n - k) '0'
     else if 0 < n && n <= 21
       then take n ds <> "." <> drop n ds
     else if -6 < n && n <= 0
       then "0." <> replicate (negate n) '0' <> ds
     else -- exponential notation
       let mantissa = case ds of
             [d] -> [d]
             (d:rest) -> d : '.' : rest
             [] -> "0"
           e = n - 1
           sign = if e >= 0 then "+" else "-"
       in mantissa <> "e" <> sign <> show (abs e)
-----------------------------------------------------------------------------
