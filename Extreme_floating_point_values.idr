--
-- Task: Extreme floating point values
--   (http://rosettacode.org/wiki/Extreme_floating_point_values)
--
-- Task Description: The IEEE floating point specification defines
-- certain 'extreme' floating point values such as minus zero, -0.0, a
-- value distinct from plus zero; not a number, NaN; and plus and minus
-- infinity. The task is to use expressions involving other 'normal'
-- floating point values in your language to calculate these, (and maybe
-- other), extreme floating point values in your language and assign them
-- to variables. Print the values of these variables if possible; and
-- show some arithmetic with these values and variables. If your language
-- can directly enter these extreme floating point values then show it.

-- There is no input syntax for floating point infinities in Idris 0.9.11.2

PositiveInfinity : Float
NegativeInfinity : Float
NegativeZero : Float

PositiveInfinity = 1.0 / 0.0
NegativeInfinity = -1.0 / 0.0
NegativeZero = 1.0 / NegativeInfinity

-- The output syntax is already there.

test_0 : so (show PositiveInfinity == "Infinity"
          && show NegativeInfinity == "-Infinity"
          && show NegativeZero == "-0.0")
test_0 = oh

test_1 : so (NegativeInfinity < PositiveInfinity
          && NegativeInfinity < NegativeZero
          && 0.0 < PositiveInfinity
          && PositiveInfinity + PositiveInfinity == PositiveInfinity
          && NegativeInfinity + 1.0e300 == NegativeInfinity)
test_1 = oh

-- According to IEEE754, comparison ignores the sign of zero.

test_2 : so (NegativeZero == 0.0
          && not (NegativeZero < 0.0)
          && not (NegativeZero > 0.0))
test_2 = oh

-- Define a NaN.

NaN : Float
NaN = PositiveInfinity - PositiveInfinity

-- Test.

test_3 : so (not (NaN == NaN)  -- IEEE754
          && NaN /= NaN)       -- Idris "x /= y" means "not (x == y)"
test_3 = oh

test_4 : so (show NaN == "NaN")
test_4 = oh

-- Compute the largest finite Float (DBL_MAX in C).

maxFloat : Float
maxFloat = up 1.0 1.0
  where
    up : Float -> Float -> Float
    fin : Float -> Bool
    up x t =
      if x + t == x then x
      else if fin (x + t) then up (x + t) (2 * t)
      else up x (0.5 * t)
    fin x = (x < PositiveInfinity)

-- Compute the smallest positive Float (denormal, 2^(-1023)).

minFloat : Float
minFloat = down 1.0
  where
    down x = if y > 0.0 then down y else x where y = 0.5 * x
