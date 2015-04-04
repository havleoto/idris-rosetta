--
-- Task: Horner's rule for polynomial evaluation
-- (http://rosettacode.org/wiki/Horner's_rule_for_polynomial_evaluation)
--
-- Task Description: Create a routine that takes a list of coefficients
-- of a polynomial in order of increasing powers of x; together with
-- a value of x to compute its value at, and return the value of the
-- polynomial at that value using Horner's rule.
--

import Data.So
import Control.Algebra  -- add to the command line: -p contrib

horner : Ring a => a -> List a -> a
horner {a} x = foldr fma neutral
  where
    fma : a -> a -> a
    fma c r = c <+> (r <.> x)

-- The type declaration for "fma" works because of the implicit type
-- parameter {a} in "horner {a} x = ...". Thanks triliyn and melvar
-- from #idris for help.

-- Unit testing in Idris style.

instance Semigroup Integer where (<+>) = (+)
instance Monoid Integer where neutral = 0
instance Group Integer where inverse x = - x
instance AbelianGroup Integer where {}
instance Ring Integer where (<.>) = (*)

test_1 : So (horner 3 [-19, 7, -4, 6] == 128)
test_1 = Oh

-- Prove some properties.

lemma_1 : Ring a => (x:a) -> horner x [] = neutral
lemma_1 x = Refl

lemma_2 : Ring a =>
  (axiom_1: (x:a) -> neutral <.> x = neutral) ->
  (axiom_2: (x:a) -> x <+> neutral = x) ->
  (x:a) -> (c:a) -> horner x [c] = c
lemma_2 = ?proof_2
proof_2 = proof
  intros
  rewrite (sym (axiom_1 x6))
  rewrite (sym (axiom_2 c))
  exact Refl

-- It would be nice if I could get the ring properties from
-- the VerifiedRing typeclass, but I have lot of difficulties
-- understanding and using Idris type classes. Maybe later.

lemma_3 : Ring a =>
  (x, h : a) -> (t : List a) ->
  horner x (h :: t) = h <+> (horner x t <.> x)
lemma_3 = ?proof_3

-- the proof is not valid in Idris 0.9.17:
-- proof_3 = proof
--   intros
--   trivial

