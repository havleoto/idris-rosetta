--
-- Task: Rot-13 (http://rosettacode.org/wiki/Rot-13)
--
-- Task Description: Implement a "rot-13" function [...]
-- The definition of the rot-13 function is to simply replace every
-- letter of the ASCII alphabet with the letter which is "rotated" 13
-- characters "around" the 26 letter alphabet from its normal cardinal
-- position (wrapping around from "z" to "a" as necessary). Thus the
-- letters "abc" become "nop" and so on. Technically rot-13 is a
-- "monoalphabetic substitution cipher" with a trivial "key". A proper
-- implementation should work on upper and lower case letters, preserve
-- case, and pass all non-alphabetic characters in the input stream
-- through without alteration. 
--

module Rot13
import Data.So
import Data.Fin
%default total

data Case : Type where
  Upper : Case
  Lower : Case

data Item : Type where
  Alphabetic : Case -> Fin 26 -> Item
  Nonalphabetic : Char -> Item

-- The data type declaration ensures that any value of the form
-- (Alphabetic q k) corresponds to a letter. It does not ensure that
-- (Nonalphabetic c) is not a letter.

charAfter : Char -> Char -> Maybe (Fin 26)
charAfter a c = integerToFin (cast (ord c - ord a)) 26

instance Cast (Fin n) Int where
    cast x = cast $ cast {to = Nat} x

instance Cast Char Item where
  cast c =
    fromMaybe (Nonalphabetic c)
              ((map (Alphabetic Upper) $ charAfter 'A' c)
                <|> (map (Alphabetic Lower) $ charAfter 'a' c))

instance Cast Item Char where
  cast (Alphabetic Upper k) = chr $ cast k + ord 'A'
  cast (Alphabetic Lower k) = chr $ cast k + ord 'a'
  cast (Nonalphabetic c) = c

-- unit tests

instance Eq Case where
  (==) Upper Upper = True
  (==) Lower Lower = True
  (==) _     _     = False

instance Eq Item where
  (==) (Alphabetic q k) (Alphabetic q' k') = (q == q' && k == k')
  (==) (Nonalphabetic c) (Nonalphabetic c') = (c == c')
  (==) _ _ = False

test_1 : So (cast {to = Item} 'a' == Alphabetic Lower 0
          && cast {to = Item} 'z' == Alphabetic Lower 25
          && cast {to = Item} 'A' == Alphabetic Upper 0
          && cast {to = Item} 'Z' == Alphabetic Upper 25
          && cast {to = Item} '*' == Nonalphabetic '*')
test_1 = Oh

--
-- decMod x : Fin (S n) ... (x - 1) mod N,
-- subMod k x : Fin (S n) ... (x - k) mod N,
-- where N = S n. The condition N = S n implies that N != 0 and
-- the modulo operation is well defined.
--

decMod : {n:Nat} -> Fin (S n) -> Fin (S n)
decMod FZ = last
decMod (FS x) = weaken x

subMod : {n:Nat} -> Nat -> Fin (S n) -> Fin (S n)
subMod Z x = x
subMod (S k) x = subMod k (decMod x)

-- encryption

rot13 : Item -> Item
rot13 (Alphabetic q k) = Alphabetic q (subMod 13 k)
rot13 (Nonalphabetic c) = Nonalphabetic c

-- public interface ("abstract" means export without implementation)

abstract charRot13 : Char -> Char
charRot13 = cast . rot13 . cast

abstract stringRot13 : String -> String
stringRot13 = pack . map charRot13 . unpack

-- unit tests

test_2 : So (stringRot13 "abc" == "nop"
          && stringRot13 "nop" == "abc")
test_2 = Oh
