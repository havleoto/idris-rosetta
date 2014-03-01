--
-- Task: Flatten a list (http://rosettacode.org/wiki/Flatten_a_list)
--
-- Task Description: Write a function to flatten the nesting in an
-- arbitrary list of values. Your program should work on the equivalent
-- of this list:
--
--   [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
--
-- Where the correct result would be the list:
--
--   [1, 2, 3, 4, 5, 6, 7, 8]
--
-- usage in Idris REPL: *Flatten> show lst1
--                      *Flatten> show (flatten lst1)
--
-- The task asks for a heterogeneous list. Heterogeneous list are
-- commonly used in dynamically typed languages like Lisp. The default
-- list data type in Idris is homogeneous. All elements of (List A)
-- must be of the same type A (this is true for many other statically
-- typed languages).

data AnyList : Type where
  aNil : AnyList
  aCons : Show t => {m : Maybe (t = AnyList)} -> t -> AnyList -> AnyList

-- AnyList can represent list of any elements. Each element is equipped
-- with a flag {m} indicating whether the element is an AnyList or not.
-- Whenever the element is an AnyList, the implicit parameter {m} should
-- contain a proof of this fact.
--
-- Here, the type class Show is used for conventience only. It is not
-- necessary for the flattening operation. However, a heterogeneous
-- list would be of little use, if the types of the list elements were
-- completely unconstrained.

instance Show AnyList where
  show xs = "[" ++ showElements xs ++ "]"
    where
      showElements aNil = ""
      showElements (aCons x aNil) = show x
      showElements (aCons x xs) = show x ++ ", " ++ showElements xs

-- The data type AnyList is far from perfect. Bug #1: The element
-- may be an AnyList even if {m = Nothing}. The flatten function will
-- not work correctly on this list.

bug_1 : AnyList
bug_1 = aCons {m = Nothing} (aCons {m = Nothing} 1 aNil) aNil

-- Bug #2: Infinite structures are possible. The flatten function will
-- not be total. Do not try to print the following value in the REPL.

bug_2 : AnyList
bug_2 = aCons {m = Just refl} bug_2 aNil

-- some syntax

infixr 4 #

class Element t where
  (#) : t -> AnyList -> AnyList

instance Element AnyList where
  (#) x xs = aCons {m = Just refl} x xs

instance Element Integer where
  (#) x xs = aCons {m = Nothing} x xs

instance Element Char where
  (#) x xs = aCons {m = Nothing} x xs

-- Define the list from the task description.

lst1 : AnyList
lst1 = (1 # aNil) # 2 # ((3 # 4 # aNil) # 5 # aNil) # ((aNil # aNil) # aNil)
       # (((6 # aNil) # aNil) # aNil) # 7 # 8 # aNil # aNil

-- Another example of heterogeneous list.

lst2 : AnyList
lst2 = ('a' # aNil) # ('b' # 1 # aNil) # aNil

-- Simple concatenation. Note that the proof {m} is copied.

aCat : AnyList -> AnyList -> AnyList
aCat aNil ys = ys
aCat (aCons {m} x xs) ys = aCons {m} x (aCat xs ys)

-- The flatten function. If the element is marked by {m = Just P},
-- the proof P of (t = AnyList) is used to "type cast" the element
-- to AnyList. The "type cast" is developed in interactive proof
-- mode. The proof becomes part of the function (different proof
-- may cause the function return different result).

flatten : AnyList -> AnyList
flatten aNil = aNil
flatten (aCons {m = Just P} x xs) = aCat (flatten ?castX) (flatten xs)
flatten (aCons {m = Nothing} x xs) = aCons {m = Nothing} x (flatten xs)

castX = proof
  intros
  rewrite P
  exact x
