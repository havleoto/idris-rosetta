--
-- Task: 100 doors (http://rosettacode.org/wiki/100_doors)
--
--   Problem: You have 100 doors in a row that are all initially closed.
--   You make 100 passes by the doors. The first time through, you visit
--   every door and toggle the door (if the door is closed, you open it;
--   if it is open, you close it). The second time you only visit every
--   2nd door (door #2, #4, #6, ...). The third time, every 3rd door
--   (door #3, #6, #9, ...), etc, until you only visit the 100th door.
--
--   Question: What state are the doors in after the last pass? Which
--   are open, which are closed?
--
-- To compile:
--    idris -o 100_doors Task_100_doors
--
-- To run in the REPL:
--    idris Task_100_doors
--    *Task_100_doors> finalState
--
-- The type declarations show that the finalState vector has 100
-- elements. Idris achieves this through dependent type machinery,
-- but it is hardly a wonder. Array dimensions can be declared in
-- Fortran and other programming languages too. On the other hand,
-- a Fortran program would probably use unchecked array indexing.
--

size : Nat
size = 100 -- Change to 10 if you want to use the REPL!

data door : Type where
  Open : door
  Closed : door

instance Show door where
  show Open = "open"
  show Closed = "closed"

toggle : door -> door
toggle Open = Closed
toggle Closed = Open

initialState : Vect size door
initialState = replicate size Closed

indicesFrom : Nat -> Vect size Nat
indicesFrom i0 = iota size i0
  where
    iota : (n : Nat) -> Nat -> Vect n Nat
    iota Z _ = Nil
    iota (S n) k = k :: iota n (k + 1)

pass : Vect size door -> Nat -> Vect size door
pass state n = map visit (zip state (indicesFrom 0))
  where
    visit : (door, Nat) -> door
    visit (d, i) = (if n == modNat i (n + 1) then toggle d else d)

finalState : Vect size door
finalState = foldl pass initialState (indicesFrom 0)

main : IO ()
main = putStrLn (concat $ map line (zip finalState (indicesFrom 1)))
  where
    line : (door, Nat) -> String
    line (d, i) = "Door #" ++ show i ++ " is " ++ show d ++ ".\n"
