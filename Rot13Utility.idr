--
-- Task: Rot-13 (http://rosettacode.org/wiki/Rot-13)
--
-- Task Description: Implement a "rot-13" function [...] Optionally
-- wrap this function in a utility program which acts like a common UNIX
-- utility, performing a line-by-line rot-13 encoding of every line of
-- input contained in each file listed on its command line, or (if no
-- filenames are passed thereon) acting as a filter on its "standard
-- input."
--
-- command line usage: idris Rot13Utility -o rot
--                     ./rot <INPUT >OUTPUT
--                     ./rot FILE ... >OUTPUT
--

module Main
import System
import Rot13

runRot13 : File -> IO ()
runRot13 f = do
  ln <- fread f
  if ln /= "" then do
    putStr (stringRot13 ln)
    runRot13 f
  else
    return ()

withInput : (File -> IO t) -> String -> IO t
withInput work name = do
  f <- openFile name Read
  a <- work f
  closeFile f
  return a

forEach : (t -> IO ()) -> List t -> IO ()
forEach f [] = return ()
forEach f (x :: xs) = do
  f x
  forEach f xs

main : IO ()
main = do
  args <- System.getArgs
  case args of
    []  => runRot13 stdin
    [_] => runRot13 stdin
    _ :: fs => forEach (withInput runRot13) fs
