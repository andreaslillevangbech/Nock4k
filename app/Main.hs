module Main where
import System.Environment (getArgs)
import System.IO (stdout, hFlush)
import Control.Monad

import Nock.Parse
import Nock.Spec

-- main = getArgs >>= print . nock . parse . head

main = do
    putStr "Nock> "
    hFlush stdout
    input <- getLine

    unless (input == ":q")
        $ (print . nock . parse) input
        >> main
