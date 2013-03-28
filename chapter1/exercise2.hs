module Main where
import System.Environment

main :: IO()
main = do
    args <- getArgs
    putStrLn(show $ read (args !! 0)  + read (args !! 1))
