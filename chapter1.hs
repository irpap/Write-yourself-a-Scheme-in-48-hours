module Main where

main :: IO()
main = do
    putStr("Hello! What's your name? ")
    name <- getLine
    putStrLn("Wow. " ++ name ++ " is certainly the best name I've ever heard!")
