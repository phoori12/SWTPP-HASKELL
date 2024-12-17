{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}

module Lib where

import Data.Char()
import Data.List.NonEmpty (append)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- blatt 1

fee :: Int
fee = 1

charge :: Int -> Int
charge a = if a > fee then a - fee else 0

putChips:: Int -> Int -> Int
putChips owned added = charge (owned + added)

takeChips :: Int -> Int -> Int
takeChips owned taken = let result = owned - taken
    in max result 0

win :: Int -> Int -> Int
win a b = if a > b then recursive a b 0 else recursive b a 0

recursive:: Int -> Int -> Int -> Int
recursive _ 0 res = res
recursive 0 _ res = res
recursive a b res =
    let setB = if a `mod` 10 == 0 then max (b - 1) 0 else b
    in recursive (a-1) setB (res+b)

-- blatt 2

data CommandS = Put | Take | Win deriving Show
evalS :: CommandS -> Int -> Int -> Int
evalS c a b = case c of
    Put -> putChips a b
    Take -> takeChips a b
    Win -> win a b

data CommandP = PutP Int Int | TakeP Int Int | WinP Int Int deriving Show
evalP :: CommandP -> Int
evalP (PutP a b) = putChips a b
evalP (TakeP a b) = takeChips a b
evalP (WinP a b) = win a b

data CommandR = PutR CommandR CommandR | TakeR CommandR CommandR | WinR CommandR CommandR | ValR Int deriving Show
evalR :: CommandR -> Int
evalR (PutR a b) = putChips (evalR a) (evalR b)
evalR (TakeR a b) = takeChips (evalR a) (evalR b)
evalR (WinR a b) = win (evalR a) (evalR b)
evalR (ValR a) = a

ascii :: [Char]
ascii = ['\0'..'\127']

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'o' = True
isVowel 'i' = True
isVowel 'u' = True
isVowel _ = False

hasVowel :: String -> Bool
hasVowel [] = False
hasVowel (x:xs) =
    if isVowel x
        then True
        else hasVowel xs

toString :: CommandR -> String
toString command = case command of
    ValR x    -> show x
    PutR x y  -> "(" ++ toString x ++ " + " ++ toString y ++ ")"
    TakeR x y -> "(" ++ toString x ++ " - " ++ toString y ++ ")"
    WinR x y  -> "(" ++ toString x ++ " * " ++ toString y ++ ")"

-- blatt 3

rep :: [a] -> Int -> [a]
rep _ 0 = []
rep xs n = xs ++ rep xs (n-1)

mirror :: [a] -> [a]
mirror [] = []
mirror xs = xs ++ reverse xs
-- mirror (x:xs) = [x] ++ mirror[xs] ++ [x] geht auch
-- Bsp. mirror [1,2,3]
-- 1 ++ mirror(2,3) ++ 1
-- 1 ++ 2 ++ mirror(3) ++ 2 ++ 1
-- 1 ++ 2 ++ 3 ++ mirror([]) ++ 3 ++ 2 ++ 1

drop2 :: [a] -> [a]
drop2 [] = []
drop2 [_] = []
drop2 (_:_:xs) = xs

dropn :: [a] -> Int -> [a]
dropn [] _ = []
dropn xs 0 = xs
dropn (x:xs) n = dropn xs (n-1)

kick :: [CommandR] -> [CommandR]
kick [] = []
kick (x:xs) = if evalR x > 0 
    then x:kick xs
    else kick xs

payback :: [CommandR] -> [CommandR]
payback [] = []
payback ((TakeR op1 (ValR _)):xs) = op1: payback xs
payback (x:xs) = x:payback xs

-- wtf is this?
share :: CommandR-> [CommandR]-> CommandR-> (CommandR, [CommandR])
share pot [] _ = (pot, [])
share pot (p:ps) part = let (restpot, newps) = share (TakeR pot part) (ps) part in
                            (restpot, (PutR p part):newps)

sqr :: Int -> Int
sqr = (^ 2)

pot :: Int -> Int
pot = (2 ^)

