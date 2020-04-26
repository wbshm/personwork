module Challenge2 (challenge2) where

import Data.List


challenge2:: String->String
challenge2 inStr = do
    let inputStr = samplify inStr
    let close = maybe (-1) (+0) (findIndex (==')') inputStr)
    if close == -1 then do
        inputStr
    else do
        let next = explain2 inputStr
        if next == inputStr then
            inputStr 
        else do
            inputStr ++ "\n" ++ challenge2(next)


explain2::String->String
explain2 inStr = do

    let inputStr = samplify inStr
    let close = maybe (-1) (+0) (findIndex (==')') inputStr)
    if close == -1 then do
        inputStr
    else do
        let str = take close inputStr
        let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
        if (length inputStr) > close + 2 then do
            if elem (inputStr !! (close + 2)) ['a'..] && (inputStr !! (close+1)) /= '(' then do
                    let res = calculate (drop start str) [(inputStr !! (close + 2))]
                    let rtn = if (length res) == 1 then res else "("++res++")" 
                    (take (start-1) inputStr) ++ rtn ++ (drop (close+3) inputStr)
            else do
                (take (close+1) inputStr) ++ (explain2(drop (close+1) inputStr))
        else do 
            inputStr


samplify:: String -> String
samplify inputStr = do
    let close = maybe (-1) (+0) (findIndex (==')') inputStr)
    if close == -1 then do
        inputStr
    else do
        let str = take close inputStr
        let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
        if close - start < 3 && start < length(str) then do
            let rtn = (take (start-1) inputStr) ++ (drop start (take close inputStr)) ++ (drop (close+1) inputStr)
            samplify rtn 
        else do
            (take (close+1) inputStr) ++ (samplify(drop (close+1) inputStr))

calculate:: String -> String ->String
calculate inputStr val = do
    let inputArr = words inputStr
    let first = head(inputArr)
    let inputArr2 = words [if c=='λ' then ' ' else c | c <-inputStr]
    let firstVal = head(inputArr2)
    if elem firstVal (tail(inputArr2)) then do
        if val == "" then 
            inputStr
        else do
            unwords (init(tail(tail(inputArr))) ++ [val])
    else do
        unwords(tail(tail(inputArr)))


count::String -> Int
count str = do
    let strArr = words [if elem c "λ()->" then ' ' else c | c<-str]
    length(rmdups strArr)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs


main = do
    -- putStrLn "λx -> (λy -> y)"
    -- putStrLn "(λx -> x)(λy -> y)"
    -- putStrLn "(λx -> λy -> x) z ((λt -> t) u)"

    -- putStrLn(show(count "(λx -> λy -> x) z ((λt -> t) u)"))
    -- putStrLn(calculate "λy -> x" "z")
    putStrLn(challenge2 "(λx -> λy -> x) z ((λt -> t) u)" )
    putStrLn(challenge2 "λx -> (λy -> y)")
    -- putStrLn(explain2 "(λy -> z) (((u)))")
