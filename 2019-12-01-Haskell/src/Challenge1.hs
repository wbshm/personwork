
import Data.List

challenge1 :: String -> String
challenge1 input = do
    let strArr = words[if elem c "λ" then ' ' else c |c <- input]
    explain strArr 0

explain :: [String] -> Int -> String
explain strArr n = do
    if elem "->" strArr then do
        let first = getVariable(n)
        let tArr = [if val==first then first++"t" else val  | val <- strArr]
        let headVal = head(tArr)
        let m = if last(tArr) == headVal then n+1 else n
        let tmpArr = [if val==headVal then first else val  | val <- tArr]
        "λ" ++ first ++ " " ++ head(tail(strArr)) ++ " " ++ (explain(tail(tail(tmpArr))) m)
    else do
        intercalate " " strArr


getVariable :: Int -> String
getVariable n = "x" ++ (show n)

main = do
    -- explain1 ["x1","->","x0"] 0
    putStrLn("x1 x0 ====> " ++ (challenge1 "x1 x0"))
    putStrLn("λx3 -> x2 ====> " ++ (challenge1 "λx3 -> x2"))
    putStrLn("λx0 -> λx1 -> x0 ====> " ++ (challenge1 "λx0 -> λx1 -> x0"))
    putStrLn("λx1 -> λx0 -> x1 ====> " ++ (challenge1 "λx1 -> λx0 -> x1"))
    putStrLn("λx1 -> λx0 -> x0 ====> " ++ (challenge1 "λx1 -> λx0 -> x0"))
    putStrLn("λx0 -> λx1 -> λx2 -> x0 ====> " ++ (challenge1 "λx0 -> λx1 -> λx2 -> x0"))





