
import Data.List

challenge1 :: String -> String
challenge1 input = do
    let strArr = words[if elem c "λ" then ' ' else c |c <- input]
    explain strArr 0

explain :: [String] -> Int -> String
explain strArr n = do
    if elem "->" strArr then do
        let first = getVariable(n)
        "λ" ++ first ++ head(tail(strArr)) ++ (explain(tail(tail(strArr))) (n+1))
    else do
        intercalate " " strArr


getVariable :: Int -> String
getVariable n = "x" ++ (show n)



-- testList :: String -> String
-- testList :: "x1 x0" = "x1 x0"
-- testList :: "λx3 -> x2" = "λx0 -> x2"
-- testList :: "λx0 -> λx1 -> x0" = "λx0 -> λx1 -> x0"
-- testList :: "λx1 -> λx0 -> x1" = "λx0 -> λx1 -> x0"
-- testList :: "λx1 -> λx0 -> x0" = "λx0 -> λx0 -> x0"
-- testList :: "λx0 -> λx1 -> λx2 -> x0" = "λx0 -> λx1 -> λx1 -> x0"


main = do
    putStrLn("x1 x0 ====> " ++ (challenge1 "x1 x0"))
    putStrLn("λx3 -> x2 ====> " ++ (challenge1 "λx3 -> x2"))
    putStrLn("λx0 -> λx1 -> x0 ====> " ++ (challenge1 "λx0 -> λx1 -> x0"))
    putStrLn("λx1 -> λx0 -> x1 ====> " ++ (challenge1 "λx1 -> λx0 -> x1"))
    putStrLn("λx0 -> λx1 -> λx2 -> x0 ====> " ++ (challenge1 "λx0 -> λx1 -> λx2 -> x0"))





