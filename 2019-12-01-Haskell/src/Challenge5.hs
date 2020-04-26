module Challenge5 (challenge5) where
import Data.List


challenge5::String -> String
challenge5 inputStr = do
    let inStr = if (take 3 inputStr) == "let" then (drop 4 inputStr) else inputStr
    explain5 inStr


explain5::String -> String
explain5 inStr = do
    let i = maybe (-1) (+0) (findIndex (=='i') inStr)
    if i /= -1 then do
        explain5 (take i inStr) ++ explain5 (drop (i+2) inStr)
    else do
        let sp = maybe (-1) (+0) (findIndex (==';') inStr)
        if sp == -1 then do
            let eq = maybe (-1) (+0) (findIndex (=='=') inStr)
            if eq == -1 then
                inStr
            else do
                let arr = words (take eq inStr)
                "λ" ++ unwords([val++" ->" |val<- arr])
        else do
            explain5 (take sp inStr) ++ explain5 (drop (sp+1) inStr)



main = do
    putStrLn(challenge5 "f0 = f0 in f0")  --(λx0 -> x0 x0) λx0 -> x0 x0 
    putStrLn(challenge5 "f1 x2 = x2 in f1") --(λx0 λx0 -> x0) λx0 λx0 -> x0 
    putStrLn(challenge5 "f1 x2 x3 = x3 x2 in f1") --λx0 -> λx1 -> x1 x0 
    putStrLn(challenge5 "let f0 x0 = f1; f1 x1 = x1 in f0") --λx0 -> λx1 -> x1 
    putStrLn(challenge5 "let f0 x0 x1 = x0; f1 x1 = f0 x1 f1 in f1") --λx0 -> x0 
