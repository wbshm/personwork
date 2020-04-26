
module Challenge3 (challenge3) where
import Data.List


simplify3:: String -> String
simplify3 inputStr = do
    let close = maybe (-1) (+0) (findIndex (==')') inputStr)
    if close == -1 then do
        inputStr
    else do
        let str = take close inputStr
        let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
        if close - start < 3 && start < length(str) then do
            let rtn = (take (start-1) inputStr) ++ (drop start (take close inputStr)) ++ (drop (close+1) inputStr)
            simplify3 rtn
        else do
            (take (close+1) inputStr) ++ (simplify3(drop (close+1) inputStr))

challenge3::String -> String
challenge3 inStr = do
    let str2 = [if elem c "Lampar" then ' ' else c |c<-inStr]
    let str3 = [if c=='V' then 'x'  else if c=='b' then 'λ' else if c=='s' then 'x' else c | c<-str2,c/=' ']
    let str4 = unwords (words str3)
    let str = [if c=='A' then ' ' else c |c<-str4]
    let strArr = words str
    let strArr2 = [if elem 'λ' val then (take 3 val) ++"->" ++ (drop 3 val)  else val  | val <- strArr]
    simplify3( unwords strArr2)


main = do
    putStrLn(challenge3 "x2 x1")
    putStrLn(challenge3 "LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))")
    putStrLn(challenge3 "LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))")
    putStrLn(challenge3 "LamAbs 1 (LamAbs 2 (LamVar 1))")
    putStrLn(challenge3 "LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))))")
