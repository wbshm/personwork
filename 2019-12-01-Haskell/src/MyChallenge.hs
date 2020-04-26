module Challenge (convertLet, challenge1,challenge2,challenge3,challenge4,challenge5,challenge6) where

-- challenge1
import Parsing
import Data.Char
import Data.List
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

handleList :: [Int] -> LamExpr -> LamExpr
handleList list e
  | length tailRemoved == 0 = LamAbs tail e
  | otherwise = handleList tailRemoved (LamAbs tail e)
  where tail = list!!((length list) - 1)
        tailRemoved = init list

convertLet :: Expr -> LamExpr
convertLet (App e1 e2) = LamApp (convertLet e1) (convertLet e2)
convertLet (Var num) = LamVar num
convertLet (Let [] e1 e2) = LamApp (convertLet e2) (convertLet e1)
convertLet (Let (a:as) e1 e2)
    | length as == 0 = LamApp (LamAbs a (convertLet e2)) (convertLet e1)
    | otherwise = LamApp (LamAbs a (convertLet e2)) (handleList as (convertLet e1))

challenge1 :: String -> String
challenge1 input = do
    let strArr = words[if elem c "λ" then ' ' else c |c <- input]
    explain1 strArr 0

explain1 :: [String] -> Int -> String
explain1 strArr n = do
    if elem "->" strArr then do
        let first = getVariable(n)
        let tArr = [if val==first then first++"t" else val  | val <- strArr]
        let headVal = head(tArr)
        let m = if last(tArr) == headVal then n+1 else n
        let tmpArr = [if val==headVal then first else val  | val <- tArr]
        "λ" ++ first ++ " " ++ head(tail(strArr)) ++ " " ++ (explain1(tail(tail(tmpArr))) m)
    else do
        intercalate " " strArr

getVariable :: Int -> String
getVariable n = "x" ++ (show n)

-- 2
challenge2:: String->String
challenge2 inStr = do
    let inputStr = simplify2 inStr
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

    let inputStr = simplify2 inStr
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


simplify2:: String -> String
simplify2 inputStr = do
    let close = maybe (-1) (+0) (findIndex (==')') inputStr)
    if close == -1 then do
        inputStr
    else do
        let str = take close inputStr
        let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
        if close - start < 3 && start < length(str) then do
            let rtn = (take (start-1) inputStr) ++ (drop start (take close inputStr)) ++ (drop (close+1) inputStr)
            simplify2 rtn
        else do
            (take (close+1) inputStr) ++ (simplify2(drop (close+1) inputStr))

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

-- 3


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

-- 4
challenge4::String -> String
challenge4 inStr = do
    let rtn = explain4 inStr
    let str = [if c=='{' then '(' else if c=='}' then ')' else if c=='_' then ' ' else c | c <- rtn]
    if (take 3 inStr) == "let"  then do
        if length(words inStr) <= 4 then
            "Nothing"
        else
            "Just (LetDef" ++str++ ")"
    else
        "Just ("++ str ++")"


explain4::String -> String
explain4 inStr = do
    let close = maybe (-1) (+0) (findIndex (==')') inStr)
    if close == -1 then do
        if (take 3 inStr) == "let"  then do
            let i = maybe (-1) (+0) (findIndex (=='i') inStr)
            if inStr!!(i+1) == 'n' then
                explain4(take (i-1) inStr) ++ explain4(drop (i+2) inStr)
            else do
                let slip = maybe (-1) (+0) (findIndex (==';') inStr)
                if slip > 0 then
                    "["++explain4(take (slip) inStr) ++ explain4("let "++(drop (slip+1) inStr))++"]"
                    -- inStr
                else do
                    let strArr = words [if c=='=' then ' ' else c | c<-inStr]
                    -- ([1,1], LetVar 2)
                    "{["++[last( strArr!!1)]++","++[last( strArr!!2)]++"], LetVar_"++[last( strArr!!3)]++"}"
                    -- strArr!!3
        else do
            let strArr = words inStr
            let strArr2 = [if 'x'==(val!!0) then "LetVar_"++ [(last val)] else if 'f'==(val!!0) then "(LetFun_"++ [(last val)] ++")"  else val | val <- strArr]

            let rtn = "LetApp{" ++ strArr2!!0 ++"}{"++ strArr2!!1 ++"}"
            if length(strArr2) > 2 then do
                explain4 (rtn ++" "++ unwords(drop 2 strArr2))
            else do
                rtn
    else do
        let str = take close inStr
        let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
        explain4( (take (start-1) inStr) ++"{"++ (explain4(drop (start) (take close inStr))) ++ "}" ++(drop (close+1) inStr) )

-- 5

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
-- 6
challenge6::String->String
challenge6 inStr = do
    let str = explain6 inStr
    let res = [if c=='_' then ' ' else c |c<-str]
    let arr = words res
    let iArr = [ c |c<-arr,(head c)=='f' || (head c)=='i']
    if length iArr >= 2 && iArr!!0 == iArr!!1 then do
        let n = (length res) - maybe (-1) (+0) (findIndex (=='f') (reverse res))
        let res2 = (take n res )++"1" ++(drop (n+1) res)
        let i = [if c=='i' then 'x' else c | c <- (unwords([iArr!!0] ++ ["f1"] ++ (drop 2 iArr)))]
        "let "++ unwords([c | c <- (words res2), head c /= 'i']) ++ " in " ++ i
    else do
        let res2= res
        let i = [if c=='i' then 'x' else c | c <- (unwords iArr)]
        "let "++ unwords([c | c <- (words res2), head c /= 'i']) ++ " in " ++ i


explain6::String -> String
explain6 inStr = do
    if inStr=="" then
        ""
    else do
        let close = maybe (-1) (+0) (findIndex (==')') inStr)
        if close == -1 then do
            let l = maybe (-1) (+0) (findIndex (=='λ') inStr)
            if l /= -1 then do
                let strArr = words inStr
                if (length strArr) == 3 then do
                    "f0_x"++[last(head(strArr))]++"_=_x"++[last(last strArr)]
                else do
                    explain6(take l inStr) ++ " " ++ explain6(drop (l+1) inStr)
            else do
                let arr = words inStr
                if length arr == 1 then do
                    let str = [ c |c<-inStr , c/=' ']
                    "i"++[last str]
                else if length arr == 3 then
                    "f0_x"++[last(head(arr))]++"_=_x"++[last(last arr)]
                else
                    "f0_x"++[last(head(arr))]++"_=_x"++[last(last arr)] ++" " ++ explain6(unwords (drop 3 arr) )
        else do
            let str = take close inStr
            let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
            (take (start-1) inStr) ++""++ (explain6(drop (start) (take close inStr))) ++ " " ++explain6 (drop (close+1) inStr)

