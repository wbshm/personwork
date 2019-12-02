module Challenge6 (challenge6) where
import Data.List


challenge6::String->String
challenge6 inStr = do 
    let str = explain inStr
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


explain::String -> String
explain inStr = do
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
                    explain(take l inStr) ++ " " ++ explain(drop (l+1) inStr)
            else do
                let arr = words inStr
                if length arr == 1 then do
                    let str = [ c |c<-inStr , c/=' ']
                    "i"++[last str]
                else if length arr == 3 then 
                    "f0_x"++[last(head(arr))]++"_=_x"++[last(last arr)]
                else
                    "f0_x"++[last(head(arr))]++"_=_x"++[last(last arr)] ++" " ++ explain(unwords (drop 3 arr) )
        else do
            let str = take close inStr
            let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
            (take (start-1) inStr) ++""++ (explain(drop (start) (take close inStr))) ++ " " ++explain (drop (close+1) inStr)


main = do
    putStrLn(challenge6 "λx0 -> x0 ")  -- let f0 x0 = x0 in f0 
    putStrLn(challenge6 "x1 λx0 -> x0")
    putStrLn(challenge6 "(λx0 -> x0) x1")
    putStrLn(challenge6 "(λx0 -> x0) (λx0 -> x0)")
    putStrLn(challenge6 "λx0 -> x0 λx1 -> x0 x1")
