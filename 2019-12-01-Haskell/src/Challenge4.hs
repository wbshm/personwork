module Challenge4 (challenge4) where
import Data.List

challenge4::String -> String
challenge4 inStr = do
    let rtn = explain inStr
    let str = [if c=='{' then '(' else if c=='}' then ')' else if c=='_' then ' ' else c | c <- rtn]
    if (take 3 inStr) == "let"  then do
        if length(words inStr) <= 4 then
            "Nothing"
        else
            "Just (LetDef" ++str++ ")"
    else 
        "Just ("++ str ++")"


explain::String -> String
explain inStr = do
    let close = maybe (-1) (+0) (findIndex (==')') inStr)
    if close == -1 then do
        if (take 3 inStr) == "let"  then do
            let i = maybe (-1) (+0) (findIndex (=='i') inStr)
            if inStr!!(i+1) == 'n' then
                explain(take (i-1) inStr) ++ explain(drop (i+2) inStr)
            else do
                let slip = maybe (-1) (+0) (findIndex (==';') inStr)
                if slip > 0 then
                    "["++explain(take (slip) inStr) ++ explain("let "++(drop (slip+1) inStr))++"]"
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
                explain (rtn ++" "++ unwords(drop 2 strArr2))
            else do
                rtn
    else do
        let str = take close inStr
        let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
        explain( (take (start-1) inStr) ++"{"++ (explain(drop (start) (take close inStr))) ++ "}" ++(drop (close+1) inStr) )


main = do
    putStrLn(challenge4 "let x1 = x2")
    putStrLn(challenge4 "x1 (x2 x3)")
    putStrLn(challenge4 "x1 x2 x3")
    putStrLn(challenge4 "let f1 x1 = x2 in f1 x1")
    putStrLn(challenge4 "let f1 x2 = x2; f2 x1 = x1 in f1 x1")
