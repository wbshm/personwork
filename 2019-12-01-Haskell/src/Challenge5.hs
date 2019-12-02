import Data.List

explain::String -> String
explain inStr = do
	let res = [if c=='_' then ' ' else c |c<-str]
    let arr = words inStr



main = do
    putStrLn "let f0 = f0 in f0"  --(λx0 -> x0 x0) λx0 -> x0 x0 
    putStrLn "let f1 x2 = x2 in f1" --(λx0 λx0 -> x0) λx0 λx0 -> x0 
    putStrLn "let f1 x2 x3 = x3 x2 in f1" --λx0 -> λx1 -> x1 x0 
    putStrLn "let f0 x0 = f1; f1 x1 = x1 in f0" --λx0 -> λx1 -> x1 
    putStrLn "let f0 x0 x1 = x0; f1 x1 = f0 x1 f1 in f1" --λx0 -> x0 