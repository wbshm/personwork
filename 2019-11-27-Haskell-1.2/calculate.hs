
import Data.List

doCalculate::[Double] -> String -> Double
doCalculate numArr optArr = do
    if optArr == "" then
        sum(numArr)
    else do
        let highOpt = maybe (-1) (+0) (findIndex (=='*') optArr)
        let highOpt2 = maybe (-1) (+0) (findIndex (=='/') optArr)
        if -1 == highOpt || ( highOpt2 < highOpt && highOpt2/= -1) then do
            if -1 == highOpt2 then do
                let lowOpt = maybe (-1) (+0) (findIndex (=='-') optArr)
                if lowOpt == -1 then do
                    sum(numArr)
                else do
                    let num = (-1) * (numArr !! (lowOpt + 1))
                    let nextNum = (take (lowOpt+1) numArr) ++ [num] ++ (drop (lowOpt + 2) numArr)
                    let nextOpt = (take lowOpt optArr) ++ "+" ++ (drop (lowOpt + 1) optArr)
                    doCalculate nextNum nextOpt
            else do
                let num = (numArr !! (highOpt2)) / (numArr !! (highOpt2+1))
                let nextNum = (take highOpt2 numArr) ++ [num] ++ (drop (highOpt2 + 2) numArr)
                let nextOpt = (take highOpt2 optArr) ++ (drop (highOpt2 + 1) optArr)
                doCalculate nextNum nextOpt
        else do
            let num = (numArr !! highOpt) * (numArr !! (highOpt+1))
            let nextNum = (take highOpt numArr) ++ [num] ++ (drop (highOpt + 2) numArr)
            let nextOpt = (take highOpt optArr) ++ (drop (highOpt + 1) optArr)
            doCalculate nextNum nextOpt

calculator:: String -> Double
calculator inputStr = do
    let nums = ['0','1','2','3','4','5','6','7','8','9']
    let opts = ['+','-','*','/']
    let len = length inputStr
    let optList = [c | c<-inputStr, c `elem` opts]
    let numList = words [if elem c "+-*/" then ' ' else c|c <- inputStr]
    doCalculate (map(\n -> read n ::Double) numList) optList

explainStr::String -> Double
explainStr inputStr = do
    let close = maybe (-1) (+0) (findIndex (==')') inputStr)
    if close == -1 then do
        calculator inputStr
    else do
        let str = take close inputStr
        let start = (length str) - (maybe (-1) (+0) (findIndex (=='(') (reverse str)))
        let num = calculator (drop start str)
        explainStr((take (start-1) inputStr) ++ show(num) ++ (drop (close+1) inputStr))


main = do
    putStrLn(show (explainStr "(123+(1+1*123)+1)"))
