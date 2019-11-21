
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

explainStr String -> Double



main = do
    let a =doCalculate [4,4,1] "//"
    putStrLn(show a)
