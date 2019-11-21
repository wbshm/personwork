import Control.Monad (void)
import Data.Maybe
import Text.Printf
import Safe          (readMay)
import Data.IORef
import Data.List

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Currency Converter"


    elSubtract   <- UI.button # set UI.text "-"
    elAdd     <- UI.button # set UI.text "+"
    elMul     <- UI.button # set UI.text "*"
    elDivide  <- UI.button # set UI.text "/"
    elIs      <- UI.button # set UI.text "="
    elDel      <- UI.button # set UI.text "CE"
    elClean   <- UI.button # set UI.text "C"
--    elLeft    <- UI.button # set UI.text "("
--    elRight    <- UI.button # set UI.text ")"
    elInput <- UI.input

    elNum1    <- UI.button # set UI.text "1"
    elNum2    <- UI.button # set UI.text "2"
    elNum3    <- UI.button # set UI.text "3"
    elNum4    <- UI.button # set UI.text "4"
    elNum5    <- UI.button # set UI.text "5"
    elNum6    <- UI.button # set UI.text "6"
    elNum7    <- UI.button # set UI.text "7"
    elNum8    <- UI.button # set UI.text "8"
    elNum9    <- UI.button # set UI.text "9"
    elNum0    <- UI.button # set UI.text "0"

    elResult <- UI.span

    getBody window #+ [
            column [
                grid [[element elNum7,element elNum8, element elNum9,element elDivide],
                     [element elNum4,element elNum5, element elNum6,element elMul],
                     [element elNum1,element elNum2, element elNum3,element elSubtract],
                     [element elNum0,element elClean,element elDel, element elAdd]],
            element elInput,
            element elIs,
            row [UI.span # set text "Res: ", element elResult]
            ]]

    on UI.click elIs $ \_ -> do
        i <- get value elInput
        let s = show(calculator i)
        element elResult # set UI.text s

    on UI.click elClean $ \_ -> do
        element elInput # set value ""

    on UI.click elDel $ \_ -> do
        i <- get value elInput
        element elInput # set value (reverse(tail(reverse i)))

    on UI.click elDivide $ \_ -> do
        i <- get value elInput
        let end = head(reverse i)
        if end `elem` "+-*/" then do
            element elInput # set value i
        else do
            element elInput # set value (i ++ "/")

    on UI.click elMul $ \_ -> do
        i <- get value elInput
        let end = head(reverse i)
        if end `elem` "+-*/" then do
            element elInput # set value i
        else do
            element elInput # set value (i ++ "*")
    on UI.click elSubtract $ \_ -> do
        i <- get value elInput
        let end = head(reverse i)
        if end `elem` "+-*/" then do
            element elInput # set value i
        else do
            element elInput # set value (i ++ "-")
    on UI.click elAdd $ \_ -> do
        i <- get value elInput
        let end = head(reverse i)
        if end `elem` "+-*/" then do
            element elInput # set value i
        else do
            element elInput # set value (i ++ "+")

    on UI.click elNum0 $ \_ -> do
        i <- get value elInput
        if i == "0" then
            element elInput # set value i
        else do
            element elInput # set value (i ++ "0")
    on UI.click elNum1 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "1")
    on UI.click elNum2 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "2")
    on UI.click elNum3 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "3")
    on UI.click elNum4 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "4")
    on UI.click elNum5 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "5")
    on UI.click elNum6 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "6")
    on UI.click elNum7 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "7")
    on UI.click elNum8 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "8")
    on UI.click elNum9 $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "9")


calculator:: String -> Double
calculator inputStr = do
    let nums = ['0','1','2','3','4','5','6','7','8','9']
    let opts = ['+','-','*','/']
    let len = length inputStr
    let optList = [c | c<-inputStr, c `elem` opts]
    let numList = words [if elem c "+-*/" then ' ' else c|c <- inputStr]
    doCalculate (map(\n -> read n ::Double) numList) optList
--    sum (map(\n -> read n ::Double) numList)

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



--state monad


{-
1. 扫描输入的字符串
2. 如果是数字则推入数字栈Calculator::
3. 如果是 +/- 则推入操作栈
4. 如果是 *// 保留操作符，遇到下个数字就和栈顶的数字运算，并将运算结果压入数字栈

-}
