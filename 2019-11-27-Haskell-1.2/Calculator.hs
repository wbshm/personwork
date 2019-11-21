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

    let btnCss = [("display","inline-block"),
                  ("padding","6px 12px"),
                  ("margin-bottom","0"),
                  ("font-size","14px"),
                  ("font-weight","400"),
                  ("line-height","1.42857143"),
                  ("text-align","center"),
                  ("white-space","nowrap"),
                  ("vertical-align","middle"),
                  ("-ms-touch-action","manipulation"),
                  ("touch-action","manipulation"),
                  ("cursor","pointer"),
                  ("-webkit-user-select","none"),
                  ("-moz-user-select","none"),
                  ("-ms-user-select","none"),
                  ("user-select","none"),
                  ("background-image","none"),
                  ("border","1px solid transparent"),
                  ("color","#333"),
                  ("background-color","#fff"),
                  ("border-color","#ccc"),
                  ("border-radius","4px")]
    let btnSize = [("width","35px")]
    let inputCss = [("width","100%"),
                    ("height","34px"),
                    ("padding","6px 12px"),
                    ("font-size","14px"),
                    ("line-height","1.42857143"),
                    ("color","#555"),
                    ("background-color","#fff"),
                    ("border","1px solid #ccc"),
                    ("border-radius","4px"),
                    ("box-shadow","inset 0 1px 1px rgba(0,0,0,.075)"),
                    ("-webkit-transition","border-color ease-in-out .15s,-webkit-box-shadow ease-in-out .15s"),
                    ("-o-transition","border-color ease-in-out .15s,box-shadow ease-in-out .15s"),
                    ("transition","border-color ease-in-out .15s,box-shadow ease-in-out .15s")]
    let spanCss = [("font-family","'Helvetica Neue',Helvetica,Arial,sans-serif"),
                   ("font-size","14px"),
                   ("line-height","1.42857143"),
                   ("display","block"),
                   ("margin-top","5px"),
                   ("margin-bottom","10px"),
                   ("color","#737373")]
    let spanCss2 = [("font-size","14px"),
                    ("line-height","1.42857143"),
                    ("color","#333"),
                    ("display","inline-block"),
                    ("max-width","100%"),
                    ("margin-bottom","5px"),
                    ("font-weight","700")]

    elSubtract   <- UI.button # set UI.text "-" #set UI.style (btnCss ++ btnSize)
    elAdd     <- UI.button # set UI.text "+" #set UI.style (btnCss ++ btnSize)
    elMul     <- UI.button # set UI.text "*" #set UI.style (btnCss ++ btnSize)
    elDivide  <- UI.button # set UI.text "/" #set UI.style (btnCss ++ btnSize)
    elIs      <- UI.button # set UI.text "=" #set UI.style (btnCss ++ btnSize)
    elDel      <- UI.button # set UI.text "CE" #set UI.style btnCss
    elClean   <- UI.button # set UI.text "C" #set UI.style (btnCss ++ btnSize)
    elLeft    <- UI.button # set UI.text "(" #set UI.style (btnCss ++ btnSize)
    elRight    <- UI.button # set UI.text ")" #set UI.style (btnCss ++ btnSize)
    elInput <- UI.input #set UI.style inputCss

    elNum1    <- UI.button # set UI.text "1" #set UI.style (btnCss ++ btnSize)
    elNum2    <- UI.button # set UI.text "2" #set UI.style (btnCss ++ btnSize)
    elNum3    <- UI.button # set UI.text "3" #set UI.style (btnCss ++ btnSize)
    elNum4    <- UI.button # set UI.text "4" #set UI.style (btnCss ++ btnSize)
    elNum5    <- UI.button # set UI.text "5" #set UI.style (btnCss ++ btnSize)
    elNum6    <- UI.button # set UI.text "6" #set UI.style (btnCss ++ btnSize)
    elNum7    <- UI.button # set UI.text "7" #set UI.style (btnCss ++ btnSize)
    elNum8    <- UI.button # set UI.text "8" #set UI.style (btnCss ++ btnSize)
    elNum9    <- UI.button # set UI.text "9" #set UI.style (btnCss ++ btnSize)
    elNum0    <- UI.button # set UI.text "0" #set UI.style (btnCss ++ btnSize)
    elDot    <- UI.button # set UI.text "." #set UI.style (btnCss ++ btnSize)

    elResult <- UI.span #set UI.style spanCss2

    getBody window #+ [
            column [
                grid [[element elNum7,element elNum8, element elNum9,element elDivide],
                     [element elNum4,element elNum5, element elNum6,element elMul],
                     [element elNum1,element elNum2, element elNum3,element elSubtract],
                     [element elNum0,element elLeft,element elRight,element elAdd]
                     ],
                 grid[[element elDot,element elClean,element elDel]],
            element elInput,
            element elIs,
            row [UI.span # set text "Res: " #set UI.style spanCss, element elResult]
            ]]

    on UI.click elIs $ \_ -> do
        i <- get value elInput
        let s = show(explainStr i)
        element elResult # set UI.text s

    on UI.click elClean $ \_ -> do
        element elInput # set value ""

    on UI.click elDel $ \_ -> do
        i <- get value elInput
        if length i > 0 then
            element elInput # set value (reverse(tail(reverse i)))
        else
            element elInput # set value ""


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

    on UI.click elLeft $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ "(")
    on UI.click elRight $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ ")")

    on UI.click elDot $ \_ -> do
        i <- get value elInput
        element elInput # set value  (i ++ ".")
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
calculator input = do
    let inputStr = formatStr input
    let nums = ['0','1','2','3','4','5','6','7','8','9']
    let opts = ['+','*','/']
    let optList = [c | c<-inputStr, c `elem` opts]
    let numList = words [if elem c opts then ' ' else c|c <- inputStr]
    doCalculate (map(\n -> read n ::Double) numList) optList

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

formatStr::String->String
formatStr inputStr = do
    let str = if (inputStr !! 0)=='-' then '0':inputStr else inputStr
    let opt = words [if elem c "+-*/" then c else ' ' |c <- str]
    let num = words [if elem c "+-*/" then ' ' else c |c <- str]
    let aft = [if c =="--" then "+" else if c=="-" then "+-" else c | c <- opt]
    let res = zipWith (++) num aft
    concat(res ++ [head(reverse num)])

--state monad


{-
工作原理：

1. 扫描输入的字符串
2. 获取优先级最高的“()”的内容，并将结果计算出来，
    2.1 计算结果，先替换"n--m" 成 "n+m",替换"n-m"成 "n+-m"
    2.2 解析替换后的结果，然后根据优先级"*/+-",从左往右，计算字符串的结果
    2.3 返回计算后的结果
3. 把计算出来的结果，替换掉2里面的"()"。
4. 如果经过3之后还有“()”,则再进行第二步
5. 没有"()"之后，就直接计算结果。
-}
