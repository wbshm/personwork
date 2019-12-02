import Challenge1
import Challenge2
import Challenge3
import Challenge4
import Challenge5
import Challenge6

testC1::String -> IO()
testC1 run = do
    putStrLn("----- challenge1 -----")
    putStrLn("x1 x0 ====> " ++ (challenge1 "x1 x0"))
    putStrLn("λx3 -> x2 ====> " ++ (challenge1 "λx3 -> x2"))
    putStrLn("λx0 -> λx1 -> x0 ====> " ++ (challenge1 "λx0 -> λx1 -> x0"))
    putStrLn("λx1 -> λx0 -> x1 ====> " ++ (challenge1 "λx1 -> λx0 -> x1"))
    putStrLn("λx1 -> λx0 -> x0 ====> " ++ (challenge1 "λx1 -> λx0 -> x0"))
    putStrLn("λx0 -> λx1 -> λx2 -> x0 ====> " ++ (challenge1 "λx0 -> λx1 -> λx2 -> x0"))



testC2::String -> IO()
testC2 run = do
    putStrLn("----- challenge2 -----")
    putStrLn(challenge2 "(λx -> λy -> x) z ((λt -> t) u)" )
    putStrLn(challenge2 "λx -> (λy -> y)")

testC3::String -> IO()
testC3 run = do
    putStrLn("----- challenge3 -----")
    putStrLn(challenge3 "x2 x1")
    putStrLn(challenge3 "LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))")
    putStrLn(challenge3 "LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))")
    putStrLn(challenge3 "LamAbs 1 (LamAbs 2 (LamVar 1))")
    putStrLn(challenge3 "LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))))")

testC4::String -> IO()
testC4 run = do
    putStrLn("----- challenge4 -----")
    putStrLn(challenge4 "let x1 = x2")
    putStrLn(challenge4 "x1 (x2 x3)")
    putStrLn(challenge4 "x1 x2 x3")
    putStrLn(challenge4 "let f1 x1 = x2 in f1 x1")
    putStrLn(challenge4 "let f1 x2 = x2; f2 x1 = x1 in f1 x1")

testC5::String -> IO()
testC5 run = do
    putStrLn("----- challenge5 -----")
    putStrLn(challenge5 "f0 = f0 in f0")
    putStrLn(challenge5 "f1 x2 = x2 in f1") 
    putStrLn(challenge5 "f1 x2 x3 = x3 x2 in f1")
    putStrLn(challenge5 "let f0 x0 = f1; f1 x1 = x1 in f0")
    putStrLn(challenge5 "let f0 x0 x1 = x0; f1 x1 = f0 x1 f1 in f1")

testC6::String -> IO()
testC6 run = do
    putStrLn("----- challenge6 -----")
    putStrLn(challenge6 "λx0 -> x0 ")  -- let f0 x0 = x0 in f0 
    putStrLn(challenge6 "x1 λx0 -> x0")
    putStrLn(challenge6 "(λx0 -> x0) x1")
    putStrLn(challenge6 "(λx0 -> x0) (λx0 -> x0)")
    putStrLn(challenge6 "λx0 -> x0 λx1 -> x0 x1")

main = do 
    testC1("")
    testC2("")
    testC3("")
    testC4("")
    testC5("")
    testC6("")