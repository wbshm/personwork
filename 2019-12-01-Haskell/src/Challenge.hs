-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- Take in a list of inputs and output expressions and return an LamExpr
handleList :: [Int] -> LamExpr -> LamExpr
handleList list e
  | length tailRemoved == 0 = LamAbs tail e
  | otherwise = handleList tailRemoved (LamAbs tail e)
  where tail = list!!((length list) - 1)
        tailRemoved = init list

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
convertLet (App e1 e2) = LamApp (convertLet e1) (convertLet e2)
convertLet (Var num) = LamVar num
convertLet (Let [] e1 e2) = LamApp (convertLet e2) (convertLet e1)
convertLet (Let (a:as) e1 e2)
  | length as == 0 = LamApp (LamAbs a (convertLet e2)) (convertLet e1)
  | otherwise = LamApp (LamAbs a (convertLet e2)) (handleList as (convertLet e1))

-- Challenge 2
-- pretty print a let expression by converting it to a string
isNotVar :: Expr -> Bool
isNotVar (Var _) = False
isNotVar (Let _ _ _) = True
isNotVar (App _ _) = True

isLet :: Expr -> Bool
isLet (Var _) = False
isLet (Let _ _ _) = True
isLet (App _ _) = False

-- Used to come up with the inputs of the let expressions
printParameters :: [Int] -> String -> String
printParameters (x:xs) output
  | length xs == 0 = newOutput
  | otherwise = printParameters xs newOutput
  where newOutput = output ++ " x" ++ show x

prettyPrint :: Expr -> String
prettyPrint (Var num) = "x" ++ show num
prettyPrint (App e1 e2)
  | isLet e1 && isNotVar e2 = "(" ++ e1String ++ ") " ++ "(" ++ e2String ++ ")"
  | isLet e1 = "(" ++ e1String ++ ") " ++ e2String
  | isNotVar e2 = e1String ++ " " ++ "(" ++ e2String ++ ")"
  | otherwise = e1String ++ " " ++ e2String
  where e1String = prettyPrint e1
        e2String = prettyPrint e2
prettyPrint (Let arr e1 e2) = "let" ++ printParameters arr "" ++ " = " ++ prettyPrint e1 ++ " in " ++ prettyPrint e2

-- Challenge 3
-- Returns the number of the Var given "x2"
getVarNum :: Parser Int
getVarNum = do
        char 'x'
        num <- nat
        space
        return (num)

-- Returns the Var itself given "x2"
varParser :: Parser Expr
varParser = do
        varNum <- getVarNum
        return (Var varNum)

-- Returns the list of the input vars given "x1 x2 x3 x4"
varListParser :: Parser [Int]
varListParser = do
        list <- many getVarNum
        space
        tail <- if list == [] then return [] else varListParser
        return (list ++ tail)

-- Handles any string that contains a parentheses
parenHandler :: Parser Expr
parenHandler = do
        space
        char '('
        result <- appParser <|> letParser <|> varParser
        char ')'
        space
        return result

-- Handles any cases where more than 1 expressions are present
appParser :: Parser Expr
appParser = do
        firstSec <- parenHandler <|> varParser
        secondSec <- parenHandler <|> varParser
        nextSec <- many (appParser <|> varParser)
        if nextSec /= []
        then return (App (App firstSec secondSec) (head nextSec))
        else return (App firstSec secondSec)

-- Handles the let expressions
letParser :: Parser Expr
letParser = do
        symbol "let"
        varList <- varListParser
        symbol "="
        var <- appParser <|> letParser <|> varParser
        symbol "in"
        end <- appParser <|> letParser <|> varParser
        return (Let varList var end)

-- Try the given parser in this specific order, if nothing hits, return Nothing.
parseLet :: String -> Maybe Expr
parseLet s
  | parsedResult == [] = Nothing
  | otherwise = Just (fst(head(parsedResult)))
  where parsedResult = parse (appParser <|> parenHandler <|> letParser <|> varParser) s

-- Challenge 4
-- count reductions using two different strategies
isLamVar :: LamExpr -> Bool
isLamVar (LamVar _) = True
isLamVar _ = False

isLamApp :: LamExpr -> Bool
isLamApp (LamApp _ _) = True
isLamApp _ = False

isLamAbs :: LamExpr -> Bool
isLamAbs (LamAbs _ _) = True
isLamAbs _ = False

getLamAbsInput (LamAbs input _) = input
getLamAbsOutput (LamAbs _ output) = output

getLamAppEp1 (LamApp ep1 _) = ep1
getLamAppEp2 (LamApp _ ep2) = ep2

-- Case where it's (\x -> xyz)(w), inputs are x, xyz, w and output is wyz
substitute :: Int -> LamExpr -> LamExpr -> LamExpr
substitute input (LamApp ep1 ep2) replacement
  -- Handles if both expressions are LamVars. If there is a match between the input variable and the output variables,
  -- replace it with the replacement expression. Otherwise, return the same
  | isLamVar ep1 && isLamVar ep2 = if ep1Eq && ep2Eq
                                   then LamApp replacement replacement
                                   else
                                    if ep1Eq
                                    then LamApp replacement ep2
                                    else
                                      if ep2Eq
                                      then LamApp ep1 replacement
                                      else LamApp ep1 ep2
  -- This handles the case where only the first expression is a LamVar
  | isLamVar ep1 = if ep1Eq then LamApp replacement ep2 else LamApp ep1 (substitute input ep2 replacement)
  -- This handles the case where only the second expression is a LamVar
  | isLamVar ep2 = if ep2Eq then LamApp ep1 replacement else LamApp (substitute input ep1 replacement) ep2
  -- If neither one is a LamVar, recursively feed them back into substitute
  | otherwise = LamApp (substitute input ep1 replacement) (substitute input ep2 replacement)
  where match = LamVar input
        ep1Eq = ep1 == match
        ep2Eq = ep2 == match

-- Case where it's (\x -> y)(z), inputs are x, y, z and output is y
substitute input (LamVar num) replacement
  | input == num = replacement
  | otherwise = LamVar num

-- Case where scenarios like (\x -> \y -> z)(w), inputs are x, \y -> z and output is \y -> w
substitute input (LamAbs inp out) replacement = lamAbsHandler [] input (LamAbs inp out) replacement

-- Used to handle nested LamAbs like (\w -> \x -> \y -> z)(v), outputs into \x -> \y -> z
lamAbsHandler :: [Int] -> Int -> LamExpr -> LamExpr -> LamExpr
lamAbsHandler record input ep replacement
  -- If the current expression is LamAbs, dissect it into input and output and pass it back
  | isLamAbs ep = lamAbsHandler newRecord (getLamAbsInput ep) (getLamAbsOutput ep) replacement
  -- Else try to find a match between the input variable and the output variable(s). If match is found,
  -- replace it with the replacement expression. Otherwise return the output unchanged.
  | otherwise = if isLamVar ep
                then
                  if ep == varToMatch
                  then constructLamAbs newRecord replacement
                  else constructLamAbs newRecord ep
                else
                  if getLamAppEp1 ep == varToMatch
                  then constructLamAbs newRecord (LamApp replacement (getLamAppEp2 ep))
                  else
                    if getLamAppEp2 ep == varToMatch
                    then constructLamAbs newRecord (LamApp (getLamAppEp1 ep) replacement)
                    else constructLamAbs newRecord ep
  where varToMatch = LamVar (last record)
        newRecord = [input] ++ record

-- Used to construct nested LamAbs from a list of ints representing all the input variables and the output variable
-- Ex inputs: [3,2,1] LamVar 4, output: LamAbs 2 (LamAbs 1 (LamVar 4))
constructLamAbs :: [Int] -> LamExpr -> LamExpr
constructLamAbs (x:xs) ep
  | xs == [] = LamAbs x ep
  -- Due to the way the int list is constructed in lamAbsHandler the LamAbs that is reduced away is included
  -- The getLamAbsOutput is here to get rid of it
  | otherwise = getLamAbsOutput (constructLamAbs xs (LamAbs x ep))

-- This takes apart a LamApp and feed it into the substitute pipeline
lamAppHandler :: LamExpr -> LamExpr
lamAppHandler (LamApp ep1 ep2)
  | isLamAbs ep1 = substitute (getLamAbsInput ep1) (getLamAbsOutput ep1) ep2
  | otherwise = LamApp ep1 ep2

-- This makes sure only LamApp can be reduced further
reduce :: LamExpr -> LamExpr
reduce (LamApp ep1 ep2) = lamAppHandler (LamApp ep1 ep2)
reduce (LamAbs inp out) = LamAbs inp out
reduce (LamVar num) = LamVar num

leftMostExpansion :: Int -> LamExpr -> LamExpr -> Int
leftMostExpansion count leftExp rightExp
  | not (isLamApp combined) = count + 1
  | leftExp == reducedLeft && rightExp == reducedRight && not (isLamAbs leftExp) = count
  | leftExp == reducedLeft && rightExp == reducedRight = count + 1
  -- If the left is already fully reduced, start reducing the right
  | leftExp == reducedLeft = leftMostExpansion (count + 1) reducedLeft reducedRight
  -- If the left is not fully reduced, keep reducing it
  | otherwise = leftMostExpansion (count + 1) reducedLeft rightExp
  where combined = reduce (LamApp leftExp rightExp)
        reducedLeft = reduce leftExp
        reducedRight = reduce rightExp

rightMostExpansion :: Int -> LamExpr -> LamExpr -> Int
rightMostExpansion count leftExp rightExp
  | not (isLamApp combined) = count + 1
  | leftExp == reducedLeft && rightExp == reducedRight && not (isLamAbs leftExp) = count
  | leftExp == reducedLeft && rightExp == reducedRight = count + 1
  -- If the right is already reduced, start reducing the left
  | rightExp == reducedRight = rightMostExpansion (count + 1) reducedLeft reducedRight
  -- If the right is not fully reduced, keep reducing it
  | otherwise = rightMostExpansion (count + 1) leftExp reducedRight
  where combined = reduce (LamApp leftExp rightExp)
        reducedLeft = reduce leftExp
        reducedRight = reduce rightExp

countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
countReds (LamAbs _ _) _ = (Just 0, Just 0)
countReds (LamVar _) _ = (Just 0, Just 0)
countReds (LamApp left right) limit
  | leftExpansion <= limit && rightExpansion <= limit = (Just leftExpansion, Just rightExpansion)
  | leftExpansion <= limit = (Just leftExpansion, Nothing)
  | rightExpansion <= limit = (Nothing, Just rightExpansion)
  | otherwise = (Nothing, Nothing)
  where leftExpansion = leftMostExpansion 0 left right
        rightExpansion = rightMostExpansion 0 left right

-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent

makeLamApp :: Int -> LamExpr -> LamExpr
makeLamApp count ep
  | count == 0 = ep
  | otherwise = makeLamApp (count - 1) (LamApp (LamVar 1) ep)

parseSimpleAdditionTail :: Parser [Int]
parseSimpleAdditionTail = do
        symbol "+"
        arg1 <- nat
        otherArgs <- many (parseSimpleAdditionTail)
        return ([arg1] ++ concat otherArgs)

-- Handles cases such as 1 + 1
parseSimpleAddition :: Parser LamExpr
parseSimpleAddition = do
        arg1 <- nat
        otherArgs <- parseSimpleAdditionTail
        return (LamAbs 1 (LamAbs 2 (makeLamApp (sum ([arg1] ++ otherArgs)) (LamVar 2))))

sumAllBrackets :: Parser Int
sumAllBrackets = do
        symbol "(+"
        num <- nat
        symbol ")"
        rest <- many (sumAllBrackets)
        return (sum ([num] ++ rest))

-- Handles cases such as (+1) (+3) 1
parseCurriedAddition :: Parser LamExpr
parseCurriedAddition = do
        brackets <- sumAllBrackets
        num <- nat
        return (LamAbs 1 (LamAbs 2 (makeLamApp (num + brackets) (LamVar 2))))

parseComplexAdditionTail :: Parser Int
parseComplexAdditionTail = do
        symbol "+"
        brackets <- sumAllBrackets
        num <- nat
        rest <- many (parseComplexAdditionTail)
        return (brackets + num + sum(rest))

-- Handles cases such as (+1) 1 + (+1) 1
parseComplexAddition :: Parser LamExpr
parseComplexAddition = do
        brackets <- sumAllBrackets
        num <- nat
        rest <- parseComplexAdditionTail
        return (LamAbs 1 (LamAbs 2 (makeLamApp (brackets + num + rest) (LamVar 2))))

-- Handles cases such as 5
parseSingleNum :: Parser LamExpr
parseSingleNum = do
        num <- nat
        return (LamAbs 1 (LamAbs 2 (makeLamApp num (LamVar 2))))

inputExpr = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))
-- Handles cases such as (+2)
parseSuccessor :: Parser LamExpr
parseSuccessor = do
        bracketSum <- sumAllBrackets
        return (LamApp (LamAbs 1 (LamAbs 2 (makeLamApp (bracketSum) (LamVar 2)))) inputExpr)

-- Syntactic sugar for the Parser function chain
parserFuncs :: Parser LamExpr
parserFuncs = parseComplexAddition <|> parseCurriedAddition <|> parseSimpleAddition <|> parseSingleNum <|> parseSuccessor

compileArith :: String -> Maybe LamExpr
compileArith s
  | parsedResult == [] || snd(head(parsedResult)) /= "" = Nothing
  | otherwise = Just (fst(head(parsedResult)))
  where parsedResult = parse (parserFuncs) s


main = do
--  convertLet(let x1 = x1 in x2)
  putStrLn(prettyPrint(Let [1,2] (Var 2) (App (Var 3) (Var 1))))
--  putStrLn(prettyPrint(Let [1] (Var 2) (Var 1)))
