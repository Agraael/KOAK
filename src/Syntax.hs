module Syntax
where

import Data.Char
import Data.List
import Data.Maybe
import Data.Typeable

import TokenLexer

data Expr = Lit Literal
          | BinOp Operator Expr Expr
          | Var String -- à remplacer par Var Type et retrouver correctement les types
          | Call String [Expr]
          | Function String [Expr] [Expr] Expr
          | If Expr Expr
          | While Expr Expr
          | End Int
          deriving (Eq, Show)


buildDefs :: [Expr] -> ([Expr], [Expr]) -> ([Expr], [Expr])
buildDefs [] res = res
buildDefs (expr:exprs) (defs, mainDefs) = do
  case expr of
    Function _ _ _ _ -> buildDefs exprs (defs ++ [expr], mainDefs)
    _ -> buildDefs exprs (defs, mainDefs ++ [expr])

buildAST' :: [Token] -> [Expr] -> [Expr]
buildAST' tokens res = do
  let currStatement = takeWhileNotTok tokens [] "EndStatement"
  let remTokens = dropWhileNotTok tokens "EndStatement"
  case currStatement of
    [] -> res
    _ -> do
      let exprRes = getExpr currStatement
      case exprRes of
        Nothing -> res ++ [End 0]
        _ -> ([snd (fromJust exprRes)] ++ (buildAST' remTokens res))

tokOperators = ["Plus", "Minus", "Mult"]

checkTokType :: Token -> String
checkTokType (TokKeyWord Def) = "Def"
checkTokType (TokIdentifier _) = "Identifier"
checkTokType (TokParenthesis Open) = "Open"
checkTokType (TokOperator Plus) = "Plus"
checkTokType (TokOperator Minus) = "Minus"
checkTokType (TokOperator Assignment) = "Assignment"
checkTokType (TokParenthesis Close) = "Close"
checkTokType (TokSpecialChar EndStatement) = "EndStatement"
checkTokType (TokSpecialChar Colon) = "Colon"
checkTokType (TokLiteral _) = "Literal"
checkTokType (TokType t) = "Type"
checkTokType _ = "Undefined"

tokVal :: Token -> String
tokVal (TokIdentifier xs) = xs
tokVal _ = ""

exprVal :: Expr -> String
exprVal (Var id) = id
exprVal _ = "ERROR IN EXPRVAL"

dropWhileNotTok :: [Token] -> String -> [Token]
dropWhileNotTok [] _ = []
dropWhileNotTok (tok:tokens) tokType 
  | checkTokType tok == tokType = tokens
  | otherwise = dropWhileNotTok tokens tokType

takeWhileNotTok :: [Token] -> [Token] -> String -> [Token]-- enlever la liste vide
takeWhileNotTok [] res tokType = res
takeWhileNotTok (tok:tokens) res tokType
  | checkTokType tok == tokType = (res ++ [tok])
  | otherwise = takeWhileNotTok tokens (res ++ [tok]) tokType

checkToken :: Token -> Expr -- à faire sur de grandes exprs
checkToken (TokLiteral lit) = Lit lit
checkToken (TokIdentifier id) = Var id
checkToken (TokType t) = Var "Double"
checkToken _ = Var "ERROR"

getExprType :: [Token] -> Expr
getExprType tokens
  | checkTokType (head tokens) == "Identifier"
    && checkTokType (head $ tail tokens) == "Open" = Var "Call"
  | checkTokType (head tokens) == "Literal" = checkToken $ head tokens
  | otherwise = Var "Error in getExprType"

getArgsValues :: [Token] -> [Expr] -> ([Token], [Expr]) -- Pas bon, il faut pouvoir évaluer les exprs.
getArgsValues (tok:tokens) res
  | checkTokType tok == "Close" = (tokens, res)
  | getExpr ([tok] ++ tokens) /= Nothing = do
      let resExpr = getExpr ([tok] ++ tokens)
      let remTokens = fst $ fromJust resExpr
      let expr = snd $ fromJust resExpr
      case expr of
        _ -> getArgsValues remTokens (res ++ [expr])
  | otherwise = getArgsValues tokens res

isLit :: [Token] -> Bool
isLit [] = False
isLit tokens = checkTokType(head tokens) == "Literal"

isVar :: [Token] -> Bool
isVar [] = False
isVar tokens = checkTokType(head tokens) == "Identifier"

lit :: [Token] -> Maybe ([Token], Expr)
lit (tok:tokens) = Just (tokens, checkToken tok)
lit _ = Nothing

var :: [Token] -> Maybe ([Token], Expr)
var (tok:tokens) = Just (tokens, checkToken tok)
var _ = Nothing

getFuncName' :: [Token] -> Maybe ([Token], String)
getFuncName' tokens = do
  let defType = checkTokType (head tokens)
  let nameArg = head (tail tokens)
  let nameType = checkTokType (head (tail tokens))
  case defType == "Def" && nameType == "Identifier" of
    True -> Just (drop 3 tokens, tokVal nameArg)
    _ -> Nothing

getFuncRet :: [Token] -> Maybe ([Token], Expr)
getFuncRet tokens = do
  case head tokens of
    TokSpecialChar Colon -> do
      let retType = checkToken (head $ tail tokens)
      let remTokens = tail $ tail tokens
      case retType of
        _ -> Just (remTokens, retType)
    _ -> Nothing

isFunc :: [Token] -> Bool
isFunc [] = False
isFunc tokens = checkTokType (head tokens) == "Def"

func :: [Token] -> Maybe ([Token], Expr)
func tokens = do
  let nameRes = getFuncName' tokens
  case nameRes of
    Nothing -> Nothing
    _ -> do
      let remTokens = fst $ fromJust nameRes
      let name = snd $ fromJust nameRes
      let argsRes = getArgsValues remTokens []
      let remTokens' = fst argsRes
      let args = snd argsRes
      let retValRes = getFuncRet remTokens'
      case retValRes of
        Nothing -> Nothing
        _ -> do
          let remTokens'' = fst $ fromJust retValRes
          let retVal = snd $ fromJust retValRes
          let bodyRes = getExpr remTokens''
          case bodyRes of
            Nothing -> Nothing
            _ -> do
              let remTokens''' = fst $ fromJust bodyRes
              let body = snd $ fromJust bodyRes
              case body of
                _ -> Just (remTokens''', Function name args [body] retVal)

call :: [Token] -> Maybe ([Token], Expr)
call tokens = do
  let name = tokVal (head tokens)
  case name of
    "" -> Nothing
    _ -> do
      let remTokens = tail tokens
      let open = checkTokType (head remTokens) == "Open"
      case open of
        False -> Nothing
        True -> do
          let remTokens' = tail remTokens
          let argsRes = getArgsValues remTokens' []
          let remTokens'' = fst argsRes
          let args = snd argsRes
          case args of
            _ -> Just (remTokens'', Call name args)
      
getExpr' :: [Token] -> Maybe ([Token], Expr)
getExpr' [] = Nothing
getExpr' tokens
  | isCall tokens = call tokens
  | isLit tokens = lit tokens
  | isVar tokens = var tokens
  | otherwise = Nothing

condExpr :: [Token] -> Maybe ([Token], Expr)
condExpr [] = Nothing
condExpr tokens
  | isBinOp Equal tokens = binOp Equal tokens Nothing
  | isBinOp Unequal tokens = binOp Unequal tokens Nothing
  | isBinOp Lesser tokens = binOp Lesser tokens Nothing
  | isBinOp Greater tokens = binOp Greater tokens Nothing
  | isBinOp Not tokens = binOp Not tokens Nothing
  | isBinOp Assignment tokens = binOp Assignment tokens Nothing
  | isCall tokens = call tokens
  | isLit tokens = lit tokens
  | isVar tokens = var tokens
  | otherwise = Nothing
                
getExpr :: [Token] -> Maybe ([Token], Expr)
getExpr [] = Nothing
getExpr tokens
  | isIf tokens = if' tokens
  | isWhile tokens = while' tokens
  | isBinOp Mult tokens = binOpMult tokens Nothing
  | isBinOp Div tokens = binOpDiv tokens Nothing
  | isBinOp Plus tokens = binOp Plus tokens Nothing
  | isBinOp Minus tokens = binOp Minus tokens Nothing
  | isBinOp Assignment tokens = binOp Assignment tokens Nothing
  | isCall tokens = call tokens
  | isLit tokens = lit tokens
  | isVar tokens = var tokens
  | isFunc tokens = func tokens
  | otherwise = Nothing

isBinOp :: Operator -> [Token] -> Bool
isBinOp _ [TokSpecialChar EndStatement] = False
isBinOp op tokens = do
  let leftExprRes = getExpr' tokens
  case leftExprRes of
    Nothing -> False
    _ -> do
      let remTokens = fst (fromJust leftExprRes)
      let leftExpr = snd (fromJust leftExprRes)
      case head remTokens == TokOperator op of
        False -> False
        True -> do
          let rightExprRes = getExpr' (tail remTokens)
          case rightExprRes of
            Nothing -> False
            _ -> True

binOp :: Operator -> [Token] -> Maybe Expr -> Maybe ([Token], Expr)
binOp op tokens Nothing =  do
  let leftExprRes = getExpr' tokens
  case leftExprRes of
    Nothing -> Nothing
    _ -> do
      let remTokens = fst (fromJust leftExprRes)
      let leftExpr = snd (fromJust leftExprRes)
      case head remTokens == TokOperator op of
        False -> Nothing
        True -> do
          let rightExprRes = getExpr (tail remTokens)
          case rightExprRes of
            Nothing -> Nothing
            _ -> do
              let remTokens' = fst (fromJust rightExprRes)
              let rightExpr = snd (fromJust rightExprRes)
              case rightExpr of
                _ -> Just (remTokens', BinOp op leftExpr rightExpr)

binOp op tokens (Just leftExpr) = do
  case head tokens == TokOperator op of
    False -> Nothing
    True -> do
      let rightExprRes = getExpr (tail tokens)
      case rightExprRes of
        Nothing -> Nothing
        _ -> do
          let remTokens' = fst (fromJust rightExprRes)
          let rightExpr = snd (fromJust rightExprRes)
          case rightExpr of
            _ -> Just (remTokens', BinOp op leftExpr rightExpr)
      
followedByOp :: [Token] -> Maybe Operator
followedByOp [] = Nothing
followedByOp tokens
  | head tokens == TokOperator Plus = Just Plus
  | head tokens == TokOperator Minus = Just Minus
  | otherwise = Nothing

isOp :: Expr -> Bool
isOp (BinOp _ _ _) = True
isOp _ = False

doOp :: Operator -> [Token] -> Maybe Expr -> Maybe ([Token], Expr)
doOp op tokens leftExpr = binOp op tokens leftExpr

isIf :: [Token] -> Bool
isIf tokens = do
  case length tokens < 6 of
    True -> False
    False -> do
      case head tokens == TokKeyWord TokenLexer.If of
        False -> False
        True -> do
          let conditionRes = condExpr $ tail tokens
          case conditionRes of
            Nothing -> False
            _ -> do
              let remTokens' = fst $ fromJust conditionRes
              let condition = snd $ fromJust conditionRes
              case head remTokens' == TokKeyWord TokenLexer.Then of
                False -> False
                True -> do
                  let thenResult = getExpr $ tail remTokens'
                  case thenResult of
                    Nothing -> False
                    _ -> True
                    
if' :: [Token] -> Maybe ([Token], Expr)
if' tokens = do
  case length tokens < 6 of
    True -> Nothing
    False -> do
      case head tokens == TokKeyWord TokenLexer.If of
        False -> Nothing
        True -> do
          let conditionRes = condExpr $ tail tokens
          case conditionRes of
            Nothing -> Nothing
            _ -> do
              let remTokens' = fst $ fromJust conditionRes
              let condition = snd $ fromJust conditionRes
              case head remTokens' == TokKeyWord TokenLexer.Then of
                False -> Nothing
                True -> do
                  let thenResult = getExpr $ tail remTokens'
                  case thenResult of
                    Nothing -> Nothing
                    _ -> do
                      let remTokens'' = fst $ fromJust thenResult
                      let res = snd $ fromJust thenResult
                      case res of
                        _ -> Just (remTokens'', Syntax.If condition res)
                          
while' :: [Token] -> Maybe ([Token], Expr)
while' tokens = do
  case length tokens < 6 of
    True -> Nothing
    False -> do
      case head tokens == TokKeyWord TokenLexer.While of
        False -> Nothing
        True -> do
          let conditionRes = condExpr $ tail tokens
          case conditionRes of
            Nothing -> Nothing
            _ -> do
              let remTokens' = fst $ fromJust conditionRes
              let condition = snd $ fromJust conditionRes
              case head remTokens' == TokKeyWord TokenLexer.Do of
                False -> Nothing
                True -> do
                  let thenResult = getExpr $ tail remTokens'
                  case thenResult of
                    Nothing -> Nothing
                    _ -> do
                      let remTokens'' = fst $ fromJust thenResult
                      let res = snd $ fromJust thenResult
                      case res of
                        _ -> Just (remTokens'', Syntax.While condition res)

isWhile :: [Token] -> Bool
isWhile tokens = do
  case length tokens < 6 of
    True -> False
    False -> do
      case head tokens == TokKeyWord TokenLexer.While of
        False -> False
        True -> do
          let conditionRes = condExpr $ tail tokens
          case conditionRes of
            Nothing -> False
            _ -> do
              let remTokens' = fst $ fromJust conditionRes
              let condition = snd $ fromJust conditionRes
              case head remTokens' == TokKeyWord TokenLexer.Do of
                False -> False
                True -> do
                  let thenResult = getExpr $ tail remTokens'
                  case thenResult of
                    Nothing -> False
                    _ -> True

isCall :: [Token] -> Bool
isCall tokens = do
  let name = tokVal (head tokens)
  case name of
    "" -> False
    _ -> do
      let remTokens = tail tokens
      let open = checkTokType (head remTokens) == "Open"
      case open of
        False -> False
        True -> do
          let remTokens' = tail remTokens
          let argsRes = getArgsValues remTokens' []
          let remTokens'' = fst argsRes
          let args = snd argsRes
          case args of
            _ -> True  
  
binOpMult :: [Token] -> Maybe Expr -> Maybe ([Token], Expr)
binOpMult tokens Nothing =  do
  let leftExprRes = getExpr' tokens
  case leftExprRes of
    Nothing -> Nothing
    _ -> do
      let remTokens = fst (fromJust leftExprRes)
      let leftExpr = snd (fromJust leftExprRes)
      case head remTokens == TokOperator Mult of
        False -> Nothing
        True -> do
          let rightExprRes = getExpr' (tail remTokens)
          let remTokens' = fst (fromJust rightExprRes)
          let rightExpr = snd (fromJust rightExprRes)
          let priority = followedByOp remTokens'
          case priority of
            Just op -> doOp op remTokens' (Just (BinOp Mult leftExpr rightExpr))
            Nothing -> do
              let rightExprRes' = getExpr (tail remTokens)
              case rightExprRes' of
                Nothing -> Nothing
                _ -> do
                  let remTokens'' = fst (fromJust rightExprRes')
                  let rightExpr' = snd (fromJust rightExprRes')
                  case rightExpr' of
                    _ -> Just (remTokens'', BinOp Mult leftExpr rightExpr')
               
  
binOpDiv :: [Token] -> Maybe Expr -> Maybe ([Token], Expr)
binOpDiv tokens Nothing =  do
  let leftExprRes = getExpr' tokens
  case leftExprRes of
    Nothing -> Nothing
    _ -> do
      let remTokens = fst (fromJust leftExprRes)
      let leftExpr = snd (fromJust leftExprRes)
      case head remTokens == TokOperator Div of
        False -> Nothing
        True -> do
          let rightExprRes = getExpr' (tail remTokens)
          let remTokens' = fst (fromJust rightExprRes)
          let rightExpr = snd (fromJust rightExprRes)
          let priority = followedByOp remTokens'
          case priority of
            Just op -> doOp op remTokens' (Just (BinOp Div leftExpr rightExpr))
            Nothing -> do
              let rightExprRes' = getExpr (tail remTokens)
              case rightExprRes' of
                Nothing -> Nothing
                _ -> do
                  let remTokens'' = fst (fromJust rightExprRes')
                  let rightExpr' = snd (fromJust rightExprRes')
                  case rightExpr' of
                    _ -> Just (remTokens'', BinOp Div leftExpr rightExpr')
               
