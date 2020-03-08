module TokenLexer where

import           Data.Char
import           System.Environment
import           System.Exit
import           Text.Printf

data Encapsulate = Open | Close
    deriving (Show, Eq)

data Operator = Plus | Minus | Mult | Div |
                Lesser | Greater | Equal | Unequal | Assignment | Not
    deriving (Show, Eq)
defOperator :: [(Operator, String)]
defOperator = [(Plus, "+"), (Minus, "-"), (Mult, "*"), (Div, "/"), (Lesser, "<"), (Greater, ">"),
                (Equal, "=="), (Unequal, "!="), (Assignment, "="), (Not, "!")]

data Literal = Decimal Int | Floating Double | Text String
    deriving (Show, Eq)

data KeyWord = For | While | If | Else | Then | Do | In | Def
    deriving (Show, Eq)
defKeyWord :: [(KeyWord, String)]
defKeyWord = [(While, "while"), (Else, "else"), (Then, "then"), (For, "for"),
                (Def, "def"), (If, "if"), (Do, "do"), (In, "in")]


data Type = Int | Double
    deriving (Show, Eq)
defType :: [(Type, String)]
defType = [(Int, "int"), (Double, "double")]

data SpecialChar = EndStatement | Colon
    deriving (Show, Eq)
defSpecialChar :: [(SpecialChar, String)]
defSpecialChar = [(EndStatement, ";"), (Colon, ":")]

data Token = TokParenthesis Encapsulate
            | TokOperator Operator
            | TokIdentifier String
            | TokLiteral Literal
            | TokType Type
            | TokKeyWord KeyWord
            | TokSpecialChar SpecialChar
            | TokEnd
            | TokErr String
    deriving (Show, Eq)



wordIn :: String -> String -> Bool
wordIn [] [] = False
wordIn word text = if word == search
    then if charLast1 == charLast0
        then True
        else if isAlphaNum charLast1 && isAlphaNum charLast0
            then False
            else True
    else False
    where search = take (length word) text
          charLast0 = (last (take ((length word)) text))
          charLast1 = (last (take ((length word) + 1) text))

findSymbol :: [(any, String)] -> String -> Bool
findSymbol [] _ = False
findSymbol ((_, symbol) : xs) text =
    if wordIn symbol text
        then True
        else findSymbol xs text

getSymbolLength :: [(any, String)] -> String -> Int
getSymbolLength [] _ = 0
getSymbolLength ((_, symbol) : xs) text =
    if wordIn symbol text
        then length symbol
        else getSymbolLength xs text

getSymbolData :: [(any, String)] -> String -> any
getSymbolData [] _ = undefined
getSymbolData ((val, symbol) : xs) text =
    if wordIn symbol text
        then val
        else getSymbolData xs text

isDouble :: Char -> Bool
isDouble c = c `elem` "0123456789."

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

identifier :: Char -> String -> [Token]
identifier c cs = let (digs, cs') = span isAlphaNum cs
                    in TokIdentifier (c : digs) : getTokens cs'

number :: Char -> String -> [Token]
number c cs = let (digs, cs') = span isDouble cs
                in case countLetters (c : digs) '.' of
                    0 -> TokLiteral (Decimal (read (c : digs) :: Int)) : getTokens cs'
                    1 -> if last (c : digs) /= '.'
                            then TokLiteral (Floating (read ('0' : (c : digs)) :: Double)) : getTokens cs'
                            else [TokErr "error floating literal"]
                    _ -> [TokErr "error number literal"]

getTokens :: String -> [Token]
getTokens [] = []
getTokens text
    | findSymbol defKeyWord text = TokKeyWord (getSymbolData defKeyWord text) : getTokens (drop (getSymbolLength defKeyWord text) text)
    | findSymbol defOperator text = TokOperator (getSymbolData defOperator text) : getTokens (drop (getSymbolLength defOperator text) text)
    | findSymbol defSpecialChar text = TokSpecialChar (getSymbolData defSpecialChar text) : getTokens (drop (getSymbolLength defSpecialChar text) text)
    | findSymbol defType text = TokType (getSymbolData defType text) : getTokens (drop (getSymbolLength defType text) text)
    | c == '(' = TokParenthesis Open : getTokens cs
    | c == ')' = TokParenthesis Close : getTokens cs
    | isSpace c = getTokens cs
    | isAlpha c = identifier c cs
    | isDouble c = number c cs
    | otherwise = [TokErr ("Cannot getTokens ")]
    where
        c = (head text)
        cs = (tail text)
