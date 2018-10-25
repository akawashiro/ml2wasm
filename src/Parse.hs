{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Parse (stringToExp, Exp(..), Type(..), TVarIndex, Op(..), Var(..)) where

import           Control.Monad.Identity
import           Control.Monad.State
import           Data.List
import qualified Text.Parsec.Combinator                 as C (chainl1, chainr1)
import           Text.ParserCombinators.Parsec          hiding (State)
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

stringToExp :: String -> Either String Exp
stringToExp input = either (Left . show) (Right . renameTypeUnit) (parse parseExp "Parse.hs" input)

type TVarIndex = Int
data Type = TInt | 
            -- TBool | 
            TFloat | 
            TFun [Type] Type | 
            TTuple [Type] | 
            TArray Type | 
            TUnit | 
            TVar TVarIndex 
            deriving (Eq, Show)

data Var = Var Type String deriving Eq
instance Show Var where
  show (Var t s) = s ++ ":" ++ show t

data Op = OLess | OPlus | OMinus | OTimes | ODiv | OFLess | OFPlus | OFMinus | OFTimes | OFDiv deriving Eq
instance Show Op where
  show OLess  = "<"
  show OPlus  = "+"
  show OMinus = "-"
  show OTimes = "*"
  show ODiv = "/"
  show OFLess  = "<."
  show OFPlus  = "+."
  show OFMinus = "-."
  show OFTimes = "*."
  show OFDiv = "/."


data Exp = EInt Type Int |
           EFloat Type Float |
           EBool Type Bool |
           EOp Type Op Exp Exp |
           EIf Type Exp Exp Exp |
           ELet Type Var Exp Exp |
           EDTuple Type [Var] Exp Exp |
           EVar Type Var |
           ERec Type Var [Var] Exp Exp |
           EApp Type Exp [Exp] |
           ETuple Type [Exp] |
           ESeq Type Exp Exp |
           EMakeA Type Exp |
           EGetA Type Exp Exp |
           ESetA Type Exp Exp Exp |
           EPrintI32 Type Exp |
           EPrintF32 Type Exp 
           deriving Eq

instance Show Exp where
  show (EInt t i) = show i ++ ":" ++ show t
  show (EFloat t f) = show f ++ ":" ++ show t
  show (EBool t b) = if b then "true" else "false" ++ ":" ++ show t
  show (EIf t e1 e2 e3) = "(if " ++ show e1 ++ "\nthen " ++ show e2 ++ "\nelse " ++ show e3 ++ "):" ++ show t
  show (EOp t o e1 e2) = "(" ++ show o ++ " " ++ show e1 ++ " " ++ show e2 ++ ")" ++ ":" ++ show t
  show (ELet t v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in\n" ++ show e2 ++ ":" ++ show t
  show (EDTuple t vs e1 e2) = "let (" ++ intercalate ", " (map show vs) ++ ") = " ++ show e1 ++ " in\n" ++ show e2 ++ " : " ++ show t
  show (EVar t v) = show v -- ++ ":" ++ show t
  show (ERec t x ys e1 e2) = "(let rec " ++ show x ++ " " ++ show ys ++ " = " ++ show e1 ++ " in\n" ++ show e2 ++ "):" ++ show t
  show (EApp t e1 e2s) = "(" ++ show e1 ++ " " ++ show e2s ++ "):" ++ show t
  show (ETuple t es) = "(" ++ intercalate ", " (map show es) ++ ")" ++ ":" ++ show t
  show (EMakeA t e1) = "(make_array " ++ show e1 ++ "):" ++ show t
  show (EGetA t e1 e2) = "(get_array " ++ show e1 ++ " " ++ show e2 ++ "):" ++ show t
  show (ESetA t e1 e2 e3) = "(set_array " ++ show e1 ++ " " ++ show e2  ++ " " ++ show e3 ++ "):" ++ show t
  show (ESeq t e1 e2) = show e1 ++ ";\n" ++ show e2 ++ "):" ++ show t
  show (EPrintI32 t e1) = "(print_i32 " ++ show e1 ++ "):" ++ show t
  show (EPrintF32 t e1) = "(print_f32 " ++ show e1 ++ "):" ++ show t

renameTypeUnit :: Exp -> Exp
renameTypeUnit e = evalState (rt e) (0,[])

ntv :: State (TVarIndex, [(String, TVarIndex)]) Type
ntv = do
  (i,m) <- get
  put (i+1,m)
  return (TVar i)

rt' :: Var -> State (TVarIndex, [(String, TVarIndex)]) Var
rt' (Var _ s) = do
  (i,m) <- get
  case lookup s m of
    Just x -> return (Var (TVar x) s)
    Nothing -> do
      put (i+1, (s,i):m)
      return (Var (TVar i) s)

rt :: Exp -> State (TVarIndex, [(String, TVarIndex)]) Exp
rt (EInt _ i) = return (EInt TInt i)
rt (EFloat _ f) = return (EFloat TFloat f)
rt (EBool _ b) = return (EBool TInt b)
rt (EOp _ o e1 e2) = EOp <$> ntv <*> pure o <*> rt e1 <*> rt e2
rt (EIf _ e1 e2 e3) = EIf <$> ntv <*> rt e1 <*> rt e2 <*> rt e3
rt (ELet _ v e1 e2) = ELet <$> ntv <*> rt' v <*> rt e1 <*> rt e2
rt (EDTuple _ vs e1 e2) = EDTuple <$> ntv <*> mapM rt' vs <*> rt e1 <*> rt e2
rt (EVar _ v) = EVar <$> ntv <*> rt' v
rt (ERec _ x ys e1 e2) = ERec <$> ntv <*> rt' x <*> mapM rt' ys <*> rt e1 <*> rt e2
rt (EApp _ e1 e2) = EApp <$> ntv <*> rt e1 <*> mapM rt e2
rt (ETuple _ es) = ETuple <$> ntv <*> mapM rt es
rt (ESeq _ e1 e2) = ESeq <$> ntv <*> rt e1 <*> rt e2
rt (EMakeA _ e1) = EMakeA <$> ntv <*> rt e1
rt (EGetA _ e1 e2) = EGetA <$> ntv <*> rt e1 <*> rt e2
rt (ESetA _ e1 e2 e3) = ESetA <$> ntv <*> rt e1 <*> rt e2 <*> rt e3
rt (EPrintI32 _ e) = EPrintI32 <$> pure TUnit <*> rt e
rt (EPrintF32 _ e) = EPrintF32 <$> pure TUnit <*> rt e

natDef :: P.GenLanguageDef String () Identity
natDef = emptyDef { P.reservedNames = keywords, P.reservedOpNames = operators }

keywords :: [String]
keywords = [ "let", "rec", "in", "true", "false", "if", "then", "else", "fun"]

operators = [ "=", "->", "+", "-", "*", "<", ",", ";", "/"]


kwLet         = P.reserved lexer "let"
kwRec         = P.reserved lexer "rec"
kwIn          = P.reserved lexer "in"
kwTrue        = P.reserved lexer "true"
kwFalse       = P.reserved lexer "false"
kwIf          = P.reserved lexer "if"
kwThen        = P.reserved lexer "then"
kwElse        = P.reserved lexer "else"
kwFun         = P.reserved lexer "fun"
kwEqual       = P.reservedOp lexer "="
kwArrowSymbol = P.reservedOp lexer "->"
kwPlusSymbol  = P.reservedOp lexer "+"
kwMinusSymbol = P.reservedOp lexer "-"
kwTimesSymbol = P.reservedOp lexer "*"
kwDivSymbol   = P.reservedOp lexer "/"
kwLessSymbol  = P.reservedOp lexer "<"
kwCommaSymbol = P.reservedOp lexer ","
kwSeqSymbol   = P.reservedOp lexer ";"
kwFPlusSymbol  = P.reservedOp lexer "+."
kwFMinusSymbol = P.reservedOp lexer "-."
kwFTimesSymbol = P.reservedOp lexer "*."
kwFDivSymbol   = P.reservedOp lexer "/."
kwFLessSymbol  = P.reservedOp lexer "<."


lexer = P.makeTokenParser natDef
parens = P.parens lexer
whiteSpace = P.whiteSpace lexer

parseExp :: Parser Exp
parseExp = do
  e1<-parseExpUni
  (kwSeqSymbol >> ESeq TUnit e1 <$> parseExpUni) <|> return e1

parseExpUni :: Parser Exp
parseExpUni = parseExpIf <|>
           try parseExpLet <|>
           try parseExpDTuple <|>
           parseExpRec <|>
           parseExpLt

parseExpIf :: Parser Exp
parseExpIf = do kwIf; e1<-parseExp; kwThen; e2<-parseExp; kwElse; EIf TUnit e1 e2 <$> parseExp;

parseExpLet :: Parser Exp
parseExpLet = do kwLet; x<-parseVar; kwEqual; e1<-parseExp; kwIn; ELet TUnit x e1 <$> parseExp;

parseExpDTuple :: Parser Exp
parseExpDTuple = do
  kwLet
  vs <- parens parseVs
  kwEqual
  e1 <- parseExp
  kwIn
  EDTuple TUnit vs e1 <$> parseExp
    where parseVs = do
            h <- parseVar
            t <- many (kwCommaSymbol >> parseVar)
            return (h:t)

parseExpRec :: Parser Exp
parseExpRec = do kwLet; kwRec; x<-parseVar; ys <- many1 parseVar; kwEqual; e1<-parseExp; kwIn; ERec TUnit x ys e1 <$> parseExp;

parseExpLt :: Parser Exp
parseExpLt =  do
  e1<-parseExpP
  (kwLessSymbol >> EOp TUnit OLess e1 <$> parseExpP) <|> 
    (kwFLessSymbol >> EOp TUnit OFLess e1 <$> parseExpP) <|> 
    return e1

parseExpP :: Parser Exp
parseExpP = C.chainl1 parseExpM ((kwPlusSymbol >> return (EOp TUnit OPlus)) <|> (kwFPlusSymbol >> return (EOp TUnit OFPlus)))

parseExpM :: Parser Exp
parseExpM = C.chainl1 parseExpT ((kwMinusSymbol >> return (EOp TUnit OMinus)) <|> (kwFMinusSymbol >> return (EOp TUnit OFMinus)))

parseExpT :: Parser Exp
parseExpT = C.chainl1 parseExpD ((kwTimesSymbol >> return (EOp TUnit OTimes)) <|> (kwFTimesSymbol >> return (EOp TUnit OFTimes)))

parseExpD :: Parser Exp
parseExpD = C.chainl1 parseExpApp ((kwDivSymbol >> return (EOp TUnit ODiv)) <|> (kwFDivSymbol >> return (EOp TUnit OFDiv)))

parseExpApp :: Parser Exp
parseExpApp = do
  es <- many1 parseExpAtom
  if length es == 1 then return (head es) else return (f es)
    where f es = case head es of
                  (EVar t (Var t' "print_i32")) -> EPrintI32 TUnit (es !! 1)
                  (EVar t (Var t' "print_f32")) -> EPrintF32 TUnit (es !! 1)
                  (EVar t (Var t' "make_array")) -> EMakeA TUnit (es !! 1)
                  (EVar t (Var t' "get_array")) -> EGetA TUnit (es !! 1) (es !! 2)
                  (EVar t (Var t' "set_array")) -> ESetA TUnit (es !! 1) (es !! 2) (es !! 3)
                  _ -> EApp TUnit (head es) (tail es)

parseExpTuple :: Parser Exp
parseExpTuple = do
  h <- parseExp
  t <- many (kwCommaSymbol >> parseExp)
  if null t then return h else return (ETuple TUnit (h:t))

parseExpAtom :: Parser Exp
parseExpAtom = parens parseExpTuple
               <|> (EBool TUnit <$> parseBool)
               <|> try (EFloat TUnit <$> parseFloat)
               <|> try (EInt TUnit <$> parseInt)
               <|> (EVar TUnit <$> parseVar)

parseBool :: Parser Bool
parseBool = (kwTrue >> return True) <|> (kwFalse >> return False)

parseFloat :: Parser Float
parseFloat = (do whiteSpace; char '-'; e <- f; whiteSpace; return (-1 * e))
           <|> (do whiteSpace; e<-f; whiteSpace; return e)
           where f = do
                  d1 <- many1 digit
                  char '.'
                  d2 <- many1 digit
                  return (read (d1 ++ ['.'] ++ d2))

parseInt :: Parser Int
parseInt = (do whiteSpace; char '-'; ds<-many1 digit; whiteSpace; return $ -1 * read ds)
           <|> (do whiteSpace; ds<-many1 digit; whiteSpace; return $ read ds)

parseVar :: Parser Var
parseVar = Var TUnit <$> P.identifier lexer

