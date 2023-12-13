module Expr where
import Parsing
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Test.QuickCheck
import Control.Applicative (Applicative(liftA2))
import Numeric (showFFloat)
import Test.QuickCheck.Gen (genDouble)



-- A
data Expr = Num Double 
        | Var 
        | Operator Char Expr Expr 
        | Function [Char] Expr
        deriving(Eq,Show)

x :: Expr
x = Var

num :: Double -> Expr
num  = Num

add,mul :: Expr -> Expr -> Expr
add  = Operator '+' 
mul  = Operator '*' 

sin,cos :: Expr -> Expr
sin  = Function "sin"
cos  = Function "cos"

size :: Expr -> Int
size (Function _ e)     =  1 + size e
size (Operator _ e1 e2) =  1 + size e1 + size e2
size Var = 0
size (Num _) = 0



-- -- B 
showExpr :: Expr -> String
showExpr (Num a) = show a
showExpr Var = "x"
showExpr (Operator '+' e1 e2) 
 = showExpr e1 ++ ['+'] ++ showExpr e2
showExpr (Operator '*' e1 e2) 
 = showFactor e1 ++ ['*'] ++ showFactor e2
 where --binding (abcdef) to e --e@(abcdef)
    showFactor e@(Operator '+' _ _) = "(" ++ showExpr e ++ ")"
    showFactor e = showExpr e

showExpr (Function s1 (Function s2 e)) 
 = s1 ++ " " ++ showExpr (Function s2 e) ++ " "

showExpr (Function a Var) = a ++ " " ++ showExpr x ++ " "

showExpr (Function a (Num n)) = a ++ " " ++ showExpr (Num n) ++ " "

showExpr (Function a e) 
   = a ++ "(" ++ showExpr e ++ ")"



-- -- C
eval :: Expr -> Double -> Double
eval (Num a) _ = a
eval Var d = d

eval (Operator '+' e1 e2) d 
 = eval e1 d + eval e2 d
eval (Operator '*' e1 e2) d 
  = eval e1 d * eval e2 d

eval (Function "sin" e) d  
 = Prelude.sin (eval e d)
eval (Function "cos" e) d 
 = Prelude.cos (eval e d)



-- --------- D
{-
expr   ::= term {"+" term}
term   ::= factor {"*" factor}
factor ::= numbor | "(" expr ")" | func | Var
func   ::= "sin(" expr ")" | "cos(" expr ")" | "sin" x
          | "cos" x | "sin" func | "cos" func
-}

number :: Parser Double
number = do
    n <- oneOrMore digit
    char '.'
    m <- oneOrMore digit
    let d = read (n++"."++m) :: Double
    return d


expr, term, factor, funcSin, funcCos :: Parser Expr
expr = foldl1 add <$> chain term (char '+')
term = foldl1 mul <$> chain factor (char '*')
factor = num <$> number <|> char '(' *> expr <* char ')' 
  <|> Expr.sin <$> funcSin 
  <|> Expr.cos <$> funcCos
  <|> do char 'x' ; return x
funcSin = 
    do
    char 's'; char 'i'; char 'n' ; char '('
    n <- expr
    char ')'
    return n 
    <|> do
    char 's'; char 'i'; char 'n' ; char ' '
    n <- expr
    char ' '
    return n 

funcCos = 
    do
    char 'c'; char 'o'; char 's' ; char '('
    n <- expr
    char ')'
    return n
    <|> do
    char 'c'; char 'o'; char 's' ; char ' '
    n <- expr
    char ' '
    return n 
    
    

readExpr :: String -> Maybe Expr
readExpr s = 
    case parse expr s of
        Just(a, "") -> Just a
        _  -> Nothing





-- -- E
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = 
    showExpr e == showExpr (fromJust (readExpr $ showExpr e)) &&
    size e == size (fromJust (readExpr $ showExpr e))


arbExpr :: Int -> Gen Expr
arbExpr 0 
 = oneof 
  [Num <$> do
  rD      <- genDouble
  let rD20 = rD*20
  let strD20 = showFFloat (Just 2) rD20 ""
  let noE = read strD20 :: Double
  let n | noE < 0.10 = 0.0
        | otherwise = noE
  return n
  , return Var]  -- Base case for size 0

arbExpr n | n>0 && n<=99 
 = oneof
  [ liftA2 add (arbExpr smaller) (arbExpr smaller)  -- Gen addition
  , liftA2 mul (arbExpr smaller) (arbExpr smaller)  -- Gen multiplication
  , fmap Expr.sin (arbExpr (n-1))  -- Gen sine
  , fmap Expr.cos (arbExpr (n-1))  -- Gne cosine
  ]
  where
    smaller = n `div` 2  -- Split the size in half for recursive calls

arbExpr n | n<0 || n>99 = arbExpr (mod (abs n) 99)



instance Arbitrary Expr where
  arbitrary = sized arbExpr



------ F

simplify :: Expr -> Expr
simplify (Num n) = Num n
simplify Var = x



simplify (Operator '+' a0 a1) | simplify a0== Num 0 = simplify a1
simplify (Operator '+' a0 a1) | simplify a1== Num 0 = simplify a0

simplify (Operator '*' a0 a1) 
  | simplify a1== Num 0 || simplify a0== Num 0  = Num 0

simplify (Operator '*' a0 a1) | simplify a1== Num 1 = simplify a0
simplify (Operator '*' a0 a1) | simplify a0== Num 1 = simplify a1

simplify (Function "sin" a) | simplify a== Num 0 = Num 0
simplify (Function "cos" a) | simplify a== Num 0 = Num 1



simplify (Operator n e1 e2) 
 = Operator n (simplify e1)  (simplify e2) 

simplify (Function n m) = Function n (simplify m)




prop_Simplify :: Expr -> Double -> Bool
prop_Simplify a b = 
  eval a b == eval (simplify a) b




-- G
differentiate' :: Expr -> Expr
differentiate' (Num _) = Num 0
differentiate' Var = Num 1  
differentiate' (Operator '+' e1 e2) -- add
  = add (differentiate' e1) (differentiate' e2)  
differentiate' (Operator '*' e1 e2) -- mul
  = add (mul e1 (differentiate' e2)) (mul (differentiate' e1) e2)  
  
differentiate' (Function "sin" e) -- sin -> cos
  = mul (Expr.cos e) (differentiate' e)  
  
differentiate' (Function "cos" e) -- cos -> sin
  = mul (num (-1)) (mul (Expr.sin e) (differentiate' e))  

differentiate' e = simplify e


differentiate :: Expr -> Expr
differentiate s = simplify $ differentiate' s


