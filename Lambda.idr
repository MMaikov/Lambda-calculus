module Lambda

import Data.SortedSet
import Data.String.Parser

data LamConst = 
    CTrue |
    CFalse |
    CCond |
    CInt Int |
    CAdd |
    CSub |
    CMul |
    CIsZero |
    CPair |
    CFst |
    CSnd |
    CNil |
    CCons |
    CNull |
    CHd |
    CTl

data Lam =
    Var String |
    Abs String Lam |
    App Lam Lam |
    Const LamConst


yCombinator: Lam
yCombinator = Abs "f" (App (Abs "x" (App (Var "f") (App (Var "x") (Var "x")))) (Abs "x" (App (Var "f") (App (Var "x") (Var "x")))))

t1: Lam
t1 = (App (Abs "x" (Var "x")) (Var "y"))

t2: Lam
t2 = App (Var "x") (Var "x'")

t3: Lam
t3 = Abs "x" (Abs "y" (Var "x"))

t4: Lam
t4 = App (Abs "x" (App (Var "x") (Var "x"))) (Abs "y" (Var "y"))

t5: Lam
t5 = App (App (App (Const CCond) (Const CTrue)) (Var "x")) (Var "y")

t6: Lam
t6 = App (App (App (Const CCond) (Const CTrue)) (App (App (App (Const CCond) (Const CFalse)) (Const CFalse)) (Const CTrue))) (Const CTrue)

t7: Lam
t7 = App (App (Const CAdd) (Const (CInt 2))) (Const (CInt 2))

t8: Lam
t8 = App (App (Const CAdd) (App (App (Const CAdd) (Const (CInt 2))) (Const (CInt 2))) ) (Const (CInt 2))

t9: Lam
t9 = App (App (Const CAdd) (Const (CInt 2))) (Var "x")

t10: Lam
t10 = App (Abs "x" (App (App (Const CAdd) (Const (CInt 2))) (Var "x"))) (Const (CInt 2))

t11: Lam
t11 = App (Const CFst) (App (App (Const CPair) (Var "x")) (Var "y"))

Eq LamConst where
    (==) CTrue CTrue = True
    (==) CFalse CFalse = True
    (==) CCond CCond = True
    (==) (CInt i) (CInt j) = i == j
    (==) CAdd CAdd = True
    (==) CSub CSub = True
    (==) CMul CMul = True
    (==) CIsZero CIsZero = True
    (==) CPair CPair = True
    (==) CFst CFst = True
    (==) CSnd CSnd = True
    (==) CNil CNil = True
    (==) CCons CCons = True
    (==) CNull CNull = True
    (==) CHd CHd = True
    (==) CTl CTl = True
    (==) _ _ = False

Eq Lam where
    (==) (Var str) (Var str1) = str == str1
    (==) (Abs str x) (Abs str1 y) = str == str1 && x == y
    (==) (App x y) (App x1 y2) = x==x1 && y==y2
    (==) (Const x) (Const y) = x==y
    (==) _ _ = False

Show LamConst where
    show CTrue = "true"
    show CFalse = "false"
    show CCond = "cond"
    show (CInt n) = if n < 0 then "(" ++ show n ++ ")" else show n
    show (CAdd) = "add"
    show (CSub) = "sub"
    show (CMul) = "mul"
    show (CIsZero) = "iszero"
    show (CPair) = "pair"
    show (CFst) = "fst"
    show (CSnd) = "snd"
    show (CNil) = "nil"
    show (CCons) = "cons"
    show (CNull) = "null"
    show (CHd) = "hd"
    show (CTl) = "tl"
    

Show Lam where
    show (Var str) = str
    show (Abs str x) = "(\\" ++ str ++ ". " ++ show x ++ ")"

    -- CCond
    show (App (App (App (Const CCond) b) t1) t2) = 
        let bs = case b of
                    (Const CTrue) => show b
                    (Const CFalse) => show b
                    _ => "(" ++ show b ++ ")" in
        let t1s = case t1 of
                    (Const CTrue) => show t1
                    (Const CFalse) => show t1
                    _ => "(" ++ show t1 ++ ")" in
        let t2s = case t2 of
                    (Const CTrue) => show t2
                    (Const CFalse) => show t2
                    _ => "(" ++ show t2 ++ ")" in
        show (Const CCond) ++ " " ++ bs ++ " " ++ t1s ++ " " ++ t2s
    
    -- CAdd
    show (App (App (Const CAdd) t1) t2) = 
        let t1s = case t1 of
                    (Const (CInt n)) => show n
                    _ => "(" ++ show t1 ++ ")" in
        let t2s = case t2 of
                    (Const (CInt n)) => show n
                    _ => "(" ++ show t2 ++ ")" in
        show (Const CAdd) ++ " " ++ t1s ++ " " ++ t2s

    -- CSub
    show (App (App (Const CSub) t1) t2) = 
        let t1s = case t1 of
                    (Const (CInt n)) => show n
                    _ => "(" ++ show t1 ++ ")" in
        let t2s = case t2 of
                    (Const (CInt n)) => show n
                    _ => "(" ++ show t2 ++ ")" in
        show (Const CSub) ++ " " ++ t1s ++ " " ++ t2s

    -- CMul
    show (App (App (Const CMul) t1) t2) = 
        let t1s = case t1 of
                    (Const (CInt n)) => show n
                    _ => "(" ++ show t1 ++ ")" in
        let t2s = case t2 of
                    (Const (CInt n)) => show n
                    _ => "(" ++ show t2 ++ ")" in
        show (Const CMul) ++ " " ++ t1s ++ " " ++ t2s

    show (App (App (Const CPair) t1) t2) = "(" ++ show (Const CPair) ++ " " ++ show t1 ++ " " ++ show t2 ++ ")"
    
    show (App (Const c) y) = show c ++ " " ++ show y
    show (App (Var x) y) = show x ++ " " ++ show y
    show (App x y) = "(" ++ show x ++ ") " ++ show y
    show (Const c) = show c

fv: Lam -> SortedSet String
fv (Var str) = fromList [str]
fv (Abs str x) = delete str (fv x)
fv (App x y) = (fv x) `union` (fv y)
fv (Const c) = empty

new_var: String -> Lam -> String
new_var y u = if contains y (fv u) then new_var (y ++ "'") u else y

sub: (String, Lam) -> Lam -> Lam
sub (x, u) (Var str) = if str == x then u else Var str
sub (x, u) (Abs y t) = if x == y then (Abs y t) else
    if contains y (fv u) then let z = new_var y u in Abs z (sub (x, u) (sub (y, Var z) t)) 
    else Abs y (sub (x, u) t)
sub (x, u) (App f y) = App (sub (x, u) f) (sub (x, u) y)
sub _ (Const c) = Const c

reduce: Lam -> Lam
reduce (Var str) = Var str
reduce (Abs str x) = Abs str x
reduce (App (App (App (Const CCond) (Const CTrue)) t1) t2) = reduce t1
reduce (App (App (App (Const CCond) (Const CFalse)) t1) t2) = reduce t2
reduce (App (App (Const CAdd) t1) t2) = 
    case (reduce t1) of
        (Const (CInt x)) => 
            case (reduce t2) of
                (Const (CInt y)) => Const (CInt (x + y))
                lam => (App (App (Const CAdd) (Const (CInt x))) lam)
        lam => (App (App (Const CAdd) lam) t2)
reduce (App (App (Const CSub) t1) t2) = 
    case (reduce t1) of
        (Const (CInt x)) => 
            case (reduce t2) of
                (Const (CInt y)) => Const (CInt (x - y))
                lam => (App (App (Const CSub) (Const (CInt x))) lam)
        lam => (App (App (Const CSub) lam) t2)

reduce (App (App (Const CMul) t1) t2) = 
    case (reduce t1) of
        (Const (CInt x)) => 
            case (reduce t2) of
                (Const (CInt y)) => Const (CInt (x * y))
                lam => (App (App (Const CMul) (Const (CInt x))) lam)
        lam => (App (App (Const CMul) lam) t2)   

reduce (App (Const CIsZero) t1) = 
    case (reduce t1) of 
        (Const (CInt n)) => if n == 0 then Const CTrue else Const CFalse
        lam => (App (Const CIsZero) lam)

reduce (App (Const CFst) (App (App (Const CPair) e1) e2)) = reduce e1
reduce (App (Const CSnd) (App (App (Const CPair) e1) e2)) = reduce e2

reduce (App (Const CNull) (Const CNil)) = Const CTrue
reduce (App (Const CNull) (App (App (Const CCons) e1) e2)) = Const CFalse


reduce (App (Const CHd) (App (App (Const CCons) e1) e2)) = reduce e1
reduce (App (Const CTl) (App (App (Const CCons) e1) e2)) = reduce e2
reduce (App (Const CHd) (Const CNil)) = Const CNil
reduce (App (Const CTl) (Const CNil)) = Const CNil

reduce (App (Abs x t) u) = (sub (x, u) t)
reduce (App x y) =
  let x' = reduce x in
  let y' = reduce y in
  case x' of
    Abs v body => reduce (sub (v, y') body)
    _ => App x' y'
reduce (Const c) = Const c

normalize: Lam -> Lam
normalize t = 
    let t' = reduce t in
        if t == t' then t else normalize t'

expr: Parser Lam
expr = do
    es <- some (lexeme atom)
    case es of
        [] => fail "expected expression"
        (x::xs) => pure (foldl App x xs)
    where
    mutual
        parensP : Lazy (Parser a) -> Parser a
        parensP p = do
            _ <- lexeme (char '(')
            x <- p
            _ <- lexeme (char ')')
            pure x

        varName: Parser String
        varName = do
            first <- satisfy isAlpha
            rest <- many (satisfy isAlphaNum)
            pure (pack (first::rest))

        var: Parser Lam
        var = do
            name <- varName
            pure (Var name)

        abs: Parser Lam
        abs = do
            _ <- lexeme (char '\\' <|> char 'λ')
            params <- some (lexeme varName)
            _ <- lexeme (string "." <|> string "=>" <|> string "->")
            t <- expr
            pure (foldr (\p => \acc => Abs (p) acc) t params)

        constPair2: Parser Lam
        constPair2 = do
            _ <- char '('
            x <- lexeme atom
            _ <- lexeme (char ',')
            y <- lexeme atom
            _ <- char ')'
            pure (App (App (Const CPair) x) y)

        atom: Parser Lam
        atom = const <|> abs <|> var <|> constPair2 <|> parensP expr

        app: Parser Lam
        app = do
            x <- expr
            _ <- char ' '
            y <- expr
            pure (App x y)

        const: Parser Lam
        const = constNum <|> constTrue <|> constFalse <|> constCond 
                <|> constAdd <|> constSub <|> constMul <|> constIsZero
                <|> constPair <|> constFst <|> constSnd <|> constNil
                <|> constCons <|> constNull <|> constHd <|> constTl
                <|> constYCombinator
        
        constNum: Parser Lam
        constNum = Const . CInt . cast <$> integer

        constTrue: Parser Lam
        constTrue = Const CTrue <$ string "true"

        constFalse: Parser Lam
        constFalse = Const CFalse <$ string "false"

        constCond: Parser Lam
        constCond = Const CCond <$ string "cond"

        constAdd: Parser Lam
        constAdd = Const CAdd <$ string "add"

        constSub: Parser Lam
        constSub = Const CSub <$ string "sub"

        constMul: Parser Lam
        constMul = Const CMul <$ string "mul"

        constIsZero: Parser Lam
        constIsZero = Const CIsZero <$ string "iszero"

        constPair: Parser Lam
        constPair = Const CPair <$ string "pair"

        constFst: Parser Lam
        constFst = Const CFst <$ string "fst"

        constSnd: Parser Lam
        constSnd = Const CSnd <$ string "snd"

        constNil: Parser Lam
        constNil = Const CNil <$ string "nil"

        constCons: Parser Lam
        constCons = Const CCons <$ string "cons"

        constNull: Parser Lam
        constNull = Const CNull <$ string "null"

        constHd: Parser Lam
        constHd = Const CHd <$ string "hd"

        constTl: Parser Lam
        constTl = Const CTl <$ string "tl"

        constYCombinator: Parser Lam
        constYCombinator = yCombinator <$ string "Y"

parseExpr: String -> Maybe Lam
parseExpr s = 
    case parse expr s of
        Right (t, _) => Just t
        Left (_) => Nothing


parseAndReduce: String -> Lam
parseAndReduce s = 
    case parseExpr s of
        Nothing => Const CNil
        Just lam => normalize lam

lamDefinition: Parser (String, Lam)
lamDefinition = do
    nameList <- lexeme (some (satisfy isAlpha))
    let name = pack nameList
    _ <- lexeme (char '=')
    lam <- expr
    case (contains name (fv lam)) of
        True => pure (name, (App yCombinator (Abs name lam)))
        False => pure ((name, lam))

parseLamDefinition: String -> Maybe (String, Lam)
parseLamDefinition s = 
    case parse lamDefinition s of
        Right (t, _) => Just t
        Left (_) => Nothing

factStr: String
factStr = "fact = (\\n. cond (iszero n) (1) (mul (n) (fact (sub n 1))))"

partial
factorialDef: Lam
factorialDef = case (parseLamDefinition factStr) of
    Just lam => snd lam
    Nothing => idris_crash "Parsing failed somehow for the factorial"
