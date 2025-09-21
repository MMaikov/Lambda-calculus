module Lambda

import Data.List

data LamConst = 
    CTrue |
    CFalse |
    CCond |
    CNat Nat |
    CAdd

data Lam =
    Var String |
    Abs String Lam |
    App Lam Lam |
    Const LamConst

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
t7 = App (App (Const CAdd) (Const (CNat 2))) (Const (CNat 2))

t8: Lam
t8 = App (App (Const CAdd) (App (App (Const CAdd) (Const (CNat 2))) (Const (CNat 2))) ) (Const (CNat 2))

t9: Lam
t9 = App (App (Const CAdd) (Const (CNat 2))) (Var "x")

t10: Lam
t10 = App (Abs "x" (App (App (Const CAdd) (Const (CNat 2))) (Var "x"))) (Const (CNat 2))

Show LamConst where
    show CTrue = "true"
    show CFalse = "false"
    show CCond = "cond"
    show (CNat n) = show n
    show (CAdd) = "add"

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
                    (Const (CNat n)) => show n
                    _ => "(" ++ show t1 ++ ")" in
        let t2s = case t2 of
                    (Const (CNat n)) => show n
                    _ => "(" ++ show t2 ++ ")" in
        show (Const CAdd) ++ " " ++ t1s ++ " " ++ t2s
    
    show (App (Const c) y) = show c ++ " " ++ show y
    show (App (Var x) y) = show x ++ " " ++ show y
    show (App x y) = "(" ++ show x ++ ") " ++ show y
    show (Const c) = show c

fv: Lam -> List String
fv (Var str) = [str]
fv (Abs str x) = delete str (fv x)
fv (App x y) = (fv x) ++ (fv y)
fv (Const c) = []

exists: Eq a => a -> List a -> Bool
exists el [] = False
exists el (x :: xs) = if el == x then True else exists el xs

new_var: String -> Lam -> String
new_var y u = if exists y (fv u) then new_var (y ++ "'") u else y

sub: (String, Lam) -> Lam -> Lam
sub (x, u) (Var str) = if str == x then u else Var str
sub (x, u) (Abs y t) = if x == y then (Abs y t) else
    if exists y (fv u) then let z = new_var y u in Abs z (sub (x, u) (sub (y, Var z) t)) 
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
        (Const (CNat x)) => 
            case (reduce t2) of
                (Const (CNat y)) => Const (CNat (x+y))
                lam => (App (App (Const CAdd) (Const (CNat x))) lam)
        lam => (App (App (Const CAdd) lam) t2)
reduce (App (Abs x t) u) = reduce (sub (x, u) t)
reduce (App x y) = App x y
reduce (Const c) = Const c

