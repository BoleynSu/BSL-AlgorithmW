#!/usr/bin/env bsl

data Int where {}

data Unit where {
  Unit:Unit
}

data Bool where {
  False:Bool;
  True:Bool
}

data Maybe a where {
  Just:forall a.a->Maybe a;
  Nothing:forall a.Maybe a
}

data List a where {
  Nil:forall a.List a;
  Cons:forall a.a->List a->List a
}

data IOImpl a where {
  Read:forall a.(Maybe Int->a)->IOImpl a;
  Write:forall a.Int->a->IOImpl a
}

data IO a where {
  Pure:forall a.a->IO a;
  Free:forall a.IOImpl (IO a)->IO a
}

data Expr a where {
  I:Int->Expr Int;
  B:Bool->Expr Bool;
  Add:Expr Int->Expr Int->Expr Int;
  Mul:Expr Int->Expr Int->Expr Int;
  Eq:Expr Int->Expr Int->Expr Bool;
  If:forall a.Expr Bool->Expr a->Expr a->Expr a
}

let fmap = \f -> \x -> case x of {
  Write s k -> Write s (f k);
  Read k -> Read (\s -> f (k s))
} in

let return = Pure in
rec bind = \x -> \f -> case x of {
  Pure x -> f x;
  Free x -> Free (fmap (\y -> bind y f) x)
} in

let getInt = Free (Read (\x -> return x)) in
let putInt = \x -> Free (Write x (return Unit)) in

rec runIO = \x -> case x of {
  Pure x -> x;
  Free x -> case x of {
    Write c x -> let _ = ffi ` (printf("%d\n", (int) $c), NULL) ` in (runIO x);
    Read g -> let x:Unit->Maybe Int = \x -> ffi ` (scanf("%d",&$x) == 1 ? BSL_RT_CALL($Just, $x) : $Nothing) ` in runIO (g (x Unit))
  }
} in

let add:Int->Int->Int = \a -> \b -> ffi ` ((int) $a) + ((int) $b) ` in
let mul:Int->Int->Int = \a -> \b -> ffi ` ((int) $a) * ((int) $b) ` in
let eq:Int->Int->Bool = \a -> \b -> ffi ` ((((int) $a) == ((int) $b)) ? $True : $False) ` in

rec eval:forall a.Expr a->a = \x -> case x of:forall a.Expr a->a {
  I n -> n;
  B b -> b;
  Add  e1 e2 -> add (eval e1) (eval e2);
  Mul  e1 e2 -> mul (eval e1) (eval e2);
  Eq   e1 e2 -> eq (eval e1) (eval e2);
  If c e1 e2 -> case eval c of {
    True -> eval e1;
    False -> eval e2;
  }
} in

let one:Int = ffi ` 1 ` in
let two:Int = ffi ` 2 ` in
let one = I one in
let two = I two in
let main = putInt (eval (If (Eq (Add one one) two) one two))
in runIO main

