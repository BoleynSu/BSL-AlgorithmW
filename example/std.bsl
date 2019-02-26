#!/usr/bin/env bsl

data Unit {
  Unit:Unit
}

data Bool {
  False:Bool;
  True:Bool
}

data Int {}

data Long {}

data Float {}

data Double {}

data Char {}

data String {}

data Maybe a {
  Just:forall a.a->Maybe a;
  Nothing:forall a.Maybe a
}

data List a {
  Nil:forall a.List a;
  Cons:forall a.a->List a->List a
}

data Either a b {
  Left:forall a.forall b.a->Either a b;
  Right:forall a.forall b.b->Either a b
}

data Pair a b {
  Pair:forall a.forall b.a->b->Pair a b;
}

data RealWorld {}

data File {}

data IO a {
  MkIO:forall a.(RealWorld->Pair RealWorld a)->IO a
}

data Monad m {
  MkMonad:forall m.(forall a.forall b.m a->(a->m b)->m b)-> -- bind
                   (forall a.a->m a)-> --return
                   Monad m
}

data Num n {
   MkNum:forall n.(n->n->n)-> -- add
                  (n->n->n)-> -- sub
                  Num n
}

data Expr {
  Var:String->Expr;
  Const:String->Expr;
  Abs:String->Expr->Expr;
  App:Expr->Expr->Expr;
  Let:String->Expr->Expr->Expr;
  Rec:List (Pair String Expr)->Expr->Expr;
  Case:Expr->List (Pair String (Pair (List String) Expr))->Expr;
  FFI:String->Expr;
}

let strcat:String->String->String = \x -> \y ->
  let z = ffi ` BSL_RT_MALLOC(strlen($x) + strlen($y) + 1) ` in
  ffi ` (strcpy($z, $x), strcat($z, $y)) `
in
rec to_string = \e -> case e of {
  Var x -> x;
  Const x -> x;
  Abs x e-> strcat ffi ` "(\\" ` (
            strcat x (
            strcat ffi ` "->" ` (
            strcat (to_string e)
                   ffi ` ")" `)));
  App e1 e2 -> strcat (to_string e1) (
               strcat ffi ` " " `
                      (to_string e2));
  Let x e1 e2 -> strcat ffi ` "(let " ` (
                 strcat x (
                 strcat ffi ` "=" ` (
                 strcat (to_string e1) (
                 strcat ffi ` " in " ` (
                 strcat (to_string e2)
                        ffi ` ")" `)))));
  Rec xes e -> ffi ` "" `;
  Case e pes -> ffi ` "" `;
  FFI x -> x;
} in
let x = ffi ` "x" ` in
let e = Let x (App (Abs x (Var x)) (Var x)) (Var x) in
let _ = let s = to_string e in ffi ` puts($s) ` in

let return = \m -> case m of {MkMonad _ r->r} in
let bind = \m -> case m of {MkMonad b _->b} in

let add = \n -> case n of {MkNum a _->a} in
let sub = \n -> case n of {MkNum _ s->s} in

let NumInt = MkNum
(let add:Int->Int->Int = \a -> \b -> ffi ` ((int) $a) + ((int) $b) ` in add)
(let sub:Int->Int->Int = \a -> \b -> ffi ` ((int) $a) - ((int) $b) ` in sub)
in

let MonadIO = MkMonad
(\ma -> \f -> MkIO \r -> case ma of {
  MkIO ma -> case ma r of {
    Pair r a -> case f a of {MkIO f->f r}
  }
})
(\a -> MkIO \r -> Pair r a)
in

let MonadMaybe = MkMonad
(\ma -> \f -> case ma of {
  Just x -> f x;
  Nothing -> Nothing;
})
Just
in

rec append = \x -> \y -> case x of {
  Nil -> y;
  Cons h t -> Cons h (append t y)
} in

rec concat = \x -> case x of {
  Nil -> Nil;
  Cons h t -> append h (concat t)
} in

rec map = \f -> \x -> case x of {
  Nil -> Nil;
  Cons h t -> Cons (f h) (map f t)
} in

let MonadList = MkMonad
(\ma -> \f -> concat (map f ma))
(\x -> Cons x Nil)
in

let MonadEither = MkMonad
(\ma -> \f -> case ma of {
  Left l -> Left l;
  Right r -> f r
})
(\x -> Right x)
in

let fopen:String->String->IO File = \path -> \mode ->
  MkIO \r -> let h:File = ffi ` fopen($path, $mode) `
             in Pair r h
in
let fgetc:File->IO Int = \handle ->
  MkIO \r -> let c:Int = ffi ` fgetc($handle) `
             in Pair r c
in

let fputc:Int->File->IO Int = \c -> \handle ->
  MkIO \r -> let u:Int = ffi ` fputc($c, $handle) `
             in Pair r u
in

let stdout:File = ffi ` stdout ` in
let filename:String = ffi ` "std.bsl" ` in
let mode:String = ffi ` "r" ` in

rec echo = \m -> \i -> \o ->
  bind m (fgetc i) \c -> case ffi ` ($c==EOF?$True:$False)` of {
    False -> bind m (fputc c o) \_ -> echo m i o;
    True -> return m Unit
  }
in
let echo = echo in

let main = \m -> return m Unit in--bind m (fopen filename mode) \h -> echo m h stdout in

case main MonadIO of {MkIO m->m ffi ` NULL `}
