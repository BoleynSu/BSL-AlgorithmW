#!/usr/bin/env bsl

data Int {}

data Unit {
  Unit:Unit
}

data Bool {
  False:Bool;
  True:Bool
}

data Maybe a {
  Just:forall a.a->Maybe a;
  Nothing:forall a.Maybe a
}

data List a {
  Nil:forall a.List a;
  Cons:forall a.a->List a->List a
}

data Lazy a {
  Val:forall a.a->Lazy a;
  Fn:forall a.(Unit->a)->Lazy a;
}

data LazyList a {
  LNil:forall a.LazyList a;
  LCons:forall a.Lazy a->Lazy (LazyList a)->LazyList a
}

data IOImpl a {
  Read:forall a.(Unit->Maybe Int->a)->IOImpl a;
  Write:forall a.Int->(Unit->a)->IOImpl a
}

data IO a {
  Pure:forall a.a->IO a;
  Free:forall a.IOImpl (IO a)->IO a
}

let fmap = \f -> \x -> case x of {
  Write s k -> Write s (\_ -> f (k Unit));
  Read k -> Read (\_ -> \s -> f (k Unit s))
} in

let return = Pure in
rec bind = \x -> \f -> case x of {
  Pure x -> f x;
  Free x -> Free (fmap (\y -> bind y f) x)
} in

let getInt = Free (Read (\_ -> \x -> return x)) in
let putInt = \x -> Free (Write x (\_ -> return Unit)) in

rec runIO = \x -> case x of {
  Pure x -> x;
  Free x -> case x of {
    Write c x -> let _ = ffi ` (printf("%d\n", (int) $c), NULL) ` in runIO (x Unit);
    Read g -> let x:Unit->Maybe Int = \x -> ffi ` (scanf("%d",&$x) == 1 ? BSL_RT_CALL($Just, $x) : $Nothing) ` in runIO (g Unit (x Unit))
  }
} in

let force = \x -> case x of {
  Val v -> v;
  Fn f -> let v = ffi ` BSL_RT_CALL($f, $Unit) ` in
          let _ = ffi ` BSL_CON_Val($x, $v) ` in v
} in

let sub:Int->Int->Int = \a -> \b -> ffi ` ((int) $a) - ((int) $b) ` in
let eq:Int->Int->Bool = \a -> \b -> ffi ` ((((int) $a) == ((int) $b))?$True:$False) ` in

rec putList = \list -> case list of {
  Nil -> return Unit;
  Cons x xs -> bind (putInt x) \_ ->
               putList xs
} in

let zero:Int = ffi ` 0 ` in
let one:Int = ffi ` 1 ` in
let two:Int = ffi ` 2 ` in
let ten:Int = ffi ` 10 ` in
let undefined:forall a.Unit->a = \_ -> ffi  ` (puts("undefined"), exit(-1), NULL) ` in
rec take = \x -> \l -> case eq zero x of {
  True -> Nil;
  False -> Cons (case force l of { LCons h _ -> force h; LNil -> undefined Unit })
                (take (sub x one) (case force l of { LCons _ t -> t; LNil -> undefined Unit }))
} in

rec a = \_ ->
LCons (Val one) (Fn b)
and b = \_ ->
LCons (Val two) (Fn a)
in
let main = bind (return (take ten (Fn a))) putList in
runIO main
