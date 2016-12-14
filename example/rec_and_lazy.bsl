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

data Lazy a where {
  Val:forall a.a->Lazy a;
  Fn:forall a.(Unit->a)->Lazy a;
}

data LazyList a where {
  LNil:forall a.LazyList a;
  LCons:forall a.Lazy a->Lazy (LazyList a)->LazyList a
}

data IOImpl a where {
  Read:forall a.(Maybe Int->a)->IOImpl a;
  Write:forall a.Int->a->IOImpl a
}

data IO a where {
  Pure:forall a.a->IO a;
  Free:forall a.IOImpl (IO a)->IO a
}

let force = \x -> case x of {
  Val v -> v;
  Fn f -> let v = ffi ` (((BSL_TYPE_Lazy*)$x)->T=BSL_TAG_Val, ((BSL_TYPE_Lazy*)$x)->arg0 = BSL_RT_CALL($f,$Unit)) ` in v
} in

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
let undefined = ffi  ` NULL ` in
rec take = \x -> \l -> case eq zero x of {
  True -> Nil;
  False -> Cons (case force l of { LCons h _ -> force h; LNil -> undefined })
                (take (sub x one) (case force l of { LCons _ t -> t; LNil -> undefined }))
} in

rec a = \_ ->
LCons (Val one) (Fn b)
and b = \_ ->
LCons (Val two) (Fn a)
in
let main = bind (return (take ten (Fn a))) putList in
runIO main
