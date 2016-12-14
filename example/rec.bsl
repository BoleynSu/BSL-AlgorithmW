#!/usr/bin/env bsl

data Int where ffi ` void `

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
rec take = \x -> \l -> case eq zero  x of {
  True -> Nil;
  False -> Cons (case l of { Cons h _ -> h; Nil -> undefined })
                (take (sub x one) (case l of { Cons _ t -> t; Nil -> undefined }))
} in
rec a = Cons one b and b = Cons two a in
let main = bind (return (take ten a)) putList in
runIO main

