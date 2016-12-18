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

data IOImpl a {
  Read:forall a.(Unit->Maybe Int->a)->IOImpl a;
  Write:forall a.Int->(Unit->a)->IOImpl a
}

data IO a {
  Pure:forall a.a->IO a;
  Free:forall a.IOImpl (IO a)->IO a
}

data Buffer {
  Buffer:forall b.
    b-> --buffer
    (Int->b->b)-> --write
    (b->IO Unit)-> --toIO
    Buffer
}

let write = \b -> \x -> case b of {
  Buffer b w t -> Buffer (w x b) w t
} in

let toIO = \b -> case b of {
  Buffer b w t -> t b
} in


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

rec putList = \list -> case list of {
  Nil -> return Unit;
  Cons x xs -> bind (putInt x) \_ ->
               putList xs
} in

let one:Int = ffi ` 1 ` in
let two:Int = ffi ` 2 ` in
let b = Buffer Nil Cons putList in
let b = write b one in
let b = write b two in
let b = write b one in
let b = write b one in
let b1 = b in
let b = Buffer one (\x->\y->x) putInt in
let b = write b one in
let b = write b two in
let b = write b one in
let b = write b one in
let b2 = b in
let bl = Cons b1 (Cons b2 Nil) in
rec loop = \bl -> case bl of {
  Nil -> return Unit;
  Cons b bl -> bind (toIO b) \_ ->
                    loop bl
} in
let main = loop bl in
runIO main

