# Algorithm W

This repo implements Algorithm W used in Hindley–Milner type system and is part of BSL (a.k.a Boleyn Su's Language) project.


# Try It

You can use following command to play with the example.

```bash
git clone https://github.com/BoleynSu/algorithm-w.git
cd algorithm-w
./bin/bsl ./example/sort.bsl
```

It will sort all inputed numbers. Enter CTRL+D to end your input.

You can replace `sort.bsl` with other examples in `example` folder.


# Change Log

GADT is supported now but I am not 100% sure if it's bug free.
```haskell
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

data Expr a {
  I:Int->Expr Int;
  B:Bool->Expr Bool;
  Add:Expr Int->Expr Int->Expr Int;
  Mul:Expr Int->Expr Int->Expr Int;
  Eq:Expr Int->Expr Int->Expr Bool;
  If:forall a.Expr Bool->Expr a->Expr a->Expr a
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
```

Error messages are more user friendly now.

C is the target language now.

GC is added. Set env BSL_RT_WITH_GC to enable GC.

Existential types is supported now.

```haskell
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
```

Constructors can be of rank-2 type now.

```haskell
#!/usr/bin/env bsl

data Unit {
  Unit:Unit
}
data Pair a b{
  Pair:forall a.forall b.a->b->Pair a b
}

data R1 {
  R1:(forall a.a->a)->R1
}

data R2 {
  R2:(forall a.R1->a->a)->R2
}

data R3 {
  R3:(forall a.R2->a->a)->R3
}

--f1::forall a.a->a=\x->X
let f1 = R1 \x->x in
--f2::forall a.(forall b.b->b)->a->a=\f x-> f x
let f2 = R2 \f1->\x-> case f1 of {
R1 f1 -> f1 x
} in
--f3::forall a.(forall b.(forall c.c->c)->b->b)->a->a=\f x = f (\x->x) x
let f3 = R3 \f2->\x-> case f2 of {
R2 f2 -> f2 (R1 \x->x) x
} in

let _ = case f3 of { R3 f3 -> f3 f2 Unit } in
--_::Unit = f3 f2 Unit

Unit
```

