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

data Expr a where {
  I   : Int  -> Expr Int;
  B   : Bool -> Expr Bool;
  Add : Expr Int -> Expr Int -> Expr Int;
  Mul : Expr Int -> Expr Int -> Expr Int;
  Eq  : Expr Int -> Expr Int -> Expr Bool;
  If  : forall a. Expr Bool -> Expr a -> Expr a -> Expr a
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
rec runIO:forall a.IO a->a = \x -> case x of {
  Pure x -> x;
  Free x -> case x of {
    Write c x -> let _ = ffi ` (std::printf("%d\n", (std::intptr_t) $v_bsl_c), nullptr) ` in (runIO x);
    Read g -> let x:Maybe Int = ffi ` [=]() -> void* {
        int x;
        if (std::scanf("%d", &x) == 1)
          return (*((std::function<void*(void*)>*)$v_bsl_Just))((void*) (std::intptr_t) x);
        else
          return $v_bsl_Nothing;
      }() `
              in runIO (g x)
  }
} in

let not = \x -> case x of {
  True -> False;
  False -> True
} in

let land = \x -> case x of {
  True -> \x -> x;
  False -> \x -> False
} in

let add:Int->Int->Int = \a -> \b -> ffi ` (void*) (((std::intptr_t) $v_bsl_a) + ((std::intptr_t) $v_bsl_b)) ` in
let neg:Int->Int = \a -> ffi ` (void*) -((std::intptr_t) $v_bsl_a) ` in
let sub:Int->Int->Int = \a -> \b -> ffi ` (void*) (((std::intptr_t) $v_bsl_a) - ((std::intptr_t) $v_bsl_b)) ` in
let mul:Int->Int->Int = \a -> \b -> ffi ` (void*) (((std::intptr_t) $v_bsl_a) * ((std::intptr_t) $v_bsl_b)) ` in
let div:Int->Int->Int = \a -> \b -> ffi ` (void*) (((std::intptr_t) $v_bsl_a) / ((std::intptr_t) $v_bsl_b)) ` in
let mod:Int->Int->Int = \a -> \b -> ffi ` (void*) (((std::intptr_t) $v_bsl_a) % ((std::intptr_t) $v_bsl_b)) ` in
let less:Int->Int->Bool = \a -> \b -> ffi ` (((std::intptr_t) $v_bsl_a) < ((std::intptr_t) $v_bsl_b))?$v_bsl_True:$v_bsl_False ` in
let eq0 = \a -> ffi ` ((std::intptr_t) $v_bsl_a) == 0?$v_bsl_True:$v_bsl_False ` in
rec gcd = \a -> \b -> case eq0 a of {
  True -> b;
  False -> gcd (mod b a) a
} in

rec concat = \a -> \b -> case a of {
  Nil -> b;
  Cons x xs -> Cons x (concat xs b)
} in
rec filter = \list -> \f -> case list of {
  Nil -> Nil;
  Cons x xs -> case f x of {
    True -> Cons x (filter xs f);
    False -> filter xs f
  }
} in
let sort = \less ->
  rec sortLess = \list -> case list of {
    Nil -> Nil;
    Cons x xs -> concat (sortLess (filter xs (\y -> not (less x y))))
                 (Cons x (sortLess (filter xs (less x) )))
  } in sortLess
in

rec getList = bind getInt \x -> case x of {
  Just x -> bind getList \xs ->
            return (Cons x xs);
  Nothing -> return Nil
} in
rec putList = \list -> case list of {
  Nil -> return Unit;
  Cons x xs -> bind (putInt x) \_ ->
               putList xs
} in

rec eval : forall a. Expr a -> a = \x -> case x of : forall a. Expr a -> a {
  I n -> n;
  B b -> b;
  Add  e1 e2 -> add (eval e1) (eval e2);
  Mul  e1 e2 -> mul (eval e1) (eval e2);
  Eq   e1 e2 -> eq0 (sub (eval e1) (eval e2));
  If c e1 e2 -> case eval c of {
    True -> eval e1;
    False -> eval e2;
  }
} in

let ignore = \a->\b->\c->c in
rec f :forall a.a->a = \x->ignore (f True) (f Nothing) x in

let _ =
runIO (bind getList \list ->
putList (sort less list))
in
let one:Int = ffi ` (void*)1 ` in
let two:Int = ffi ` (void*)2 ` in
let one = I one in
let two = I two in
eval (If (Eq (Add one one) one) one two)
