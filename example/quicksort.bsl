data Int where ffi ` int `

data Bool where {
  False::Bool;
  True::Bool
}

data Maybe a where {
  Just::forall a.a->Maybe a;
  Nothing::forall a.Maybe a
}

data List a where {
  Nil::forall a.List a;
  Cons::forall a.a->List a->List a
}

data IO a where {
  Return::forall a.a->IO a;
  Bind::forall a.forall b.IO a->(a->IO b)->IO b;
  Read::forall a.(Maybe Int->IO a)->IO a;
  Write::forall a.Int->IO a->IO a
}

let return = Return in
let bind = Bind in
let getInt = Read (\x -> Return x) in
let putInt::Int->IO Int = \x -> Write x (Return ffi ` new int(0) `) in
rec runIO::forall a.IO a->a = \x -> case x of {
  Return x -> x;
  Bind x f -> case x of {
    Return x -> runIO (f x);
    Bind y g -> runIO (f (runIO (Bind y g)));
    Read g -> runIO (Read (\x -> Bind (g x) f));
    Write c x -> runIO (Write c (Bind x f))
  };
  Read g -> let x::Maybe Int = ffi ` [=]() -> void* { int *x = new int; if (scanf("%d", x) == 1) return (*((function<void*(void*)>*)$v_bsl_Just))(x); else return $v_bsl_Nothing; }() `
            in runIO (g x);
  Write c x -> let _ = ffi ` (printf("%d\n", *((int*)$v_bsl_c)), (void*)0) ` in (runIO x)
} in

let add::Int->Int->Int = \a -> \b -> ffi ` new int((*(int*)$v_bsl_a) + (*(int*)$v_bsl_b)) ` in
let neg::Int->Int = \a -> ffi ` new int(-(*(int*)$v_bsl_a)) ` in
let sub::Int->Int->Int = \a -> \b -> ffi ` new int((*(int*)$v_bsl_a) - (*(int*)$v_bsl_b)) ` in
let mul::Int->Int->Int = \a -> \b -> ffi ` new int((*(int*)$v_bsl_a) * (*(int*)$v_bsl_b)) ` in
let div::Int->Int->Int = \a -> \b -> ffi ` new int((*(int*)$v_bsl_a) / (*(int*)$v_bsl_b)) ` in
let mod::Int->Int->Int = \a -> \b -> ffi ` new int((*(int*)$v_bsl_a) / (*(int*)$v_bsl_b)) ` in
let eq0::Int->Bool = \a -> ffi ` new $t_bsl_Bool{ (*(int*)$v_bsl_a) == 0 } ` in
let less::Int->Int->Bool = \a -> \b -> ffi ` new $t_bsl_Bool{ (*(int*)$v_bsl_a) < (*(int*)$v_bsl_b) } ` in
rec gcd = \a -> \b -> case eq0 a of {
  True -> b;
  False -> gcd (mod b a) a
} in

let not = \x -> case x of {
  True -> False;
  False -> True;
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

let getList = bind getInt \x -> case x of { 
  Just x -> rec f = \x -> case let zero::Int = ffi ` new int(0)` in not (less zero x) of {
    True -> return Nil;
    False -> bind getInt \y -> case y of {
      Just y -> bind (f (sub x ffi ` new int(1) `)) \ys ->
                return (Cons y ys);
      Nothing -> return Nil
    }
  } in f x;
  Nothing -> return Nil
} in
rec putList = \list -> case list of {
  Nil -> return Nothing;
  Cons x xs -> bind (putInt x) \_ ->
               putList xs
} in

runIO (bind getList \list ->
putList (sort less list))
