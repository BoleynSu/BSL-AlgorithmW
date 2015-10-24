data Int where {}

data Bool where {
  False::Bool;
  True::Bool
}

data Pair a b where {
  Pair::forall a.forall b.a->b->Pair a b
}

data Maybe a where {
  Just::forall a.a->Maybe a;
  Nothing::forall a.Maybe a
}

data IO a where {
  Data::forall a.a->IO a;
  Read::forall a.(Int->IO a)->IO a;
  Write::forall a.Int->IO a->IO a
}

rec bind :: forall a.forall b. IO a->(a->IO b)->IO b = \x -> \f ->
  case x of {
    Data x -> f x;
    Read g -> Read (\x -> bind (g x) f);
    Write c x -> Write c (bind x f)
  }
in

let return = Data
in

let getInt = Read (\x -> Data x)
in

let putInt :: Int->IO Int= \x -> Write x (Data ffi ` new int(0) `)
in

rec runIO :: IO a->Int = \x ->
  case x of {
    Data x -> ffi ` new int(0) `;
    Read g -> let x = ffi ` [=]() -> void* { int *x = new int; if (scanf("%d", x) == 1) return (*((function<void*(void*)>*)$v_bsl_Just))(x); else return $v_bsl_Nothing; }() `
              in case x of {
                Just x -> runIO (g x);
                Nothing -> ffi ` new int(-1) `
              };
    Write c x -> let _ = ffi ` (printf("%d\n", *((int*)$v_bsl_c)), (void*)0) ` in (runIO x)
  }
in

let add :: Int->Int->Int = \a -> \b -> ffi ` new int((*(int*)$v_bsl_a) + (*(int*)$v_bsl_b)) `
in

let neg :: Int->Int = \a -> ffi ` new int(-(*(int*)$v_bsl_a)) `
in

let sub :: Int->Int->Int = \a -> \b -> ffi ` new int((*(int*)$v_bsl_a) - (*(int*)$v_bsl_b)) `
in

let mul :: Int->Int->Int = \a -> \b -> ffi ` new int((*(int*)$v_bsl_a) * (*(int*)$v_bsl_b)) `
in

let div :: Int->Int->Pair Int Int = \a -> \b ->
let x :: Int = ffi ` new int((*(int*)$v_bsl_a) / (*(int*)$v_bsl_b)) ` in
let y :: Int = ffi ` new int((*(int*)$v_bsl_a) % (*(int*)$v_bsl_b)) ` in
(Pair x y)
in

let eq0 :: Int->Bool = \a -> ffi ` new $t_bsl_Bool{ (*(int*)$v_bsl_a) == 0 } `
in

rec gcd = \a -> \b ->
  case eq0 a of {
    True -> b;
    False -> case div b a of {
      Pair c d -> gcd d a
    }
  }
in

rec echo = bind
getInt \x -> bind
(putInt x) \_ ->
echo
in

runIO (bind
getInt \x -> bind
(putInt x) \_ -> bind
getInt \y -> bind
(putInt y) \_ -> bind
(putInt (gcd x y)) \_ ->
echo)
