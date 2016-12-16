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

let not = \x -> case x of {
  True -> False;
  False -> True
} in

let less:Int->Int->Bool = \a -> \b -> ffi ` ((((int) $a) < ((int) $b)) ? $True : $False) ` in

let sort = \less ->
  rec concat = \a -> \b -> case a of {
    Nil -> b;
    Cons x xs -> concat xs (Cons x b)
  } in
  let filter = \f ->
    rec filter = \acc -> \list -> case list of {
      Nil -> acc;
      Cons x xs -> case f x of {
        True -> filter (Cons x acc) xs;
        False -> filter acc xs
      }
    } in filter Nil
  in
  let notLess = \x -> \y -> not (less x y) in
  rec sort = \less -> \notLess -> \list -> case list of {
    Nil -> Nil;
    Cons x xs -> concat (sort notLess less (filter (notLess x) xs))
                 (Cons x (sort less  notLess (filter (less x) xs)))
  } in sort less notLess
in

let getList =
  rec getList = \xs -> bind getInt \x -> case x of {
    Just x -> getList (Cons x xs);
    Nothing -> return xs
  } in getList Nil
in
rec putList = \list -> case list of {
  Nil -> return Unit;
  Cons x xs -> bind (putInt x) \_ ->
               putList xs
} in

let main = bind getList \list ->
                putList (sort less list)
in runIO main
