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

rec rev_append = \l1 -> \l2 -> case l1 of {
  Nil -> l2;
  Cons h t -> rev_append t (Cons h l2)
} in

let length:forall a.List a->Int =
  rec length = \n -> \l -> case l of {
    Nil -> n;
    Cons x xs -> length ffi ` 1+$n ` xs
  } in length ffi ` 0 `
in

rec drop:forall a.Int->List a->List a = \n -> \l -> case ffi ` ((int)$n)==0?$True:$False` of {
  True -> l;
  False -> case l of {
    Nil -> Nil;
    Cons x xs -> drop ffi ` ((int)$n)-1 ` xs;
  }
} in

let sort = \less -> \list ->
  rec rev_merge = \l1 -> \l2 -> \acc -> case l1 of {
    Nil -> rev_append l2 acc;
    Cons h1 t1 -> case l2 of {
      Nil -> rev_append l1 acc;
      Cons h2 t2 -> case less h1 h2 of {
        True -> rev_merge t1 l2 (Cons h1 acc);
        False -> rev_merge l1 t2 (Cons h2 acc)
      }
    }
  } in
  rec rev_merge_rev = \l1 -> \l2 -> \acc -> case l1 of {
    Nil -> rev_append l2 acc;
    Cons h1 t1 -> case l2 of {
      Nil -> rev_append l1 acc;
      Cons h2 t2 -> case less h1 h2 of {
        False -> rev_merge_rev t1 l2 (Cons h1 acc);
        True -> rev_merge_rev l1 t2 (Cons h2 acc)
      }
    }
  } in
  rec sort = \n -> \l ->
    case ffi ` ((int)$n)<2?$True:$False ` of {
      True -> case l of {Nil->Nil;Cons h t->Cons h Nil};
      False ->
        let n1 = ffi ` ((int)$n)>>1 ` in
        let n2 = ffi ` ((int)$n) - ((int)$n1) ` in
        let l2 = drop n1 l in
        let s1 = rev_sort n1 l in
        let s2 = rev_sort n2 l2 in
        rev_merge_rev s1 s2 Nil
    }
  and rev_sort = \n -> \l ->
    case ffi ` ((int)$n)<2?$True:$False ` of {
      True -> case l of {Nil->Nil;Cons h t->Cons h Nil};
      False ->
        let n1 = ffi ` ((int)$n)>>1 ` in
        let n2 = ffi ` ((int)$n) - ((int)$n1) ` in
        let l2 = drop n1 l in
        let s1 = sort n1 l in
        let s2 = sort n2 l2 in
        rev_merge s1 s2 Nil
    }
  in
  let l = length list in
  case ffi ` ((int)$l)<2?$True:$False ` of {
    True -> list;
    False -> sort l list
  }
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

let less:Int->Int->Bool = \a -> \b -> ffi ` ((((int) $a) < ((int) $b)) ? $True : $False) ` in

let main = bind getList \list ->
                putList (sort less list)
in runIO main
