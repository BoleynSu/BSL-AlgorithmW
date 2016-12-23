#!/usr/bin/env bsl

data Unit {
  Unit:Unit
}

data Bool {
  False:Bool;
  True:Bool
}

data Int {}

data Long {}

data Float {}

data Double {}

data Char {}

data Maybe a {
  Just:forall a.a->Maybe a;
  Nothing:forall a.Maybe a
}

data List a {
  Nil:forall a.List a;
  Cons:forall a.a->List a->List a
}

data Either a b {
  Left:forall a.forall b.a->Either a b;
  Right:forall a.forall b.b->Either a b
}

data Pair a b {
  Pair:forall a.forall b.a->b->Pair a b;
}

data Realworld {
  Realworld:Realworld
}

data IO a {
  MkIO:forall a.(Realworld->Pair Realworld a)->IO a
}

data Monad m {
  MkMonad:forall m.(forall a.forall b.m a->(a->m b)->m b)-> -- bind
                   (forall a.a->m a)-> --return
                   Monad m
}

data Num n {
   MkNum:forall n.(n->n->n)-> -- add
                  (n->n->n)-> -- sub
                  Num n
}

let return = \m -> case m of {MkMonad _ r->r} in
let bind = \m -> case m of {MkMonad b _->b} in

let add = \n -> case n of {MkNum a _->a} in
let sub = \n -> case n of {MkNum _ s->s} in

let NumInt = MkNum
(let add:Int->Int->Int = \a -> \b -> ffi ` ((int) $a) + ((int) $b) ` in add)
(let sub:Int->Int->Int = \a -> \b -> ffi ` ((int) $a) - ((int) $b) ` in sub)
in

let MonadIO = MkMonad
(\ma -> \f -> MkIO \r -> case ma of {
  MkIO ma -> case ma r of {
    Pair r a -> case f a of {MkIO f->f r}
  }
})
(\a -> MkIO \r -> Pair r a)
in

let MonadMaybe = MkMonad
(\ma -> \f -> case ma of {
  Just x -> f x;
  Nothing -> Nothing;
})
in

let MonadMaybe = MkMonad
(\ma -> \f -> case ma of {
  Just x -> f x;
  Nothing -> Nothing;
})
Just
in

ffi ` NULL `
