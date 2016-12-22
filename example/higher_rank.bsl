#!/usr/bin/env bsl

data Unit {
  Unit:Unit
}
data Int { one:Int
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

data Bool {True:Bool;False:Bool}
data List a{
Nil:forall a.List a;
Cons:forall a.a->List a->List a;
}
data ListModule {
MkListModule:(forall a.List a->a)-> -- head
             (forall a.List a->List a)-> -- tail
             (forall a.(a->Bool)->List a->List a)-> -- fillter
             ListModule
}
rec filter = \f -> \list -> case list of {
  Nil -> Nil;
  Cons x xs -> case f x of {
    True -> Cons x (filter f xs);
    False -> filter f xs
  }
} in
let undefined:forall a.a = ffi ` NULL` in
let mk = MkListModule in
let list = mk
(\x -> case x of {Cons h _->h;Nil->undefined})
(\x -> case x of {Cons _ t->t;Nil->undefined})
filter
in

--f1::forall a.a->a=\x->x
let f1 = R1 \x->x in
--f2::forall a.(forall b.b->b)->a->a=\f x-> f x
let f2 = R2 \f1->\x-> case f1 of {
R1 f1 -> f1 x
} in
--f3::forall a.(forall b.(forall c.c->c)->b->b)->a->a=\f x -> f (\x->x) x
let f3 = R3 \f2->\x-> case f2 of {
R2 f2 -> f2 (R1 \x->x) x
} in

let _ = case f3 of { R3 f3 -> f3 f2 Unit } in
--_::Unit = f3 f2 Unit


let f:(forall a.a->a)->(forall b.b->b)->Pair Unit Int = \f ->let h =f in \g ->Pair (g Unit) (g one)  in
rec f:(forall a.a->a)->(forall b.b->b)->Pair Unit Int = \x ->let h =x in \g -> f h g in
let f1:forall a.a->a=\x->x in
let f2:forall a.(forall b.b->b)->a->a=\f ->\x-> f x in
let f3:forall a.(forall b.(forall c.c->c)->b->b)->a->a =\f ->\x -> f (\x->x) x in
let _:Unit = f3 f2 Unit in

let eq :forall a.a->a->Unit = \x ->\y->Unit in
let g:(forall b.(forall a.a->a)->b)->Unit = \x-> Unit in 
let g' = g in--\x-> Unit in
let f:(forall a.forall b.(a->a)->b)->Unit = \x-> Unit in 

Unit

