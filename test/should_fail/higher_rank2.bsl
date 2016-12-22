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
{-
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


let f:(forall a.a->a)->(forall b.b->b)->Pair Unit Int = \f ->let h =f in \g -> Pair (h Unit) (g one) in
let f1:forall a.a->a=\x->x in
let f2:forall a.(forall b.b->b)->a->a=\f ->\x-> f x in
let f3:forall a.(forall b.(forall c.c->c)->b->b)->a->a =\f ->\x -> f (\x->x) x in
let _:Unit = f3 f2 Unit in

let eq :forall a.a->a->Unit = \x ->\y->Unit in
let g:(forall b.(forall a.a->a)->b)->Unit = \x-> Unit in 
let g' = g in--\x-> Unit in
--}
let f:(forall a.forall b.(a->a)->b)->Unit = \x-> Unit in 
let g:(forall b.(forall a.a->a)->b)->Unit = f in--\x-> Unit in 

Unit

