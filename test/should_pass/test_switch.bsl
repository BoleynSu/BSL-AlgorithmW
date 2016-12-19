#!/usr/bin/env bsl

data Int {}

data Unit {
  Unit:Unit
}

data ABC {A:ABC;b:ABC;C:ABC}

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

let x = case A of {
A->Unit;
b->Unit;
C->Unit;
} in

case Just Nil of {
Nothing -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
Just x->x
}
