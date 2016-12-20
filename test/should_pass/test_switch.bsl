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

let x = case C of {
A -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
b -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
C->Unit;
} in

let x = case b of {
C -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
A -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
b->Unit;
} in

let x = case A of {
C -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
A->Unit;
b -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
} in
let x = case b of {
C -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
--b -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
b->Unit;
A -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
--A->Unit;
} in

case Just Nil of {
Nothing -> ffi ` (puts("ERROR!!!"),exit(1),NULL) `;
Just x->x
}
