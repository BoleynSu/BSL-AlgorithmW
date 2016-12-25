#!/usr/bin/env bsl

data X {}
data Y {}
data Kill a b {}
data WillDie a {
  fact1:forall x.forall y.Kill y x->WillDie x
}
data HasKiller {
  fact2:forall x.forall y.Kill x y->HasKiller
}

let fact3 = \x -> case x of { --forall x.exists y.WillDie x->Kill y x
  fact1 y -> y
} in
let fact4:Kill X Y = ffi ` NULL ` in

let conc1 = fact1 fact4 in -- leads to WillDie Y
let conc2 = fact3 conc1 in -- leads to exists x.Kill x Y
let conc3 = fact2 conc2 in -- leads to HasKiller

conc3

