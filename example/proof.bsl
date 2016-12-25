#!/usr/bin/env bsl

data X {}
data Y {}
data Kill a b {}
data WillDie a {
  fact1:forall x.forall y.Kill y x->WillDie x
}
data HasAKiller {
  fact2:forall x.forall y.Kill x y->HasAKiller
}

let fact3:Kill X Y = ffi ` NULL ` in

let conc1 = fact1 fact3 in -- WillDie Y

let conc2 = \t -> case t of { -- forall x.WillDie x
  fact1 t -> -- -> exists y.Kill y x
    fact2 t -- -> HasAKiller
} in

let goal = conc2 conc1 in

goal


