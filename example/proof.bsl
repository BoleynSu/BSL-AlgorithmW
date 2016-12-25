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

let fact3:Kill X Y = ffi ` NULL ` in

let conc1 = fact1 fact3 in --conc1:WillDie Y

case conc1 of {
  fact1 conc2 -> --conc2:exists x.Kill x Y
    let conc3 = fact2 conc2 in --conc3:HasKiller
    let conc2_ = conc2 in
    let eq:forall a.a->a->a=\a-> \a -> a in
    let _ = eq conc2 conc2_ in
    conc3
}
