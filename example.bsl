data int where {
  zero::int;
  suc::int->int
}

data bool where {
  false::bool;
  true::bool
}

data T a where {
  I::int->T int;
  B::bool->T bool;
  A::T int->T int->T int;
  E::T int->T int->T bool
}

rec add = \a -> \b ->
  case a of {
    zero -> b;
    suc c -> suc (add c b)
  }
in

rec eq = \a -> \b ->
  case a of {
    zero ->
      case b of {
        zero -> true;
        suc d -> false
      };
    suc c ->
      case b of {
        zero -> false;
        suc d -> eq c d
      }
  }
in

rec eval = \x ->
  case x of {
    I i -> i;
    B b -> b;
    A a b -> add (eval a) (eval b);
    Eq a b -> eq (eval a) (eval b)
  }
in eval;
