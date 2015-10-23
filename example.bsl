data Int where {
  Zero::Int;
  Suc::Int->Int
}

data Bool where {
  False::Bool;
  True::Bool
}

data T a where {
  I::Int->T Int;
  B::Bool->T Bool;
  A::T Int->T Int->T Int;
  E::T Int->T Int->T Bool
}

rec add = \a -> \b ->
  case a of {
    Zero -> b;
    Suc c -> Suc (add c b)
  }
in

rec eq = \a -> \b ->
  case a of {
    Zero ->
      case b of {
        Zero -> True;
        Suc d -> False
      };
    Suc c ->
      case b of {
        Zero -> False;
        Suc d -> eq c d
      }
  }
in

rec eval = \x ->
  case x of {
    I i -> i;
    B b -> b;
    A a b -> add (eval a) (eval b);
    E a b -> eq (eval a) (eval b)
  }
in eval;
