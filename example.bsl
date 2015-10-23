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

data IO a where {
  Data::forall a.a->IO a;
  Read::forall a.(Int->IO a)->IO a;
  Write::forall a.Int->IO a->IO a
}

rec bind = \x -> \f ->
  case x of {
    Data x -> f x;
    Read g -> Read (\x -> bind (g x) f);
    Write c x -> Write c (bind x f)
  }
in

let return = Data
in

let getInt = Read (\x -> Data x)
in

let putInt = \x -> Write x (Data Zero)
in

rec runIO = \x ->
  case x of {
    Data x -> x;
    Read g -> runIO (g ffi ffiblock [=]() -> void* { return $v_bsl_Zero; }() ffiblock);
    Write c x -> (\x -> \y -> y)
                 ffi ffiblock [=](void* x) -> void* {
                   int v = 0;
                   for (;;) {
                     $t_bsl_Int * i = ($t_bsl_Int *) x;
                     if (i->T == 0) {
                       break;
                     }
                     v++;
                     x = (($d_bsl_Suc*) (i->ptr))->d0;
                   }
                   printf("%d\n", v);
                   return $v_bsl_Zero;
                 }($v_bsl_c) ffiblock
                 (runIO x)
  }
in

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
in runIO (putInt (eval (let four = let two = let one = I (Suc Zero) in A one one in A two two in four)))

