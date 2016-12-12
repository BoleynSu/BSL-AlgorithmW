#!/usr/bin/env bsl

data Pair a b where {
  Pair:forall a.forall b.a->b->Pair a b;
}

data List a where {
  Nil:forall a.List a;
  Cons:forall a.a->List a->List a;
}
data String where {
}

data Expr where {
  EVar:String->Expr;
  EApp:Expr->Expr->Expr;
  EAbs:String->Expr;
  ELet:String->Expr->Expr;
  ERec:List (Pair String Expr)->Expr;
  ECaseOf:List (Pair (List String) Expr)->Expr;
}

data Mono where {
  MVar:String->Mono;
  MApp:String->List Mono->Mono;
}

data Poly where {
  PFromMono:Mono->Poly;
  PQua:String->Poly;
}

let x = \x -> x in x

