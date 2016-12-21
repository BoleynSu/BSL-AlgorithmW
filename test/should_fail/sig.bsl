#!/usr/bin/env bsl

data IO a {
}

data Monad {
}

let MkMonadIO:IO a->Monada = ffi ` NULL ` in
MkMonadIO
