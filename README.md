# Algorithm W

This repo implements Algorithm W used in Hindley–Milner type system and is part of BSL (a.k.a Boleyn Su's Language) project.


# Try It

You can use following command to play with the example.

```bash
git clone https://github.com/BoleynSu/algorithm-w.git
cd algorithm-w
./bin/bsl ./example/sort.bsl
```

It will sort all inputed numbers. Enter CTRL+D to end your input.

You can replace `sort.bsl` with other examples in `example` folder.


# Change Log

GADT is supported now but I am not 100% sure if it's bug free.

Error messages are more user friendly now.

C is the target language now.

GC is added. Set env BSL_RT_WITH_GC to enable GC.

Existential Types are supported now.

Rank-N Types are supported now.

Higher Kinded Types are suppoted now, but there are some bugs to be fixed. The following code should work, but does not now.

```
#!/usr/bin/env bsl

data U{U:U}
data A a{A:forall a.a->A a}
data B a b{B:forall a.a->B a a}
data T a {
T:forall a.forall b.a b->T a
}
let any= ffi ` NULL ` in
let x = A U in
let x = B U in
let t= T x in 
any
```
