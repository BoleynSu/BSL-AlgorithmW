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

Higher Kinded Types are suppoted now.

