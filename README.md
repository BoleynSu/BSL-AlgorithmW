# Algorithm W

This repo implements Algorithm W used in Hindley–Milner type system and is part of BSL (a.k.a Boleyn Su's Language) project.


# Try It

You can use following command to play with the example.

```bash
git clone https://github.com/BoleynSu/algorithm-w.git
cd algorithm-w
g++ src/main.cpp -std=c++11 -o bslc &&
./bslc example/sort.bsl > example/sort.c &&
gcc -w -Irt example/sort.c -o example/sort &&
./example/sort
```

It will sort all inputed numbers. Enter CTRL+D to end your input.

You can replace `sort.bsl` with other examples in `example` folder.


# Change Log

GADT is supported now but I am not 100% sure if it's bug free.

Error messages are more user friendly now.

C is the target language now.

GC is added. Set env BSL_RT_WITH_GC to enable GC.

