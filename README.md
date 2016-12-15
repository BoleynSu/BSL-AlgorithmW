# algorithm-w
Algorithm W used in Hindley–Milner type system

You can use following command to play with the example.

```bash
git clone https://github.com/BoleynSu/algorithm-w.git
cd algorithm-w
g++ main.cpp -std=c++11 -o bslc &&
./bslc example/example.bsl > example/example.c &&
gcc -w -Irt example/example.c -o example/example &&
./example/example
```
It will sort all inputed numbers. Use CTRL+D to end your input.

# change log

GADT is supported now but I am not 100% sure if it's bug free.

Error message is more user friendly now.

C is the target language now.

GC is added. Set env BSL_RT_WITH_GC to enable GC.
