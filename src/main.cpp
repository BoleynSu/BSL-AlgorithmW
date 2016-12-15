#include <cstdlib>
#include <iostream>

#include "code_generate.h"
#include "lex.h"
#include "parse.h"

using namespace std;

int main(int argc, char **argv) {
  if (argc != 2) {
    cout << argv[0] << " file" << endl;
    return EXIT_FAILURE;
  }

  Lexer lexer(argv[1]);
  Parser parser(lexer);
  CodeGenerator code_generator(parser, cout);
}
