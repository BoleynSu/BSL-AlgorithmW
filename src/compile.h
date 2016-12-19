#ifndef SU_BOLEYN_BSL_COMPILER_H
#define SU_BOLEYN_BSL_COMPILER_H

#include <cstdlib>
#include <iostream>
#include <memory>

#include "code_generate.h"
#include "ds/unit.h"
#include "lex.h"
#include "optimize.h"
#include "parse.h"
#include "sig_check.h"
#include "type_infer.h"

using namespace std;

struct Compiler {
  Compiler(int argc, char **argv) {
    if (argc != 2) {
      cout << argv[0] << " file" << endl;
      exit(EXIT_FAILURE);
    }
    Lexer lexer(argv[1]);
    Parser parser(lexer);
    auto unit = parser.parse();

    SigChecker sig_checker(unit);
    TypeInfer type_infer(unit);

    Optimizer optimizer;
    CodeGenerator code_generator(unit, optimizer, cout);
  }
};

#endif
