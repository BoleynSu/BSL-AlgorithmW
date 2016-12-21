#ifndef SU_BOLEYN_BSL_COMPILER_H
#define SU_BOLEYN_BSL_COMPILER_H

#include <cstdlib>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "code_generate.h"
#include "ds/unit.h"
#include "lex.h"
#include "optimize.h"
#include "parse.h"
#include "type_infer.h"

using namespace std;

struct Compiler {
  string cmd;
  void usage() {
    cerr << "Usage: " << cmd << " [options] file..." << endl
         << "Options:" << endl
         << "  -c\t\t\tCompile to C only" << endl
         << "  -i $include_path\tAdd an include path" << endl
         << "  -m $options\t\tPass more options to gcc" << endl
         << "  -e $executable\tCompile to an executable" << endl;
    exit(EXIT_FAILURE);
  }
  Compiler(int argc, char** argv) : cmd(argv[0]) {
    string source, executable;
    bool c_only = false;
    vector<string> include_path;
    string more;
    for (int i = 1; i < argc; i++) {
      if (argv[i][0] == '-') {
        switch (argv[i][1]) {
          case 'c': {
            c_only = true;
            break;
          }
          case 'i': {
            i++;
            if (!(i < argc)) {
              usage();
            }
            include_path.push_back(argv[i]);
            break;
          }
          case 'm': {
            i++;
            if (!(i < argc) || !more.empty()) {
              usage();
            }
            more = argv[i];
            break;
          }
          case 'e':
            i++;
            if (!(i < argc) || !executable.empty()) {
              usage();
            }
            executable = argv[i];
            break;
          default:
            usage();
        }
      } else {
        if (!source.empty()) {
          usage();
        }
        source = argv[i];
      }
    }
    if (source.empty()) {
      usage();
    }

    ifstream src(source);
    Lexer lexer(source, src);
    Parser parser(lexer);
    auto unit = parser.parse();

    TypeInfer type_infer(unit);

    ofstream csrc(source + ".c");
    CodeGenerator code_generator(csrc, unit, make_shared<Optimizer>());

    if (!c_only) {
      stringstream gcc_cmd;
      gcc_cmd << "gcc " << source << ".c";
      for (auto& ip : include_path) {
        gcc_cmd << " -I" << ip;
      }
      if (!more.empty()) {
        gcc_cmd << " " << more;
      }
      if (!executable.empty()) {
        gcc_cmd << " -o " << executable;
      }
      if (int code = system(gcc_cmd.str().c_str())) {
        exit(code);
      }

      if (executable.empty()) {
        if (int code = system("time ./a.out")) {
          exit(code);
        }
      }

      stringstream clean_cmd;
      clean_cmd << "rm " << source << ".c"
                << ";";
      if (executable.empty()) {
        clean_cmd << "rm a.out;";
      }
      if (int code = system(clean_cmd.str().c_str())) {
        exit(code);
      }
    }
  }
};

#endif
