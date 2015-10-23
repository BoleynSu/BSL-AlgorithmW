#include <iostream>
#include <fstream>
#include <memory>
#include <utility>

#include "codegen.h"
#include "parse.h"

using namespace std;

int main() {
	ifstream in("main.bsl");
	Parser parser(in);
	Codegener codegener { cout };
	codegener.codegen(parser.parse());
}
