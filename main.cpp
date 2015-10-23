#include <iostream>
#include <utility>

#include "codegen.h"
#include "parse.h"

using namespace std;

int main() {
	Parser parser(cin);
	Codegener codegener { cout };
	codegener.codegen(parser.parse());
}
