#ifndef SU_BOLEYN_BSL_DATA_H
#define SU_BOLEYN_BSL_DATA_H

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "type.h"

using namespace std;

struct Data {
	string name;
	vector<pair<string, shared_ptr<Poly> > > constructors;
};

#endif
