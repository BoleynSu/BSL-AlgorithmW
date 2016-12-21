#ifndef SU_BOLEYN_BSL_DS_POSITION_H
#define SU_BOLEYN_BSL_DS_POSITION_H

#include <iostream>
#include <string>

using namespace std;

struct Position {
  string filename;
  size_t beginRow, beginColumn, endRow, endColumn;
};

string to_string(const Position &p) {
  stringstream out;
  out << p.filename << ":[" << p.beginRow << "," << p.beginColumn << "-"
      << p.endRow << "," << p.endColumn << ")";
  return out.str();
}

#endif
