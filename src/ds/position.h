#ifndef SU_BOLEYN_BSL_DS_POSITION_H
#define SU_BOLEYN_BSL_DS_POSITION_H

#include <iostream>
#include <string>

using namespace std;

struct Position {
  string filename;
  size_t beginRow, beginColumn, endRow, endColumn;
};

ostream &operator<<(ostream &out, const Position &p) {
  out << p.filename << ":[" << p.beginRow << "," << p.beginColumn << "-"
      << p.endRow << "," << p.endColumn << ")";
  return out;
}

#endif
