#ifndef SU_BOLEYN_BSL_EXPR_H
#define SU_BOLEYN_BSL_EXPR_H

#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "lex.h"

using namespace std;

struct Mono;
struct Poly;

struct Expr {
  int T;
  string x;
  shared_ptr<Expr> e1, e2, e;
  vector<pair<string, shared_ptr<Expr> > > xes;
  vector<pair<vector<string>, shared_ptr<Expr> > > pes;
  string ffi;
  shared_ptr<Mono> type;
  shared_ptr<Poly> sig, gadt;
  Position pos;

  string to_string() {
    switch (T) {
      case 0:
        return x;
      case 1:
        return e1->to_string() + " (" + e2->to_string() + ")";
      case 2:
        return "(\\" + x + "->" + e->to_string() + ")";
      case 3:
        return "(let " + x + " = " + e1->to_string() + " in " +
               e2->to_string() + ")";
      case 4: {
        stringstream s;
        s << "(rec";
        for (int i = 0; i < xes.size(); i++) {
          if (i) {
            s << " and ";
          } else {
            s << " ";
          }
          s << xes[i].first << " = " << xes[i].second->to_string();
        }
        s << " in " << e->to_string() << ")";
        return s.str();
      }
      case 5: {
        stringstream s;
        s << "(case " << e->to_string() << " of {";
        for (int i = 0; i < pes.size(); i++) {
          for (int j = 0; j < pes[i].first.size(); j++) {
            s << pes[i].first[j] << (j + 1 == pes[i].first.size() ? "" : " ");
          }
          s << "->" << pes[i].second->to_string()
            << (i + 1 == pes.size() ? "" : ";");
        }
        s << "})";
        return s.str();
      }
      case 6:
        return ffi;
    }
  }
};

#endif
