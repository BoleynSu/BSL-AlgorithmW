#ifndef SU_BOLEYN_BSL_CODEGEN_H
#define SU_BOLEYN_BSL_CODEGEN_H

#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <utility>

#include "data.h"
#include "expr.h"
#include "type.h"

using namespace std;

struct Codegener {
  ostream& out;

  void codegen(shared_ptr<Expr> expr) {
    switch (expr->T) {
      case 0:
        out << "$v_bsl_" << expr->x;
        return;
      case 1:
        out << "(*((std::function<void*(void*)>*)(";
        codegen(expr->e1);
        out << ")))(";
        codegen(expr->e2);
        out << ")";
        return;
      case 2:
        out << "new std::function<void*(void*)>([=](void* "
            << "$v_bsl_" << expr->x << ") -> void* { return ";
        codegen(expr->e);
        out << "; })";
        return;
      case 3:
        out << "[=]() -> void* { void* "
            << "$tmp_bsl_tmp"
            << " = ";
        codegen(expr->e1);
        out << ";  void* "
            << "$v_bsl_" << expr->x << " = $tmp_bsl_tmp; return ";
        codegen(expr->e2);
        out << "; } ()";
        return;
      case 4:  // TODO FIXME rec x = x + 1
        out << "[=]() -> void* {";
        out << " void";
        for (int i = 0; i < expr->xes.size(); i++) {
          auto t = find(expr->xes[i].second->type);
          if (t->T == 1) {
            out << (i ? ", *" : " *") << "$v_bsl_" << expr->xes[i].first
                << " = new ";
            if (t->D == "->") {
              out << "std::function<void*(void*)>";
            } else {
              out << "$t_bsl_" << t->D;
            }
            out << "()";
          } else {
            cerr << "ERROR!" << endl;
            cerr << "rec " << expr->xes[i].first << " = "
                 << expr->xes[i].second->to_string() << " in ..." << endl;
          }
        }
        out << ";";
        for (int i = 0; i < expr->xes.size(); i++) {
          auto t = find(expr->xes[i].second->type);
          out << " { void* $tmp_bsl_tmp = ";
          codegen(expr->xes[i].second);
          out << "; std::memcpy($v_bsl_" << expr->xes[i].first
              << ", $tmp_bsl_tmp, sizeof (";
          if (t->D == "->") {
            out << "std::function<void*(void*)>";
          } else {
            out << "$t_bsl_" << t->D;
          }
          out << ")); }";
        }
        out << " return ";
        codegen(expr->e);
        out << "; } ()";
        return;
      case 5:
        out << "[=]() -> void* { void* $tmp_bsl_tmp = ";
        codegen(expr->e);
        out << "; switch ((($t_bsl_" << find(expr->e->type)->D
            << "*)($tmp_bsl_tmp))->T) { ";
        for (int i = 0; i < expr->pes.size(); i++) {
          out << "case $t_bsl_" << find(expr->e->type)->D << "::$e_bsl_"
              << expr->pes[i].first[0] << ": {";
          for (int j = 1; j < expr->pes[i].first.size(); j++) {
            out << "void* "
                << "$v_bsl_" << expr->pes[i].first[j] << " = (($d_bsl_"
                << expr->pes[i].first[0] << "*)((($t_bsl_"
                << find(expr->e->type)->D << "*)($tmp_bsl_tmp))->ptr))->d"
                << j - 1 << ";";
          }
          out << " return ";
          codegen(expr->pes[i].second);
          out << "; } ";
        }
        out << "} }()";
        return;
      case 6:
        out << expr->ffi;
        return;
    }
  }

  void codegen(
      pair<shared_ptr<map<string, shared_ptr<Data>>>, shared_ptr<Expr>> prog) {
    auto data = prog.first;
    auto expr = prog.second;

    out << "#include <cstddef>" << endl
        << "#include <cstdint>" << endl
        << "#include <cstdio>" << endl
        << "#include <cstdlib>" << endl
        << "#include <cstring>" << endl
        << "#include <functional>" << endl
        << "void *operator new(std::size_t sz) { "
           "static char *b, *e; "
           "if (b + sz > e) { "
           "b = (char *) std::malloc(sz > (1 << 30) ? sz : (1 << 30)); "
           "e = b + (sz > (1 << 30) ? sz : (1 << 30)); "
           "} "
           "return e -= sz; "
           "}"
        << endl;
    map<string, int> cl;
    for (auto dai : *data) {
      auto da = dai.second;
      switch (da->T) {
        case 0: {
          out << "struct $t_bsl_" << da->name << " { enum {";
          for (int i = 0; i < da->constructors.size(); i++) {
            auto c = da->constructors[i];
            out << (i ? ", " : " ") << "$e_bsl_" << c.first;
          }
          out << " } T; void* ptr; };" << endl;
          for (int i = 0; i < da->constructors.size(); i++) {
            auto c = da->constructors[i];
            out << "struct $d_bsl_" << c.first << " {";
            auto t1 = c.second;
            while (t1->T == 1) {
              t1 = t1->sigma;
            }
            auto t2 = t1->tau;
            int& j = cl[c.first];
            while (t2->T == 1 && t2->D == "->") {
              out << " void* "
                  << "d" << j << ";";
              j++;
              t2 = t2->tau[1];
            }
            out << " };" << endl;
          }
          break;
        }
        case 1: {
          out << "typedef " << da->ffi << " $t_bsl_" << da->name << ";" << endl;
          break;
        }
      }
    }

    for (auto dai : *data) {
      auto da = dai.second;
      if (da->T == 0) {
        for (int i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          auto e = make_shared<Expr>();
          e->T = 3;
          e->x = c.first;
          auto lam = make_shared<Expr>();
          auto cur = lam;
          for (int j = 0; j < cl[c.first]; j++) {
            stringstream s;
            s << "arg" << j;
            cur->T = 2;
            cur->x = s.str();
            cur->e = make_shared<Expr>();
            cur = cur->e;
          }
          stringstream s;
          s << "new "
            << "$t_bsl_" << da->name << " { "
            << "$t_bsl_" << da->name << "::$e_bsl_" << c.first << ", new "
            << "$d_bsl_" << c.first << "{";
          for (int j = 0; j < cl[c.first]; j++) {
            s << " $v_bsl_arg" << j << (j + 1 == cl[c.first] ? "" : ",");
          }
          s << " } }";
          cur->T = 6;
          cur->ffi = s.str();
          e->e1 = lam;
          e->e1->sig = c.second;
          e->e2 = expr;
          expr = e;
        }
      }
    }
    stringstream c;
    infer(expr, make_shared<map<string, shared_ptr<Poly>>>(), cl);
    out << "int main() { ";
    codegen(expr);
    out << "; }" << endl;
  }
};

#endif
