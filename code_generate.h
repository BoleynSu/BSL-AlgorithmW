#ifndef SU_BOLEYN_BSL_CODE_GENERATE_H
#define SU_BOLEYN_BSL_CODE_GENERATE_H

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "data.h"
#include "expr.h"
#include "parse.h"
#include "type_inference.h"

using namespace std;

struct CodeGenerator {
  Parser& parser;
  ostream& out;
  CodeGenerator(Parser& parser, ostream& out) : parser(parser), out(out) {
    codegen(parser.parse());
  }

  void codegen(shared_ptr<Expr> expr) {
    switch (expr->T) {
      case ExprType::VAR:
        out << "$v_bsl_" << expr->x;
        return;
      case ExprType::APP:
        out << "(*((std::function<void*(void*)>*)(";
        codegen(expr->e1);
        out << ")))(";
        codegen(expr->e2);
        out << ")";
        return;
      case ExprType::ABS:
        out << "new std::function<void*(void*)>([=](void* "
            << "$v_bsl_" << expr->x << ") -> void* { return ";
        codegen(expr->e);
        out << "; })";
        return;
      case ExprType::LET:
        out << "[=]() -> void* { void* "
            << "$tmp_bsl_tmp"
            << " = ";
        codegen(expr->e1);
        out << ";  void* "
            << "$v_bsl_" << expr->x << " = $tmp_bsl_tmp; return ";
        codegen(expr->e2);
        out << "; } ()";
        return;
      case ExprType::REC:  // TODO FIXME rec x = x + 1
        out << "[=]() -> void* {";
        out << " void";
        for (size_t i = 0; i < expr->xes.size(); i++) {
          auto t = find(expr->xes[i].second->type);
          if (t->is_const) {
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
        for (size_t i = 0; i < expr->xes.size(); i++) {
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
      case ExprType::CASE:
        out << "[=]() -> void* { void* $tmp_bsl_tmp = ";
        codegen(expr->e);
        out << "; switch ((($t_bsl_" << find(expr->e->type)->D
            << "*)($tmp_bsl_tmp))->T) { ";
        for (size_t i = 0; i < expr->pes.size(); i++) {
          out << "case $t_bsl_" << find(expr->e->type)->D << "::$e_bsl_"
              << expr->pes[i].first[0] << ": {";
          for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
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
      case ExprType::FFI:
        out << expr->ffi;
        return;
    }
  }

  void codegen(pair<pair<shared_ptr<map<string, shared_ptr<Data>>>,
                         shared_ptr<map<string, shared_ptr<Constructor>>>>,
                    shared_ptr<Expr>>
                   prog) {
    auto data = prog.first.first;
    auto cons = prog.first.second;
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
           "b = (char *) std::malloc(sz > (1 << 23) ? sz : (1 << 23)); "
           "e = b + (sz > (1 << 23) ? sz : (1 << 23)); "
           "} "
           "return e -= sz; "
           "}"
        << endl;
    map<string, size_t> cl;
    for (auto dai : *data) {
      auto da = dai.second;
      if (da->is_ffi) {
        out << "typedef " << da->ffi << " $t_bsl_" << da->name << ";" << endl;
      } else {
        out << "struct $t_bsl_" << da->name << " { enum {";
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          out << (i ? ", " : " ") << "$e_bsl_" << c->name;
        }
        out << " } T; void* ptr; };" << endl;
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          out << "struct $d_bsl_" << c->name << " {";
          auto t1 = c->type;
          while (t1->is_poly) {
            t1 = t1->sigma;
          }
          auto t2 = t1->tau;
          size_t& j = cl[c->name];
          while (t2->is_const && t2->D == "->") {
            out << " void* "
                << "d" << j << ";";
            j++;
            t2 = t2->tau[1];
          }
          out << " };" << endl;
        }
      }
    }

    for (auto dai : *data) {
      auto da = dai.second;
      if (!da->is_ffi) {
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          auto e = make_shared<Expr>();
          e->T = ExprType::LET;
          e->x = c->name;
          auto lam = make_shared<Expr>();
          auto cur = lam;
          for (size_t j = 0; j < cl[c->name]; j++) {
            stringstream s;
            s << "arg" << j;
            cur->T = ExprType::ABS;
            cur->x = s.str();
            cur->e = make_shared<Expr>();
            cur = cur->e;
          }
          stringstream s;
          s << "new "
            << "$t_bsl_" << da->name << " { "
            << "$t_bsl_" << da->name << "::$e_bsl_" << c->name << ", new "
            << "$d_bsl_" << c->name << "{";
          for (size_t j = 0; j < cl[c->name]; j++) {
            s << " $v_bsl_arg" << j << (j + 1 == cl[c->name] ? "" : ",");
          }
          s << " } }";
          cur->T = ExprType::FFI;
          cur->ffi = s.str();
          e->e1 = lam;
          e->e1->sig = c->type;
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
