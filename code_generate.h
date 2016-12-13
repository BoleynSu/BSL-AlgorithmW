#ifndef SU_BOLEYN_BSL_CODE_GENERATE_H
#define SU_BOLEYN_BSL_CODE_GENERATE_H

#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "data.h"
#include "expr.h"
#include "parse.h"
#include "type.h"
#include "type_inference.h"

using namespace std;

struct CodeGenerator {
  Parser& parser;
  ostream& out;
  CodeGenerator(Parser& parser, ostream& out) : parser(parser), out(out) {
    codegen(parser.parse());
  }

  string var(string v) { return "$v_bsl_" + v; }
  string tmp() { return "$tmp_bsl"; }
  string type(string t) { return "$type_bsl_" + t; }
  string tag(string t) { return "$tag_bsl_" + t; }
  string arg(size_t i) {
    stringstream ss;
    ss << "arg" << i;
    return ss.str();
  }
  string ffi(string f) { return f; }

  void codegen(shared_ptr<Expr> expr) {
    switch (expr->T) {
      case ExprType::VAR:
        out << var(expr->x);
        return;
      case ExprType::APP:
        out << "(*((std::function<void*(void*)>*)(";
        codegen(expr->e1);
        out << ")))(";
        codegen(expr->e2);
        out << ")";
        return;
      case ExprType::ABS:
        out << "new std::function<void*(void*)>([=](void* " << var(expr->x)
            << ") -> void* { return ";
        codegen(expr->e);
        out << "; })";
        return;
      case ExprType::LET:
        out << "[=]() -> void* { void* " << tmp() << " = ";
        codegen(expr->e1);
        out << "; void* " << var(expr->x) << " = " << tmp() << "; return ";
        codegen(expr->e2);
        out << "; } ()";
        return;
      case ExprType::REC:  // TODO FIXME rec x = x + 1
        out << "[=]() -> void* {";
        out << " void";
        for (size_t i = 0; i < expr->xes.size(); i++) {
          auto t = find(expr->xes[i].second->type);
          if (t->is_const) {
            out << (i ? ", *" : " *") << var(expr->xes[i].first) << " = new ";
            if (t->D == "->") {
              out << "std::function<void*(void*)>";
            } else {
              out << type(t->D);
            }
            out << "()";
          } else {
            cerr << "code generator:" << endl
                 << "rec " << expr->xes[i].first << " = "
                 << expr->xes[i].second->to_string() << " in ..." << endl;
            exit(EXIT_FAILURE);
          }
        }
        out << ";";
        for (size_t i = 0; i < expr->xes.size(); i++) {
          auto t = find(expr->xes[i].second->type);
          out << " { void* " << tmp() << " = ";
          codegen(expr->xes[i].second);
          out << "; std::memcpy(" << var(expr->xes[i].first) << ", " << tmp()
              << ", sizeof (";
          if (t->D == "->") {
            out << "std::function<void*(void*)>";
          } else {
            out << type(t->D);
          }
          out << ")); }";
        }
        out << " return ";
        codegen(expr->e);
        out << "; } ()";
        return;
      case ExprType::CASE:
        out << "[=]() -> void* { void* " << tmp() << " = ";
        codegen(expr->e);
        out << "; switch (((" << type(find(expr->e->type)->D) << "*)(" << tmp()
            << "))->T) { ";
        for (size_t i = 0; i < expr->pes.size(); i++) {
          out << "case " << type(find(expr->e->type)->D)
              << "::" << tag(expr->pes[i].first[0]) << ": {";
          for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
            out << " void* " << var(expr->pes[i].first[j]) << " = (("
                << type(find(expr->e->type)->D) << "*)(" << tmp() << "))->"
                << arg(j - 1) << ";";
          }
          out << " return ";
          codegen(expr->pes[i].second);
          out << "; } ";
        }
        out << "} }()";
        return;
      case ExprType::FFI:
        out << ffi(expr->ffi);
        return;
    }
  }

  void codegen(pair<pair<shared_ptr<map<string, shared_ptr<Data>>>,
                         shared_ptr<map<string, shared_ptr<Constructor>>>>,
                    shared_ptr<Expr>>
                   prog) {
    auto data = prog.first.first;
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
    for (auto dai : *data) {
      auto da = dai.second;
      if (da->is_ffi) {
        out << "typedef " << ffi(da->ffi) << " " << type(da->name) << ";"
            << endl;
      } else {
        size_t maxarg = 0;
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          maxarg = max(maxarg, c->arg);
        }
        out << "struct " << type(da->name) << " { enum {";
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          out << (i ? ", " : " ") << tag(c->name);
        }
        out << " } T; ";
        for (size_t i = 0; i < maxarg; i++) {
          out << "void *" << arg(i) << "; ";
        }
        out << "};" << endl;
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
          for (size_t j = 0; j < c->arg; j++) {
            cur->T = ExprType::ABS;
            cur->x = arg(j);
            cur->e = make_shared<Expr>();
            cur = cur->e;
          }
          stringstream s;
          s << "new " << type(da->name) << " { " << type(da->name)
            << "::" << tag(c->name) << ", ";
          for (size_t j = 0; j < c->arg; j++) {
            s << var(arg(j)) << (j + 1 == c->arg ? " " : ", ");
          }
          s << "}";
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
    infer(expr, make_shared<map<string, shared_ptr<Poly>>>(), prog.first);
    out << "int main() { ";
    codegen(expr);
    out << "; }" << endl;
  }
};

#endif
