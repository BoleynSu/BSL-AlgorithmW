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

	void codegen(shared_ptr<Expr> expr, map<string, int>& ci) {
		switch (expr->T) {
		case 0:
			out << "$v_bsl_" << expr->x;
			return;
		case 1:
			out << "(*((function<void*(void*)>*)(";
			codegen(expr->e1, ci);
			out << ")))(";
			codegen(expr->e2, ci);
			out << ")";
			return;
		case 2:
			out << "new function<void*(void*)>([=](void* " << "$v_bsl_"
					<< expr->x << ") -> void* { return ";
			codegen(expr->e, ci);
			out << "; })";
			return;
		case 3:
			out << "[=]() -> void* { void* " << "$tmp_bsl_tmp" << " = ";
			codegen(expr->e1, ci);
			out << ";  void* " << "$v_bsl_" << expr->x
					<< " = $tmp_bsl_tmp; return ";
			codegen(expr->e2, ci);
			out << "; } ()";
			return;
		case 4: //TODO FIXME
			out << "[=]() -> void* {";
			out << " void*";
			for (int i = 0; i < expr->xes.size(); i++) {
				auto t = find(expr->xes[i].second->type);
				if (t->T == 1) {
					out << (i ? ", " : " ") << "$v_bsl_" << expr->xes[i].first
							<< " = new ";
					if (t->D == "->") {
						out << "function<void*(void*)>";
					} else {
						out << "$t_bsl_" << t->D;
					}
					out << "()";
				} else {
					cerr << "ERROR!" << endl;
				}
			}
			out << ";";
			for (int i = 0; i < expr->xes.size(); i++) {
				auto t = find(expr->xes[i].second->type);
				out << " { void* $tmp_bsl_tmp = ";
				codegen(expr->xes[i].second, ci);
				out << "; *((";
				if (t->D == "->") {
					out << "function<void*(void*)>";
				} else {
					out << "$t_bsl_" << t->D;
				}
				out << "*)$v_bsl_" << expr->xes[i].first << ") = *((";
				if (t->D == "->") {
					out << "function<void*(void*)>";
				} else {
					out << "$t_bsl_" << t->D;
				}
				out << "*)$tmp_bsl_tmp); }";
			}
			out << " return ";
			codegen(expr->e, ci);
			out << "; } ()";
			return;
		case 5:
			out << "[=]() -> void* { void* $tmp_bsl_tmp = ";
			codegen(expr->e, ci);
			out << "; switch ((($t_bsl_" << find(expr->e->type)->D
					<< "*)($tmp_bsl_tmp))->T) {";
			for (int i = 0; i < expr->pes.size(); i++) {
				out << "case " << ci[expr->pes[i].first[0]] << ": {";
				for (int j = 1; j < expr->pes[i].first.size(); j++) {
					out << "void* " << "$v_bsl_" << expr->pes[i].first[j]
							<< " = (($d_bsl_" << expr->pes[i].first[0]
							<< "*)((($t_bsl_" << find(expr->e->type)->D
							<< "*)($tmp_bsl_tmp))->ptr))->d" << j - 1 << ";";
				}
				out << " return ";
				codegen(expr->pes[i].second, ci);
				out << "; }";
			}
			out << "} }()";
			return;
		case 6:
			out << expr->ffi;
			return;
		}
	}

	void codegen(
			pair<shared_ptr<map<string, shared_ptr<Data> > >, shared_ptr<Expr> > prog) {
		auto data = prog.first;
		auto expr = prog.second;

		out << "#include <functional>" << endl << "#include <cstdio>"
				<< endl << "using std::function;" << endl;
		map<string, int> cl, ci;
		for (auto dai : *data) {
			auto da = dai.second;
			out << "struct $t_bsl_" << da->name << " { int T; void* ptr; };"
					<< endl;
			for (int i = 0; i < da->constructors.size(); i++) {
				auto c = da->constructors[i];
				cerr << "//" << c.first << " :: " << to_string(c.second)
						<< endl;
				out << "struct $d_bsl_" << c.first << " {";
				auto t1 = c.second;
				while (t1->T == 1) {
					t1 = t1->sigma;
				}
				auto t2 = t1->tau;
				int &j = cl[c.first];
				while (t2->T == 1 && t2->D == "->") {
					out << "void* " << "d" << j << ";";
					j++;
					t2 = t2->tau[1];
				}
				ci[c.first] = i;
				out << " };" << endl;
			}
		}

		for (auto dai : *data) {
			auto da = dai.second;
			for (int i = 0; i < da->constructors.size(); i++) {
				auto c = da->constructors[i];
				auto e = make_shared<Expr>();
				e->T = 3;
				e->x = c.first;
				auto lam = make_shared<Expr>();
				auto cur = lam;
				for (int j = 0; j < cl[c.first]; j++) {
					stringstream s;
					s << j;
					cur->T = 2;
					cur->x = s.str();
					cur->e = make_shared<Expr>();
					cur = cur->e;
				}
				stringstream s;
				s << "new " << "$t_bsl_" << da->name << " { " << i << ", new "
						<< "$d_bsl_" << c.first << "{";
				for (int j = 0; j < cl[c.first]; j++) {
					s << " $v_bsl_" << j << (j + 1 == cl[c.first] ? "" : ",");
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
		stringstream c;
		infer(expr, make_shared<map<string, shared_ptr<Poly> > >(), cl);
		out << "int main() { ";
		codegen(expr, ci);
		out << "; }" << endl;
	}
};

#endif
