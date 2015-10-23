#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using namespace std;

struct Mono {
	int T;
	shared_ptr<Mono> alpha;
	string D;
	vector<shared_ptr<Mono> > tau;
};

struct Poly {
	int T;
	shared_ptr<Mono> tau;
	shared_ptr<Mono> alpha;
	shared_ptr<Poly> sigma;
};

shared_ptr<Mono> find(shared_ptr<Mono> x) {
	switch (x->T) {
	case 0:
		if (x->alpha != nullptr) {
			x->alpha = find(x->alpha);
			return x->alpha;
		} else {
			return x;
		}
	case 1:
		return x;
	}
}

string to_string(shared_ptr<Mono> tau) {
	tau = find(tau);
	switch (tau->T) {
	case 0: {
		stringstream s;
		s << "t" << find(tau);
		return s.str();
	}
	case 1:
		if (tau->D == "->") {
			return "(" + to_string(tau->tau[0]) + ")->("
					+ to_string(tau->tau[1]) + ")";
		} else {
			string ret = tau->D;
			for (auto t : tau->tau) {
				ret += " (" + to_string(t) + ")";
			}
			return ret;
		}
	}
}

string to_string(shared_ptr<Poly> sigma) {
	switch (sigma->T) {
	case 0:
		return to_string(sigma->tau);
	case 1: {
		stringstream s;
		s << "t" << find(sigma->alpha);
		return "forall " + s.str() + " . " + to_string(sigma->sigma);
	}
	}
}

shared_ptr<Mono> newvar() {
	return make_shared<Mono>(Mono { 0 });
}

shared_ptr<Mono> inst(shared_ptr<Mono> tau,
		map<shared_ptr<Mono>, shared_ptr<Mono> >& m) {
	tau = find(tau);
	switch (tau->T) {
	case 0:
		if (m.count(tau)) {
			return m[tau];
		} else {
			return tau;
		}
	case 1: {
		auto t = make_shared<Mono>();
		t->T = tau->T;
		t->D = tau->D;
		for (int i = 0; i < tau->tau.size(); i++) {
			t->tau.push_back(inst(tau->tau[i], m));
		}
		return t;
	}
	}
}

shared_ptr<Mono> inst(shared_ptr<Poly> sigma,
		map<shared_ptr<Mono>, shared_ptr<Mono> >& m) {
	switch (sigma->T) {
	case 0:
		return inst(sigma->tau, m);
	case 1:
		if (!m.count(find(sigma->alpha))) {
			m[find(sigma->alpha)] = newvar();
		}
		return inst(sigma->sigma, m);
	}
}

shared_ptr<Mono> inst(shared_ptr<Poly> sigma) {
	map<shared_ptr<Mono>, shared_ptr<Mono> > m;
	return inst(sigma, m);
}

void ftv(set<shared_ptr<Mono> >& f, shared_ptr<Mono> tau) {
	tau = find(tau);
	switch (tau->T) {
	case 0:
		f.insert(tau);
		return;
	case 1:
		for (int i = 0; i < tau->tau.size(); i++) {
			ftv(f, tau->tau[i]);
		}
		return;
	}
}

void ftv(set<shared_ptr<Mono> >& f, shared_ptr<Poly> sigma) {
	switch (sigma->T) {
	case 0:
		ftv(f, sigma->tau);
		return;
	case 1:
		ftv(f, sigma->sigma);
		f.erase(find(sigma->alpha));
		return;
	}
}

shared_ptr<Poly> gen(shared_ptr<map<string, shared_ptr<Poly> > > context,
		shared_ptr<Mono> tau) {
	tau = find(tau);
	set<shared_ptr<Mono> > f;
	for (auto c : *context) {
		set<shared_ptr<Mono> > fi;
		ftv(fi, c.second);
		f.insert(fi.begin(), fi.end());
	}
	set<shared_ptr<Mono> > fp;
	ftv(fp, tau);
	for (auto i : f) {
		fp.erase(i);
	}
	map<shared_ptr<Mono>, shared_ptr<Mono> > m;
	for (auto f : fp) {
		m[f] = newvar();
	}
	auto g = make_shared<Poly>(Poly { 0, inst(tau, m) });
	for (auto f : m) {
		g = make_shared<Poly>(Poly { 1, nullptr, f.second, g });
	}
	return g;
}

bool occ(shared_ptr<Mono> a, shared_ptr<Mono> b) {
	switch (b->T) {
	case 0:
		return a == b;
	case 1:
		for (int i = 0; i < b->tau.size(); i++) {
			if (occ(a, find(b->tau[i]))) {
				return true;
			}
		}
		return false;
	}
}

void unify(shared_ptr<Mono> a, shared_ptr<Mono> b) {
	a = find(a);
	b = find(b);
	if (a->T == 1 && b->T == 1 && a->D == b->D
			&& a->tau.size() == b->tau.size()) {
		for (int i = 0; i < a->tau.size(); i++) {
			unify(a->tau[i], b->tau[i]);
		}
	} else if (a->T == 0) {
		if (a != b) {
			if (occ(a, b)) {
				cout << "ERROR!" << endl; //FIXME
				cout << to_string(a) << " ~ " << to_string(b) << endl;
			} else {
				a->alpha = b;
			}
		}
	} else if (b->T == 0) {
		if (a != b) {
			if (occ(b, a)) {
				cout << "ERROR!" << endl; //FIXME
				cout << to_string(b) << " ~ " << to_string(a) << endl;
			} else {
				b->alpha = a;
			}
		}
	} else {
		cout << "ERROR!" << endl; //FIXME
		cout << to_string(a) << " != " << to_string(b) << endl;
	}
}

struct Expr {
	int T;
	string x;
	shared_ptr<Expr> e1, e2, e;
	vector<pair<string, shared_ptr<Expr> > > xes;
	vector<pair<vector<string>, shared_ptr<Expr> > > pes;
	string ffi;
	shared_ptr<Mono> type;
	shared_ptr<Poly> sig;

	void infer(shared_ptr<map<string, shared_ptr<Poly> > > context,
			map<string, int>& cl) {
		switch (T) {
		case 0:
			if (context->count(x)) {
				type = inst((*context)[x]);
			} else {
				cout << "ERROR!" << endl; //FIXME
				cout << x << " not in context" << endl;
			}
			break;
		case 1: {
			e1->infer(context, cl);
			e2->infer(context, cl);
			type = newvar();
			auto t = make_shared<Mono>();
			t->T = 1;
			t->D = "->";
			t->tau.push_back(e2->type);
			t->tau.push_back(type);
			unify(e1->type, t);
			break;
		}
		case 2: {
			auto tau = newvar();
			auto contextx = context->count(x) ? (*context)[x] : nullptr;
			(*context)[x] = make_shared<Poly>(Poly { 0, tau });
			e->infer(context, cl);
			type = make_shared<Mono>();
			type->T = 1;
			type->D = "->";
			type->tau.push_back(tau);
			type->tau.push_back(e->type);
			if (contextx == nullptr) {
				context->erase(x);
			} else {
				(*context)[x] = contextx;
			}
			break;
		}
		case 3: {
			e1->infer(context, cl);
			auto contextx = context->count(x) ? (*context)[x] : nullptr;
			(*context)[x] = gen(context, e1->type);
			e2->infer(context, cl);
			type = e2->type;
			if (contextx == nullptr) {
				context->erase(x);
			} else {
				(*context)[x] = contextx;
			}
			break;
		}
		case 4: {
			vector<shared_ptr<Mono> > taus_1;
			vector<shared_ptr<Poly> > contextx_1;
			for (int i = 0; i < xes.size(); i++) {
				if (context->count(xes[i].first)) {
					contextx_1.push_back((*context)[xes[i].first]);
				} else {
					contextx_1.push_back(nullptr);
				}
				taus_1.push_back(newvar());
				(*context)[xes[i].first] = make_shared<Poly>(
						Poly { 0, taus_1[i] });
			}
			for (int i = 0; i < xes.size(); i++) {
				xes[i].second->infer(context, cl);
				unify(xes[i].second->type, taus_1[i]);
			}
			for (int i = 0; i < xes.size(); i++) {
				if (contextx_1[i] == nullptr) {
					context->erase(xes[i].first);
				} else {
					(*context)[xes[i].first] = contextx_1[i];
				}
			}
			vector<shared_ptr<Poly> > taus_2;
			vector<shared_ptr<Poly> > contextx_2;
			for (int i = 0; i < xes.size(); i++) {
				taus_2.push_back(gen(context, xes[i].second->type));
			}
			for (int i = 0; i < xes.size(); i++) {
				if (context->count(xes[i].first)) {
					contextx_2.push_back((*context)[xes[i].first]);
				} else {
					contextx_2.push_back(nullptr);
				}
				(*context)[xes[i].first] = taus_2[i];
			}
			e->infer(context, cl);
			type = e->type;
			for (int i = 0; i < xes.size(); i++) {
				if (contextx_2[i] == nullptr) {
					context->erase(xes[i].first);
				} else {
					(*context)[xes[i].first] = contextx_2[i];
				}
			}
			break;
		}
		case 5: {
			e->infer(context, cl);
			type = newvar();
			for (int i = 0; i < pes.size(); i++) {
				//begin constructor check
				if (!cl.count(pes[i].first[0])
						|| cl[pes[i].first[0]] != pes[i].first.size() - 1) {
					cout << "ERROR!" << endl; //FIXME
					cout << pes[i].first[0]
							<< " data constructor not does exist or does not match"
							<< endl;
					return;
				}
				set<string> v;
				for (int j = 1; j < pes[i].first.size(); j++) {
					if (v.count(pes[i].first[j])) {
						cout << "ERROR!" << endl; //FIXME
						cout << pes[i].first[0] << " variable names conflict"
								<< endl;
						return;
					}
					v.insert(pes[i].first[j]);
				}
				//end constructor check
				auto tau = inst((*context)[pes[i].first[0]]);
				vector<shared_ptr<Mono> > taus_1;
				vector<shared_ptr<Poly> > contextx_1;
				for (int j = 1; j < pes[i].first.size(); j++) {
					if (context->count(pes[i].first[j])) {
						contextx_1.push_back((*context)[pes[i].first[j]]);
					} else {
						contextx_1.push_back(nullptr);
					}
					taus_1.push_back(newvar());
					auto t1 = make_shared<Mono>(), t2 = newvar();
					t1->T = 1;
					t1->D = "->";
					t1->tau.push_back(taus_1[j - 1]);
					t1->tau.push_back(t2);
					unify(t1, tau);
					tau = t2;
					(*context)[pes[i].first[j]] = make_shared<Poly>(Poly { 0,
							taus_1[j - 1] });
				}
				unify(e->type, tau);
				pes[i].second->infer(context, cl);
				unify(type, pes[i].second->type);
				for (int j = 1; j < pes[i].first.size(); j++) {
					if (contextx_1[j - 1] == nullptr) {
						context->erase(pes[i].first[j]);
					} else {
						(*context)[pes[i].first[j]] = contextx_1[j - 1];
					}
				}
			}
			break;
		}
		case 6: {
			type = newvar();
			break;
		}
		}
		if (sig != nullptr) {
			unify(type, inst(sig));
		}
	}

	string to_string() {
		switch (T) {
		case 0:
			return x;
		case 1:
			return e1->to_string() + " (" + e2->to_string() + ")";
		case 2:
			return "(\\" + x + "->" + e->to_string() + ")";
		case 3:
			return "(let " + x + " = " + e1->to_string() + " in "
					+ e2->to_string() + ")";
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
					s << pes[i].first[j]
							<< (j + 1 == pes[i].first.size() ? "" : " ");
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

	void codegen(ostream& s, map<string, int>& ci) {
		switch (T) {
		case 0:
			s << "$v_bsl_" << x;
			return;
		case 1:
			s << "(*((function<void*(void*)>*)(";
			e1->codegen(s, ci);
			s << ")))(";
			e2->codegen(s, ci);
			s << ")";
			return;
		case 2:
			s << "new function<void*(void*)>([=](void* " << "$v_bsl_" << x
					<< ") -> void* { return ";
			e->codegen(s, ci);
			s << "; })";
			return;
		case 3:
			s << "[=]() -> void* { void* " << "$v_bsl_" << x << " = ";
			e1->codegen(s, ci);
			s << "; return ";
			e2->codegen(s, ci);
			s << "; } ()";
			return;
		case 4:
//FIXME
//			s << "[=]() -> void* {";
//			s << " void* ";
//			for (int i = 0; i < xes.size(); i++) {
//				s << (i ? ", " : "") << "$v_bsl_" << xes[i].first << " = new ";
//				//TODO
//			}
//			for (int i = 0; i < xes.size(); i++) {
//				s << "{ void* $t_bsl_tmp = ";
//				xes[i].second->codegen(s, ci);
//				s << "; memcpy }";
//			}
//			s << " return ";
//			e2->codegen(s, ci);
//			s << "; } ()";
			return;
		case 5:
			s << "[=]() -> void* { void* $t_bsl_tmp = ";
			e->codegen(s, ci);
			s << "; switch ((($d_bsl_" << find(e->type)->D
					<< "*)($t_bsl_tmp))->T) {";
			for (int i = 0; i < pes.size(); i++) {
				s << "case " << ci[pes[i].first[0]] << ": {";
				for (int j = 1; j < pes[i].first.size(); j++) {
					s << "void* " << "$v_bsl_" << pes[i].first[j]
							<< " = (($d_bsl_" << pes[i].first[0]
							<< ")((($d_bsl_" << e->type->D
							<< "*)($t_bsl_tmp))->ptr))->$v_bsl_" << j << ";";
				}
				s << " return ";
				pes[i].second->codegen(s, ci);
				s << "; }";
			}
			s << "} }()";
			return;
		case 6:
			s << ffi;
			return;
		}
	}
};

struct Data {
	string name;
	vector<pair<string, shared_ptr<Poly> > > constructors;
};

pair<shared_ptr<map<string, shared_ptr<Data> > >, shared_ptr<Expr> > parse() {
	auto data = make_shared<map<string, shared_ptr<Data> > >();

//data bool where true::bool ; false::bool
	auto _bool = make_shared<Data>(Data { "bool" });
	_bool->constructors.push_back(make_pair("false", make_shared<Poly>(Poly { 0,
			make_shared<Mono>(Mono { 1, nullptr, "bool" }) })));
	_bool->constructors.push_back(make_pair("true", make_shared<Poly>(Poly { 0,
			make_shared<Mono>(Mono { 1, nullptr, "bool" }) })));
	(*data)[_bool->name] = _bool;
//data int where zero::int ; suc::int->int
	auto _int = make_shared<Data>(Data { "int" });
	_int->constructors.push_back(make_pair("zero", make_shared<Poly>(Poly { 0,
			make_shared<Mono>(Mono { 1, nullptr, "int" }) })));
	_int->constructors.push_back(make_pair("suc", make_shared<Poly>(Poly { 0,
			make_shared<Mono>(Mono { 1, nullptr, "->",
					vector<shared_ptr<Mono> > { make_shared<Mono>(Mono { 1,
							nullptr, "int" }), make_shared<Mono>(Mono { 1,
							nullptr, "int" }) } }) })));
	(*data)[_int->name] = _int;
//data term where int_term::int->term int ; bool_term::bool->term bool

//(\x->(case x of {false->true;true->false}))
	auto expr = make_shared<Expr>();
	expr->T = 2;
	expr->x = "x";
	auto d = expr->e = make_shared<Expr>();
	d->T = 5;
	d->e = make_shared<Expr>(Expr { 0, "x" });
	d->pes.push_back(
			make_pair(vector<string> { "false" }, make_shared<Expr>(Expr { 0,
					"true" })));
	d->pes.push_back(
			make_pair(vector<string> { "true" }, make_shared<Expr>(Expr { 0,
					"false" })));

	return make_pair(data, expr);
}

void codegen(
		pair<shared_ptr<map<string, shared_ptr<Data> > >, shared_ptr<Expr> > prog) {
	auto data = prog.first;
	auto expr = prog.second;

	stringstream d;
	map<string, int> cl, ci;
	for (auto dai : *data) {
		auto da = dai.second;
		d << "struct $d_bsl_" << da->name << " { int T; void* ptr; };" << endl;
		for (int i = 0; i < da->constructors.size(); i++) {
			auto c = da->constructors[i];
			d << "struct $d_bsl_" << c.first << " {";
			auto t1 = c.second;
			while (t1->T == 1) {
				t1 = t1->sigma;
			}
			auto t2 = t1->tau;
			int &j = cl[c.first];
			while (t2->T == 1 && t2->D == "->") {
				d << "void* " << "d" << j << ";";
				j++;
				t2 = t2->tau[1];
			}
			ci[c.first] = i;
			d << " };" << endl;
		}
	}
	cout << d.str() << endl;
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
				s << "$v_bsl_" << j;
				cur->T = 2;
				cur->x = s.str();
				cur->e = make_shared<Expr>();
				cur = cur->e;
			}
			stringstream s;
			s << "new " << "$d_bsl_" << da->name << " { " << i << ", new "
					<< "$d_bsl_" << c.first << "{";
			for (int j = 0; j < cl[c.first]; j++) {
				s << " $v_bsl_" << j << (j + 1 == cl[c.first] ? "" : " ");
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
	expr->infer(make_shared<map<string, shared_ptr<Poly> > >(), cl);
	cout << "#include <functional>" << endl << "using std::function;" << endl
			<< "//" << expr->to_string() << " :: " << to_string(expr->type)
			<< endl;
	expr->codegen(c, ci);
	cout << "int main() { " << c.str() << "; }" << endl;
}

int main() {
	codegen(parse());
}
