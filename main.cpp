#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
#include <set>

using namespace std;

struct mono {
	int T;
	shared_ptr<mono> alpha;
	string D;
	vector<shared_ptr<mono> > tau;
};

struct poly {
	int T;
	shared_ptr<mono> tau;
	shared_ptr<mono> alpha;
	shared_ptr<poly> sigma;
};

shared_ptr<mono> find(shared_ptr<mono> x) {
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

string to_string(shared_ptr<mono> tau) {
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

string to_string(shared_ptr<poly> sigma) {
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

shared_ptr<mono> newvar() {
	return make_shared<mono>(mono { 0 });
}

shared_ptr<mono> inst(shared_ptr<mono> tau,
		map<shared_ptr<mono>, shared_ptr<mono> >& m) {
	tau = find(tau);
	switch (tau->T) {
	case 0:
		if (m.count(tau)) {
			return m[tau];
		} else {
			return tau;
		}
	case 1: {
		auto t = make_shared<mono>();
		t->T = tau->T;
		t->D = tau->D;
		for (int i = 0; i < tau->tau.size(); i++) {
			t->tau.push_back(inst(tau->tau[i], m));
		}
		return t;
	}
	}
}

shared_ptr<mono> inst(shared_ptr<poly> sigma,
		map<shared_ptr<mono>, shared_ptr<mono> >& m) {
	switch (sigma->T) {
	case 0:
		return inst(sigma->tau, m);
	case 1:
		m[find(sigma->alpha)] = newvar();
		return inst(sigma->sigma, m);
	}
}

shared_ptr<mono> inst(shared_ptr<poly> sigma) {
	map<shared_ptr<mono>, shared_ptr<mono> > m;
	return inst(sigma, m);
}

void free(set<shared_ptr<mono> >& f, set<shared_ptr<mono> >& nf,
		shared_ptr<mono> tau) {
	tau = find(tau);
	switch (tau->T) {
	case 0:
		if (!nf.count(tau)) {
			f.insert(tau);
		}
		return;
	case 1:
		for (int i = 0; i < tau->tau.size(); i++) {
			free(f, nf, tau->tau[i]);
		}
		return;
	}
}

void free(set<shared_ptr<mono> >& f, set<shared_ptr<mono> >& nf,
		shared_ptr<poly> sigma) {
	switch (sigma->T) {
	case 0:
		free(f, nf, sigma->tau);
		return;
	case 1:
		nf.insert(find(sigma->alpha));
		free(f, nf, sigma->sigma);
		return;
	}
}

shared_ptr<poly> gen(shared_ptr<map<string, shared_ptr<poly> > > context,
		shared_ptr<mono> tau) {
	tau = find(tau);
	set<shared_ptr<mono> > f;
	for (auto c : *context) {
		set<shared_ptr<mono> > nf;
		free(f, nf, c.second);
	}
	set<shared_ptr<mono> > ms;
	free(ms, f, tau);
	map<shared_ptr<mono>, shared_ptr<mono> > m;
	for (auto f : ms) {
		m[f] = newvar();
	}
	auto g = make_shared<poly>(poly { 0, inst(tau, m) });
	for (auto f : m) {
		g = make_shared<poly>(poly { 1, nullptr, f.second, g });
	}
	return g;
}

bool occ(shared_ptr<mono> a, shared_ptr<mono> b) {
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

void unify(shared_ptr<mono> a, shared_ptr<mono> b) {
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

struct expr {
	int T;
	string x;
	shared_ptr<expr> e1, e2, e;
	shared_ptr<mono> type;
	vector<pair<string, shared_ptr<expr> > > es;

	void infer(shared_ptr<map<string, shared_ptr<poly> > > context) {
		switch (T) {
		case 0:
			if (context->count(x)) {
				type = inst((*context)[x]);
			} else {
				cout << "ERROR!" << endl; //FIXME
				cout << x << " not in context" << endl;
			}
			return;
		case 1: {
			e1->infer(context);
			e2->infer(context);
			type = newvar();
			auto t = make_shared<mono>();
			t->T = 1;
			t->D = "->";
			t->tau.push_back(e2->type);
			t->tau.push_back(type);
			unify(e1->type, t);
			return;
		}
		case 2: {
			auto tau = newvar();
			auto contextx = context->count(x) ? (*context)[x] : nullptr;
			(*context)[x] = make_shared<poly>(poly { 0, tau });
			e->infer(context);
			type = make_shared<mono>();
			type->T = 1;
			type->D = "->";
			type->tau.push_back(tau);
			type->tau.push_back(e->type);
			if (contextx == nullptr) {
				context->erase(x);
			} else {
				(*context)[x] = contextx;
			}
			return;
		}
		case 3: {
			e1->infer(context);
			auto contextx = context->count(x) ? (*context)[x] : nullptr;
			(*context)[x] = gen(context, e1->type);
			e2->infer(context);
			type = e2->type;
			if (contextx == nullptr) {
				context->erase(x);
			} else {
				(*context)[x] = contextx;
			}
		}
		case 4: {
			vector<shared_ptr<mono> > taus_1;
			map<string, shared_ptr<poly> > contextx_1;
			for (int i = 0; i < es.size(); i++) {
				if (context->count(es[i].first)) {
					contextx_1[es[i].first] = (*context)[es[i].first];
				} else {
					contextx_1[es[i].first] = nullptr;
				}
				taus_1.push_back(newvar());
				(*context)[es[i].first] = make_shared<poly>(
						poly { 0, taus_1[i] });
			}
			for (int i = 0; i < es.size(); i++) {
				es[i].second->infer(context);
				unify(es[i].second->type, taus_1[i]);
			}
			for (auto contextx : contextx_1) {
				if (contextx.second == nullptr) {
					context->erase(contextx.first);
				} else {
					(*context)[contextx.first] = contextx.second;
				}
			}
			vector<shared_ptr<poly> > taus_2;
			map<string, shared_ptr<poly> > contextx_2;
			for (int i = 0; i < es.size(); i++) {
				taus_2.push_back(gen(context, es[i].second->type));
			}
			for (int i = 0; i < es.size(); i++) {
				if (context->count(es[i].first)) {
					contextx_2[es[i].first] = (*context)[es[i].first];
				} else {
					contextx_2[es[i].first] = nullptr;
				}
				(*context)[es[i].first] = taus_2[i];
			}
			e->infer(context);
			type = e->type;
			for (auto contextx : contextx_2) {
				if (contextx.second == nullptr) {
					context->erase(contextx.first);
				} else {
					(*context)[contextx.first] = contextx.second;
				}
			}
		}
			return;
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
			for (int i = 0; i < es.size(); i++) {
				if (i) {
					s << " and ";
				} else {
					s << " ";
				}
				s << es[i].first << " = " << es[i].second->to_string();
			}
			s << " in " << e->to_string() << ")";
			return s.str();
		}
		}
	}
};

void test1() {
	auto id = make_shared<expr>();
	id->T = 2;
	id->x = "x";
	id->e = make_shared<expr>();
	id->e->T = 0;
	id->e->x = "x";
	auto v1 = newvar();
	auto v2 = newvar();
	auto pair = newvar();
	pair->T = 1;
	pair->D = "pair";
	pair->tau.push_back(v1);
	pair->tau.push_back(v2);
	auto tttt = newvar();
	tttt->T = 1;
	tttt->D = "->";
	tttt->tau.push_back(v2);
	tttt->tau.push_back(pair);
	auto pf = newvar();
	pf->T = 1;
	pf->D = "->";
	pf->tau.push_back(v1);
	pf->tau.push_back(tttt);
	auto context = make_shared<map<string, shared_ptr<poly> > >();
	(*context)["pair"] = gen(context, pf);
	auto ntype = make_shared<poly>();
	ntype->T = 0;
	ntype->tau = make_shared<mono>();
	ntype->tau->T = 1;
	ntype->tau->D = "int";
	(*context)["n"] = ntype;
	auto mtype = make_shared<poly>();
	mtype->T = 0;
	mtype->tau = make_shared<mono>();
	mtype->tau->T = 1;
	mtype->tau->D = "char";
	(*context)["m"] = mtype;
	auto let = make_shared<expr>();
	let->T = 3;
	let->x = "id";
	let->e1 = id;

	auto e2 = make_shared<expr>();
	e2->T = 1;
	e2->e1 = make_shared<expr>(expr { 0, "pair" });
	auto t = make_shared<expr>();
	t->T = 1;
	t->e1 = make_shared<expr>(expr { 0, "id" });
	t->e2 = make_shared<expr>(expr { 0, "n" });
	e2->e2 = t;
	auto e3 = make_shared<expr>();
	e3->T = 1;
	e3->e1 = e2;
	auto tt = make_shared<expr>();
	tt->T = 1;
	tt->e1 = make_shared<expr>(expr { 0, "id" });
	tt->e2 = make_shared<expr>(expr { 0, "m" });
	e3->e2 = tt;

//		cout<<id->to_string()<<endl;

	auto pr = make_shared<expr>();
	pr->T = 0;
	pr->x = "pair";
//		pr->infer(context);
//		cout<<"infered"<<endl;
//		cout<<to_string(pr->type)<<endl;
	{
		auto v1 = newvar();
		auto t = newvar();
		auto tt = newvar();
		tt->T = 1;
		tt->D = "Map";
		t->T = 1;
		t->D = "C";
		t->tau.push_back(tt);
		t->tau.push_back(v1);
		auto a = newvar();
		a->T = 1;
	}
	let->e2 = e3;
	let->infer(context);
	cout << "Context:" << endl;
	for (auto kv : *context) {
		cout << kv.first << " :: " << to_string(kv.second) << endl;
	}
	cout << "Result:" << endl;
	cout << let->to_string() << " :: " << to_string(let->type) << endl;
}

void test2() {
	auto context = make_shared<map<string, shared_ptr<poly> > >();
	auto w = make_shared<expr>();
	w->T = 2;
	w->x = "x";
	w->e = make_shared<expr>();
	w->e->T = 1;
	w->e->e1 = make_shared<expr>(expr { 0, "x" });
	w->e->e2 = make_shared<expr>(expr { 0, "x" });
	w->infer(context);
	cout << "Context:" << endl;
	for (auto kv : *context) {
		cout << kv.first << " :: " << to_string(kv.second) << endl;
	}
	cout << "Result:" << endl;
	cout << w->to_string() << " :: " << to_string(w->e->e1->type) << endl;
}

int main() {
	auto context = make_shared<map<string, shared_ptr<poly> > >();
	auto w = make_shared<expr>();
	w->T = 4;
	auto f = make_shared<expr>();
	f->T = 1;
	f->e1 = make_shared<expr>(expr { 0, "f" });
	f->e2 = make_shared<expr>(expr { 0, "x" });
	w->es.push_back(make_pair("x", f));
	w->e = make_shared<expr>();
	w->e->T = 0;
	w->e->x = "x";
	auto ww = make_shared<expr>();
	ww->T = 2;
	ww->x = "f";
	ww->e = w;
	ww->infer(context);
	cout << "Context:" << endl;
	for (auto kv : *context) {
		cout << kv.first << " :: " << to_string(kv.second) << endl;
	}
	cout << "Result:" << endl;
	cout << ww->to_string() << " :: " << to_string(gen(context, ww->type))
			<< endl;
}
