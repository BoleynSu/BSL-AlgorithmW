#ifndef SU_BOLEYN_BSL_TYPE_H
#define SU_BOLEYN_BSL_TYPE_H

#include <initializer_list>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "expr.h"

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
				cerr << "//" << "ERROR!" << endl; //FIXME
				cerr << "//" << to_string(a) << " ~ " << to_string(b) << endl;
			} else {
				a->alpha = b;
			}
		}
	} else if (b->T == 0) {
		if (a != b) {
			if (occ(b, a)) {
				cerr << "//" << "ERROR!" << endl; //FIXME
				cerr << "//" << to_string(b) << " ~ " << to_string(a) << endl;
			} else {
				b->alpha = a;
			}
		}
	} else {
		cerr << "//" << "ERROR!" << endl; //FIXME
		cerr << "//" << to_string(a) << " != " << to_string(b) << endl;
	}
}

void infer(shared_ptr<Expr> expr,
		shared_ptr<map<string, shared_ptr<Poly> > > context,
		map<string, int>& cl) {
	switch (expr->T) {
	case 0:
		if (context->count(expr->x)) {
			expr->type = inst((*context)[expr->x]);
		} else {
			cerr << "//" << "ERROR!" << endl; //FIXME
			cerr << "//" << expr->x << " not in context" << endl;
		}
		break;
	case 1: {
		infer(expr->e1, context, cl);
		infer(expr->e2, context, cl);
		expr->type = newvar();
		auto t = make_shared<Mono>();
		t->T = 1;
		t->D = "->";
		t->tau.push_back(expr->e2->type);
		t->tau.push_back(expr->type);
		unify(expr->e1->type, t);
		break;
	}
	case 2: {
		auto tau = newvar();
		auto contextx = context->count(expr->x) ? (*context)[expr->x] : nullptr;
		(*context)[expr->x] = make_shared<Poly>(Poly { 0, tau });
		infer(expr->e, context, cl);
		expr->type = make_shared<Mono>();
		expr->type->T = 1;
		expr->type->D = "->";
		expr->type->tau.push_back(tau);
		expr->type->tau.push_back(expr->e->type);
		if (contextx == nullptr) {
			context->erase(expr->x);
		} else {
			(*context)[expr->x] = contextx;
		}
		break;
	}
	case 3: {
		infer(expr->e1, context, cl);
		auto contextx = context->count(expr->x) ? (*context)[expr->x] : nullptr;
		(*context)[expr->x] = gen(context, expr->e1->type);
		cerr << "//" << expr->x << " :: " << to_string((*context)[expr->x])
				<< endl;
		infer(expr->e2, context, cl);
		expr->type = expr->e2->type;
		if (contextx == nullptr) {
			context->erase(expr->x);
		} else {
			(*context)[expr->x] = contextx;
		}
		break;
	}
	case 4: {
		//begin rec check
		set<string> v;
		for (int i = 0; i < expr->xes.size(); i++) {
			if (v.count(expr->xes[i].first)) {
				cerr << "//" << "ERROR!" << endl; //FIXME
				cerr << "//" << expr->xes[i].first << " variable names conflict"
						<< endl;
				return;
			}
			v.insert(expr->xes[i].first);
		}
		//end rec check
		vector<shared_ptr<Mono> > taus_1;
		vector<shared_ptr<Poly> > contextx_1;
		for (int i = 0; i < expr->xes.size(); i++) {
			if (context->count(expr->xes[i].first)) {
				contextx_1.push_back((*context)[expr->xes[i].first]);
			} else {
				contextx_1.push_back(nullptr);
			}
			taus_1.push_back(newvar());
			(*context)[expr->xes[i].first] = make_shared<Poly>(Poly { 0,
					taus_1[i] });
		}
		for (int i = 0; i < expr->xes.size(); i++) {
			infer(expr->xes[i].second, context, cl);
			unify(expr->xes[i].second->type, taus_1[i]);
		}
		for (int i = 0; i < expr->xes.size(); i++) {
			if (contextx_1[i] == nullptr) {
				context->erase(expr->xes[i].first);
			} else {
				(*context)[expr->xes[i].first] = contextx_1[i];
			}
		}
		vector<shared_ptr<Poly> > taus_2;
		vector<shared_ptr<Poly> > contextx_2;
		for (int i = 0; i < expr->xes.size(); i++) {
			taus_2.push_back(gen(context, expr->xes[i].second->type));
		}
		for (int i = 0; i < expr->xes.size(); i++) {
			if (context->count(expr->xes[i].first)) {
				contextx_2.push_back((*context)[expr->xes[i].first]);
			} else {
				contextx_2.push_back(nullptr);
			}
			(*context)[expr->xes[i].first] = taus_2[i];
			cerr << "//" << expr->xes[i].first << " :: "
					<< to_string((*context)[expr->xes[i].first]) << endl;
		}
		infer(expr->e, context, cl);
		expr->type = expr->e->type;
		for (int i = 0; i < expr->xes.size(); i++) {
			if (contextx_2[i] == nullptr) {
				context->erase(expr->xes[i].first);
			} else {
				(*context)[expr->xes[i].first] = contextx_2[i];
			}
		}
		break;
	}
	case 5: { //TODO FIXME
		//begin con check
		for (int i = 0; i < expr->pes.size(); i++) {
			if (!cl.count(expr->pes[i].first[0])
					|| cl[expr->pes[i].first[0]]
							!= expr->pes[i].first.size() - 1) {
				cerr << "//" << "ERROR!" << endl; //FIXME
				cerr << "//" << expr->pes[i].first[0]
						<< " data constructor not does exist or does not match"
						<< endl;
				return;
			}
			set<string> v;
			for (int j = 1; j < expr->pes[i].first.size(); j++) {
				if (v.count(expr->pes[i].first[j])) {
					cerr << "//" << "ERROR!" << endl; //FIXME
					cerr << "//" << expr->pes[i].first[0]
							<< " variable names conflict" << endl;
					return;
				}
				v.insert(expr->pes[i].first[j]);
			}
		}
		//end con check
		infer(expr->e, context, cl);
		expr->type = newvar();
		for (int i = 0; i < expr->pes.size(); i++) {
			auto tau = inst((*context)[expr->pes[i].first[0]]);
			vector<shared_ptr<Mono> > taus_1;
			vector<shared_ptr<Poly> > contextx_1;
			for (int j = 1; j < expr->pes[i].first.size(); j++) {
				if (context->count(expr->pes[i].first[j])) {
					contextx_1.push_back((*context)[expr->pes[i].first[j]]);
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
				(*context)[expr->pes[i].first[j]] = make_shared<Poly>(Poly { 0,
						taus_1[j - 1] });
			}
			unify(expr->e->type, tau);
			infer(expr->pes[i].second, context, cl);
			unify(expr->type, expr->pes[i].second->type);
			for (int j = 1; j < expr->pes[i].first.size(); j++) {
				if (contextx_1[j - 1] == nullptr) {
					context->erase(expr->pes[i].first[j]);
				} else {
					(*context)[expr->pes[i].first[j]] = contextx_1[j - 1];
				}
			}
		}
		break;
	}
	case 6: {
		expr->type = newvar();
		break;
	}
	}
	if (expr->sig != nullptr) {
		unify(expr->type, inst(expr->sig));
	}
}

#endif
