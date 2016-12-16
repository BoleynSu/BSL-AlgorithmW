#ifndef SU_BOLEYN_BSL_PARSE_H
#define SU_BOLEYN_BSL_PARSE_H

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "data.h"
#include "expr.h"
#include "lex.h"
#include "type.h"

using namespace std;

struct Parser {
  Lexer &lexer;
  Token t;
  shared_ptr<map<string, shared_ptr<Data>>> data_decl;
  shared_ptr<map<string, shared_ptr<Constructor>>> constructor_decl;

  Parser(Lexer &lexer) : lexer(lexer) {}

  bool match(TokenType token_type) {
    while (lexer.look_at(0).token_type == TokenType::HASHBANG ||
           lexer.look_at(0).token_type == TokenType::SPACE ||
           lexer.look_at(0).token_type == TokenType::COMMENT) {
      lexer.next();
    }
    t = lexer.look_at(0);
    return lexer.look_at(0).token_type == token_type;
  }

  bool accept(TokenType token_type) {
    if (match(token_type)) {
      lexer.next();
      return true;
    }
    return false;
  }

  void expect(TokenType token_type) {
    if (!accept(token_type)) {
      string data = lexer.look_at(0).data;
      if (data.length() > 78) {
        data = data.substr(0, 75) + "...";
      }
      cerr << "parser: " << lexer.look_at(0).position << " "
           << "expect " << token_type << " but get "
           << lexer.look_at(0).token_type << endl
           << "`" << data << "`" << endl;
      exit(EXIT_FAILURE);
    }
  }

  shared_ptr<Mono_> parse_monotype(map<string, shared_ptr<Mono_>> &m) {
    shared_ptr<Mono_> mo = parse_monotype_(m);
    if (accept(TokenType::RIGHTARROW)) {
      auto t = new_const_var("->");
      t->tau.push_back(to_poly(mo));
      t->tau.push_back(to_poly(parse_monotype(m)));
      mo = t;
    }
    return mo;
  }

  shared_ptr<Mono_> parse_monotype_(map<string, shared_ptr<Mono_>> &m) {
    shared_ptr<Mono_> mo;
    if (accept(TokenType::IDENTIFIER)) {
      if (m.count(t.data)) {
        mo = m[t.data];
      } else {
        mo = new_const_var(t.data);
      }
    } else {
      expect(TokenType::LEFT_PARENTHESIS);
      mo = parse_monotype(m);
      expect(TokenType::RIGHT_PARENTHESIS);
    }
    if (mo->is_const && mo->D != "->") {
      auto t1 = mo;
      while (match(TokenType::IDENTIFIER) ||
             match(TokenType::LEFT_PARENTHESIS)) {
        if (accept(TokenType::IDENTIFIER)) {
          if (m.count(t.data)) {
            mo = m[t.data];
          } else {
            mo = new_const_var(t.data);
          }
        } else {
          expect(TokenType::LEFT_PARENTHESIS);
          mo = parse_monotype(m);
          expect(TokenType::RIGHT_PARENTHESIS);
        }
        t1->tau.push_back(to_poly(mo));
      }
      mo = t1;
    }
    return mo;
  }

  shared_ptr<Poly_> parse_polytype(map<string, shared_ptr<Mono_>> &m) {
    if (accept(TokenType::FORALL)) {
      expect(TokenType::IDENTIFIER);
      if (m.count(t.data)) {
        string data = t.data;
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "parser: " << t.position << " type variable names conflict"
             << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      auto alpha = new_forall_var();
      m[t.data] = alpha;
      expect(TokenType::DOT);
      return to_poly(alpha, parse_polytype(m));
    } else {
      return to_poly(parse_monotype(m));
    }
  }

  shared_ptr<Constructor> parse_constructor() {
    auto c = make_shared<Constructor>();
    expect(TokenType::IDENTIFIER);
    if (constructor_decl->count(t.data)) {
      string data = t.data;
      if (data.length() > 78) {
        data = data.substr(0, 75) + "...";
      }
      cerr << "parser: " << t.position << " constructor names conflict" << endl
           << "`" << data << "`" << endl;
      exit(EXIT_FAILURE);
    }
    c->name = t.data;
    expect(TokenType::COLON);
    map<string, shared_ptr<Mono_>> m;
    c->type = parse_polytype(m);
    (*constructor_decl)[c->name] = c;
    auto tp = c->type;
    while (!tp->is_mono) {
      tp = tp->sigma;
    }
    auto tm = tp->tau;
    c->arg = 0;
    while (tm->is_const && tm->D == "->") {
      c->arg++;
      auto tp = tm->tau[1];
      while (!tp->is_mono) {
        tp = tp->sigma;
      }
      tm = tp->tau;
    }
    return c;
  }

  shared_ptr<Data> parse_data() {
    auto d = make_shared<Data>();
    expect(TokenType::IDENTIFIER);
    if (data_decl->count(t.data)) {
      string data = t.data;
      if (data.length() > 78) {
        data = data.substr(0, 75) + "...";
      }
      cerr << "parser: " << t.position << " type names conflict" << endl
           << "`" << data << "`" << endl;
      exit(EXIT_FAILURE);
    }
    d->name = t.data;
    d->arg = 0;
    set<string> st;
    while (!match(TokenType::LEFT_BRACE)) {
      expect(TokenType::IDENTIFIER);
      if (st.count(t.data)) {
        string data = t.data;
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "parser: " << t.position << " type variable names conflict"
             << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      st.insert(t.data);
      d->arg++;
    }
    expect(TokenType::LEFT_BRACE);
    while (!accept(TokenType::RIGHT_BRACE)) {
      d->constructors.push_back(parse_constructor());
      d->constructors.back()->data_name = d->name;
      if (!match(TokenType::RIGHT_BRACE)) {
        expect(TokenType::SEMICOLON);
      }
    }
    (*data_decl)[d->name] = d;
    return d;
  }

  shared_ptr<Expr> parse_expr() {
    auto expr = make_shared<Expr>();
    if (accept(TokenType::LAMBDA)) {
      expr->T = ExprType::ABS;
      expect(TokenType::IDENTIFIER);
      expr->x = t.data;
      expect(TokenType::RIGHTARROW);
      expr->e = parse_expr();
    } else if (accept(TokenType::LET)) {
      shared_ptr<Poly_> s;
      expr->T = ExprType::LET;
      expect(TokenType::IDENTIFIER);
      expr->x = t.data;
      if (accept(TokenType::COLON)) {
        map<string, shared_ptr<Mono_>> m;
        s = parse_polytype(m);
      }
      expect(TokenType::EQUAL);
      expr->e1 = parse_expr();
      expr->e1->sig = s;
      expect(TokenType::IN);
      expr->e2 = parse_expr();
    } else if (accept(TokenType::REC)) {
      expr->T = ExprType::REC;
      set<string> st;
      do {
        shared_ptr<Poly_> s;
        expect(TokenType::IDENTIFIER);
        if (st.count(t.data)) {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << t.position << " variable names conflict" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        st.insert(t.data);
        expr->xes.push_back(make_pair(t.data, nullptr));
        if (accept(TokenType::COLON)) {
          map<string, shared_ptr<Mono_>> m;
          s = parse_polytype(m);
        }
        expect(TokenType::EQUAL);
        expr->xes.back().second = parse_expr();
        expr->xes.back().second->sig = s;
      } while (accept(TokenType::AND));
      expect(TokenType::IN);
      expr->e = parse_expr();
    } else {
      expr = parse_expr_();
      if (match(TokenType::LAMBDA) || match(TokenType::LET) ||
          match(TokenType::REC)) {
        auto e1 = parse_expr();
        auto e2 = make_shared<Expr>();
        e2->T = ExprType::APP;
        e2->e1 = expr;
        e2->e2 = e1;
        expr = e2;
      }
    }
    return expr;
  }

  shared_ptr<Expr> parse_expr_() {
    auto expr = parse_expr__();
    while (match(TokenType::IDENTIFIER) || match(TokenType::LEFT_PARENTHESIS) ||
           match(TokenType::CASE) || match(TokenType::FFI)) {
      auto t1 = expr;
      expr = parse_expr__();
      auto t2 = make_shared<Expr>();
      t2->T = ExprType::APP;
      t2->e1 = t1;
      t2->e2 = expr;
      expr = t2;
    }
    return expr;
  }
  shared_ptr<Expr> parse_expr__() {
    auto expr = make_shared<Expr>();
    if (accept(TokenType::IDENTIFIER)) {
      expr->T = ExprType::VAR;
      expr->x = t.data;
    } else if (accept(TokenType::LEFT_PARENTHESIS)) {
      expr = parse_expr();
      expect(TokenType::RIGHT_PARENTHESIS);
    } else if (accept(TokenType::CASE)) {
      expr->T = ExprType::CASE;
      expr->e = parse_expr();
      expect(TokenType::OF);
      shared_ptr<Poly_> g;
      if (accept(TokenType::COLON)) {
        map<string, shared_ptr<Mono_>> m;
        g = parse_polytype(m);
      }
      expr->gadt = g;
      expect(TokenType::LEFT_BRACE);
      string data_name;
      do {
        expect(TokenType::IDENTIFIER);
        if (!constructor_decl->count(t.data)) {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << t.position << " constructor not found" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        if (expr->pes.count(t.data)) {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << t.position << " constructors conflict" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        string cname = t.data;
        expr->pes[cname] = make_pair(vector<string>{}, nullptr);
        auto &pes = expr->pes[cname];
        auto c = (*constructor_decl)[cname];
        if (data_name.empty()) {
          data_name = c->data_name;
        } else if (data_name != c->data_name) {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << t.position << " constructor of " << data_name
               << " expected, but found" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        set<string> st;
        for (size_t i = 0; i < c->arg; i++) {
          expect(TokenType::IDENTIFIER);
          if (st.count(t.data)) {
            string data = t.data;
            if (data.length() > 78) {
              data = data.substr(0, 75) + "...";
            }
            cerr << "parser: " << t.position << " variable names conflict"
                 << endl
                 << "`" << data << "`" << endl;
            exit(EXIT_FAILURE);
          }
          st.insert(t.data);
          pes.first.push_back(t.data);
        }
        expect(TokenType::RIGHTARROW);
        pes.second = parse_expr();
        if (!match(TokenType::RIGHT_BRACE)) {
          expect(TokenType::SEMICOLON);
        }
      } while (!match(TokenType::RIGHT_BRACE));
      expect(TokenType::RIGHT_BRACE);
    } else if (accept(TokenType::FFI)) {
      expr->T = ExprType::FFI;
      stringstream s(t.data);
      s.get();
      s.get();
      s.get();
      string sep;
      s >> sep;
      size_t a = t.data.find(sep);
      expr->ffi =
          t.data.substr(a + sep.size(), t.data.size() - (a + 2 * sep.size()));
    }
    return expr;
  }
  void check(shared_ptr<Constructor> c, shared_ptr<Mono_> p,
             set<shared_ptr<Mono_>> &st, bool m, bool r) {
    if (p->is_const) {
      if (p->D == "->") {
        assert(p->tau.size() == 2);
        {
          auto tp = p->tau[0];
          while (!tp->is_mono) {
            tp = tp->sigma;
          }
          check(c, tp->tau, st, m && false, r);
        }
        {
          auto tp = p->tau[1];
          while (!tp->is_mono) {
            tp = tp->sigma;
          }
          check(c, tp->tau, st, m && true, r);
        }
      } else {
        if (data_decl->count(p->D)) {
          if (m) {
            if (p->D != c->data_name) {
              cerr << "parser: in constructor " << c->name << ":"
                   << to_string(c->type) << endl
                   << "return type is " << p->D << " instead of "
                   << c->data_name << endl;
              exit(EXIT_FAILURE);
            }
          }
          if (p->tau.size() != (*data_decl)[p->D]->arg) {
            cerr << "parser: in constructor " << c->name << ":"
                 << to_string(c->type) << endl
                 << p->D << " expects " << (*data_decl)[p->D]->arg
                 << " arguments, but only gets " << p->tau.size() << endl;
            exit(EXIT_FAILURE);
          }
          for (size_t i = 0; i < p->tau.size(); i++) {
            auto tp = p->tau[i];
            while (!tp->is_mono) {
              tp = tp->sigma;
            }
            check(c, tp->tau, st, false, m || r);
          }
          if (m) {
            if (!st.empty()) {
              cerr << "parser: in constructor " << c->name << ":"
                   << to_string(c->type) << endl
                   << "existential type is not supported yet" << endl;
              exit(EXIT_FAILURE);
            }
          }
        } else {
          cerr << "parser: in constructor " << c->name << ":"
               << to_string(c->type) << endl
               << p->D << " is not a type" << endl;
          exit(EXIT_FAILURE);
        }
      }
    } else {
      if (m) {
        cerr << "parser: in constructor " << c->name << ":"
             << to_string(c->type) << endl
             << "return type is a type variable instead of " << c->data_name
             << endl;
        exit(EXIT_FAILURE);
      }
      if (r) {
        st.erase(p);
      }
    }
  }
  pair<pair<shared_ptr<map<string, shared_ptr<Data>>>,
            shared_ptr<map<string, shared_ptr<Constructor>>>>,
       shared_ptr<Expr>>
  parse() {
    data_decl = make_shared<map<string, shared_ptr<Data>>>();
    constructor_decl = make_shared<map<string, shared_ptr<Constructor>>>();
    while (accept(TokenType::DATA)) {
      parse_data();
    }
    for (auto &c : *constructor_decl) {
      shared_ptr<Poly_> p = c.second->type;
      set<shared_ptr<Mono_>> st;
      while (!p->is_mono) {
        st.insert(p->alpha);
        p = p->sigma;
      }
      check(c.second, p->tau, st, true, false);
    }
    auto expr = parse_expr();
    expect(TokenType::END);
    return make_pair(make_pair(data_decl, constructor_decl), expr);
  }
};

#endif
