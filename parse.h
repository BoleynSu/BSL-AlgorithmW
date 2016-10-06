#ifndef SU_BOLEYN_BSL_PARSE_H
#define SU_BOLEYN_BSL_PARSE_H

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "data.h"
#include "expr.h"
#include "lex.h"
#include "type.h"

using namespace std;

struct Parser {
  Lexer lexer;
  Token t;

  Parser(istream& in) : lexer(in) {}

  bool match(TokenType token_type) {
    while (lexer.look_at(0).token_type == SPACE) {
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
      cerr << "ERROR!" << endl;  // FIXME
      cerr << lexer.look_at(0).position.beginRow << ","
           << lexer.look_at(0).position.beginColumn << ":"
           << "expect " << token_type << " but get "
           << lexer.look_at(0).token_type << " (" << lexer.look_at(0).data
           << ")" << endl;
    }
  }

  shared_ptr<Mono> parse_monotype(map<string, shared_ptr<Mono> >& m) {  // FIXME
    shared_ptr<Mono> mo = parse_monotype_(m);
    if (accept(RIGHTARROW)) {
      auto t = make_shared<Mono>();
      t->T = 1;
      t->D = "->";
      t->tau.push_back(mo);
      t->tau.push_back(parse_monotype(m));
      mo = t;
    }
    return mo;
  }

  shared_ptr<Mono> parse_monotype_(
      map<string, shared_ptr<Mono> >& m) {  // FIXME
    shared_ptr<Mono> mo;
    if (accept(IDENTIFIER)) {
      if (m.count(t.data)) {
        mo = m[t.data];
      } else {
        mo = make_shared<Mono>();
        mo->T = 1;
        mo->D = t.data;
      }
    } else {
      expect(LEFT_PARENTHESIS);
      mo = parse_monotype(m);
      expect(RIGHT_PARENTHESIS);
    }
    if (mo->T == 1 && mo->D != "->") {
      auto t1 = mo;
      while (match(IDENTIFIER) || match(LEFT_PARENTHESIS)) {
        if (accept(IDENTIFIER)) {
          if (m.count(t.data)) {
            mo = m[t.data];
          } else {
            mo = make_shared<Mono>();
            mo->T = 1;
            mo->D = t.data;
          }
        } else {
          expect(LEFT_PARENTHESIS);
          mo = parse_monotype(m);
          expect(RIGHT_PARENTHESIS);
        }
        t1->tau.push_back(mo);
      }
      mo = t1;
    }
    return mo;
  }

  shared_ptr<Poly> parse_polytype(map<string, shared_ptr<Mono> >& m) {  // FIXME
    if (accept(FORALL)) {
      expect(IDENTIFIER);
      auto alpha = m[t.data] = newvar();
      expect(DOT);
      return make_shared<Poly>(Poly{1, nullptr, alpha, parse_polytype(m)});
    } else {
      return make_shared<Poly>(Poly{0, parse_monotype(m)});
    }
  }

  pair<string, shared_ptr<Poly> > parse_constructor() {  // FIXME
    pair<string, shared_ptr<Poly> > c;
    expect(IDENTIFIER);
    c.first = t.data;
    expect(COLON);
    map<string, shared_ptr<Mono> > m;
    c.second = parse_polytype(m);
    return c;
  }

  shared_ptr<Data> parse_data() {  // FIXME
    auto d = make_shared<Data>();
    expect(IDENTIFIER);
    d->name = t.data;
    while (!accept(WHERE)) {
      expect(IDENTIFIER);
    }
    if (accept(LEFT_BRACE)) {
      d->T = 0;
      while (!accept(RIGHT_BRACE)) {
        d->constructors.push_back(parse_constructor());
        accept(SEMICOLON);
      }
    } else {
      expect(FFI);
      d->T = 1;
      stringstream s(t.data);
      s.get();
      s.get();
      s.get();
      string sep;
      s >> sep;
      int a = t.data.find(sep);
      d->ffi =
          t.data.substr(a + sep.size(), t.data.size() - (a + 2 * sep.size()));
    }
    return d;
  }

  shared_ptr<Expr> parse_expr() {
    auto expr = make_shared<Expr>();
    if (accept(LAMBDA)) {
      expr->T = 2;
      expect(IDENTIFIER);
      expr->x = t.data;
      expect(RIGHTARROW);
      expr->e = parse_expr();
    } else if (accept(LET)) {
      shared_ptr<Poly> s;
      expr->T = 3;
      expect(IDENTIFIER);
      expr->x = t.data;
      if (accept(COLON)) {
        map<string, shared_ptr<Mono> > m;
        s = parse_polytype(m);
      }
      expect(EQUAL);
      expr->e1 = parse_expr();
      expr->e1->sig = s;
      expect(IN);
      expr->e2 = parse_expr();
    } else if (accept(REC)) {
      expr->T = 4;
      do {
        shared_ptr<Poly> s;
        expect(IDENTIFIER);
        expr->xes.push_back(make_pair(t.data, nullptr));
        if (accept(COLON)) {
          map<string, shared_ptr<Mono> > m;
          s = parse_polytype(m);
        }
        expect(EQUAL);
        expr->xes.back().second = parse_expr();
        expr->xes.back().second->sig = s;
      } while (accept(AND));
      expect(IN);
      expr->e = parse_expr();
    } else {
      expr = parse_expr_();
      if (match(LAMBDA) || match(LET) || match(REC)) {
        auto e1 = parse_expr();
        auto e2 = make_shared<Expr>();
        e2->T = 1;
        e2->e1 = expr;
        e2->e2 = e1;
        expr = e2;
      }
    }
    return expr;
  }

  shared_ptr<Expr> parse_expr_() {
    auto expr = make_shared<Expr>();
    if (accept(IDENTIFIER)) {
      expr->T = 0;
      expr->x = t.data;
    } else if (accept(LEFT_PARENTHESIS)) {
      expr = parse_expr();
      expect(RIGHT_PARENTHESIS);
    } else if (accept(CASE)) {
      expr->T = 5;
      expr->e = parse_expr();
      expect(OF);
      expect(LEFT_BRACE);
      while (!match(RIGHT_BRACE)) {
        expr->pes.push_back(make_pair(vector<string>{}, nullptr));
        do {
          expect(IDENTIFIER);
          expr->pes.back().first.push_back(t.data);
        } while (!accept(RIGHTARROW));
        expr->pes.back().second = parse_expr();
        accept(SEMICOLON);
      }
      expect(RIGHT_BRACE);
    } else if (accept(FFI)) {
      expr->T = 6;
      stringstream s(t.data);
      s.get();
      s.get();
      s.get();
      string sep;
      s >> sep;
      int a = t.data.find(sep);
      expr->ffi =
          t.data.substr(a + sep.size(), t.data.size() - (a + 2 * sep.size()));
    }
    while (match(IDENTIFIER) || match(LEFT_PARENTHESIS) || match(CASE) ||
           match(FFI)) {
      auto t1 = expr;
      expr = make_shared<Expr>();
      if (accept(IDENTIFIER)) {
        expr->T = 0;
        expr->x = t.data;
      } else if (accept(LEFT_PARENTHESIS)) {
        expr = parse_expr();
        expect(RIGHT_PARENTHESIS);
      } else if (accept(CASE)) {
        expr->T = 5;
        expr->e = parse_expr();
        expect(OF);
        expect(LEFT_BRACE);
        while (!match(RIGHT_BRACE)) {
          expr->pes.push_back(make_pair(vector<string>{}, nullptr));
          do {
            expect(IDENTIFIER);
            expr->pes.back().first.push_back(t.data);
          } while (!accept(RIGHTARROW));
          expr->pes.back().second = parse_expr();
          accept(SEMICOLON);
        }
        expect(RIGHT_BRACE);
      } else if (accept(FFI)) {
        expr->T = 6;
        stringstream s(t.data);
        s.get();
        s.get();
        s.get();
        string sep;
        s >> sep;
        int a = t.data.find(sep);
        expr->ffi =
            t.data.substr(a + sep.size(), t.data.size() - (a + 2 * sep.size()));
      }
      auto t2 = make_shared<Expr>();
      t2->T = 1;
      t2->e1 = t1;
      t2->e2 = expr;
      expr = t2;
    }
    return expr;
  }

  pair<shared_ptr<map<string, shared_ptr<Data> > >, shared_ptr<Expr> > parse() {
    auto data = make_shared<map<string, shared_ptr<Data> > >();
    while (accept(DATA)) {
      auto d = parse_data();
      (*data)[d->name] = d;
    }
    auto expr = parse_expr();
    expect(END);
    return make_pair(data, expr);
  }
};

#endif
