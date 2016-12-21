#ifndef SU_BOLEYN_BSL_LEX_H
#define SU_BOLEYN_BSL_LEX_H

#include <cstdio>
#include <cstdlib>
#include <deque>
#include <fstream>
#include <iostream>
#include <string>

using namespace std;

enum class TokenType {
  HASHBANG,

  DATA,
  FORALL,
  DOT,
  COLON,
  RIGHTARROW,
  SEMICOLON,

  LAMBDA,
  LET,
  EQUAL,
  IN,
  REC,
  AND,
  CASE,
  OF,
  FFI,

  LEFT_PARENTHESIS,
  RIGHT_PARENTHESIS,
  LEFT_BRACE,
  RIGHT_BRACE,

  IDENTIFIER,

  SPACE,
  COMMENT,
  END,

  ERROR
};
ostream &operator<<(ostream &out, TokenType t) {
  switch (t) {
    case TokenType::HASHBANG:
      out << "HASHBANG";
      break;
    case TokenType::DATA:
      out << "DATA";
      break;
    case TokenType::FORALL:
      out << "FORALL";
      break;
    case TokenType::DOT:
      out << "DOT";
      break;
    case TokenType::COLON:
      out << "COLON";
      break;
    case TokenType::RIGHTARROW:
      out << "RIGHTARROW";
      break;
    case TokenType::SEMICOLON:
      out << "SEMICOLON";
      break;
    case TokenType::LAMBDA:
      out << "LAMBDA";
      break;
    case TokenType::LET:
      out << "LET";
      break;
    case TokenType::EQUAL:
      out << "EQUAL";
      break;
    case TokenType::IN:
      out << "IN";
      break;
    case TokenType::REC:
      out << "REC";
      break;
    case TokenType::AND:
      out << "AND";
      break;
    case TokenType::CASE:
      out << "CASE";
      break;
    case TokenType::OF:
      out << "OF";
      break;
    case TokenType::FFI:
      out << "FFI";
      break;
    case TokenType::LEFT_PARENTHESIS:
      out << "LEFT_PARENTHESIS";
      break;
    case TokenType::RIGHT_PARENTHESIS:
      out << "RIGHT_PARENTHESIS";
      break;
    case TokenType::LEFT_BRACE:
      out << "LEFT_BRACE";
      break;
    case TokenType::RIGHT_BRACE:
      out << "RIGHT_BRACE";
      break;
    case TokenType::IDENTIFIER:
      out << "IDENTIFIER";
      break;
    case TokenType::SPACE:
      out << "SPACE";
      break;
    case TokenType::COMMENT:
      out << "COMMENT";
      break;
    case TokenType::END:
      out << "END";
      break;
    case TokenType::ERROR:
      out << "ERROR";
      break;
  }
  return out;
}

struct Token {
  TokenType token_type;
  string data;
  Position position;
};

struct Lexer {
  string filename;
  istream &in;
  deque<Token> tokens;
  Lexer(const string &filename, istream &in) : filename(filename), in(in) {
    Position position;
    position.filename = filename;
    position.endRow = 1;
    position.endColumn = 1;
    for (;;) {
      position.beginRow = position.endRow;
      position.beginColumn = position.endColumn;
      TokenType token_type;
      string data;
      char c = in.get();
      if (c == EOF) {
        break;
      } else if (c == ' ' || c == '\t') {
        data.push_back(c);
        position.endColumn++;
      } else if (c == '\n') {
        data.push_back(c);
        position.endColumn++;
        position.endRow++;
        position.endColumn = 1;
      } else if (c == '\r') {
        data.push_back(c);
        position.endColumn++;
        c = in.get();
        if (c != EOF) {
          if (c == '\n') {
            data.push_back(c);
            position.endColumn++;
          } else {
            in.putback(c);
          }
        }
        position.endRow++;
        position.endColumn = 1;
      } else if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_') {
        for (;;) {
          data.push_back(c);
          position.endColumn++;
          c = in.get();
          if (c == EOF) {
            break;
          }
          if (!(('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') ||
                ('a' <= c && c <= 'z') || c == '_' || c == '\'')) {
            in.putback(c);
            break;
          }
        }
      } else {
        data.push_back(c);
        position.endColumn++;
        if (c == '-') {
          c = in.get();
          if (c != EOF) {
            if (c == '>') {  //"->"
              data.push_back(c);
              position.endColumn++;
            } else if (c == '-') {  //"--"
              data.push_back(c);
              position.endColumn++;
            } else {
              in.putback(c);
            }
          }
        } else if (c == '#') {
          if (position.beginRow == 1 && position.beginColumn == 1) {
            c = in.get();
            if (c != EOF) {
              if (c == '!') {  //"#!"
                data.push_back(c);
                position.endColumn++;
              } else {
                in.putback(c);
              }
            }
          }
        } else if (c == '{') {
          c = in.get();
          if (c != EOF) {
            if (c == '-') {  //"{-"
              data.push_back(c);
              position.endColumn++;
            } else {
              in.putback(c);
            }
          }
        }
      }
      if (data == "#!") {
        for (;;) {
          c = in.get();
          if (c != EOF) {
            if (c == '\n' || c == '\r') {
              in.putback(c);
              break;
            } else {
              data.push_back(c);
              position.endColumn++;
            }
          } else {
            break;
          }
        }
        token_type = TokenType::HASHBANG;
      } else if (data == "data") {
        token_type = TokenType::DATA;
      } else if (data == "forall") {
        token_type = TokenType::FORALL;
      } else if (data == ".") {
        token_type = TokenType::DOT;
      } else if (data == ":") {
        token_type = TokenType::COLON;
      } else if (data == "->") {
        token_type = TokenType::RIGHTARROW;
      } else if (data == ";") {
        token_type = TokenType::SEMICOLON;
      } else if (data == "\\") {
        token_type = TokenType::LAMBDA;
      } else if (data == "let") {
        token_type = TokenType::LET;
      } else if (data == "=") {
        token_type = TokenType::EQUAL;
      } else if (data == "in") {
        token_type = TokenType::IN;
      } else if (data == "rec") {
        token_type = TokenType::REC;
      } else if (data == "and") {
        token_type = TokenType::AND;
      } else if (data == "case") {
        token_type = TokenType::CASE;
      } else if (data == "of") {
        token_type = TokenType::OF;
      } else if (data == "ffi") {
        token_type = TokenType::FFI;
        do {
          data.push_back(c);
          c = in.get();
          if (c == EOF) {
            break;
          } else if (c == '\n') {
            position.endRow++;
            position.endColumn = 1;
          } else if (c == '\r') {
            c = in.get();
            if (c != EOF) {
              if (c == '\n') {
                data.push_back(c);
              } else {
                in.putback(c);
              }
            }
            position.endRow++;
            position.endColumn = 1;
          } else {
            position.endColumn++;
          }
        } while (c == ' ' || c == '\t' || c == '\n' || c == '\r');
        if (c == EOF) {
          token_type = TokenType::ERROR;
        } else {
          string sep;
          do {
            data.push_back(c);
            position.endColumn++;
            sep.push_back(c);
            c = in.get();
            if (c == EOF) {
              break;
            }
          } while (!(c == ' ' || c == '\t' || c == '\n' || c == '\r'));
          if (c == EOF) {
            token_type = TokenType::ERROR;
          } else {
            for (;;) {
              data.push_back(c);
              if (data.substr(data.size() - sep.size(), sep.size()) == sep) {
                break;
              }
              c = in.get();
              if (c == EOF) {
                break;
              } else if (c == '\n') {
                position.endRow++;
                position.endColumn = 1;
              } else if (c == '\r') {
                c = in.get();
                if (c != EOF) {
                  if (c == '\n') {
                    data.push_back(c);
                  } else {
                    in.putback(c);
                  }
                }
                position.endRow++;
                position.endColumn = 1;
              } else {
                position.endColumn++;
              }
            }
            if (c == EOF) {
              token_type = TokenType::ERROR;
            }
          }
        }
      } else if (data == "(") {
        token_type = TokenType::LEFT_PARENTHESIS;
      } else if (data == ")") {
        token_type = TokenType::RIGHT_PARENTHESIS;
      } else if (data == "{") {
        token_type = TokenType::LEFT_BRACE;
      } else if (data == "}") {
        token_type = TokenType::RIGHT_BRACE;
      } else if (data == "--") {
        for (;;) {
          c = in.get();
          if (c != EOF) {
            if (c == '\n' || c == '\r') {
              in.putback(c);
              break;
            } else {
              data.push_back(c);
              position.endColumn++;
            }
          } else {
            break;
          }
        }
        token_type = TokenType::COMMENT;
      } else if (data == "{-") {
        token_type = TokenType::COMMENT;
        for (;;) {
          c = in.get();
          if (c != EOF) {
            if (c == '-') {
              data.push_back(c);
              position.endColumn++;
              c = in.get();
              if (c != EOF) {
                if (c == '}') {  //"-}"
                  data.push_back(c);
                  position.endColumn++;
                  break;
                } else {
                  in.putback(c);
                }
              } else {
                break;
              }
            } else {
              data.push_back(c);
              if (c == '\n') {
                position.endRow++;
                position.endColumn = 1;
              } else if (c == '\r') {
                c = in.get();
                if (c != EOF) {
                  if (c == '\n') {
                    data.push_back(c);
                  } else {
                    in.putback(c);
                  }
                }
                position.endRow++;
                position.endColumn = 1;
              } else {
                position.endColumn++;
              }
              position.endColumn++;
            }
          } else {
            break;
          }
        }
        if (c == EOF) {
          token_type = TokenType::ERROR;
        }
      } else {
        c = data[0];
        if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_') {
          token_type = TokenType::IDENTIFIER;
        } else if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
          token_type = TokenType::SPACE;
        } else {
          token_type = TokenType::ERROR;
        }
      }
      if (token_type == TokenType::ERROR) {
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "lexer: " << to_string(position) << " token not recognized"
             << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      tokens.push_back({token_type, data, position});
    }
    tokens.push_back({TokenType::END, "", position});
  }
  Token look_at(size_t i) { return tokens[i]; }
  Token next() {
    Token token = tokens.front();
    tokens.pop_front();
    return token;
  }
};

#endif
