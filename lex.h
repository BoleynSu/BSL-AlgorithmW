#ifndef SU_BOLEYN_BSL_LEX_H
#define SU_BOLEYN_BSL_LEX_H

#include <iostream>
#include <string>
#include <queue>
#include <cstdio>

using namespace std;

struct Position {
	string fileNamespace, filename;
	int beginRow, beginColumn, endRow, endColumn;
};

enum TokenType {
	DATA, WHERE, FORALL, DOT, COLON, RIGHTARROW, SEMICOLON,

	IDENTIFIER, LAMBDA, LET, EQUAL, IN, REC, COMMA, CASE, OF, FFI,

	LEFT_PARENTHESIS, RIGHT_PARENTHESIS, LEFT_BRACE, RIGHT_BRACE,

	SPACE, ERROR, END
};

struct Token {
	TokenType token_type;
	string data;
	Position position;
};

struct Lexer {
	deque<Token> tokens;
	Lexer(istream& is) {
		Position position;
		position.endRow = 1;
		position.endColumn = 1;
		for (;;) {
			position.beginRow = position.endRow;
			position.beginColumn = position.endColumn;
			TokenType token_type;
			string data;
			char c = is.get();
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
				c = is.get();
				if (c != EOF) {
					if (c == '\n') {
						data.push_back(c);
						position.endColumn++;
					} else {
						is.putback(c);
					}
				}
				position.endRow++;
				position.endColumn = 1;
			} else if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
					|| c == '_' || c == '\'') {
				for (;;) {
					data.push_back(c);
					position.endColumn++;
					c = is.get();
					if (c == EOF) {
						break;
					}
					if (!(('0' <= c && c <= '9') || ('A' <= c && c <= 'Z')
							|| ('a' <= c && c <= 'z') || c == '_' || c == '\'')) {
						is.putback(c);
						break;
					}
				}
			} else {
				data.push_back(c);
				position.endColumn++;
				if (c == ':') {
					c = is.get();
					if (c != EOF) {
						if (c == ':') {
							data.push_back(c);
							position.endColumn++;
						} else {
							is.putback(c);
						}
					}
				} else if (c == '-') {
					c = is.get();
					if (c != EOF) {
						if (c == '>') {
							data.push_back(c);
							position.endColumn++;
						} else {
							is.putback(c);
						}
					}
				}
			}
			if (data == "data") {
				token_type = DATA;
			} else if (data == "where") {
				token_type = WHERE;
			} else if (data == "forall") {
				token_type = FORALL;
			} else if (data == ".") {
				token_type = DOT;
			} else if (data == ":") {
				token_type = COLON;
			} else if (data == "where") {
				token_type = WHERE;
			} else if (data == "->") {
				token_type = RIGHTARROW;
			} else if (data == ";") {
				token_type = SEMICOLON;
//			} else if (data == "identifier") {
//				token_type = IDENTIFIER;
			} else if (data == "\\") {
				token_type = LAMBDA;
			} else if (data == "let") {
				token_type = LET;
			} else if (data == "=") {
				token_type = EQUAL;
			} else if (data == "in") {
				token_type = IN;
			} else if (data == "rec") {
				token_type = REC;
			} else if (data == ",") {
				token_type = COMMA;
			} else if (data == "case") {
				token_type = CASE;
			} else if (data == "of") {
				token_type = OF;
			} else if (data == "ffi") {
				token_type = FFI;
				do {
					data.push_back(c);
					c = is.get();
					if (c == EOF) {
						break;
					} else if (c == '\n') {
						position.endRow++;
						position.endColumn = 1;
					} else if (c == '\r') {
						c = is.get();
						if (c != EOF) {
							if (c == '\n') {
								data.push_back(c);
							} else {
								is.putback(c);
							}
						}
						position.endRow++;
						position.endColumn = 1;
					} else {
						position.endColumn++;
					}
				} while (c == ' ' || c == '\t' || c == '\n' || c == '\r');
				if (c == EOF) {
					token_type = ERROR;
				} else {
					string sep;
					do {
						data.push_back(c);
						position.endColumn++;
						sep.push_back(c);
						c = is.get();
						if (c == EOF) {
							break;
						}
					} while (!(c == ' ' || c == '\t' || c == '\n' || c == '\r'));
					if (c == EOF) {
						token_type = ERROR;
					} else {
						for (;;) {
							data.push_back(c);
							if (data.substr(data.size() - sep.size(),
									sep.size()) == sep) {
								break;
							}
							c = is.get();
							if (c == EOF) {
								break;
							} else if (c == '\n') {
								position.endRow++;
								position.endColumn = 1;
							} else if (c == '\r') {
								c = is.get();
								if (c != EOF) {
									if (c == '\n') {
										data.push_back(c);
									} else {
										is.putback(c);
									}
								}
								position.endRow++;
								position.endColumn = 1;
							} else {
								position.endColumn++;
							}
						}
						if (c == EOF) {
							token_type = ERROR;
						}
					}
				}
			} else if (data == "(") {
				token_type = LEFT_PARENTHESIS;
			} else if (data == ")") {
				token_type = RIGHT_PARENTHESIS;
			} else if (data == "{") {
				token_type = LEFT_BRACE;
			} else if (data == "}") {
				token_type = RIGHT_BRACE;
			} else {
				c = data[0];
				if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_'
						|| c == '\'') {
					token_type = IDENTIFIER;
				} else if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
					token_type = SPACE;
				} else {
					token_type = ERROR;
				}
			}
			tokens.push_back( { token_type, data, position });
		}
		tokens.push_back( { END, "", position });
	}
	Token look_at(int i) {
		return tokens[i];
	}
	Token next() {
		Token token = tokens.front();
		tokens.pop_front();
		return token;
	}
};

#endif
