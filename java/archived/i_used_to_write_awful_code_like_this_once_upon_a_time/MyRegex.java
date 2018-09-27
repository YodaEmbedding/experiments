package com.mulhaq.Experimentation1;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * Created by Mateen on 2015-12-30.
 */
public class MyRegex {
	String str;
	String pattern;
	int cursor;
	List<Token> tokens;

	MyRegex(String str, String pattern) {
		this.str = str;
		this.pattern = pattern;

		tokenize();
	}

	private void tokenize() {
		tokens = new ArrayList<>();

		for (int pos = 0; pos < pattern.length(); pos++) {
			Token token = new Token();

			switch (pattern.charAt(pos)) {
				case '\\':
					// Check second character
					if (pos + 1 < pattern.length()) {
						token.str = pattern.substring(pos, pos + 2);

						if (pattern.charAt(pos + 1) == 'w') {
							token.type = TokenType.WORDCHAR;
							pos++;
						} else if (pattern.charAt(pos + 1) == 'd') {
							token.type = TokenType.NUMCHAR;
							pos++;
						} else {
							token.type = TokenType.OTHER;
						}
					} else {
						token.str = pattern.substring(pos, pos + 1);
						token.type = TokenType.OTHER;
					}
					break;

				case '.':
					token.str = ".";
					token.type = TokenType.ANYCHAR;
					break;

				case '*':
					// Check second character
					if (pos + 1 < pattern.length() &&
						pattern.charAt(pos + 1) == '?') {
						token.str = "*?";
						token.type = TokenType.QASTERISK;
						pos++;
					} else {
						token.str = "*";
						token.type = TokenType.ASTERISK;
					}
					break;

				case '+':
					// Check second character
					if (pos + 1 < pattern.length() &&
						pattern.charAt(pos + 1) == '?') {
						token.str = "+?";
						token.type = TokenType.QPLUS;
						pos++;
					} else {
						token.str = "+";
						token.type = TokenType.PLUS;
					}
					break;

				case '?':
					token.str = "?";
					token.type = TokenType.QUESTION;
					break;

				default:
					token.str = pattern.substring(pos, pos + 1);
					token.type = TokenType.CHAR;
					break;
			}

			tokens.add(token);
		}
	}

	private Position matchAtIndex(int idx, ListIterator<Token> it, boolean ignoreNext) {
		int pos = idx;

		if (it == null) {
			it = tokens.listIterator();
		}

		while (it.hasNext()) {
			ListIterator<Token> it_prev = it;
			Token curr = it.next();
			Token next = it.hasNext() ? it.next() : null;
			if (next != null) it.previous();

			switch (next.type) {
				case QUESTION:
					// FIXME: 2015-12-31
					// This doesn't take into account more complex regexes...
					// Like .*?? or even a??
					// Nor does \w? work
					// May need recursion matchAtIdx(idx, it)
					// More functions (isW())...?
					// ignoreNext?
					if (curr.str.charAt(0) == str.charAt(pos))
						pos++;
					break;

				case ASTERISK:
					Position p = matchAtIndex(pos, it_prev, true);
					// FIXME: 2016-01-01 
					// pos =;

					break;

				case QASTERISK:
					break;

				case PLUS:
					break;

				case QPLUS:
					break;

				default:
					if (curr.str.charAt(0) == str.charAt(pos))
						pos++;
					else
						return new Position(0, 0);
					break;
			}
		}

		return new Position(idx, pos - idx);
	}

	public Position match() {
		for (int i = 0; i < str.length(); i++) {
			// FIXME: 2016-01-01 What does ignoreNext even do...
			Position p = matchAtIndex(i, null, false);

			if (p.length > 0)
				return p;
		}

		return new Position(0, 0);
	}

	public String substr(Position position) {
		return position.substr(str);
	}

	enum TokenType {
		OTHER,
		CHAR,       // typical character
		ANYCHAR,    // .
		WORDCHAR,   // \w
		NUMCHAR,    // \d
		QUESTION,   // ?
		ASTERISK,   // *
		QASTERISK,  // *?
		PLUS,       // +
		QPLUS,      // +?
		// Brackets need tree... list of lists?
	}

	public class Token {
		public String str;
		TokenType type;
		// List<Token> children; // children of TokenNode...?
		// recursive calls?
	}

	// Information on position of string
	// FIXME: 2015-12-31
	// Switch to start, end?
	public class Position {
		public Integer pos;
		public Integer length;

		Position() {
		}

		Position(int pos, int length) {
			this.pos = pos;
			this.length = length;
		}

		public String substr(String str) {
			return str.substring(pos, pos + length);
		}

		@Override
		public String toString() {
			return pos.toString() + ", " + length.toString();
		}
	}
}
