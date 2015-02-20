package language

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"unicode"
	"unicode/utf8"
)

type Token struct {
	item  item
	val   string
	scope int
}

type item int

// Keywords and literal types
const (
	eof            = -1
	itemError item = iota
	itemEOF

	itemAnd      // and
	itemBreak    // break
	itemDo       // do
	itemElse     // else
	itemElseIf   // elseif
	itemEnd      // end
	itemFalse    // false
	itemFor      // for
	itemFunction // function
	itemGoto     // goto
	itemIf       // if
	itemIn       // in
	itemLocal    // local
	itemNil      // nil
	itemNot      // not
	itemOr       // or
	itemRepeat   // repeat
	itemReturn   // return
	itemThen     // then
	itemTrue     // true
	itemUntil    // until
	itemWhile    // while

	itemAdd    // +
	itemSub    // - (Subtraction and unary minus)
	itemMul    // *
	itemDiv    // /
	itemMod    // %
	itemPow    // ^
	itemLen    // #
	itemBitAnd // &
	itemBitXor // ~ (Xor and unary bitwise negation)
	itemBitOr  // |
	itemShl    // <<
	itemShr    // >>
	itemIdiv   // //
	itemEq     // ==
	itemNeq    // ~=
	itemLte    // <=
	itemGte    // >=
	itemLt     // <
	itemGt     // >

	itemAssign   // =
	itemLParen   // (
	itemRParen   // )
	itemLBrace   // {
	itemRBrace   // }
	itemLBracket // [
	itemRBracket // ]
	itemDblColon // ::
	itemSemi     // ;
	itemColon    // :
	itemComma    // ,
	itemDot      // .
	itemConcat   // ..
	itemElipsis  // ...

	itemSpace // the parser can determine if it's relevant.
	itemBareword
	itemLabel
)

type stateFn func(*lexer) stateFn

type lexer struct {
	state stateFn
	in    *bytes.Buffer
	input []byte
	start int
	pos   int
	width int
	scope int
	items chan Token
}

func NewLexer() *lexer {
	l := &lexer{
		in:    &bytes.Buffer{},
		items: make(chan Token),
	}
	return l
}

func (l *lexer) run() {
	for l.state = lexSpace; l.state != nil; {
		l.state = l.state(l)
	}
}

func (l *lexer) emit(i item) {
	l.items <- Token{i, l.cur(), l.scope}
	l.ignore()
}

func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- Token{itemError, fmt.Sprintf(format, args...)}
	return nil
}

func (l *lexer) Write(b []byte) (int, error) {
	return l.input.Write(b)
}

func (l *lexer) next() rune {
	r, w := utf8.DecodeRune(l.input[l.pos:])
	if r == utf8.RuneError {
		if w == 0 && l.in.Len() == 0 {
			return eof
		}
		if _, err := io.ReadFull(l.in, l.input); err != nil && err != io.ErrUnexpectedEOF {
			l.errorf(err)
			return eof
		}
		return l.next()
	}
	l.pos += w
	l.width = w
	return r
}

func (l *lexer) backup() {
	l.pos -= l.width
}

func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

func (l *lexer) ignore() {
	l.start = l.pos
	l.width = 0
}

func (l *lexer) cur() string {
	return string(l.input[l.start:l.pos])
}

func (l *lexer) accept(s string) bool {
	if strings.IndexRune(s, l.next()) != -1 {
		return true
	}
	l.backup()
	return false
}

func (l *lexer) acceptRun(s string) {
	for strings.IndexRune(s, l.next()) != -1 {
	}
	l.backup()
}

func (l *lexer) discardWhitespace() {
	for r := l.next(); unicode.IsSpace(r); r = l.next() {
	}
	l.backup()
	l.ignore()
}

// This consumes a "Name" as defined by the Lua spec: http://www.lua.org/manual/5.3/manual.html#3.1
func (l *lexer) consumeName() {
	for r := l.next(); unicode.IsLetter(r) || unicode.IsDigit || r == '_'; r = l.next() {
	}
	l.backup()
}

func lexStmt(l *lexer) stateFn {
	l.discardWhitespace()
	switch l.peek() {
	case ';':
		l.next()
		l.emit(itemSemi)
		return lexStmt
	case '-':
		return lexComment
	case ':':
		l.next()
		if l.peek() != ':' {
			l.errorf("expected ':', got %q", l.peek())
			return nil
		}
		l.next()
		return lexLabel
	}
	l.consumeName()
	switch l.cur() {
	case "break":
		l.emit(itemBreak)
		return lexStmt
	case "goto":
		l.ignore()
		l.discardWhitespace()
		l.consumeName()
		l.emit(itemGoto)
		return lexStmt
	case "do":
		l.emit(itemDo)
		l.block++
		return lexStmt
	case "end":
		l.emit(itemEnd)
		l.block--
		return lexStmt
	case "while":
		l.emit(itemWhile)
		return lexExp
	case "repeat":
		l.emit(itemRepeat)
		l.block++
		return lexStmt
	case "until":
		l.emit(itemUntil)
		l.block--
		return lexExp
	case "if":
		l.emit(itemIf)
		return lexExp
	case "then":
		l.emit(itemThen)
		l.block++
		return lexStmt
	case "elseif":
		l.block--
		l.emit(itemElseIf)
		return lexExp
	case "else":
		l.block--
		l.emit(itemElse)
		l.block++
		return lexStmt
	}
}

func lexSpace(l *lexer) stateFn {
	for unicode.IsSpace(l.next()) {
	}
	l.backup()
	l.emit(itemSpace)
	switch l.peek() {
	case '-':
		return lexComment
	default:
		return lexBlock
	}
}

func lexComment(l *lexer) stateFn {
	l.next()
	if !l.accept("-") {
		l.errorf("unexpected: %q", l.peek())
		return nil
	}
	l.next()
	if lexLiteral(l) == nil {
		for r := l.next(); r != '\n'; r = l.next() {
			// if we don't find a valid literal opening, just read to newline
		}
	}
	return lexSpace
}

func lexLiteral(l *lexer) stateFn {
	level := 0
	r := l.next()
	for ; r == '='; r = l.next() {
		level++
	}
	if r != '[' {
		l.errorf("unexpected: %q", r)
		return nil
	}
	// define this up here for the loop so we can goto
	i := 0
	// this is to handle lower "level" or incomplete end sigils
	t := make([]rune, 0)
consume:
	for _, r := range t {
		l.keep(r)
	}
	t = t[:0]
	for ; r != ']'; r = l.next() {
		l.keep(r)
	}
	for ; i < level; i++ {
		if l.peek() != '=' {
			goto consume
		}
		t = append(t, l.next())
	}
	if l.peek() != ']' {
		goto consume
	}
	return lexSpace
}

func lexBlock(l *lexer) stateFn {
	r := l.next()
	l.keep(r)
	switch {
	case r == ':':
		if l.peek() != ':' {
			l.errorf("unexpected: %q", l.peek())
			return nil
		}
		l.next()
		return lexLabel
	case unicode.IsLetter(r) || r == '_':
		// it's a name or a keyword!
		return lexIdentifier
	}
	l.errorf("unexpected: %q", r)
	return nil
}

func lexLabel(l *lexer) stateFn {
	r := l.next()
	for ; r != ':'; r = l.next() {
		l.keep(r)
	}
	if l.peek() != ':' {
		l.errorf("invalid label: unexpected %q", l.peek())
		return nil
	}
	l.next()
	l.emit(itemLabel)
	return lexSpace
}

func lexIdentifier(l *lexer) stateFn {
	l.consumeIdentifier()
	l.acceptWhitespace()
	switch l.cur.String() {
	case "and":
		l.emit(itemAnd)
		return lexExpr
	case "break":
		l.emit(itemBreak)
	case "do":
		l.emit(itemDo)
		return lexBlock
	case "else":
		l.emit(itemElse)
		return lexExpr
	case "elseif":
		l.emit(itemElseIf)
		return lexExpr
	case "end":
		l.emit(itemEnd)
		return lexBlock
	case "false":
		l.emit(itemFalse)
		return lexExpr
	case "for":
		l.emit(itemFor)
		return lexExpr
	case "function":
		l.consumeIdentifier()
		l.emit(itemFunction)
		return lexBlock
	case "goto":
		l.consumeIdentifier()
		l.emit(itemGoto)
		return lexBlock
	case "if":
		l.emit(itemIf)
		return lexExpr
	case "in":
		l.emit(itemIn)
		return lexExpr
	case "local":
		l.emit(itemLocal)
		return lexIdentifier
	case "nil":
		l.emit(itemNil)
		return nil
	case "not":
		l.emit(itemNot)
		return lexExpr
	case "or":
		l.emit(itemOr)
		return lexExpr
	case "repeat":
		l.emit(itemRepeat)
		return lexExpr
	case "return":
		l.emit(itemReturn)
		return lexExpr
	case "then":
		l.emit(itemThen)
		return lexBlock
	case "true":
		l.emit(itemTrue)
		return lexIdentifier
	case "until":
		l.emit(itemUntil)
		return lexExpr
	case "while":
		l.emit(itemWhile)
		return lexExpr
	}
	l.emit(itemBareword)
	r := l.next()
	l.keep(r)
	switch r {
	case '=':
		l.emit(itemAssign)
	case '(':
		l.emit(itemLParen)
	case ')':
		l.emit(itemRParen)
	case '{':
		l.emit(itemLBrace)
	case '}':
		l.emit(itemRBrace)
	case '[':
		l.emit(itemLBracket)
	case ']':
		l.emit(itemRBracket)
	case ';':
		l.emit(itemSemi)
	case ':':
		l.emit(itemColon)
	case ',':
		l.emit(itemComma)
	case '.':
		l.emit(itemDot)
	case '-':
		if l.peek() == '-' {
			l.next()
			return lexComment
		}
	}
	l.acceptWhitespace()
	return lexIdentifier
}

func lexExpr(l *lexer) stateFn {
	return nil
}
