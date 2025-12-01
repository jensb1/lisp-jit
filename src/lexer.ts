// Lisp Lexer - Tokenizes Lisp source code

export enum TokenType {
  LPAREN = 'LPAREN',
  RPAREN = 'RPAREN',
  NUMBER = 'NUMBER',
  SYMBOL = 'SYMBOL',
  STRING = 'STRING',
  EOF = 'EOF',
}

export interface Token {
  type: TokenType;
  value: string;
  line: number;
  column: number;
}

export class Lexer {
  private source: string;
  private pos: number = 0;
  private line: number = 1;
  private column: number = 1;

  constructor(source: string) {
    this.source = source;
  }

  private peek(): string {
    return this.source[this.pos] || '\0';
  }

  private advance(): string {
    const ch = this.source[this.pos++] || '\0';
    if (ch === '\n') {
      this.line++;
      this.column = 1;
    } else {
      this.column++;
    }
    return ch;
  }

  private skipWhitespace(): void {
    while (this.pos < this.source.length) {
      const ch = this.peek();
      if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
        this.advance();
      } else if (ch === ';') {
        // Skip comments
        while (this.peek() !== '\n' && this.peek() !== '\0') {
          this.advance();
        }
      } else {
        break;
      }
    }
  }

  private readNumber(): Token {
    const startLine = this.line;
    const startColumn = this.column;
    let value = '';

    if (this.peek() === '-') {
      value += this.advance();
    }

    while (/[0-9]/.test(this.peek())) {
      value += this.advance();
    }

    return { type: TokenType.NUMBER, value, line: startLine, column: startColumn };
  }

  private readSymbol(): Token {
    const startLine = this.line;
    const startColumn = this.column;
    let value = '';

    while (/[a-zA-Z0-9_+\-*/<>=!?]/.test(this.peek())) {
      value += this.advance();
    }

    return { type: TokenType.SYMBOL, value, line: startLine, column: startColumn };
  }

  private readString(): Token {
    const startLine = this.line;
    const startColumn = this.column;
    this.advance(); // skip opening quote
    let value = '';

    while (this.peek() !== '"' && this.peek() !== '\0') {
      if (this.peek() === '\\') {
        this.advance();
        const escaped = this.advance();
        switch (escaped) {
          case 'n': value += '\n'; break;
          case 't': value += '\t'; break;
          case '\\': value += '\\'; break;
          case '"': value += '"'; break;
          default: value += escaped;
        }
      } else {
        value += this.advance();
      }
    }

    if (this.peek() === '"') {
      this.advance(); // skip closing quote
    }

    return { type: TokenType.STRING, value, line: startLine, column: startColumn };
  }

  nextToken(): Token {
    this.skipWhitespace();

    if (this.pos >= this.source.length) {
      return { type: TokenType.EOF, value: '', line: this.line, column: this.column };
    }

    const ch = this.peek();
    const startLine = this.line;
    const startColumn = this.column;

    if (ch === '(') {
      this.advance();
      return { type: TokenType.LPAREN, value: '(', line: startLine, column: startColumn };
    }

    if (ch === ')') {
      this.advance();
      return { type: TokenType.RPAREN, value: ')', line: startLine, column: startColumn };
    }

    if (ch === '"') {
      return this.readString();
    }

    if (/[0-9]/.test(ch) || (ch === '-' && /[0-9]/.test(this.source[this.pos + 1] || ''))) {
      return this.readNumber();
    }

    if (/[a-zA-Z_+\-*/<>=!?]/.test(ch)) {
      return this.readSymbol();
    }

    throw new Error(`Unexpected character '${ch}' at line ${this.line}, column ${this.column}`);
  }

  tokenize(): Token[] {
    const tokens: Token[] = [];
    let token: Token;

    do {
      token = this.nextToken();
      tokens.push(token);
    } while (token.type !== TokenType.EOF);

    return tokens;
  }
}
