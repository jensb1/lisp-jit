// Lisp Parser - Builds an AST from tokens

import { Token, TokenType, Lexer } from './lexer';

export type ASTNode =
  | NumberNode
  | SymbolNode
  | StringNode
  | ListNode;

export interface NumberNode {
  type: 'number';
  value: number;
}

export interface SymbolNode {
  type: 'symbol';
  name: string;
}

export interface StringNode {
  type: 'string';
  value: string;
}

export interface ListNode {
  type: 'list';
  elements: ASTNode[];
}

export class Parser {
  private tokens: Token[];
  private pos: number = 0;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  private peek(): Token {
    return this.tokens[this.pos] || { type: TokenType.EOF, value: '', line: 0, column: 0 };
  }

  private advance(): Token {
    return this.tokens[this.pos++];
  }

  private parseAtom(): ASTNode {
    const token = this.advance();

    switch (token.type) {
      case TokenType.NUMBER:
        return { type: 'number', value: parseInt(token.value, 10) };
      case TokenType.SYMBOL:
        return { type: 'symbol', name: token.value };
      case TokenType.STRING:
        return { type: 'string', value: token.value };
      default:
        throw new Error(`Unexpected token ${token.type} at line ${token.line}, column ${token.column}`);
    }
  }

  private parseList(): ListNode {
    this.advance(); // skip LPAREN
    const elements: ASTNode[] = [];

    while (this.peek().type !== TokenType.RPAREN && this.peek().type !== TokenType.EOF) {
      elements.push(this.parseExpr());
    }

    if (this.peek().type !== TokenType.RPAREN) {
      throw new Error('Unexpected end of input, expected )');
    }
    this.advance(); // skip RPAREN

    return { type: 'list', elements };
  }

  parseExpr(): ASTNode {
    const token = this.peek();

    if (token.type === TokenType.LPAREN) {
      return this.parseList();
    }

    return this.parseAtom();
  }

  parse(): ASTNode[] {
    const program: ASTNode[] = [];

    while (this.peek().type !== TokenType.EOF) {
      program.push(this.parseExpr());
    }

    return program;
  }
}

export function parse(source: string): ASTNode[] {
  const lexer = new Lexer(source);
  const tokens = lexer.tokenize();
  const parser = new Parser(tokens);
  return parser.parse();
}
