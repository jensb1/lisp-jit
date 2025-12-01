// JIT Code Generator - Generates ARM64 machine code for Lisp
// Extended with primitives for self-hosting compiler

import { ASTNode, ListNode, NumberNode, SymbolNode, StringNode } from './parser';
import { ARM64Assembler, X0, X1, X2, X9, X10, X11, X12, X13, X14, X28, X29 } from './arm64-assembler';
import { RuntimeAddresses } from './jit';

interface FunctionDef {
  name: string;
  params: string[];
  body: ASTNode;
}

export class JitCodeGenerator {
  private asm: ARM64Assembler;
  private functions: Map<string, FunctionDef> = new Map();
  private globalIndices: Map<string, number> = new Map();  // name -> index in globals vector
  private strings: Map<string, string> = new Map();  // value -> label
  private currentLocals: Map<string, number> = new Map();
  private labelCounter: number = 0;
  private stringCounter: number = 0;
  private addresses: RuntimeAddresses;
  private stackSize: number = 0;
  private tempStackDepth: number = 0;

  constructor(addresses: RuntimeAddresses) {
    this.asm = new ARM64Assembler();
    this.addresses = addresses;
  }

  private newLabel(prefix: string = 'L'): string {
    return `${prefix}${this.labelCounter++}`;
  }

  private pushTemp(): void {
    this.asm.pushX0();
    this.tempStackDepth += 16;
  }

  private popToX0(): void {
    this.asm.popX0();
    this.tempStackDepth -= 16;
  }

  private popToX1(): void {
    this.asm.popX1();
    this.tempStackDepth -= 16;
  }

  private loadAddress(reg: number, addr: bigint): void {
    const low = Number(addr & 0xFFFFn);
    const mid1 = Number((addr >> 16n) & 0xFFFFn);
    const mid2 = Number((addr >> 32n) & 0xFFFFn);
    const high = Number((addr >> 48n) & 0xFFFFn);

    this.asm.movImm(reg, low);
    if (mid1 !== 0) this.asm.movk(reg, mid1, 16);
    if (mid2 !== 0) this.asm.movk(reg, mid2, 32);
    if (high !== 0) this.asm.movk(reg, high, 48);
  }

  private callRuntime(addr: bigint): void {
    this.loadAddress(X9, addr);
    this.asm.blr(X9);
  }

  generate(program: ASTNode[]): Uint8Array {
    // First pass: collect definitions
    const globalVars: { name: string; value: ASTNode }[] = [];

    for (const node of program) {
      if (node.type === 'list') {
        const list = node as ListNode;
        if (list.elements.length > 0 && list.elements[0].type === 'symbol') {
          const op = (list.elements[0] as SymbolNode).name;
          if (op === 'define' && list.elements.length >= 3) {
            const nameOrSig = list.elements[1];
            if (nameOrSig.type === 'list') {
              // Function definition: (define (name params...) body)
              const sig = nameOrSig as ListNode;
              if (sig.elements.length > 0 && sig.elements[0].type === 'symbol') {
                const funcName = (sig.elements[0] as SymbolNode).name;
                const params = sig.elements.slice(1).map(p => {
                  if (p.type !== 'symbol') throw new Error('Function parameters must be symbols');
                  return (p as SymbolNode).name;
                });
                // Wrap multiple body expressions in implicit begin
                const bodyExprs = list.elements.slice(2);
                const body: ASTNode = bodyExprs.length === 1
                  ? bodyExprs[0]
                  : { type: 'list', elements: [{ type: 'symbol', name: 'begin' } as SymbolNode, ...bodyExprs] } as ListNode;
                this.functions.set(funcName, { name: funcName, params, body });
              }
            } else if (nameOrSig.type === 'symbol') {
              // Variable definition: (define name value)
              const varName = (nameOrSig as SymbolNode).name;
              this.globalIndices.set(varName, globalVars.length);
              globalVars.push({ name: varName, value: list.elements[2] });
            }
          }
        }
      }
    }

    // Jump to main
    this.asm.b('_main');

    // Generate user-defined functions
    for (const [, func] of this.functions) {
      this.generateFunction(func);
    }

    // Generate print_int helper
    this.generatePrintInt();

    // Generate main entry point
    this.asm.label('_main');
    this.asm.prologue();

    // Allocate globals vector using make_vector if there are globals
    // Store pointer in X28 (callee-saved register)
    const numGlobals = globalVars.length;
    if (numGlobals > 0) {
      // Save X28 on stack (it's callee-saved)
      this.asm.allocStack(16);
      this.asm.strImm(X28, 31, 0);

      // Call make_vector(numGlobals) to allocate globals table
      this.asm.movImm(X0, numGlobals);
      this.callRuntime(this.addresses.makeVector);

      // Store result in X28 (globals table pointer)
      this.asm.mov(X28, X0);
    }

    // Generate top-level expressions (including variable initializations)
    for (const node of program) {
      if (node.type === 'list') {
        const list = node as ListNode;
        if (list.elements.length > 0 && list.elements[0].type === 'symbol') {
          const op = (list.elements[0] as SymbolNode).name;
          if (op === 'define') {
            if (list.elements[1].type === 'list') {
              continue; // Skip function definitions
            } else if (list.elements[1].type === 'symbol') {
              // Variable definition - evaluate and store in globals vector
              const varName = (list.elements[1] as SymbolNode).name;
              const globalIdx = this.globalIndices.get(varName)!;

              this.generateExpr(list.elements[2]);

              // Store in globals vector: vector_set(X28, idx, X0)
              this.pushTemp();  // save the value
              this.asm.mov(X0, X28);  // vector
              this.asm.movImm(X1, globalIdx);  // index
              this.asm.ldrPost(X2, 31, 16);  // value from stack
              this.tempStackDepth -= 16;
              this.callRuntime(this.addresses.vectorSet);
              continue;
            }
          }
        }
      }
      this.generateExpr(node);
    }

    // Restore X28 and return
    if (numGlobals > 0) {
      this.asm.ldrImm(X28, 31, 0);
      this.asm.freeStack(16);
    }

    // Return 0 from main
    this.asm.movImm(X0, 0);
    this.asm.epilogue();

    return this.asm.getCode();
  }

  private generatePrintInt(): void {
    this.asm.label('_print_int');
    this.asm.prologue();
    this.asm.allocStack(32);

    this.asm.mov(X9, X0);
    this.asm.addImm(X10, 31, 30);
    this.asm.movImm(X11, 0);
    this.asm.strb(11, 10, 0);
    this.asm.subImm(X10, X10, 1);
    this.asm.movImm(X12, 10);

    this.asm.cmpImm(X9, 0);
    this.asm.bcond(ARM64Assembler.GE, '_print_int_positive');
    this.asm.neg(X9, X9);
    this.asm.movImm(X13, 1);
    this.asm.b('_print_int_loop');

    this.asm.label('_print_int_positive');
    this.asm.movImm(X13, 0);
    this.asm.cmpImm(X9, 0);
    this.asm.bcond(ARM64Assembler.NE, '_print_int_loop');

    this.asm.movImm(X11, 48);
    this.asm.strb(11, 10, 0);
    this.asm.b('_print_int_final');

    this.asm.label('_print_int_loop');
    this.asm.cmpImm(X9, 0);
    this.asm.bcond(ARM64Assembler.EQ, '_print_int_check_neg');

    this.asm.udiv(X14, X9, X12);
    this.asm.msub(X11, X14, X12, X9);
    this.asm.addImm(X11, X11, 48);
    this.asm.strb(11, 10, 0);
    this.asm.subImm(X10, X10, 1);
    this.asm.mov(X9, X14);
    this.asm.b('_print_int_loop');

    this.asm.label('_print_int_check_neg');
    this.asm.cmpImm(X13, 0);
    this.asm.bcond(ARM64Assembler.EQ, '_print_int_done');
    this.asm.movImm(X11, 45);
    this.asm.strb(11, 10, 0);
    this.asm.b('_print_int_final');

    this.asm.label('_print_int_done');
    this.asm.addImm(X10, X10, 1);

    this.asm.label('_print_int_final');
    this.asm.mov(X0, X10);
    this.callRuntime(this.addresses.printStr);

    this.asm.freeStack(32);
    this.asm.epilogue();
  }

  private generateFunction(func: FunctionDef): void {
    this.asm.label(`_lisp_${func.name}`);
    this.asm.prologue();

    this.currentLocals = new Map();
    this.tempStackDepth = 0;

    const numParams = func.params.length;
    if (numParams > 0) {
      this.stackSize = Math.ceil(numParams / 2) * 16;
      this.asm.allocStack(this.stackSize);

      for (let i = 0; i < numParams; i++) {
        const spOffset = this.stackSize - (i + 1) * 8;
        this.currentLocals.set(func.params[i], spOffset);
        this.asm.strImm(i, 31, spOffset);
      }
    } else {
      this.stackSize = 0;
    }

    this.generateExpr(func.body);

    if (numParams > 0) {
      this.asm.freeStack(this.stackSize);
    }
    this.asm.epilogue();

    this.currentLocals = new Map();
    this.stackSize = 0;
    this.tempStackDepth = 0;
  }

  private generateExpr(node: ASTNode): void {
    switch (node.type) {
      case 'number':
        this.generateNumber(node as NumberNode);
        break;
      case 'symbol':
        this.generateSymbol(node as SymbolNode);
        break;
      case 'string':
        this.generateString(node as StringNode);
        break;
      case 'list':
        this.generateList(node as ListNode);
        break;
    }
  }

  private generateNumber(node: NumberNode): void {
    this.asm.movImm(X0, node.value);
  }

  private generateSymbol(node: SymbolNode): void {
    const name = node.name;

    // Check local variables first
    if (this.currentLocals.has(name)) {
      const baseOffset = this.currentLocals.get(name)!;
      const adjustedOffset = baseOffset + this.tempStackDepth;
      this.asm.ldrImm(X0, 31, adjustedOffset);
      return;
    }

    // Check global variables (stored in vector pointed to by X28)
    if (this.globalIndices.has(name)) {
      const globalIdx = this.globalIndices.get(name)!;
      // Call vector_ref(X28, idx)
      this.asm.mov(X0, X28);  // vector pointer
      this.asm.movImm(X1, globalIdx);  // index
      this.callRuntime(this.addresses.vectorRef);
      return;
    }

    throw new Error(`Undefined symbol: ${name}`);
  }

  private generateString(node: StringNode): void {
    // Create string at runtime using make_string
    const str = node.value;
    const bytes = Buffer.from(str, 'utf8');

    // We need to embed the string data and call make_string
    // For now, store characters on stack and call make_string
    const len = bytes.length;

    // Allocate space for string + null terminator, 16-byte aligned
    const allocSize = ((len + 1 + 15) >> 4) << 4;
    this.asm.allocStack(allocSize);

    // Write bytes to stack
    for (let i = 0; i < len; i++) {
      this.asm.movImm(X11, bytes[i]!);
      this.asm.strb(11, 31, i);
    }
    // Null terminator
    this.asm.movImm(X11, 0);
    this.asm.strb(11, 31, len);

    // Call make_string(sp, len)
    this.asm.addImm(X0, 31, 0);  // copy SP to X0 (mov uses XZR for r31, not SP)
    this.asm.movImm(X1, len);
    this.callRuntime(this.addresses.makeString);

    this.asm.freeStack(allocSize);
  }

  private generateList(node: ListNode): void {
    if (node.elements.length === 0) {
      this.asm.movImm(X0, 0);
      return;
    }

    const first = node.elements[0];
    if (first.type !== 'symbol') {
      throw new Error('First element of list must be a symbol');
    }

    const op = (first as SymbolNode).name;
    const args = node.elements.slice(1);

    // Built-in operations
    switch (op) {
      case '+': this.generateArithmetic(args, 'add'); break;
      case '-': this.generateArithmetic(args, 'sub'); break;
      case '*': this.generateArithmetic(args, 'mul'); break;
      case '/': this.generateArithmetic(args, 'sdiv'); break;
      case '<': this.generateComparison(args, ARM64Assembler.LT); break;
      case '>': this.generateComparison(args, ARM64Assembler.GT); break;
      case '<=': this.generateComparison(args, ARM64Assembler.LE); break;
      case '>=': this.generateComparison(args, ARM64Assembler.GE); break;
      case '=': this.generateComparison(args, ARM64Assembler.EQ); break;
      case 'if': this.generateIf(args); break;
      case 'print': this.generatePrint(args); break;
      case 'define': break; // Handled elsewhere

      // List operations
      case 'cons': this.generateCons(args); break;
      case 'car': this.generateCar(args); break;
      case 'cdr': this.generateCdr(args); break;
      case 'null?': this.generateNullCheck(args); break;
      case 'pair?': this.generatePairCheck(args); break;
      case 'list': this.generateListConstruct(args); break;

      // String operations
      case 'string-length': this.generateStringLength(args); break;
      case 'string-ref': this.generateStringRef(args); break;
      case 'substring': this.generateSubstring(args); break;
      case 'string-append': this.generateStringAppend(args); break;
      case 'string=?': this.generateStringEqual(args); break;
      case 'number->string': this.generateIntToString(args); break;
      case 'string->number': this.generateStringToInt(args); break;
      case 'make-char-string': this.generateMakeCharString(args); break;

      // Character predicates
      case 'char-whitespace?': this.generateCharPredicate(args, this.addresses.charWhitespace); break;
      case 'char-numeric?': this.generateCharPredicate(args, this.addresses.charDigit); break;
      case 'char-alphabetic?': this.generateCharPredicate(args, this.addresses.charAlpha); break;

      // Vector operations
      case 'make-vector': this.generateMakeVector(args); break;
      case 'vector-ref': this.generateVectorRef(args); break;
      case 'vector-set!': this.generateVectorSet(args); break;
      case 'vector-length': this.generateVectorLength(args); break;

      // I/O
      case 'read-file': this.generateReadFile(args); break;
      case 'write-file': this.generateWriteFile(args); break;
      case 'error': this.generateError(args); break;

      // Bitwise operations
      case 'bitand': this.generateBitwise(args, this.addresses.bitand); break;
      case 'bitor': this.generateBitwise(args, this.addresses.bitor); break;
      case 'bitxor': this.generateBitwise(args, this.addresses.bitxor); break;
      case 'bitnot': this.generateUnaryBitwise(args, this.addresses.bitnot); break;
      case 'lsl': this.generateBitwise(args, this.addresses.lsl); break;
      case 'asr': this.generateBitwise(args, this.addresses.asr); break;
      case 'lsr': this.generateBitwise(args, this.addresses.lsr); break;

      // Sequence
      case 'begin': this.generateBegin(args); break;
      case 'let': this.generateLet(args); break;

      // Logical
      case 'and': this.generateAnd(args); break;
      case 'or': this.generateOr(args); break;
      case 'not': this.generateNot(args); break;

      default:
        if (this.functions.has(op)) {
          this.generateCall(op, args);
        } else {
          throw new Error(`Unknown operator: ${op}`);
        }
    }
  }

  private generateArithmetic(args: ASTNode[], operation: string): void {
    if (args.length === 0) {
      this.asm.movImm(X0, 0);
      return;
    }

    if (args.length === 1) {
      this.generateExpr(args[0]);
      if (operation === 'sub') {
        this.asm.neg(X0, X0);
      }
      return;
    }

    this.generateExpr(args[0]);
    this.pushTemp();

    for (let i = 1; i < args.length; i++) {
      this.generateExpr(args[i]);
      this.popToX1();

      switch (operation) {
        case 'add': this.asm.addReg(X0, X1, X0); break;
        case 'sub': this.asm.subReg(X0, X1, X0); break;
        case 'mul': this.asm.mul(X0, X1, X0); break;
        case 'sdiv': this.asm.sdiv(X0, X1, X0); break;
      }

      if (i < args.length - 1) {
        this.pushTemp();
      }
    }
  }

  private generateComparison(args: ASTNode[], cond: number): void {
    if (args.length !== 2) throw new Error('Comparison requires exactly 2 arguments');

    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.popToX1();
    this.asm.cmpReg(X1, X0);
    this.asm.cset(X0, cond);
  }

  private generateIf(args: ASTNode[]): void {
    if (args.length < 2 || args.length > 3) throw new Error('if requires 2 or 3 arguments');

    const elseLabel = this.newLabel('else');
    const endLabel = this.newLabel('endif');

    this.generateExpr(args[0]);
    this.asm.cmpImm(X0, 0);
    this.asm.bcond(ARM64Assembler.EQ, elseLabel);

    this.generateExpr(args[1]);
    this.asm.b(endLabel);

    this.asm.label(elseLabel);
    if (args.length === 3) {
      this.generateExpr(args[2]);
    } else {
      this.asm.movImm(X0, 0);
    }

    this.asm.label(endLabel);
  }

  private generatePrint(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('print requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.asm.bl('_print_int');
  }

  private generateCons(args: ASTNode[]): void {
    if (args.length !== 2) throw new Error('cons requires exactly 2 arguments');
    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.asm.mov(X1, X0);
    this.popToX0();
    this.callRuntime(this.addresses.cons);
  }

  private generateCar(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('car requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.car);
  }

  private generateCdr(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('cdr requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.cdr);
  }

  private generateNullCheck(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('null? requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.null);
  }

  private generatePairCheck(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('pair? requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.consp);
  }

  private generateListConstruct(args: ASTNode[]): void {
    if (args.length === 0) {
      this.asm.movImm(X0, 0);
      return;
    }
    // Build list backwards: (list a b c) = (cons a (cons b (cons c nil)))
    this.asm.movImm(X0, 0);  // nil
    for (let i = args.length - 1; i >= 0; i--) {
      this.pushTemp();
      this.generateExpr(args[i]);
      this.asm.mov(X1, X0);
      this.popToX0();
      // Swap: we want (cons element list), but we have element in X1, list in X0
      // Actually we need: X0=element, X1=list
      // Push list, generate element, pop list to X1
      this.pushTemp();  // push list (in X0)
      this.generateExpr(args[i]);  // element in X0
      this.popToX1();  // list in X1
      this.callRuntime(this.addresses.cons);
    }
    // This is getting complicated, let me simplify
    // Actually, start from the end
    this.asm.movImm(X0, 0);  // start with nil
    for (let i = args.length - 1; i >= 0; i--) {
      this.pushTemp();  // save current list
      this.generateExpr(args[i]);
      this.asm.mov(X1, X0);  // element to X1? No, we need element in X0, list in X1
      // X0 has element, we need list in X1
      // Pop saved list to X1
      this.asm.ldrPost(X1, 31, 16);
      this.tempStackDepth -= 16;
      // Now X0=element, X1=list
      // But cons takes (car, cdr), so X0=car, X1=cdr
      // Wait, we want to prepend, so cons(element, list)
      // X0=element, X1=list is correct
      this.callRuntime(this.addresses.cons);
    }
  }

  private generateStringLength(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('string-length requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.stringLength);
  }

  private generateStringRef(args: ASTNode[]): void {
    if (args.length !== 2) throw new Error('string-ref requires exactly 2 arguments');
    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.asm.mov(X1, X0);
    this.popToX0();
    this.callRuntime(this.addresses.stringRef);
  }

  private generateSubstring(args: ASTNode[]): void {
    if (args.length !== 3) throw new Error('substring requires exactly 3 arguments');
    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.pushTemp();
    this.generateExpr(args[2]);
    this.asm.mov(X2, X0);
    this.popToX1();
    this.popToX0();
    this.callRuntime(this.addresses.substring);
  }

  private generateStringAppend(args: ASTNode[]): void {
    if (args.length < 2) {
      if (args.length === 1) {
        this.generateExpr(args[0]);
        return;
      }
      // Empty string
      this.generateString({ type: 'string', value: '' } as StringNode);
      return;
    }
    this.generateExpr(args[0]);
    for (let i = 1; i < args.length; i++) {
      this.pushTemp();
      this.generateExpr(args[i]);
      this.asm.mov(X1, X0);
      this.popToX0();
      this.callRuntime(this.addresses.stringAppend);
    }
  }

  private generateStringEqual(args: ASTNode[]): void {
    if (args.length !== 2) throw new Error('string=? requires exactly 2 arguments');
    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.asm.mov(X1, X0);
    this.popToX0();
    this.callRuntime(this.addresses.stringEqual);
  }

  private generateIntToString(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('number->string requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.intToString);
  }

  private generateStringToInt(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('string->number requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.stringToInt);
  }

  private generateMakeCharString(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('make-char-string requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.makeCharString);
  }

  private generateCharPredicate(args: ASTNode[], addr: bigint): void {
    if (args.length !== 1) throw new Error('char predicate requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(addr);
  }

  private generateMakeVector(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('make-vector requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.makeVector);
  }

  private generateVectorRef(args: ASTNode[]): void {
    if (args.length !== 2) throw new Error('vector-ref requires exactly 2 arguments');
    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.asm.mov(X1, X0);
    this.popToX0();
    this.callRuntime(this.addresses.vectorRef);
  }

  private generateVectorSet(args: ASTNode[]): void {
    if (args.length !== 3) throw new Error('vector-set! requires exactly 3 arguments');
    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.pushTemp();
    this.generateExpr(args[2]);
    this.asm.mov(X2, X0);
    this.popToX1();
    this.popToX0();
    this.callRuntime(this.addresses.vectorSet);
    this.asm.movImm(X0, 0);  // return nil
  }

  private generateVectorLength(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('vector-length requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.vectorLength);
  }

  private generateReadFile(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('read-file requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.readFile);
  }

  private generateWriteFile(args: ASTNode[]): void {
    if (args.length !== 2) throw new Error('write-file requires exactly 2 arguments');
    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.asm.mov(X1, X0);
    this.popToX0();
    this.callRuntime(this.addresses.writeFile);
  }

  private generateError(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('error requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(this.addresses.error);
  }

  private generateBegin(args: ASTNode[]): void {
    for (const arg of args) {
      this.generateExpr(arg);
    }
  }

  private generateLet(args: ASTNode[]): void {
    // (let ((var1 val1) (var2 val2)) body...)
    if (args.length < 2) throw new Error('let requires bindings and body');

    const bindings = args[0];
    if (bindings.type !== 'list') throw new Error('let bindings must be a list');

    const bindingList = (bindings as ListNode).elements;
    const savedLocals = new Map(this.currentLocals);
    const savedStackSize = this.stackSize;

    // Allocate space for let bindings
    const numBindings = bindingList.length;
    let bindingStackSize = 0;
    const savedTempStackDepth = this.tempStackDepth;

    if (numBindings > 0) {
      bindingStackSize = Math.ceil(numBindings / 2) * 16;
      this.asm.allocStack(bindingStackSize);
      // Track let allocation in tempStackDepth so existing locals get adjusted
      this.tempStackDepth += bindingStackSize;

      // Evaluate and store each binding
      for (let i = 0; i < numBindings; i++) {
        const binding = bindingList[i];
        if (binding.type !== 'list') throw new Error('let binding must be (var val)');
        const parts = (binding as ListNode).elements;
        if (parts.length !== 2) throw new Error('let binding must be (var val)');
        if (parts[0].type !== 'symbol') throw new Error('let binding var must be symbol');

        const varName = (parts[0] as SymbolNode).name;
        this.generateExpr(parts[1]);

        // Store at offset from current SP
        const spOffset = bindingStackSize - (i + 1) * 8;
        this.asm.strImm(X0, 31, spOffset);
        // Store the absolute offset from the stack base:
        // When we access this binding, tempStackDepth tells us where SP is relative to function base
        // This binding is at: (function stack base) - tempStackDepth + spOffset
        // So we store: spOffset - tempStackDepth (negative value)
        // When loading: baseOffset + currentTempStackDepth = (spOffset - savedTempStackDepth - bindingStackSize) + currentTempStackDepth
        // If currentTempStackDepth == savedTempStackDepth + bindingStackSize, this gives spOffset (correct)
        // If currentTempStackDepth is larger (inner let), we get the right adjusted offset
        this.currentLocals.set(varName, spOffset - this.tempStackDepth);
      }
    }

    // Evaluate body
    for (let i = 1; i < args.length; i++) {
      this.generateExpr(args[i]);
    }

    // Restore
    if (numBindings > 0) {
      this.asm.freeStack(bindingStackSize);
      this.tempStackDepth = savedTempStackDepth;
    }
    this.currentLocals = savedLocals;
    this.stackSize = savedStackSize;
  }

  private generateAnd(args: ASTNode[]): void {
    if (args.length === 0) {
      this.asm.movImm(X0, 1);
      return;
    }

    const endLabel = this.newLabel('and_end');

    for (let i = 0; i < args.length; i++) {
      this.generateExpr(args[i]);
      if (i < args.length - 1) {
        this.asm.cmpImm(X0, 0);
        this.asm.bcond(ARM64Assembler.EQ, endLabel);
      }
    }

    this.asm.label(endLabel);
  }

  private generateOr(args: ASTNode[]): void {
    if (args.length === 0) {
      this.asm.movImm(X0, 0);
      return;
    }

    const endLabel = this.newLabel('or_end');

    for (let i = 0; i < args.length; i++) {
      this.generateExpr(args[i]);
      if (i < args.length - 1) {
        this.asm.cmpImm(X0, 0);
        this.asm.bcond(ARM64Assembler.NE, endLabel);
      }
    }

    this.asm.label(endLabel);
  }

  private generateNot(args: ASTNode[]): void {
    if (args.length !== 1) throw new Error('not requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.asm.cmpImm(X0, 0);
    this.asm.cset(X0, ARM64Assembler.EQ);
  }

  private generateBitwise(args: ASTNode[], addr: bigint): void {
    if (args.length !== 2) throw new Error('Bitwise operation requires exactly 2 arguments');
    this.generateExpr(args[0]);
    this.pushTemp();
    this.generateExpr(args[1]);
    this.asm.mov(X1, X0);
    this.popToX0();
    this.callRuntime(addr);
  }

  private generateUnaryBitwise(args: ASTNode[], addr: bigint): void {
    if (args.length !== 1) throw new Error('Unary bitwise operation requires exactly 1 argument');
    this.generateExpr(args[0]);
    this.callRuntime(addr);
  }

  private generateCall(funcName: string, args: ASTNode[]): void {
    const func = this.functions.get(funcName);
    if (!func) throw new Error(`Undefined function: ${funcName}`);

    if (args.length !== func.params.length) {
      throw new Error(`Function ${funcName} expects ${func.params.length} arguments, got ${args.length}`);
    }

    for (let i = args.length - 1; i >= 0; i--) {
      this.generateExpr(args[i]);
      this.pushTemp();
    }

    for (let i = 0; i < args.length && i < 8; i++) {
      this.asm.ldrPost(i, 31, 16);
      this.tempStackDepth -= 16;
    }

    this.asm.bl(`_lisp_${funcName}`);
  }
}
