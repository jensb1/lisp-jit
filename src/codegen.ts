// ARM64 Code Generator for macOS

import { ASTNode, ListNode, NumberNode, SymbolNode, StringNode } from './parser';

interface FunctionDef {
  name: string;
  params: string[];
  body: ASTNode;
}

export class CodeGenerator {
  private output: string[] = [];
  private dataSection: string[] = [];
  private functions: Map<string, FunctionDef> = new Map();
  private globals: Map<string, number> = new Map();
  private labelCounter: number = 0;
  private stringCounter: number = 0;
  private currentLocals: Map<string, number> = new Map();
  private stackOffset: number = 0;

  private emit(line: string): void {
    this.output.push(line);
  }

  private emitData(line: string): void {
    this.dataSection.push(line);
  }

  private newLabel(prefix: string = 'L'): string {
    return `${prefix}${this.labelCounter++}`;
  }

  private addString(value: string): string {
    const label = `str${this.stringCounter++}`;
    // Escape string for assembly
    const escaped = value
      .replace(/\\/g, '\\\\')
      .replace(/"/g, '\\"')
      .replace(/\n/g, '\\n')
      .replace(/\t/g, '\\t');
    this.emitData(`${label}: .asciz "${escaped}"`);
    return label;
  }

  generate(program: ASTNode[]): string {
    // First pass: collect function definitions
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
                this.functions.set(funcName, {
                  name: funcName,
                  params,
                  body: list.elements[2]
                });
              }
            }
          }
        }
      }
    }

    // Generate code
    this.emit('.section __TEXT,__text');
    this.emit('.global _main');
    this.emit('.align 4');
    this.emit('');

    // Generate user-defined functions
    for (const [name, func] of this.functions) {
      this.generateFunction(func);
    }

    // Generate main
    this.emit('_main:');
    this.emit('    stp x29, x30, [sp, #-16]!');
    this.emit('    mov x29, sp');

    // Generate top-level expressions (skip function definitions)
    for (const node of program) {
      if (node.type === 'list') {
        const list = node as ListNode;
        if (list.elements.length > 0 && list.elements[0].type === 'symbol') {
          const op = (list.elements[0] as SymbolNode).name;
          if (op === 'define') {
            const nameOrSig = list.elements[1];
            if (nameOrSig.type === 'list') {
              continue; // Skip function definitions
            }
          }
        }
      }
      this.generateExpr(node);
    }

    // Return 0 from main
    this.emit('    mov x0, #0');
    this.emit('    ldp x29, x30, [sp], #16');
    this.emit('    ret');
    this.emit('');

    // Print integer helper function
    // Converts integer to string and prints using puts
    this.emit('_print_int:');
    this.emit('    stp x29, x30, [sp, #-16]!');
    this.emit('    mov x29, sp');
    this.emit('    sub sp, sp, #32');         // Buffer for digits
    this.emit('    mov x9, x0');              // Save number in x9
    this.emit('    add x10, sp, #30');        // Point to end of buffer
    this.emit('    mov x11, #0');
    this.emit('    strb w11, [x10]');         // Null terminator
    this.emit('    sub x10, x10, #1');
    this.emit('    mov x12, #10');            // Divisor
    this.emit('    cmp x9, #0');
    this.emit('    b.ge _print_int_positive');
    this.emit('    neg x9, x9');              // Make positive
    this.emit('    mov x13, #1');             // Remember it was negative
    this.emit('    b _print_int_loop');
    this.emit('_print_int_positive:');
    this.emit('    mov x13, #0');
    this.emit('    cmp x9, #0');
    this.emit('    b.ne _print_int_loop');
    this.emit('    mov w11, #48');            // Handle zero case
    this.emit('    strb w11, [x10]');
    this.emit('    b _print_int_final');
    this.emit('_print_int_loop:');
    this.emit('    cmp x9, #0');
    this.emit('    b.eq _print_int_check_neg');
    this.emit('    udiv x14, x9, x12');       // x14 = x9 / 10
    this.emit('    msub x11, x14, x12, x9');  // x11 = x9 - (x14 * 10) = remainder
    this.emit('    add x11, x11, #48');       // Convert to ASCII
    this.emit('    strb w11, [x10]');
    this.emit('    sub x10, x10, #1');
    this.emit('    mov x9, x14');
    this.emit('    b _print_int_loop');
    this.emit('_print_int_check_neg:');
    this.emit('    cmp x13, #0');
    this.emit('    b.eq _print_int_done');
    this.emit('    mov w11, #45');            // Minus sign
    this.emit('    strb w11, [x10]');
    this.emit('    b _print_int_final');
    this.emit('_print_int_done:');
    this.emit('    add x10, x10, #1');        // Point to first digit
    this.emit('_print_int_final:');
    this.emit('    mov x0, x10');
    this.emit('    bl _puts');
    this.emit('    add sp, sp, #32');
    this.emit('    ldp x29, x30, [sp], #16');
    this.emit('    ret');
    this.emit('');

    // Print string helper function (uses puts which adds newline)
    this.emit('_print_str:');
    this.emit('    stp x29, x30, [sp, #-16]!');
    this.emit('    mov x29, sp');
    this.emit('    bl _puts');
    this.emit('    ldp x29, x30, [sp], #16');
    this.emit('    ret');
    this.emit('');

    // Data section
    this.emit('.section __DATA,__data');
    this.emit('fmt_int: .asciz "%lld\\n"');
    this.emit('fmt_str: .asciz "%s\\n"');
    for (const line of this.dataSection) {
      this.emit(line);
    }

    return this.output.join('\n');
  }

  private generateFunction(func: FunctionDef): void {
    this.emit(`_lisp_${func.name}:`);
    this.emit('    stp x29, x30, [sp, #-16]!');
    this.emit('    mov x29, sp');

    // Save parameters to stack using frame-pointer relative addressing
    this.currentLocals = new Map();
    this.stackOffset = 0;

    const numParams = func.params.length;
    if (numParams > 0) {
      const stackSize = Math.ceil(numParams / 2) * 16;
      this.emit(`    sub sp, sp, #${stackSize}`);

      // Store parameters at negative offsets from frame pointer
      for (let i = 0; i < numParams; i++) {
        const offset = (i + 1) * 8;  // -8, -16, -24, ...
        this.currentLocals.set(func.params[i], offset);
        this.emit(`    str x${i}, [x29, #-${offset}]`);
      }
    }

    // Generate function body
    this.generateExpr(func.body);

    // Restore stack and return (result is in x0)
    if (numParams > 0) {
      const stackSize = Math.ceil(numParams / 2) * 16;
      this.emit(`    add sp, sp, #${stackSize}`);
    }
    this.emit('    ldp x29, x30, [sp], #16');
    this.emit('    ret');
    this.emit('');

    this.currentLocals = new Map();
    this.stackOffset = 0;
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
    const value = node.value;
    if (value >= 0 && value < 65536) {
      this.emit(`    mov x0, #${value}`);
    } else if (value < 0 && value > -65536) {
      this.emit(`    mov x0, #${-value}`);
      this.emit('    neg x0, x0');
    } else {
      // For larger numbers, load in parts
      const low = value & 0xFFFF;
      const high = (value >> 16) & 0xFFFF;
      this.emit(`    mov x0, #${low}`);
      if (high !== 0) {
        this.emit(`    movk x0, #${high}, lsl #16`);
      }
    }
  }

  private generateSymbol(node: SymbolNode): void {
    const name = node.name;

    // Check if it's a local variable (using frame-pointer relative addressing)
    if (this.currentLocals.has(name)) {
      const offset = this.currentLocals.get(name)!;
      this.emit(`    ldr x0, [x29, #-${offset}]`);
      return;
    }

    // Check if it's a global
    if (this.globals.has(name)) {
      this.emit(`    adrp x0, _global_${name}@PAGE`);
      this.emit(`    add x0, x0, _global_${name}@PAGEOFF`);
      this.emit('    ldr x0, [x0]');
      return;
    }

    throw new Error(`Undefined symbol: ${name}`);
  }

  private generateString(node: StringNode): void {
    const label = this.addString(node.value);
    this.emit(`    adrp x0, ${label}@PAGE`);
    this.emit(`    add x0, x0, ${label}@PAGEOFF`);
  }

  private generateList(node: ListNode): void {
    if (node.elements.length === 0) {
      this.emit('    mov x0, #0');
      return;
    }

    const first = node.elements[0];
    if (first.type !== 'symbol') {
      throw new Error('First element of list must be a symbol');
    }

    const op = (first as SymbolNode).name;
    const args = node.elements.slice(1);

    switch (op) {
      case '+':
        this.generateArithmetic(args, 'add');
        break;
      case '-':
        this.generateArithmetic(args, 'sub');
        break;
      case '*':
        this.generateArithmetic(args, 'mul');
        break;
      case '/':
        this.generateArithmetic(args, 'sdiv');
        break;
      case '%':
      case 'mod':
        this.generateMod(args);
        break;
      case '<':
        this.generateComparison(args, 'lt');
        break;
      case '>':
        this.generateComparison(args, 'gt');
        break;
      case '<=':
        this.generateComparison(args, 'le');
        break;
      case '>=':
        this.generateComparison(args, 'ge');
        break;
      case '=':
        this.generateComparison(args, 'eq');
        break;
      case 'if':
        this.generateIf(args);
        break;
      case 'print':
        this.generatePrint(args);
        break;
      case 'define':
        this.generateDefine(args);
        break;
      case 'begin':
        this.generateBegin(args);
        break;
      case 'and':
        this.generateAnd(args);
        break;
      case 'or':
        this.generateOr(args);
        break;
      case 'not':
        this.generateNot(args);
        break;
      default:
        // Try to call a user-defined function
        if (this.functions.has(op)) {
          this.generateCall(op, args);
        } else {
          throw new Error(`Unknown operator: ${op}`);
        }
    }
  }

  private generateArithmetic(args: ASTNode[], instruction: string): void {
    if (args.length === 0) {
      this.emit('    mov x0, #0');
      return;
    }

    if (args.length === 1) {
      this.generateExpr(args[0]);
      if (instruction === 'sub') {
        this.emit('    neg x0, x0');
      }
      return;
    }

    // Evaluate first argument
    this.generateExpr(args[0]);
    this.emit('    str x0, [sp, #-16]!');

    // Process remaining arguments
    for (let i = 1; i < args.length; i++) {
      this.generateExpr(args[i]);
      this.emit('    ldr x1, [sp], #16');
      this.emit(`    ${instruction} x0, x1, x0`);
      if (i < args.length - 1) {
        this.emit('    str x0, [sp, #-16]!');
      }
    }
  }

  private generateMod(args: ASTNode[]): void {
    if (args.length !== 2) {
      throw new Error('mod requires exactly 2 arguments');
    }

    this.generateExpr(args[0]);
    this.emit('    str x0, [sp, #-16]!');
    this.generateExpr(args[1]);
    this.emit('    ldr x1, [sp], #16');
    this.emit('    sdiv x2, x1, x0');
    this.emit('    msub x0, x2, x0, x1');
  }

  private generateComparison(args: ASTNode[], cond: string): void {
    if (args.length !== 2) {
      throw new Error(`Comparison requires exactly 2 arguments`);
    }

    this.generateExpr(args[0]);
    this.emit('    str x0, [sp, #-16]!');
    this.generateExpr(args[1]);
    this.emit('    ldr x1, [sp], #16');
    this.emit('    cmp x1, x0');
    this.emit(`    cset x0, ${cond}`);
  }

  private generateIf(args: ASTNode[]): void {
    if (args.length < 2 || args.length > 3) {
      throw new Error('if requires 2 or 3 arguments');
    }

    const elseLabel = this.newLabel('else');
    const endLabel = this.newLabel('endif');

    // Evaluate condition
    this.generateExpr(args[0]);
    this.emit('    cmp x0, #0');
    this.emit(`    b.eq ${elseLabel}`);

    // Then branch
    this.generateExpr(args[1]);
    this.emit(`    b ${endLabel}`);

    // Else branch
    this.emit(`${elseLabel}:`);
    if (args.length === 3) {
      this.generateExpr(args[2]);
    } else {
      this.emit('    mov x0, #0');
    }

    this.emit(`${endLabel}:`);
  }

  private generatePrint(args: ASTNode[]): void {
    if (args.length !== 1) {
      throw new Error('print requires exactly 1 argument');
    }

    const arg = args[0];

    // Check if it's a string literal
    if (arg.type === 'string') {
      this.generateString(arg as StringNode);
      this.emit('    bl _print_str');
    } else {
      this.generateExpr(arg);
      this.emit('    bl _print_int');
    }
  }

  private generateDefine(args: ASTNode[]): void {
    if (args.length !== 2) {
      throw new Error('define requires exactly 2 arguments');
    }

    const nameOrSig = args[0];

    // Function definition is handled in first pass
    if (nameOrSig.type === 'list') {
      return;
    }

    // Variable definition
    if (nameOrSig.type !== 'symbol') {
      throw new Error('define name must be a symbol');
    }

    const name = (nameOrSig as SymbolNode).name;
    this.globals.set(name, 0);

    // Add to data section
    this.emitData(`_global_${name}: .quad 0`);

    // Generate initialization
    this.generateExpr(args[1]);
    this.emit(`    adrp x1, _global_${name}@PAGE`);
    this.emit(`    add x1, x1, _global_${name}@PAGEOFF`);
    this.emit('    str x0, [x1]');
  }

  private generateBegin(args: ASTNode[]): void {
    for (const arg of args) {
      this.generateExpr(arg);
    }
  }

  private generateAnd(args: ASTNode[]): void {
    if (args.length === 0) {
      this.emit('    mov x0, #1');
      return;
    }

    const endLabel = this.newLabel('and_end');

    for (let i = 0; i < args.length; i++) {
      this.generateExpr(args[i]);
      if (i < args.length - 1) {
        this.emit('    cmp x0, #0');
        this.emit(`    b.eq ${endLabel}`);
      }
    }

    this.emit(`${endLabel}:`);
  }

  private generateOr(args: ASTNode[]): void {
    if (args.length === 0) {
      this.emit('    mov x0, #0');
      return;
    }

    const endLabel = this.newLabel('or_end');

    for (let i = 0; i < args.length; i++) {
      this.generateExpr(args[i]);
      if (i < args.length - 1) {
        this.emit('    cmp x0, #0');
        this.emit(`    b.ne ${endLabel}`);
      }
    }

    this.emit(`${endLabel}:`);
  }

  private generateNot(args: ASTNode[]): void {
    if (args.length !== 1) {
      throw new Error('not requires exactly 1 argument');
    }

    this.generateExpr(args[0]);
    this.emit('    cmp x0, #0');
    this.emit('    cset x0, eq');
  }

  private generateCall(funcName: string, args: ASTNode[]): void {
    const func = this.functions.get(funcName);
    if (!func) {
      throw new Error(`Undefined function: ${funcName}`);
    }

    if (args.length !== func.params.length) {
      throw new Error(`Function ${funcName} expects ${func.params.length} arguments, got ${args.length}`);
    }

    // Evaluate arguments and push to stack
    for (let i = args.length - 1; i >= 0; i--) {
      this.generateExpr(args[i]);
      this.emit('    str x0, [sp, #-16]!');
    }

    // Pop arguments into registers x0-x7
    for (let i = 0; i < args.length && i < 8; i++) {
      this.emit(`    ldr x${i}, [sp], #16`);
    }

    // Call the function
    this.emit(`    bl _lisp_${funcName}`);
  }
}

export function generate(program: ASTNode[]): string {
  const generator = new CodeGenerator();
  return generator.generate(program);
}
