#!/usr/bin/env node
// Lisp JIT REPL

import * as readline from 'readline';
import { parse } from './parser';
import { JitCodeGenerator } from './jit-codegen';
import { JitExecutor } from './jit';

class LispRepl {
  private executor: JitExecutor;
  private rl: readline.Interface;
  private buffer: string = '';

  constructor() {
    this.executor = new JitExecutor();
    this.executor.allocate(4 * 1024 * 1024, 64 * 1024 * 1024); // 4MB code, 64MB heap

    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
    });
  }

  private countParens(str: string): number {
    let count = 0;
    for (const ch of str) {
      if (ch === '(') count++;
      else if (ch === ')') count--;
    }
    return count;
  }

  private evaluate(source: string): void {
    try {
      const ast = parse(source);

      const codegen = new JitCodeGenerator(this.executor.getAddresses());
      const code = codegen.generate(ast);

      this.executor.write(code);
      const result = this.executor.execute();

      // Only print result if it's not 0 (which is the default return)
      // and there was no print statement
      if (result !== 0) {
        console.log(`=> ${result}`);
      }
    } catch (e: any) {
      console.error(`Error: ${e.message}`);
    }
  }

  run(): void {
    console.log('Lisp JIT REPL');
    console.log('Type expressions to evaluate. Ctrl+D to exit.');
    console.log('');

    const prompt = () => {
      const p = this.buffer ? '... ' : '> ';
      this.rl.question(p, (line) => {
        if (line === undefined) {
          // EOF
          console.log('\nBye!');
          this.executor.free();
          this.rl.close();
          return;
        }

        this.buffer += line + '\n';

        // Check if we have balanced parentheses
        const parenCount = this.countParens(this.buffer);

        if (parenCount <= 0) {
          const input = this.buffer.trim();
          this.buffer = '';

          if (input) {
            this.evaluate(input);
          }
        }

        prompt();
      });
    };

    prompt();
  }
}

// Alternative: Run a file in JIT mode
function runFile(filename: string): void {
  const fs = require('fs');

  const source = fs.readFileSync(filename, 'utf-8');

  const executor = new JitExecutor();
  executor.allocate(4 * 1024 * 1024, 64 * 1024 * 1024); // 4MB code, 64MB heap

  try {
    const ast = parse(source);
    const codegen = new JitCodeGenerator(executor.getAddresses());

    const code = codegen.generate(ast);
    executor.write(code);
    executor.execute();
  } catch (e: any) {
    console.error(`Error: ${e.message}`);
    process.exit(1);
  } finally {
    executor.free();
  }
}

// Main
const args = process.argv.slice(2);

if (args.length === 0) {
  // Start REPL
  const repl = new LispRepl();
  repl.run();
} else if (args[0] === '--help' || args[0] === '-h') {
  console.log('Usage: lisp-jit [file.lisp]');
  console.log('');
  console.log('If no file is given, starts an interactive REPL.');
  console.log('If a file is given, compiles and runs it with JIT.');
} else {
  // Run file
  runFile(args[0]);
}
