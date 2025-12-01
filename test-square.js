#!/usr/bin/env node
// Test: Execute the Lisp-compiled square(6) = 36 program

const { execSync } = require('child_process');
const { JitExecutor } = require('./dist/jit');

// Run the Lisp code generator
const output = execSync('node dist/repl.js lisp/test-full-bootstrap.lisp', { encoding: 'utf-8' });
const lines = output.trim().split('\n');

const byteCount = parseInt(lines[0], 10);
const bytes = [];
for (let i = 1; i <= byteCount && i < lines.length; i++) {
  bytes.push(parseInt(lines[i], 10));
}

console.log(`Generated ${bytes.length} bytes of machine code`);
console.log('');
console.log('Program: (define (square x) (* x x)) (square 6)');
console.log('Expected result: 36');
console.log('');

const code = new Uint8Array(bytes);
const executor = new JitExecutor();
executor.allocate(1024 * 1024, 64 * 1024 * 1024);
executor.write(code);

console.log('Executing...');
const result = executor.execute();
executor.free();

console.log(`Result: ${result}`);
console.log('');
if (result === 36) {
  console.log('SUCCESS! Full bootstrap compiler works:');
  console.log('  - Lexer tokenized the source');
  console.log('  - Parser built the AST');
  console.log('  - Code generator produced ARM64 machine code');
  console.log('  - Function with parameter and multiplication worked');
  console.log('');
  console.log('The self-hosting Lisp compiler pipeline is complete!');
} else {
  console.log('FAILED - unexpected result');
}
