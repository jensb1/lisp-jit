#!/usr/bin/env node
// Test: Execute the Lisp-compiled double(21) = 42 program

const { execSync } = require('child_process');
const { JitExecutor } = require('./dist/jit');

// Run the Lisp code generator
const output = execSync('node dist/repl.js lisp/working-bootstrap.lisp', { encoding: 'utf-8' });
const lines = output.trim().split('\n');

const byteCount = parseInt(lines[0], 10);
const bytes = [];
for (let i = 1; i <= byteCount && i < lines.length; i++) {
  bytes.push(parseInt(lines[i], 10));
}

console.log(`Generated ${bytes.length} bytes of machine code`);
console.log('');
console.log('Program: (define (double x) (* x 2)) (double 21)');
console.log('Expected result: 42');
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
if (result === 42) {
  console.log('SUCCESS! The Lisp compiler correctly compiled and executed a program with:');
  console.log('  - Function definition (double)');
  console.log('  - Parameter passing');
  console.log('  - Arithmetic operation (*)');
  console.log('  - Function call');
  console.log('');
  console.log('The self-hosting Lisp compiler is working!');
} else {
  console.log('FAILED - unexpected result');
}
