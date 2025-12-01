#!/usr/bin/env node
// Test: Execute the Lisp-compiled factorial(5) = 120 program

const { execSync } = require('child_process');
const { JitExecutor } = require('./dist/jit');

// Run the Lisp code generator
const output = execSync('node dist/repl.js lisp/test-factorial.lisp', { encoding: 'utf-8' });
const lines = output.trim().split('\n');

const byteCount = parseInt(lines[0], 10);
const bytes = [];
for (let i = 1; i <= byteCount && i < lines.length; i++) {
  bytes.push(parseInt(lines[i], 10));
}

console.log(`Generated ${bytes.length} bytes of machine code`);
console.log('');
console.log('Program: (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)');
console.log('Expected result: 120');
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
if (result === 120) {
  console.log('SUCCESS! Recursive function compiled correctly:');
  console.log('  - Recursive factorial function');
  console.log('  - Conditional branching (if)');
  console.log('  - Comparison operator (=)');
  console.log('  - Arithmetic (*, -)');
  console.log('  - Recursive call');
  console.log('');
  console.log('The self-hosting Lisp compiler handles recursion!');
} else {
  console.log('FAILED - unexpected result');
}
