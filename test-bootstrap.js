#!/usr/bin/env node
// Test the bootstrap - capture output from Lisp code generator and execute it

const { execSync } = require('child_process');
const { JitExecutor } = require('./dist/jit');

// Run the Lisp code generator and capture its output
const output = execSync('node dist/repl.js lisp/test-compile-simple.lisp', { encoding: 'utf-8' });
const lines = output.trim().split('\n');

// Parse the output - first line is byte count, rest are bytes
const byteCount = parseInt(lines[0], 10);
console.log(`Byte count: ${byteCount}`);

const bytes = [];
for (let i = 1; i <= byteCount && i < lines.length; i++) {
  bytes.push(parseInt(lines[i], 10));
}

console.log(`Captured ${bytes.length} bytes`);
console.log('Bytes:', bytes.slice(0, 32).join(', '));

// Execute the generated code
const code = new Uint8Array(bytes);

const executor = new JitExecutor();
executor.allocate(1024 * 1024, 64 * 1024 * 1024);
executor.write(code);

console.log('');
console.log('Executing Lisp-generated machine code...');
const result = executor.execute();
console.log(`Result: ${result}`);
console.log(`Expected: 42`);
console.log(result === 42 ? 'SUCCESS - Bootstrap works!' : 'FAILED');

executor.free();
