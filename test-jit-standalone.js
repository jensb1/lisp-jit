#!/usr/bin/env node
// Test: Execute the standalone compiler's output through JIT

const { execSync } = require('child_process');
const { JitExecutor } = require('./dist/jit');

console.log('Running self-hosting Lisp compiler...');

// Get the raw code bytes (skip Mach-O header)
// We need to extract just the code, not the Mach-O header

// First, let's create a test that just outputs code (no Mach-O)

const output = execSync('node dist/repl.js lisp/self-hosting-compiler.lisp', {
  encoding: 'utf-8',
  maxBuffer: 10 * 1024 * 1024
});

const lines = output.trim().split('\n');
const byteCount = parseInt(lines[0], 10);

console.log(`Total binary size: ${byteCount} bytes`);

// The Mach-O header is 16384 bytes, code starts after
const headerSize = 16384;
const codeStart = headerSize;
const linkeditSize = 80;
const codeSize = byteCount - headerSize - linkeditSize;

console.log(`Code size: ${codeSize} bytes (starts at offset ${headerSize})`);

// Extract code bytes - lines[i+1] contains byte at offset i
// So bytes at offset headerSize start at lines[headerSize + 1]
const bytes = [];
for (let i = 0; i < codeSize; i++) {
  const lineIndex = headerSize + i + 1;  // +1 because lines[0] is byteCount
  if (lineIndex < lines.length) {
    bytes.push(parseInt(lines[lineIndex], 10));
  }
}

// Debug: Show first 20 bytes
console.log('First 20 extracted bytes:', bytes.slice(0, 20));

// Debug: Show as instructions
console.log('First 5 instructions:');
for (let j = 0; j < 5 && j*4+3 < bytes.length; j++) {
  const word = bytes[j*4] | (bytes[j*4+1] << 8) | (bytes[j*4+2] << 16) | (bytes[j*4+3] << 24);
  console.log('  ' + j + ': 0x' + (word >>> 0).toString(16).padStart(8, '0'));
}

console.log(`Extracted ${bytes.length} bytes of code`);

const code = new Uint8Array(bytes);
const executor = new JitExecutor();
executor.allocate(1024 * 1024, 64 * 1024 * 1024);
executor.write(code);

console.log('\nExecuting...');
console.log('Program: (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)');
console.log('Expected: 120');
console.log('');

const result = executor.execute();
executor.free();

console.log(`Result: ${result}`);

if (result === 120) {
  console.log('\nSUCCESS! Generated code works correctly!');
  console.log('The Mach-O packaging is the issue, not the code generation.');
} else {
  console.log('\nFAILED - unexpected result');
}
