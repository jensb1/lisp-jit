#!/usr/bin/env node
// Test standalone Mach-O binary generation
// This script runs the self-hosting compiler to generate a Mach-O binary,
// writes it to a file, and executes it directly

const { execSync, spawnSync } = require('child_process');
const fs = require('fs');
const path = require('path');

console.log('Running self-hosting Lisp compiler...');

// Run the compiler
const output = execSync('node dist/repl.js lisp/self-hosting-compiler.lisp', {
  encoding: 'utf-8',
  maxBuffer: 10 * 1024 * 1024  // 10MB buffer for binary output
});

const lines = output.trim().split('\n');

// First line is byte count
const byteCount = parseInt(lines[0], 10);
console.log(`Mach-O binary size: ${byteCount} bytes`);

if (byteCount < 100 || byteCount > 1048576) {
  console.error('Error: Invalid byte count');
  process.exit(1);
}

// Parse bytes
const bytes = new Uint8Array(byteCount);
for (let i = 0; i < byteCount && i + 1 < lines.length; i++) {
  bytes[i] = parseInt(lines[i + 1], 10);
}

// Verify Mach-O magic (use >>> 0 to get unsigned 32-bit)
const magic = (bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24)) >>> 0;
if (magic !== 0xFEEDFACF) {
  console.error(`Error: Invalid Mach-O magic: 0x${magic.toString(16)}`);
  process.exit(1);
}
console.log('Mach-O header valid (magic: 0xFEEDFACF)');

// Write binary to file
const binaryPath = path.join(__dirname, 'test-binary');
fs.writeFileSync(binaryPath, bytes);
fs.chmodSync(binaryPath, 0o755);
console.log(`Binary written to: ${binaryPath}`);

// Execute it
console.log('\nExecuting binary...');
console.log('Program: (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)');
console.log('Expected exit code: 120 (5! = 120)');
console.log('');

const result = spawnSync(binaryPath, [], {
  encoding: 'utf-8',
  stdio: ['inherit', 'inherit', 'inherit']
});

const exitCode = result.status;
console.log(`\nExit code: ${exitCode}`);

if (exitCode === 120) {
  console.log('\nSUCCESS! Standalone binary works:');
  console.log('  - No TypeScript/Node.js runtime');
  console.log('  - Direct syscalls for exit');
  console.log('  - Complete Mach-O executable');
  console.log('  - Recursive factorial computed correctly');
} else {
  console.log('\nFAILED - expected exit code 120');
}

// Cleanup
// fs.unlinkSync(binaryPath);
