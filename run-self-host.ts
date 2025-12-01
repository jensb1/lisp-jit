#!/usr/bin/env npx tsx
// Run the self-hosting compiler and capture its output as an object file
import * as fs from 'fs';
import { parse } from './src/parser';
import { JitCodeGenerator } from './src/jit-codegen';
import { JitExecutor } from './src/jit';

async function main() {
  const stderr = process.stderr.write.bind(process.stderr);
  stderr('Loading self-hosting compiler...\n');

  // Read the self-hosting compiler source
  const compilerSource = fs.readFileSync('lisp/self-hosting-compiler.lisp', 'utf-8');

  // Set up JIT executor
  const executor = new JitExecutor();
  executor.allocate(4 * 1024 * 1024, 64 * 1024 * 1024);
  const addresses = executor.getAddresses();

  // Parse the compiler
  stderr('Parsing...\n');
  const ast = parse(compilerSource);

  // Compile using JIT codegen
  stderr('Compiling with JIT...\n');
  const codegen = new JitCodeGenerator(addresses);
  const code = codegen.generate(ast);

  // Execute to get output
  executor.write(code);

  // Capture output (first number is byte count, then bytes)
  const output: number[] = [];
  let byteCount = 0;
  let bytesRead = 0;
  let capturing = false;

  const oldLog = console.log;
  console.log = (...args: unknown[]) => {
    const val = Number(args[0]);
    if (!isNaN(val)) {
      if (!capturing) {
        byteCount = val;
        capturing = true;
        stderr(`Expecting ${byteCount} bytes...\n`);
      } else if (bytesRead < byteCount) {
        output.push(val);
        bytesRead++;
        if (bytesRead % 100 === 0) {
          stderr(`Read ${bytesRead}/${byteCount} bytes...\n`);
        }
      }
    }
  };

  stderr('Executing...\n');
  const result = executor.execute();
  console.log = oldLog;

  stderr(`Execution complete, result: ${result}\n`);
  stderr(`Captured ${output.length} bytes\n`);

  if (output.length > 0) {
    // Write as object file
    const objData = new Uint8Array(output);
    fs.writeFileSync('output.o', objData);
    stderr('Written to output.o\n');

    // Verify the object file
    stderr('\nVerifying object file...\n');
    stderr(`Magic: ${objData[0].toString(16)} ${objData[1].toString(16)} ${objData[2].toString(16)} ${objData[3].toString(16)}\n`);

    // Try to link it
    stderr('\nTo link and run:\n');
    stderr('  clang -arch arm64 -o output output.o -Wl,-e,_main\n');
    stderr('  ./output; echo Exit: $?\n');
  } else {
    stderr('No output captured\n');
  }

  executor.free();
}

main().catch(e => {
  console.error('Error:', e.message);
  process.exit(1);
});
