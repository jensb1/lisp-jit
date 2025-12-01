#!/usr/bin/env npx tsx
// Run a Lisp file through the JIT compiler
import * as fs from 'fs';
import { parse } from './src/parser';
import { JitCodeGenerator } from './src/jit-codegen';
import { JitExecutor } from './src/jit';

async function main() {
  const args = process.argv.slice(2);
  const sourceFile = args[0] || 'lisp/repl.lisp';

  if (!fs.existsSync(sourceFile)) {
    console.error(`File not found: ${sourceFile}`);
    process.exit(1);
  }

  console.error(`Running ${sourceFile}...`);

  // Read source
  const source = fs.readFileSync(sourceFile, 'utf-8');

  // Set up JIT executor
  const executor = new JitExecutor();
  executor.allocate(4 * 1024 * 1024, 64 * 1024 * 1024);
  const addresses = executor.getAddresses();

  // Parse
  console.error('Parsing...');
  const ast = parse(source);

  // Compile
  console.error('Compiling...');
  const codegen = new JitCodeGenerator(addresses);
  const code = codegen.generate(ast);

  // Execute
  executor.write(code);
  console.error('Executing...');
  console.error('---');

  const result = executor.execute();

  console.error('---');
  console.error(`Exit code: ${result}`);

  executor.free();
}

main().catch(e => {
  console.error('Error:', e.message);
  process.exit(1);
});
