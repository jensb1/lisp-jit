#!/usr/bin/env node
// Lisp Compiler Main Entry Point

import * as fs from 'fs';
import * as path from 'path';
import { execSync } from 'child_process';
import { parse } from './parser';
import { generate } from './codegen';

function compile(inputFile: string, outputFile?: string): void {
  // Read source file
  const source = fs.readFileSync(inputFile, 'utf-8');

  // Parse
  console.log(`Parsing ${inputFile}...`);
  const ast = parse(source);

  // Generate assembly
  console.log('Generating ARM64 assembly...');
  const asm = generate(ast);

  // Determine output paths
  const baseName = path.basename(inputFile, path.extname(inputFile));
  const dirName = path.dirname(inputFile);
  const asmFile = path.join(dirName, `${baseName}.s`);
  const objFile = path.join(dirName, `${baseName}.o`);
  const exeFile = outputFile || path.join(dirName, baseName);

  // Write assembly
  fs.writeFileSync(asmFile, asm);
  console.log(`Assembly written to ${asmFile}`);

  // Assemble
  console.log('Assembling...');
  try {
    execSync(`as -o "${objFile}" "${asmFile}"`, { stdio: 'inherit' });
  } catch (e) {
    console.error('Assembly failed');
    process.exit(1);
  }

  // Link
  console.log('Linking...');
  try {
    execSync(`ld -o "${exeFile}" "${objFile}" -lSystem -syslibroot \`xcrun --show-sdk-path\` -e _main -arch arm64`, {
      stdio: 'inherit',
      shell: '/bin/bash'
    });
  } catch (e) {
    console.error('Linking failed');
    process.exit(1);
  }

  // Clean up intermediate files
  fs.unlinkSync(objFile);

  console.log(`Executable created: ${exeFile}`);
  console.log(`\nRun with: ./${path.relative(process.cwd(), exeFile)}`);
}

function main(): void {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.log('Usage: lisp-compiler <input.lisp> [output]');
    console.log('');
    console.log('Options:');
    console.log('  <input.lisp>  Input Lisp source file');
    console.log('  [output]      Optional output executable name');
    console.log('');
    console.log('Example:');
    console.log('  lisp-compiler hello.lisp');
    console.log('  ./hello');
    process.exit(1);
  }

  const inputFile = args[0];
  const outputFile = args[1];

  if (!fs.existsSync(inputFile)) {
    console.error(`Error: File not found: ${inputFile}`);
    process.exit(1);
  }

  try {
    compile(inputFile, outputFile);
  } catch (e: any) {
    console.error(`Compilation error: ${e.message}`);
    process.exit(1);
  }
}

main();
