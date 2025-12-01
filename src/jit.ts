// JIT Wrapper - TypeScript interface for the native JIT runtime

import * as path from 'path';

interface JitRuntimeNative {
  allocateMemory(size: number): boolean;
  allocateHeap(size: number): boolean;
  writeCode(code: Buffer): boolean;
  execute(): number;
  freeMemory(): boolean;

  // Runtime function addresses
  getPrintIntAddr(): bigint;
  getPrintStrAddr(): bigint;
  getPrintAddr(): bigint;
  getAllocAddr(): bigint;
  getConsAddr(): bigint;
  getCarAddr(): bigint;
  getCdrAddr(): bigint;
  getConspAddr(): bigint;
  getNullAddr(): bigint;
  getMakeStringAddr(): bigint;
  getStringLengthAddr(): bigint;
  getStringRefAddr(): bigint;
  getStringDataAddr(): bigint;
  getSubstringAddr(): bigint;
  getStringAppendAddr(): bigint;
  getStringEqualAddr(): bigint;
  getIntToStringAddr(): bigint;
  getStringToIntAddr(): bigint;
  getMakeCharStringAddr(): bigint;
  getCharWhitespaceAddr(): bigint;
  getCharDigitAddr(): bigint;
  getCharAlphaAddr(): bigint;
  getMakeVectorAddr(): bigint;
  getVectorRefAddr(): bigint;
  getVectorSetAddr(): bigint;
  getVectorLengthAddr(): bigint;
  getReadFileAddr(): bigint;
  getWriteFileAddr(): bigint;
  getErrorAddr(): bigint;
  getBitandAddr(): bigint;
  getBitorAddr(): bigint;
  getBitxorAddr(): bigint;
  getBitnotAddr(): bigint;
  getLslAddr(): bigint;
  getAsrAddr(): bigint;
  getLsrAddr(): bigint;
}

let runtime: JitRuntimeNative | null = null;

export function loadRuntime(): JitRuntimeNative {
  if (runtime) return runtime;

  try {
    const modulePath = path.join(__dirname, '..', 'build', 'Release', 'jit_runtime.node');
    runtime = require(modulePath) as JitRuntimeNative;
    return runtime;
  } catch (e) {
    throw new Error(
      'JIT runtime not built. Run: npm run build:native\n' +
      'Error: ' + (e as Error).message
    );
  }
}

export interface RuntimeAddresses {
  printInt: bigint;
  printStr: bigint;
  print: bigint;
  alloc: bigint;
  cons: bigint;
  car: bigint;
  cdr: bigint;
  consp: bigint;
  null: bigint;
  makeString: bigint;
  stringLength: bigint;
  stringRef: bigint;
  stringData: bigint;
  substring: bigint;
  stringAppend: bigint;
  stringEqual: bigint;
  intToString: bigint;
  stringToInt: bigint;
  makeCharString: bigint;
  charWhitespace: bigint;
  charDigit: bigint;
  charAlpha: bigint;
  makeVector: bigint;
  vectorRef: bigint;
  vectorSet: bigint;
  vectorLength: bigint;
  readFile: bigint;
  writeFile: bigint;
  error: bigint;
  bitand: bigint;
  bitor: bigint;
  bitxor: bigint;
  bitnot: bigint;
  lsl: bigint;
  asr: bigint;
  lsr: bigint;
}

export class JitExecutor {
  private runtime: JitRuntimeNative;
  private addresses: RuntimeAddresses;

  constructor() {
    this.runtime = loadRuntime();
    this.addresses = {
      printInt: this.runtime.getPrintIntAddr(),
      printStr: this.runtime.getPrintStrAddr(),
      print: this.runtime.getPrintAddr(),
      alloc: this.runtime.getAllocAddr(),
      cons: this.runtime.getConsAddr(),
      car: this.runtime.getCarAddr(),
      cdr: this.runtime.getCdrAddr(),
      consp: this.runtime.getConspAddr(),
      null: this.runtime.getNullAddr(),
      makeString: this.runtime.getMakeStringAddr(),
      stringLength: this.runtime.getStringLengthAddr(),
      stringRef: this.runtime.getStringRefAddr(),
      stringData: this.runtime.getStringDataAddr(),
      substring: this.runtime.getSubstringAddr(),
      stringAppend: this.runtime.getStringAppendAddr(),
      stringEqual: this.runtime.getStringEqualAddr(),
      intToString: this.runtime.getIntToStringAddr(),
      stringToInt: this.runtime.getStringToIntAddr(),
      makeCharString: this.runtime.getMakeCharStringAddr(),
      charWhitespace: this.runtime.getCharWhitespaceAddr(),
      charDigit: this.runtime.getCharDigitAddr(),
      charAlpha: this.runtime.getCharAlphaAddr(),
      makeVector: this.runtime.getMakeVectorAddr(),
      vectorRef: this.runtime.getVectorRefAddr(),
      vectorSet: this.runtime.getVectorSetAddr(),
      vectorLength: this.runtime.getVectorLengthAddr(),
      readFile: this.runtime.getReadFileAddr(),
      writeFile: this.runtime.getWriteFileAddr(),
      error: this.runtime.getErrorAddr(),
      bitand: this.runtime.getBitandAddr(),
      bitor: this.runtime.getBitorAddr(),
      bitxor: this.runtime.getBitxorAddr(),
      bitnot: this.runtime.getBitnotAddr(),
      lsl: this.runtime.getLslAddr(),
      asr: this.runtime.getAsrAddr(),
      lsr: this.runtime.getLsrAddr(),
    };
  }

  allocate(codeSize: number = 1024 * 1024, heapSize: number = 64 * 1024 * 1024): void {
    if (!this.runtime.allocateMemory(codeSize)) {
      throw new Error('Failed to allocate JIT memory');
    }
    if (!this.runtime.allocateHeap(heapSize)) {
      throw new Error('Failed to allocate heap');
    }
  }

  write(code: Uint8Array): void {
    const buffer = Buffer.from(code);
    if (!this.runtime.writeCode(buffer)) {
      throw new Error('Failed to write code to JIT memory');
    }
  }

  execute(): number {
    return this.runtime.execute();
  }

  free(): void {
    this.runtime.freeMemory();
  }

  getAddresses(): RuntimeAddresses {
    return this.addresses;
  }

  // Legacy accessors
  getPrintIntAddress(): bigint { return this.addresses.printInt; }
  getPrintStrAddress(): bigint { return this.addresses.printStr; }
}
