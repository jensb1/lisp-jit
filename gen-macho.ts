// Generate a minimal Mach-O binary - static (LC_UNIXTHREAD) approach
import * as fs from 'fs';

class MachOWriter {
  private buf: number[] = [];

  emit8(b: number): void {
    this.buf.push(b & 0xff);
  }

  emit32(i: number): void {
    this.emit8(i);
    this.emit8(i >> 8);
    this.emit8(i >> 16);
    this.emit8(i >> 24);
  }

  emit64(i: bigint): void {
    const lo = Number(i & 0xffffffffn);
    const hi = Number((i >> 32n) & 0xffffffffn);
    this.emit32(lo);
    this.emit32(hi);
  }

  emitString(s: string, len: number): void {
    for (let i = 0; i < len; i++) {
      this.emit8(i < s.length ? s.charCodeAt(i) : 0);
    }
  }

  emitPadding(align: number): void {
    while (this.buf.length % align !== 0) {
      this.emit8(0);
    }
  }

  emitZeros(n: number): void {
    for (let i = 0; i < n; i++) {
      this.emit8(0);
    }
  }

  get position(): number {
    return this.buf.length;
  }

  getBuffer(): Uint8Array {
    return new Uint8Array(this.buf);
  }
}

// Mach-O constants
const MH_MAGIC_64 = 0xfeedfacf;
const CPU_TYPE_ARM64 = 0x0100000c;
const MH_EXECUTE = 2;
const MH_NOUNDEFS = 0x1;
const MH_PIE = 0x200000;

const LC_SEGMENT_64 = 0x19;
const LC_SYMTAB = 0x2;
const LC_BUILD_VERSION = 0x32;
const LC_UNIXTHREAD = 0x5;

// ARM64 thread state constants
const ARM_THREAD_STATE64 = 6;
const ARM_THREAD_STATE64_COUNT = 68;

const VM_PROT_READ = 0x1;
const VM_PROT_EXECUTE = 0x4;
const S_ATTR_PURE_INSTRUCTIONS = 0x80000000;
const S_ATTR_SOME_INSTRUCTIONS = 0x400;
const PLATFORM_MACOS = 1;

// Generate ARM64 code for: mov x0, #120; mov x16, #1; svc #0x80
function generateTestCode(): number[] {
  return [
    // mov x0, #120  (0xD2800F00)
    0x00, 0x0f, 0x80, 0xd2,
    // mov x16, #1   (0xD2800030)
    0x30, 0x00, 0x80, 0xd2,
    // svc #0x80     (0xD4001001)
    0x01, 0x10, 0x00, 0xd4,
  ];
}

function generateStaticMachO(): Uint8Array {
  const w = new MachOWriter();
  const code = generateTestCode();
  const codeSize = code.length;

  // For static binary, put code right after headers (minimal padding)
  // We need to calculate header size based on load commands
  const machHeaderSize = 32;

  // Load commands:
  // LC_SEGMENT_64 __PAGEZERO: 72
  // LC_SEGMENT_64 __TEXT: 152 (72 + 80 section)
  // LC_SEGMENT_64 __LINKEDIT: 72
  // LC_SYMTAB: 24
  // LC_BUILD_VERSION: 24
  // LC_UNIXTHREAD: 288 (8 + 4 + 4 + 272 for thread state)
  // Total: 632

  const loadCmdsSize = 72 + 152 + 72 + 24 + 24 + 288;
  const ncmds = 6;

  // Align header to page boundary for simplicity
  const headerSize = 16384;
  const baseVmaddr = 0x100000000n;
  const textVmaddr = baseVmaddr + BigInt(headerSize);
  const linkeditVmaddr = baseVmaddr + 0x8000n;
  const linkeditFileoff = headerSize + codeSize;
  const textFilesize = headerSize + codeSize;
  const textVmsize = 0x8000;
  const linkeditSize = 80;

  // Static binary: only NOUNDEFS and PIE (no DYLDLINK, no TWOLEVEL)
  const mhFlags = MH_NOUNDEFS | MH_PIE;

  // Mach header (32 bytes)
  w.emit32(MH_MAGIC_64);
  w.emit32(CPU_TYPE_ARM64);
  w.emit32(0); // cpusubtype
  w.emit32(MH_EXECUTE);
  w.emit32(ncmds);
  w.emit32(loadCmdsSize);
  w.emit32(mhFlags);
  w.emit32(0); // reserved

  // LC_SEGMENT_64 __PAGEZERO (72 bytes)
  w.emit32(LC_SEGMENT_64);
  w.emit32(72);
  w.emitString("__PAGEZERO", 16);
  w.emit64(0n);
  w.emit64(baseVmaddr);
  w.emit64(0n);
  w.emit64(0n);
  w.emit32(0); // maxprot
  w.emit32(0); // initprot
  w.emit32(0); // nsects
  w.emit32(0); // flags

  // LC_SEGMENT_64 __TEXT (152 bytes)
  w.emit32(LC_SEGMENT_64);
  w.emit32(152);
  w.emitString("__TEXT", 16);
  w.emit64(baseVmaddr);
  w.emit64(BigInt(textVmsize));
  w.emit64(0n); // fileoff
  w.emit64(BigInt(textFilesize));
  w.emit32(VM_PROT_READ | VM_PROT_EXECUTE);
  w.emit32(VM_PROT_READ | VM_PROT_EXECUTE);
  w.emit32(1); // nsects
  w.emit32(0);

  // __text section (80 bytes)
  w.emitString("__text", 16);
  w.emitString("__TEXT", 16);
  w.emit64(textVmaddr);
  w.emit64(BigInt(codeSize));
  w.emit32(headerSize); // offset
  w.emit32(2); // align 2^2 = 4
  w.emit32(0); // reloff
  w.emit32(0); // nreloc
  w.emit32(S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS);
  w.emit32(0);
  w.emit32(0);
  w.emit32(0);

  // LC_SEGMENT_64 __LINKEDIT (72 bytes)
  w.emit32(LC_SEGMENT_64);
  w.emit32(72);
  w.emitString("__LINKEDIT", 16);
  w.emit64(linkeditVmaddr);
  w.emit64(0x4000n);
  w.emit64(BigInt(linkeditFileoff));
  w.emit64(BigInt(linkeditSize));
  w.emit32(VM_PROT_READ);
  w.emit32(VM_PROT_READ);
  w.emit32(0);
  w.emit32(0);

  // LC_SYMTAB (24 bytes)
  w.emit32(LC_SYMTAB);
  w.emit32(24);
  w.emit32(linkeditFileoff); // symoff
  w.emit32(0); // nsyms
  w.emit32(linkeditFileoff); // stroff
  w.emit32(1); // strsize

  // LC_BUILD_VERSION (24 bytes)
  w.emit32(LC_BUILD_VERSION);
  w.emit32(24);
  w.emit32(PLATFORM_MACOS);
  w.emit32(0xd0000); // minos 13.0
  w.emit32(0xd0000); // sdk 13.0
  w.emit32(0); // ntools

  // LC_UNIXTHREAD (288 bytes)
  // Sets initial thread state including PC to entry point
  w.emit32(LC_UNIXTHREAD);
  w.emit32(288);
  w.emit32(ARM_THREAD_STATE64);  // flavor
  w.emit32(ARM_THREAD_STATE64_COUNT);  // count

  // ARM64 thread state: 68 x 32-bit words = 272 bytes
  // x0-x28 (29 registers * 8 bytes = 232 bytes, but stored as pairs of 32-bit)
  // Then fp, lr, sp, pc, cpsr, pad
  // Total: 33 64-bit values = 264 bytes + 2 32-bit = 272 bytes

  // x0-x28: 29 64-bit registers, all zero
  for (let i = 0; i < 29; i++) {
    w.emit64(0n);
  }
  // fp (x29)
  w.emit64(0n);
  // lr (x30)
  w.emit64(0n);
  // sp
  w.emit64(0n);
  // pc - set to entry point
  w.emit64(textVmaddr);
  // cpsr
  w.emit32(0);
  // padding
  w.emit32(0);

  console.log(`Header position after load commands: ${w.position}`);
  console.log(`Expected: ${machHeaderSize + loadCmdsSize}`);

  // Pad to header size
  w.emitPadding(headerSize);
  console.log(`After padding: ${w.position}`);

  // Emit code
  for (const b of code) {
    w.emit8(b);
  }
  console.log(`After code: ${w.position}`);

  // Emit linkedit data
  w.emit8(0);
  w.emitZeros(linkeditSize - 1);
  console.log(`Final size: ${w.position}`);

  return w.getBuffer();
}

const binary = generateStaticMachO();
fs.writeFileSync('test-macho-static', binary);
fs.chmodSync('test-macho-static', 0o755);
console.log('Generated test-macho-static');
