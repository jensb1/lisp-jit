// Generate a Mach-O object file that can be linked by ld
import * as fs from 'fs';

class ObjectWriter {
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

// Mach-O object file constants
const MH_MAGIC_64 = 0xfeedfacf;
const CPU_TYPE_ARM64 = 0x0100000c;
const MH_OBJECT = 1;  // Object file, not executable
const MH_SUBSECTIONS_VIA_SYMBOLS = 0x2000;

const LC_SEGMENT_64 = 0x19;
const LC_SYMTAB = 0x2;
const LC_BUILD_VERSION = 0x32;

const VM_PROT_READ = 0x1;
const VM_PROT_EXECUTE = 0x4;
const S_ATTR_PURE_INSTRUCTIONS = 0x80000000;
const S_ATTR_SOME_INSTRUCTIONS = 0x400;
const PLATFORM_MACOS = 1;

const N_EXT = 0x01;  // External symbol
const N_SECT = 0x0e;  // Symbol defined in section

// Simple test: exit(42)
function generateTestCode(): number[] {
  return [
    // mov x0, #42  (exit code)
    0x40, 0x05, 0x80, 0xd2,
    // mov x16, #1  (exit syscall)
    0x30, 0x00, 0x80, 0xd2,
    // svc #0x80
    0x01, 0x10, 0x00, 0xd4,
  ];
}

function generateObjectFile(): Uint8Array {
  const w = new ObjectWriter();
  const code = generateTestCode();
  const codeSize = code.length;

  // Mach-O object file layout:
  // 1. Mach header (32 bytes)
  // 2. Load commands
  // 3. __TEXT,__text section content
  // 4. Symbol table
  // 5. String table

  const machHeaderSize = 32;

  // Load commands:
  // LC_SEGMENT_64 __TEXT: 152 (72 + 80 section)
  // LC_SYMTAB: 24
  // LC_BUILD_VERSION: 24
  // Total: 200

  const loadCmdsSize = 152 + 24 + 24;
  const ncmds = 3;
  const headerTotal = machHeaderSize + loadCmdsSize;

  // Code starts right after headers (aligned to 4)
  let codeOffset = headerTotal;
  while (codeOffset % 4 !== 0) codeOffset++;

  // Symbol table after code
  const symtabOffset = codeOffset + codeSize;
  const symtabSize = 16;  // One nlist_64 entry (16 bytes)

  // String table after symbol table
  const strtabOffset = symtabOffset + symtabSize;
  const symbolName = "_main\0";  // Symbol name with null terminator
  const strtabSize = 1 + symbolName.length;  // Leading null + symbol

  // Total file size
  const fileSize = strtabOffset + strtabSize;

  // Mach header (32 bytes)
  w.emit32(MH_MAGIC_64);
  w.emit32(CPU_TYPE_ARM64);
  w.emit32(0); // cpusubtype
  w.emit32(MH_OBJECT);  // Object file type
  w.emit32(ncmds);
  w.emit32(loadCmdsSize);
  w.emit32(MH_SUBSECTIONS_VIA_SYMBOLS);
  w.emit32(0); // reserved

  // LC_SEGMENT_64 __TEXT (152 bytes)
  // For object files, segment name is empty and vmaddr is 0
  w.emit32(LC_SEGMENT_64);
  w.emit32(152);
  w.emitString("", 16);  // Empty segment name for object files
  w.emit64(0n);  // vmaddr = 0 for object files
  w.emit64(BigInt(codeSize));  // vmsize = size of code
  w.emit64(BigInt(codeOffset));  // fileoff
  w.emit64(BigInt(codeSize));  // filesize
  w.emit32(VM_PROT_READ | VM_PROT_EXECUTE);
  w.emit32(VM_PROT_READ | VM_PROT_EXECUTE);
  w.emit32(1); // nsects
  w.emit32(0);

  // __text section (80 bytes)
  w.emitString("__text", 16);
  w.emitString("__TEXT", 16);
  w.emit64(0n);  // addr = 0 for object files
  w.emit64(BigInt(codeSize));
  w.emit32(codeOffset);
  w.emit32(2); // align 2^2 = 4
  w.emit32(0); // reloff
  w.emit32(0); // nreloc
  w.emit32(S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS);
  w.emit32(0);
  w.emit32(0);
  w.emit32(0);

  // LC_SYMTAB (24 bytes)
  w.emit32(LC_SYMTAB);
  w.emit32(24);
  w.emit32(symtabOffset);
  w.emit32(1);  // 1 symbol
  w.emit32(strtabOffset);
  w.emit32(strtabSize);

  // LC_BUILD_VERSION (24 bytes)
  w.emit32(LC_BUILD_VERSION);
  w.emit32(24);
  w.emit32(PLATFORM_MACOS);
  w.emit32(0xd0000); // minos 13.0
  w.emit32(0xd0000); // sdk 13.0
  w.emit32(0); // ntools

  // Pad to code offset
  while (w.position < codeOffset) {
    w.emit8(0);
  }

  // Emit code
  for (const b of code) {
    w.emit8(b);
  }

  // Pad to symbol table offset
  while (w.position < symtabOffset) {
    w.emit8(0);
  }

  // Symbol table entry (nlist_64 structure, 16 bytes)
  // _main symbol
  w.emit32(1);  // n_strx: offset into string table (after leading null)
  w.emit8(N_SECT | N_EXT);  // n_type: external symbol defined in section
  w.emit8(1);  // n_sect: section 1 (__text)
  w.emit32(0);  // n_desc (16-bit, but aligned)
  // Actually n_desc is 16 bits, let me fix this
  // nlist_64: n_strx(4), n_type(1), n_sect(1), n_desc(2), n_value(8) = 16 bytes

  // Let me rewrite the nlist_64 correctly
  w.buf.length = symtabOffset;  // Reset to symtab offset
  w.emit32(1);  // n_strx
  w.emit8(N_SECT | N_EXT);  // n_type
  w.emit8(1);  // n_sect
  w.emit8(0);  // n_desc low byte
  w.emit8(0);  // n_desc high byte
  w.emit64(0n);  // n_value: address (0 for object file)

  // String table
  w.emit8(0);  // Leading null
  w.emitString("_main", 6);  // _main\0

  console.log(`Generated object file: ${w.position} bytes`);
  console.log(`Code at offset ${codeOffset}, ${codeSize} bytes`);
  console.log(`Symtab at offset ${symtabOffset}`);
  console.log(`Strtab at offset ${strtabOffset}`);

  return w.getBuffer();
}

const binary = generateObjectFile();
fs.writeFileSync('test.o', binary);
console.log('Generated test.o');
