// ARM64 Binary Assembler - Emits raw machine code

export class ARM64Assembler {
  private buffer: number[] = [];
  private labels: Map<string, number> = new Map();
  private fixups: Array<{ offset: number; label: string; type: 'b' | 'bl' | 'bcond' | 'adr' }> = [];

  // Emit a 32-bit instruction
  emit32(instruction: number): void {
    this.buffer.push(instruction & 0xFF);
    this.buffer.push((instruction >> 8) & 0xFF);
    this.buffer.push((instruction >> 16) & 0xFF);
    this.buffer.push((instruction >> 24) & 0xFF);
  }

  // Get current offset
  get offset(): number {
    return this.buffer.length;
  }

  // Define a label at current position
  label(name: string): void {
    this.labels.set(name, this.offset);
  }

  // Resolve all label references
  resolve(): void {
    for (const fixup of this.fixups) {
      const target = this.labels.get(fixup.label);
      if (target === undefined) {
        throw new Error(`Undefined label: ${fixup.label}`);
      }

      const offset = (target - fixup.offset) >> 2; // Offset in instructions

      // Read existing instruction
      let instruction =
        this.buffer[fixup.offset]! |
        (this.buffer[fixup.offset + 1]! << 8) |
        (this.buffer[fixup.offset + 2]! << 16) |
        (this.buffer[fixup.offset + 3]! << 24);

      if (fixup.type === 'b' || fixup.type === 'bl') {
        // B/BL: imm26 field
        instruction |= (offset & 0x3FFFFFF);
      } else if (fixup.type === 'bcond') {
        // B.cond: imm19 field at bits 5-23
        instruction |= ((offset & 0x7FFFF) << 5);
      } else if (fixup.type === 'adr') {
        // ADR: immlo (bits 29-30) and immhi (bits 5-23)
        const byteOffset = target - fixup.offset;
        instruction |= ((byteOffset & 0x3) << 29);
        instruction |= (((byteOffset >> 2) & 0x7FFFF) << 5);
      }

      // Write back
      this.buffer[fixup.offset] = instruction & 0xFF;
      this.buffer[fixup.offset + 1] = (instruction >> 8) & 0xFF;
      this.buffer[fixup.offset + 2] = (instruction >> 16) & 0xFF;
      this.buffer[fixup.offset + 3] = (instruction >> 24) & 0xFF;
    }
  }

  // Get the generated machine code
  getCode(): Uint8Array {
    this.resolve();
    return new Uint8Array(this.buffer);
  }

  // Reset the assembler
  reset(): void {
    this.buffer = [];
    this.labels.clear();
    this.fixups = [];
  }

  // === Data Processing Instructions ===

  // MOV (immediate) - MOVZ for positive small values
  movImm(rd: number, imm: number): void {
    if (imm >= 0 && imm < 65536) {
      // MOVZ Xd, #imm
      const instruction = 0xD2800000 | (imm << 5) | rd;
      this.emit32(instruction);
    } else if (imm < 0 && imm > -65536) {
      // MOVN Xd, #(~imm)
      const instruction = 0x92800000 | ((~imm & 0xFFFF) << 5) | rd;
      this.emit32(instruction);
    } else {
      // For larger values, use MOVZ + MOVK
      this.movImm(rd, imm & 0xFFFF);
      if ((imm >> 16) & 0xFFFF) {
        this.movk(rd, (imm >> 16) & 0xFFFF, 16);
      }
      if ((imm >> 32) & 0xFFFF) {
        this.movk(rd, (imm >> 32) & 0xFFFF, 32);
      }
      if ((imm >> 48) & 0xFFFF) {
        this.movk(rd, (imm >> 48) & 0xFFFF, 48);
      }
    }
  }

  // MOVK Xd, #imm, LSL #shift
  movk(rd: number, imm: number, shift: number): void {
    const hw = shift >> 4;
    const instruction = 0xF2800000 | (hw << 21) | ((imm & 0xFFFF) << 5) | rd;
    this.emit32(instruction);
  }

  // MOV (register) - ORR Xd, XZR, Xm
  mov(rd: number, rm: number): void {
    const instruction = 0xAA0003E0 | (rm << 16) | rd;
    this.emit32(instruction);
  }

  // ADD Xd, Xn, Xm
  addReg(rd: number, rn: number, rm: number): void {
    const instruction = 0x8B000000 | (rm << 16) | (rn << 5) | rd;
    this.emit32(instruction);
  }

  // ADD Xd, Xn, #imm
  addImm(rd: number, rn: number, imm: number): void {
    const instruction = 0x91000000 | ((imm & 0xFFF) << 10) | (rn << 5) | rd;
    this.emit32(instruction);
  }

  // SUB Xd, Xn, Xm
  subReg(rd: number, rn: number, rm: number): void {
    const instruction = 0xCB000000 | (rm << 16) | (rn << 5) | rd;
    this.emit32(instruction);
  }

  // SUB Xd, Xn, #imm
  subImm(rd: number, rn: number, imm: number): void {
    const instruction = 0xD1000000 | ((imm & 0xFFF) << 10) | (rn << 5) | rd;
    this.emit32(instruction);
  }

  // MUL Xd, Xn, Xm (MADD Xd, Xn, Xm, XZR)
  mul(rd: number, rn: number, rm: number): void {
    const instruction = 0x9B007C00 | (rm << 16) | (rn << 5) | rd;
    this.emit32(instruction);
  }

  // SDIV Xd, Xn, Xm
  sdiv(rd: number, rn: number, rm: number): void {
    const instruction = 0x9AC00C00 | (rm << 16) | (rn << 5) | rd;
    this.emit32(instruction);
  }

  // UDIV Xd, Xn, Xm
  udiv(rd: number, rn: number, rm: number): void {
    const instruction = 0x9AC00800 | (rm << 16) | (rn << 5) | rd;
    this.emit32(instruction);
  }

  // MSUB Xd, Xn, Xm, Xa (Xd = Xa - Xn * Xm)
  msub(rd: number, rn: number, rm: number, ra: number): void {
    const instruction = 0x9B008000 | (rm << 16) | (ra << 10) | (rn << 5) | rd;
    this.emit32(instruction);
  }

  // NEG Xd, Xm (SUB Xd, XZR, Xm)
  neg(rd: number, rm: number): void {
    this.subReg(rd, 31, rm);
  }

  // CMP Xn, Xm (SUBS XZR, Xn, Xm)
  cmpReg(rn: number, rm: number): void {
    const instruction = 0xEB00001F | (rm << 16) | (rn << 5);
    this.emit32(instruction);
  }

  // CMP Xn, #imm (SUBS XZR, Xn, #imm)
  cmpImm(rn: number, imm: number): void {
    const instruction = 0xF100001F | ((imm & 0xFFF) << 10) | (rn << 5);
    this.emit32(instruction);
  }

  // CSET Xd, cond (CSINC Xd, XZR, XZR, invert(cond))
  cset(rd: number, cond: number): void {
    const invertedCond = cond ^ 1;
    const instruction = 0x9A9F07E0 | (invertedCond << 12) | rd;
    this.emit32(instruction);
  }

  // === Memory Instructions ===

  // LDR Xt, [Xn, #imm]
  ldrImm(rt: number, rn: number, imm: number): void {
    const scaledImm = imm >> 3;
    const instruction = 0xF9400000 | ((scaledImm & 0xFFF) << 10) | (rn << 5) | rt;
    this.emit32(instruction);
  }

  // STR Xt, [Xn, #imm]
  strImm(rt: number, rn: number, imm: number): void {
    const scaledImm = imm >> 3;
    const instruction = 0xF9000000 | ((scaledImm & 0xFFF) << 10) | (rn << 5) | rt;
    this.emit32(instruction);
  }

  // LDR Xt, [Xn], #imm (post-index)
  ldrPost(rt: number, rn: number, imm: number): void {
    const instruction = 0xF8400400 | ((imm & 0x1FF) << 12) | (rn << 5) | rt;
    this.emit32(instruction);
  }

  // STR Xt, [Xn, #imm]! (pre-index)
  strPre(rt: number, rn: number, imm: number): void {
    const instruction = 0xF8000C00 | ((imm & 0x1FF) << 12) | (rn << 5) | rt;
    this.emit32(instruction);
  }

  // STRB Wt, [Xn] (store byte)
  strb(rt: number, rn: number, imm: number = 0): void {
    const instruction = 0x39000000 | ((imm & 0xFFF) << 10) | (rn << 5) | rt;
    this.emit32(instruction);
  }

  // STP Xt1, Xt2, [Xn, #imm]!
  stpPre(rt1: number, rt2: number, rn: number, imm: number): void {
    const scaledImm = (imm >> 3) & 0x7F;
    const instruction = 0xA9800000 | (scaledImm << 15) | (rt2 << 10) | (rn << 5) | rt1;
    this.emit32(instruction);
  }

  // LDP Xt1, Xt2, [Xn], #imm
  ldpPost(rt1: number, rt2: number, rn: number, imm: number): void {
    const scaledImm = (imm >> 3) & 0x7F;
    const instruction = 0xA8C00000 | (scaledImm << 15) | (rt2 << 10) | (rn << 5) | rt1;
    this.emit32(instruction);
  }

  // === Branch Instructions ===

  // B label
  b(label: string): void {
    this.fixups.push({ offset: this.offset, label, type: 'b' });
    this.emit32(0x14000000);
  }

  // BL label
  bl(label: string): void {
    this.fixups.push({ offset: this.offset, label, type: 'bl' });
    this.emit32(0x94000000);
  }

  // BLR Xn
  blr(rn: number): void {
    const instruction = 0xD63F0000 | (rn << 5);
    this.emit32(instruction);
  }

  // RET (RET X30)
  ret(): void {
    this.emit32(0xD65F03C0);
  }

  // B.cond label
  bcond(cond: number, label: string): void {
    this.fixups.push({ offset: this.offset, label, type: 'bcond' });
    this.emit32(0x54000000 | cond);
  }

  // Condition codes
  static readonly EQ = 0;   // Equal
  static readonly NE = 1;   // Not equal
  static readonly GE = 10;  // Signed greater or equal
  static readonly LT = 11;  // Signed less than
  static readonly GT = 12;  // Signed greater than
  static readonly LE = 13;  // Signed less or equal

  // === Address Generation ===

  // ADR Xd, label
  adr(rd: number, label: string): void {
    this.fixups.push({ offset: this.offset, label, type: 'adr' });
    this.emit32(0x10000000 | rd);
  }

  // === Utility ===

  // Push X0 to stack
  pushX0(): void {
    this.strPre(0, 31, -16);
  }

  // Pop to X0 from stack
  popX0(): void {
    this.ldrPost(0, 31, 16);
  }

  // Pop to X1 from stack
  popX1(): void {
    this.ldrPost(1, 31, 16);
  }

  // Function prologue
  prologue(): void {
    this.stpPre(29, 30, 31, -16);
    this.mov(29, 31);
  }

  // Function epilogue
  epilogue(): void {
    this.ldpPost(29, 30, 31, 16);
    this.ret();
  }

  // Allocate stack space
  allocStack(size: number): void {
    this.subImm(31, 31, size);
  }

  // Deallocate stack space
  freeStack(size: number): void {
    this.addImm(31, 31, size);
  }
}

// Register constants
export const X0 = 0;
export const X1 = 1;
export const X2 = 2;
export const X3 = 3;
export const X4 = 4;
export const X5 = 5;
export const X6 = 6;
export const X7 = 7;
export const X8 = 8;
export const X9 = 9;
export const X10 = 10;
export const X11 = 11;
export const X12 = 12;
export const X13 = 13;
export const X14 = 14;
export const X28 = 28;  // Globals pointer
export const X29 = 29;  // Frame pointer
export const X30 = 30;  // Link register
export const SP = 31;   // Stack pointer / Zero register
export const XZR = 31;
