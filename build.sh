#!/bin/bash
# Build Lisp programs using the self-hosting compiler
# Usage: ./build.sh [source.lisp] [output]

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Default source is the factorial test embedded in self-hosting-compiler.lisp
SOURCE="${1:-lisp/self-hosting-compiler.lisp}"
OUTPUT="${2:-output}"

echo "Building Lisp program..."
echo "  Source: $SOURCE"
echo "  Output: $OUTPUT"
echo ""

# Run the self-hosting compiler and capture output
echo "Step 1: Running self-hosting compiler..."
BYTE_COUNT=$(npx tsx run-self-host.ts 2>/dev/null | head -1)
echo "  Generated $BYTE_COUNT bytes of machine code"

# Convert output to object file
echo "Step 2: Creating object file..."
npx tsx run-self-host.ts 2>/dev/null | tail -n +2 | head -$BYTE_COUNT | node -e "
const readline = require('readline');
const fs = require('fs');
const rl = readline.createInterface({ input: process.stdin });
const bytes = [];
rl.on('line', line => bytes.push(parseInt(line)));
rl.on('close', () => fs.writeFileSync('${OUTPUT}.o', Buffer.from(bytes)));
"
echo "  Created ${OUTPUT}.o"

# Link
echo "Step 3: Linking..."
clang -arch arm64 -o "$OUTPUT" "${OUTPUT}.o" -Wl,-e,_main
rm -f "${OUTPUT}.o"
echo "  Created $OUTPUT"

echo ""
echo "Done! Run with: ./$OUTPUT"
echo "Check exit code with: ./$OUTPUT; echo \$?"
