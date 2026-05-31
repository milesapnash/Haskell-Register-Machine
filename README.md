# Haskell Register Machine

A register machine simulator in Haskell. Encodes and decodes programs as Gödel numbers via a pairing function, parses a human-readable instruction format, and executes programs with full trace support.

## What is a register machine?

A register machine operates on a finite set of registers (R0, R1, ...) each holding a non-negative integer. A program is a sequence of labelled instructions:

- **HALT** — stop execution
- **R*n*+ -> L*j*** — increment register *n*, jump to label *j*
- **R*n*- -> L*j*, L*k*** — if register *n* > 0, decrement it and jump to *j*; otherwise jump to *k*

Programs can be encoded as a single integer (a Gödel number) using a pairing function, making them first-class data that other register machines can manipulate.

## Building

```
cabal build all
```

## Usage

```
rm-sim run [--trace] [--max-steps N] <file> [r0 r1 ...]
rm-sim encode <file>
rm-sim decode <integer>
```

### Run a program

```
$ cabal run rm-sim -- run examples/addition.rm 3 5
Registers: [8,0]
```

### Trace execution

```
$ cabal run rm-sim -- run --trace examples/addition.rm 3 5
(R1- -> L1, L2,[0,3,5])
(R0+ -> L0,[1,3,4])
(R1- -> L1, L2,[0,4,4])
(R0+ -> L0,[1,4,3])
...
(HALT,[-1,8,0])
```

### Encode a program as a Gödel number

```
$ cabal run rm-sim -- encode examples/addition.rm
786432
```

### Decode a Gödel number back to a program

```
$ cabal run rm-sim -- decode 786432
PROGRAM:
L0:	R0- -> L0, L2
L1:	HALT
```

## Program format

Programs are written one instruction per line with label prefixes:

```
L0:	R1- -> L1, L2
L1:	R0+ -> L0
L2:	HALT
```

## Examples

| File | Description | Example |
|------|-------------|---------|
| `examples/addition.rm` | R0 = R0 + R1 | `run examples/addition.rm 3 5` → `[8,0]` |
| `examples/multiplication.rm` | R2 = R0 × R1 | `run examples/multiplication.rm 4 3` → `[4,0,12,0]` |
| `examples/copy.rm` | R1 = R0 (non-destructive) | `run examples/copy.rm 7` → `[7,7,0]` |

## Testing

```
cabal test --test-show-details=always
```

Runs roundtrip properties (pairing, instruction encoding, list encoding, program encoding, parser) and execution properties (halt, increment, decrement branching, inc/dec identity, addition correctness) via QuickCheck.

## License

MIT
