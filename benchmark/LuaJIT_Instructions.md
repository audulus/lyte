# LuaJIT Bytecode Instruction Format

LuaJIT uses **fixed 32-bit instructions**. Each instruction contains an **8-bit opcode** and operands stored in **byte-aligned fields**, which simplifies decoding and speeds up the assembly interpreter.

## Base Layout

```
31        24 23        16 15         8 7        0
+-----------+-----------+------------+----------+
|     C     |     B     |     A      |  opcode  |
+-----------+-----------+------------+----------+
   8 bits       8 bits       8 bits      8 bits
```

Fields:

| Field  | Size   | Meaning                |
| ------ | ------ | ---------------------- |
| opcode | 8 bits | instruction identifier |
| A      | 8 bits | destination register   |
| B      | 8 bits | operand                |
| C      | 8 bits | operand                |

Registers are numbered from **0–255**.

---

# Instruction Formats

LuaJIT interprets the same 32-bit instruction using several formats.

## ABC Format

Used for most arithmetic and register operations.

```
31        24 23        16 15         8 7        0
+-----------+-----------+------------+----------+
|     C     |     B     |     A      |  opcode  |
+-----------+-----------+------------+----------+
```

Example:

```
ADDVV A B C
```

Meaning:

```
R[A] = R[B] + R[C]
```

Example instruction:

```
ADDVV 2 0 1
```

```
R2 = R0 + R1
```

---

## AD Format

Used when a **16-bit immediate or constant index** is required.

```
31                16 15         8 7        0
+------------------+------------+----------+
|        D         |     A      |  opcode  |
+------------------+------------+----------+
      16 bits           8 bits      8 bits
```

Example:

```
KSTR A D
```

Meaning:

```
R[A] = string_constant[D]
```

Example:

```
KSHORT 0 1
```

```
R0 = 1
```

---

## AJ Format (Jumps)

Used for control flow with **signed 16-bit offsets**.

```
31                16 15         8 7        0
+------------------+------------+----------+
|        J         |     A      |  opcode  |
+------------------+------------+----------+
```

Meaning:

```
pc += J
```

Example:

```
JMP -3
```

Jump three instructions backward.

---

# Example Bytecode

For the Lua code:

```lua
local a = 1
local b = 2
return a + b
```

LuaJIT bytecode (conceptual):

```
KSHORT   0   1
KSHORT   1   2
ADDVV    2   0   1
RET1     2   2
```

Meaning:

```
R0 = 1
R1 = 2
R2 = R0 + R1
return R2
```

---

# Notes

* All instructions are **32 bits**.
* Operands are **byte-aligned** for fast decoding.
* The VM is **register-based** (not stack-based).
* LuaJIT uses **specialized opcodes** (e.g., `ADDVV`, `ADDVN`) to reduce runtime type checks and simplify JIT compilation.
