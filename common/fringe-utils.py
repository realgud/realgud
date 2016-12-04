#!/usr/bin/env python3

def bit2char(byte, offset):
    return "X" if byte & (1 << offset) else " "

def char2bit(char, offset):
    return (0 if char == " " else 1) << offset

def decompile_bitmap(bmp_bytes):
    lines = []
    for b in bmp_bytes:
        lines.append("".join(bit2char(b, offset) for offset in range(8)))
    return lines

def compile_bitmap(bmp_lines):
    bmp_bytes = []
    for line in bmp_lines:
        s = sum(char2bit(c, offset) for (offset, c) in enumerate(line))
        bmp_bytes.append(s.to_bytes(1, byteorder="big"))
    return b"".join(bmp_bytes)

hollow_circle = ["  XXXX  ",
                 " X    X ",
                 "X      X",
                 "X      X",
                 "X      X",
                 "X      X",
                 " X    X ",
                 "  XXXX  "]

def print_compiled(bmp):
    print("".join(r'\x{:02x}'.format(b) for b in bmp))

print("\n".join(decompile_bitmap(b"\x3c\x7e\xff\xff\xff\xff\x7e\x3c")))
print_compiled(compile_bitmap(decompile_bitmap(b"\x3c\x7e\xff\xff\xff\xff\x7e\x3c")))
print_compiled(compile_bitmap(hollow_circle))

