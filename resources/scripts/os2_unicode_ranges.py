"""Generates ranges to paste into fontbe/src/os2.rs"""
from fontTools.ttLib.tables.O_S_2f_2 import OS2_UNICODE_RANGES

print("const UNICODE_RANGES: &[(u32, u32, u32)] = &[")
rust_ranges = []
for bit, ranges in enumerate(OS2_UNICODE_RANGES):
    for name, (low, high) in ranges:
        rust_ranges.append((low, high, bit, name))
rust_ranges.sort()

for low, high, bit, name in rust_ranges:
    print(f"    (0x{low:04X}, 0x{high:04X}, {bit}),  // {name}")
print("];")