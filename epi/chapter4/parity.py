# def parity(x: int) -> int:
#    result = 0
#    while x:
#        result ^= x & 1
#        x >>= 1
#    return result

def parity(x: int) -> int:
    result = 0
    while x:
        result ^= 1
        x &= x - 1 # Drops the lowest set bit of xself.
    return result

print(parity(10))
print(parity(11))
