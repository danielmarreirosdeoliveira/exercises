
def int_to_str(x: int) -> str:
    is_negative = False
    if x < 0:
        x, is_negative = -x, True

    s = []
    while True:
        s.append(chr(ord('0') + x % 10))
        x //= 10
        if x == 0:
            break

    return ('-' if is_negative else '') + ''.join(reversed(s))

#
#print(int_to_str(423))
#print(int_to_str(0))
#print(int_to_str(-423))

import functools
import string

def str_to_int(s: str) -> int:
    return functools.reduce(
        lambda acc, val:
            acc * 10 + string.digits.index(val)
    , s[s[0] == '-':], 0) * (-1 if s[0] == '-' else 1)

print(str_to_int('123'))
print(str_to_int('-123'))
