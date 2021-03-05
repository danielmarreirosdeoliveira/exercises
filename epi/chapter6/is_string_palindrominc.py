from typing import List




s = [6, 5, 4, 5, 6]
s = "abccba"
s = [6, 5, 4, 4, 5, 6]
s = "abcba"
s = [6, 5, 4, 5, 6]
s = "abcba"

# print(all(s[i] == s[~i] for i in range(len(s) // 2)))

for i in range(5):
    print('i', i)
    print('~i', ~i)
