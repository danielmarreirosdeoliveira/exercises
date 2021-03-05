# exec(open("./dutch_flag_a.py").read())
from typing import List

RED, WHITE, BLUE = range(3) # 0, 1, 2. where pivot is 1

A: List[int] = [RED, WHITE, WHITE, BLUE, WHITE, BLUE, RED, WHITE, BLUE, RED, RED, WHITE, RED]

# using O(1) extra space
def dutch_flag_partition(A: List[int]) -> None:
    B: List[int] = []

    r, w, b = 0, 0, 0

    for i in range(len(A)):
        if A[i] == RED:
            r += 1
        elif A[i] == WHITE:
            w += 1
        else:
            b += 1

    for i in range(r):
        B.append(RED)
    for i in range(w):
        B.append(WHITE)
    for i in range(b):
        B.append(BLUE)

    return B


print(len(A))
B = dutch_flag_partition(A)
print(B)
print(len(B))



# def dutch_flag_partition(A: List[int]) -> None:
#
#     l, m, r = 0, 0, len(A) - 1
#
#     while m <= r:
#         print('before',A)
#         if A[m] == 0:
#             print(0, l, m, r)
#             A[m], A[l] = A[l], A[m]
#             l, m = l + 1, m + 1
#         elif A[m] == 1:
#             print(1, l, m, r)
#             m += 1
#         else:
#             print(2, l, m, r)
#             A[m], A[r] = A[r], A[m]
#             r = r - 1
