from typing import List

def even_odd(A: List[int]) -> None:
    next_even, next_odd = 0, len(A) - 1

    while next_even < next_odd:
        if A[next_even] % 2 == 0:
            next_even += 1
        else:
            A[next_even], A[next_odd] = A[next_odd], A[next_even]
            next_odd -= 1

    return A


A = [1, 8, 7, 4, 17, 18, 199, 200]

print(even_odd(A))
