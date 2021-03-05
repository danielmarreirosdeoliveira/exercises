from typing import List

RED, WHITE, BLUE = range(3)

def dutch_flag_partition(pivot_index: int, A: List[int]) -> None:
    pivot = A[pivot_index]

    for i in range(len(A)):
        for j in range(i + 1, len(A)):
            if A[j] < pivot:
                print(':', A[j], pivot)
                A[i], A[j] = A[j], A[i]
                break

    for i in reversed(range(len(A))):
        for j in reversed(range(i)):
            if A[j] > pivot:
                A[i], A[j] = A[j], A[i]
                break


A = [1,9,12,7,11]

dutch_flag_partition(1, A)
print(A)
