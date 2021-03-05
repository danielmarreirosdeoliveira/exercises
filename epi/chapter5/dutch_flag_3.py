# exec(open("./dutch_flag_3.py").read())
from typing import List




def dutch_flag_partition(pivot_index: int, A: List[int]) -> None:

    pivot = A[pivot_index]
    smaller, equal, bigger = 0, 0, len(A) - 1

    while equal < bigger:

        if A[equal] < pivot:
            A[equal], A[smaller] = A[smaller], A[equal]
            smaller, equal = smaller + 1, equal + 1

        if A[equal] == pivot:
            equal += 1

        if A[equal] > pivot:
            A[equal], A[bigger] = A[bigger], A[equal]
            bigger -= 1







A = [9, 18, 93, 3, 9, 23, 9, 91, 0, -1]
dutch_flag_partition(0, A)
print(A)
