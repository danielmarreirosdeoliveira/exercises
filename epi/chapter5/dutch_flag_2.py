# exec(open("./dutch_flag_2.py").read())
from typing import List

def dutch_flag_partition(pivot_index: int, A: List[int]) -> None:
    pivot = A[pivot_index]

    index_rightmost_smaller_than_pivot = 0
    for i in range(len(A)):
        if A[i] < pivot:
            A[i], A[index_rightmost_smaller_than_pivot] = A[index_rightmost_smaller_than_pivot], A[i]
            index_rightmost_smaller_than_pivot += 1

    index_leftmost_greater_than_pivot = len(A) - 1
    for i in reversed(range(len(A))):
        if A[i] > pivot:
            print(i, A[i])
            A[i], A[index_leftmost_greater_than_pivot] = A[index_leftmost_greater_than_pivot], A[i]
            index_leftmost_greater_than_pivot -= 1
    # forward pass


# A = [14, 13, 9, 11, 5]
A = [14, 13, 9, 11, 5, 344, 1, -1]

dutch_flag_partition(2, A)

print(A)
