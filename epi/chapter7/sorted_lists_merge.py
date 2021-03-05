from typing import Optional

class ListNode:
    def __init__(self, data=0, next=None):
        self.data = data
        self.next = next

def printL(L: ListNode) -> None:
    while L:
        print(L.data)
        L = L.next

def merge_two_sorted_lists(L1: Optional[ListNode], L2: Optional[ListNode]) -> Optional[ListNode]:
    t = h = ListNode()

    while L1 and L2:
        if L1.data < L2.data:
            t.next, L1 = L1, L1.next
        else:
            t.next, L2 = L2, L2.next
        t = t.next

    t.next = L1 or L2
    return h.next

L1 = ListNode(2, ListNode(5))
L2 = ListNode(4, ListNode(8))
printL(merge_two_sorted_lists(L1, L2))
