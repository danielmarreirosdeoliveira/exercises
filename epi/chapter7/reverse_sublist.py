class ListNode:
    def __init__(self, data=0, next=None):
        self.data = data
        self.next = next

    def p(self):
        d = []
        L = self
        while L:
            d.append(L.data)
            L = L.next
        print(d)

############################################
# Reversing a sublist of a singly linked list

def reverse(L: ListNode, s: int, f: int) -> ListNode:

    head = L

    for _ in range(1, s-1):
        head = head.next

    # reverse
    iter = head.next
    for _ in range(f-s):
        temp = iter.next
        iter.next = temp.next
        temp.next = head.next
        head.next = temp

    return L


############################################

l1 = ListNode(7, ListNode(9, ListNode(10, ListNode(20, ListNode(19)))))
l1.p()
l2 = reverse(l1, 2, 4)
l2.p()
