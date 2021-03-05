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
# Reversing a singly linked list

def reverse(L: ListNode) -> ListNode:
    head = None

    while L:
        next = L.next # save pointer before redirect
        L.next = head
        head = L
        L = next

    return head

############################################

l1 = ListNode(7, ListNode(9, ListNode(10)))
l1.p()
l2 = reverse(l1)
l2.p()
