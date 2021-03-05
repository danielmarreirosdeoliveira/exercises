class ListNode:
    def __init__(self, data=0, next=None):
        self.data = data
        self.next = next


def search(L: ListNode, key: int) -> ListNode:
    while L and L.data != key:
        L = L.next
    return L

def printL(L: ListNode) -> None:
    while L:
        print(L.data)
        L = L.next

def insert_after(node: ListNode, new_node: ListNode) -> None:
    new_node.next = node.next
    node.next = new_node

def delete_after(node: ListNode) -> None:
    node.next = node.next.next

l1 = ListNode(7, ListNode(9, ListNode(10)))
delete_after(l1)
printL(l1)

#l1 = ListNode(7, ListNode(9))
#l2 = ListNode(10, None)
#insert_after(l1, l2)
#printL(l1)

#printL(search(ListNode(0, (ListNode(7, ListNode(8, None)))), 7))
