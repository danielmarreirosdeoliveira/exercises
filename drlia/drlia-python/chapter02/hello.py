import torch

x = torch.Tensor([2,4])
m = torch.randn(2, requires_grad=True)
b = torch.randn(1, requires_grad=True)

# b = 1
# m = torch.Tensor([7,9])

y = m*x+b
# y = torch.mul(m,x)
# print(y)

loss = (torch.sum(torch.Tensor([3,5]) - y))**2
loss.backward()

print(m.grad)