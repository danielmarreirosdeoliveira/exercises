import torch
import random
import numpy as np
from matplotlib import pyplot as plt
from Gridworld import Gridworld

l1 = 64
l2 = 150
l3 = 100
l4 = 4

model = torch.nn.Sequential(
    torch.nn.Linear(l1, l2),
    torch.nn.ReLU(),
    torch.nn.Linear(l2, l3),
    torch.nn.ReLU(),
    torch.nn.Linear(l3, l4)
)
loss_fn = torch.nn.MSELoss()
learning_rate = 1e-3
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

gamma = 0.9
epsilon = 1.0

action_set = {
    0: 'u',
    1: 'd',
    2: 'l',
    3: 'r',
}

def best_action(qval):
    qval_ = qval.data.numpy()
    if (random.random() < epsilon):
        action_ = np.random.randint(0,4)
    else:
        action_ = np.argmax(qval_)
    action = action_set[action_]
    return action_, action

def make_state(game_board):
    state_ = game_board.render_np().reshape(1,64) \
             + np.random.rand(1,64)/10.0
    return torch.from_numpy(state_).float()

def calc_target_q(reward, maxQ):
    if reward == -1:
        Y = reward + (gamma * maxQ)
    else:
        Y = reward
    return Y

#####

epochs = 1000
losses = []
for i in range(epochs):
    game = Gridworld(size=4, mode='static')

    state1 = make_state(game.board)

    keep_going = 1
    while(keep_going == 1):
        qval = model(state1)
        action_index, action = best_action(qval)
        game.makeMove(action)
        reward = game.reward()

        state2 = make_state(game.board)
        with torch.no_grad():
            newQ = model(state2.reshape(1,64))
        maxQ = torch.max(newQ)

        X = qval.squeeze()[action_index]
        Y = calc_target_q(reward, maxQ)
        Y = torch.Tensor([Y]).detach()
        loss = loss_fn(X, Y)
        optimizer.zero_grad()
        loss.backward()
        losses.append(loss.item())
        # print(loss.item())
        optimizer.step()
        state1 = state2

        if reward != -1:
            keep_going = 0

    if epsilon > 0.1:
        epsilon -= (1/epochs)

plt.figure(figsize=(10,7))
plt.plot(losses)
plt.xlabel("Epochs",fontsize=22)
plt.ylabel("Loss",fontsize=22)
plt.show()