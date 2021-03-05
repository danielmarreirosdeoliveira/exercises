import gym
import numpy as np
import torch

env = gym.make('CartPole-v0')

layer1 = 4
layer2 = 150
layer3 = 2

model = torch.nn.Sequential(
    torch.nn.Linear(layer1, layer2),
    torch.nn.LeakyReLU(),
    torch.nn.Linear(layer2, layer3),
    torch.nn.Softmax(dim=-1)
)

learning_rate = 0.009
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

def discount_rewards(rewards, gamma=0.99):
    lenr = len(rewards)
    disc_return = torch.pow(gamma,torch.arange(lenr).float()) * rewards
    disc_return /= disc_return.max()
    return disc_return

def run_episode(max_dur):
    transitions = []
    curr_state = env.reset()

    for t in range(max_dur):
        act_prob = model(torch.from_numpy(curr_state).float())
        action = np.random.choice(np.array([0,1]), p=act_prob.data.numpy())
        prev_state = curr_state
        curr_state, _, done, info = env.step(action)
        env.render()
        transitions.append((prev_state, action, t+1))
        if done:
            break

    return transitions


# pred_batch:
#   [[0.4808, 0.5192],
#    [0.4766, 0.5234],
#    [0.4715, 0.5285]]
# action_batch:
#   [0.,0.,1.]
#
# returns:
#   [0.4808, 0.4766, 0.5285]
def to_prob_batch(pred_batch, action_batch):
    return pred_batch.gather(dim=1,index=action_batch.long().view(-1,1)).squeeze() # todo adjust apidoc

def loss_fn(preds, r):
    return -1 * torch.sum(r * torch.log(preds))

MAX_DUR = 200
MAX_EPISODES = 100
gamma = 0.99
score = []

for episode in range(MAX_EPISODES):

    transitions = run_episode(MAX_DUR)

    score.append(len(transitions))

    reward_batch = torch.Tensor([r for (s, a, r) in transitions]).flip(dims=(0,))
    disc_reward = discount_rewards(reward_batch)
    state_batch  = torch.Tensor([s for (s,a,r) in transitions])
    action_batch = torch.Tensor([a for (s,a,r) in transitions])
    pred_batch = model(state_batch)
    # print(pred_batch)
    prob_batch = to_prob_batch(pred_batch, action_batch)

    loss = loss_fn(prob_batch, disc_reward)
    print("episode",episode,"len",len(transitions), "loss", loss.item())

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()
