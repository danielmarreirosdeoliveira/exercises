


import numpy as np


# gradients per step

step_1_gradient = np.array([1.,4.,-5.])
step_2_gradient = np.array([-1.,5.,-5.])
step_1_reward = 1.
step_2_reward = 2.

game_1_gradient = np.array([step_1_gradient, step_2_gradient])
game_1_rewards = np.array([step_1_reward, step_2_reward])

step_1_gradient = np.array([2.,-1.,-5.])
step_2_gradient = np.array([-1.,-4.,-5.])
step_1_reward = -1.
step_2_reward = 0.

game_2_gradient = np.array([step_1_gradient, step_2_gradient])
game_2_rewards = np.array([step_1_reward, step_2_reward])


iteration_gradients=np.array([game_1_gradient, game_2_gradient])
print(iteration_gradients)

iteration_rewards=np.array([game_1_rewards, game_2_rewards])
print(iteration_rewards)


print([reward * iteration_gradients[game][step] for game, rewards in enumerate(iteration_rewards) for step, reward in enumerate(rewards)])
print(np.mean([reward * iteration_gradients[game][step] for game, rewards in enumerate(iteration_rewards) for step, reward in enumerate(rewards)], axis=0))
