# Python Implementation from the Book

```bash
python3 bot_vs_bot.py
python3 human_vs_bot.py
```

One can select from the available Agents.

```python
bots = {
    gotypes.Player.black: agent.random_agent.RandomAgent(),
    gotypes.Player.white: agent.depth_prune_agent.DepthPruneAgent()
}

agent.random_agent.RandomAgent()
agent.depth_prune_agent.DepthPruneAgent()
agent.alpha_beta_agent.AlphaBetaAgent()
```
