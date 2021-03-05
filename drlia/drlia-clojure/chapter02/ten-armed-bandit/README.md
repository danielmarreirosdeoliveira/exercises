# Ten armed bandit

Basic reinforcement learning problem, as from the beginning of the book

    $ clj
    $ (use 'go)
    user=> (go-bandit 500)

[[hidden-probability] [nr-lever-has-been-pulled average-payoff] ...]

The entry with the highest average payoff should approximate, when divided by 10, approximate the corresponding hidden-probability. Also this lever should have been pulled the most often.

For further analysis, 

    user => (spit-running-averages 500)
    
Strip the parentheses manually. Then display with

    python3 plot-running-averages.py

