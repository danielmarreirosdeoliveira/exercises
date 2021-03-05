from dlgo import agent
from dlgo.agent.random_agent import RandomAgent
from dlgo.agent.depth_prune_agent import DepthPruneAgent
from dlgo.agent.alpha_beta_agent import AlphaBetaAgent
from dlgo.engine import goboard_fast as goboard
from dlgo.engine import gotypes
from dlgo.utils import print_board, print_move
import time


def main():
    board_size = 9
    game = goboard.GameState.new_game(board_size)
    bots = {
        gotypes.Player.black: agent.random_agent.RandomAgent(),
        gotypes.Player.white: agent.random_agent.RandomAgent()
    }
    while not game.is_over():
        time.sleep(1.2)

        print(chr(27) + "[2J")
        print_board(game.board)
        bot_move = bots[game.next_player].select_move(game)
        print_move(game.next_player, bot_move)
        game = game.apply_move(bot_move)

if __name__ == '__main__':
    main()