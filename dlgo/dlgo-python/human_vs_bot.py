from dlgo import agent
from dlgo.agent.random_agent import RandomAgent
from dlgo.agent.depth_prune_agent import DepthPruneAgent
from dlgo.agent.alpha_beta_agent import AlphaBetaAgent
from dlgo.engine import goboard_fast as goboard
from dlgo.engine import gotypes
from dlgo.utils import print_board, print_move, point_from_coords
from six.moves import input


def main():
    board_size = 3
    game = goboard.GameState.new_game(board_size)
    # bot = agent.depth_prune_agent.DepthPruneAgent()
    bot = agent.alpha_beta_agent.AlphaBetaAgent()

    while not game.is_over():
        print(chr(27) + "[2J")
        print_board(game.board)
        if game.next_player == gotypes.Player.black:
            human_move = input('-- ')
            point = point_from_coords(human_move.strip())
            move = goboard.Move.play(point)
        else:
            move = bot.select_move(game)
        print_move(game.next_player, move)
        game = game.apply_move(move)


if __name__ == '__main__':
    main()