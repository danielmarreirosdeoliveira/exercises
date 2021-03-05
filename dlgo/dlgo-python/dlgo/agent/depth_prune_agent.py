import random
from dlgo.agent.agent import Agent
from dlgo.engine import gotypes

MAX_SCORE = 999999
MIN_SCORE = -999999
MAX_DEPTH = 3


def capture_diff(game_state):
    black_stones = 0
    white_stones = 0
    for r in range(1, game_state.board.num_rows + 1):
        for c in range(1, game_state.board.num_cols + 1):
            p = gotypes.Point(r, c)
            color = game_state.board.get(p)
            if color == gotypes.Player.black:
                black_stones += 1
            elif color == gotypes.Player.white:
                white_stones += 1

    if game_state.next_player == gotypes.Player.black:
        return black_stones - white_stones
    else:
        return white_stones - black_stones


# Scores from the perspective of game_state.next_player,
# meaning the higher the score the better for next_player.
def best_result(game_state, max_depth):

#   TODO enable
#     if game_state.is_over():
#         if game_state.winner() == game_state.next_player:
#             return MAX_SCORE
#         else:
#             return MIN_SCORE

    if max_depth == 0:
        return capture_diff(game_state)

    best_so_far = MIN_SCORE
    for candidate_move in game_state.legal_moves():
        next_state = game_state.apply_move(candidate_move)
        opponent_best_result = best_result(next_state, max_depth - 1)
        our_result = -1 * opponent_best_result
        if our_result > best_so_far:
            best_so_far = our_result

    return best_so_far


class DepthPruneAgent(Agent):
    def select_move(self, game_state):
        best_moves = []
        best_score = None
        for possible_move in game_state.legal_moves():
            next_state = game_state.apply_move(possible_move)
            opponent_best_outcome = best_result(next_state, MAX_DEPTH)
            our_best_outcome = -1 * opponent_best_outcome
            if (not best_moves) or our_best_outcome > best_score:
                best_moves = [possible_move]
                best_score = our_best_outcome
            elif our_best_outcome == best_score:
                best_moves.append(possible_move)
        return random.choice(best_moves)