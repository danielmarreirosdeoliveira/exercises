import random

from dlgo.agent.agent import Agent
from dlgo.engine.gotypes import Player
import dlgo.agent.helpers as helpers

MAX_SCORE = 999999
MIN_SCORE = -999999
MAX_DEPTH = 3


def alpha_beta_result(game_state, max_depth, best_black, best_white):

#   TODO enable
#     if game_state.is_over():
#         if game_state.winner() == game_state.next_player:
#             return MAX_SCORE
#         else:
#             return MIN_SCORE

    if max_depth == 0:
        return helpers.capture_diff(game_state)

    best_so_far = MIN_SCORE
    for candidate_move in game_state.legal_moves():
        next_state = game_state.apply_move(candidate_move)
        opponent_best_result = alpha_beta_result(
            next_state, max_depth - 1,
            best_black, best_white)
        our_result = -1 * opponent_best_result

        if our_result > best_so_far:
            best_so_far = our_result
        if game_state.next_player == Player.white:
            if best_so_far > best_white:
                best_white = best_so_far
            outcome_for_black = -1 * best_so_far
            if outcome_for_black < best_black:
                return best_so_far
        elif game_state.next_player == Player.black:
            if best_so_far > best_black:
                best_black = best_so_far
            outcome_for_white = -1 * best_so_far
            if outcome_for_white < best_white:
                return best_so_far

    return best_so_far


class AlphaBetaAgent(Agent):
    def select_move(self, game_state):
        best_moves = []
        best_score = None
        best_black = MIN_SCORE
        best_white = MIN_SCORE
        for possible_move in game_state.legal_moves():
            next_state = game_state.apply_move(possible_move)
            opponent_best_outcome = alpha_beta_result(
                next_state, MAX_DEPTH,
                best_black, best_white)
            our_best_outcome = -1 * opponent_best_outcome
            if (not best_moves) or our_best_outcome > best_score:
                best_moves = [possible_move]
                best_score = our_best_outcome
                if game_state.next_player == Player.black:
                    best_black = best_score
                elif game_state.next_player == Player.white:
                    best_white = best_score
            elif our_best_outcome == best_score:
                best_moves.append(possible_move)
        return random.choice(best_moves)
