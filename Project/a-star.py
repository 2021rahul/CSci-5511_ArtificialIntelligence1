#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 17 19:01:02 2018

@author: 2021rahul
"""

import numpy as np
import copy


def manhattan_distance(goal_coord, state_coord):
    return abs(goal_coord[0][0]-state_coord[0][0])+abs(goal_coord[1][0]-state_coord[1][0])


def get_state_string(state):
    string = ""
    for row in state:
        for el in row:
            string += el
    return string


def get_string_state(string):
    state = []
    for i in range(3):
        ls = []
        for j in range(3):
            ls.append(string[(3*i)+j])
        state.append(ls)
    return np.asarray(state)


def get_heuristic(state, goal):
    h_n = 0
    for i in range(1, 9):
        goal_coord = np.where(goal == str(i))
        state_coord = np.where(state == str(i))
        h_n += manhattan_distance(goal_coord, state_coord)
    return h_n


def is_solved(state, goal):
    solved = True
    print(state)
    for i in range(1, 9):
        goal_coord = np.where(goal == str(i))
        state_coord = np.where(state == str(i))
        if goal_coord[0][0] != state_coord[0][0] or goal_coord[1][0] != state_coord[1][0]:
            solved = False
    return solved


def check_valid_position(position):
    valid = True
    if position[0]<0 or position[0]>2 or position[1]<0 or position[1]>2:
        valid = False
    return valid


def get_successors(state):
    actions = [[-1, 0], [1, 0], [0, -1], [0, 1]]
    e_coord = np.where(state == 'E')
    new_positions = []
    for action in actions:
        positions = [e_coord[0][0]+action[0], e_coord[1][0]+action[1]]
        if check_valid_position(positions):
            new_positions.append(positions)
    return new_positions


def get_new_state(state, position):
    e_coord = np.where(state == 'E')
    state[e_coord[0][0], e_coord[1][0]] = state[position[0], position[1]]
    state[position[0], position[1]] = 'E'
    return state


def add_to_dict(state, goal, g):
    dictionary = {}
    dictionary["g"] = g
    dictionary["h"] = get_heuristic(state, goal)
    dictionary["f"] = dictionary["g"] + dictionary["h"]
    return dictionary


def get_lowest_f_state(opened_list):
    sorted_f = []
    for i, keys in enumerate(opened_list.keys()):
        el = [keys, opened_list[keys]["f"]]
        sorted_f.append(el)
    sorted_f = np.asarray(sorted_f)
    sorted_f = sorted_f[np.argsort(sorted_f[:, 1])]
    return sorted_f[0, 0]


def Astar(start, goal):
    opened = {}
    closed = {}
    opened[get_state_string(start)] = add_to_dict(start, goal, 0)
    opened[get_state_string(start)]["f"] = 0
    opened[get_state_string(start)]["h"] = 0
    while opened:
        state_string = get_lowest_f_state(opened)
        closed[state_string] = opened.pop(state_string)
        state = get_string_state(state_string)

        if is_solved(state, goal):
            return

        successors = get_successors(state)
        successors = np.asarray(successors)
        for successor in successors:
            successor_state = copy.copy(state)
            successor_state = get_new_state(successor_state, successor)
            successor_string = get_state_string(successor_state)
            if successor_string in closed.keys():
                continue
            successor_info = add_to_dict(successor_state, goal, closed[state_string]["g"]+1)
            if successor_string in opened.keys():
                if closed[state_string]["g"]+1 > opened[successor_string]["g"]:
                    continue
            opened[successor_string] = successor_info


if __name__ == "__main__":
    goal = [['1', '2', '3'], ['8', 'E', '4'], ['7', '6', '5']]
    easy_start = [['1', '3', '4'], ['8', '6', '2'], ['7', 'E', '5']]
    medium_start = [['2', '8', '1'], ['E', '4', '3'], ['7', '6', '5']]
    hard_start = [['2', '8', '1'], ['4', '6', '3'], ['E', '7', '5']]
    tough_start = [['2', '1', '3'], ['8', 'E', '4'], ['6', '7', '5']]
    worst_start = [['5', '6', '7'], ['4', 'E', '8'], ['3', '2', '1']]
#    Astar(np.asarray(worst_start), np.asarray(goal), {})
    Astar(np.asarray(easy_start), np.asarray(goal))
