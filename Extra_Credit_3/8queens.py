import numpy as np
import random
from operator import itemgetter

def get_board(state):
	board = np.zeros((8, 8))
	for j,i in enumerate(state):
		i = int(i)
		board[i,j] = 1
	return board

def is_valid(coord):
	valid = True
	if coord[0]<0 or coord[0]>7 or coord[1]<0 or coord[1]>7:
		valid = False
	return valid

def get_row(coord):
	positions = []
	for col in range(0,8):
		if col!=coord[1]:
			positions.append([coord[0],col])
	return positions

def get_diagonal(coord):
	directions = [[-1, 1], [1, 1], [-1, -1], [1, -1]]
	positions = []
	for direction in directions:
		new_coord = [coord[0]+direction[0], coord[1]+direction[1]]
		while is_valid(new_coord):
			positions.append(new_coord)
			new_coord = [new_coord[0]+direction[0], new_coord[1]+direction[1]]
	return positions

def get_check_positions(coord):
	positions = get_row(coord)
	positions = positions + get_diagonal(coord)
	return positions

def get_conflicts(state):
	board = get_board(state)
	attack = []
	for j, i in enumerate(state):
		i = int(i)
		h = 0
		positions = get_check_positions([i, j])
		for position in positions:
			h += board[position[0], position[1]]
		attack.append(h)
	return attack

def get_col_val(state, col):
	board = get_board(state)
	vals = []
	for val in range(8):
		h = 0
		positions = get_check_positions([val, col])
		for position in positions:
			h += board[position[0], position[1]]
		vals.append(h)
	col_val = min(enumerate(vals), key=itemgetter(1))[0]
	return col_val

def is_solved(conflicts):
	solved = True
	for conflict in conflicts:
		if conflict!=0:
			solved = False
	return solved

def iterative_repair(state):
	steps = 0
	while True:
		print(state)
		steps += 1
		print(steps)
		conflicts = get_conflicts(state)
		if is_solved(conflicts):
			return

		col = max(enumerate(conflicts), key=itemgetter(1))[0]
		col_val = get_col_val(state, col)
		new_state = state.copy()
		new_state[col] = col_val
		if new_state == state:
			state = generate_board()
			# steps = 0
		else:
			state = new_state

def generate_board():
	state = random.sample(range(0,8), 8)
	return state

if __name__ == "__main__":
	start = generate_board()
	iterative_repair(start)





