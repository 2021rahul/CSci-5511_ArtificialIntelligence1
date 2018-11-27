import numpy as np

def get_board(state):
	board = np.zeros((8, 8))
	for j,i in enumerate(state):
		i = int(i)
		board[i,j] = 1
	return board

def get_successors(state):
	successors = []
	for i in range(8):
		successors.append(state+str(i))
	return successors

def is_valid(coord):
	valid = True
	if coord[0]<0 or coord[0]>7 or coord[1]<0 or coord[1]>7:
		valid = False
	return valid

def check_valid_position(position):
	valid = True
	if position[0]<0 or position[0]>2 or position[1]<0 or position[1]>2:
		valid = False
	return valid

def get_row(coord):
	positions = []
	for col in range(coord[1]+1,8):
		positions.append([coord[0],col])
	return positions

def get_diagonal(coord):
	directions = [[-1, 1], [1, 1]]
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

def evaluate(state):
	board = get_board(state)
	h = 0
	for j, i in enumerate(state):
		i = int(i)
		positions = get_check_positions([i, j])
		for position in positions:
			h += board[position[0], position[1]]
	return h

def get_lowest_f_state(opened_list):
    sorted_h = []
    for key in opened_list.keys():
        el = [key, opened_list[key]]
        sorted_h.append(el)
    sorted_h = np.asarray(sorted_h)
    sorted_h = sorted_h[np.argsort(sorted_h[:, 1])]
    return sorted_h[0, 0]

def greedy():
	opened = {}
	closed = {}
	state = "3"
	opened[state] = evaluate(state)
	while opened:
		state = get_lowest_f_state(opened)
		closed[state] = opened.pop(state)

		if len(state)==8 and not evaluate(state):
			print(state)
			return
		print(state)
		successors = get_successors(state)
		for successor in successors:
			if successor in closed.keys():
				continue
			successor_info = evaluate(successor)
			opened[successor] = successor_info


if __name__ == "__main__":
	greedy()