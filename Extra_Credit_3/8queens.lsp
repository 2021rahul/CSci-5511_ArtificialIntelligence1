; Get successors from the current_state by moving the blank sqaure to the four neighbours
; Input: current_state
; Output: successor_list
(defun get_successors (state)
	(let (successors)
		(do ((i 0 (+ 1 i)))
			((> i 8))
			(setf successors (cons (concatenate 'string state (write-to-string i)) successors))
		)
		successors
	)
)

; Check if the (x, y) coordinates lie inside the 3x3 grid
; Input: (x, y) coordinaates)
; Output: Boolean(T if valid, F if not valid)
(defun check_valid_positions (coord)
	(let ((valid nil))
		(if (>= (nth 0 coord) 0)
			(if (<= (nth 0 coord) 7)
				(if (>= (nth 1 coord) 0)
					(if (<= (nth 1 coord) 7)
						(setf valid t)))))
		valid
	)
)

; Get the state with the lowest f value
; Input: openeList
; Output: sorted openList in increasing order
(defun get_lowest_f_state (opened_list)
	(sort opened_list #'< :key #'second)
	opened_list
)

; Get the estimate of the cost to reach from current_state to goal_state based on manhattan distance
; Input: current_state, goal_state
; Output: cost
(defun heuristic (state)
	(let ((h 0))
		(do ((i 0 (+ 1 i)))
			((> i (list-length state)) h)
			(setf coord (cons (nth i state) i))
			(setf check_positions (get_check_positions coord))
			(do ((i 0 (+ 1 i)))
			)
			(setf correct_pos (get_coordinates (position i goal)))
			(setf state_pos (get_coordinates (position i state)))
			(setf score (+ score (manhattan correct_pos state_pos)))
		)
	)
)


def evaluate(state):
	board = get_board(state)
	h = 0
	for j, i in enumerate(state):
		i = int(i)
		positions = get_check_positions([i, j])
		for position in positions:
			h += board[position[0], position[1]]
	return h



