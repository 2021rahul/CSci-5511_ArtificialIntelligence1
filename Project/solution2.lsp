; Get coordinates in a 3x3 grid given the position index in state list
; Input: position_index
; Output: (x, y) coordinates
(defun get_coordinates (index)
	(setf (values q r) (floor index 3))
	(list q r)
)

; Get position index in state list given the coordinates in a 3x3 grid
; Input: (x, y) coordinates
; Output: position_index
(defun get_index (coord)
	(+ (* 3 (nth 0 coord)) (nth 1 coord))
)

; Get new_state from the current state by moving the blank position to the new_coordinates
; Input: state, current_coordinates, new_coordinates
; Output: new_state
(defun get_new_state (state coord newcoord)
	(let (new_state)
		(setf new_state (copy-list state))
		(setf (nth (get_index newcoord) new_state) (nth (get_index coord) state))
		(setf (nth (get_index coord) new_state) (nth (get_index newcoord) state))
		new_state
	)
)

; Check if the (x, y) coordinates lie inside the 3x3 grid
; Input: (x, y) coordinaates)
; Output: Boolean(T if valid, F if not valid)
(defun check_valid_positions (coord)
	(let ((valid nil))
		(if (>= (nth 0 coord) 0)
			(if (<= (nth 0 coord) 2)
				(if (>= (nth 1 coord) 0)
					(if (<= (nth 1 coord) 2)
						(setf valid t)))))
		valid
	)
)

; Get successors from the current_state by moving the blank sqaure to the four neighbours
; Input: current_state
; Output: successor_list
(defun get_successors (state)
	(let (newstates coord newcoord directions)
		(setf directions '((-1 0) (1 0) (0 -1) (0 1)))
		(setf coord (get_coordinates (position 0 state)))
		(do ((i 0 (+ 1 i)))
			((> i 3))
			(setf newcoord (mapcar '+ coord (nth i directions)))
			(if (check_valid_positions newcoord)
				(setf newstates (cons (get_new_state state coord newcoord) newstates))))
		newstates
	)
)

; Get the state with the lowest f value
; Input: openeList
; Output: sorted openList in increasing order
(defun get_lowest_f_state (opened_list)
	(sort opened_list #'< :key #'second)
	opened_list
)

; Get the manhattan distance between two set of coordinates
; Input: a_coordinates, b_coordinates
; Output: distance
(defun manhattan (a_coord b_coord)
	(+ (abs (- (nth 0 a_coord) (nth 0 b_coord))) (abs (- (nth 1 a_coord) (nth 1 b_coord))))
)

; Get the estimate of the cost to reach from current_state to goal_state based on manhattan distance
; Input: current_state, goal_state
; Output: cost
(defun heuristic (state goal)
	(let ((score 0) state_pos correct_pos)
		(do ((i 1 (+ 1 i)))
			((> i 8) score)
			(setf correct_pos (get_coordinates (position i goal)))
			(setf state_pos (get_coordinates (position i state)))
			(setf score (+ score (manhattan correct_pos state_pos)))
		)
	)
)

; Calculate the f, g and h value for a given node
; Input: current_state, goal, current_state_depth
; Output: list containing f,g and h values
(defun evaluate_node (state goal depth)
	(setf g depth)
	(setf h (heuristic state goal))
	(list state (+ g h) g h)
)

; Given a start_state determine if it is solvable or not
; Input: start_state
; Output: Boolean(T if solvable)
(defun is_solvable (state)
	(let ((count 0))
		(do ((i 0 (+ 1 i)))
			((> i 8) count)
			(unless (= (nth i state) 0)
				(do ((j i (+ 1 j)))
					((> j 8))
					(when (and (not (= (nth j state) 0)) (> ( nth i state ) ( nth j state )))
						(setf count (+ 1 count))
					)
				)
			)
		)
		(evenp count)
	)
)

; A* algorithm
(defun Astar (start goal)
	(let (opened closed node state (expanded 0))
		(setf opened (cons (evaluate_node start goal 0) opened))
		(loop while (not (null opened)) do
			(setf ret_val (get_lowest_f_state opened))
			(setf node (car ret_val))
			(setf opened (cdr ret_val))
			(setf state (car node))
			(setf closed (cons node closed))
			(setf expanded (+ expanded 1))
			(if (equal state goal)
				(progn
					(format t "number of nodes expanded ~S~%" expanded)
					(return expanded)
				)
			)
			(setf successors (get_successors state))
			(format t "level ~S~% generated states ~S~%" expanded successors)
			(do ((i 0 (+ 1 i)))
				((> i (- (list-length successors) 1)))
				(setf successor (nth i successors))
				; (format t "generated states ~S~%" successor)
				(if (not (assoc successor closed))
					(progn
						(setf info (evaluate_node successor goal (+ (nth 2 node) 1)))
						(if (assoc successor opened)
							(if (> (nth 2 (assoc successor opened))) (+ (nth 2 node) 1)
								(setf opened (cons (evaluate_node successor goal (+ (nth 2 node) 1)) opened))
							)
							(setf opened (cons (evaluate_node successor goal (+ (nth 2 node) 1)) opened))
						)
					)
				)
			)
		)
	)
)

; Driver function
(defun main ()
	(setf goal '(1 2 3 4 5 6 7 8 0))
	(setf start '(0 1 3 4 2 5 7 8 6))
	(format t "Goal State ~S~%" goal)
	(format t "Start State ~S~%" start)
	(if (is_solvable start)
		(Astar start goal)
		(format t "The given puzzle is not solvable")
	)
)
