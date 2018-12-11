; Check if the (x, y) coordinates lie inside the 8x8 grid
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

; Get the positions on the same row to check
; Input: x,y coordinates
; Output: list of x, y coordinates lying in the same row
(defun get_row (coord)
	(let (positions)
		(do ((col 0 (+ 1 col)))
			((> col 7) positions)
			(if (not (equal col (nth 1 coord)))
				(setf positions (cons (list (nth 0 coord) col) positions))
			)
		)
		positions
	)
)

; Get the coordinates in a given direction
; Input: x,y coordinates
; Output: new x, y coordinates lying in the same row in the board
(defun get_new_coord (coord direction)
	(list (+ (nth 0 coord) (nth 0 direction)) (+ (nth 1 coord) (nth 1 direction)))
)

; Get the positions on the diagonals to check
; Input: x,y coordinates
; Output: list of x, y coordinates lying in the diagonals
(defun get_diagonals (coord)
	(let (directions positions new_coord)
		(setf directions '((-1 1) (1 1) (-1 -1) (1 -1)))
		(do ((i 0 (+ 1 i)))
			((> i (- (list-length directions) 1)))
			(setf new_coord (get_new_coord coord (nth i directions)))
			(do ()
				((not (check_valid_positions new_coord)))
				(setf positions (cons new_coord positions))
				(setf new_coord (get_new_coord new_coord (nth i directions)))
			)
		)
		positions
	)
)

; Get the positions to check
; Input: x,y coordinates
; Output: list of x, y coordinates la positions to check
(defun get_check_positions (coord)
	(let (positions)
		(setf positions (get_row coord))
		(append positions (get_diagonals coord))
	)
)

; Get the the complete 8x8 board from the state
; Input: state
; Output: 8x8 board
(defun get_board (state)
	(let (board)
		(setf board (make-array '(8 8) :initial-element 0))
		(do ((i 0 (+ 1 i)))
			((> i (- (list-length state) 1)))
			(setf (aref board (nth i state) i) 1)
		)
		board
	)
)

; Get the number of conflicts from the given state of the board
; Input: state
; Output: number of attacks on each queens
(defun get_conflicts (state)
	(let (board attack)
		(setf board (get_board state))
		(do ((i 0 (+ 1 i)))
			((> i (- (list-length state) 1)))
			(setf h 0)
			(setf positions (get_check_positions (list (nth i state) i)))
			(do ((j 0 (+ 1 j)))
				((> j (- (list-length positions) 1)))
				(setf h (+ h (aref board (nth 0 (nth j positions)) (nth 1 (nth j positions)))))
			)
			(setf attack (append attack (list h)))
		)
		attack
	)
)

; Get the column value for a given column which has the minimum number of attacks
; Input: state, col
; Output: value for the particular queen
(defun get_col_val (state col)
	(let (board vals)
		(setf board (get_board state))
		(do ((i 0 (+ 1 i)))
			((> i 7))
			(setf h 0)
			(setf positions (get_check_positions (list i col)))
			(do ((j 0 (+ 1 j)))
				((> j (- (list-length positions) 1)))
				(setf h (+ h (aref board (nth 0 (nth j positions)) (nth 1 (nth j positions)))))
			)
			(setf vals (append vals (list h)))
		)
		(position (reduce #'min vals) vals :from-end nil)
	)
)

; Check if the board is solved
; Input: conflicts for each column value
; Output: boolean(T if solved)
(defun is_solved (conflicts)
	(let (solved)
		(setf solved t)
		(do ((i 0 (+ 1 i)))
			((> i 7))
			(if (not (equal (nth i conflicts) 0))
				(setf solved nil)
			)
		)
		solved
	)
)

; Iterative repair algorithm
(defun iterative_repair (state)
	(let (steps)
		(setf steps 0)
		(do ()
			(nil)
			(setf steps (+ steps 1))
			(format t "steps ~S~%" steps)
			(setf conflicts (get_conflicts state))
			(if (is_solved conflicts)
				(progn
					(format t "Solved State ~S~%" state)
					(return)
				)
			)
			(setf col (position (reduce #'max conflicts) conflicts :from-end nil))
			(setf col_val (get_col_val state col))
			(setq new_state (copy-list state))
			(setf (nth col new_state) col_val)
			(format t "state ~S~%" state)
			(if (equal state new_state)
				(progn
					(format t "Generate new board")
					(setf state (generate_board))
				)
				(setf state new_state)
			)
		)
	)
)

; Get a random initialized board
; Input: 
; Output: randomly initialized board state
(defun generate_board ()
	(remove-duplicates (loop for i from 0 below 100 collect (random 8)) :test #'equal :from-end t)
)

; driver function
(defun main ()
	(setf start (generate_board))
	(iterative_repair start)
)


