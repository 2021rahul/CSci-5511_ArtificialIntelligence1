;;;a-star
;;;Assumes you have defined
;;;1. A set of operators as lisp function
;;;2. a function solution-state?(description) to determine if a state is a solution
;;;3. a function to judge how close a state is to the goal: estimated-distance-from-goal(description)
;;;4. a function that indicates the cost of applying an operator cost-of-operator(Current-state, operator)


(defvar *trace-search* nil)

(defvar *graphing* nil "Should the search space be graphed? Only works for cities")
(defvar *pausing* nil "Should the system pause after each item graphed? Only works for cities")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilities to measure size of search space



(defstruct search-statistics
  (nodes-visited 0)
  (maximum-length-of-node-list 0)
  (length-of-solution 0)
  (maximum-depth 0)
  (cost-of-solution 0))

(defparameter *stats*(make-search-statistics))

(defun reset-stats()
  (setq *stats* (make-search-statistics)))

(defun update-statistics-for-a-star (stats e node-list)
  "Changes stat structure to reflect new number of nodes visited,
  and optionally new maximum length and depth"
  (setf (search-statistics-nodes-visited stats)
        (+ 1 (search-statistics-nodes-visited stats)))
  (when (> (length node-list)
           (search-statistics-maximum-length-of-node-list stats))
    (setf (search-statistics-maximum-length-of-node-list stats)
          (length node-list)))
  (when (> (a-node-depth e)
           (search-statistics-maximum-depth stats))
    (setf (search-statistics-maximum-depth stats)
          (a-node-depth e))))
           

;;;Search utilities
(defstruct (a-node ;elements of node-list
            (:print-function print-node))
  state
  transition-cost  ;cost of applying last operator to get here
  operator
  estimated-distance-from-goal
  cost-of-plan-so-far
  estimated-total-cost ; sum of estimated-distance-from-goal estimated-total-cost
  (path nil)
  (successors nil)
  (parent nil))


(defun print-node (o s d)
  (declare (ignore d))
  (format s "<Node :State ~a~% :path ~a~% :cost-of-plan-so-far ~4F :estimated-total-cost ~4F :depth ~a>"
          (a-node-state o)  (a-node-path o)(a-node-cost-of-plan-so-far o)(a-node-estimated-total-cost o)
          (a-node-depth o)))

(defun successor-state (description operator)
  "Given a state description and an operator, 
   returns the state when the operator is applied
   or nil if the operator can't be applied.
   Operator is the name of a function of 1 argument, a state description"
  (if (fboundp operator)  ;;if it has a function definition
    (funcall (symbol-function operator) description)
    (error "The operator ~a does not have a function definition" operator)))

(defun successor-a-node(node operator)
  "Given a node and an operator, 
   returns the node when the operator is applied.
   or nil if the operator can't be applied.
   The next node is formed from the next state and adding the operator
   to the front of the path of the current node
   Operator is the name of a function of 1 argument, a state description"
  (let ((next (successor-state (a-node-state node) operator)))
    (when next
      (when *graphing* (draw-transition (a-node-state node) next)
            (when *pausing*
              (format t "~%pause> ")
              (setq *pausing* (not (eq #\n (read-char))))))
      (let* ((transition-cost (cost-of-applying-operator (a-node-state node) operator))
             (estimated-distance-from-goal (estimated-distance-from-goal next))
             (cost-of-plan-so-far (+ transition-cost (a-node-cost-of-plan-so-far node)))
             (estimated-total-cost (+ cost-of-plan-so-far estimated-distance-from-goal)))
        (make-a-node :state next
                   :operator operator
                   :transition-cost transition-cost
                   :estimated-distance-from-goal estimated-distance-from-goal
                   :cost-of-plan-so-far cost-of-plan-so-far
                   :estimated-total-cost  estimated-total-cost 
                   )))))


(defun add-to-front(atom list)
  "Create a new list with atom at the end of list"
  (cons atom list))

(defun sort-by-estimated-total-cost(list)
  (sort list #'< :key #'a-node-estimated-total-cost))

(defun a-star (initial-state operators)
  "returns a list whose first element is the search statistics and 
  whose second elements is node indicating sequence of operator application
  and the cost of the plan.  see pg 76-77
  like best first graph search, but uses total cost"
  (let ((open (list (make-a-node :state initial-state 
                               :transition-cost 0
                               :cost-of-plan-so-far 0
                               :estimated-distance-from-goal (estimated-distance-from-goal initial-state)
                               :estimated-total-cost (estimated-distance-from-goal initial-state)
                               :parent nil
                               :successors nil)))
        (solved nil)
        (closed nil) ;; a list of states already explored
        (successor nil)
        (successor-was-on-open nil)
        (successor-was-on-closed nil)
        (best-node nil))
    (when *graphing* (create-map-window "a star"))
    (loop until (or (null open) ;;no more states
                    solved)
          do (setq best-node (first open))
          (setq closed (cons  best-node closed)) ;;add best-node to closed
          (update-statistics-for-a-star *stats* best-node open)
          (when *trace-search*
            (format t "~%~%exploring ~a" best-node))
          (setq open (rest open))
          
          (cond ((solution-state? (a-node-state best-node)) 
                 ;;if e is a solution, exit with success  
                 (setq solved t))
                (t ;;otherwise add successor to the end of open
                 (loop for rule in operators 
                       do (setq successor (successor-a-node best-node rule))
                       (when successor
                         (setq successor-was-on-open nil)
                         (setq successor-was-on-closed nil)
                         
                         (setf (a-node-parent successor) best-node)  ;a
                         (loop for old in open                     ;c
                               until (when (equalp (a-node-state successor)
                                                   (a-node-state old))
                                       (setq successor-was-on-open t)
                                       (setf (a-node-successors best-node)
                                             (cons old (a-node-successors best-node)))
                                       (when (< (a-node-cost-of-plan-so-far successor)
                                                (a-node-cost-of-plan-so-far old))
                                         (setf (a-node-parent old) best-node
                                               (a-node-cost-of-plan-so-far old) (a-node-cost-of-plan-so-far successor)
                                               (a-node-estimated-total-cost old) (a-node-estimated-total-cost successor))
                                         )
                                       t))
                         (unless successor-was-on-open
                           (loop for old in closed                     ;c
                               until (when (equalp (a-node-state successor)
                                                   (a-node-state old))
                                       (setq successor-was-on-closed t)
                                       (setf (a-node-successors best-node)
                                             (cons old (a-node-successors best-node)))
                                       (when (< (a-node-cost-of-plan-so-far successor)
                                                (a-node-cost-of-plan-so-far old))
                                         (setf (a-node-parent old) best-node
                                               (a-node-cost-of-plan-so-far old) (a-node-cost-of-plan-so-far successor)
                                               (a-node-estimated-total-cost old) (a-node-estimated-total-cost successor))
                                         (update-successors old) )
                                       t)))
                         (unless (or successor-was-on-open
                                     successor-was-on-closed)
                           (setf (a-node-successors best-node)
                                 (cons successor (a-node-successors best-node)))
                           (setq open (add-to-front successor open)))))
                 (setq open (sort-by-estimated-total-cost open)))))
    (when solved
      (let ((path (reverse (collect-path best-node))))
        (setf (search-statistics-cost-of-solution *stats*)
            (a-node-cost-of-plan-so-far best-node))
        (setf (search-statistics-length-of-solution *stats*)
              (length path))
        (setf (a-node-path best-node) path)
        (when *graphing*
          (draw-solution (cons *start-state* path)))
        best-node))))


(defun update-successors(old)
  (loop for s in (a-node-successors old)
        when (< (+ (a-node-cost-of-plan-so-far old)
                   (a-node-transition-cost s))
                (a-node-cost-of-plan-so-far s))
        do (setf (a-node-cost-of-plan-so-far s) 
                 (+ (a-node-cost-of-plan-so-far old)
                    (a-node-transition-cost s)))
        (setf (a-node-estimated-total-cost s)
              (+ (a-node-estimated-distance-from-goal s)
                 (a-node-cost-of-plan-so-far s)))
        (update-successors s)))


                  
                
                     

(defun collect-path(node)
  (when (and node (a-node-operator node))
    (cons  (a-node-operator node) (collect-path (a-node-parent node)))))

(defun a-node-depth(node)
  (if node
    (1+  (a-node-depth (a-node-parent node)))
    0))


(defvar *search-methods* nil "Used by Search Menu")
(unless (member 'a-star *search-methods*)
  (setf *search-methods* (cons 'a-star *search-methods*)))
  