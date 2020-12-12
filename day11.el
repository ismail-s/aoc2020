;;; day11.el -*- lexical-binding: t; -*-
;;; Code:

(defconst day11-input-example "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL" "day11 input example")

(defconst day11-input "LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLL.LL.LLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLL
LLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLL.LLLL.LLLLLLLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLL..LLLLLLL..LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLLLLL.LL.LLLLL.LLLLLLLLLLLLLLLLLLLLL..LLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLL
LLLLLLLLL.LLLLLLLLLLL..LLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLLLLLLL.L.LL.L..LLLLL.LLLLLLLL
LLLLLLLLL.LLLLLLLLLLL.LLLL.LLL.LLLLL.LLLLLLLL.LL.L.LLLLLL..LL.LLLLLL.LLLL..LLL.LLLLLL.LLLLLL.L
LLLLLLLL..LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLL.LL.LLL.LLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLL
LLLLLLLLLLLLLLLLLLLLL.LLL..LLLLLL.LL.LLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL
.LLL..L..........L.LL.LL..L..L.....L.....L.....L..L..L....L..L..LL..L.L..L..LLL..LL...L.LLLLL.
.LLLLLLLL.LL.L.LLLLLL.LLL..LLLLLLLLL.LLLLLLLLL.LLL.LLLLLLLLLL.L.L.LLLLLLLLLLLL.LLLLLLLLLLLLLLL
LLL.LLLLL.LLL.L.L.LLLLLLLL.LLLL.LLLL.LLLLLLLL...LL.LLLLLLLLLLLLLL.LLLLLLLLLL.L..LLL..LLLLLLLLL
LLLLLLLLLLLLLLLLLLLLL.L.LL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLL.
LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLL.L.LLLL.LLLL.LLLLLLLLLL
...LL.L......LL.......LLL...L.LL.LLL.L..L...L.L.LL...L.LLL.....L...L..L......L..LLLL........L.
LLLLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLL.L.LLLL.LL.LL..LLLL.LL.LLLLLLL.LLLL.LLLL..LLLL.LLLL
LLLLLLL.L.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL.LLL
.LL.LLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.L.LLLLLL.LLLL.LLL.LL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLL
LLLLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLL..LLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL
LLLLLLLLLLLLLLLLLLLLL..LLL.LLLLLLLLL.L.LLLLLL.LLLL.LLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLL.LL
LLLLLLLLL.LLLL..LLLLL.LLLL.LLLLLLLLLLLLLLL.LL.LLLL.LLLLLL.LL..LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLL
LL...L.L.LL.L....LL..L..L.L.LL.L..L.L...L.LL..L.L..L....L.....L.L.L.L.......L....L.....L..LLLL
LLLLLLLLL..LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.L.LLLLL.LLLLLL..L.LLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LL.LLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLL..LLLLLLLLLLLLLLLLLLL
LLLLLLLLL.LLL.LLLLLL..LLLL.LLLLLLLLL.L.LLLLLL...LL.L.LLL..LLLLLLL.LLLLL.L.LLL.LLLLLLLLLLL.LLLL
LLLLLLLLLLLLLLL.LLLLL.LL.LLLLLLLLLLL.LLL.LLLL.LLLL.LLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL
.L..LL.L.L......LL..L..L....LL.LLLLL.L.L.L.....L.L...LLL.LL...LL..L......L....L.LL..L...L.L...
LLLLLLLLL.LLLLL.LLLLL..LLL.LLLLLLLLL.LLLLLLLL.L.LL.LLLLLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLL.LLLLL.LL.LLLL.LLLLLLLLLLLLLL.LLL.LLL.LLLL.LLLL.LLLLLLLLLL
.LLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLL.LLLLLLLL.LL..LLLLLLL.LLLLLLL.LLLLLLL.LLL..LLLL.L.LLLLLLLL
L.LLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLL.LLLL.LLL.LL.L.LLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LL
LLLL.LLLLLLLLLL.LLLLL.LLLL.LLLLLLLL..LLLLLLLLLLLLL.LLLLLL.LLL.LLL.L.LLLLLLLLLL.LLLL.LLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLL.LLL.LL.LLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL
L.LLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLL.LLLL.L
LLLLLLLLL.LL.LL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLL.LLLLL.L.LLLLLLLLLLLL.LLLL.LLLLLLLLLL
LL..L.L.LL..L..L.LLLLL.L.L......L..L.....L..LLL.L...LL.........L...L.........LL...LL...L..L...
L.LLLLLLLLLLL.L.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLL
LLLLLLLLL..LLLL.LLLLL.LLLL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLL.LLL.LL.LLLLLLL.LLLLL
LLLLLLL.L.LLLLLLLLLLL.LLLL.LL.LLLLLL.LLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLL.LLLLLLL.LLLLLLL.LLLL
LLLLLLLLL.LLLLL..LLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLL.LLLLL.LLLL
LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLL.LL.LLLLLLLLLLL.LLLLLLL.L.LL.LLLL.LL.LLLLLLL
LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.L.LLLLLL.LLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLLLLLL
LLLLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLLLLL.LLLLLLLLLLL
...L......L.L...L...L...LL..LL..LLLL..L....LLLL....L...L.L.....LL..LL...LL........LL..L.L...LL
LLLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLL.LL.LLLLL.LLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLL..LLL.LLLL.L
LLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLL.LL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LL.LLL.L.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL
..L.L..LL.....L....LL.L.L.L..LLL......L..LLL..L..L....LL..L.LLL..L.....LL.L.L...........L..L..
LLLLLLLLLLLLLL.L.LLLL.LLLL.LL.LLLLLL.LLLLLLL..LLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLL.L.L
.LLLLLLLL.L.LLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL
LLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLL.LLLLLLLLL.LL.LLL.LLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLL.
..LLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLL
.L..L..LL..L.L.L.L...L..LL............L..LL..LL.L.L.........LL.....L...L.L.LLL.L.L..........L.
LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL..LLLL.LLLLLL.L.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLL
LLLLLLLLL.LLLLL.L.LLL.LLLL.LLLLLLLLLLLLLLLL.L.LLLL..LLLLLLLLLLLLL.LLLLLLL.LLLL.LL.L.LLLLLL.LLL
LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLL.L.LLLLLL.LLLL.LLLL.L.LLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLL.LL.LLLLLLL.LLLLLLL.LLLL.LLLL.L.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLL.LLLL..LL.LLLLLLL.LLLLLL.LLLLLLLLLLL.LL.LLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LL.LLLLLLLL.L.LLLLLL.LLLLLLL.LLL.L.LLLLLL.LLLL.LLLLLLLLLL
LLL.LLL.LLLLLLL.LLLLL..LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.LL.LLLLLLL.LLLLLL..LLLLLLLLL.LLLLLLLLLL
....L.L..L.L..L....L.L..L............LL.............L......L.L....L....L..L.LL.........LLL..L.
LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLL.L.LLLLLLLLLL.LLLL.LLLLLLLLLL
LLLLLLLLL.LLLLLLLLLLL.LLLL.LL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLL..LLL.LLLL.LLLLLL.LLL
LLLLLLLLL.LLLLL.LL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.L.LLLLLLL.LLLL.LLLLLLLLLLLLLLL
LLLLLLLLL.LL..LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLLL..LLLLLLL.LLLLLLLLL.LLLLLLL.LL
.LL..L..L..LLL...L..LL.L...LL..L.....L.L...L.L...L.L..L.L........L.L..L.L..L....L.....L..L..LL
LLLLLLLLL.LLLLLLLLLL..LLLL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLL.LL.L.LL.LLLLLLL
LLLLLLLLL.LLL.L.LLLLLLLLLL.L.LLLLLLLLLLLLLLLLLLLLL.LL.LLL.LLLLLLL.LLLLL.LLL.LL.LLLL.LLLLLLLLLL
LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLL.LLLLL.LLLLLL.LLLLLLL..LL.LLLLLLLLLLLLL.LLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLL
LLL.LLLLLLLL.LL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLL.LLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLL
..L....L........L....LL..L...LL.L.....L.LL.L..L....L....LL......L.L.....L.L..LL..L.....L.LLL.L
LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLL
LLLLL.LLLL.LLLLLLLLLL.LLLL.L.LLLLLLL.LLLLLLLL.LLLL.L.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL..LLLLLLLLLL
LL.LLLLLL.LLLLL.LLL.LL.LLL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLL.LLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.L
LLLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLL.LLLLLLLLLL.LLLLLLLLLLL.LLLLLL.L.LLLLLL.LLLL.LLLL.LLLLLLL.LL
LLL.LLLL..LLLLLLLLLLL.LLLL.LLLLLLLLL..LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLL..LLLLLLLLLLLLL.LLLLLLLLL.LLL.LLLLLL.LLLLLLL.LLLLLLLLLLLL.LLL..LLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL..LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLLLLLL.LLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLL.LLLLL.L.LL.LLLLL.LL.LLLLLLL.LLLLLLLL
LLLLLLLLLLL.LLL.LLLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLL.L
...LL.L..L...LL...LLL.........L.L.L..L.....LLLL.LL....L..L...L...LL..L..L......L..LLLL....L..L
LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLL.L.LLLLLL.LLLL.LLLL.L.LLL.LLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLL
LLLLL.LLL.LLL.L.LL.LL.LLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLL.L.LLLLLLL..LLL.LLLL.L.LLLLLLLL
LLLLLLLLLLLLL.L.LLLLL.LLLLL.LLLLLLLL.LLLLLLLLLLLLL..LL.L..LLLLLLL.LLLL.LL.LLLL.LLLL.LLLLLLLLLL
LLLLLLLL..LLLLLLLLLLL.L.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL..LL.LLLLLL.L..LLL.LLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLL.L.LLLLLLLL
.L....L.LL...L...LL......LL..........L.LLL...L...L..........L.........LL..L.L.L...L..L........
LLLLLLLLL.LLLLLLLLL.L.LLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLL..LLLLLLLLL
LLLLLLLLL.LLLLL.LL.LL.LLLL.LLLL.LLLL.LLLLLLLL.LLLL.LLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLL
LL.LLLLLL.LLLLL.LLLLL.LLLL.LL.LLLLLL.LLLLL.LL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLL.LLLLLLLLL
LLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLL.LL.LLLLL.LLLL.LL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LL
LLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLL..LLL.LLLLLL.LLLLLLL.LLLLLLL.LLLL.LL.LLLLLLLLLLLL
LLLLLLLLLL.L.LL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLL.LLLLLLL.L.LLLLL.LLLL.LLLLLLLLLLLLLLL" "Day11 input")

(defun day11-get-seats-adjacent-to (row col grid)
  (let* ((num-of-rows (length grid))
         (num-of-columns (length (car grid)))
         (left (if (= 0 col) ?u (elt (elt grid row) (1- col))))
         (right (if (<= (1- num-of-columns) col) ?u (elt (elt grid row) (1+ col))))
         (up (if (= 0 row) ?u (elt (elt grid (1- row)) col)))
         (down (if (<= (1- num-of-rows) row) ?u (elt (elt grid (1+ row)) col)))
         (up-left (if (or (= ?u up) (= ?u left)) ?u (elt (elt grid (1- row)) (1- col))))
         (up-right (if (or (= ?u up) (= ?u right)) ?u (elt (elt grid (1- row)) (1+ col))))
         (down-right (if (or (= ?u down) (= ?u right)) ?u (elt (elt grid (1+ row)) (1+ col))))
         (down-left (if (or (= ?u down) (= ?u left)) ?u (elt (elt grid (1+ row)) (1- col)))))
    (apply #'string (seq-remove (lambda (c) (= c ?u)) (list left right up down up-left up-right down-right down-left)))))

(defun day11-compute-grid-point (row col grid)
  (let* ((current-seat (elt (elt grid row) col))
         (adjacent-seats (day11-get-seats-adjacent-to row col grid)))
    (cond ((equal ?. current-seat) ".")
          ((equal ?L current-seat)
           (if (= 0 (length (seq-filter (lambda (s) (equal ?# s)) adjacent-seats))) "#" "L"))
          (t
           (if (<= 4 (length (seq-filter (lambda (s) (equal ?# s)) adjacent-seats))) "L" "#")))))

(defun day11-compute-next-grid (grid)
  (let* ((new-grid '())
         (new-row "")
         (num-of-rows (length grid))
         (num-of-columns (length (car grid))))
    (dolist (row (reverse (number-sequence 0 (1- num-of-rows))) new-grid)
      ; (setq new-row (dolist (col (reverse (number-sequence 0 (1- num-of-columns))))))
      (setq new-row (apply #'concat (mapcar (lambda (col) (day11-compute-grid-point row col grid)) (number-sequence 0 (1- num-of-columns)))))
      (setq new-grid (cons new-row new-grid)))))
; TODO maybe change grid to a vector of strings
(defun day11-num-of-occupied-seats (grid)
  (apply '+ (mapcar (lambda (grid-line) (length (seq-filter (lambda (c) (equal c ?#))
                                                            grid-line)))
                    grid)))

(defun day11-part1 (input)
  (let* ((grid (split-string input "\n" t))
         (new-grid (day11-compute-next-grid grid)))
    ; (message "grid is: %s" new-grid)
    (while (not (equal grid new-grid))
      (setq grid new-grid
            new-grid (day11-compute-next-grid grid)))
    (day11-num-of-occupied-seats grid)))

(defun day11-compute-up-left-for (row col grid num-of-rows num-of-columns)
  (cond ((= row 0) nil)
        ((= col 0) nil)
        (t
         (let* ((result nil)
                (blah nil)
                (curr-row (1- row))
                (curr-col (1- col)))
           (while (and (not result) (>= curr-row 0) (>= curr-col 0))
             (setq blah (elt (elt grid curr-row) curr-col))
             (when (not (= blah ?.)) (setq result blah))
             (setq curr-row (1- curr-row) curr-col (1- curr-col)))
           result))))

(defun day11-compute-up-right-for (row col grid num-of-rows num-of-columns)
  (cond ((= row 0) nil)
        ((= col (1- num-of-columns)) nil)
        (t
         (let* ((result nil)
                (blah nil)
                (curr-row (1- row))
                (curr-col (1+ col)))
           (while (and (not result) (>= curr-row 0) (< curr-col num-of-columns))
             (setq blah (elt (elt grid curr-row) curr-col))
             (when (not (= blah ?.)) (setq result blah))
             (setq curr-row (1- curr-row) curr-col (1+ curr-col)))
           result))))

(defun day11-compute-down-right-for (row col grid num-of-rows num-of-columns)
  (cond ((= row (1- num-of-rows)) nil)
        ((= col (1- num-of-columns)) nil)
        (t
         (let* ((result nil)
                (blah nil)
                (curr-row (1+ row))
                (curr-col (1+ col)))
           (while (and (not result) (< curr-row num-of-rows) (< curr-col num-of-columns))
             (setq blah (elt (elt grid curr-row) curr-col))
             (when (not (= blah ?.)) (setq result blah))
             (setq curr-row (1+ curr-row) curr-col (1+ curr-col)))
           result))))

(defun day11-compute-down-left-for (row col grid num-of-rows num-of-columns)
  (cond ((= row (1- num-of-rows)) nil)
        ((= col 0) nil)
        (t
         (let* ((result nil)
                (blah nil)
                (curr-row (1+ row))
                (curr-col (1- col)))
           (while (and (not result) (< curr-row num-of-rows) (>= curr-col 0))
             (setq blah (elt (elt grid curr-row) curr-col))
             (when (not (= blah ?.)) (setq result blah))
             (setq curr-row (1+ curr-row) curr-col (1- curr-col)))
           result))))

(defun seq-firstt (seq)
  (if (> (length seq) 0) (seq-first seq) nil))

(defun day11-get-seats-in-sight-of (row col grid)
  (let* ((num-of-rows (length grid))
         (num-of-columns (length (car grid)))
         (left (seq-firstt (seq-drop-while (lambda (s) (= s ?.)) (reverse (seq-take (elt grid row) col)))))
         ; so it's either nil, ?# or ?L
         (right (seq-firstt (seq-drop-while (lambda (s) (= s ?.)) (seq-drop (elt grid row) (1+ col)))))
         (up (seq-firstt (seq-drop-while (lambda (s) (= s ?.)) (reverse (seq-take (mapcar (lambda (grid-line) (elt grid-line col)) grid) row)))))
         (down (seq-firstt (seq-drop-while (lambda (s) (= s ?.)) (seq-drop (mapcar (lambda (grid-line) (elt grid-line col)) grid) (1+ row)))))
         (up-left (day11-compute-up-left-for row col grid num-of-rows num-of-columns))
         (up-right (day11-compute-up-right-for row col grid num-of-rows num-of-columns))
         (down-right (day11-compute-down-right-for row col grid num-of-rows num-of-columns))
         (down-left (day11-compute-down-left-for row col grid num-of-rows num-of-columns)))
    ; (when (= 1 row col) (message "Am here with %s %s %s %s %s %s %s %s" left right up down up-left up-right down-left down-right))
    (apply #'string (seq-remove (lambda (c) (equal c nil)) (list left right up down up-left up-right down-right down-left)))))

(defun day11-compute-grid-pointt (row col grid)
  (let* ((current-seat (elt (elt grid row) col))
         (adjacent-seats (day11-get-seats-in-sight-of row col grid)))
    (cond ((equal ?. current-seat) ".")
          ((equal ?L current-seat)
           (if (= 0 (length (seq-filter (lambda (s) (equal ?# s)) adjacent-seats))) "#" "L"))
          (t
           (if (<= 5 (length (seq-filter (lambda (s) (equal ?# s)) adjacent-seats))) "L" "#")))))

(defun day11-compute-next-gridd (grid)
  (let* ((new-grid '())
         (new-row "")
         (num-of-rows (length grid))
         (num-of-columns (length (car grid))))
    (dolist (row (reverse (number-sequence 0 (1- num-of-rows))) new-grid)
      ; (setq new-row (dolist (col (reverse (number-sequence 0 (1- num-of-columns))))))
      (setq new-row (apply #'concat (mapcar (lambda (col) (day11-compute-grid-pointt row col grid)) (number-sequence 0 (1- num-of-columns)))))
      (setq new-grid (cons new-row new-grid)))))
; TODO-maybe change grid to a vector of strings

(defun day11-part2 (input)
  (let* ((grid (split-string input "\n" t))
         (new-grid (day11-compute-next-gridd grid)))
    (message "grid is: %s" new-grid)
    (while (not (equal grid new-grid))
      (setq grid new-grid
            new-grid (day11-compute-next-gridd grid)))
    (day11-num-of-occupied-seats grid)))


; Performance of this code is very poor.
(message "Num of occupied seats: %d" (day11-part1 day11-input))
(message "Second num of occupied seats: %d" (day11-part2 day11-input))

(provide 'day11)
;;; day11.el ends here
