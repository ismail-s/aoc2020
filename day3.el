;; -*- lexical-binding: t; -*-
;;; Code:

(setq day3-input-example "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(setq day3-input "............#....#.............
...........##....#......#..#..#
......#.......#......#.........
..#.#....#....#.............##.
..#........####....#...#.......
..##.....#.#.#..#.........#....
...#.#..#..#....#..#..#........
#.......#.........#....##.###..
......##..#.#...#.......#.#....
................##.........#.##
..##..........#...#.........#.#
..........#...##...............
#...#......#..#.#..#...##..#...
..##....#.......#......#..#....
....#......#......#....#.......
.........#.....#..#............
.#...#.#.........#........#....
#..........####.....#..........
......##.....#....#..#........#
#......#......#...........#....
....#.........#....#...#..#..#.
.#........#......#.#.....#.....
..#.#.#..........#....#.......#
......#.#........##....##....##
.....#.#..#...#................
......#......##...............#
..#..##.............#...##.....
......##......##..#......#.....
....#.............#..##.....##.
........#...............##.....
..#......#.##..#...#....#...#..
#......#.......#.............#.
.....#....##..............#....
#.#.........#....#..##....#....
.#...#...#....#.#............#.
...#...#.#..##.##.......##.....
......#..#....##..#.#..#..#....
.......##..#..#......#..#.....#
.##..#......#..........#....#..
.....#................#..#....#
........#..#....#.......#....#.
..#......#.......#......#....#.
....#...#.##........##....#....
.....#........#...........#....
...#....##..........#..#...#.#.
...#.......#......#...##...#...
.#.....#........#........#.#..#
.#.........#..##.....#.......#.
....#..#....#.......#......#...
.#.#...##..##................##
......#.#...#.......#....#....#
........#....#..#.....#......#.
.......#..........#......#.....
...............................
..#..#####..#..#..........#.#..
.....#....##................#.#
.................##............
.#...#...#..#...........#...##.
..#..#.#...........#.....##....
.#.......#.....#..##..#.#....#.
..........#.#......##...##.....
........##..#......##...#......
#......................#.......
............#.....#....#.#...#.
#......#..........##..#........
.........#.......#...#.#.......
...........##....#........#....
#........#.....#...#........##.
.#......##......#.##.......#..#
.....#......#.#......#.......#.
.....#.#.........#.............
...........#..#....#.....#.#...
...#............#...#..........
..#..#...#.....................
......#..#...#....#............
.#.#.#........#..#...#.........
..........#........#..#........
..............#...#....#.......
..#....#....##.......#...#.##..
.#.........#...#......#........
..#......#...#.........##.#...#
...#.....#...#..#.............#
.##........#.#.#.............#.
..#.............#..#.#...#....#
#...#.........#......#......#..
.......##..#.#..........#...#..
.......#.............#..#.#....
.#..#....#.#...................
....##...#..#....#..#..........
....#.#............#...........
###........##..#.#..#..........
.#.#.#.......#...........#..#.#
..........##..#.............#..
.#...........#......#.#..#..##.
...###......#.##........#.....#
....#..#..#...#................
...#.....#........#............
....#...#...#..#..##.##.......#
#.......#......#....#.......#..
#.............#...#............
##......#..#...#....##.#...#...
.##....................#....#..
..#.....#....#.#....#......#...
.......#..#..#............#...#
.#.....#.......#..#..#..#......
......##.......................
#..#...#.#.#....#.....#..#.....
...................#...#...#...
........#....##..#....#........
##......#.#......##.###........
.........#...##................
.......#...#...#.......##......
....#.......#......#.........##
....#....#.#..#.....#..........
...........#.......#........#..
..#.........###.#........#.....
.......#...........#.#.....##..
..#...#..#..........#..........
..........#.#....#.............
.##....#........##.............
.............#.#####........#.#
.................##...#........
##...#.#.......##........#.....
.#...#...#..#..#....#....#.....
..#...#........#..#............
##...#.#........#......##.#..##
.##......#..............##.#..#
.........#...#............#...#
....#..#....#...........#......
........#..#....#...##...#.....
..#..............#...#.#.....#.
.#.......#.#.....#..###.......#
...................#.......#...
........##.....#..#.......##...
.....#....................#...#
...#.#....#............#.#.....
#.......#.......#....#.........
..#...............#............
##...#...#...#..............#..
...#..........#..#....##.......
#............##.##......#.#.#..
.#...........#.........#....##.
..##....##.#....#.#.#.##...##.#
........#.#.#.............#....
.#...........#....##...#...#.#.
.##...#.................#......
....#.#..#....................#
.##......#........#..#.........
...#...............#...........
.#.#..##..##.#........#........
...........#....#.#.#......#...
...................#........#.#
..#............#...#.#........#
....#....#.#.##......#...#.....
..................#............
..........................#....
........#......................
......#.#...#.#..##......#.#.#.
.........#...#..#..............
..#.......#..........##..#.....
.........#............#........
......#..#..#...###....#....#..
#..#..............##.###..##..#
.#..................#.....#...#
........#........#........#....
.........#........#.##......#..
..#.....#.#..###...#....#......
..#................##....#.....
..#.#....##.....#......##...#..
...#.......#........##.........
#........#...#.#..........##..#
................#...#.#.....#..
.........#..#..#.#..#.#...#....
##....#...##.........#.#...#.##
....#..#.....##.....#.....##...
................#............#.
..#..#...#.....#......#.....##.
....#.......#...#...#...#..#...
....#..##....#.###.#...#..#....
#..##.....#.....#.##..##...##.#
.............###..........#....
..................#.....###....
..........#....#...#......#....
...#..##.......#......#.#...#..
..#.......................##.#.
..#..#..#....#......#...#...##.
#.............#................
..........#.#.#.........#.#....
.....##..#......##.#...........
.#.#.#.#....#.#...#.....#.#...#
......#.....##..............##.
#..#.......##..##..............
#..#..#................###.....
.....#......#.........#........
#...........#........#.#.......
#........#.#...#....#....###..#
###..#.#...........#.##.....#.#
..#..........#..#............#.
...#....#.......#..#.....###...
.#....#.##.#..###..............
.....#.##.##.......###.##...#.#
..#..##.......###..............
.#.........###..#..............
..................###.....#..#.
#....#....#.........#.....#....
.........#.#..#....#.....#.....
....##.......##.......#.#......
.....#...#.##.....#............
....#.#.#.......#..............
.##..#.#..#.......##...........
....#....##..#.....##.......#.#
.....##....#..#.#........#.....
........#.#.#....#....##...#..#
..#......#.#.#..#.##....#.#.#..
..#...#........#..#..........#.
.........#...................#.
........#.....##..#....#....#..
#..............#..........#....
#........#.#...........#.#.....
..#......................#.#..#
.........#.#.....#.#..........#
......#....#.#.##........#.....
.#....##......##..#...#.......#
..#........#...#.##....#..#.#..
.......#.....#..........#.....#
.........#.#..#.........#....#.
..........#.##.........##..#...
......#.#..#.....#.#..........#
......#.#.#..#..#.#............
...##.#..#..............#....#.
#..........#...................
.#....#..#.#.......#........#..
...#...#......#....#......#....
..#.#.......#.......#.......#.#
...#.#...#........#.....#......
#.......#..#...................
#..#..#.............#..#..#..#.
#.......................#....##
.#.........#....#....#.........
...............#...#..#....#..#
#.....#.#...#.#.....#..........
....##.#..#...#.#....###...#.#.
.................#....#........
####.......##...##.......#.##..
#..#....#....##............#...
..##......#..#........#........
....#..#..........#......#...##
..#.#.............#...........#
#...............#...#.......#.#
#..#.........#.##.#.......#...#
......#.....#.............#...#
......#.##.........##...#......
..#......##.#........#.......#.
#..#.........#.##..............
..#....#...#...#..#.....#.#....
................#.......#......
#.....#..............##....#.##
##.....#...#.#.....#..##...#...
#.#............##..........#..#
..#.##......#..#....#..........
....##.#....#.......##.....#...
......#.#....###...#...........
..................#......#....#
..............##...............
......#..#....#.....#..........
.......#........#...#..........
..#......#......##..#.##..#....
..#.#...#...............#......
....#.#.............#.#......#.
....#.#.....#......#..#.......#
........................#..#...
.................#...........#.
#......#......#.#.#.....##.....
..#....##...#.....##.#.....#..#
....#.........#....#.##.#.#....
..#....###.....................
.....#.#....#......#....##....#
#.......#...#......##.......#..
#....#.........##.....#........
#.....#...........#..#.....#...
.................#.....#..##..#
..#...#......####...##.........
...............................
#........#.....#...............
.#.........#....#.#......##....
...#..........#.........#.#.#.#
......##......#....###........#
.....................#.#.#.....
......#..#..#.......#...#......
...##.#.............#.#.......#
..#.#...#..#....#.....#.....#..
..#..#.....................#..#
........#....#..........#..#...
#.##....#..#.#..#............#.
..............###.............#
.#.#..........#.#....#...#....#
....#..........#.#..#......#...
.........##.#...#..............
..................#.....#.#....
.#....#.......#.##.#.........#.
.##..#...#......#..#...........
.#.........#..........#.#......
#.#......#.#.#.#.......#...#.#.
.......#....#.#......#......#..
...#..#....#.#..#..##...##.....
#.#.#.......#....#.........##..
#..#....#........###....#.#....
....#..#.........#....#...#....
...#.#.#.#..#..##.....#.##.....
.......#.......#...............
#.#.#......##....#.............
...#.##........#.....#...##.#..
...#.#.###..........#.......#..
.....#...#.......#.........#...
............#..#...#..##.......
...#....#..##.##..........#.##.
..................#........#...
....#.##.#.##........#.#.......
.#...........##.....##.......#.
#...#.........#.....##.........
#..#....#.#.........#..........
..#......#.#.#......#.....#..#.
..##......#..............#.....")


(defun day3-part1 (input)
  (let* ((input-list (vconcat (split-string input "\n" t)))
         (x-length (length (elt input-list 0)))
         (total 0)
         (x 0)
         (y 0)
         (x-change 3)
         (y-change 1))
    (while (< y (length input-list))
      (when (eql ?# (elt (elt input-list y) x))
        (setq total (1+ total)))
      (setq x (% (+ x-change x) x-length))
      (setq y (+ y-change y)))
    (message "Number of trees encountered: %d" total)))

(defun day3-try-slope (input-list x-change y-change)
  (let* ((x-length (length (elt input-list 0)))
         (total 0)
         (x 0)
         (y 0))
    (while (< y (length input-list))
      (when (eql ?# (elt (elt input-list y) x))
        (setq total (1+ total)))
      (setq x (% (+ x-change x) x-length))
      (setq y (+ y-change y)))
    total))

(defun day3-part2 (input)
  (let* ((input-list (vconcat (split-string input "\n" t))))
    (* (day3-try-slope input-list 1 1)
       (day3-try-slope input-list 3 1)
       (day3-try-slope input-list 5 1)
       (day3-try-slope input-list 7 1)
       (day3-try-slope input-list 1 2))))

(day3-part1 day3-input)
(day3-part2 day3-input)

(provide 'day3)
;;; day3.el ends here
