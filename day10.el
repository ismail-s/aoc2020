;;; day10.el -*- lexical-binding: t; -*-
;;; Code:

(require 'cl-lib)

(defconst day10-input-example1 "16
10
15
5
1
11
7
19
6
12
4" "Day10 input example 1")

(defconst day10-input-example2 "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3" "Day10 input example 2")

(defconst day10-input "145
3
157
75
84
141
40
20
60
48
15
4
2
21
129
113
54
28
69
42
34
1
155
63
151
8
139
135
33
81
70
132
150
112
102
59
154
53
144
149
116
13
41
156
85
22
165
51
14
125
52
64
16
134
110
71
107
124
164
160
10
25
66
74
161
111
122
166
140
87
126
123
146
35
91
106
133
26
77
19
86
105
39
99
76
58
31
96
78
88
168
119
27
45
9
92
138
38
97
32
7
98
167
95
55
65" "Day10 input")

(defun day10-part1 (input)
  (let* ((adapters (mapcar #'string-to-number (split-string input "\n" t)))
         (sorted-adapters (sort adapters #'<))
         (sorted-adapters-with-ends (append '(0) sorted-adapters (mapcar (lambda (x) (+ x 3)) (last sorted-adapters))))
         (differences (cl-mapcar #'- (cdr sorted-adapters-with-ends) sorted-adapters-with-ends))
         (num-of-1-jolts (length (seq-filter (lambda (x) (eql x 1)) differences)))
         (num-of-3-jolts (length (seq-filter (lambda (x) (eql x 3)) differences))))
    (* num-of-1-jolts num-of-3-jolts)))

(defvar day10-adapters-arrangement-cache (make-hash-table :test 'eq))

(defun day10-num-of-arrangements (adapters)
  (cond ((gethash (car adapters) day10-adapters-arrangement-cache)
         (gethash (car adapters) day10-adapters-arrangement-cache))
        ((<= (length adapters) 2)
         (puthash (car adapters) 1 day10-adapters-arrangement-cache))
        (t
         (let* ((adapter (car adapters))
                (rest-of-adapters (cdr adapters))
                (adapters-within-3 (seq-take-while (lambda (a) (<= (- a adapter) 3)) rest-of-adapters)))
           (puthash adapter
                    (apply '+ (mapcar (lambda (a) (day10-num-of-arrangements (seq-drop-while (lambda (r) (not (eq r a))) rest-of-adapters)))
                                      adapters-within-3))
                    day10-adapters-arrangement-cache)))))

(defun day10-part2 (input)
  (let* ((adapters (mapcar #'string-to-number (split-string input "\n" t)))
         (sorted-adapters (sort adapters #'<))
         (sorted-adapters-with-ends (append '(0) sorted-adapters (mapcar (lambda (x) (+ x 3)) (last sorted-adapters)))))
    (setq day10-adapters-arrangement-cache (make-hash-table :test 'eq))
    (day10-num-of-arrangements sorted-adapters-with-ends)))

(message "Num of 1-jolt differences * num of 3-jolt differences: %d" (day10-part1 day10-input))
(message "Num of adapter arrangements: %d" (day10-part2 day10-input))

(provide 'day10)
;;; day10.el ends here
