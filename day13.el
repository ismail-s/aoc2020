;;; day13.el -*- lexical-binding: t; -*-
;;; Code:

(require 'seq)

(defconst day13-input-example "939
7,13,x,x,59,x,31,19")

(defconst day13-input "1001287
13,x,x,x,x,x,x,37,x,x,x,x,x,461,x,x,x,x,x,x,x,x,x,x,x,x,x,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,739,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,23")

(defun day13-part1 (input)
  (let* ((input-split (split-string input "\n" t))
         (earliest-timestamp (string-to-number (car input-split)))
         (bus-ids (mapcar #'string-to-number (seq-remove (lambda (bus-id) (equal bus-id "x")) (split-string (elt input-split 1) ","))))
         (temp-storage-space nil)
         (earliest-bus-id (car bus-ids))
         (time-to-wait-for-earliest-bus (round (- (* earliest-bus-id (fceiling (float (/ earliest-timestamp (float earliest-bus-id))))) earliest-timestamp))))
    (dolist (bus-id bus-ids)
      (setq temp-storage-space
            (round (- (* bus-id (fceiling (float (/ earliest-timestamp (float bus-id))))) earliest-timestamp)))
      (when (< temp-storage-space time-to-wait-for-earliest-bus)
        (setq earliest-bus-id bus-id
              time-to-wait-for-earliest-bus temp-storage-space)))
    (* earliest-bus-id time-to-wait-for-earliest-bus)))

(message "ID of earliest bus * num of mins to wait for that bus: %d" (day13-part1 day13-input))
; Part 2 I solved using Wolfram Alpha: https://www.wolframalpha.com/input/?i=solve+13x+%3D+t+%3B+37y+%3D+t%2B7+%3B+461z+%3D+t%2B13+%3B+17a+%3D+t%2B27+%3B+19b+%3D+t%2B32+%3B+29c+%3D+t%2B42+%3B+739d+%3D+t%2B44+%3B+41e+%3D+t%2B54+%3B+23f+%3D+t%2B67
; That query gives an integer solution which has, amongst other things, t=<some-long-number>n + 552612234243498 . So setting n=0 gives the smallest value of t that solves the equation with all positive values.
; Below is a bit of working out to come up with the equations to solve.

; 7x = t
; 13y = t + 1
; 59z = t + 4
; 31a = t + 6
; 19b = t + 7

; 13x = t
; 37y = t+7
; 461z = t+13
; 17a = t+27
; 19b = t+32
; 29c = t+42
; 739d = t+44
; 41e = t+54
; 23f = t+67

(provide 'day13)
;;; day13.el ends here
