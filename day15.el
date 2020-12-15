;;; day15.el -*- lexical-binding: t; -*-
;;; Code:

(defconst day15-input-example "0,3,6")

(defconst day15-input "0,12,6,13,20,1,17")

(defun day15 (input nth-num-to-find)
  (let* ((starting-numbers (mapcar #'string-to-number (split-string input "," t)))
         (previously-spoken-numbers (make-hash-table :test #'eql))
         (last-spoken-number (car (last starting-numbers)))
         (last-spoken-num-pos (length starting-numbers))
         (temp-storage-space nil))
    (dotimes (i (length starting-numbers))
      (puthash (elt starting-numbers i) (1+ i) previously-spoken-numbers))
    (while (not (= nth-num-to-find last-spoken-num-pos))
      (if (gethash last-spoken-number previously-spoken-numbers)
          (progn
            (setq temp-storage-space (- last-spoken-num-pos (gethash last-spoken-number previously-spoken-numbers)))
            (puthash last-spoken-number last-spoken-num-pos previously-spoken-numbers)
            (setq last-spoken-number temp-storage-space))
          (puthash last-spoken-number last-spoken-num-pos previously-spoken-numbers)
          (setq last-spoken-number 0))
      (setq last-spoken-num-pos (1+ last-spoken-num-pos)))
    last-spoken-number))

(message "2020th number spoken is %d" (day15 day15-input 2020))
(message "30000000th number spoken is %d" (day15 day15-input 30000000))


(provide 'day15)
;;; day15.el ends here
