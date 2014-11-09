;; made this for the coursera + stanford algorithm course
;; task was to count the number of comparisons that were made.
;; modified it slightly to return the sorted sequence


(defun load_file ()
  ;; assumes that file is a text file and each new line is a
  ;; seperate number
  (with-open-file (numbers "QuickSort.txt" :direction :input)
    (loop for line = (read-line numbers nil)
       while line
       collect (parse-integer line :junk-allowed t))))


(defun choose_median_pivot (lst)
  ;; returns a cons pair
  ;; first item is value
  ;; second item is position within supplied list
  (let ((1_v (cons (car lst) 0)) ;; first
        (mid_v (if (equal 0 (mod (length lst) 2))
                   (cons (nth (- (/ (length lst) 2) 1) lst) (- (/ (length lst) 2) 1))
                   (cons (nth (/ (- (length lst) 1) 2) lst) (/ (- (length lst) 1) 2)))) ;; middle
        (last_v (cons (car (last lst)) (- (length lst) 1)))) ;; last
    (second (sort (list 1_v mid_v last_v) #'< :key #'first))))


(defun quicksort (sequence)
  ;; quick sort using the median of the first, middle and last element in the
  ;; sequence that is passed in.
  ;; Should average n log n running time.
  ;; Runs 'in place' so log n memory used.
  (labels ((swap (x y)
             (rotatef (elt sequence x) (elt sequence y)))
           (sub-sort (left right)
             (if (< left right)
                 (let ((pivot (car (choose_median_pivot (subseq sequence left (1+ right)))))
                       (pivot_index (+ left (cdr (choose_median_pivot (subseq sequence left (1+ right))))))
                       (index (+ 1 left)))
                   (swap left pivot_index)
                   (loop for i from (+ 1 left) to right
                      when (<= (elt sequence i) pivot)
                      do (swap i (prog1 index (incf index))))
                   (swap left (- index 1))
                   (+ (if (<= left (1- index))
                          (- (1- index) left)
                          0)
                      (if (<= index right)
                          (- right (1- index))
                          0)
                      (sub-sort left (- index 2))
                      (sub-sort index right))
                   )
                 0)))
    (sub-sort 0 (1- (length sequence)))
    (print sequence)))
