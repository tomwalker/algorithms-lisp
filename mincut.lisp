(defun random-elt (seq)
  ;; Pick a random element out of a sequence
  (elt seq (random (length seq))))

(defun mincut (ht)
  ;; takes in hash table comprising of nodes as the keys and
  ;; what each node is connected to as the values. Using a
  ;; random contraction, this algorithm recursively reduces
  ;; hash table down to two nodes
  (if (<= (length (hash-table-keys ht)) 2)
      ht
      (let ((node1 (random-elt (hash-table-keys ht))))
        (let ((node2 (random-elt (gethash node1 ht))))
          (setf (gethash node1 ht)
                (concatenate 'list (remove node2 (gethash node1 ht))
                                   (remove node1 (gethash node2 ht))))
          (loop for key being the hash-keys of ht
               do (setf (gethash key ht)
                        (substitute node1 node2 (gethash key ht))))
          (remhash node2 ht)
          (mincut2 ht)))))

(defun load_file ()
  ;; take in file as hashtable
  ;; file format:
  ;; first integer is node number, subsequent numbers
  ;; are connected nodes
  (with-open-file (numbers "kargerMinCut.txt" :direction :input)
    (defparameter ht (make-hash-table))
    (loop for line = (read-line numbers nil)
       while line
       do (let ((processed-line (with-input-from-string (s line)
                                  (loop for y = (read s nil :end)
                                        until (eq y :end)
                                        collect y))))
            (setf (gethash (car processed-line) ht) (cdr processed-line))))
    ht))

(defun add-hash-values-length (ht)
  ;; takes in a hashtable, adds up the length of each value
  ;; returns the average length
  (let ((total 0))
    (loop for key being the hash-keys of ht
       using (hash-value value)
       do (setq total (+ total (length value))))
    (/ total (length (hash-table-keys ht)))))



(defun repeat-x-times (x)
  ;; runs algorithm X number of times and returns the
  ;; smallest number of cuts found
  ;; n^2 log n times will give a high likelihood of
  ;; the correct answer
  (let ((smallest 1000))
    (loop for i
       below x
       do (let ((current (add-hash-values-length (mincut (load_file)))))
            (if (< current smallest)
                (setq smallest current))))
    smallest))
