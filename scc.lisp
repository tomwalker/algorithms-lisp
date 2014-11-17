;; recursive implementation

(defun kosaraju (graph)
  (load "hash-tables")
  (let ((f-order (dfs-loop-1st (transpose-graph graph))))
    (dfs-loop-2nd graph f-order)))

(defun dfs-loop-1st (graph)
  (defparameter *t-counter* 0)
  (defparameter *f* (make-hash-table))
  (defparameter *explored* '())
  (loop
     for node in (hash-table-keys graph)
     do
       (if (not (member node *explored*))
           (dfs graph node)))
  *f*)

(defun dfs-loop-2nd (graph f-order)
  (defparameter *leader* (make-hash-table))
  (defparameter *s* 0)
  (defparameter *explored* '())
  (loop
       for node in (sort (hash-table-keys f-order) #'>)
       do
         (if (not (member (gethash node f-order) *explored*))
             (progn
               (setf *s* (gethash node f-order))
               (dfs2 graph (gethash node f-order)))))
  *leader*)

(defun dfs (graph start)
  (let ((arcs (gethash start graph)))
    (if *explored*
        (setf *explored* (cons start *explored*))
        (setf *explored* (list start)))
    (loop
         for arc in arcs
         do
         (if (not (member arc *explored*))
             (dfs graph arc)))
    (setf *t-counter* (1+ *t-counter*))
    (setf (gethash *t-counter* *f*) start)))

(defun dfs2 (graph start)
  (let ((arcs (gethash start graph)))
    (if *explored*
        (setf *explored* (cons start *explored*))
        (setf *explored* (list start)))
    (setf (gethash *s* *leader*)
          (if (gethash *s* *leader*)
              (1+ (gethash *s* *leader*))
              1))
    (loop
         for arc in arcs
         do
         (if (not (member arc *explored*))
             (dfs2 graph arc)))
    (setf *t-counter* (1+ *t-counter*))
    (setf (gethash *t-counter* *f*) start)))

(defun transpose-graph (old-graph)
  ;; pass in directed graph as hash table
  ;; return graph with arcs going in opposite direction
  (let ((new-graph (make-hash-table)))
    (loop
       for node in (hash-table-keys old-graph)
       do
         (loop
            for value in (gethash node old-graph)
            do
              (progn
                (setf (gethash value new-graph)
                      (if (gethash value new-graph)
                          (append (gethash value new-graph) (list node))
                          (list node)))
                )))
    new-graph))

(defun load_file ()
  ;; put file as hashtable with first item on each line as key
  (with-open-file (numbers "test1.txt" :direction :input)
    (defparameter x (make-hash-table))
    (loop for line = (read-line numbers nil)
       while line
       do (let ((processed-line (with-input-from-string (s line)
                                  (loop for y = (read s nil :end)
                                     until (eq y :end)
                                     collect y))))
            (setf (gethash (car processed-line) x)
                  (append (gethash (car processed-line) x) (cdr processed-line)))))
    x))

(maphash #'(lambda (key value) (print value) (print "###")) (kosaraju (load_file)))
