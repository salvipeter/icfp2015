(in-package :icfp2015)

;;; Global variables

(defvar *problem*)                      ; JSON data
(defvar *width*)
(defvar *height*)
(defvar *board*)                        ; two-dimensional array
(defvar *units*)                        ; internal storage: ((pivot-x pivot-y) ((x y) (x y) ...))
(defvar *current*)                      ; current unit

(defvar *ps-stream* nil)

(defparameter *phrases-of-power*
  '("Ei!" "Ia! Ia!" "R'lyeh" "Yuggoth"))

;;; Constants

(defparameter +api-token+ "g+1iBwAdnx5tGoHEAdObMHGEQG+4AFZ9DWkpM6UuFbU=")
(defparameter +command-code+
  '((w   #\p #\' #\! #\. #\0 #\3)
    (e   #\b #\c #\e #\f #\y #\2)
    (sw  #\a #\g #\h #\i #\j #\4)
    (se  #\l #\m #\n #\o #\  #\5)
    (rc  #\d #\q #\r #\v #\z #\1)
    (rcc #\k #\s #\t #\u #\w #\x)))


;;; Input/Output

(defun read-problem (filename)
  (cl-json:decode-json-from-string (read-file-into-string filename)))

(defun package-solution (id seed solution &optional tag)
  `((:problem-id . ,id)
    (:seed . ,seed)
    ,@(when tag `((:tag . ,tag)))
    (:solution . ,solution)))

(defun package-solutions (id seeds solutions)
  (iter (for seed in seeds)
        (for solution in solutions)
        (collect (package-solution id seed solution))))

(defun encode-solutions (stream id seeds solutions &optional tag)
  (cl-json:encode-json
   (coerce (iter (for seed in seeds)
                 (for solution in solutions)
                 (collect (package-solution id seed solution tag)))
           'vector)
   stream))

(defun send-solutions (id seeds solutions &optional tag)
  (let ((str (with-output-to-string (s)
               (encode-solutions s id seeds solutions tag))))
    (sb-ext:run-program "/usr/bin/curl"
                        (list "--user" (concatenate 'string ":" +api-token+)
                              "-X" "POST"
                              "-H" "Content-Type: application/json"
                              "-d" (format nil "~a" str)
                              "https://davar.icfpcontest.org/teams/22/solutions"))))

(defun initialize-board ()
  (setf *board* (make-array (list *width* *height*)
                            :initial-element 'empty))
  (dolist (xy (cdr (assoc :filled *problem*)))
    (setf (aref *board* (cdr (assoc :x xy)) (cdr (assoc :y xy)))
          'filled)))

(defun place-unit (pivot members)
  "Translates the unit to its spawning position."
  (let* ((min-x *width*)
         (max-x 0)
         (min-y *height*))
    (iter (for (x y) in members)
          (when (< x min-x)
            (setf min-x x))
          (when (> x max-x)
            (setf max-x x))
          (when (< y min-y)
            (setf min-y y)))
    (let* ((w (- max-x min-x -1))
           (left (floor (- *width* w) 2))
           (by (list (- left min-x) (- min-y))))
      (flet ((translate (p) (list (+ (first p) (first by)) (+ (second p) (second by)))))
        (list (translate pivot) (mapcar #'translate members))))))

(defun initialize (problem)
  (setf *problem* problem)
  (setf *width* (cdr (assoc :width *problem*)))
  (setf *height* (cdr (assoc :height *problem*)))
  (flet ((cell (xy) (list (cdr (assoc :x xy)) (cdr (assoc :y xy)))))
    (setf *units*
          (coerce (iter (for unit in (cdr (assoc :units problem)))
                        (collect (place-unit (cell (cdr (assoc :pivot unit)))
                                             (mapcar #'cell (cdr (assoc :members unit))))))
                  'vector)))
  t)


;;; Source generation

(defun generate-source (seed length)
  (iter (repeat length)
        (for next first seed then (logand (+ (* next 1103515245) 12345) #.(1- (expt 2 32))))
        (collect (ash (logand next #.(1- (expt 2 31))) -16))))


;;; Power command generation

(defun generate-power-commands (commands)
  "For the lightning round, this will be just random."
  (with-output-to-string (s)
    (dolist (cmd commands)
      (format s "~a" (random-elt (cdr (assoc cmd +command-code+)))))))


;;; Gameplay

(defun pivot (unit) (first unit))
(defun members (unit) (second unit))

(defun move-unit (unit dx dy)
  (flet ((translate (p)
           (let ((x (first p))
                 (y (second p)))
             (if (= dy 1)
                 (cond ((and (= dx -1) (oddp y))
                        (list x (1+ y)))
                       ((and (= dx 1) (evenp y))
                        (list x (1+ y)))
                       (t (list (+ x dx) (1+ y))))
                 (list (+ x dx) y)))))
    (list (translate (pivot unit))
          (mapcar #'translate (members unit)))))

(defun move-unit-w (unit) (move-unit unit -1 0))
(defun move-unit-e (unit) (move-unit unit 1 0))
(defun move-unit-sw (unit) (move-unit unit -1 1))
(defun move-unit-se (unit) (move-unit unit 1 1))

(defun directions (from to)
  (if (equal from to)
      '()
      (destructuring-bind ((x1 y1) (x2 y2))
          (list from to)
        (if (= y1 y2)
            (make-list (abs (- x2 x1)) :initial-element (if (< x1 x2) 'e 'w))
            (let ((dir (if (< y1 y2)
                           (if (< x1 x2) 'se 'sw)
                           (if (< x1 x2) 'ne 'nw))))
              (cons dir (directions (walk from (list dir)) to)))))))

(defun walk (from dirs)
  (if (null dirs)
      from
      (let* ((x (first from))
             (y (second from))
             (odd (oddp y)))
        (walk (ecase (first dirs)
                (ne (list (if odd (1+ x) x) (1- y)))
                (e (list (1+ x) y))
                (se (list (if odd (1+ x) x) (1+ y)))
                (sw (list (if odd x (1- x)) (1+ y)))
                (w (list (1- x) y))
                (nw (list (if odd x (1- x)) (1- y))))
              (rest dirs)))))

(defun rotate-unit (unit clockwisep)
  (let ((pivot (pivot unit))
        (dirs (if clockwisep
                  '(ne e se sw w nw ne)
                  '(ne nw w sw se e ne))))
    (labels ((next-dir (dir) (cadr (member dir dirs)))
             (rotate-point (p) (walk pivot (mapcar #'next-dir (directions pivot p)))))
      (list pivot
            (mapcar #'rotate-point (members unit))))))

(defun rotate-unit-clockwise (unit) (rotate-unit unit t))
(defun rotate-unit-counter-clockwise (unit) (rotate-unit unit nil))

(defun placeablep (unit)
  (iter (for (x y) in (members unit))
        (when (or (< x 0) (>= x *width*)
                  (< y 0) (>= y *height*)
                  (eq (aref *board* x y) 'filled))
          (return-from placeablep nil)))
  t)

(defun place-current ()
  (iter (for (x y) in (members *current*))
        (setf (aref *board* x y)
              'moving)))

(defun remove-current ()
  (iter (for (x y) in (members *current*))
        (setf (aref *board* x y)
              'empty)))

(defun full-row-p (row)
  (iter (for i from 0 below *width*)
        (unless (eq (aref *board* i row) 'filled)
          (return-from full-row-p nil)))
  t)

(defun move-down-to (row)
  (iter (for y from (1- row) downto 0)
        (iter (for x from 0 below *width*)
              (setf (aref *board* x (1+ y))
                    (aref *board* x y))))
  (iter (for x from 0 below *width*)
        (setf (aref *board* x 0) 'empty)))

(defun delete-full-rows (from)
  (when (>= from 0)
    (if (full-row-p from)
        (progn
          (move-down-to from)
          (delete-full-rows from))
        (delete-full-rows (1- from)))))

(defun lock-current ()
  (iter (for (x y) in (members *current*))
        (setf (aref *board* x y)
              'filled))
  (delete-full-rows (1- *height*))
  t)

(defun spawn-unit (k)
  "Returns NIL if the game ends."
  (setf *current* (elt *units* (mod k (length *units*))))
  (when (placeablep *current*)
    (place-current)
    t))

(defun solve-current ()
  (when *ps-stream*
    (write-ps-board *ps-stream*))
  (let ((e (move-unit-e *current*))
        (sw (move-unit-sw *current*))
        (se (move-unit-se *current*)))
    (cond ((placeablep e)
           (remove-current)
           (setf *current* e)
           (place-current)
           (cons 'e (solve-current)))
          ((placeablep sw)
           (remove-current)
           (setf *current* sw)
           (place-current)
           (cons 'sw (solve-current)))
          ((placeablep se)
           (remove-current)
           (setf *current* se)
           (place-current)
           (cons 'se (solve-current)))
          (t (lock-current)
             '(sw)))))

(defun solve (source)
  (initialize-board)
  (when *ps-stream*
    (write-ps-header *ps-stream*))
  (generate-power-commands
   (iter (for next in source)
         (while (spawn-unit next))
         (appending (solve-current)))))


;;; Visualization

(defun write-ps-header (stream)
  (let ((size (max *width* *height*))
        (header (read-file-into-string "honeycomb.eps")))
    (format stream "~a/size ~f def~%" header (/ 500 (sqrt 3) (1+ size)))))

(defun write-ps-board (stream)
  (format stream "~{~{~(~a~)~^ ~}~%~}showpage~%"
          (iter (for i from 0 below *width*)
                (appending (iter (for j from 0 below *height*)
                                 (collect (list i j (aref *board* i j))))))))

(defun write-ps (filename)
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (write-ps-header s)
    (write-ps-board s)))


;;; Main program - command line parameters etc.

(defun load-problem (n)
  (initialize (read-problem (format nil "problems/problem_~a.json" n)))
  (initialize-board))

(defun run-file (filename &optional sendp tag)
  (initialize (read-problem filename))
  (let* ((seeds (cdr (assoc :source-seeds *problem*)))
         (length (cdr (assoc :source-length *problem*)))
         (solutions (iter (for seed in seeds)
                          (for source = (generate-source seed length))
                          (collect (solve source)))))
    (if sendp
        (send-solutions (cdr (assoc :id *problem*)) seeds solutions tag)
        (package-solutions (cdr (assoc :id *problem*)) seeds solutions))))

(defun main ()
  (let ((n (length sb-ext:*posix-argv*))
        (files '()))
    (iter (for i from 1 to n by 2)
          (when (string= (elt sb-ext:*posix-argv* i) "-f")
            (collect (elt sb-ext:*posix-argv* (1+ i)))))
    (when (null files)
      (format *error-output* "Usage: ~a -f <filename>~%Other options are ignored.~%"
              (first sb-ext:*posix-argv*)))
    (cl-json:encode-json (coerce (iter (for file in files) (collect (run-file file))) 'vector)
                         *standard-output*)))
