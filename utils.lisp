(defun percent-of (percent total)
  (* 100 (/ (float percent) (float total))))

(defun percent-of* (percent total)
  "Rounds the result."
  (round (* 100 (/ (float percent) (float total)))))

(defun /* (a b)
  "Rounds the result."
  (round (/ a b)))

(defun matching-ratio (value total1 total2)
  "The same percent that VALUE is to TOTAL1, for TOTAL2"
  (* (/ (percent-of value total1) 100) total2))

(defun matching-ratio* (value total1 total2)
  "The same percent that VALUE is to TOTAL1, for TOTAL2. Rounds the result."
  (round (* (/ (percent-of value total1) 100) total2)))

(defun cdr-assoc (item list)
  "(cdr (assoc {item} {list}))"
  (cdr (assoc item list)))

(defun quadratic-solution-p (a b c)
  "Instead of returning the quadratic solution, only tells whether a solution exists"
  (let ((root (- (* b b) (* 4 a c))))
    (if (or (< root 0)
	    (= a 0))
        nil
	t)))

(defun rgb (r g b)
  "Returns arguments as one RGB value. Assumes alpha channel is opaque. Arguments >255 are treated as =255."
  (if (> r 255)
      (setf r 255))
  (if (> g 255)
      (setf g 255))
  (if (> b 255)
      (setf b 255))
  (logior #xff000000
          (+ (ash r 16)
             (ash g 8)
             b)))

(defun quad (a b c)
  "Solve for x in ax^2 + bx + c"
  (let ((root (- (* b b) (* 4 a c))))
    (cond ((< root 0) nil)
	  ((= a 0) nil)
	  (t (progn
	       (let ((numerator-max (+ (- b) (sqrt root)))
		     (numerator-min (- (- b) (sqrt root)))
		     (denominator (* 2 a)))
		 (values (/ numerator-max denominator) (/ numerator-min denominator))))))))


(defun compare-cdr (list1 list2 &key (test #'eql))
  "To compare association lists. Returns true if car of each item in list1 exists in list2, and both associated cdrs return true when passed to TEST."
  (let ((samep t))
    (dolist (item list1)
      (if (not (and (assoc item list2)
	            (funcall test (cdr item) (cdr-assoc (car item) list2))))
	  (setf samep nil)))
    samep))
