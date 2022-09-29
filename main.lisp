(defparameter *window-width* 0)
(defparameter *window-height* 0)
(defparameter *viewport-width* 1.6)
(defparameter *viewport-height* 0.9)

(defstruct sphere center radius color smoothness)
(defstruct light type center intensity direction)
(defparameter *spheres* (list (make-sphere :center (vec 2.0 0.0 5.0) :radius 1.0 :color #xffff0000 :smoothness 50.0)
			      (make-sphere :center (vec 0.0 -0.5 4.0) :radius 1.0 :color #xff00ff00 :smoothness 1000.0)
			      (make-sphere :center (vec -2.0 0.0 5.0) :radius 1.0 :color #xff0000ff :smoothness 0.0)))
(defparameter *all-lights* (list (make-light :center (vec 0.0 7.0 2.0)
					     :type 'point
					     :intensity 1.0
					     :direction nil)
				 (make-light :center nil
					     :type 'ambient
					     :direction nil
					     :intensity 0.2)))
(defparameter *camera* (vec 0 0 0))

(defun sphere-red (sphere)
  (ash (logand (sphere-color sphere) #x00ff0000) -16))

(defun sphere-green (sphere)
  (ash (logand (sphere-color sphere) #x0000ff00) -8))

(defun sphere-blue (sphere)
  (logand (sphere-color sphere) #x000000ff))

(defun canvas->viewport (x y)
  (vec (* (float x) 
	  (float (/ *viewport-width* *window-width*)))
       (* (float y)
	  (float (/ *viewport-height* *window-height*)))
       1.0))

(defun compute-smoothness (viewport-point point sphere)
  (let ((final-intensity 0.0))
    (dolist (light *all-lights*)
      (if (and (> (sphere-smoothness sphere) 0)
	       (not (eq (light-type light) 'ambient)))
	  (progn
            (let* ((point-normal (v/ (v- point (sphere-center sphere)) (vlength (v- point (sphere-center sphere)))))
      	           (l (v- (light-center light) point))
      	           (r (v- (v* 2 point-normal (v. point-normal l)) l))
      	           (increment (/ (v. r viewport-point) (* (vlength r) (vlength viewport-point)))))
      	          (if (> increment 0)
      	              (incf final-intensity (expt increment (sphere-smoothness sphere))))))))
    final-intensity))

(defun compute-lighting (viewport-point point sphere)
  (let ((final-intensity 0.0))
    (dolist (light *all-lights*)
      (if (eq (light-type light) 'ambient)
	  (incf final-intensity (light-intensity light))
	  (progn
            (let* ((point-normal (v/ (v- point (sphere-center sphere)) (vlength (v- point (sphere-center sphere)))))
      	           (light-vector (v- (light-center light) point))
      	           (increment (* (light-intensity light) 
				 (/ (v. point-normal light-vector) 
				    (* (vlength point-normal) (vlength light-vector))))))
      	      (if (> increment 0)
      	          (incf final-intensity increment))
	      (incf final-intensity (* (light-intensity light) (compute-smoothness (v- viewport-point) point sphere)))))))
    (let ((red (round (* (sphere-red sphere) final-intensity)))
	  (green (round (* (sphere-green sphere) final-intensity)))
	  (blue (round (* (sphere-blue sphere) final-intensity))))
      (rgb red green blue))))

(defun get-pixel (x y)
  (let ((desired-color #xff222233)
	(closest-sphere nil)
	(closest-distance 1000000))
    (dolist (sphere *spheres*)
      (let* ((d (v- (canvas->viewport x y) *camera*))
             (co (v- *camera* (sphere-center sphere)))
             (a (v. d d))
             (b (* 2 (v. co d)))
             (c (- (v. co co) (* (sphere-radius sphere) (sphere-radius sphere))))
	     (quad-solution (multiple-value-list (quad a b c)))
	     (min-distance 0))
	(if (numberp (first quad-solution))
	    (progn
	      (setf min-distance (min (first quad-solution) (second quad-solution)))
	      (if (> min-distance 1)
		  (progn
		    (if (< min-distance closest-distance)
		        (progn (setf closest-distance min-distance)
			       (setf closest-sphere sphere)))))))))
    (if closest-sphere
        (setf desired-color (compute-lighting (canvas->viewport x y) (v+ *camera* (v* closest-distance (v- (canvas->viewport x y) *camera*))) closest-sphere)))
    desired-color))

(defun main ()
  (spill:init)
  (let* ((window (spill:create-window "Renderer" 0 0 (getf (spill:get-screen-size) :w) (getf (spill:get-screen-size) :h)))
	 (window-width (getf window :width))
	 (window-height (getf window :height))
	 (surface (spill:fill-surface (spill:create-surface window 0 0 window-width window-height) #xff000000))
	 (gui (spill:create-gui window)))
    (setf *window-width* window-width)
    (setf *window-height* window-height)
    (loop for y from (- (/* *window-height* 2)) to (/* *window-height* 2) do
	  (loop for x from (- (/* *window-width* 2)) to (/* *window-width* 2) do
	     (spill:draw-point* surface x y (get-pixel x y))))
    (spill:when-running gui
			'default
			(lambda (data)
			  (declare (ignorable data))
			  (spill:blit surface window))
			nil)
    (spill:run-application gui)
    (spill:close-application gui)
    (spill:free-surface surface))
  (spill:quit))
