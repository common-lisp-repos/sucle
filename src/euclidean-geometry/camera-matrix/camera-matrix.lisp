(defpackage #:camera-matrix
  (:use #:cl)
  (:export
   #:make-camera
   #:camera-vec-forward
   #:camera-vec-position
   #:camera-vec-noitisop
   #:camera-aspect-ratio
   #:camera-fov
   #:camera-frustum-far
   #:camera-frustum-near
   #:camera-matrix-projection-view-player)
  (:export
   #:update-matrices))
(in-package #:camera-matrix)

(defstruct camera
  (vec-position (rtg-math.vector3:make 0.0 0.0 0.0) :type rtg-math.types:vec3)

  (vec-up (rtg-math.vector3:make 0.0 1.0 0.0) :type rtg-math.types:vec3)
  (vec-forward (rtg-math.vector3:make 1.0 0.0 0.0) :type rtg-math.types:vec3)

  (vec-noitisop (rtg-math.vector3:make 0.0 0.0 0.0) :type rtg-math.types:vec3) ;;;the negative of position
  (matrix-player (rtg-math.matrix4:identity)) ;;positional information of camera
  (matrix-view (rtg-math.matrix4:identity))		    ;;view matrix
  (matrix-projection (rtg-math.matrix4:identity))	    ;;projection matrix
  (matrix-projection-view (rtg-math.matrix4:identity)) ;;projection * view matrix
  (matrix-projection-view-player (rtg-math.matrix4:identity))
  
  (fov (coerce (/ pi 2.0) 'single-float) :type single-float)

  (aspect-ratio 1.0 :type single-float)
  (frustum-near 0.0078125 :type single-float)
  (frustum-far 128.0 :type single-float))

(defun projection-matrix (result camera)
  (let ((fovy (camera-fov camera))
	(aspect (camera-aspect-ratio camera))
	(near (camera-frustum-near camera))
	(far (camera-frustum-far camera)))
    (let ((cot (/ (cos (/ fovy 2.0))
		  (sin (/ fovy 2.0)))))
      (let ((sum (+ far near))
	    (difference (- near far)))
	(rtg-math.matrix4.non-consing:set-components
	 (/ cot aspect) 0.0 0.0 0.0
	 0.0 cot 0.0 0.0
	 0.0 0.0 (/ sum difference) (/ (* 2.0 far near) difference)
	 0.0 0.0 -1.0 0.0
	 result)))))

(defun relative-lookat (result relative-target up)
  (let ((camright (rtg-math.vector3:cross up relative-target)))
    (declare (dynamic-extent camright))
    (rtg-math.vector3.non-consing:normalize camright)
    (let ((camup (rtg-math.vector3:cross relative-target camright)))
      (declare (dynamic-extent camup))
      (get-lookat result
		  camright
		  camup
		  relative-target))))

(defun get-lookat (result right up direction)
  (let ((rx (aref right 0))
	(ry (aref right 1))
	(rz (aref right 2))
	(ux (aref up 0))
	(uy (aref up 1))
	(uz (aref up 2))
	(dx (aref direction 0))
	(dy (aref direction 1))
	(dz (aref direction 2)))    
    (rtg-math.matrix4.non-consing:set-components
     rx ry rz 0.0
     ux uy uz 0.0
     dx dy dz 0.0
     0.0 0.0 0.0 1.0
     result)))



(progn
  ;;FIXME::RTG MATH does not have a way to multiply matrices into another matrix,
  ;;so reimplementing here. Ripped from sb-cga
  ;;FIXME::find a better place for this?x
  (declaim (ftype (function
		   (rtg-math.types:mat4 rtg-math.types:mat4 rtg-math.types:mat4)
		   rtg-math.types:mat4)
		  %matrix*))
  (defun %matrix* (result left right)
    "Multiply MATRICES. The result might not be freshly allocated if all,
or all but one multiplicant is an identity matrix."
    (macrolet ((inline-mul (left right dest)
		 `(progn
		    ,@(loop for i below 4
			 append (loop for j below 4
				   collect
				     `(psetf ;;has to be psetf? not sure?
				       (rtg-math.matrix4:mref ,dest ,j ,i)
				       (+ ,@(loop for k below 4
					       collect `(* (rtg-math.matrix4:mref ,left ,k ,i)
							   (rtg-math.matrix4:mref ,right ,j ,k))))))))))
      (inline-mul left right result)
      result)))

(defun update-matrices (camera)
  (let ((projection-matrix (camera-matrix-projection camera))
	(view-matrix (camera-matrix-view camera))
	(projection-view-matrix (camera-matrix-projection-view camera))
	(projection-view-player-matrix (camera-matrix-projection-view-player camera))
	(player-matrix (camera-matrix-player camera))
	(forward (camera-vec-forward camera))
	(up (camera-vec-up camera)))
    (projection-matrix projection-matrix camera)
    (relative-lookat view-matrix forward up)
    (%matrix* projection-view-matrix projection-matrix view-matrix)
    (rtg-math.matrix4.non-consing:set-from-translation player-matrix (camera-vec-noitisop camera))
    (%matrix* projection-view-player-matrix
	      projection-view-matrix player-matrix)))


;;;
;;;
;;;
#+nil
(defun spec-projection-matrix (near far left right top bottom)
  (let ((near-2 (* 2 near))
	(top-bottom (- top bottom))
	(far-near (- far near)))
      (rtg-math.matrix4:make
       (/ near-2 (- right left)) 0.0 (/ (+ right left) (- right left)) 0.0
       0.0 (/ near-2 top-bottom) (/ (+ top bottom) top-bottom) 0.0
       0.0 0.0 (- (/ (+ far near) far-near)) (/ (* -2 far near) far-near)
       0.0 0.0 -1.0 0.0)))
