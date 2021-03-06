
(in-package :sucle)

;;;;************************************************************************;;;;
;;;;<BOXES?>
(defun create-aabb (&optional (maxx 1.0) (maxy maxx) (maxz maxx)
		      (minx (- maxx)) (miny (- maxy)) (minz (- maxz)))
  (aabbcc:make-aabb
   :minx minx
   :maxx maxx
   :miny miny
   :maxy maxy
   :minz minz
   :maxz maxz))

(defparameter *block-aabb*
  ;;;;1x1x1 cube
  (create-aabb 1.0 1.0 1.0 0.0 0.0 0.0))

(defparameter *slab-aabb*
  ;;;;slab
  (create-aabb 1.0  #+nil 0.5 1.0 1.0 0.0 0.0 0.0))

;;;;[FIXME]The point of this is to reduce the amount of bits to store the hitbox.
;;;;Why? because when there is an inexact number, like 0.3, there are bits at the end which
;;;;get chopped off or something, thus leading to strange clipping.
;;;;This effectively reduces the precision, giving leeway for math operations.
;;;;My prediction could be wrong though.
(defun round-to-nearest (x &optional (n (load-time-value (/ 1.0 128.0))))
  (* n (round (/ x n))))
(defparameter *player-aabb*
  (apply #'create-aabb
	 (mapcar 'round-to-nearest	 
		 '(0.3 0.12 0.3 -0.3 -1.5 -0.3))))

;;;a very small cubic fist
(defparameter *fist-aabb* (create-aabb 0.00005))

(defparameter *chunk-aabb*
  (apply 'create-aabb
	 (mapcar 'floatify
		 (list
		  voxel-chunks:*chunk-size-x*
		  voxel-chunks:*chunk-size-y*
		  voxel-chunks:*chunk-size-z*
		  0.0
		  0.0
		  0.0))))

(defparameter *big-fist-aabb*
  (create-aabb
   ;;0.5
   ;;1.5
   8.0))
;;;;</BOXES?>

(defparameter *with-functions*
  #+nil
  (list
   (lambda (x)
     (print 34)
     (unwind-protect 
	  (funcall x)
       (print 2))))
  (list
   'call-with-world-meshing-lparallel))
(defun run-with (fun)
  (flet ((nest (with-fun cont)
	   (lambda ()
	     (funcall with-fun cont))))
    (dolist (with-fun *with-functions*)
      (setf fun (nest with-fun fun))))
  fun)

(defun start ()
  (application:main
   *sucle-app-function*
   :width (floor (* 80 text-sub:*block-width*))
   :height (floor (* 25 text-sub:*block-height*))
   :title ""))

(defparameter *sucle-app-function*
  (run-with
   (lambda ()
     #+nil
     (setf (entity-fly? *ent*) nil
	   (entity-gravity? *ent*) t)
     ;;(our-load)
     (let ((text-sub:*text-data-what-type* :framebuffer))
       (window:set-vsync t)
       (fps:set-fps 60)
       (progn
	 (setf world:*world-directory*
	       ;;"first/"
	       ;;#+nil
	       ;;"test/"
	       "other/"
	       )
	 #+nil
	 (progn
	   (setf world:*some-saves*
		 (cdr (assoc (machine-instance) 
			     '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
			       ("nootboke" . #P"/home/terminal256/Documents/saves/"))
			     :test 'equal))))
	 ;;#+nil
	 (progn
	   (setf world:*some-saves*
		 (sucle-temp:path "save/"))))
       (unwind-protect
	    (loop
	       (application:poll-app)
	       (per-frame))
	 (progn
	   (world:msave)))))))

;;;;

#+nil
(defun start ()
  (application:main
   (lambda ()
     (call-with-world-meshing-lparallel 
      (lambda ()
	(loop
	   (application:poll-app)
	   (per-frame)))))
   :width 720
   :height 480
   :title "conceptually simple block game"))
#+nil
(defun load-world-again (name)
  (setf world:*persist* nil)
  (setf world:*world-directory* name)
  (load-world t))

;;;;************************************************************************;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;************************************************************************;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defparameter *mouse-multiplier* 0.002617)
(defparameter *mouse-multiplier-aux* (/ (* 0.5 pi 0.9999) *mouse-multiplier*))
(defun moused (&optional (data (load-time-value (cons 0.0d0 0.0d0))))
  (multiple-value-bind (x y) (values window:*mouse-x* window:*mouse-y*)
    (multiple-value-prog1
	(values (- x (car data))
		(- y (cdr data)))
	(setf (car data) x
	      (cdr data) y))))
(progn
  (defparameter *tmouse-x* 0.0d0)
  (defparameter *tmouse-y* 0.0d0)
  (defparameter *prev-tmouse-x* 0.0d0)
  (defparameter *prev-tmouse-y* 0.0d0)
  (defparameter *lerp-mouse-x* 0.0d0)
  (defparameter *lerp-mouse-y* 0.0d0)
  (defparameter *lerp-mouse-x0* 0.0d0)
  (defparameter *lerp-mouse-y0* 0.0d0)
  (defun update-moused (clamp &optional (smoothing-factor 1.0))
    (multiple-value-bind (dx dy) (moused)
      (let ((x (+ *tmouse-x* dx))
	    (y (+ *tmouse-y* dy)))
	(when (> y clamp)
	  (setf y clamp))
	(let ((negative (- clamp)))
	  (when (< y negative)
	    (setf y negative)))
	(setf *prev-tmouse-x* *tmouse-x*)
	(setf *prev-tmouse-y* *tmouse-y*)
	(setf *tmouse-x* x)
	(setf *tmouse-y* y)
	(setf *lerp-mouse-x0* (alexandria:lerp smoothing-factor
					      *prev-tmouse-x*;;*lerp-mouse-x*
					      *tmouse-x*))
	(setf *lerp-mouse-y0* (alexandria:lerp smoothing-factor
					      *prev-tmouse-y*;;*lerp-mouse-y*
					      *tmouse-y*))
	(let ((smoothing-factor2 0.9))
	  (setf *lerp-mouse-x* (alexandria:lerp smoothing-factor2
						*lerp-mouse-x*
						*lerp-mouse-x0*))
	  (setf *lerp-mouse-y* (alexandria:lerp smoothing-factor2
						*lerp-mouse-y*
						*lerp-mouse-y0*)))))))

(defun unit-pitch-yaw (result pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (with-vec (x y z) (result symbol-macrolet)
      (setf x (* cos-pitch (sin yaw))
	    y (sin pitch)
	    z (* cos-pitch (cos yaw)))))
  result)

;;;;
(defparameter *fov* (* (floatify pi) (/ 85 180)))
(defparameter *camera*
  (camera-matrix:make-camera
   :frustum-far (* 256.0)
   :frustum-near (/ 1.0 8.0)))
(defparameter *fog-ratio* 0.75)
(defparameter *time-of-day* 1.0)

(defun update-camera (&optional (camera *camera*))
  (setf (camera-matrix:camera-aspect-ratio camera)
	(/ (floatify window:*width*)
	   (floatify window:*height*)))
  (setf (camera-matrix:camera-fov camera) *fov*)
  (setf (camera-matrix:camera-frustum-far camera) (* 1024.0 256.0))
  (camera-matrix:update-matrices camera))

(defparameter *last-session* nil)
(defparameter *paused* nil)
(defparameter *session* nil)
(defparameter *game-ticks-per-iteration* 0)
(defparameter *fraction-for-fps* 0.0)
(defun per-frame ()
  ;;[FIXME]where is the best place to flush the job-tasks?
  (sucle-mp:flush-job-tasks)

  ;;set the chunk center aroun the player
  (with-vec (x y z) ((player-position))
    (world:set-chunk-coordinate-center x y z))
  
  (application:on-session-change *session*
    (world:load-world t))
  (when (window:button :key :pressed :escape)
    (application:quit))
  (when (window:button :key :pressed #\e)
    (window:toggle-mouse-capture)
    ;;Flush changes to the mouse so
    ;;moving the mouse while not captured does not
    ;;affect the camera
    (moused))
  (setf *paused* (window:mouse-free?))

  
  ;;Polling
  ;;Physics
  ;;Rendering Chunks
  ;;Rendering Other stuff
  ;;Meshing
  ;;Waiting on vsync
  ;;Back to polling
  
  ;;Physics and Polling should be close together to prevent lag
  
  ;;physics
  (select-block-with-scroll-wheel)
  ;;Jump if space pressed
  (setf (entity-jump? *ent*)
	(window:button :key :down #\Space))
  ;;Set the sneaking state
  (setf (entity-sneak? *ent*)
	(cond
	  ((window:button :key :down :left-shift)
	   0)
	  ((window:button :key :down :left-control)
	   1)))
  ;;Toggle noclip with 'v'
  (when (window:button :key :pressed #\v)
    (toggle (entity-clip? *ent*)))
  ;;Toggle flying with 'f'
  (when (window:button :key :pressed #\f)
    (toggle (entity-fly? *ent*))
    (toggle (entity-gravity? *ent*)))
  ;;Set the direction with WASD
  (setf
   (entity-hips *ent*)
   (control:wasd-mover
    (window:button :key :down #\w)
    (window:button :key :down #\a)
    (window:button :key :down #\s)
    (window:button :key :down #\d)))
  ;;Calculate what bocks are selected etc..
  (unless *paused*
    (fist-stuff (player-position)))
  ;;Run the game ticks
  
  (cond
    (*paused*
     (fps:tick))
    (t
     (setf
   (values *fraction-for-fps* *game-ticks-per-iteration*)
   (fps:tick
     (incf *ticks*)
     (setf *time-of-day* 1.0)
     ;;run the physics
     (physentity *ent*)))))

  ;;update the internal mouse state
  ;;taking into consideration fractions
  (when (window:mouse-locked?)
    (update-moused
     *mouse-multiplier-aux*
     ;;[FIXME]is this formula correct?
     (/ (+ *fraction-for-fps*
	   *game-ticks-per-iteration*)
	(+ *game-ticks-per-iteration* 1))))
  ;;Calculate the camera position from
  ;;the past, current position of the player and the frame fraction
  (set-camera-position *fraction-for-fps*)
  ;;Set the pitch and yaw of the player based on the
  ;;mouse position
  (setf (necking-yaw (entity-neck *ent*))
	(floatify (- (* *lerp-mouse-x* *mouse-multiplier*)))
	(necking-pitch (entity-neck *ent*))
	(floatify (* *lerp-mouse-y* *mouse-multiplier*)))
  ;;Set the direction of the camera based on the
  ;;pitch and yaw of the player
  (unit-pitch-yaw (camera-matrix:camera-vec-forward *camera*)
		  (necking-pitch (entity-neck *ent*))
		  (necking-yaw (entity-neck *ent*)))
  
  (modify-camera-position-for-sneak)
  
  (when (window:button :key :pressed #\p)
    (update-world-vao))
  ;;load or unload chunks around the player who may have moved
  (world:load-world)
  ;;render chunks and such
  ;;handle chunk meshing
  (application:on-session-change *last-session*
    (reset-chunk-display-list)
    (update-world-vao))
  ;;update the camera
  (update-camera *camera*)
  (draw-to-default-area)
  ;;this also clears the depth and color buffer.
  (apply #'render-sky (the-sky-color))
  (use-chunk-shader
   :camera *camera*
   :sky-color *sky-color-foo*
   :time-of-day *time-of-day*
   :fog-ratio *fog-ratio*
   )
  (render-chunks)
  (use-occlusion-shader *camera*)
  (render-chunk-occlusion-queries)
  ;;selected block and crosshairs
  (use-solidshader *camera*)
  (render-fist *fist*)
  (render-crosshairs)

  (complete-render-tasks)
  (dispatch-mesher-to-dirty-chunks))

(defparameter *sky-color* '(0.68 0.8 1.0))
(defparameter *sky-color-foo* '(0.0 0.0 0.0))
(defun the-sky-color ()
  (map-into *sky-color-foo*
	    (lambda (x)
	    (alexandria:clamp (* x *time-of-day*) 0.0 1.0))
	  *sky-color*))

;;;

(defparameter *ent* (gentity))
(defparameter *fist* (gen-fister))
(defparameter *swinging* nil)
(defparameter *ticks* 0)

(defun select-block-with-scroll-wheel ()
  (setf *blockid*
	(let ((seq
	       #(3 13 12 24 1 2 18 17 20 5 89)))
	  (elt seq (mod (round window:*scroll-y*)
			(length seq))))))

(defun player-position ()
  (let* ((player-pointmass (entity-particle *ent*))
	 (curr (pointmass-position player-pointmass)))
    curr))
(defun player-position-old ()
  (let* ((player-pointmass (entity-particle *ent*))
	 (prev (pointmass-position-old player-pointmass)))
    prev))
(defparameter *reach* 64.0)
(defun set-camera-position (fraction)
  (let ((vec (camera-matrix:camera-vec-position *camera*)))
    (nsb-cga:%vec-lerp vec (player-position-old) (player-position) fraction)))
(defun modify-camera-position-for-sneak ()
  (let ((vec (camera-matrix:camera-vec-position *camera*)))
    (when (and (not (entity-fly? *ent*))
	       (eql 0 (entity-sneak? *ent*)))
      (nsb-cga:%vec- vec vec (load-time-value (nsb-cga:vec 0.0 0.125 0.0))))))

(defparameter *big-fist-reach* 32)

(defparameter *x* 0)
(defparameter *y* 0)
(defparameter *z* 0)
(defun fist-stuff (pos)
  (let ((look-vec (load-time-value (nsb-cga:vec 0.0 0.0 0.0))))
    (nsb-cga:%vec* look-vec (camera-matrix:camera-vec-forward *camera*) -1.0)
    (with-vec (px py pz) (pos)
      (with-vec (vx vy vz) (look-vec)	
	(when (window:mouse-locked?)
	  (when (window:button :key :pressed #\2) 
	    (toggle *dirtying2*))
	  (when (window:button :key :pressed #\1) 
	    (toggle *dirtying*))

	  (when (window:button :key :pressed #\3) 
	    (toggle *swinging*))
	  (when *swinging*
	    (let ((u *big-fist-reach*))
	      (aabbcc:aabb-collect-blocks
		  (px py pz (* u vx) (* u vy) (* u vz)
		      *big-fist-aabb*)
		  (x y z contact)
		(declare (ignorable contact))
		(let ((*x* x)
		      (*y* y)
		      (*z* z))
		  (funcall *big-fist-fun* x y z))))))
	(let ((fist *fist*))
	  (let ((left-p (window:button :mouse :pressed :left))
		(right-p (window:button :mouse :pressed :right))
		(middle-p (window:button :mouse :pressed :middle))
		(4-p (window:button :mouse :pressed :4))
		(5-p (window:button :mouse :pressed :5)))
	    #+nil
	    (when (or left-p right-p))
	    (standard-fist
	     fist
	     px py pz
	     (* *reach* vx) (* *reach* vy) (* *reach* vz))
	    (let ((fist? (fister-exists fist))
		  (selected-block (fister-selected-block fist))
		  (normal-block (fister-normal-block fist)))
	      (when fist?
		;;[FIXME]reactive? functional?
		(when left-p
		  (with-vec (a b c) (selected-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *left-fist-fnc* a b c))))
		(when right-p
		  (with-vec (a b c) (normal-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *right-fist-fnc* a b c))))
		(when middle-p
		  (with-vec (a b c) (selected-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *middle-fist-fnc* a b c))))
		(when 4-p
		  (with-vec (a b c) (selected-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *4-fist-fnc* a b c))))
		(when 5-p
		  (with-vec (a b c) (selected-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *5-fist-fnc* a b c))))))))))))

;;;detect more entities
;;;detect block types?
(defun not-occupied (x y z &optional (ent *ent*))
  (let ((aabb (entity-aabb ent))
	(pos (pointmass-position
	      (entity-particle ent))))
    (aabbcc:aabb-not-overlap
     (pos-to-block-aabb x y z)
     (floatify x)
     (floatify y)
     (floatify z)
     aabb
     (aref pos 0)
     (aref pos 1)
     (aref pos 2))))

(defun destroy-block-at (x y z)
  ;;(blocksound x y z)
  (world:plain-setblock x y z (block-data:lookup :air) 15))

(defparameter *blockid* 1)

(defun place-block-at (x y z &optional (blockval *blockid*))
  (when (not-occupied x y z)
    (world:plain-setblock
     x
     y
     z
     blockval
     (block-data:data blockval :light))
    ;;(blocksound x y z)
    ))


(defparameter *big-fist-fun* (constantly nil))
(defparameter *left-fist-fnc* 'destroy-block-at)
(defparameter *right-fist-fnc* 'place-block-at)
(defparameter *middle-fist-fnc* 'place-block-at)
(defparameter *4-fist-fnc* 'tree)
(defparameter *5-fist-fnc* '5fun)
#+nil
(progn
  (setf *big-fist-fun* 'correct-earth)
  (setf *middle-fist-fnc* 'player-feet-at)
  (setf *middle-fist-fnc* 'line-to-player-feet))
