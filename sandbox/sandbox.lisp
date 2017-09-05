(in-package #:sandbox)

(defun initialization1 ()
  (clrhash *g/call-list*)
  (clrhash *g/chunk-call-list*)
  
  (glinnit) ;opengl
  (physinnit) ;physics
  )
(defun thunkit (control-state)
  (physics control-state))
(defparameter *save* (case 3
		       (0 #P"terrarium2/")
		       (1 #P"first/")
		       (2 #P"second/")
		       (3 #P"third/")
		       (4 #P"fourth/")
		       (5 #P"world/")
		       (6 #P"terrarium/")
		       (7 #P"holymoly/")
		       (8 #P"funkycoolclimb/")
		       (9 #P"ahole/")
		       (10 #P"maze-royale/")))

(defparameter *saves-dir* (merge-pathnames #P"saves/" ourdir))

(defun save (filename &rest things)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (thing things)
	(print thing stream)))))

(defun save2 (thingfilename &rest things)
  (apply #'save (merge-pathnames (format nil "~s" thingfilename) *save*) things))

(defun savechunk (position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (save2 position-list
	   (gethash position world:chunkhash)
	   (gethash position world:lighthash)
	   (gethash position world:skylighthash))))

(defun save-world ()
  (maphash (lambda (k v)
	     (declare (ignorable v))
	     (savechunk k))
	   world:chunkhash))

(defun looad-world ()
  (let ((files (uiop:directory-files (merge-pathnames *save* *saves-dir*))))
    (dolist (file files)
      (loadchunk (apply #'world:chunkhashfunc (read-from-string (pathname-name file)))))))

(defun myload2 (thingfilename)
  (myload (merge-pathnames (format nil "~s" thingfilename) *save*)))

(defun myload (filename)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (let ((things nil))
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
	(tagbody rep
	   (let ((thing (read stream nil nil)))
	     (when thing
	       (push thing things)
	       (go rep)))))
      (nreverse things))))

(defun loadchunk (position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (let ((data (myload2 position-list)))
      (when data 
	(destructuring-bind (blocks light sky) data
	  (setf (gethash position world:chunkhash)
		(coerce blocks '(simple-array (unsigned-byte 8) (*))))
	  (setf (gethash position world:lighthash)
		(coerce light '(simple-array (unsigned-byte 4) (*))))
	  (setf (gethash position world:skylighthash)
		(coerce sky '(simple-array (unsigned-byte 4) (*)))))
	(return-from loadchunk t)))))  
  
(defun color-grasses ()
  (modify-greens 64 192)
  (modify-greens 80 192)
  (modify-greens 0 240))

(defun ubyte-mult (a b)
  (truncate (* a b) 256))

(defun multiply-into (vecinto other)
  (macrolet ((aux (a b num)
	       `(let ((at (aref ,a ,num))
		      (bt (aref ,b ,num)))
		  (setf (aref ,a ,num) (ubyte-mult at bt)))))
    (aux vecinto other 0)
    (aux vecinto other 1)
    (aux vecinto other 2)
    (aux vecinto other 3)))

(progno #(113 174 70 255)  #(198 304 122 255))
;;;grass is 0 240
;;;leaves is [64 80] 192
(defun modify-greens (xpos ypos
		      &optional
			(image (aplayground::get-stuff
				:grass-png aplayground::*stuff*
				aplayground::*backup*))
			(color
			 (case 1
			   (0 #(1742848/8775 2673664/8775 1079296/8775 255))
			   (1 (imagewise:getapixel 255 0 image))
			   (2 (imagewise:getapixel 0 0 image))
			   (3 (imagewise:getapixel 255 255 image)))
			 ) (terrain (aplayground::get-stuff
				     :terrain-png aplayground::*stuff*
				     aplayground::*backup*)))
  (dobox ((x xpos (+ 16 xpos)) (y ypos (+ 16 ypos)))
	 (multiply-into (imagewise:getapixel y x terrain) color)))

(defparameter *box* #(0 128 0 128 -128 0))
(with-unsafe-speed
  (defun map-box (func box)
    (declare (type (function (fixnum fixnum fixnum)) func))
    (etouq
     (with-vec-params (quote (x0 x1 y0 y1 z0 z1)) (quote (box))
		      (quote (dobox ((x x0 x1)
				     (y y0 y1)
				     (z z0 z1))
				    (funcall func x y z)))))))
(defun invert (&optional (box *box*))
  (map-box (lambda (x y z)
	     (let ((blockid (world:getblock x y z)))
	       (if (= blockid 0)
		   (plain-setblock x y z 1 ;(aref #(56 21 14 73 15) (random 5))
				   0
				   )
		   (plain-setblock x y z 0 0)
		   )))
	   box))

(defun grassify (&optional (box *box*))
  (map-box (lambda (x y z)
	     (let ((blockid (world:getblock x y z)))
	       (when (= blockid 3)
		 (let ((idabove (world:getblock x (1+ y) z)))
		   (when (zerop idabove)
		     (plain-setblock x y z 2 0))))))
	   box))

(defun dirts (&optional (box *box*))
  (map-box (lambda (x y z)
	     (let ((blockid (world:getblock x y z)))
	       (when (= blockid 1)
		 (when (or (zerop (world:getblock x (+ 2 y) z))
			   (zerop (world:getblock x (+ 3 y) z)))
		   (plain-setblock x y z 3 0)))))
	   box))


(defun simple-relight (&optional (box *box*))
  (map-box (lambda (x y z)
	     (let ((blockid (world:getblock x y z)))
					;(unless (zerop blockid))
	       (let ((light (aref mc-blocks::lightvalue blockid)))
		 (if (zerop light)
		     (plain-setblock x y z blockid light 0)
		     (setblock-with-update x y z blockid light)))))
	   box)
  (map-box (function sky-light-node) #(0 128 128 129 -128 0))
  (map-box (function light-node) #(0 128 128 129 -128 0)))

(defun neighbors (x y z)
  (let ((tot 0))
    (macrolet ((aux (i j k)
		 `(unless (zerop (world:getblock (+ x ,i) (+ y ,j) (+ z ,k)))
		   (incf tot))))
      (aux 1 0 0)
      (aux -1 0 0)
      (aux 0 1 0)
      (aux 0 -1 0)
      (aux 0 0 1)
      (aux 0 0 -1))
    tot))

(defun bonder (&optional (box *box*))
  (map-box (lambda (x y z)
	     (let ((blockid (world:getblock x y z)))
	       (unless (zerop blockid)
		 (let ((naybs (neighbors x y z)))
		   (when (> 3 naybs)		     
		     (plain-setblock x y z 0 0 0))))))
	   box))

(defun invert-light (&optional (box *box*))
  (map-box (lambda (x y z)
	     (when (zerop (world:getblock x y z))
	       (let ((blockid (world:getlight x y z))
		     (blockid2 (world:skygetlight x y z)))
	;	 (Setf (world:getlight x y z) (- 15 blockid))
		 (Setf (world:skygetlight x y z) (- 15 blockid2)))))
	   box))


(defun edge-bench (&optional (box *box*))
  (map-box (lambda (x y z)
	     (let ((blockid (world:getblock x y z)))
	       (unless (zerop blockid)
		 (when (= 4 (neighbors x y z))
		   (plain-setblock x y z 58 0 0)))))
	   box))

(defun corner-obsidian (&optional (box *box*))
  (map-box (lambda (x y z)
	     (let ((blockid (world:getblock x y z)))
	       (unless (zerop blockid)
		 (when (= 3 (neighbors x y z))
		   (plain-setblock x y z 49 0 0)))))
	   box))


(defun clearblock? (id &optional (box *box*))
  (declare (type fixnum id))
  (map-box (lambda (x y z)
	     (let ((blockid (world:getblock x y z)))
	       (when (= blockid id)
		 (plain-setblock x y z 0 0))))
	   box))


(defun testicle (&optional (box *box*))
  (dotimes (x 1)
    (sandbox::edge-bench box)
    (sandbox::corner-obsidian box)
    (sandbox::clearblock? 49 box)
    (sandbox::clearblock? 58 box)
    (dotimes (x 3) (sandbox::bonder box))))

(defun goto (x y z)
  (setf *xpos* (float x)
	*ypos* (float y)
	*zpos* (float z)))
