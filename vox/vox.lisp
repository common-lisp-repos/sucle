(in-package :vox)

;;chunkhash stores all of the chunks in a hasmap.
;;chunks accessed by '(x y z) in chunk coords
(defparameter chunkhash (make-hash-table :test (function eql)))
(defparameter lighthash (make-hash-table :test (function eql)))
(defparameter skylighthash (make-hash-table :test (function eql)))
;;dirty chunks is a list of modified chunks 
(defparameter dirtychunks nil)

(defun clearworld ()
  (send-to-free-mem chunkhash)
  (clrhash chunkhash)
  (send-to-free-mem lighthash)
  (clrhash lighthash)
  (send-to-free-mem skylighthash)
  (clrhash skylighthash)
  (setf dirtychunks nil))

(defun send-to-free-mem (hash)
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (free-chunk v))
   hash))

(defparameter freechunkmempool nil)

(defun free-chunk (lechunk)
  (push lechunk freechunkmempool))

(defun empty-chunk ()
  "makes an empty chunk"
  (make-array (* 16 16 16) :element-type '(unsigned-byte 8)))

(defun getachunk ()
  (let ((somechunk (pop freechunkmempool)))
    (if somechunk
	somechunk
	(empty-chunk))))

(defun clearchunk (achunk)
  (nsubstitute-if-not 0 #'zerop achunk)
  achunk)

(defun plus2^19 (n)
  (declare (type fixnum n))
  (+ n (ash 1 19)))

(defun minus2^19 (n)
  (declare (type fixnum n))
  (- n (ash 1 19)))

(defun chunkhashfunc (x y z)
  (declare (type fixnum x y z))
  (+ (plus2^19 z) (ash (+ (plus2^19 y) (ash (plus2^19 x) 20)) 20)))

(defun unchunkhashfunc (ah)
  (declare (type fixnum ah))
  (let* ((z (logand ah (1- (ash 1 20))))
	 (xy (ash ah -20))
	 (y (logand xy (1- (ash 1 20))))
	 (x (ash xy -20)))
    (list (minus2^19 x) (minus2^19 y) (minus2^19 z))))

(defun getchunkat (hash x y z)
  (gethash (chunkhashfunc x y z) hash))

(defun setchunkat (x y z newchunk)
  (setf
   (gethash (chunkhashfunc x y z) chunkhash)
   newchunk)
  newchunk)

(defun chunkexistsat (x y z)
  (getchunkat chunkhash x y z))

(defun get-chunk-block (chunk i j k)
  (aref chunk (+  i (* 16 (+ (* 16 j) k)))))

(defun set-chunk-block (chunk i j k new)
  (setf
   (aref chunk (+  i (* 16 (+ (* 16 j) k))))
   new))

(defun getblock (i j k)
  (multiple-value-bind (x xd) (floor i 16)
    (Multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let ((chunk (getchunkat chunkhash x y z)))
	  (if chunk
	      (get-chunk-block chunk xd yd zd)
	      0))))))

(defun getlight (i j k)
  (multiple-value-bind (x xd) (floor i 16)
    (Multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let ((chunk (getchunkat lighthash x y z)))
	  (if chunk
	      (get-chunk-block chunk xd yd zd)
	      15))))))

(defun skygetlight (i j k)
  (multiple-value-bind (x xd) (floor i 16)
    (Multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let ((chunk nil))
	  (setf chunk (getchunkat skylighthash x y z))
	  (if chunk
	      (round (get-chunk-block chunk xd yd zd))
	      15))))))

(defun empty-chunk-at (x y z)
  (let ((oldchunk (getchunkat chunkhash x y z)))
    (if oldchunk
	(clearchunk oldchunk)
	(setchunkat x y z (clearchunk (getachunk))))))

(defun destroy-chunk-at (x y z)
  (let ((oldchunk (getchunkat chunkhash x y z)))
    (if oldchunk
	(progn
	  (remhash (chunkhashfunc x y z) chunkhash)
	  (free-chunk oldchunk)))))

(defun setblock (i j k blockid)
  (multiple-value-bind (x xd) (floor i 16)
    (multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let* ((chunk (getchunkat chunkhash x y z)))
	  (if (not chunk)
	      (setf chunk (empty-chunk-at x y z)))
	  (let ((old (get-chunk-block chunk xd yd zd)))
	    (if (/= old blockid)
		(set-chunk-block chunk xd yd zd (the (unsigned-byte 8) blockid))
		nil)))))))

(defun block-dirtify (i j k)
  (pushnew (list (ash i -4) (ash j -4) (ash k -4)) dirtychunks :test 'equal))

(defun dirtify (x y z)
  (pushnew (list x y z) dirtychunks :test 'equal))

(defun blockcoordtochunk (x y z)
  (declare (type fixnum x y z))
  (chunkhashfunc (ash x -4) (ash y -4) (ash z -4)))

(defun setblock-with-update (i j k blockid)
  (if (setblock i j k blockid)
      (block-dirtify i j k)))