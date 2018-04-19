(defpackage #:coge
  (:use #:cl)

;;;code generation
  (:export
   #:gen-spec
   #:add-spec #:spec-assoc #:is-param 
   #:rp #:legalp #:get-actual-args #:defspec))

(in-package :coge)

;;1. start with a bunch of constant specifications
;;2. generate multiple functions according to the specifications
;;3. specifications are in a hash table which hash names bound to constants
;;4. function generating functions are toplevel so they can be tested
(defun gen-spec ()
  (make-hash-table :test 'eq))

(defun add-spec (spec body)
  (dolist (pair body)
    (setf (gethash (car pair) spec) (cdr pair)))
  spec)

(defun spec-assoc (hash)
  (let ((acc nil))
    (maphash
     (lambda (k v)
       (push (cons k v) acc))
     hash)
    acc))

;;replace all symbols which start with p!x.. with
;;the value from the hash table with keyx..
(defun is-param (symbol)
  (let ((string (symbol-name symbol)))
    (if (> (length string) 2)
	(if (string= "P!" (subseq string 0 2))
	    (intern (subseq string 2) (symbol-package symbol))))))

;;rp = replace params
(defun rp (spec code)
  (labels ((rec (piece)
	     (if (atom piece)
		 (if (symbolp piece)
		     (let ((val (is-param piece)))
		       (if val
			   (gethash val spec)
			   piece))
		     piece)
		 (cons (rec (car piece))
		       (rec (cdr piece))))))
    (rec code)))

;;below is for dealing with function headers
;;is the symbol actually a parameter or a keyword

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun legal-p (symbol)
    (if (member symbol '(&optional &rest &body &key &optional &aux t))
	nil
	symbol))
  (defun get-actual-args (lambda-list)
    (let ((actual-args nil))
      (dolist (arg lambda-list actual-args)
	(if (atom arg)
	    (if (legal-p arg)
		(push arg actual-args))
	    (push (car arg) actual-args))))))

(defpackage #:vox
  (:use #:cl))

;;;;create a data structure to store chunks which in turn store voxels
;;;;or some sort of three dimensional data
;;;;Currently implemented by a hash table filled with arrays.
(in-package :vox)

;;;;different types of specification
;;;;1: layout of 3 numbers in the fixnum
;;;;2: size of trucator for each of the 3 numbers -- how large the individual
;;;;chunks are in that dimension
;;;;3. signed/unsigned/offset of 3 numbers
;;;;4. names of the different generated functions so they can refer to each other
;;;;by name
;;;;5. type of data to be stored, names of the functions, the hash table,
;;;;default "vacuum" state, space generators

(defmacro defspec (name lambda-list)
  (let ((dynamic-var-names (mapcar (lambda (sym)
				     (utility:symbolicate2
				      (list "*" sym "*")))
				   lambda-list)))
    `(progn
       ,@(mapcar (lambda (name) `(defparameter ,name nil))
		 dynamic-var-names)
       (defun ,name ,lambda-list
	 (progn
	   ,@(mapcar (lambda (var dyn-var)
		       `(setf ,dyn-var ,var))
		     lambda-list
		     dynamic-var-names))))))

(defspec layout
    (num0-size num0-start num1-size num1-start num2-size num2-start))
(defspec truncation
    (chopx chopy chopz))
(defspec names
    (unhashfunc chunkhashfunc chop anti-chop rem-flow %%ref add))
(defspec offset
    (offset0 offset1 offset2))
(defspec field
    (data-type chunk-container vacuum-state space-provider))
(defspec access
    (getter setter))

;;inline the headless lambda, give it a name, give it speed and recklessness

;;instead of using a struct or a vector to represent a point,
;;a single fixnum can be used.
;;a fixnum is 62 bits on a 64 bit machine [lets not worry about the 32 bit]
;;a minecraft world is 256 blocks high and 60 mil block wide and long
;;1 bit to pad, 9 bits z, 1 bit pad, 25 bits y, 1 bit pad, 25 bit x
;;== 512 high, 33554432 long, 33554432 wide. not exactly minecraft specs,
;;but close enough
;;chunks are aligned to 16 16 16 which means that by chopping off
;;the last 4 bits of each constituent number in the fixnum
;;we get the code for the chunk! and this is done in one mask!
;;the padding is added so we can use another number as a type of
;;vector to add to the fixnum position. If after addition the number
;;overflows, we chop off the most significant bit with a single mask
;;because the y axis is the most significant, the codes can be sorted
;;in order of height!
;;bit 1 - 25 is x = (byte 25 0)
;;bit 26 is overflow padding for x
;;bit 27 - 51 is y (byte 25 26)
;;bit 52 is overflow padding for z
;;bit 53 - 61 is z (byte 9 52)
;;bit 62 is overflow padding for y
;;20 bits go to x, 20 bits go to y, 20 bits go to z, 2 bits are left over = 62 bits
;;(defparameter spec `((20 ,(ash 1 19)) (20 ,(ash 1 19)) (20 ,(ash 1 19))))
;;(eval (CREATE-PACKED-NUM 'chunkhashfunc 'unhashfunc spec))

(defmacro fastnum (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args
       (declare (optimize (speed 3) (safety 0)))
       (declare (type fixnum ,@args))
       ,@body)))

(defun define-fixnum-ops ()
  `(progn
     (fastnum
	 ,*unhashfunc*
	 (fixnum)
       (values ,(signed-unsigned
		 (if (zerop *num0-start*)
		     `(logand fixnum ,(1- (ash 1 *num0-size*)))  
		     `(ldb (byte ,*num0-size* ,*num0-start*) fixnum))
		 *uoffset0*)
	       ,(signed-unsigned
		 `(LDB (byte ,*num1-size* ,*num1-start*) fixnum)
		 *uoffset1*)
	       ,(signed-unsigned
		 `(ash fixnum ,(- *num2-start*))
		 *uoffset2*)))
     (fastnum
	 ,*chunkhashfunc*
	 (x y z)
       (THE FIXNUM
	    (logior (the fixnum (ash (mod z ,*uoffset2*) ,*num2-start*))
		    (the fixnum (ash (mod y ,*uoffset1*) ,*num1-start*))
		    (the fixnum (ash (mod x ,*uoffset0*) ,*num0-start*)))))
     (fastnum
	 ,*chop*
	 (pos)
       (the fixnum (logand ,*truncate-mask* pos)))
     (fastnum
	 ,*anti-chop*
	 (pos)
       (the fixnum (logand ,*anti-truncate-mask* pos)))
     (fastnum
	 ,*rem-flow*
	 (pos)
       (the fixnum (logand ,*overflow-mask* pos)))
     (fastnum
	 ,*%%ref*
	 (code)
       ,(let ((size-0 *chopx*)
	      (size-1 *chopy*)
	      (size-2 *chopz*)
	      (start-1 *num1-start*)
	      (start-2 *num2-start*))
	     (let ((first-shift (- start-2 size-0 size-1))
		   (second-shift (- start-2 start-1 size-1))
		   (end-size (+ size-0 size-1 size-2)))
	       `(let ((c (,*anti-chop* code)))	  
		  (the (unsigned-byte ,end-size)
		       ,(if (and (= size-0 size-1 size-2)
				 (= start-1 26)
				 (= start-2 52))
			    `(ash (logior (the fixnum (ash c ,first-shift))
					  (the fixnum (ash c ,second-shift))
					  c)
				  ,(- first-shift))
			    `(ash (logior (the fixnum
					       (ash (logand c ,(1- (ash 1 size-0)))
						    ,first-shift))
					  (the fixnum
					       (ash
						(logand c ,(ash (1- (ash 1 size-1))
								start-1))
						,second-shift))
					  c)
				  ,(- first-shift))))))))
     (fastnum
	 ,*add*
	 (a b)
       (,*rem-flow* (+ a b)))))

;;;;which one is faster???

#+nil
(defun signed-unsigned (x n)
  `(- (mod (the fixnum (+ ,x ,(/ n 2))) ,n) ,(/ n 2)))

(defun signed-unsigned (x n)
  (utility:once-only (x)
    `(if (> ,(/ n 2) ,x) 
	 ,x
	 (- ,x ,n))))

(defparameter *uoffset0* nil)
(defparameter *uoffset1* nil)
(defparameter *uoffset2* nil)
(defparameter *overflow-mask* nil)
(defparameter *truncate-mask* nil)
(defparameter *anti-truncate-mask* nil)
(defun derived-parts ()
  (setf *uoffset0* (ash 1 *num0-size*)
	*uoffset1* (ash 1 *num1-size*)
	*uoffset2* (ash 1 *num2-size*)
	*overflow-mask* (lognot (logior (ash 1 (+ *num0-start* *num0-size*))
					(ash 1 (+ *num1-start* *num1-size*))
					(ash 1 (+ *num2-start* *num2-size*))))
	*truncate-mask* (lognot (+ (ash (1- (ash 1 *chopy*)) *num0-start*)
				   (ash (1- (ash 1 *chopx*)) *num1-start*)
				   (ash (1- (ash 1 *chopz*)) *num2-start*)))
	*anti-truncate-mask* (+ (ash (1- (ash 1 *chopy*)) *num0-start*)
				(ash (1- (ash 1 *chopx*)) *num1-start*)
				(ash (1- (ash 1 *chopz*)) *num2-start*))))

;;the type of hash table that lets fixnums be equated [default is eql]
;;combine the number of bits per individual item, the get/set names,
;;the hash which hashes the fixnums, the default value, and the method
;;to create new chunks

(defun prep-hash ()
  `(progn
     (utility:with-unsafe-speed
       (defun ,*getter* (block-code)
	 (declare (type fixnum block-code))
	 (let ((chunk-code (,*chop* block-code)))
	   (let ((chunk (gethash chunk-code ,*chunk-container*)))
	     (declare (type (or ,*data-type* null) chunk))
	     (if chunk
		 (values (aref chunk (,*%%ref* block-code)) t)
		 (values ,*vacuum-state* nil)))))
       (defun ,*setter* (block-code blockid)
	 (declare (type fixnum block-code blockid))
	 (let ((chunk-code (,*chop* block-code)))
	   (let ((chunk (or (gethash chunk-code
				     ,*chunk-container*)
			    (setf
			     (gethash chunk-code
				      ,*chunk-container*)
			     ,*space-provider*))))
	     (declare (type ,*data-type* chunk))
	     (setf (aref chunk (,*%%ref* block-code)) blockid))))
       (defun (setf ,*getter*) (new location)
	 (,*setter* location new)))))
