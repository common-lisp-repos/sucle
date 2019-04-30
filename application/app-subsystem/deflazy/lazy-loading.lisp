(defpackage #:deflazy
  (:use #:cl #:utility)
  (:export
   #:getfnc
   #:deflazy
   #:refresh
   #:flush-refreshes))

(in-package :deflazy)

(defvar *stuff* (make-hash-table :test 'eq))
(eval-always
  (defvar *function-stuff* (make-hash-table :test 'eq)))

(eval-always
  (defun separate-bindings (deps)
    (values (mapcar (lambda (x)
		      (etypecase x
			(symbol x)
			(list (first x))))
		    deps)
	    (mapcar (lambda (x)
		      (etypecase x
			(symbol x)
			(list (second x))))
		    deps))))

(defun get-node (name)
  (multiple-value-bind (value existsp) (gethash name *stuff*)
    (if existsp
	value
	(progn
	  (multiple-value-bind (fun existsp) (gethash name *function-stuff*)
	    (if existsp
		(let ((new-value (funcall (car fun))))
		  (setf (gethash name *stuff*)
			new-value)
		  new-value)
		(error "no deflazy node defined named ~s" name)))))))

;;deflazy can take multiple forms:
;;(deflazy name ((nick name) other))
;;(deflazy (name :unchanged-if eql) ())
(defmacro deflazy (name (&rest deps) &body gen-forms)
  (let ((unchanged-if nil)) ;;FIXME::backwards compatibility deflazy hack
    (etypecase name
      (symbol)
      (list (destructuring-bind (unwrapped-name
				 &key ((:unchanged-if nick) nil))
		name
	      (setf name unwrapped-name)
	      (setf unchanged-if nick))))
    (multiple-value-bind (lambda-args names)
	(separate-bindings deps)
       (let ((let-args (mapcar (lambda (lambda-arg name)
				`(,lambda-arg (getfnc ',name)))
			      lambda-args
			      names))
	    (dummy-redefinition-node (symbolicate2 `("%*%" ,name "-deflazy-redefine%*%")))
	    (scrambled-name (symbolicate2 `("%*%deflazy-function-" ,name "-deflazy-function%*%")))
	    (scrambled-name2 (symbolicate2
			      `("%*%deflazy-cell-function-" ,name "-deflazy-cell-function%*%"))))
	`(progn
	   (setf (gethash ',name *function-stuff*)
		 (cons ',scrambled-name ',dummy-redefinition-node))
	   (defparameter ,dummy-redefinition-node
	     (if (boundp ',dummy-redefinition-node)
		 (let ((old-value (symbol-value ',dummy-redefinition-node)))
		   (%%refresh old-value)
		   old-value)
		 (make-instance 'node :value (cells:c? "nothing"))))
	   (defun ,scrambled-name2 (self)
	     (let ,let-args
	       (declare (ignorable ,@lambda-args))
	       (node-update-p self)
	       (node-update-p ,dummy-redefinition-node)
	       (locally
		   ,@gen-forms)))
	   (defun ,scrambled-name ()
	     (make-instance
	      ',(ecase unchanged-if
		  ((nil) 'node)
		  (eql 'node-eql)
		  (= 'node-=))
	      :value 
	      (cells:c?_
		(,scrambled-name2 cells:self)))))))))

(defparameter *refresh* (make-hash-table :test 'eq))
(defparameter *refresh-lock* (bordeaux-threads:make-recursive-lock "refresh"))
(defun refresh (name &optional (main-thread nil))
  (if main-thread
      (%refresh name)
      (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
	(setf (gethash name *refresh*) t))))
(defun flush-refreshes ()
  (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
    (let ((length (hash-table-count *refresh*)))
      (unless (zerop length)
	(dohash (name value) *refresh*
		(declare (ignore value))
		(%refresh name))
	(clrhash *refresh*)))))

(defgeneric cleanup-node-value (object))
(defmethod cleanup-node-value ((object t))
  (declare (ignorable object)))

#+nil ;;attempt to make it so when code is reevaluated, all cells defined within get updated
(defmacro runtime-once-only (&body body)
  (let ((cell (gensym)))
    `(let ((,cell
	    (load-time-value (cons nil nil))))
       (unless (car ,cell)
	 (setf (car ,cell) t)
	 (locally ,@body)))))

;;TODO? have an automatic system for different comparison operators?
(cells:defmodel node ()
  ((update-p :cell t
	     :initform (cells:c-in 0)
	     :accessor node-update-p)
   (value :initarg :value
	  :unchanged-if (constantly nil)
	  :accessor node-value
	  :cell t)))
(cells:defmodel node-eql ()
  ((update-p :cell t
	     :initform (cells:c-in 0)
	     :accessor node-update-p)
   (value :initarg :value
	  ;;:unchanged-if #'eql
	  :accessor node-value
	  :cell t)))
(cells:defmodel node-= ()
  ((update-p :cell t
	     :initform (cells:c-in 0)
	     :accessor node-update-p)
   (value :initarg :value
	  :unchanged-if #'=
	  :accessor node-value
	  :cell t)))

(defun getfnc (name)
  (%getfnc (get-node name)))
(defun %getfnc (node)
  (node-update-p node)
  (node-value node))

(defun (setf %getfnc) (new node)
  (setf (node-value node) new))

(cells:defobserver value (self new-value old-value old-value-boundp)
  (when old-value-boundp
    ;;(print old-value)
    (cleanup-node-value old-value)))

(defun %refresh (name)
  (%%refresh (get-node name)))
(defun %%refresh (node)
  (incf (node-update-p node)))

;;test cases for deflazy
(deflazy (bar :unchanged-if eql) () 122242344)
(deflazy foobar (bar)
  (+ 9 (print bar)))

(deflazy noop () "wat")

;;FIXME::does not actually work, or does it?
;;Does not clean up cells, TODO?
(defun destroy-all ()
  (cells::cells-reset)
  (clrhash *stuff*)
  (dohash (name value) *function-stuff*
	  (declare (ignorable name))
	  (makunbound (cdr value))))
