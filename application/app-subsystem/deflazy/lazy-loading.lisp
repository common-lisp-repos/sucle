(defpackage #:deflazy
  (:use #:cl #:utility)
  (:export
   #:getfnc
   #:deflazy
   #:refresh
   #:flush-refreshes))

(in-package :deflazy)

;;;;;TODO: clean this area up with dependency graph
;;#+nil
(defvar *stuff* (make-hash-table :test 'eq))
(defvar *function-stuff* (make-hash-table :test 'eq))
#+nil
(defmacro deflazy (name (&rest deps) &rest gen-forms)
  `(eval-when (:load-toplevel :execute)
     (let ((dependency-graph::*namespace* *stuff*))
       (refresh-new-node ',name)
       ,(multiple-value-bind
	 (fun node-deps) (dependency-graph::%defnode deps gen-forms)
	  `(dependency-graph::redefine-node ,fun ',node-deps ',name)))))
#+nil
(defun %defnode (deps body)
  (multiple-value-bind (lambda-args node-deps) (separate-bindings deps)
    (values `(lambda ,lambda-args ,@body)
	    node-deps)))

(eval-always
  (defun separate-bindings (deps)
    (let ((lambda-args ())
	  (node-deps ()))
      (dolist (item deps)
	(if (symbolp item)
	    (progn (push item lambda-args)
		   (push item node-deps))
	    (destructuring-bind (var dep) item
	      (push var lambda-args)
	      (push dep node-deps))))
      (values lambda-args
	      node-deps))))
#+nil
(progn
  (defun get-node (name)
    (symbol-value name))
  (defun name-defined-p (name)
    (boundp name))
  (defmacro define-named-node (name &body body)
    `(defparameter ,name ,@body)))

(progn
  (defun get-node (name)
    (gethash name *stuff*))
  (defun name-defined-p (name)
    (multiple-value-bind (value existsp)
	(gethash name *stuff*)
      (declare (ignore value))
      existsp))
  (defmacro define-named-node (name form)
    (with-gensyms (fun)
      `(let ((,fun (lambda () ,form)))
	 (setf (gethash ',name *function-stuff*)
	       ,fun)
	 (setf (gethash ',name *stuff*)
	       (funcall ,fun))))))

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
			      names)))
	`(progn
	   (when (name-defined-p ',name)
	     (%refresh ',name))
	   (define-named-node ,name
	     (make-instance
	      ',(ecase unchanged-if
		  ((nil) 'node)
		  (eql 'node-eql))
	      :value 
	      (cells:c?_
		(let ,let-args
		  (declare (ignorable ,@lambda-args))
		  (node-update-p cells:self)
		  (locally
		      ,@gen-forms))))))))))

#+nil
(defvar foo
  (symbol-macrolet 
      (define-node )))

;;;;queue node to be unloaded if it already has stuff in it
#+nil
(defun refresh-new-node (name)
  (let ((node (dependency-graph::ensure-node name *stuff*)))
    (unless (= 0 (dependency-graph::timestamp node))
      (refresh name))))

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

#+nil
(defun %refresh (name)
  (let ((node (dependency-graph::get-node name *stuff*)))
    (when node
      (dependency-graph::touch-node node)
      (clean-and-invalidate-node node)
      (dependency-graph::map-dependents2
       name
       #'clean-and-invalidate-node
       #'dependency-graph::dirty-p
       *stuff*))))

#+nil
(defun getfnc (name)
  (dependency-graph::get-value name *stuff*))

(defgeneric cleanup-node-value (object))
(defmethod cleanup-node-value ((object t))
  (declare (ignorable object)))
(defun cleanup-node (node)
  (let ((value (dependency-graph::value node)))
    (cleanup-node-value value)))

(defun clean-and-invalidate-node (node)
  (when (dependency-graph::state node)
    (cleanup-node node))
  (dependency-graph::%invalidate-node node))



(defmacro runtime-once-only (&body body)
  (let ((cell (gensym)))
    `(let ((,cell
	    (load-time-value (cons nil nil))))
       (unless (car ,cell)
	 (setf (car ,cell) t)
	 (locally ,@body)))))


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

(defun getfnc (name)
  (%getfnc (get-node name)))
(defun %getfnc (node)
  (node-update-p node)
  (node-value node))

(cells:defobserver value (self new-value old-value old-value-boundp)
  (when old-value-boundp
    (cleanup-node-value old-value)))

(defun %refresh (name)
  (%%refresh (get-node name)))
(defun %%refresh (node)
  (incf (node-update-p node)))

(deflazy (bar :unchanged-if eql) () 12423)
(deflazy foobar (bar)
  (+ 9 (print bar)))

;;FIXME::does not actually work, or does it?
;;Does not clean up cells, TODO?
(defun destroy-all ()
  (cells::cells-reset)
  (clrhash *stuff*)
  (dohash (name fun) *function-stuff*
	  (setf (gethash name *stuff*)
		(funcall fun))))
