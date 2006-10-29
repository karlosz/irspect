(in-package :irspect)

(define-application-frame irspect ()
    ((components :initform nil :accessor components))
  (:pointer-documentation t)
  (:menu-bar menubar-command-table)
  (:panes
    (interactor :interactor :scroll-bars t)
    (source :drei :syntax :lisp))
  (:layouts
    (default
      (vertically ()
        source
	(make-pane 'clim-extensions:box-adjuster-gadget)
	interactor)))
  (:top-level (toplevel)))

(define-symbol-macro <frame> *application-frame*)

(defmacro knopf (name)
  `(find-pane-named <frame> ',name))

(defun slurp (pathname)
  (with-output-to-string (out)
    (with-open-file (in pathname)
      (loop
	  for l = (read-line in nil)
	  while l
	  do (write-line l out)))))

(defun toplevel (frame)
  (com-load-quicksave)
  (default-frame-top-level frame))

(make-command-table
 'menubar-command-table
 :errorp nil
 :menu '(("File" :menu file-command-table)
	 ("View" :menu view-command-table)))

(make-command-table
 'file-command-table
 :errorp nil
 :menu '(("Quicksave" :command com-quicksave)
	 ("Load Quicksave" :command com-load-quicksave)
	 ("Compile" :command com-compile)
	 ("Load Heap" :command com-load-heap)
	 ("Quit" :command com-quit)))

(make-command-table
 'view-command-table
 :errorp nil
 :menu '(("Show Components" :command com-show-components)
	 ("Clear" :command com-clear)))

(define-irspect-command (com-quit :name t) ()
  (frame-exit <frame>))

(defun quicksave-pathname ()
  (merge-pathnames ".irspect.lisp" (user-homedir-pathname)))

(define-irspect-command (com-quicksave :name t) ()
  (let ((p (quicksave-pathname)))
    (with-open-file (s p :direction :output :if-exists :rename-and-delete)
      (write-string (gadget-value (knopf source)) s))
    (format t "Source saved to ~A." p)))

(define-irspect-command (com-load-quicksave :name t) ()
  (let ((p (merge-pathnames ".irspect.lisp" (user-homedir-pathname))))
    (when (probe-file p)
      (setf (gadget-value (knopf source)) (slurp p)))))

(defmacro with-wrapper ((pathname name) &body body)
  `(invoke-with-wrapper (lambda () ,@body) ,pathname ,name))

(defvar *components-being-dumped*)

(defun invoke-with-wrapper (body pathname name)
  (sb-ext:without-package-locks
   (let ((orig (fdefinition name)))
     (unwind-protect
	 (progn
	   (setf (fdefinition name)
		 (lambda (component &rest args)
		   (apply orig component args)
		   (let ((id (gethash component *components-being-dumped*)))
		     (unless id
		       (setf id (hash-table-count *components-being-dumped*))
		       (setf (gethash component *components-being-dumped*) id))
		     (sb-heapdump:dump-object (cons id component)
					      pathname
					      :if-exists :append
					      :initializer 'fetch-component))))
	   (funcall body))
       (setf (fdefinition name) orig)))))

(defvar *heap-file-components*)

(defun fetch-component (x)
  (push (cdr x) (gethash (car x) *heap-file-components*)))

(defun load-components (pathname)
  (let ((*heap-file-components* (make-hash-table)))
    (sb-heapdump:load-dumpfile pathname)
    *heap-file-components*))

(define-irspect-command (com-compile :name t) ()
  (com-quicksave)
  (let* ((p (quicksave-pathname))
	 (q (make-pathname :type "trace" :defaults p))
	 (r (make-pathname :type "heap" :defaults p))
	 (*standard-output* (knopf interactor))
	 (*components-being-dumped* (make-hash-table)))
    (when (probe-file r)
      (delete-file r))
    (with-wrapper (r 'sb-c::describe-component)
      (compile-file p :trace-file q))
    (com-load-heap)))

(define-irspect-command (com-load-heap :name t) ()
  (setf (components <frame>)
	(load-components
	 (make-pathname :type "heap" :defaults (quicksave-pathname))))
  (com-show-components))

(define-presentation-type component ())
(define-presentation-type cblock ())

(define-presentation-method present (object (type component) stream view &key)
  (declare (ignore view))
  (with-text-family (stream :sans-serif)
    (princ "<component " stream)
    (with-text-face (stream :italic)
      (princ (sb-c::component-name object) stream))
    (princ ">" stream)))

(define-presentation-method present (object (type cblock) stream view &key)
  (declare (ignore view))
  (with-text-family (stream :sans-serif)
    (princ "<cblock " stream)
    (with-text-face (stream :italic)
      (princ (sb-c::block-number object) stream))
    (princ ">" stream)))

(define-irspect-command (com-show-components :name t) ()
  (let ((*standard-output* (knopf interactor)))
    (fresh-line)
    (if (components <frame>)
	(loop
	    initially (format t "Components available for inspection:~%")
	    for cx being the hash-values in (components <frame>)
	    for c = (car cx)
	    do
	      (present c 'component)
	      (fresh-line))
	(format t "No components compiled yet.~%"))))

(defun draw-arrow-arc
    (stream from-object to-object x1 y1 x2 y2 &rest drawing-options)
  (declare (ignore from-object to-object))
  (apply #'draw-arrow* stream x1 y1 x2 y2 drawing-options))

(defun format-blocks (blocks)
  (format-graph-from-roots 
   blocks
   (lambda (object stream)
     (present object 'cblock :stream stream))
   #'sb-c::block-succ
   :arc-drawer #'draw-arrow-arc
   :graph-type :tree
   :orientation :vertical
   :merge-duplicates t)
  (fresh-line))

(defun closure (thing fn)
  (let ((things '())
	(worklist (list thing)))
    (loop
	for x = (pop worklist)
	while x
	do
	  (push x things)
	  (dolist (next (funcall fn x))
	    (unless (find next things)
	      (pushnew next worklist))))
    things))

(define-irspect-command com-show-component
    ((component 'component :gesture :select))
  (let ((*standard-output* (knopf interactor))
	(blocks '()))
    (fresh-line)
    (format t "Blocks in components ~A:~%~%" component)
    (sb-c::do-blocks (block component)
      (push block blocks))
    (loop
	for block = (pop blocks)
	while block
	do
	  (let* ((web (closure block
			       (lambda (x)
				 (union (sb-c::block-succ x)
					(sb-c::block-pred x)))))
		 (roots (remove-if #'sb-c::block-pred web)))
	    (format-blocks roots)
	    (setf blocks (set-difference blocks web))))))

(define-irspect-command com-inspect-component
    ((component 'component :gesture :menu))
  (clouseau:inspector component))

(define-irspect-command (com-clear :name t) ()
  (window-clear (knopf interactor)))

(defun irspect (&rest args &key width height background &allow-other-keys)
  (if background
      (clim-sys:make-process
       (lambda ()
	 (apply #'irspect :background nil args)))
      (run-frame-top-level
       (apply #'make-application-frame
	      'irspect
	      :allow-other-keys t
	      :width (or width 800)
	      :height (or height 600)
	      args))))

#+(or)
(irspect:irspect)
