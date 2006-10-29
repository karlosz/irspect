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
      (tab-layout:with-tab-layout ('pane :name 'tabs)
	("Source" source 'pane)
	("IR" interactor 'pane))))
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
(define-presentation-type node ())
(define-presentation-type lvar ())

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
    (cond
      ((eq object (sb-c::component-head (sb-c::block-component object)))
	(princ "head " stream))
      ((eq object (sb-c::component-tail (sb-c::block-component object)))
	(princ "tail " stream))
      (t
	(princ "cblock " stream)))
    (with-text-face (stream :italic)
      (princ (sb-c::block-number object) stream))))

(define-presentation-method present (object (type node) stream view &key)
  (declare (ignore view))
  (with-text-family (stream :sans-serif)
    (princ "ctran " stream)
    (with-text-face (stream :italic)
      (princ (sb-c::cont-num object) stream))
    (princ " " stream)
    (let ((node (sb-c::ctran-next object)))
      (etypecase node
	(sb-c::ref (sb-c::print-leaf (sb-c::ref-leaf node)))
	(sb-c::basic-combination
	  (let ((kind (sb-c::basic-combination-kind node)))
	    (format t "~(~A~A ~A~) "
		    (if (sb-c::node-tail-p node) "tail " "")
		    kind
		    (type-of node))
	    (sb-c::print-lvar (sb-c::basic-combination-fun node))
	    (dolist (arg (sb-c::basic-combination-args node))
	      (if arg
		  (sb-c::print-lvar arg)
		  (format t "<none> ")))))
	(sb-c::cset
	  (write-string "set ")
	  (sb-c::print-leaf (sb-c::set-var node))
	  (write-char #\space)
	  (sb-c::print-lvar (sb-c::set-value node)))
	(sb-c::cif
	  (write-string "if ")
	  (sb-c::print-lvar (sb-c::if-test node))
	  (sb-c::print-ctran (sb-c::block-start (sb-c::if-consequent node)))
	  (sb-c::print-ctran (sb-c::block-start (sb-c::if-alternative node))))
	(sb-c::bind
	  (write-string "bind ")
	  (sb-c::print-leaf (sb-c::bind-lambda node))
	  (when (sb-c::functional-kind (sb-c::bind-lambda node))
	    (format t " ~S ~S"
		    :kind
		    (sb-c::functional-kind (sb-c::bind-lambda node)))))
	(sb-c::creturn
	  (write-string "return ")
	  (sb-c::print-lvar (sb-c::return-result node))
	  (sb-c::print-leaf (sb-c::return-lambda node)))
	(sb-c::entry
	  (let ((cleanup (sb-c::entry-cleanup node)))
	    (case (sb-c::cleanup-kind cleanup)
	      ((:dynamic-extent)
		(format t "entry DX~{ v~D~}"
			(mapcar #'sb-c::cont-num (sb-c::cleanup-info cleanup))))
	      (t
		(format t "entry ~S" (sb-c::entry-exits node))))))
	(sb-c::exit
	  (let ((value (sb-c::exit-value node)))
	    (cond (value
		    (format t "exit ")
		    (sb-c::print-lvar value))
	      ((sb-c::exit-entry node)
		(format t "exit <no value>"))
	      (t
		(format t "exit <degenerate>")))))
	(sb-c::cast
	  (let ((value (sb-c::cast-value node)))
	    (format t "cast v~D ~A[~S -> ~S]" (sb-c::cont-num value)
		    (if (sb-c::cast-%type-check node) #\+ #\-)
		    (sb-c::cast-type-to-check node)
		    (sb-c::cast-asserted-type node))))))))


(define-presentation-method present (object (type lvar) stream view &key)
  (declare (ignore view))
  (with-text-family (stream :sans-serif)
    (princ "lvar " stream)
    (with-text-face (stream :italic)
      (princ (sb-c::cont-num object) stream))))

(defun tabs (tab-layout)
  (mapcar #'tab-layout::tab-pane-pane
	  (tab-layout::tab-panes-of-tab-layout tab-layout)))

(defun switch-to-tab (tab)
  (tab-layout:switch-to-pane tab 'tab-layout:tab-layout-pane))

(define-irspect-command (com-show-components :name t) ()
  (switch-to-tab (second (tabs (knopf tabs))))
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

(defun format-nodes (block)
  (format-graph-from-roots
   (append (sb-c::block-pred block)
	   (when (sb-c::block-start block)
	     (list (sb-c::block-start block))))
   (lambda (object stream)
     (etypecase object
       (sb-c::cblock
	 (present object 'cblock :stream stream))
       (sb-c::ctran
	 (present object 'node :stream stream))
       (sb-c::lvar
	 (present object 'lvar :stream stream))))
   (lambda (x)
     (etypecase x
       (sb-c::cblock
	 (when (find x (sb-c::block-pred block))
	   (list (sb-c::block-start block))))
       (sb-c::ctran
	 (let* ((node (sb-c::ctran-next x))
		(next (sb-c::node-next node))
		(lvar (and (sb-c::valued-node-p node) (sb-c::node-lvar node))))
	   (append (if next
		       (list next)
		       (sb-c::block-succ block))
		   (when lvar (list lvar)))))
       (sb-c::lvar
	 (let ((y (sb-c::node-prev (sb-c::lvar-dest x))))
	   (when y
	     (list y))))))
   :arc-drawer (lambda (stream from-object to-object x1 y1 x2 y2
			&rest drawing-options)
		 (if (or (typep (graph-node-object from-object) 'sb-c::lvar)
			 (typep (graph-node-object to-object) 'sb-c::lvar))
		     (draw-arrow* stream x1 y1 x2 y2 :line-dashes #(1.0 2.0))
		     (apply #'draw-arrow* stream x1 y1 x2 y2 drawing-options)))
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

(define-irspect-command com-show-block
    ((b 'cblock :gesture :select))
  (let ((*standard-output* (knopf interactor)))
    (fresh-line)
    (format-nodes b)))

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
