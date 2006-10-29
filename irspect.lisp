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

(define-presentation-type inspectable-object ())
(define-presentation-type component () :inherit-from 'inspectable-object)
(define-presentation-type cblock () :inherit-from 'inspectable-object)
(define-presentation-type node () :inherit-from 'inspectable-object)
(define-presentation-type lvar () :inherit-from 'inspectable-object)

(define-presentation-method present (object (type component) stream view &key)
  (declare (ignore view))
  (with-text-family (stream :sans-serif)
    (princ "component " stream)
    (with-text-face (stream :italic)
      (princ (sb-c::component-name object) stream))))

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

(defmacro with-node ((name node) &body body)
  `(invoke-with-node (lambda () ,@body) ,name ,node))

(defun invoke-with-node (fn name node)
  (with-text-family (t :sans-serif)
    (with-text-size (t :small)
      (format t "(~D) " (sb-c::cont-num node)))
    (princ name)
    (princ " ")
    (with-text-face (t :italic)
      (funcall fn))))

(defun present-leaf (x)
  (with-output-as-presentation (t x 'inspectable-object)
    (let ((str (with-output-to-string (s) (sb-c::print-leaf x s))))
      (if (>= (length str) 20)
	  (format t "~A..." (substitute #\space #\newline (subseq str 0 20)))
	  (write-string str)))))

(defgeneric present-node (ctran node))

(defmethod present-node (ctran (node sb-c::ref))
  (with-node ("ref" ctran)
    (present-leaf (sb-c::ref-leaf node))))

(defmethod present-node (ctran (node sb-c::cif))
  (with-node ("if" ctran)
    (present (sb-c::if-test node) 'lvar)))

(defmethod present-node (ctran (node sb-c::entry))
  (let ((cleanup (sb-c::entry-cleanup node)))
    (if (eq (sb-c::cleanup-kind cleanup) :dynamic-extent)
	(with-node ("entry DX" ctran)
	  (present (sb-c::cleanup-info cleanup) 'inspectable-object))
	(with-node ("entry" ctran)
	  (when (sb-c::entry-exits node)
	    (present (sb-c::entry-exits node) 'inspectable-object))))))

(defmethod present-node (ctran (node sb-c::bind))
  (with-node ("bind" ctran)
    (present-leaf (sb-c::bind-lambda node))
    (when (sb-c::functional-kind (sb-c::bind-lambda node))
      (format t " ~S ~S"
	      :kind
	      (sb-c::functional-kind (sb-c::bind-lambda node))))))

(defmethod present-node (ctran (node sb-c::basic-combination))
  (let ((kind (sb-c::basic-combination-kind node)))
    (with-node ((format nil "~(~A~A ~A~) "
			(if (sb-c::node-tail-p node) "tail " "")
			kind
			(type-of node))
		ctran)
      (present (sb-c::basic-combination-fun node) 'lvar)
      (dolist (arg (sb-c::basic-combination-args node))
	(princ " ")
	(if arg
	    (present arg 'lvar)
	    (format t "<none> "))))))

(defmethod present-node (ctran (node sb-c::creturn))
  (with-node ("return" ctran)
    (present (sb-c::return-result node) 'lvar)
    (princ " ")
    (present-leaf (sb-c::return-lambda node))))

(defmethod present-node (ctran (node sb-c::cset))
  (with-node ("set" ctran)
    (present-leaf (sb-c::set-var node))
    (princ " ")
    (present (sb-c::set-value node) 'lvar)))

(defmethod present-node (ctran (node sb-c::exit))
  (let ((value (sb-c::exit-value node)))
    (cond
      (value
	(with-node ("exit" ctran)
	  (present value 'lvar)))
      ((sb-c::exit-entry node)
	(with-node ("exit <no value>" ctran)))
      (t
	(with-node ("exit <degenerate>" ctran))))))

(defmethod present-node (ctran (node sb-c::cast))
  (with-node ("cast" ctran)
    (present (sb-c::cast-value node) 'lvar)
    (format t " ~A[~S -> ~S]"
	    (if (sb-c::cast-%type-check node) #\+ #\-)
	    (sb-c::cast-type-to-check node)
	    (sb-c::cast-asserted-type node))))

(defmethod present-node (ctran (node t))
  (with-node ("unknown node" ctran)
    (princ node)))

(define-presentation-method present (object (type node) stream view &key)
  (declare (ignore view))
  (let ((*standard-output* stream))
    (surrounding-output-with-border (t :shape :rectangle)
      (present-node object (sb-c::ctran-next object)))))

(define-presentation-method present (object (type lvar) stream view &key)
  (declare (ignore view))
  (with-text-family (stream :sans-serif)
    (princ "lvar " stream)
    (with-text-family (stream :sans-serif)
      (with-text-face (stream :italic)
	(princ (sb-c::cont-num object) stream)))))

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
	 (present object 'lvar :stream stream))
       ((eql :empty-block)
	 (with-text-face (t :italic)
	   (princ "empty block")))))
   (lambda (x)
     (etypecase x
       (sb-c::cblock
	 (when (find x (sb-c::block-pred block))
	   (list (or (sb-c::block-start block) :empty-block))))
       (sb-c::ctran
	 (let* ((node (sb-c::ctran-next x))
		(next (sb-c::node-next node))
		(lvar (and (sb-c::valued-node-p node)
			   (sb-c::node-lvar node))))
	   (append (if next
		       (list next)
		       (sb-c::block-succ block))
		   (when lvar (list lvar)))))
       (sb-c::lvar
	 (let ((y (sb-c::node-prev (sb-c::lvar-dest x))))
	   (when y
	     (list y))))
       ((eql :empty-block)
	 (sb-c::block-succ block))))
   :arc-drawer (lambda (stream from-object to-object x1 y1 x2 y2
			&rest drawing-options)
		 (if (or (typep (graph-node-object from-object) 'sb-c::lvar)
			 (typep (graph-node-object to-object) 'sb-c::lvar))
		     (draw-arrow* stream x1 y1 x2 y2 :line-dashes #(1.0 2.0))
		     (apply #'draw-arrow* stream x1 y1 x2 y2
			    drawing-options)))
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

(define-irspect-command com-inspect-object
    ((object 'inspectable-object :gesture :menu))
  (clouseau:inspector object))

(define-irspect-command com-select-default
    ((object 'inspectable-object :gesture :select))
  (clouseau:inspector object))

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
