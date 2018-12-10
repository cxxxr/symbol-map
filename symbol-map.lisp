;; -*- mode:lisp; package:symbol-map -*-

(in-package :symbol-map)

(defstruct definition
  symbol
  pathname
  form-path
  (position 0)
  form)

(defun collect-definitions (name)
  (when (member name '(initialize-instance print-object))
    (return-from collect-definitions))
  (loop :for definition-form :in (let ((swank::*buffer-package* (find-package :cl-user))
                                       (swank::*buffer-readtable* *readtable*))
                                   (mapcar #'swank::xref>elisp (swank::find-definitions name)))
        :collect (let ((definition (make-definition :symbol name :form (first definition-form))))
                   (loop :for x :in (cdr (assoc :location (rest definition-form)))
                         :do (optima:match x
                               ((list :file filename)
                                (setf (definition-pathname definition) (probe-file filename)))
                               ((list :position position)
                                (setf (definition-position definition) position))))
                   definition)))

(defstruct filenode
  (id (gensym))
  pathname
  definitions
  refers)

(defun filenode-name (filenode)
  (symbol-name (filenode-id filenode)))

(defun adjoin-definition (def definitions)
  (if (find def definitions :test #'equalp)
      definitions
      (merge 'list
             (list def)
             definitions
             #'<
             :key #'definition-position)))

(defun add-definitions (filenode symbol pathnames)
  (dolist (def (collect-definitions symbol))
    (cond ((uiop:pathname-equal (definition-pathname def)
                                (filenode-pathname filenode))
           (setf (filenode-definitions filenode)
                 (adjoin-definition def (filenode-definitions filenode))))
          ((member (definition-pathname def)
                   pathnames
                   :test #'uiop:pathname-equal)
           (setf (filenode-refers filenode)
                 (adjoin-definition def (filenode-refers filenode)))))))

(defun gen-filenodes (pathnames)
  (let ((filenodes))
    (dolist (pathname pathnames)
      (let ((filenode (make-filenode :pathname pathname)))
        (do-read-file (x pathname)
          (dolist (x (alexandria:flatten x))
            (when (and (symbolp x) (not (keywordp x)))
              (add-definitions filenode x pathnames))))
        (push filenode filenodes)))
    (nreverse filenodes)))

(defvar *filenodes*)

(defun find-filenode-from-pathname (pathname)
  (dolist (filenode *filenodes*)
    (when (uiop:pathname-equal pathname
                               (filenode-pathname filenode))
      (return filenode))))

(defun find-filenode-from-def (def)
  (dolist (filenode *filenodes*)
    (when (uiop:pathname-equal
           (definition-pathname def)
           (filenode-pathname filenode))
      (return filenode))))

(defun filenode-who-references (filenode)
  (let ((acc))
    (dolist (f *filenodes*)
      (unless (uiop:pathname-equal (filenode-pathname f)
                                   (filenode-pathname filenode))
        (dolist (def (filenode-refers f))
          (when (equalp filenode (find-filenode-from-def def))
            (push (cons f def) acc)))))
    acc))

(defun who-references-list (pathname pathnames)
  (let ((*filenodes* (gen-filenodes pathnames)))
    (loop
      :for (filenode . def)
      :in (filenode-who-references
           (find-filenode-from-pathname pathname))
      :collect (cons (filenode-pathname filenode) def))))

(defun absolute-pathname (pathname)
  (merge-pathnames (uiop:physicalize-pathname pathname)
                   (probe-file ".")))

(defun print-pathname/definitions-table (table)
  (maphash (lambda (pathname definitions)
             (format t "~&~A~%" pathname)
             (dolist (def (sort definitions #'<
                                :key #'definition-position))
               (format t
                       "~2T~A~%"
                       (definition-symbol def))))
           table))  

(export
 (defun who-references (pathname &optional (pathnames (lisp-files)))
   (setf pathname (absolute-pathname pathname))
   (let ((pathname/definitions-table (make-hash-table :test 'equal)))
     (loop :for (pathname . def) :in (who-references-list pathname pathnames)
       :do (push def
                 (gethash pathname
                          pathname/definitions-table)))
     (print-pathname/definitions-table pathname/definitions-table))
   (values)))

(export
 (defun depends-on (pathname &optional (pathnames (lisp-files)))
   (setf pathname (absolute-pathname pathname))
   (let* ((*filenodes* (gen-filenodes pathnames))
          (filenode (find-filenode-from-pathname pathname))
          (pathname/definitions-table (make-hash-table :test 'equal)))
     (dolist (def (filenode-refers filenode))
       (push def
             (gethash (definition-pathname def)
                      pathname/definitions-table)))
     (print-pathname/definitions-table pathname/definitions-table)
     (values))))

(defun file-graph-output (stream pathnames)
  (format stream "~&digraph {~%")
  (let ((*filenodes* (gen-filenodes pathnames)))
    (dolist (filenode *filenodes*)
      (format stream
              "~A [label = \"~A\"];~%"
              (filenode-name filenode)
              (file-namestring (filenode-pathname filenode))))
    (dolist (filenode *filenodes*)
      (let ((edges nil))
        (dolist (def (filenode-refers filenode))
          (let ((filenode2 (find-filenode-from-def def)))
            (push (filenode-name filenode2) edges)))
        (dolist (edge (delete-duplicates edges :test #'string=))
          (format stream "~A -> ~A;~%" (filenode-name filenode) edge)))))
  (format stream "}~%"))

(defun file-graph (&optional (pathnames (lisp-files)))
  (uiop:with-temporary-file (:stream stream
                             :direction :output
                             :pathname output-file
                             :keep t)
    (file-graph-output stream pathnames)
    :close-stream
    (uiop:run-program (format nil "dot -Tgif ~A -o ~A.gif" output-file output-file))
    (uiop:run-program (format nil "firefox ~A.gif &" output-file))))
