;; -*- mode:lisp; package:symbol-map -*-

(in-package :symbol-map)

(export
 (defun lisp-files (&optional (dir "."))
   (remove-if-not (lambda (p)
                    (string= "lisp" (pathname-type p)))
                  (cl-fad:list-directory dir))))

(defun read-in-package (package stream eof-value)
  (let ((*package* (find-package package)))
    (read stream nil eof-value)))

(defun starts-with (list x)
  (and (consp list)
       (eql x (car list))))

(defun read-file (pathname function)
  (let ((eof-value (gensym))
        (package (find-package :cl-user)))
    (with-open-file (in pathname)
      (loop :for x := (read-in-package package in eof-value)
        :until (eq x eof-value)
        :do (if (starts-with x 'in-package)
                (setf package (cadr x))
                (funcall function x))))))

(defmacro do-read-file ((var pathname) &body body)
  `(read-file ,pathname
              (lambda (,var) ,@body)))
