(asdf:defsystem symbol-map
  :depends-on (:alexandria :cl-fad :optima)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "symbol-map")))
