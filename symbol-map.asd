(asdf:defsystem symbol-map
  :depends-on (:sb-introspect :uiop :alexandria :cl-fad)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "symbol-map")))
