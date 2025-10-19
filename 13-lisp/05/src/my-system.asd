(asdf:defsystem "my-system"
  :description "My Lisp system"
  :author "George"
  :version "0.1"
  :components ((:file "package")
               (:file "main" :depends-on ("package")))
  :depends-on ("cl-ppcre")) 
