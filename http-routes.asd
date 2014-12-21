(asdf:defsystem http-routes
  :version "0.1"
  :description "URL routing library"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :licence "MIT"
  :depends-on (:alexandria :split-sequence)
  :serial t
  :components ((:file "src/packages")
               (:file "src/matcher")
               (:file "src/route-parser")
               (:file "src/route")
               (:file "src/packet")
               (:file "src/helpers"))
  ;; :long-description ""
  )
