(asdf:defsystem http-routes-test
  :version "0"
  :description "HTTP-ROUTES test suite"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :licence "MIT"
  :depends-on (:fiveam :http-routes :cl-coveralls)
  :serial t
  :components ((:file "test/package")
               (:file "test/matcher")
               (:file "test/parser")
               (:file "test/route")
               (:file "test/packet")))
