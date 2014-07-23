;;;; cl-date-time-parser/cl-date-time-parser-test.asd

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-date-time-parser/LICENSE

;;====================================================================
;; System for CL-DATE-TIME-PARSER Test
;;====================================================================

(in-package #:cl-user)

(asdf:defsystem #:cl-date-time-parser-test
  :name        "cl-date-time-parser-test"
  :description "Test for cl-date-time-parser."
  :version     "0.1.13"
  :licence     "MIT (see file LICENCE for details)"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :depends-on  (#:cl-date-time-parser #:fiveam #:local-time)
  :components  ((:file "test")))


;;====================================================================
