;;;; cl-date-time-parser/test.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-date-time-parser/LICENSE

;;====================================================================
;; TEST for CL-DATE-TIME-PARSER
;;====================================================================
;; some examples from http://pythonhosted.org/feedparser/date-parsing.html

(in-package :cl-user)

(defpackage #:cl-date-time-parser-test
  (:export #:run-tests)
  (:use #:cl
        #:fiveam
        #:local-time)
  (:import-from #:cl-date-time-parser
                #:parse-date-time))

(in-package #:cl-date-time-parser-test)

(def-suite ?cl-date-time-parser)
(in-suite  ?cl-date-time-parser)


;;--------------------------------------------------------------------
;; Utils
;;--------------------------------------------------------------------

(defun run-tests ()
  (let* ((result-list  (run '?cl-date-time-parser))
         (total-result (every (lambda (r) (typep r 'fiveam::test-passed)) ; !
                              result-list)))
    (explain! result-list)
    total-result))

(defmacro =>? (expr expected &rest reason-args)
  `(is (equal (multiple-value-list ,expr)
              (multiple-value-list ,expected))
       ,@reason-args))

(defmacro =>t? (expr &rest reason-args)
  `(is-true ,expr ,@reason-args))

(defmacro =>nil? (expr &optional reason-args)
  `(is-false ,expr ,@reason-args))

(defun ut (&rest args)
  (apply #'encode-universal-time args))


;;--------------------------------------------------------------------
;; Internal functions
;;--------------------------------------------------------------------

(test ?get-offset
  (=>? (cl-date-time-parser::get-offset "JST") (* -9 60 60))
  (=>? (cl-date-time-parser::get-offset "GMT") 0)
  (=>? (cl-date-time-parser::get-offset "MDT") (* -6 60 60)))


(test ?year-to-ut
  (=>? (cl-date-time-parser::year-to-ut 1900) 0)
  (=>? (cl-date-time-parser::year-to-ut 1901) (* 365 24 60 60))
  (=>? (cl-date-time-parser::year-to-ut 1902) (+ (cl-date-time-parser::year-to-ut 1901) (* 365 24 60 60)))
  (=>? (cl-date-time-parser::year-to-ut 1905) (+ (cl-date-time-parser::year-to-ut 1904) (* 366 24 60 60)))
  (=>? (cl-date-time-parser::year-to-ut 2000) (local-time:timestamp-to-universal
                                               (local-time:parse-timestring "2000-01-01T00:00:00Z")))
  (=>? (cl-date-time-parser::year-to-ut 2013) (local-time:timestamp-to-universal
                                               (local-time:parse-timestring "2013-01-01T00:00:00Z"))))


(test ?leap-year-p
  (=>nil? (cl-date-time-parser::leap-year-p 1999))
  (=>t?   (cl-date-time-parser::leap-year-p 2000))
  (=>nil? (cl-date-time-parser::leap-year-p 2001)))


(test ?month-to-ut
  (=>t? (every (lambda (m) (= (cl-date-time-parser::month-to-ut m t)
                              (cl-date-time-parser::month-to-ut m nil)))
               '("Jan" "January" 1)))
  (=>t? (every (lambda (m) (= (cl-date-time-parser::month-to-ut m t)
                              (cl-date-time-parser::month-to-ut m nil)))
               '("Feb" "February" 2)))
  (=>t? (every (lambda (m) (= (* 24 60 60)
                              (- (cl-date-time-parser::month-to-ut m t)
                                 (cl-date-time-parser::month-to-ut m nil))))
               '("Mar" "March" 3)))
  (=>t? (every (lambda (m) (= (* 24 60 60)
                              (- (cl-date-time-parser::month-to-ut m t)
                                 (cl-date-time-parser::month-to-ut m nil))))
               '("Apr" "April" 4)))
  (=>t? (every (lambda (m) (= (* 24 60 60)
                              (- (cl-date-time-parser::month-to-ut m t)
                                 (cl-date-time-parser::month-to-ut m nil))))
               '("Dec" "December" 12))))


;;--------------------------------------------------------------------
;; parse-rfc822-genus
;;--------------------------------------------------------------------

(defun rfc822 (dt)
  (cl-date-time-parser::parse-rfc822-genus dt))

(test ?parse-rfc822-genus
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 GMT")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 gmt")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 Z")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00Z")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 z")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00z")
       (values (ut 0 0 0 1 1 2013 0) 0))

  (=>? (rfc822 "Sat, 02 Mar 2013 01:23:45 EDT")
       (values (ut 45 23 1 2 3 2013 -4) 0))
  (=>? (rfc822 "Sat, 02 Mar 2013 01:23:45 FOOBAZBAR")
       (values (ut 45 23 1 2 3 2013 0) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 JST")
       (values (ut 0 0 0 1 1 2013 -9) 0))
  (=>? (rfc822 "Sat, 01 Feb 2013 01:23:45 JST")
       (values (ut 45 23 1 1 2 2013 -9) 0))
  (=>? (rfc822 "01 Dec 13 00:00 JST")
       (values (ut 0 0 0 1 12 2013 -9) 0))

  (=>? (rfc822 "24 Dec 49 12:00 EST")
       (values (ut 0 0 12 24 12 2049 -5) 0))
  (=>? (rfc822 "24 Dec 50 12:00 EST")
       (values (ut 0 0 12 24 12 1950 -5) 0))

  (=>? (rfc822 "Sat, 01 Jan 2000 00:00:00.42 GMT")
       (values (ut 0 0 0 1 1 2000 0) 0.42))
  (=>? (rfc822 "Sat, 01 Jan 2000 00:00:00:42 GMT")
       (values (ut 0 0 0 1 1 2000 0) 0.42))

  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 +0700")
       (values (ut 0 0 0 1 1 2013 -7) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00+1300")
       (values (ut 0 0 0 1 1 2013 -13) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 +07:00")
       (values (ut 0 0 0 1 1 2013 -7) 0))
  (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00+13:00")
       (values (ut 0 0 0 1 1 2013 -13) 0))

  (=>? (rfc822 "Thu, 01 Jan 2004")
       (values (ut 0 0 0 1 1 2004 0) 0))
  (=>? (rfc822 "01 Jan 2004")
       (values (ut 0 0 0 1 1 2004 0) 0))
  (=>? (rfc822 "1 Jan 04")
       (values (ut 0 0 0 1 1 2004 0) 0))

  (=>? (rfc822 "Sun Jan 4 16:29:06 PST 2004")
       (values (ut 6 29 16 4 1 2004 -8) 0))

  (=>? (rfc822 "Thu Jul 23 19:42:23 JST 2013")
       (values (ut 23 42 19 23 7 2013 -9) 0))

  (=>? (rfc822 "Thudesday, 23-Jul-13 19:42:23 GMT")
       (values (ut 23 42 19 23 7 2013 0) 0)))


(test ?parse-rfc822-genus.suppiled-year
  (let ((this-year (nth-value 5 (decode-universal-time
                                 (get-universal-time) 0))))
    (=>? (rfc822 "Thu, 01 Jan")
         (values (ut 0 0 0 1 1 this-year 0) 0))
    (=>? (rfc822 "01 Jan")
         (values (ut 0 0 0 1 1 this-year 0) 0))
    (=>? (rfc822 "1 Jan")
         (values (ut 0 0 0 1 1 this-year 0) 0))))


;;--------------------------------------------------------------------
;; parse-iso8601-genus
;;--------------------------------------------------------------------

(defun iso8601 (dt)
  (cl-date-time-parser::parse-iso8601-genus dt))

(test ?parse-iso8601-genus
  (=>? (iso8601 "2000-01-01T00:00:00Z")
       (values (ut 0 0 0 1 1 2000 0) 0))

  (=>? (iso8601 "2003-12-31T10:14:55.7-08:00")
       (values (ut 55 14 10 31 12 2003 +8) 0.7))
  (=>? (iso8601 "2003-12-31T10:14:55.7-0800")
       (values (ut 55 14 10 31 12 2003 +8) 0.7))

  (=>? (iso8601 "2003-12-31T10:14:55+08:00")
       (values (ut 55 14 10 31 12 2003 -8) 0))
  (=>? (iso8601 "2003-12-31T10:14:55+0800")
       (values (ut 55 14 10 31 12 2003 -8) 0))

  (=>? (iso8601 "2003-12-31T10:14:55.7Z")
       (values (ut 55 14 10 31 12 2003 0) 0.7))
  (=>? (iso8601 "2003-12-31T10:14:55Z")
       (values (ut 55 14 10 31 12 2003 0) 0))

  (=>? (iso8601 "2003-12-31T10:14Z")
       (values (ut 0 14 10 31 12 2003 0) 0))

  (=>? (iso8601 "2003-12-31T101455-08:00")
       (values (ut 55 14 10 31 12 2003 +8) 0))
  (=>? (iso8601 "2003-12-31T101455-0800")
       (values (ut 55 14 10 31 12 2003 +8) 0))
  (=>? (iso8601 "2003-12-31T1014-08:00")
       (values (ut 0 14 10 31 12 2003 +8) 0))
  (=>? (iso8601 "2003-12-31T1014-0800")
       (values (ut 0 14 10 31 12 2003 +8) 0))
  (=>? (iso8601 "2003-12-31T1014557Z")
       (values (ut 55 14 10 31 12 2003 0) 0.7))
  (=>? (iso8601 "2003-12-31T101455Z")
       (values (ut 55 14 10 31 12 2003 0) 0))
  (=>? (iso8601 "2003-12-31T1014Z")
       (values (ut 0 14 10 31 12 2003 0) 0))
  (=>? (iso8601 "2003-12-31T10Z")
       (values (ut 0 0 10 31 12 2003 0) 0))

  (=>? (iso8601 "2003")
       (values (ut 0 0 0 1 1 2003 0) 0))
  (=>? (iso8601 "2003-12")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (iso8601 "2003-12-31")
       (values (ut 0 0 0 31 12 2003 0) 0))
  (=>? (iso8601 "20031231")
       (values (ut 0 0 0 31 12 2003 0) 0))

  (=>? (iso8601 "-03-12")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (iso8601 "-03")
       (values (ut 0 0 0 1 1 2003 0) 0))
  (=>? (iso8601 "-0312")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (iso8601 "-03-12-31")
       (values (ut 0 0 0 31 12 2003 0) 0))

  (=>? (iso8601 "03-12-31")
       (values (ut 0 0 0 31 12 2003 0) 0))

  (=>? (iso8601 "031231")
       (values (ut 0 0 0 31 12 2003 0) 0))

  (=>? (iso8601 "2003335")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (iso8601 "2003-335")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (iso8601 "03335")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (iso8601 "03-335")
       (values (ut 0 0 0 1 12 2003 0) 0)))


;;--------------------------------------------------------------------
;; parse-date-time
;;--------------------------------------------------------------------

(defun parse (dt) (cl-date-time-parser:parse-date-time dt))

(test ?parse-date-time.rfc822-genus
  (=>? (parse "Sat, 01 Jan 2013 00:00:00 GMT")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00 gmt")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00 Z")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00Z")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00 z")
       (values (ut 0 0 0 1 1 2013 0) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00z")
       (values (ut 0 0 0 1 1 2013 0) 0))

  (=>? (parse "Sat, 02 Mar 2013 01:23:45 EDT")
       (values (ut 45 23 1 2 3 2013 -4) 0))
  (=>? (parse "Sat, 02 Mar 2013 01:23:45 FOOBAZBAR")
       (values (ut 45 23 1 2 3 2013 0) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00 JST")
       (values (ut 0 0 0 1 1 2013 -9) 0))
  (=>? (parse "Sat, 01 Feb 2013 01:23:45 JST")
       (values (ut 45 23 1 1 2 2013 -9) 0))
  (=>? (parse "01 Dec 13 00:00 JST")
       (values (ut 0 0 0 1 12 2013 -9) 0))

  (=>? (parse "24 Dec 49 12:00 EST")
       (values (ut 0 0 12 24 12 2049 -5) 0))
  (=>? (parse "24 Dec 50 12:00 EST")
       (values (ut 0 0 12 24 12 1950 -5) 0))

  (=>? (parse "Sat, 01 Jan 2000 00:00:00.42 GMT")
       (values (ut 0 0 0 1 1 2000 0) 0.42))
  (=>? (parse "Sat, 01 Jan 2000 00:00:00:42 GMT")
       (values (ut 0 0 0 1 1 2000 0) 0.42))

  (=>? (parse "Sat, 01 Jan 2013 00:00:00 +0700")
       (values (ut 0 0 0 1 1 2013 -7) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00+1300")
       (values (ut 0 0 0 1 1 2013 -13) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00 +07:00")
       (values (ut 0 0 0 1 1 2013 -7) 0))
  (=>? (parse "Sat, 01 Jan 2013 00:00:00+13:00")
       (values (ut 0 0 0 1 1 2013 -13) 0))

  (=>? (parse "Thu, 01 Jan 2004")
       (values (ut 0 0 0 1 1 2004 0) 0))
  (=>? (parse "01 Jan 2004")
       (values (ut 0 0 0 1 1 2004 0) 0))
  (=>? (parse "1 Jan 04")
       (values (ut 0 0 0 1 1 2004 0) 0))

  (=>? (parse "Sun Jan 4 16:29:06 PST 2004")
       (values (ut 6 29 16 4 1 2004 -8) 0)))


(test ?parse-date-time.iso8601-genus
  (=>? (parse "2000-01-01T00:00:00Z")
       (values (ut 0 0 0 1 1 2000 0) 0))

  (=>? (parse "2003-12-31T10:14:55.7-08:00")
       (values (ut 55 14 10 31 12 2003 +8) 0.7))
  (=>? (parse "2003-12-31T10:14:55.7-0800")
       (values (ut 55 14 10 31 12 2003 +8) 0.7))

  (=>? (parse "2003-12-31T10:14:55+08:00")
       (values (ut 55 14 10 31 12 2003 -8) 0))
  (=>? (parse "2003-12-31T10:14:55+0800")
       (values (ut 55 14 10 31 12 2003 -8) 0))

  (=>? (parse "2003-12-31T10:14:55.7Z")
       (values (ut 55 14 10 31 12 2003 0) 0.7))
  (=>? (parse "2003-12-31T10:14:55Z")
       (values (ut 55 14 10 31 12 2003 0) 0))

  (=>? (parse "2003-12-31T10:14Z")
       (values (ut 0 14 10 31 12 2003 0) 0))

  (=>? (parse "2003-12-31T101455-08:00")
       (values (ut 55 14 10 31 12 2003 +8) 0))
  (=>? (parse "2003-12-31T101455-0800")
       (values (ut 55 14 10 31 12 2003 +8) 0))
  (=>? (parse "2003-12-31T1014-08:00")
       (values (ut 0 14 10 31 12 2003 +8) 0))
  (=>? (parse "2003-12-31T1014-0800")
       (values (ut 0 14 10 31 12 2003 +8) 0))
  (=>? (parse "2003-12-31T1014557Z")
       (values (ut 55 14 10 31 12 2003 0) 0.7))
  (=>? (parse "2003-12-31T101455Z")
       (values (ut 55 14 10 31 12 2003 0) 0))
  (=>? (parse "2003-12-31T1014Z")
       (values (ut 0 14 10 31 12 2003 0) 0))
  (=>? (parse "2003-12-31T10Z")
       (values (ut 0 0 10 31 12 2003 0) 0))

  (=>? (parse "2003")
       (values (ut 0 0 0 1 1 2003 0) 0))
  (=>? (parse "2003-12")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (parse "2003-12-31")
       (values (ut 0 0 0 31 12 2003 0) 0))
  (=>? (parse "20031231")
       (values (ut 0 0 0 31 12 2003 0) 0))

  (=>? (parse "-03-12")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (parse "-03")
       (values (ut 0 0 0 1 1 2003 0) 0))
  (=>? (parse "-0312")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (parse "-03-12-31")
       (values (ut 0 0 0 31 12 2003 0) 0))
  
  (=>? (parse "03-12-31")
       (values (ut 0 0 0 31 12 2003 0) 0))

  (=>? (parse "031231")
       (values (ut 0 0 0 31 12 2003 0) 0))

  (=>? (parse "2003335")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (parse "2003-335")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (parse "03335")
       (values (ut 0 0 0 1 12 2003 0) 0))
  (=>? (parse "03-335")
       (values (ut 0 0 0 1 12 2003 0) 0)))


(test ?parse-date-time.mssql
  ;; MSSQL date time format
  (=>? (parse "2004-07-08 23:56:58.7")
       (values (ut 58 56 23 8 7 2004 0) 0.7))

  ;; MSSQL-ish date time format (without fractional second)
  (=>? (parse "2004-07-08 23:56:58")
       (values (ut 58 56 23 8 7 2004 0) 0)))


(test ?parse-date-time.bogus-W3CDTF
  ;; bogus W3CDTF (invalid hour)
  (=>? (parse "2003-12-31T25:14:55Z")
       (values (ut 55 14 1 1 1 2004 0) 0))

  ;; bogus W3CDTF (invalid minute)
  (=>? (parse "2003-12-31T10:61:55Z")
       (values (ut 55 1 11 31 12 2003 0) 0))

  ;; bogus W3CDTF (invalid second)
  (=>? (parse "2003-12-31T10:14:61Z")
       (values (ut 1 15 10 31 12 2003 0) 0)))


(test ?parse-date-time.rfc3339.2d-years
  ;; c.f. RFC 3339, 3. Two Digit Years, last item
  (=>? (parse ":0-09-09")
       (values (ut 0 0 0 9 9 2000 0) 0))
  (=>? (parse ";0-09-09")
       (values (ut 0 0 0 9 9 2010 0) 0)))


(test ?parse-date-time.rfc822-genus.suppiled-year
  (let ((this-year (nth-value 5 (decode-universal-time
                                 (get-universal-time) 0))))
    (=>? (parse "Thu, 01 Jan")
         (values (ut 0 0 0 1 1 this-year 0) 0))
    (=>? (parse "01 Jan")
         (values (ut 0 0 0 1 1 this-year 0) 0))
    (=>? (parse "1 Jan")
         (values (ut 0 0 0 1 1 this-year 0) 0))))


;;====================================================================
