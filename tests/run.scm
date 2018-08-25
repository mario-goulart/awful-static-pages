(cond-expand
  (chicken-4
   (use irregex srfi-13 setup-api utils)
   (use awful awful-static-pages test))
  (chicken-5
   (import (chicken format)
           (chicken irregex)
           (chicken io)
           (chicken file)
           (chicken pathname)
           (chicken process)
           (chicken process-context))
   (import awful awful-static-pages test))
  (else
   (error "Unsupported CHICKEN version.")))

(import awful awful-static-pages srfi-13 test)

(when (file-exists? "static")
  (delete-directory "static" 'recursive))

(page-template
 (lambda (content . args)
   content))

(define-page "/foo"
  (lambda ()
    "This is foo"))

(define (procedure-matcher path)
  (and (string-prefix? "/procedure" path)
       (list path)))

(define-page procedure-matcher
  (lambda (path)
    (string-append "This is " path)))

(define-page (irregex "/regex/[0-9]")
  (lambda (path)
    (string-append "This is " path)))

(request-pages
 '("/procedure/test"
   "/regex/4"))

(define (slurp file)
  (with-input-from-file file read-string))

(generate-static-pages! "static")

(test-begin "awful-static-pages")

(test-begin "library")
(test "This is foo\n" (slurp "static/foo/index.html"))
(test "This is /procedure/test\n" (slurp "static/procedure/test/index.html"))
(test "This is /regex/4\n" (slurp "static/regex/4/index.html"))
(test-end "library")

;;; Command line tool

(define asp
  (make-pathname
   (if (get-environment-variable "SALMONELLA_RUNNING")
       #f
       (pathname-directory (car (argv))))
   "awful-static-pages"))

(print "Using " asp)

(delete-directory "static" 'recursive)
(system (sprintf "~a --config=test-conf.scm test-app.scm static" asp))

(test-begin "command line tool")
(test "foo\n" (slurp "static/foo/index.html"))
(test "bar\n" (slurp "static/bar/index.html"))
(test "baz\n" (slurp "static/baz/index.html"))
(test "4\n" (slurp "static/1/2/3/4/index.html"))
(test "/regex/3\n" (slurp "static/regex/3/index.html"))
(test "/regex/7\n" (slurp "static/regex/7/index.html"))
(test "/procedure/foo\n" (slurp "static/procedure/foo/index.html"))
(test-end "command line tool")

(test-end "awful-static-pages")

(test-exit)
