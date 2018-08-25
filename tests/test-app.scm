(cond-expand
  (chicken-4
   (use irregex srfi-13)
   (use awful awful-static-pages))
  (chicken-5
   (import (chicken irregex)
           (chicken string))
   (import awful awful-static-pages srfi-13))
  (else
   (error "Unsupported CHICKEN version.")))

(page-template
 (lambda (content . args)
   content))

(define-page "/foo"
  (lambda ()
    "foo"))

(define-page "/bar"
  (lambda ()
    "bar"))

(define-page "/baz"
  (lambda ()
    "baz"))

(define-page "/1/2/3/4"
  (lambda ()
    "4"))

(define-page (irregex "/regex/[0-9]")
  (lambda (path)
    (->string path)))

(define-page
  (lambda (path)
    (and (string-prefix? "/procedure" path)
         (list path)))
  (lambda (path)
    path))
