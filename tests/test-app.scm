(use awful regex srfi-13)

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

(define-page (regexp "/regex/[0-9]")
  (lambda (path)
    (->string path)))

(define-page
  (lambda (path)
    (and (string-prefix? "/procedure" path)
         (list path)))
  (lambda (path)
    path))
