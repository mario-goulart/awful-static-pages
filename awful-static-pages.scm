(module awful-static-pages

(generate-static-pages! index-file request-pages)

(import scheme)
(cond-expand
  (chicken-4
   (import chicken)
   (use data-structures files irregex posix srfi-1 srfi-13 srfi-69)
   (use awful))
  (chicken-5
   (import (chicken base)
           (chicken file)
           (chicken irregex)
           (chicken pathname)
           (chicken sort)
           (chicken type))
   (import awful srfi-1 srfi-13 srfi-69))
  (else
   (error "Unsupported CHICKEN version.")))


(define index-file (make-parameter "index.html"))
(define request-pages (make-parameter '()))


(define (write-static-file! outdir path producer)
  (let* ((static-dir (normalize-pathname (make-pathname outdir path)))
         (static-file (make-pathname static-dir (index-file))))
    (create-directory static-dir 'with-parents)
    (print "Writing " static-file)
    (with-output-to-file static-file
      (lambda ()
        (print (producer))))))


(define (write-static-page/string! path handler outdir)
  (write-static-file! outdir path handler))


(define (write-static-page/regex! path handler outdir)
  (let loop ((reqs (request-pages)))
    (unless (null? reqs)
      (let ((req (car reqs)))
        (let ((match (irregex-match path req)))
          (if match
              (begin
                (write-static-file! outdir req (lambda ()
                                                 (handler
                                                  (irregex-match-substring match))))
                (loop (cdr reqs)))
              (loop (cdr reqs))))))))


(define (write-static-page/procedure! path handler outdir)
  (let loop ((reqs (request-pages)))
    (unless (null? reqs)
      (let ((req (car reqs)))
        (let ((match (path req)))
          (when (list? match)
            (parameterize ((%path-procedure-result match))
              (write-static-file! outdir req (lambda () (apply handler match)))))
          (loop (cdr reqs)))))))


(define (generate-static-pages! outdir #!key resources)
  (create-directory outdir 'with-parents)
  (for-each
   (lambda (res)
     (let ((path (caar res))
           (method (caddar res))
           (handler (let ((maybe-handler (cdr res)))
                      ;; awful 0.42.0 changed the resources table
                      ;; format: the hash table value is now a pair
                      ;; (<handler> . <strict?>)
                      (if (pair? maybe-handler)
                          (car maybe-handler)
                          maybe-handler))))
       (when (eq? method 'GET)
         (cond ((string? path)
                (write-static-page/string! path handler outdir))
               ((irregex? path)
                (write-static-page/regex! path handler outdir))
               ((procedure? path)
                (write-static-page/procedure! path handler outdir))))))
   (or resources
       (let ((awful-resources (hash-table->alist (awful-resources-table))))
         ;; Sort resources for a deterministic order: first
         ;; procedures, then regex matchers, then sorted string
         ;; matchers
         (append (filter (lambda (res)
                           (procedure? (caar res)))
                         awful-resources)
                 (filter (lambda (res)
                           (irregex? (caar res)))
                         awful-resources)
                 (sort (filter (lambda (res)
                                 (string? (caar res)))
                               awful-resources)
                       (lambda (a b)
                         (string< (caar a) (caar b)))))))))

) ;; end module
