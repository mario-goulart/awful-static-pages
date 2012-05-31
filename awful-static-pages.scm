(declare (uses chicken-syntax))
(use awful regex srfi-1 srfi-13)

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
        (let ((match (string-match path req)))
          (if match
              (begin
                (write-static-file! outdir req (lambda () (handler match)))
                (loop (cdr reqs)))
              (loop (cdr reqs))))))))


(define (generate-static-pages! apps outdir)
  (load-apps apps)
  (create-directory outdir 'with-parents)
  (for-each
   (lambda (res)
     (let ((path (caar res))
           (method (caddar res))
           (handler (cdr res)))
       (when (eq? method 'GET)
         (cond ((string? path)
                (write-static-page/string! path handler outdir))
               ((regexp? path)
                (write-static-page/regex! path handler outdir))))))
   (hash-table->alist (awful-resources-table))))


(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (cut string-match (conc option "=(.*)") <>) args)))
    (and val (cadr val))))


(define (usage #!optional exit-code)
  (print "Usage: " (pathname-strip-directory (program-name))
         " [ --config=<config file> ] <app1 app2 ... appn> <output dir>")
  (when exit-code
    (exit exit-code)))


(let ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))
  (let* ((config (cmd-line-arg '--config args))
         (apps+outdir (remove (lambda (arg)
                                (string-prefix? "--" arg))
                              args)))
    (when (or (null? apps+outdir)
              (null? (cdr apps+outdir)))
      (usage 1))
    (let ((apps (butlast apps+outdir))
          (outdir (last apps+outdir)))
      (when config
        (load config))
      (generate-static-pages! apps outdir))))
