(module awful-static-pages-cmd ()

(import scheme)
(cond-expand
  (chicken-4
   (import chicken)
   (declare (uses chicken-syntax))
   (use data-structures files irregex srfi-1 srfi-13)
   (use awful awful-static-pages))
  (chicken-5
   (import (chicken base)
           (chicken file)
           (chicken irregex)
           (chicken string)
           (chicken pathname)
           (chicken process-context))
   (import awful awful-static-pages srfi-1 srfi-13))
  (else
   (error "Unsupported CHICKEN version.")))


(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))


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
      (load-apps apps)
      (generate-static-pages! outdir))))

) ;; end module
