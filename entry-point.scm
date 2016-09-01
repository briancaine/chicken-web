(define-syntax define-entry-func
  (syntax-rules ()
    ((define-entry-func (name args) exp ...)
     (begin
       (define (name args) exp ...)
       (cond-expand
         (compiling
          (name (command-line-arguments)))
         (else))))))

;; (define-entry-func (main args) ...)
;; produces a function that can be called with the -ss flag with csi
;; and also has a cond-expand that'll call main in a compiled executable
;; in the same way as if you were running it from the interpreter
