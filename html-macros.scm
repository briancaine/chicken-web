(define-syntax define-rule
  (syntax-rules ()
    ((define-rule (name . args) body . rest)
     (set! *my-rules*
       (add-rule
         (make-normal-rule 'name
           (lambda (key others)
             (apply (lambda args body . rest) others)))
         *my-rules*)))))

(define-syntax define-macro-rule
  (syntax-rules ()
    ((define-macro-rule (name . args) body . rest)
     (set! *my-rules*
       (add-rule
         (make-macro-rule 'name
           (lambda (key others)
             (apply (lambda args body . rest) others)))
         *my-rules*)))))
