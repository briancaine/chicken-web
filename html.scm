(import chicken scheme)
(use sxml-transforms ports utf8 srfi-1)

(define (alist-ref/equal? key alist #!optional default)
  (alist-ref key alist equal? default))

(define (alist-update/equal? key value alist)
  (alist-update key value alist equal?))

(define *my-rules* '())

(define (rule-name rule) (car rule))
(define (rule-body rule) (cdr rule))

(define (add-rule rule rules)
  (alist-update/equal? (rule-name rule) (rule-body rule) rules))

(define (make-normal-rule name handler)
  (cons name handler))

(define (make-additional-bindings-rule name bindings handler)
  (cons name (cons bindings handler)))

(define (make-macro-rule name handler)
  `(,name *macro* . ,handler))

(define (make-preorder-rule name handler)
  `(,name *preorder* . ,handler))

(define (->html x)
  (SRV:send-reply
    (pre-post-order* x
      (append *my-rules* universal-conversion-rules*))))

(define (->html-string x)
  (with-output-to-string (lambda () (->html x))))
