(use json ports extras (srfi 69) (srfi 1) vector-lib)

(use message-digest sha1)

(define (sha1-hash-string str)
  (message-digest-string (sha1-primitive) str))

(use uuid)

(define (touch! filename)
  (with-input-from-pipe (format "touch ~s" filename) read-string)
  (file-exists? filename))

(define (with-temp-file func)
  (let* ((filename (string-append "/tmp/temp_" (uuid-v4)))
         (exists? (touch! filename)))
    (dynamic-wind
        (lambda () (unless exists? (error "Unable to make temp file" filename)))
        (lambda () (func filename))
        (lambda ()
          (if exists?
              (begin (delete-file filename)
                     (set! exists? #f))
              (error "File doesn't exist" filename))))))


(define ->symbol (compose string->symbol ->string))

(define (void? x)
  (eq? x (void)))

(define (json-normalize json)
  (cond
   ((hash-table? json)
    (json-normalize (list->vector (hash-table->alist json))))
   ((vector? json)
    (sort (vector-map (lambda (idx val)
                        (cons (->string (car val))
                              (json-normalize (cdr val))))
                      json)
          (lambda (a b)
            (string<? (car a) (car b)))))
   ((list? json)
    (map json-normalize json))
   ((symbol? json)
    (->string json))
   ((or (number? json) (void? json) (string? json) (boolean? json))
    json)
   (else (error "Bad JSON" json))))

(define (json-hash json)
  (sha1-hash-string
   (with-output-to-string (lambda () (write (json-normalize json))))))

(define (pp-command filename)
; perl pretty print
;  (format "json_pp < ~s" filename)
; python pretty print
;  (format "python -mjson.tool < ~s" filename)
; yajl tools
  (format "json_reformat < ~s" filename)
)

(define (json-pretty-print sexp)
  (with-temp-file
   (lambda (filename)
     (with-output-to-file filename (lambda () (json-write sexp)))
     (display (with-input-from-pipe (pp-command filename)
                                    read-string)))))

(define (alist? x)
  (and (list? x)
       (every pair? x)
       #t))

(define (avector? x)
  (and (vector? x)
       (alist? (vector->list x))))

(define (json-table? x)
  (or (avector? x)
      (hash-table? x)))

(define (json-parse-error? exn)
  (and (condition? exn)
       (and-let* ((args (condition-property-accessor 'exn 'arguments exn)))
         (and (pair? args)
              (pair? (car args))
              (eq? (caar args) 'json-parse-error)))))

(define parsed-json-data (make-parameter #f))

(define (make-json-handler next-handler)
  (error "Not implemented anymore"))

(define json-mime-type (make-parameter 'text/plain))

(define (json-handler-writer out)
  (lambda (data)
    (out (with-output-to-string
           (lambda ()
             (printf "Content-type: ~a\n\n" (json-mime-type))
             (json-write data)
             (newline))))))

(define (json-get name obj #!optional default)
  (cond
   ((hash-table? obj)
    (json-get name (list->vector (hash-table->alist obj)) default))
   ((vector? obj)
    (let iter ((pairs (vector->list obj)))
      (cond
       ((null? pairs) default)
       ((equal? (->string name) (->string (caar pairs))) (cdar pairs))
       (else (iter (cdr pairs))))))
   (else (error "Can't get, bad json structure" obj))))

(define (json-contains? name obj)
  (let* ((default (list #f))
         (res (json-get name obj default)))
    (not (eq? res default))))

(define (alist->json lst)
  (list->vector lst))

(define (json-delete key obj)
  (cond
   ((vector? obj)
    (list->vector
     (filter (lambda (pair) (not (eq? (->symbol (car pair)) (->symbol key))))
             (vector->list obj))))
   ((hash-table? obj) (json-delete key (list->vector (hash-table->alist obj))))
   (else (error "Can't delete, bad json structure" obj))))

(define (json-update key val obj)
  (alist->json (cons (cons key val) (json->alist (json-delete key obj)))))

(define (json-keys obj)
  (cond
   ((hash-table? obj) (json-keys (list->vector (hash-table->alist obj))))
   ((vector? obj) (map (compose ->symbol car) (vector->list obj)))
   (else (error "Not a table" obj))))

(define (set-equal? a b)
  (if (and (null? a) (null? b))
      #t
      (if (pair? a)
          (and (member (car a) b)
               (set-equal? (cdr a) (delete (car a) b)))
          #f)))

(define (json->alist json)
  (cond
   ((vector? json) (map (lambda (pair) (cons (->symbol (car pair)) (cdr pair)))
                        (vector->list json)))
   ((hash-table? json) (json->alist (list->vector (hash-table->alist json))))
   (else (error "Can't be an alist" json))))
