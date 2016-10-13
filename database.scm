(use couchdb http-client intarweb)
(use coops coops-utils json vector-lib uri-common)

(define-syntax alist-or-nothing
  (syntax-rules ()
    ((alist-or-nothing name)
     (if name
         (list (cons 'name name))
         '()))))

(define database-connection (make-parameter #f))

(define (ensure-database!)
  (or (get-database-info (database-connection))
      (create-database (database-connection))))

(define (409-response-condition? exn)
  (and-let* ((_ (condition? exn))
             (response ((condition-property-accessor 'client-error 'response)
                        exn)))
    (= 409 (response-code response))))

(define (something->class-name x)
  (cond
   ((symbol? x) x)
   ((string? x) (->symbol x))
   ((class? x) (something->class-name (class-name x)))
   (else (error "Unknown object" x))))

(define (trim-class-name x)
  (let ((str (->string (something->class-name x))))
    (->symbol (substring str 1 (sub1 (string-length str))))))

(define (untrim-class-name x)
  (symbol-append '< (->symbol x) '>))

(define *views* '())

(define (can-merge-views? a b)
  (equal? (document-id a) (document-id b)))

(define (add-view-document! doc)
  (if (any (lambda (v) (can-merge-views? doc v)) *views*)
      (set! *views*
            (map (lambda (x)
                   (if (can-merge-views? x doc) (merge-view-documents x doc) x))
                 *views*))
      (set! *views* (cons doc *views*))))

(define (view-function-equal? a b)
  (and (equal? (json-get 'map a)    (json-get 'map b))
       (equal? (json-get 'reduce a) (json-get 'reduce b))))

(define (view-document-equal? a b)
  (and-let* ((_ (not (and a (not b))))
             (_ (not (and b (not a))))
             (a-body (json-normalize (document-body a)))
             (b-body (json-normalize (document-body b)))

             (a-views (json-get 'views a-body))
             (b-views (json-get 'views b-body))
             )
    (and (equal? (json-get 'language a-body) (json-get 'language b-body))
         (equal? (json-keys a-views) (json-keys b-views))
         (every (lambda (a-key)
                  (view-function-equal? (json-get a-key a-views)
                                        (json-get a-key b-views)))
                (json-keys a-views))
         #t)))

(define (forcibly-save-document connection document)
  (let ((existing (get-document connection (document-id document))))
    (handle-exceptions
        exn
        (if (409-response-condition? exn)
            (forcibly-save-document connection document)
            (abort exn))
      (if (view-document-equal? existing document)
          #t
          (save-document connection
                         (if existing
                             (update-document document rev: (document-rev existing))
                             document))))))

(define (json->string json)
  (with-output-to-string (lambda () (json-write json))))

(define (make-view-document #!key name (language 'javascript)
                                  (views '()))
  (make-document id: (format "_design/~a" name)
                 body: `#((language . ,language)
                          (views . ,(list->vector views)))))

(define (merge-view-documents a . rest)
  (define (merge a b)
    (let ((a-lang (json-get 'language (document-body a))))
      (unless (equal? (document-id a) (document-id b))
              (error "ID mismatch"))
      (unless (equal? a-lang (json-get 'language (document-body b)))
              (error "Language mismatch"))
      (update-document a
                       body: `#((language . ,a-lang)
                                (views .
                                 ,(vector-append
                                   (json-get 'views (document-body a))
                                   (json-get 'views (document-body b))))))))
  (fold merge a rest))

(define (with-map-view-func class name code func)
  (let* ((trimmed-class-name (->string (trim-class-name class)))
         (name (->string name))
         (view (make-view-document name: trimmed-class-name
                                   views: `((,name . #((map . ,code)))))))
    (add-view-document! view)
    (lambda args
      (apply func trimmed-class-name name args))))

(define (with-map-reduce-view-func class name map-code reduce-code func)
  (let* ((trimmed-class-name (->string (trim-class-name class)))
         (name (->string name))
         (view (make-view-document
                name: trimmed-class-name
                views: `((,name . #((map . ,map-code)
                                    (reduce . ,reduce-code)))))))
    (add-view-document! view)
    (lambda args
      (apply func trimmed-class-name name args))))

(define (ensure-view! doc)
  (forcibly-save-document (database-connection) doc))

(define (ensure-views!)
  (or (and (every ensure-view! *views*) #t)
      (error "Failed to ensure every view")))

(define (raw-get-view-values conn view-name params)
  (parameterize ((form-urlencoded-separator "&;"))
    (with-input-from-request
     (let ((uri (connection-uri conn)))
       (update-uri uri
                   path: (append
                          (uri-path uri)
                          (list "_design" (car view-name)
                                "_view" (cadr view-name)))
                   query: (map (lambda (pair)
                                 (cons (->string (car pair))
                                       (->string (cdr pair))))
                               params)))
     #f
     json-read)))

(define (raw-get-view conn view-name params)
  (call-with-values
      (lambda () (raw-get-view-values conn view-name params))
    (lambda (a . rest) a)))

(define (get-view-rows view-name view-member-name params)
  (json-get 'rows (get-view (database-connection)
                            (list view-name view-member-name)
                            params)))

(define (get-view-alist view-name view-member-name #!optional (params '()))
  (map (lambda (x) (cons (json-get 'key x) (json-get 'value x)))
       (get-view-rows view-name view-member-name params)))
