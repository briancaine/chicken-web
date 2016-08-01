(use coops coops-primitive-objects couchdb)

(define-class <couchdb-document> ()
  ((document accessor: document initform: (make-document body: '#()))))

(define-method (type (obj <couchdb-document>))
  (document-ref obj 'type))
(define-method ((setter type) (obj <couchdb-document>) (val #t))
  (set! (document-ref obj 'type) val))

(define-method (initialize-instance (obj <couchdb-document>))
  (call-next-method)
  (set! (type obj)                           (type obj)))

(define-method (db-id (doc <couchdb-document>))
  (document-id (document doc)))

(define-method (db-rev (doc <couchdb-document>))
  (document-rev (document doc)))

(define (json->document json)
  (make-document id: (json-get '_id json)
                 rev: (json-get '_rev json)
                 body: (json-delete '_rev (json-delete '_id json))))

(define (get-view-rows-single-conv-doc view-name view-member-name key)
  (let ((rows (get-view-rows view-name view-member-name
                             `((key . ,key) (include_docs . #t)))))
    (when (> (length rows) 1)
      (printf "WARNING: single row view returned multiple rows: ~s ~s ~s\n"
              view-name view-member-name key))
    (and (pair? rows)
         (document->object/error (json-get 'doc (car rows))))))

(define-method (document-contains? (doc <couchdb-document>) (key #t))
  (let ((key (->string key)))
    (or (and (member key '("_id" "_rev")) #t)
        (json-contains? key (document-body (document doc))))))

(define-method (document-ref (doc <couchdb-document>) (key #t))
  (let ((key (->string key)))
    (cond
     ((equal? key "_id")  (document-id (document doc)))
     ((equal? key "_rev") (document-rev (document doc)))
     (else                (json-get key (document-body (document doc)))))))

(define-method ((setter document-ref) (doc <couchdb-document>) (key #t) (val #t))
  (let ((key (->string key)))
    (cond
     ((equal? key "_id")
      (set! (document doc) (update-document (document doc) id: val)))
     ((equal? key "_rev")
      (set! (document doc) (update-document (document doc) rev: val)))
     (else
      (set! (document doc)
        (update-document
         (document doc)
         body: (json-update key val
                            (document-body (document doc)))))))))

(define-method (couchdb-document? (obj #t)) #f)
(define-method (couchdb-document? (obj <couchdb-document>)) #t)

(define (->class name)
  (and-let* ((type-sym (->symbol name))
             (class (handle-exceptions
                        exn
                        (if (unbound-variable-exn? exn)
                            #f
                            (abort exn))
                      (eval type-sym))))
    class))

(define (document->object/error doc)
  (unless doc (error "No such document"))
  (or (and-let* ((doc doc)
                 (doc-obj (make <couchdb-document> 'document doc))
                 (type-sym (->symbol (document-ref doc-obj 'type)))
                 (class (handle-exceptions
                            exn
                            (if (unbound-variable-exn? exn)
                                #f
                                (abort exn))
                          (eval type-sym))))
        (make class 'document doc))
      (error "Couldn't identify object type" doc (and doc (document-id doc)))))

;; some macros

(define-syntax define-document-getter
  (syntax-rules ()
    ((define-document-getter type (entry-name entry-type))
     (define-document-getter type (entry-name entry-type) #f))
    ((define-document-getter type (entry-name entry-type) default)
     (define-method (entry-name (obj type))
       (if (document-contains? obj 'entry-name)
           (from-json (document-ref obj 'entry-name))
           default)))))

(define-syntax define-document-setter
  (syntax-rules ()
    ((define-document-setter type (entry-name entry-type))
     (define-document-setter type (entry-name entry-type) #f))
    ((define-document-setter type (entry-name entry-type) default)
     (define-method ((setter entry-name) (obj type) (val entry-type))
       (set! (document-ref obj 'entry-name) (to-json val))))))

(define-syntax define-document-accessor
  (syntax-rules ()
    ((define-document-accessor type (entry-name entry-type))
     (define-document-accessor type (entry-name entry-type) #f))
    ((define-document-accessor type (entry-name entry-type) default)
     (begin
       (define-document-getter type (entry-name entry-type) default)
       (define-document-setter type (entry-name entry-type) default)))))

;; and just general json backed objects

(define-class <json-document> ()
  ((json-data accessor: json-data initform: '#())))

(define-method (initialize-instance (obj <json-document>))
  (call-next-method)
  (set! (type obj)                           (type obj)))

(define-method (document-contains? (doc <json-document>) (key #t))
  (let ((key (->string key)))
    (json-contains? key (json-data doc))))

(define-method (document-ref (doc <json-document>) (key #t))
  (let ((key (->string key)))
    (json-get key (json-data doc))))

(define-method ((setter document-ref) (doc <json-document>) (key #t) (val #t))
  (let ((key (->string key)))
    (set! (json-data doc) (json-update key val (json-data doc)))))

;; don't rely on these too extensively
(define-method (to-json (item #t)) item)
(define-method (from-json (item #t)) item)

(define-method (from-json (item <hash-table>))
  (from-json (list->vector (hash-table->alist item))))

(define (unbound-variable-exn? exn)
  (and (condition? exn)
       (equal? "unbound variable"
               ((condition-property-accessor 'exn 'message) exn))))

(define-method (from-json/complex (item #t) (obj #t))
  obj)

(define-method (from-json/complex (item #t) (obj <json-document>))
  (set! (json-data obj) item)
  obj)

(define-method (from-json/complex (item <vector>) (obj <couchdb-document>))
  (set! (document obj) (json->document item))
  obj)

(define-method (from-json (item <vector>))
  (or (and-let* ((type-name (json-get 'type item))
                 (type-var (handle-exceptions
                               exn
                               (if (unbound-variable-exn? exn)
                                   #f
                                   (abort exn))
                               (eval (->symbol type-name)))))
        (from-json/complex item (make type-var)))
      (call-next-method)))

(define-method (from-json (item <list>))
  (map from-json item))

(define-method (to-json (item <list>))
  (map to-json item))

(define-method (to-json (item <vector>))
  (list->vector
   (map (lambda (pair) (cons (to-json (car pair)) (to-json (cdr pair))))
        (vector->list item))))

(define-method (to-json (item <json-document>))
  (json-data item))

(define-method (to-json (item <couchdb-document>))
  (list->vector
   `((_id .  ,(document-id (document item)))
     (_rev . ,(document-rev (document item)))
     ,@(if (hash-table? (document-body (document item)))
           (hash-table->alist (document-body (document item)))
           (vector->list (document-body (document item)))))))

;; getting and setting from the database

(define (get-couchdb-document id)
  (document->object/error (and (not (string-null? id))
                               (get-document (database-connection) id))))

(define (get-couchdb-document/false id)
  (and-let* ((_ (not (string-null? id)))
             (doc (get-document (database-connection) id)))
    (document->object/error doc)))

(define (save-couchdb-document! doc)
  (let ((new-obj (document->object/error (save-document (database-connection) (document doc)))))
    (set! (db-id doc) (db-id new-obj))
    (set! (db-rev doc) (db-rev new-obj))
    new-obj))

(define schema-json-source
  (with-input-from-file "web/schema.json" json-read))

(define examine-json-validation? (make-parameter #f))

(define-method (schema-definition-ref (obj #t))
  (format "#/definitions/~a" (trim-class-name (class-of obj))))

(define-method (jsonable-validates? (obj #t))
  (let ((json (to-json obj))
        (schema (json-update
                 '$ref
                 (format "#/definitions/~a" (trim-class-name (class-of obj)))
                 schema-json-source)))
    (when (examine-json-validation?)
      (with-output-to-file "/tmp/test_json.json"
        (lambda () (json-pretty-print json)))
      (with-output-to-file "/tmp/test_schema.json"
        (lambda () (json-pretty-print schema))))
    (json-validates? schema json)))

