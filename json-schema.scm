(use uri-common)

;; not promising to be conformant
;; though bugs against the implementation are OK

;; https://spacetelescope.github.io/understanding-json-schema/

(define +schema-version+ "http://json-schema.org/schema#")
(define +schema-declaration+ `#(("$schema" . ,+schema-version+)))

;; ok, so, the process for validating a json scheme involves:
;; * expanding all the $refs
;; * validating

;; to expand all the refs, we first check to see if our current item
;; has $ref

;; we also need to know our current path

(define (json-schema-ref schema path)
  (and-let* ((path-uri (uri-reference path))
             (frag (uri-fragment path-uri))
             (frag-uri (uri-reference frag)))
    (unless (null? (uri-path path-uri))
      (error "References in other files aren't supported yet" path))
    (let iter ((path (cdr (uri-path frag-uri)))
               (schema schema))
      (if (null? path)
          schema
          (iter (cdr path)
                (json-get (car path) schema))))))

(define (vector-cdr vect)
  (list->vector (cdr (vector->list vect))))

(define (json-merge new-fields old-fields)
  (if (zero? (vector-length new-fields))
      old-fields
      (let ((next (vector-ref new-fields 0)))
        (json-merge
         (vector-cdr new-fields)
         (json-update (car next)
                      (cdr next)
                      old-fields)))))

(define (int-expand-json-schema root schema)
  (define (expand-list-schemas key)
    (if (json-contains? key schema)
        `#((,key . ,(map (lambda (item) (int-expand-json-schema root item))
                         (json-get key schema))))
        '#()))
  ;; so immediately, if our current schema has any $refs, we resolve those first
  (cond
   ((and (vector? schema) (json-get '$ref schema)) =>
    (lambda (ref)
      (unless (string? ref)
        (error "ref isn't a string" root schema ref))
      (let ((ref-val (json-schema-ref root ref)))
        (unless ref-val
          (error "Failed to resolve reference" root schema ref))
        (unless (vector? ref-val)
          (error "reference wrong type" root schema ref ref-val))
        (int-expand-json-schema
         root
         (vector-append ref-val (json-delete '$ref schema))))))
   ;; potential reference containing children
   ((vector? schema)
    (json-merge
     (vector-append
      (if (json-contains? 'properties schema)
          `#((properties .
              ,(vector-map
                (lambda (idx pair)
                  `(,(car pair) . ,(int-expand-json-schema root (cdr pair))))
                (json-get 'properties schema))))
          '#())
      (if (json-contains? 'items schema)
          (let ((items (json-get 'items schema)))
            (cond
             ((list? items)
              `#((items .
                  ,(map (lambda (item) (int-expand-json-schema root item))
                        items))))
             ((vector? items)
              `#((items . ,(int-expand-json-schema root items))))
             (else (error "Bad items value" root schema items))))
          '#())
      (expand-list-schemas 'anyOf)
      (expand-list-schemas 'allOf)
      (expand-list-schemas 'oneOf)
      (if (json-contains? 'not schema)
          `#((not . ,(int-expand-json-schema root (json-get 'not schema))))
          '#()))
     schema))
   (else schema)))

;; expands all the refs
(define (expand-json-schema schema)
  ;; so immediately, if our current schema has any 
  (int-expand-json-schema schema schema))

;; validation is a pretty simple tree search

(define (correct-type? type json)
  (cond
   ((equal? type "string") (string? json))
   ((equal? type "integer") (integer? json))
   ((equal? type "number") (number? json))
   ((equal? type "object") (vector? json))
   ((equal? type "array") (list? json))
   ((equal? type "boolean") (boolean? json))
   ((equal? type "null") (void? json))
   ((list? type) (any (lambda (type) (correct-type? type json)) type))
   (else (error "Unknown type" type json))))

(define (internal-validate schema json)
  (cond
   ;; basic type check
   ((json-get 'type schema) =>
    (lambda (type)
      (let ((schema (json-delete 'type schema)))
        (and (correct-type? type json)
             (internal-validate schema json)))))
   ;; generic keywords
   ((json-get 'allOf schema) =>
    (lambda (all-of)
      (let ((schema (json-delete 'allOf schema)))
        (unless (list? all-of)
          (error "Bad schema, allOf should be a list" all-of schema json))
        (and (every (lambda (matcher) (internal-validate matcher json))
                    all-of)
             (internal-validate schema json)))))
   ((json-get 'anyOf schema) =>
    (lambda (any-of)
      (let ((schema (json-delete 'anyOf schema)))
        (unless (list? any-of)
          (error "Bad schema, anyOf should be a list" any-of schema json))
        (and (any (lambda (matcher) (internal-validate matcher json))
                  any-of)
             (internal-validate schema json)))))
   ((json-get 'oneOf schema) =>
    (lambda (one-of)
      (let ((schema (json-delete 'oneOf schema)))
        (unless (list? one-of)
          (error "Bad schema, oneOf should be a list" one-of schema json))
        (let iter ((one-of one-of))
          (if (null? one-of)
              #f
              (if (internal-validate (car one-of) json)
                  (and (every (lambda (matcher) (not (internal-validate matcher json)))
                              (cdr one-of))
                       (internal-validate schema json))
                  (iter (cdr one-of))))))))
   ((json-get 'not schema) =>
    (lambda (not-matcher)
      (let ((schema (json-delete 'not schema)))
        (and (not (internal-validate not-matcher json))
             (internal-validate schema json)))))
   ((json-get 'enum schema) =>
    (lambda (enum)
      (let ((schema (json-delete 'enum schema)))
        (unless (list? enum) (error "Invalid enum" enum))
        (and (any (lambda (current-enum)
                    (equal? (json-normalize current-enum)
                            (json-normalize json)))
                  enum)
             (internal-validate schema json)))))
   ;; string keywords
   ((json-get 'minLength schema) =>
    (lambda (min-length)
      (unless (integer? min-length)
        (error "Bad schema, minLength has to be an integer" min-length schema json))
      (let ((schema (json-delete 'minLength schema)))
        (and (string? json)
             (>= (string-length json) min-length)
             (internal-validate schema json)))))
   ((json-get 'maxLength schema) =>
    (lambda (max-length)
      (unless (integer? max-length)
        (error "Bad schema, maxLength has to be an integer" max-length schema json))
      (let ((schema (json-delete 'maxLength schema)))
        (and (string? json)
             (<= (string-length json) max-length)
             (internal-validate schema json)))))
   ;; number keywords
   ((json-get 'multipleOf schema) =>
    (lambda (multiple)
      (let ((schema (json-delete 'multipleOf schema)))
        (printf "WARNING: multipleOf isn't implemented\n")
        (internal-validate schema json))))
   ((json-get 'minimum schema) =>
    (lambda (minimum)
      (let ((schema (json-delete 'minimum schema)))
        (printf "WARNING: minimum isn't implemented\n")
        (internal-validate schema json))))
   ((json-get 'maximum schema) =>
    (lambda (maximum)
      (let ((schema (json-delete 'maximum schema)))
        (printf "WARNING: maximum isn't implemented\n")
        (internal-validate schema json))))
   ((json-get 'exclusiveMinimum schema) =>
    (lambda (exclusive-minimum)
      (let ((schema (json-delete 'exclusiveMinimum schema)))
        (printf "WARNING: exclusiveMinimum isn't implemented\n")
        (internal-validate schema json))))
   ((json-get 'exclusiveMaximum schema) =>
    (lambda (exclusive-maximum)
      (let ((schema (json-delete 'exclusiveMaximum schema)))
        (printf "WARNING: exclusiveMaximum isn't implemented\n")
        (internal-validate schema json))))
   ;; object keywords
   ((json-get 'properties schema) =>
    (lambda (properties)
      (let ((schema (json-delete 'properties schema)))
        (unless (vector? properties)
          (error "Invalid schema, properties is supposed to be a vector"
                 properties schema json))
        (and (every
              (lambda (pair)
                (let ((name (car pair))
                      (matcher (cdr pair)))
                  (if (json-contains? name json)
                      (internal-validate matcher (json-get name json))
                      #t)))
              (vector->list properties))
             (if (and (json-contains? 'additionalProperties schema)
                      (not (json-get 'additionalProperties schema)))
                 (equal? (json-keys json) (json-keys properties))
                 #t)
             (internal-validate schema json)))))
   ((json-get 'required schema) =>
    (lambda (required)
      (let ((schema (json-delete 'required schema)))
        (unless (list? required)
          (error "Bad schema, required is supposed to be a list"
                 required schema json))
        (and (every
              (lambda (key) (json-contains? key json))
              required)
             (internal-validate schema json)))))
   ((json-get 'minProperties schema) =>
    (lambda (min-properties)
      (let ((schema (json-delete 'minProperties schema)))
        (printf "WARNING: minProperties isn't implemented\n")
        (internal-validate schema json))))
   ((json-get 'maxProperties schema) =>
    (lambda (max-properties)
      (let ((schema (json-delete 'maxProperties schema)))
        (printf "WARNING: maxProperties isn't implemented\n")
        (internal-validate schema json))))
   ((json-get 'dependencies schema) =>
    (lambda (dependencies)
      (let ((schema (json-delete 'dependencies schema)))
        (printf "WARNING: dependencies isn't implemented\n")
        (internal-validate schema json))))
   ((json-get 'patternProperties schema) =>
    (lambda (pattern-properties)
      (let ((schema (json-delete 'patternProperties schema)))
        (printf "WARNING: patternProperties isn't implemented\n")
        (internal-validate schema json))))
   ;; array properties
   ((json-get 'items schema) =>
    (lambda (items)
      (let ((schema (json-delete 'items schema)))
        (and (list? json)
             (cond
              ((vector? items)
               (every (lambda (element) (internal-validate items element))
                      json))
              ((list? items)
               (every internal-validate
                      items
                      json))
              (else (error "Bad schema, items should be a vector or list"
                           items schema json)))
             (internal-validate schema json)))))
   ((json-get 'minItems schema) =>
    (lambda (min-items)
      (unless (integer? min-items)
        (error "Bad schema, minItems has to be an integer" min-items schema json))
      (let ((schema (json-delete 'minItems schema)))
        (and (list? json)
             (>= (length json) min-items)
             (internal-validate schema json)))))
   ((json-get 'maxItems schema) =>
    (lambda (max-items)
      (unless (integer? max-items)
        (error "Bad schema, maxItems has to be an integer" max-items schema json))
      (let ((schema (json-delete 'maxItems schema)))
        (and (list? json)
             (<= (length json) max-items)
             (internal-validate schema json)))))
   ((json-get 'uniqueItems schema) =>
    (lambda (unique-items)
      (let ((schema (json-delete 'uniqueItems schema)))
        (printf "WARNING: uniqueItems isn't implemented\n")
        (internal-validate schema json))))
   (else
    #t)))

(define (internal-root-validate schema json)
  (unless (and (vector? schema)
               (equal? +schema-version+ (json-get '$schema schema)))
    (error "Not a schema" schema))
  (internal-validate schema json))

(define (json-validates? schema json
                         #!key (suffix '#()))
  (internal-root-validate
   (json-normalize (expand-json-schema
                    (vector-append (json-normalize schema)
                                   (json-normalize suffix))))
   (json-normalize json)))
