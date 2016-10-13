(use intarweb)

;; some useful functions...
(define (read-string/bytes #!optional num port)
  (with-input-from-port
      (or port (current-input-port))
    (lambda ()
      (if num
          (blob->string (u8vector->blob (read-u8vector num)))
          (read-string)))))

(define (read-post-data req)
  (with-input-from-port
      (request-port req)
    (lambda ()
      (read-string/bytes
       (header-value 'content-length (request-headers req) 0)))))
