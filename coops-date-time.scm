(use (srfi 19) srfi-19-period coops)

(define-primitive-class <date> date?)
(define-primitive-class <time> time?)
(define-primitive-class <time-period> time-period?)

(define-primitive-class <calendar-date> (<date>)
  (lambda (x)
    (and (date? x)
         (date-at-midnight? x))))

(define-primitive-class <time-duration> (<time>)
  (lambda (x)
    (and (time? x)
         (eq? (time-type x) time-duration))))

(define-primitive-class <time-utc> (<time>)
  (lambda (x)
    (and (time? x)
         (eq? (time-type x) time-utc))))

(define-method (to-json (item <time>))
  `#((type . <time>)
     (time-type . ,(time-type item))
     (seconds . ,(time-second item))
     (nanoseconds . ,(time-nanosecond item))))

(define-method (to-json (item <calendar-date>))
  `#((type . <calendar-date>)
     (day . ,(date-day item))
     (month . ,(date-month item))
     (year . ,(date-year item))))

(define-method (from-json/complex (json #t) (_ <time>))
  (let ((type (->symbol (json-get 'time-type json))))
    (unless (member type (list time-duration time-monotonic time-process
                               time-tai time-thread time-utc))
            (error "Bad time type" type))
    (make-time type (json-get 'nanoseconds json) (json-get 'seconds json))))

(define-method (from-json/complex (json #t) (_ <calendar-date>))
  (make-calendar-date month: (inexact->exact (json-get 'month json))
                      day: (inexact->exact (json-get 'day json))
                      year: (inexact->exact (json-get 'year json))))

(define-method (to-json (item <time-period>))
  `#((type . <time-period>)
     (begin . ,(to-json (time-period-begin item)))
     (end . ,(to-json (time-period-end item)))))

(define-method (from-json/complex (json #t) (_ <time-period>))
  (make-time-period (from-json (json-get 'begin json))
                    (from-json (json-get 'end json))))
