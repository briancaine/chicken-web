(use (srfi 19) srfi-19-period srfi-13)

(define (timezone-offset->string tz)
  (let* ((scaled (/ tz (* 60 60)))
         (positive (abs scaled)))
    (unless (integer? scaled) (error "Not an integer"))
    (string-append
     (if (negative? scaled) "-" "+")
     (string-pad-right (string-pad (->string positive) 2 #\0) 4 #\0))))

;; a timeframe is (start end) where start and end are utc times
;; including start, but not end

(define (make-timeframe start end) (list start end))
(define (timeframe-start x) (list-ref x 0))
(define (timeframe-end x) (list-ref x 1))

(define (coherent-timeframe? x)
  (time<? (timeframe-start x) (timeframe-end x)))

(define (ensure-coherent-timeframe a)
  (unless (coherent-timeframe? a)
          (error "Incoherent timeframe" a)))

(define (timeframes-dont-overlap? a b)
  (ensure-coherent-timeframe a)
  (ensure-coherent-timeframe b)
  (or (time<=? (timeframe-end a) (timeframe-start b))
      (time<=? (timeframe-end b) (timeframe-start a))))

(define (timeframes-overlap? a b)
  (not (timeframes-dont-overlap? a b)))

(define (timeframe-contains? tf time)
  (ensure-coherent-timeframe tf)
  (and (time<=? (timeframe-start tf) time)
       (time<? time (timeframe-end tf))))

(define update-date
  (let ((default (list #f)))
    (lambda (d #!key (nanosecond default)
                     (second default)
                     (minute default)
                     (hour default)
                     (day default)
                     (month default)
                     (year default)
                     (zone-offset default))
      (make-date
       (if (eq? nanosecond default)  (date-nanosecond d)  nanosecond)
       (if (eq? second default)      (date-second d)      second)
       (if (eq? minute default)      (date-minute d)      minute)
       (if (eq? hour default)        (date-hour d)        hour)
       (if (eq? day default)         (date-day d)         day)
       (if (eq? month default)       (date-month d)       month)
       (if (eq? year default)        (date-year d)        year)
       (if (eq? zone-offset default) (date-zone-offset d) zone-offset)))))

(define (make-calendar-date #!key month day year)
  (make-date 0 0 0 0 day month year))

(define (advance-date d seconds)
  (time-utc->date (add-duration (date->time-utc d)
                                (make-time time-duration 0 seconds))
                  (date-zone-offset d)))

(define (retreat-date d seconds)
  (time-utc->date (subtract-duration (date->time-utc d)
                                     (make-time time-duration 0 seconds))
                  (date-zone-offset d)))

(define (date-next-week d)
  (advance-date d (* 7 24 60 60)))

(define (date-previous-week d)
  (retreat-date d (* 7 24 60 60)))

(define (date-previous-day d)
  (retreat-date d (* 24 60 60)))

(define (date-next-day d)
  (advance-date d (* 24 60 60)))

(define (date-at-midnight d)
  (update-date d
               nanosecond: 0
               second: 0
               minute: 0
               hour: 0))

(define (date-at-midnight? d)
  (and (zero? (date-nanosecond d))
       (zero? (date-second d))
       (zero? (date-minute d))
       (zero? (date-hour d))))

(define (current-utc-time)
  (date->time-utc (current-date)))

(define (date-week-day/symbol d)
  (alist-ref (date-week-day d)
             '((0 . sun)
               (1 . mon)
               (2 . tue)
               (3 . wed)
               (4 . thu)
               (5 . fri)
               (6 . sat))))

(define (russian-date-week-day/symbol d)
  (alist-ref (date-week-day/symbol d)
             '((mon . pon)
               (tue . vt)
               (wed . sr)
               (thu . cht)
               (fri . pt)
               (sat . sb)
               (sun . vs))))

(define (current-timezone-offset)
  (date-zone-offset (current-date)))

(define (next-date-of day-symbol tz)
  (let iter ((date (date-at-midnight (current-date tz))))
    (if (eq? day-symbol (date-week-day/symbol date))
        date
        (iter (date-next-day date)))))

(define (next-dates-of day-symbol count)
  (if (zero? count)
      '()
      (let ((first (next-date-of day-symbol)))
        (unless (positive? count) (error "Can't have negative count" count))
        (let iter ((count (sub1 count))
                   (dates (list first)))
          (if (zero? count)
              (reverse dates)
              (iter (sub1 count) (cons (date-next-week (car dates)) dates)))))))

(define (robust-time->seconds tm)
  (+ (time-second tm)
     (/ (time-nanosecond tm) 1000000000)))

(define (time-modulo-duration time duration)
  (make-time time-utc 0
             (modulo (robust-time->seconds time)
                     (robust-time->seconds duration))))

(define (time->duration tm)
  (make-duration nanoseconds: (time-nanosecond tm)
                 seconds: (time-second tm)))

(define (clip-to-duration time duration)
  (subtract-duration time
                     (time->duration (time-modulo-duration time duration))))

(define (make-time-period/fix begin end . rest)
  (make-time-period
   begin
   (if (eq? time-duration (time-type end))
       (add-duration begin end)
       end)))

(define (make-date/keyword #!key (nanosecond 0)
                                 (second 0)
                                 (minute 0)
                                 hour day month (year 2015) zone-offset)
  (apply make-date nanosecond second minute hour day month year
         (if zone-offset
             (list zone-offset)
             '())))

(define (time-in-range? time range)
  (and (time<=? (car range) time)
       (time<?  time        (cadr range))))

(define (time-absolute-value t)
  (make-time (time-type t)
             (abs (time-nanosecond t))
             (abs (time-second t))))

(define (num->time num type)
  (let* ((seconds (inexact->exact (truncate num)))
         (nanoseconds (inexact->exact (truncate (* 1000000 (- num seconds))))))
    (make-time type nanoseconds seconds)))
