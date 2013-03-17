
(declare (unit scdtl))

(foreign-declare "

#include <time.h>

// allocates a calendar time on the heap
struct tm* scdtl_malloc_tm()
{
  struct tm* tm = malloc(sizeof(struct tm));
  return tm;
}

// returns the year of a calendar time
int scdtl_tm_year(struct tm* tm)
{
  return tm->tm_year;
}

// returns the month of a calendar time
int scdtl_tm_mon(struct tm* tm)
{
  return tm->tm_mon;
}

// returns the day of a calendar time
int scdtl_tm_mday(struct tm* tm)
{
  return tm->tm_mday;
}

// returns the hour of a calendar time
int scdtl_tm_hour(struct tm* tm)
{
  return tm->tm_hour;
}

// returns the minute of a calendar time
int scdtl_tm_min(struct tm* tm)
{
  return tm->tm_min;
}

// returns the second of a calendar time
int scdtl_tm_sec(struct tm* tm)
{
  return tm->tm_sec;
}

// frees the specified calendar time
void scdtl_free_tm(struct tm* tm)
{
  free(tm);
}

")

;; calendar time pointers definitions
(define-foreign-type scdtl-tm "struct tm")
(define-foreign-type scdtl-tm* (c-pointer scdtl-tm))

;; calendar time pointers memory management
(define scdtl-malloc-tm (foreign-lambda scdtl-tm* "scdtl_malloc_tm"))
(define scdtl-free-tm (foreign-lambda void "scdtl_free_tm" scdtl-tm*))

;; gets the parts of a calendar time
(define scdtl-tm-year (foreign-lambda int "scdtl_tm_year" scdtl-tm*))
(define scdtl-tm-mon (foreign-lambda int "scdtl_tm_mon" scdtl-tm*))
(define scdtl-tm-mday (foreign-lambda int "scdtl_tm_mday" scdtl-tm*))
(define scdtl-tm-hour (foreign-lambda int "scdtl_tm_hour" scdtl-tm*))
(define scdtl-tm-min (foreign-lambda int "scdtl_tm_min" scdtl-tm*))
(define scdtl-tm-sec (foreign-lambda int "scdtl_tm_sec" scdtl-tm*))

;; converts a string to calendar time
(define scdtl-strptime (foreign-lambda int "strptime" c-string c-string scdtl-tm*))
