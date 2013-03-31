
(declare (unit pcre))

(foreign-declare "

#include <pcre.h>

// wraps the pcre_compile function
// takes care of the error pointers
pcre* pcre_compile_wrapped(char* pattern)
{
  const char* error;
  int erroffset;

  pcre* pcre = pcre_compile(pattern, 0, &error, &erroffset, NULL);
  return pcre;
}

// the result of the wrapped pcre_exec function
struct pcre_exec_result_t
{
  int captures_count;
  char** captures;
};

// wraps the pcre_exec function
// returns a structure containing the captures
struct pcre_exec_result_t* pcre_exec_wrapped(pcre* pcre, char* subject)
{
  struct pcre_exec_result_t* pcre_exec_result = malloc(sizeof(struct pcre_exec_result_t));
  if (pcre_exec_result == NULL)
  {
    return NULL;
  }

  int ovector[30];
  int result = pcre_exec(pcre, NULL, subject, strlen(subject), 0, 0, ovector, 30);

  if (result < -1)
  {
    free(pcre_exec_result);
    return NULL;
  }

  if (result == -1)
  {
    pcre_exec_result->captures_count = 0;
    pcre_exec_result->captures = NULL;
    
    return pcre_exec_result;
  }

  int captures_count = result;

  pcre_exec_result->captures_count = captures_count;
  pcre_exec_result->captures = malloc(sizeof(char*) * captures_count);

  int capture_index;
  for (capture_index = 0; capture_index < captures_count; capture_index++)
  {
    const char** stringptr = (const char**)(pcre_exec_result->captures + capture_index);
    pcre_get_substring(subject, ovector, captures_count, capture_index, stringptr);
  }

  return pcre_exec_result;
}

// returns the captures count of a pcre_exec_result
int pcre_exec_result_captures_count(struct pcre_exec_result_t* pcre_exec_result)
{
  int captures_count = pcre_exec_result->captures_count;
  return captures_count;
}

// returns a capture of a pcre_exec_result
// allows scheme to take a copy of the string
char* pcre_exec_result_capture(struct pcre_exec_result_t* pcre_exec_result, int capture_index)
{
  char* capture = pcre_exec_result->captures[capture_index];
  return capture;
}

// frees a pcre_exec_result
// as well as the captures it points to
void free_pcre_exec_result(struct pcre_exec_result_t* pcre_exec_result)
{
  int capture_index;
  for (capture_index = 0; capture_index < pcre_exec_result->captures_count; capture_index++)
  {
    char* stringptr = pcre_exec_result->captures[capture_index];
    pcre_free(stringptr);
  }

  free(pcre_exec_result->captures);
  free(pcre_exec_result);
}

")

;; pcre pointers definitions
(define-foreign-type pcre "pcre")
(define-foreign-type pcre* (c-pointer pcre))

;; pcre-exec-result pointers definitions
(define-foreign-type pcre-exec-result "struct pcre_exec_result_t")
(define-foreign-type pcre-exec-result* (c-pointer pcre-exec-result))

;; compiles a regular expression into an internal form
(define pcre-compile (foreign-lambda pcre* "pcre_compile_wrapped" c-string))

;; matches a compiled regular expression against a given subject string
(define pcre-exec (foreign-lambda pcre-exec-result* "pcre_exec_wrapped" pcre* c-string))

;; returns the captures count of a pcre-exec-result
(define pcre-exec-result-captures-count (foreign-lambda int "pcre_exec_result_captures_count" pcre-exec-result*))

;; returns a capture of a pcre-exec-result
(define pcre-exec-result-capture (foreign-lambda c-string "pcre_exec_result_capture" pcre-exec-result* int))

;; frees a pcre-exec-result
(define free-pcre-exec-result (foreign-lambda void "free_pcre_exec_result" pcre-exec-result*))

;; frees a pcre
(define free-pcre (foreign-lambda void "pcre_free" pcre*))
