
(declare (unit hungarian))

(foreign-declare "

#include <hungarian.h>

// allocates a hungarian_problem on the heap
hungarian_problem_t* malloc_hungarian_problem()
{
  hungarian_problem_t* hungarian_problem = malloc(sizeof(hungarian_problem_t));
  return hungarian_problem;
}

// returns an assignment value from a solved hungarian_problem
int hungarian_problem_assignment_matrix_value(hungarian_problem_t* hungarian_problem, int row_index, int column_index)
{
  return hungarian_problem->assignment[row_index][column_index];
}

// frees a hungarian_problem
void free_hungarian_problem(hungarian_problem_t* hungarian_problem)
{
  hungarian_free(hungarian_problem);
}

// allocates a hungarian_cost_matrix on the heap
int*** malloc_hungarian_cost_matrix(int rows, int columns)
{
  int*** hungarian_cost_matrix = malloc(sizeof(int**));

  *hungarian_cost_matrix = malloc(sizeof(int*) * rows);

  int i;

  for (i = 0; i < rows; i++)
  {
    (*hungarian_cost_matrix)[i] = malloc(sizeof(int) * columns);
  }

  return hungarian_cost_matrix;
}

// sets a hungarian_cost_matrix value
void hungarian_cost_matrix_value_set(int*** hungarian_cost_matrix, int row_index, int column_index, int value)
{
  (*hungarian_cost_matrix)[row_index][column_index] = value;
}

// indirects a hungarian_cost_matrix
int** indirect_hungarian_cost_matrix(int*** hungarian_cost_matrix)
{
  return *hungarian_cost_matrix;
}

// frees a hungarian_cost_matrix
// already done in part by hungarian_free
void free_hungarian_cost_matrix(int*** hungarian_cost_matrix)
{
  free(hungarian_cost_matrix);
}

")

;; hungarian-problem pointers definitions
(define-foreign-type hungarian-problem "hungarian_problem_t")
(define-foreign-type hungarian-problem* (c-pointer hungarian-problem))

;; hungarian-cost-matrix pointers definitions
(define-foreign-type hungarian-cost-matrix** (c-pointer (c-pointer int)))
(define-foreign-type hungarian-cost-matrix*** (c-pointer hungarian-cost-matrix**))

;; hungarian-assignment-matrix pointers definitions
(define-foreign-type hungarian-assignment-matrix** (c-pointer (c-pointer int)))
(define-foreign-type hungarian-assignment-matrix*** (c-pointer hungarian-cost-matrix**))

;; hungarian-problem pointers memory management
(define malloc-hungarian-problem (foreign-lambda hungarian-problem* "malloc_hungarian_problem"))
(define free-hungarian-problem (foreign-lambda void "free_hungarian_problem" hungarian-problem*))

;; hungarian-cost-matrix memory management
(define malloc-hungarian-cost-matrix** (foreign-lambda hungarian-cost-matrix*** "malloc_hungarian_cost_matrix" int int))
(define indirect-hungarian-cost-matrix*** (foreign-lambda hungarian-cost-matrix** "indirect_hungarian_cost_matrix" hungarian-cost-matrix***))
(define free-hungarian-cost-matrix** (foreign-lambda void "free_hungarian_cost_matrix" hungarian-cost-matrix***))

;; sets a hungarian-cost-matrix value
(define hungarian-cost-matrix-value-set! (foreign-lambda void "hungarian_cost_matrix_value_set" hungarian-cost-matrix*** int int int))

;; initializes a hungarian problem
(define hungarian-init (foreign-lambda int "hungarian_init" hungarian-problem* hungarian-cost-matrix** int int int))

;; solves a hungarian problem
(define hungarian-solve (foreign-lambda void "hungarian_solve" hungarian-problem*))

;; returns an assignment value from a solved hungarian_problem
(define hungarian-problem-assignment-matrix-value (foreign-lambda int "hungarian_problem_assignment_matrix_value" hungarian-problem* int int))

;; frees a hungarian problem
(define hungarian-free (foreign-lambda void "hungarian_free" hungarian-problem*))

;; hungarian modes
(define hungarian-mode-minimize-cost (foreign-value "HUNGARIAN_MODE_MINIMIZE_COST" int))
(define hungarian-mode-maximize-util (foreign-value "HUNGARIAN_MODE_MAXIMIZE_UTIL" int))

;; hungarian states
(define hungarian-not-assigned (foreign-value "HUNGARIAN_NOT_ASSIGNED" int))
(define hungarian-assigned (foreign-value "HUNGARIAN_ASSIGNED" int))
