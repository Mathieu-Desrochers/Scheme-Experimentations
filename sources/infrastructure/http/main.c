
#include <chicken.h>
#include <fcgi_config.h>
#include <fcgi_stdio.h>
#include <stdlib.h>

extern void http_handle_request(FCGX_Request*);

int main()
{
  CHICKEN_run(C_toplevel);

  FCGX_Request* request = malloc(sizeof(FCGX_Request));

  FCGX_Init();
  FCGX_InitRequest(request, 0, 0);

  while (FCGX_Accept_r(request) == 0)
  {
    http_handle_request(request);
    FCGX_Finish_r(request);
  }

  FCGX_Free(request, 1);

  free(request);
}
