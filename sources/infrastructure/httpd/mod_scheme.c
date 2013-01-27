
#include <chicken.h>

#include <httpd.h>
#include <http_core.h>
#include <http_protocol.h>
#include <http_request.h>

// forward declarations
static void register_hooks(apr_pool_t *pool);
static int scheme_handler(request_rec *r);

// declare the module
module AP_MODULE_DECLARE_DATA scheme_module =
{
    STANDARD20_MODULE_STUFF,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    register_hooks
};

// register the module hooks
static void register_hooks(apr_pool_t* pool) 
{
	// start the chicken runtime
	CHICKEN_run(C_toplevel);
    
    // register the scheme handler
    ap_hook_handler(scheme_handler, NULL, NULL, APR_HOOK_LAST);
}

// the scheme handler
static int scheme_handler(request_rec* r)
{
    ap_rputs("Hello, world!<br/>", r);
    return OK;
}

// external declarations
extern void httpd_handle_request(request_rec* r);

// enslaves the calling scheme thread
// and use it to serve http requests
void httpd_enslave_scheme_thread()
{
}
