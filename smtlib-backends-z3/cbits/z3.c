#include "z3.h"

// This prototype causes compilation to fail if the type
// of Z3_eval_smtlib2_string changes.
const char* Z3_eval_smtlib2_string(Z3_context ctx, const char* cmd);
