#ifndef CVC5_H
#define CVC5_H
#ifndef __cplusplus
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#else
#include <cstddef>
#include <cstdint>
#endif


#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct cvc5_solver cvc5_solver;

    typedef struct cvc5_parser cvc5_parser;

    cvc5_solver *cvc5_solver_init();

    void cvc5_solver_free(cvc5_solver *);

    void cvc5_solver_set_option(cvc5_solver *, char const *, char const *);

    cvc5_parser *cvc5_parser_init(cvc5_solver *);

    void cvc5_parser_free(cvc5_parser *);

    char *cvc5_eval_smtlib2_string(cvc5_solver *, cvc5_parser *, char const *);

#ifdef __cplusplus
}
#endif

#endif