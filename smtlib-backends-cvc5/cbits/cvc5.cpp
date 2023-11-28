#include "cvc5.h"
#include <cvc5/cvc5.h>
#include <cvc5/cvc5_parser.h>

#include <iostream>
#include <cstring>

using namespace cvc5;
using namespace cvc5::parser;


extern "C"
{

struct cvc5_solver
{
    std::unique_ptr<Solver> ptr_;

    cvc5_solver(Solver *slv)
        : ptr_(slv)
    {
    };
};


struct cvc5_parser
{
    std::unique_ptr<InputParser> ptr_;

    cvc5_parser(InputParser *parser)
        : ptr_(parser)
    {
        parser->setIncrementalStringInput(modes::InputLanguage::SMT_LIB_2_6, "smtlib");
    };
};

cvc5_solver *cvc5_solver_init()
{
    auto slv = new Solver();
    return new cvc5_solver(slv);
}

void cvc5_solver_free(cvc5_solver *slv)
{
    delete slv;
}

cvc5_parser *cvc5_parser_init(cvc5_solver *slv)
{
    auto parser = new InputParser(slv->ptr_.get());
    return new cvc5_parser(parser);
}

void cvc5_parser_free(cvc5_parser *parser)
{
    delete parser;
}

void cvc5_solver_set_option(cvc5_solver *slv, char const *key, char const *val)
{
  slv->ptr_->setOption(key, val);
}


char *cvc5_eval_smtlib2_string(cvc5_solver *slv, cvc5_parser *parser, char const *in_str)
{
  auto os = std::ostringstream{};

  try
  {
    parser->ptr_->appendIncrementalStringInput(in_str);

    // get the symbol manager of the parser, used when invoking commands below
    SymbolManager* sm = parser->ptr_->getSymbolManager();

    // parse commands until finished
    Command cmd;

    while (true)
    {
      cmd = parser->ptr_->nextCommand();
      if (cmd.isNull())
      {
        break;
      }

      cmd.invoke(slv->ptr_.get(), sm, os);
    }
  }
  catch (CVC5ApiException& e)
  {
    os << "(error \"" << e.getMessage() << "\")" << std::endl;
  }
  // catch (ParserException& e)
  catch (...) 
  {
    os << "(error \"unknown parser exception\")" << std::endl;
  }
  
  auto str = os.str();

  // Include null terminator
  auto total_length = str.length() + 1;

  auto c_str = reinterpret_cast<char *>(malloc(total_length * sizeof(char)));
  std::strncpy(c_str, str.c_str(), total_length);

  return c_str;
}

}
