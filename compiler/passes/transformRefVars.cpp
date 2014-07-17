#include "passes.h"

#include "astutil.h"
#include "baseAST.h"
#include "bb.h"
#include "expr.h"


//new CallExpr(PRIM_MOVE, var, new CallExpr(PRIM_ADDR_OF, varLocation))
void transformRefVars() {
  forv_Vec(FnSymbol, fn, gFnSymbols) {
    buildBasicBlocks(fn);

    for_vector(BasicBlock, bb, *fn->basicBlocks) {
      for_vector(Expr, expr, bb->exprs) {
        if (CallExpr* call = toCallExpr(expr)) {
          if (call->isPrimitive(PRIM_CREATE_REF)) {
            Expr* lhs = call->get(1);
            //Expr* rhs = call->get(2);

            SymExpr* sym = toSymExpr(lhs);
            INT_ASSERT(sym);
            if (sym->var->type->refType)
              sym->var->type = sym->var->type->refType;

            //SET_LINENO(call);
            ////VarSymbol* tmp = newTemp("ref_var_temp");
            ////call->insertBefore(
            ////    new CallExpr(PRIM_MOVE, tmp, new CallExpr(PRIM_ADDR_OF, rhs)));
            ////call->replace(new CallExpr(PRIM_MOVE, lhs, tmp));
            //call->replace(
            //    new CallExpr(PRIM_MOVE, lhs, new CallExpr(PRIM_ADDR_OF, rhs)));
          }
        }
      }
    }
  }
}
