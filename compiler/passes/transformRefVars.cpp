#include "passes.h"

#include "astutil.h"
#include "baseAST.h"
#include "bb.h"
#include "expr.h"

static bool shouldDerefForCall(CallExpr* call, Expr* actual);
static void derefRefVarsForUse(CallExpr* call, CallExpr* outer);

//new CallExpr(PRIM_MOVE, var, new CallExpr(PRIM_ADDR_OF, varLocation))
void transformRefVars() {
  // Just iterating over gCallExprs would cause us to loop forever
  forv_Vec(FnSymbol, fn, gFnSymbols) {
    buildBasicBlocks(fn);

    for_vector(BasicBlock, bb, *fn->basicBlocks) {
      for_vector(Expr, expr, bb->exprs) {
        if (CallExpr* call = toCallExpr(expr)) {
          if (call->isPrimitive(PRIM_CREATE_REF)) {
            Expr* lhs = call->get(1)->remove();
            Expr* rhs = call->get(1)->remove();

            SymExpr* sym = toSymExpr(lhs);
            INT_ASSERT(sym);
            //if (sym->var->type->refType)
            //  sym->var->type = sym->var->type->refType;

            SET_LINENO(call);
            if (!rhs->typeInfo()->symbol->hasFlag(FLAG_REF)) {
              CallExpr* addrOf = new CallExpr(PRIM_ADDR_OF, rhs);
              sym->var->type = addrOf->typeInfo();
              call->replace(new CallExpr(PRIM_CREATE_REF, lhs, addrOf));
            } else {
              call->replace(new CallExpr(PRIM_CREATE_REF, lhs, rhs));
            }
          } else {
            derefRefVarsForUse(call, call);
          }
        }
      } // for all bb->exprs
    } // for all fn->basicBlocks
  } // for all gFnSymbols
}



static void derefRefVarsForUse(CallExpr* call, CallExpr* outer) {
  for_actuals(actual, call) {
    if (SymExpr* se = toSymExpr(actual)) {
      if (se->var->hasFlag(FLAG_REF_VAR)) {
        if (shouldDerefForCall(call, actual)) {
          SET_LINENO(se);
          CallExpr* deref = new CallExpr(PRIM_DEREF, se->copy());
          VarSymbol* tmp = newTemp("ref_var_call_tmp", deref->typeInfo());
          actual->replace(new SymExpr(tmp));
          outer->insertBefore(new DefExpr(tmp));
          outer->insertBefore(new CallExpr(PRIM_MOVE, tmp, deref));
        }
      }
    } else if (CallExpr* nestedCall = toCallExpr(actual)) {
      derefRefVarsForUse(nestedCall, outer);
    }
  }
}


// returns true if the ref var should be dereferenced for a call
static bool shouldDerefForCall(CallExpr* call, Expr* actual) {
  if (call->get(1) == actual) {
    if (call->isPrimitive(PRIM_MOVE) ||
        call->isPrimitive(PRIM_ASSIGN) ||
        call->isPrimitive(PRIM_ADD_ASSIGN) ||
        call->isPrimitive(PRIM_SUBTRACT_ASSIGN) ||
        call->isPrimitive(PRIM_MULT_ASSIGN) ||
        call->isPrimitive(PRIM_DIV_ASSIGN) ||
        call->isPrimitive(PRIM_MOD_ASSIGN) ||
        call->isPrimitive(PRIM_LSH_ASSIGN) ||
        call->isPrimitive(PRIM_RSH_ASSIGN) ||
        call->isPrimitive(PRIM_AND_ASSIGN) ||
        call->isPrimitive(PRIM_OR_ASSIGN) ||
        call->isPrimitive(PRIM_XOR_ASSIGN) ||
        call->isPrimitive(PRIM_CREATE_REF)) {
      return false;
    }
  }
  if (AggregateType* at = toAggregateType(actual->typeInfo())) {
    if (at->symbol->hasFlag(FLAG_REF)) {
      if (AggregateType* actualAt = toAggregateType(at->getField("_val")->type)) {
        if (actualAt->aggregateTag == AGGREGATE_RECORD ||
            actualAt->aggregateTag == AGGREGATE_UNION) {
          return false;
        }
      }
    }
  }
  return true;
}
