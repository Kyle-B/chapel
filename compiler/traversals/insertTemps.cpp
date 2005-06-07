#include "insertTemps.h"
#include "symtab.h"
#include "symscope.h"
#include "symbol.h"
#include "type.h"
#include "expr.h"
#include "stmt.h"
#include "stringutil.h"


//
// This is a hack to handle some basic appends and prepends until
// where is implemented and # is overloaded in the sequence module.
//
static bool notSequenceType(Type* type) {
  return type == dtInteger || type == dtString;
}
static void handleBasicSequenceAppendPrependOperations(BinOp* binOp) {
  Type* leftType = binOp->left->typeInfo();
  Type* rightType = binOp->right->typeInfo();

  if (notSequenceType(leftType) && notSequenceType(rightType)) {
    INT_FATAL(binOp, "Bad # operation");
  }

  if (notSequenceType(leftType)) {
    binOp->replace(new ParenOpExpr(
                     new MemberAccess(binOp->right->copy(),
                       new UnresolvedSymbol("_chplSeqPrepend")),
                     new AList<Expr>(binOp->left->copy())));
  } else if (notSequenceType(rightType)) {
    binOp->replace(new ParenOpExpr(
                     new MemberAccess(binOp->left->copy(),
                       new UnresolvedSymbol("_chplSeqAppend")),
                     new AList<Expr>(binOp->right->copy())));
  }
}


static void insertSequenceLiteralTemp(SeqExpr* seqExpr) {
  static int uid = 1;

  if (!analyzeAST) {
    INT_FATAL(seqExpr, "Analysis required for sequences");
  }

  char* name = glomstrings(2, "_seq_temp_", intstring(uid++));
  Type* type = Symboltable::lookup("seq2")->typeInfo();
  Type* elt_type = seqExpr->exprls->representative()->typeInfo();

  if (!elt_type || elt_type == dtUnknown) {
    INT_FATAL(seqExpr, "Sequence literal is of unknown type, not handled");
  }

  Expr* init =
    new ParenOpExpr(new Variable(Symboltable::lookup("seq2")->typeInfo()->symbol),
                    new AList<Expr>(new Variable(elt_type->symbol)));
  
  DefStmt* def_stmt = Symboltable::defineSingleVarDefStmt(name,
                                                          type,
                                                          init,
                                                          VAR_NORMAL,
                                                          VAR_VAR);
  
  seqExpr->getStmt()->insertBefore(def_stmt);
  
  Symbol* seq = def_stmt->defExprls->representative()->sym;
  
  //
  // Eventually, want to use overloaded # operator (as below)
  //
  for_alist(Expr, tmp, seqExpr->exprls) {
    Expr* append =
      new ParenOpExpr(
                      new MemberAccess(
                                       new Variable(seq),
                                       new UnresolvedSymbol("_chplSeqAppend")),
                      new AList<Expr>(tmp->copy()));
    seqExpr->getStmt()->insertBefore(new ExprStmt(append));
  }
  seqExpr->replace(new Variable(seq));
}


void InsertTemps::postProcessExpr(Expr* expr) {
  if (SeqExpr* seqExpr = dynamic_cast<SeqExpr*>(expr)) {
    insertSequenceLiteralTemp(seqExpr);
  } else if (BinOp* binOp = dynamic_cast<BinOp*>(expr)) {
    if (binOp->type == BINOP_SEQCAT) {
      handleBasicSequenceAppendPrependOperations(binOp);
    }
  }
}
