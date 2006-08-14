/*** cleanup
 ***
 *** This pass and function cleans up the AST after parsing but before
 *** scope resolution.
 ***/

#include "astutil.h"
#include "expr.h"
#include "passes.h"
#include "runtime.h"
#include "stmt.h"
#include "symbol.h"
#include "symscope.h"
#include "stringutil.h"

static void normalize_nested_function_expressions(DefExpr* def);
static void destructure_tuple(CallExpr* call);
static void build_constructor(ClassType* ct);
static void build_setters_and_getters(ClassType* ct);
static void flatten_primary_methods(FnSymbol* fn);
static void add_this_formal_to_method(FnSymbol* fn);
static void change_cast_in_where(FnSymbol* fn);
static void fixup_array_formals(FnSymbol* fn);


static void
flatten_scopeless_block(BlockStmt* block) {
  for_alist(Stmt, stmt, block->body)
    block->insertBefore(stmt->remove());
  block->remove();
}


static void
process_import_expr(CallExpr* call) {
  if (call->isPrimitive(PRIMITIVE_USE)) {
    ModuleSymbol* mod = NULL;
    if (SymExpr* sym = dynamic_cast<SymExpr*>(call->get(1))) {
      mod = dynamic_cast<ModuleSymbol*>(sym->lookup(sym->var->name));
      if (!mod)
        USR_FATAL(call, "Cannot find module '%s'", sym->var->name);
    } else
      INT_FATAL(call, "Use primitive has no module");
    if (mod != compilerModule)
      call->parentStmt->insertBefore(new CallExpr(mod->initFn));
    call->parentScope->astParent->modUses.add(mod);
    call->parentStmt->remove();
  }
}


static void
add_class_to_hierarchy(ClassType* ct, Vec<ClassType*>* seen = NULL) {
  if (!ct->inherits)
    return;

  if (seen == NULL) {
    Vec<ClassType*> seen;
    add_class_to_hierarchy(ct, &seen);
    return;
  }

  forv_Vec(ClassType, at, *seen)
    if (at == ct)
      USR_FATAL(ct, "Class hierarchy is cyclic");

  // make root records inherit from value
  // make root classes inherit from object
  if (ct->inherits->length() == 0) {
    if (ct->classTag == CLASS_RECORD) {
      ct->dispatchParents.add(dtValue);
      dtValue->dispatchChildren.add(ct);
    } else {
      ct->dispatchParents.add(dtObject);
      dtObject->dispatchChildren.add(ct);
    }
  }

  for_alist(Expr, expr, ct->inherits) {
    TypeSymbol* ts = dynamic_cast<TypeSymbol*>(expr->lookup(expr));
    if (!ts)
      USR_FATAL(expr, "Illegal super class");
    ClassType* pt = dynamic_cast<ClassType*>(ts->type);
    if (!pt)
      USR_FATAL(expr, "Illegal super class %s", ts->name);
    if (ct->classTag == CLASS_RECORD && pt->classTag == CLASS_CLASS)
      USR_FATAL(expr, "Record %s inherits from class %s",
                ct->symbol->name, pt->symbol->name);
    if (ct->classTag == CLASS_CLASS && pt->classTag == CLASS_RECORD)
      USR_FATAL(expr, "Class %s inherits from record %s",
                ct->symbol->name, pt->symbol->name);
    if (pt->inherits) {
      seen->add(ct);
      add_class_to_hierarchy(pt, seen);
    }
    ct->dispatchParents.add(pt);
    pt->dispatchChildren.add(ct);
    for_fields_backward(field, pt) {
      if (dynamic_cast<VarSymbol*>(field))
        ct->fields->insertAtHead(field->defPoint->copy());
    }
  }

  ct->inherits = NULL;
}


void cleanup(void) {
  forv_Vec(ModuleSymbol, mod, allModules) {
    Vec<BaseAST*> asts;
    collect_asts(&asts, mod);
    forv_Vec(BaseAST, ast, asts) {
      if (CallExpr* a = dynamic_cast<CallExpr*>(ast)) {
        process_import_expr(a);
      }
    }
  }

  forv_Vec(ModuleSymbol, mod, allModules)
    cleanup(mod);
}


void cleanup(BaseAST* base) {
  Vec<BaseAST*> asts;
  collect_asts(&asts, base);
  forv_Vec(BaseAST, ast, asts) {
    if (BlockStmt* block = dynamic_cast<BlockStmt*>(ast)) {
      if (block->blockTag == BLOCK_SCOPELESS)
        flatten_scopeless_block(block);
    }
  }

  asts.clear();
  collect_asts(&asts, base);
  forv_Vec(BaseAST, ast, asts) {
    if (CallExpr* cast = dynamic_cast<CallExpr*>(ast)) {
      if (cast->isPrimitive(PRIMITIVE_CAST)) {
        if (SymExpr* sym = dynamic_cast<SymExpr*>(cast->get(1))) {
          if (sym->var->type == dtString)
            cast->replace(new CallExpr("_tostring", cast->get(2)->copy(),
                                       cast->get(1)->copy()));
        }
      }
    }
  }

  asts.clear();
  collect_asts(&asts, base);
  forv_Vec(BaseAST, ast, asts) {
    if (CallExpr* a = dynamic_cast<CallExpr*>(ast)) {
      process_import_expr(a);
    }
  }

  asts.clear();
  collect_asts(&asts, base);
  forv_Vec(BaseAST, ast, asts) {
    if (DefExpr* def = dynamic_cast<DefExpr*>(ast)) {
      if (TypeSymbol* ts = dynamic_cast<TypeSymbol*>(def->sym)) {
        if (ClassType* ct = dynamic_cast<ClassType*>(ts->type)) {
          add_class_to_hierarchy(ct);
        }
      }
    }
  }

  asts.clear();
  collect_asts(&asts, base);
  forv_Vec(BaseAST, ast, asts) {
    currentLineno = ast->lineno;
    currentFilename = ast->filename;
    if (DefExpr* a = dynamic_cast<DefExpr*>(ast)) {
      normalize_nested_function_expressions(a);
    } else if (CallExpr* a = dynamic_cast<CallExpr*>(ast)) {
      SymExpr* base = dynamic_cast<SymExpr*>(a->baseExpr);
      if (base && !strcmp(base->var->name, "_tuple")) {
        CallExpr* parent = dynamic_cast<CallExpr*>(a->parentExpr);
        if (parent && parent->isNamed("=") && parent->get(1) == a)
          destructure_tuple(parent);
      }
    }
  }

  asts.clear();
  collect_asts(&asts, base);
  forv_Vec(BaseAST, ast, asts) {
    if (TypeSymbol* type = dynamic_cast<TypeSymbol*>(ast)) {
      if (ClassType* ct = dynamic_cast<ClassType*>(type->type)) {
        build_setters_and_getters(ct);
        build_constructor(ct);
      }
    }
  }

  asts.clear();
  collect_asts(&asts, base);
  forv_Vec(BaseAST, ast, asts) {
    currentLineno = ast->lineno;
    currentFilename = ast->filename;
    if (FnSymbol* fn = dynamic_cast<FnSymbol*>(ast)) {
      flatten_primary_methods(fn);
      add_this_formal_to_method(fn);
      change_cast_in_where(fn);
      fixup_array_formals(fn);
    }
  }

  asts.clear();
  collect_asts(&asts, base);
  forv_Vec(BaseAST, ast, asts) {
    if (DefExpr* def = dynamic_cast<DefExpr*>(ast)) {
      if (ArgSymbol* arg = dynamic_cast<ArgSymbol*>(def->sym)) {
        if (DefExpr* tdef = dynamic_cast<DefExpr*>(def->exprType)) {
          if (TypeSymbol* ts = dynamic_cast<TypeSymbol*>(tdef->sym)) {
            if (UserType* ut = dynamic_cast<UserType*>(ts->type)) {
              if (dynamic_cast<SymExpr*>(ut->typeExpr)) {
                ut->typeExpr->replace(new CallExpr(PRIMITIVE_TYPEOF, arg));
                arg->type = dtAny;
              }
            }
          }
        }
      }
    }
  }
}


/*** normalize_nested_function_expressions
 ***   moves expressions that are parsed as nested function
 ***   definitions into their own statement
 ***   NOTE: during parsing, these are embedded in call expressions
 ***/
static void normalize_nested_function_expressions(DefExpr* def) {
  if ((!strncmp("_anon_record", def->sym->name, 12)) ||
      (!strncmp("_forallexpr", def->sym->name, 11)) ||
      (!strncmp("_let_fn", def->sym->name, 7)) ||
      (!strncmp("_if_fn", def->sym->name, 6)) ||
      (!strncmp("_forif_fn", def->sym->name, 9))) {
    Stmt* stmt = def->parentStmt;
    if (stmt->getFunction() == stmt->getModule()->initFn)
      stmt = dynamic_cast<Stmt*>(stmt->getFunction()->defPoint->parentStmt->next);
    def->replace(new SymExpr(def->sym->name));
    stmt->insertBefore(def);
  }
}


/*** destructure_tuple
 ***
 ***   (i,j) = expr;    ==>    i = expr(1);
 ***                           j = expr(2);
 ***
 ***   NOTE: handles recursive tuple destructuring, (i,(j,k)) = ...
 ***/
static void destructure_tuple(CallExpr* call) {
  Stmt* stmt = call->parentStmt;
  VarSymbol* temp = new VarSymbol("_tuple_destruct");
  stmt->insertBefore(new DefExpr(temp));
  CallExpr* tuple = dynamic_cast<CallExpr*>(call->get(1));
  call->replace(new CallExpr(PRIMITIVE_MOVE, temp, call->get(2)->remove()));
  int i = 1;
  for_alist(Expr, expr, tuple->argList) {
    if (i > 1)
      stmt->insertAfter(
        new CallExpr("=", expr->remove(),
          new CallExpr(temp, new_IntLiteral(i-1))));
    i++;
  }
}


static void build_constructor(ClassType* ct) {
  if (ct->defaultConstructor)
    return;

  char* name = stringcat("_construct_", ct->symbol->name);
  FnSymbol* fn = new FnSymbol(name);
  ct->defaultConstructor = fn;
  fn->fnClass = FN_CONSTRUCTOR;
  fn->cname = stringcat("_construct_", ct->symbol->cname);

  if (ct->symbol->hasPragma("tuple"))
    fn->addPragma("tuple");

  AList<DefExpr>* args = new AList<DefExpr>();

  for_fields(tmp, ct) {
    if (!dynamic_cast<VarSymbol*>(tmp))
      continue;
    char* name = tmp->name;
    Type* type = tmp->type;
    Expr* exprType = tmp->defPoint->exprType;
    if (exprType)
      exprType->remove();
    Expr* init = tmp->defPoint->init;
    if (init) {
      init->remove();
      if (!exprType)
        exprType = init->copy();
    } else if (!tmp->isTypeVariable)
      init = new SymExpr(gNil);
    VarSymbol *vtmp = dynamic_cast<VarSymbol*>(tmp);
    ArgSymbol* arg = new ArgSymbol((vtmp && vtmp->consClass == VAR_PARAM) ? INTENT_PARAM : INTENT_BLANK, name, type, init);
    DefExpr* defExpr = new DefExpr(arg, NULL, exprType);
    arg->isTypeVariable = tmp->isTypeVariable;
    if (!exprType && arg->type == dtUnknown)
      arg->type = dtAny;
    args->insertAtTail(defExpr);
  }

  fn->formals = args;

  reset_file_info(fn, ct->symbol->lineno, ct->symbol->filename);
  ct->symbol->defPoint->parentStmt->insertBefore(new DefExpr(fn));

  fn->_this = new VarSymbol("this", ct);

  char* description = stringcat("instance of class ", ct->symbol->name);
  Expr* alloc_rhs = new CallExpr(PRIMITIVE_CHPL_ALLOC,
                                 ct->symbol,
                                 new_StringLiteral(description));
  CallExpr* alloc_expr = new CallExpr(PRIMITIVE_MOVE, fn->_this, alloc_rhs);

  fn->insertAtTail(new DefExpr(fn->_this));
  fn->insertAtTail(alloc_expr);
  if (ct->classTag == CLASS_CLASS)
    fn->insertAtTail(new CallExpr(PRIMITIVE_SETCID, fn->_this));

  // assign formals to fields by name
  for_fields(field, ct) {
    for_formals(formal, fn) {
      if (!formal->variableExpr && !strcmp(formal->name, field->name)) {
        CallExpr* call = dynamic_cast<CallExpr*>(formal->defPoint->exprType);
        if (call && call->isNamed("_build_array_type")) {
          VarSymbol* tmp = new VarSymbol("_tmp");
          fn->insertAtTail(new DefExpr(tmp, NULL, call->copy()));
          fn->insertAtTail(new CallExpr(PRIMITIVE_SET_MEMBER, fn->_this, 
                                        new_StringSymbol(field->name), tmp));
          fn->insertAtTail(new CondStmt(new SymExpr(formal), new ExprStmt(new CallExpr("=",
                             new CallExpr(".",
                                          fn->_this,
                                          new_StringSymbol(field->name)),
                                                                                       formal))));
        } else {
          Expr* assign_expr = new CallExpr(PRIMITIVE_SET_MEMBER, fn->_this, 
                                           new_StringSymbol(field->name), formal);
          fn->insertAtTail(assign_expr);
        }
      }
    }
  }

  forv_Vec(FnSymbol, method, ct->methods) {
    if (!strcmp(method->name, "initialize")) {
      if (method->formals->length() == 2) {
        fn->insertAtTail(new CallExpr("initialize"));
        break;
      }
    }
  }

  fn->insertAtTail(new ReturnStmt(fn->_this));
  fn->retType = ct;
}


static void build_getter(ClassType* ct, Symbol *field) {
  forv_Vec(FnSymbol, fn, ct->methods) {
    if (fn->_getter == field)
      return;
  }

  FnSymbol* fn = new FnSymbol(field->name);
  fn->addPragma("inline");
  fn->_getter = field;
  ArgSymbol* _this = new ArgSymbol(INTENT_BLANK, "this", ct);
  fn->formals->insertAtTail(new ArgSymbol(INTENT_BLANK, "_methodTokenDummy", dtMethodToken));
  fn->formals->insertAtTail(_this);
  fn->body = new BlockStmt(new ReturnStmt(new CallExpr(PRIMITIVE_GET_MEMBER, new SymExpr(_this), new SymExpr(new_StringSymbol(field->name)))));
  DefExpr* def = new DefExpr(fn);
  ct->symbol->defPoint->parentStmt->insertBefore(def);
  reset_file_info(fn, field->lineno, field->filename);
  ct->methods.add(fn);
  fn->isMethod = true;
  fn->cname = stringcat("_", ct->symbol->cname, "_", fn->cname);
  fn->noParens = true;
  fn->_this = _this;
}


static void build_setter(ClassType* ct, Symbol* field) {
  forv_Vec(FnSymbol, fn, ct->methods) {
    if (fn->_setter == field)
      return;
  }

  FnSymbol* fn = new FnSymbol(field->name);
  fn->addPragma("inline");
  fn->_setter = field;
  fn->retType = dtVoid;

  ArgSymbol* _this = new ArgSymbol(INTENT_BLANK, "this", ct);
  ArgSymbol* fieldArg = new ArgSymbol(INTENT_BLANK, "_arg", dtAny);
  fn->formals->insertAtTail(new ArgSymbol(INTENT_BLANK, "_methodTokenDummy", dtMethodToken));
  fn->formals->insertAtTail(_this);
  fn->formals->insertAtTail(new ArgSymbol(INTENT_BLANK, "_setterTokenDummy", dtSetterToken));
  fn->formals->insertAtTail(fieldArg);
  Expr *valExpr = new CallExpr(PRIMITIVE_GET_MEMBER, new SymExpr(_this), new SymExpr(new_StringSymbol(field->name)));
  Expr *assignExpr = new CallExpr("=", valExpr, fieldArg);
  fn->body->insertAtTail(
    new CallExpr(PRIMITIVE_SET_MEMBER, new SymExpr(_this), new SymExpr(new_StringSymbol(field->name)), assignExpr));
  ct->symbol->defPoint->parentStmt->insertBefore(new DefExpr(fn));
  reset_file_info(fn, field->lineno, field->filename);

  ct->methods.add(fn);
  fn->isMethod = true;
  fn->cname = stringcat("_", ct->symbol->cname, "_", fn->cname);
  fn->noParens = true;
  fn->_this = _this;
}


static void build_setters_and_getters(ClassType* ct) {
  for_fields(field, ct) {
    VarSymbol *cfield = dynamic_cast<VarSymbol*>(field);
    // if suppress for cobegin created arg classes
    if (cfield && !cfield->is_ref) {
      build_setter(ct, field);
      build_getter(ct, field);
    }
  }
}


static void flatten_primary_methods(FnSymbol* fn) {
  if (TypeSymbol* ts = dynamic_cast<TypeSymbol*>(fn->defPoint->parentSymbol)) {
    Stmt* insertPoint = ts->defPoint->parentStmt;
    while (dynamic_cast<TypeSymbol*>(insertPoint->parentSymbol))
      insertPoint = insertPoint->parentSymbol->defPoint->parentStmt;
    DefExpr* def = fn->defPoint;
    def->remove();
    insertPoint->insertBefore(new ExprStmt(def));
  }
}


static void add_this_formal_to_method(FnSymbol* fn) {
  if (fn->isSetter &&
      fn->formals->get(fn->formals->length()-1)->sym->type != dtSetterToken)
    fn->formals->last()->insertBefore
      (new DefExpr(new ArgSymbol(INTENT_BLANK, "_st", dtSetterToken)));
}


static void change_cast_in_where(FnSymbol* fn) {
  if (fn->where) {
    Vec<BaseAST*> asts;
    collect_asts(&asts, fn->where);
    forv_Vec(BaseAST, ast, asts) {
      if (CallExpr* call = dynamic_cast<CallExpr*>(ast)) {
        if (call->isPrimitive(PRIMITIVE_CAST))
          call->primitive = primitives[PRIMITIVE_ISSUBTYPE];
      }
    }
  }
}


static void fixup_array_formals(FnSymbol* fn) {
  Vec<BaseAST*> asts;
  collect_top_asts(&asts, fn);
  forv_Vec(BaseAST, ast, asts) {
    if (CallExpr* call = dynamic_cast<CallExpr*>(ast)) {
      if (call->isNamed("_build_array_type")) {
        SymExpr* sym = dynamic_cast<SymExpr*>(call->get(1));
        if (sym && sym->var == gNil) {
          DefExpr* def = dynamic_cast<DefExpr*>(call->parentExpr);
          if (!def || !dynamic_cast<ArgSymbol*>(def->sym) || def->exprType != call)
            USR_FATAL(call, "Array with empty domain can only be used as a formal argument type");
          def->exprType->replace(new SymExpr("_array"));
          if (!fn->where) {
            fn->where = new BlockStmt(new ExprStmt(new SymExpr(gTrue)));
            insert_help(fn->where, NULL, NULL, fn, fn->argScope);
          }
          ExprStmt* stmt = dynamic_cast<ExprStmt*>(fn->where->body->only());
          Expr* expr = stmt->expr;
          expr->replace(new CallExpr("&&", expr->copy(), new CallExpr("==", call->get(2)->copy(), new CallExpr(".", def->sym, new_StringLiteral("elt_type")))));
        }
      }
    }
  }
}
