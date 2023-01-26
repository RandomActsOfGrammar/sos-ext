grammar sos:core:main:concreteSyntax;


closed nonterminal MainFile_c layout {Spacing_t, Comment_t}
   with ast<MainFile>, location;

concrete productions top::MainFile_c
| name::ModuleDecl_c x::EmptyNewlines d::MainDeclList_c
  { top.ast = mainFile(name.ast, d.ast, location=top.location); }



closed nonterminal MainDeclList_c layout {Spacing_t, Comment_t}
   with ast<MainDecls>, location;

concrete productions top::MainDeclList_c
|
  { top.ast = emptyMainDecls(location=top.location); }
| d::FunDecl_c x::EmptyNewlines rest::MainDeclList_c
  { top.ast =
        branchMainDecls(funMainDecl(d.ast, location=top.location),
                        rest.ast, location=top.location); }



closed nonterminal FunDecl_c layout {Spacing_t, Comment_t}
   with ast<FunDecl>, location;
closed nonterminal Params_c layout {Spacing_t, Comment_t}
   with ast<Params>, location;

concrete productions top::FunDecl_c
-- Function name : params -> return type { body }
| 'Function' x1::EmptyNewlines name::LowerId_t x2::EmptyNewlines ':'
  x3::EmptyNewlines p::Params_c '->'
  x5::EmptyNewlines ty::Type_c x6::EmptyNewlines '{'
  x7::EmptyNewlines body::MainExpr_c '}'
  { top.ast = funDecl(name.lexeme, p.ast, ty.ast, body.ast,
                      location=top.location); }

concrete productions top::Params_c
|
  { top.ast = emptyParams(location=top.location); }
| '<' x1::EmptyNewlines name::UpperId_t x2::EmptyNewlines ':'
  x3::EmptyNewlines ty::Type_c x4::EmptyNewlines '>'
  x5::EmptyNewlines rest::Params_c
  { top.ast = branchParams(
                 oneParams(name.lexeme, ty.ast, location=top.location),
                 rest.ast, location=top.location); }



--Add to existing types
concrete productions top::Type_c
| '[' ty::Type_c ']'
  { top.ast = listType(ty.ast, location=top.location); }
| 'bool'
  { top.ast = boolType(location=top.location); }



closed nonterminal MainExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal AndExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal CompExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal AddExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal MultExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal FactorExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal Args_c layout {Spacing_t, Comment_t}
   with ast<Args>, location;
closed nonterminal Vars_c layout {Spacing_t, Comment_t}
   with ast<[String]>, location;

concrete productions top::MainExpr_c
| e1::MainExpr_c '||' e2::AndExpr_c
  { top.ast = orExpr(e1.ast, e2.ast, location=top.location); }
| e::AndExpr_c
  { top.ast = e.ast; }

concrete productions top::AndExpr_c
| e1::AndExpr_c '&&' e2::CompExpr_c
  { top.ast = andExpr(e1.ast, e2.ast, location=top.location); }
| e::CompExpr_c
  { top.ast = e.ast; }

concrete productions top::CompExpr_c
| e1::CompExpr_c '<' e2::AddExpr_c
  { top.ast = ltExpr(e1.ast, e2.ast, location=top.location); }
| e1::CompExpr_c '>' e2::AddExpr_c
  { top.ast = gtExpr(e1.ast, e2.ast, location=top.location); }
| e1::CompExpr_c '<=' e2::AddExpr_c
  { top.ast = leExpr(e1.ast, e2.ast, location=top.location); }
| e1::CompExpr_c '>=' e2::AddExpr_c
  { top.ast = geExpr(e1.ast, e2.ast, location=top.location); }
| e1::AddExpr_c '=' e2::AddExpr_c
  { top.ast = eqExpr(e1.ast, e2.ast, location=top.location); }
| e::AddExpr_c
  { top.ast = e.ast; }

concrete productions top::AddExpr_c
| e1::AddExpr_c '+' e2::MultExpr_c
  { top.ast = plusExpr(e1.ast, e2.ast, location=top.location); }
| e1::AddExpr_c '-' e2::MultExpr_c
  { top.ast = minusExpr(e1.ast, e2.ast, location=top.location); }
| e1::AddExpr_c '++' e2::MultExpr_c
  { top.ast = appendExpr(e1.ast, e2.ast, location=top.location); }
| e::MultExpr_c
  { top.ast = e.ast; }

concrete productions top::MultExpr_c
| e1::MultExpr_c '*' e2::FactorExpr_c
  { top.ast = multExpr(e1.ast, e2.ast, location=top.location); }
| e1::MultExpr_c '/' e2::FactorExpr_c
  { top.ast = divExpr(e1.ast, e2.ast, location=top.location); }
| e1::MultExpr_c '%' e2::FactorExpr_c
  { top.ast = modExpr(e1.ast, e2.ast, location=top.location); }
| e::FactorExpr_c '[' ind::MainExpr_c ']'
  { top.ast = listIndexExpr(e.ast, ind.ast, location=top.location); }
| e::FactorExpr_c
  { top.ast = e.ast; }

concrete productions top::FactorExpr_c
| 'Derive' j::Judgment_c 'Assigning' '[' vars::Vars_c ']'
  { top.ast = deriveExpr(j.ast, vars.ast, location=top.location); }
| 'Derive' j::Judgment_c 'Assigning' '[' ']'
  { top.ast = deriveExpr(j.ast, [], location=top.location); }
| 'Parse' x1::EmptyNewlines nt::LowerQName_t
  x2::EmptyNewlines 'from' x5::EmptyNewlines e::FactorExpr_c
  { top.ast = parseExpr(toQName(nt.lexeme, nt.location), e.ast,
                        location=top.location); }
| 'Parse' x1::EmptyNewlines nt::LowerId_t
  x2::EmptyNewlines 'from' x5::EmptyNewlines e::FactorExpr_c
  { top.ast = parseExpr(toQName(nt.lexeme, nt.location), e.ast,
                        location=top.location); }
| name::UpperId_t
  { top.ast = varExpr(name.lexeme, location=top.location); }
| num::Integer_t
  { top.ast = intExpr(toInteger(num.lexeme), location=top.location); }
| name::LowerQName_t '(' args::Args_c ')'
  { top.ast = funCall(toQName(name.lexeme, name.location), args.ast,
                      location=top.location); }
| name::LowerId_t '(' args::Args_c ')'
  { top.ast = funCall(toQName(name.lexeme, name.location), args.ast,
                      location=top.location); }
| name::LowerQName_t '(' ')'
  { top.ast = funCall(toQName(name.lexeme, name.location),
                      nilArgs(location=top.location),
                      location=top.location); }
| name::LowerId_t '(' ')'
  { top.ast = funCall(toQName(name.lexeme, name.location),
                      nilArgs(location=top.location),
                      location=top.location); }
| s::String_t
  { top.ast = stringExpr(substring(1, length(s.lexeme) - 1, s.lexeme),
                         location=top.location); }
| 'true'
  { top.ast = trueExpr(location=top.location); }
| 'false'
  { top.ast = falseExpr(location=top.location); }
| '(' e::MainExpr_c ')'
  { top.ast = e.ast; }

concrete productions top::Args_c
| e::MainExpr_c
  { top.ast = consArgs(e.ast, nilArgs(location=top.location),
                       location=top.location); }
| e::MainExpr_c ',' rest::Args_c
  { top.ast = consArgs(e.ast, rest.ast, location=top.location); }

concrete productions top::Vars_c
| v::UpperId_t
  { top.ast = [v.lexeme]; }
| v::UpperId_t ',' rest::Vars_c
  { top.ast = v.lexeme::rest.ast; }
