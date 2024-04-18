grammar sos:translation:semantic:lambdaProlog;


import silver:util:cmdargs;
import sos:core:modules;
import sos:core;


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser,
             mainFileParser, ioin);
}


aspect function run
IOVal<Integer> ::= _ _ _ _ _
{
  nonTransActions <-
      [actionSpec(runFun = runLambdaProlog,
                  shouldDoFun = \ a::Decorated CmdArgs ->
                                  a.outputLambdaProlog,
                  actionDesc = "Lambda Prolog Translation")];
}


function runLambdaProlog
IOVal<ReturnVals> ::= m::ModuleList r::ReturnVals genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken =
      printT("Producing Lambda Prolog output\n", i);

  local moduleName::String = makeLPModuleName(a.generateModuleName);
  local sigFile::String = moduleName ++ ".sig";
  local modFile::String = moduleName ++ ".mod";
  local importedMods::[String] =
      remove(a.generateModuleName, m.nameList);
  local outputSig::IOToken =
      writeFileT(sigFile, buildSig(moduleName, importedMods,
                                   m.lpDecls), message);
  local outputMod::IOToken =
      writeFileT(modFile, buildMod(moduleName, importedMods,
                                   m.lpRules), outputSig);

  return ioval(outputMod, returnVals(
            returnCode = 0,
            fileLocs = []
            ));
}




synthesized attribute outputLambdaProlog::Boolean occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.outputLambdaProlog = false;
}


abstract production lambdaPrologFlag
top::CmdArgs ::= rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLocs = rest.rootLocs;

  top.outputLambdaProlog = true;

  top.concTranslations = rest.concTranslations;
  top.semTranslations = rest.semTranslations;
  --once we modify this to be runnable, it will add to semTranslations

  forwards to rest;
}


aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  nonTransFlags <-
     [flagSpec(name="--lprolog",
               paramString=nothing(),
               help="output Lambda Prolog translation",
               flagParser=flag(lambdaPrologFlag))];
}

