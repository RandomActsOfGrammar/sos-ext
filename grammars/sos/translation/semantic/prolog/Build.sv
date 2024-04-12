grammar sos:translation:semantic:prolog;


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
  semTransActions <-
      [actionSpec(runFun = runProlog,
                  shouldDoFun = \ a::Decorated CmdArgs ->
                                  a.outputProlog,
                  actionDesc = "Prolog Translation")];
}


function runProlog
IOVal<ReturnVals> ::= m::ModuleList r::ReturnVals genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken = printT("Producing Prolog output\n", i);

  local prologProgram::PrologProgram =
        buildPrologProgram(m.prologRules ++ m.instanTransPrologRules);
  local prologString::String = prologProgram.pp;

  --write Prolog specification
  local dir::String =
      genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
      "prolog/";

  local fileLoc::String = dir ++ a.generateModuleName ++ ".pl";
  local mkDirectory::IOVal<Integer> =
      systemT("mkdir -p " ++ dir, message);
  local output::IOToken =
      writeFileT(fileLoc, prologString, mkDirectory.io);


  --write Silver pieces for running
  local genDerive::IOVal<Integer> =
      genSilverFunctions(genLoc, a.generateModuleName, fileLoc,
                         output);

  return
      if mkDirectory.iovalue != 0
      then returnVals(
            returnCode = mkDirectory.iovalue,
            fileLocs = []
            )
      else returnVals(
            returnCode = 0,
            fileLocs = [fileLoc]
            );
}

--Generate the pieces for running a language using this
function genSilverFunctions
IOVal<Integer> ::= genLoc::String module::String prologFile::String
                   ioin::IOToken
{
  --
  local parserFunction::String =
      "parser parsePrologOutput::PrologOutput{\n" ++
      "   sos:translation:semantic:prolog:parseProlog;\n" ++
      "}";

  --init function for Prolog interaction, starting Prolog process
  local initFunction::String =
      "function init_derive\nIOVal<DeriveConfig> ::= " ++
                       "ioin::IOToken\n{\n" ++
      "   return spawnProcess(\"swipl\", [\"" ++ prologFile ++
                                           "\"], ioin);\n}";

  --derive function
  local deriveFunction::String =
      "function derive\nIOVal<Maybe<[(String, Term)]>> ::= " ++
           "d::DeriveConfig j::Judgment inArgs::[(String, Term)] " ++
           "ioin::IOToken\n{\n" ++
      "   local args::String = " ++
             "foldr(\\ p::(String, Term) rest::String -> " ++
                "p.1 ++ \"=\" ++ p.2.prolog.pp ++ \", \" ++ rest, " ++
                   "j.prolog.pp, inArgs);\n" ++
      "   local s::IOToken = sendToProcess(d, " ++
                               "args ++ \". .\\n\", ioin);\n" ++
                     --end with ". ." so we get the next prompt always
      "   local output::IOVal<String> = " ++
             "let throwAway1::IOVal<String> = readLineFromProcess(d, s) in " ++
             --actual is first line of output
             "let actual::IOVal<String> = readLineFromProcess(d, throwAway1.io) in " ++
             --anymore is anything left after that
             "let anymore::IOVal<String> = readAllFromProcess(d, actual.io) in " ++
             "ioval(anymore.io, actual.iovalue ++ anymore.iovalue) end end end;\n" ++
      "   local parsed::ParseResult<PrologOutput> = " ++
             "parsePrologOutput(output.iovalue," ++
                              "\"<<prolog output>>\");\n" ++
      "   return ioval(output.io, parsed.parseTree.result);\n}";

  --end function for Prolog interaction, killing background process
  local endFunction::String =
      "function end_derive\nIOToken ::= d::DeriveConfig " ++
                                       "ioin::IOToken\n{\n" ++
      "   return waitForProcess(d, sendToProcess(d, \"halt.\\n\", " ++
                                                 "ioin));\n}";

  local grammarInfo::(String, String) =
      buildFinalGrammar(module, genLoc);

  --contents of the Derive.sv file
  local completeContents::String =
      "grammar " ++ grammarInfo.2 ++ ";\n" ++
      "import silver:util:subprocess;\n" ++
      "import sos:core:common:abstractSyntax;\n" ++
      "import sos:core:semanticDefs:abstractSyntax;\n" ++
      "import sos:translation:semantic:prolog;\n" ++
      "import sos:translation:semantic:prolog:parseProlog;\n" ++
      "type DeriveConfig = ProcessHandle;\n\n" ++
      parserFunction ++ "\n\n" ++
      initFunction ++ "\n\n" ++
      deriveFunction ++ "\n\n" ++
      endFunction ++ "\n";

  --write it out
  local filename::String = grammarInfo.1 ++ "/Derive.sv";
  local mkDirectory::IOVal<Integer> =
      systemT("mkdir -p " ++ grammarInfo.1, ioin);
  local written::IOToken =
      writeFileT(filename, completeContents, mkDirectory.io);

  return
      if mkDirectory.iovalue == 0
      then ioval(written, 0)
      else mkDirectory;
}




synthesized attribute outputProlog::Boolean occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.outputProlog = false;
}


abstract production prologFlag
top::CmdArgs ::= rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLocs = rest.rootLocs;

  top.outputProlog = true;

  top.concTranslations = rest.concTranslations;
  top.semTranslations = "Prolog"::rest.semTranslations;

  forwards to rest;
}


aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  semTransFlags <-
     [flagSpec(name="--prolog",
               paramString=nothing(),
               help="output Prolog translation of semantics",
               flagParser=flag(prologFlag))];
}

--Adding prolog definitions to the .jar
--Ask what the exact file path should be, if it should be sterling.jar or stlc.host.jar, and 
