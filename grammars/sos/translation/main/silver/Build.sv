grammar sos:translation:main:silver;

import silver:util:cmdargs;
import sos:core;
import sos:core:modules only ModuleList, stdLibName;


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser,
             mainFileParser, ioin);
}


aspect function run
IOVal<Integer> ::= _ _ _ _ _
{
  mainTransActions <-
      [actionSpec(
          runFun = runSilverMain,
          shouldDoFun =
             \ a::Decorated CmdArgs ->
               !null(a.concTranslations) && !null(a.semTranslations),
          actionDesc = "Silver Main Function Translation")];
}


function runSilverMain
IOVal<ReturnVals> ::= m::ModuleList r::ReturnVals genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken =
      printT("Producing Silver output for main function\n\n", i);

  local silverGenLoc::String =
      genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
      "silverMain";
  local genGrammars::IOVal<Boolean> =
      genSilverMainGrammars(m.silverFunDefsModules, silverGenLoc,
                            message);
  local genGrammarsError::IOToken =
      printT("Error producing Silver grammar files\n", genGrammars.io);
  --generate and compile pieces for running
  local genMain::IOVal<Integer> =
      genSilverMainFunction(genLoc, r, grmmrsLoc, a,
         a.generateModuleName, map(fst,  m.silverFunDefsModules),
         genGrammars.io);

  return
      if !genGrammars.iovalue
      then ioval(genGrammarsError, returnVals(
            returnCode = 2,
            fileLocs = []
            ))
      else ioval(message, returnVals(
            returnCode = genMain.iovalue,
            fileLocs = []
            ));
      
}


function genSilverMainGrammars
IOVal<Boolean> ::= mods::[(String, [SilverFunDef])] genLoc::String
                   i::IOToken
{
  local decls::String = implode("\n", map((.pp), head(mods).2));
  local grmmr::String = "silverMain:" ++ head(mods).1;
  local contents::String =
      "grammar " ++ grmmr ++ ";\n" ++
      "import sos:core:common:abstractSyntax;\n" ++
      "import sos:core:semanticDefs:abstractSyntax;\n" ++
      (if head(mods).1 == stdLibName
       then "" --don't import itself
       else "import silverMain:" ++ stdLibName ++ ";\n") ++
      decls ++ "\n";
  local modSplit::[String] = explode(":", head(mods).1);
  local dir::String =
      genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
      implode("/", modSplit);
  local mkDirectory::IOVal<Boolean> =
      let run::IOVal<Integer> = systemT("mkdir -p " ++ dir, i)
      in
        ioval(run.io, run.iovalue == 0)
      end;
  local filename::String = dir ++ "/Main.sv";
  local writeGrammarFile::IOToken =
      writeFileT(filename, contents, mkDirectory.io);
  local rest::IOVal<Boolean> =
      genSilverMainGrammars(tail(mods), genLoc, writeGrammarFile);

  return
      case mods of
      | [] -> ioval(i, true)
      | _::_ -> if mkDirectory.iovalue
                then rest
                else ioval(mkDirectory.io, false)
      end;
}


function genSilverMainFunction
IOVal<Integer> ::= genLoc::String r::ReturnVals grmmrsLoc::String a::Decorated CmdArgs
                   module::String allGrmmrs::[String] ioin::IOToken
{
  --Silver imports
  local importGrammars::[String] =
      map(\ s::String -> "import silverMain:" ++ s ++ ";",
          stdLibName::allGrmmrs);

  --main function
  local mainFunction::String =
    "function main\nIOVal<Integer> ::= " ++
    "args::[String] ioin::IOToken\n{\n" ++
    "   local startP::IOVal<ParserConfig> = init_parse(ioin);\n" ++
    "   local startD::IOVal<DeriveConfig> = init_derive(ioin);\n" ++
    "   local parserConfig__::ParserConfig = startP.iovalue;\n" ++
    "   local deriveConfig__::DeriveConfig = startD.iovalue;\n" ++
    "   local parseFun__::(IOVal<Either<String Term>> ::= " ++
                             "String String IOToken) = " ++
           "parse(parserConfig__, _, _, _);\n" ++
    "   local deriveFun__::(IOVal<Maybe<[(String, Term)]>> ::= " ++
                       "Judgment [(String, Term)] IOToken) = " ++
           "derive(deriveConfig__, _, _, _);\n" ++
    "   local run::IOVal<Integer> = " ++
          "silverMain:" ++ module ++ ":" ++ funName("main") ++
             "(args, parseFun__, deriveFun__, startD.io);\n" ++
    "   local endP::IOToken = end_parse(parserConfig__, run.io);\n" ++
    "   local endD::IOToken = end_derive(deriveConfig__, endP);\n" ++
    "   return ioval(endD, run.iovalue);\n" ++
    "}";

  local completeContents::String =
      "grammar main:" ++ a.generateModuleName ++ ";\n" ++
      "import sos:core:semanticDefs:abstractSyntax;\n" ++
      implode("\n", importGrammars) ++ "\n\n" ++
      mainFunction ++ "\n";

  local grammarInfo::(String, String) =
      buildFinalGrammar(module, genLoc);

  --write it out
  local filename::String = grammarInfo.1 ++ "/Main.sv";
  local mkDirectory::IOVal<Integer> =
      systemT("mkdir -p " ++ grammarInfo.1, ioin);
  local written::IOToken =
      writeFileT(filename, completeContents, mkDirectory.io);

  local jarName::String =
        if null(a.outputName)
                --Java has a problem with JAR file names having colons
               then "-o " ++ substitute(":", ".", module) ++ ".jar"
               else "-o " ++ head(a.outputName);
  
  --compile it, since this is the last piece of the main
  local compileCmd::String =
      "silver -I " ++ genLoc ++ " " ++
             "-I " ++ grmmrsLoc ++ " " ++
             jarName ++ " " ++
             "main:" ++ a.generateModuleName;

  local compile::IOVal<Integer> = systemT(compileCmd, written);
  local printCompileError::IOToken =
      printT("Error compiling runnable translation\n" ++
             "  (command: " ++ compileCmd ++ "; returned " ++
             toString(compile.iovalue) ++ ")\n", compile.io);

    --Not sure if the semicolon is needed at the line, there is a bug on this line but it persists with or without the semicolon--
  local addToJarFile::IOVal<Integer> =
        systemT("jar -uf jarName " ++ implode(" ", r.fileLocs), compile.io);

  return
      if mkDirectory.iovalue != 0
      then mkDirectory
      else if compile.iovalue != 0
      then ioval(printCompileError, 2)
      else compile;
}
