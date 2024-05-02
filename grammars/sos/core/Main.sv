grammar sos:core;


imports sos:core:semanticDefs:concreteSyntax;
exports sos:core:semanticDefs:abstractSyntax;

imports sos:core:modules;

imports sos:core:concreteDefs:concreteSyntax;

imports sos:core:main:concreteSyntax;


import silver:util:cmdargs;


parser abstractSyntaxParser::File_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
}

parser concreteSyntaxParser::ConcreteFile_c {
  sos:core:common:concreteSyntax;
  sos:core:concreteDefs:concreteSyntax;
}

parser mainFileParser::MainFile_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
  sos:core:main:concreteSyntax;
}


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser,
             mainFileParser, ioin);
}


function run
IOVal<Integer> ::= args::[String]
   abstractFileParse::(ParseResult<File_c> ::= String String)
   concreteFileParse::(ParseResult<ConcreteFile_c> ::= String String)
   mainFileParse::(ParseResult<MainFile_c> ::= String String)
   ioin::IOToken
{
  --actions that aren't for producing runnable translations
  production attribute nonTransActions::[ActionSpec] with ++;
  nonTransActions :=
     [
      actionSpec(
         runFun =
            \ m::ModuleList r::ReturnVals gen::String grmmrs::String
              a::Decorated CmdArgs i::IOToken ->
              let message::IOToken =
                  printT("Checking for errors\n", i)
              in
                if m.errorString != ""
                then ioval(printT(m.errorString ++ "\n", message), 
                     returnVals(
                     returnCode = 1,
                     fileLocs = []
                     ))
                
                else ioval(printT("No errors found\n", message), 
                     returnVals(
                     returnCode = 0,
                     fileLocs = []
                     ))
              end,
         shouldDoFun = \ a::Decorated CmdArgs -> true,
         actionDesc = "Error Checking")
     ];

  --actions for producing runnable translations for derivations
  production attribute semTransActions::[ActionSpec] with ++;
  semTransActions := [];
  --actions for producing runnable translations for parsing
  production attribute concTransActions::[ActionSpec] with ++;
  concTransActions := [];
  --actions for producing runnable translations for main function
  production attribute mainTransActions::[ActionSpec] with ++;
  mainTransActions := [];

  local actions::[ActionSpec] =
      nonTransActions ++ semTransActions ++ concTransActions ++
      mainTransActions;

  local e::Either<String  Decorated CmdArgs> = parseArgs(args);
  local a::Decorated CmdArgs = e.fromRight;
  local rootLocs::[String] = a.rootLocs;

  local modules::IOVal<Either<String ModuleList>> =
        buildModuleList(a.generateModuleName, rootLocs,
           abstractFileParse, concreteFileParse, mainFileParse, ioin);
  local rVals::ReturnVals = returnVals(returnCode = 0, fileLocs = []);
  local genLoc::IOVal<String> =
        envVarT("STERLING_GENERATED", modules.io);
  local grmmrsLoc::IOVal<String> =
        envVarT("STERLING_GRAMMARS", genLoc.io);

  local startMessage::IOToken =
        printT("\n*************** Sterling ***************\n\n" ++
               "   Module:  " ++ a.generateModuleName ++ "\n",
               grmmrsLoc.io);
  local runs::IOVal<ReturnVals> =
        runActions(actions, modules.iovalue.fromRight, rVals, genLoc.iovalue,
                   grmmrsLoc.iovalue, a, startMessage);

  return
     case e of
     | left(err) ->
       ioval(printT("Error parsing commandline input:\n" ++ err, ioin), 1)
     | right(_) ->
       case modules.iovalue of
       | left(err) ->
         ioval(printT(err ++ "\n", modules.io), 1)
       | right(_) ->
         ioval(printT("\n", runs.io), runs.iovalue.returnCode)
       end
     end;
}


--run all the actions in the order in which they occur
function runActions
IOVal<ReturnVals> ::=
    actions::[ActionSpec] mods::ModuleList rVals::ReturnVals genLoc::String
    grmmrsLoc::String a::Decorated CmdArgs ioin::IOToken
{
  local act::ActionSpec = head(actions);
  local pre::IOToken =
      printT("\n---------------------------------------\n\n", ioin);

  local runAct::IOVal<ReturnVals> =
      act.runFun(mods, rVals, genLoc, grmmrsLoc, a, pre);
      
  return
      case actions of
      | [] -> ioval(ioin, returnVals(returnCode = 0, fileLocs = []))
      | _::tl when act.shouldDoFun(a) ->
        if runAct.iovalue.returnCode != 0 --error in this action
        then runAct
        else runActions(tl, mods, runAct.iovalue, genLoc, grmmrsLoc, a, runAct.io)
      | _::tl -> runActions(tl, mods, rVals, genLoc, grmmrsLoc, a, ioin)
      end;
}


--location and grammar name of final, runnable Silver grammar
function buildFinalGrammar
(String, String) ::= module::String genLoc::String
{
  return (genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
             "main/" ++ implode("/", explode(":", module)),
          "main:" ++ module);
}

nonterminal ReturnVals with returnCode, fileLocs;
annotation returnCode::Integer;
annotation fileLocs::[String];

production returnVals
top::ReturnVals ::=
{ }

nonterminal ActionSpec with runFun, shouldDoFun, actionDesc;
--How to run the action (return 0 for success, non-zero for fail)
--Any printed output should end with a single newline
--   result ::= compiled mods  gen loc  grammars loc  args  io
annotation runFun::(IOVal<ReturnVals> ::= ModuleList ReturnVals String  String
                                       Decorated CmdArgs  IOToken);
--Whether to run the action (true => run, false => skip)
--Should not do any IO actions
annotation shouldDoFun::(Boolean ::= Decorated CmdArgs);
--A short name/description of the action, currently for debugging
--May be made part of the standard output in the future
annotation actionDesc::String;

production actionSpec
top::ActionSpec ::=
{ }

attribute
   errors, generateModuleName, rootLocs, outputName, helpRequest,
   concTranslations, semTranslations
occurs on CmdArgs;

synthesized attribute errors::[String];
synthesized attribute generateModuleName::String;
synthesized attribute rootLocs::[String];
synthesized attribute outputName::[String];

synthesized attribute helpRequest::Boolean;

--lists of given runnable translations
--should contain names that can easily be traced to flags
synthesized attribute concTranslations::[String];
synthesized attribute semTranslations::[String];

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.errors =
      case l of
      | [] -> ["Must give a module name"]
      | [_] -> []
      | l -> ["Cannot give more than one module name; found " ++
              toString(length(l))]
      end;

  top.generateModuleName = head(l);

  {-include current directory always, but last, so it comes after any
    locations given by the user-}
  top.rootLocs = ["."];
  top.outputName = [];

  top.helpRequest = false;

  top.concTranslations = [];
  top.semTranslations = [];
}


abstract production locationOption
top::CmdArgs ::= loc::String rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  --normalize so all have the file separator at the end
  top.rootLocs =
      (if endsWith(loc, "/") then loc else loc ++ "/")::rest.rootLocs;
  top.outputName = rest.outputName;

  top.helpRequest = rest.helpRequest;

  top.concTranslations = rest.concTranslations;
  top.semTranslations = rest.semTranslations;

  forwards to rest;
}


abstract production outputNameOption
top::CmdArgs ::= filename::String rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLocs = rest.rootLocs;
  top.outputName = filename::rest.outputName;

  top.helpRequest = rest.helpRequest;

  top.concTranslations = rest.concTranslations;
  top.semTranslations = rest.semTranslations;

  forwards to rest;
}


abstract production helpFlag
top::CmdArgs ::= rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLocs = rest.rootLocs;
  top.outputName = rest.outputName;

  top.helpRequest = true;

  top.concTranslations = rest.concTranslations;
  top.semTranslations = rest.semTranslations;

  forwards to rest;
}



function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  --flags that aren't for producing runnable translations
  production attribute nonTransFlags::[FlagSpec] with ++;
  nonTransFlags :=
     [flagSpec(name="-I",
               paramString=just("<path>"),
               help="path to modules",
               flagParser=option(locationOption)),
      flagSpec(name="-o",
               paramString=just("<filename>"),
               help="runnable file to produce",
               flagParser=option(outputNameOption)),
      flagSpec(name="--help",
               paramString=nothing(),
               help="display help message",
               flagParser=flag(helpFlag))];

  --runnable translations for derivations
  production attribute semTransFlags::[FlagSpec] with ++;
  semTransFlags := [];
  --runnable translations for parsing
  production attribute concTransFlags::[FlagSpec] with ++;
  concTransFlags := [];
  --runnable translations for main function
  production attribute mainTransFlags::[FlagSpec] with ++;
  mainTransFlags := [];

  local flags::[FlagSpec] =
      nonTransFlags ++ semTransFlags ++ concTransFlags ++
      mainTransFlags;

  local usage::String = 
        "Usage: sterling [options] <module name>\n\n" ++
        "Flag options:\n" ++ flagSpecsToHelpText(flags) ++ "\n";

  -- Parse the command line
  production a::CmdArgs = interpretCmdArgs(flags, args);

  production attribute errors::[String] with ++;
  errors := a.errors;
  errors <-
     case a.outputName of
     | [] -> []
     | [_] ->
       if length(a.concTranslations) < 1 ||
          length(a.semTranslations) < 1
       then ["Must give both runnable concrete and semantic " ++
             "translations when giving output filename"]
       else []
     | _::_::l -> ["Can only give one output filename; found " ++
                   toString(length(l) + 2)]
     end;
  --only allowed AT MOST one conc translation and one sem translation
  errors <-
     if length(a.concTranslations) <= 1
     then []
     else ["Can give at most one runnable translation of " ++
           "concrete syntax; found " ++
           implode(", ", a.concTranslations)];
  errors <-
     if length(a.semTranslations) <= 1
     then []
     else ["Can give at most one runnable translation of " ++
           "semantics; found " ++
           implode(", ", a.semTranslations)];

  return if !null(errors)
         then left(implode("\n", errors) ++ "\n\n" ++ usage)
         else if a.helpRequest
         then left(usage)
         else right(a);
}

