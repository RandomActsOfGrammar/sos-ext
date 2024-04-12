grammar sos:translation:semantic:latex;


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
      [actionSpec(runFun = runLaTeX,
                  shouldDoFun = \ a::Decorated CmdArgs ->
                                  !null(a.latexLocation),
                  actionDesc = "LaTeX Translation")];
}


function runLaTeX
IOVal<ReturnVals> ::= m::ModuleList r::ReturnVals genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local latexString::String = m.ppLaTeX;

  local message::IOToken = printT("Producing LaTeX output\n", i);

  local fileLoc::String = head(a.latexLocation);
  local output::IOToken = writeFileT(fileLoc, latexString, message);

  return ioval(output, 0);
}




synthesized attribute latexLocation::[String] occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.latexLocation = [];
}


abstract production latexOption
top::CmdArgs ::= filename::String rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLocs = rest.rootLocs;

  top.latexLocation = filename::rest.latexLocation;

  top.concTranslations = rest.concTranslations;
  top.semTranslations = rest.semTranslations;

  forwards to rest;
}


aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  nonTransFlags <-
     [flagSpec(name="--latex",
               paramString=just("<filename>"),
               help="filename for LaTeX output",
               flagParser=option(latexOption))];

  errors <-
     if length(a.latexLocation) > 1
     then ["Can only give one location for LaTeX output; found " ++
           toString(length(a.latexLocation))]
     else [];
}
