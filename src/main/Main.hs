-- | Main entry point to haskell-trace.
--
-- Add tracing to modules.
--
-- The -F option lets you run a pre-processor as part of the overall
-- GHC compilation pipeline, which has the advantage over running a
-- Haskell pre-processor separately in that it works in interpreted
-- mode and you can continue to take reap the benefits of GHC's
-- recompilation checker.
--
-- The pre-processor is run just before the Haskell compiler proper
-- processes the Haskell input, but after the literate markup has been
-- stripped away and (possibly) the C pre-processor has washed the
-- Haskell input.
--

module Main where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Language.Haskell.Exts.Annotated
import Language.Haskell.Trace.Preprocessor
import System.Environment
import Text.Read

-- | Main entry point.
--
-- When invoked, the cmd pre-processor is given at least three
-- arguments on its command-line: the first argument is the name of
-- the original source file, the second is the name of the file
-- holding the input, and the third is the name of the file where cmd
-- should write its output to.
--
-- Additional arguments to the pre-processor can be passed in using
-- the -optF option. These are fed to cmd on the command line after
-- the three standard input and output arguments.
--
main :: IO ()
main =
  do args <- getArgs
     case args of
       (originalFP:contentsFP:outFP:rest) ->
         do string <- readFile contentsFP
            case parseModuleWithMode parseMode string of
              ParseOk m ->
                writeFile outFP
                          (prettyPrint
                             (fix (addTracing originalFP
                                              (filters rest)
                                              m)))
              ParseFailed _ e -> error e
       _ ->
         error "Expected <original-fp> <contents-fp> <output-fp>"
  where parseMode =
          defaultParseMode {extensions = defaultExtensions
                           ,fixities = Nothing}
        fix ast =
          fromMaybe ast (applyFixities baseFixities ast)

-- | Collect filters from the argument list.
filters :: [String] -> [(Int,Int,Int,Int)]
filters =
  mapMaybe (tuplize .
            mapMaybe readMaybe .
            groupBy (on (==) isDigit)) .
  mapMaybe (stripPrefix "--filter=")
  where tuplize [a,b,c,d] =
          Just (a,b,c,d)
        tuplize x = Nothing

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions =
  [e | e@EnableExtension{} <- knownExtensions] \\
  map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
  [Arrows -- steals proc
  ,TransformListComp -- steals the group keyword
  ,XmlSyntax
  ,RegularPatterns -- steals a-b
  ,UnboxedTuples -- breaks (#) lens operator
   ]
