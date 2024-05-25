
\section{Simple Tests}
\label{sec:simpletests}

We will discuss the tests below.

\begin{code}
module Main where

import Test.Hspec

import qualified SimpleTransformerSpec
import qualified ClassicTransformerSpec
import qualified ExplainTestsSpec
\end{code}

We test the implementations using hspec. We verify that the 

\begin{code}
main :: IO ()
main = hspec $ do
    describe "SimpleTransformer" SimpleTransformerSpec.spec
    describe "ClassicTransformer" ClassicTransformerSpec.spec
    describe "Explain" ExplainTestsSpec.spec
\end{code}



To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
