
\subsection{Using the \texttt{optimize} function}\label{ssec:optTrf}
The SMCDEL library contains an \texttt{optimize} function which aims to minimize the size of the knowledge structure by removing redundant propositions.
Usually this is run at the end of a sequence of calls, but we will now define a few wrappers to interleave the optimisation step between each individual call.

Simply trimming redundant propositions that were added by the classical transformer could potentially already provide a reasonable speed improvement.

\hide{
\begin{code}
module OptimizedTransformer where

import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
\end{code}
}

\begin{code}
doCallOpt :: [Prp] -> KnowScene -> (Int, Int) -> KnowScene
doCallOpt vocab start (a,b) = optimize vocab $ start `update` call (length $ agentsOf start) (a,b)

afterOpt :: Int -> [(Int,Int)] -> KnowScene
afterOpt n = foldl (doCallOpt $ vocabOf (gossipInit n)) (gossipInit n)
\end{code}