\subsection{Using the \texttt{optimize} function}
The SMCDEL library contains an \texttt{optimize} function which aims to minimize the size of the knowledge structure by removing redundant propositions.

If

\hide{\begin{code}
module OptimizedTransformer where

import SMCDEL.Examples.GossipS5
import SMCDEL.Symbolic.S5
import SMCDEL.Language
\end{code}
}

\begin{code}
doCallOpt :: KnowScene -> (Int, Int) -> KnowScene
doCallOpt start (a,b) = optimize (vocabOf start) (start `update` call (length $ agentsOf start) (a,b))

afterOpt :: Int -> [(Int,Int)] -> KnowScene
afterOpt n = foldl doCallOpt (gossipInit n)
\end{code}