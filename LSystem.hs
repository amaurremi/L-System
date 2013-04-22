module LSystem where

import Data.Map (Map, lookup, fromList)
import Data.List (sortBy)
import Data.Ord (comparing)
import Maybe (fromMaybe)

type Var = String
type Sentence = [Var]
type InitVar = Var
type ReplVar = Var
type Rules = [([InitVar], [ReplVar])]
type Iterations = Int

applyRules :: Rules -> Sentence -> Sentence
applyRules _ []     = []
applyRules [] s     = s
applyRules rul@(r:rs) s = let (initVar, replVar) = r
                              index              = indexOf initVar s
                          in case index of
                                Nothing -> applyRules rs s
                                Just i  -> let before  = take i s
                                               after   = drop (i + length initVar) s
                                               apply   = applyRules rul
                                           in apply before ++ replVar ++ apply after
                                
indexOf :: [ReplVar] -> Sentence -> Maybe Int   -- finds the index of the variable to be replaced in the sentence
indexOf vars sent = iOf vars sent 0 False
    where iOf :: [ReplVar] -> Sentence -> Int -> Bool -> Maybe Int
          iOf [] _ n True                 = Just $ n - length vars
          iOf rep@(r:rs) (v:vs) n isFound = let indOf repl = iOf repl vs (n + 1) in
                                                if r == v
                                                    then indOf rs True
                                                    else indOf rep False
          iOf _ _ _ _                     = Nothing

sortRules :: Rules -> Rules
sortRules = sortBy (comparing (negate . length . snd))

createLSystem :: Rules -> Sentence -> [Sentence]
createLSystem = iterate . applyRules . sortRules

interpreteSentence :: (Monad m) => Sentence -> (Var -> m()) -> m ()
interpreteSentence [] _ = return ()
interpreteSentence (v:vs) m = do
                           m v
                           interpreteSentence vs m

