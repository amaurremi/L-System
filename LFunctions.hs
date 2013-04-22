module LFunctions where

import LSystem
import TwoD

execute :: (Monad m) => Sentence -> Rules -> Iterations -> (Var -> m ()) -> m ()
execute start rules = LSystem.interpreteSentence . (createLSystem rules start !!)

executeSentence :: (Monad m) => Sentence -> (Var -> m ()) -> m ()
executeSentence = LSystem.interpreteSentence

createSentence :: Sentence -> Rules -> Iterations -> Sentence
createSentence sentence rules i = LSystem.createLSystem rules sentence !! i


drawLSystem :: [((Double, Double, Double), (Double, Double, Double))] -> IO ()
drawLSystem = TwoD.drawVerteces
