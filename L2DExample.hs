module L2DExample where

import LFunctions
import Control.Monad.State
import LExamples

type Angle  = Double
type Vector = (Double, Double, Double)
data LState = LState Vector Angle deriving (Show, Eq)
type Graphic = Vector

step :: Double
step = 0.02

createGraphics :: String -> State LState Graphic
createGraphics var = do
                        s <- get
                        let lstate@(LState vector _) = go var s
                        put lstate
                        return vector

go :: String -> LState -> LState
go s (LState (x, y, z) a) = let lstate a = LState (x, y, z) $ a 
                            in case s of 
                                     "F"   -> LState (x + step * cos a, y + step * sin a, z) a
                                     "R"   -> LState (x + step * cos a, y + step * sin a, z) a
                                     "+"   -> lstate $ a + pi / 2
                                     "-"   -> lstate $ a - pi / 2
                                     "+45" -> lstate $ a + pi / 4
                                     "-45" -> lstate $ a - pi / 4
                                     "+60" -> lstate $ a + pi / 3
                                     "-60" -> lstate $ a - pi / 3
                                     _   -> error "unknown symbol"

initialGraphic = (0, 0, 0)

runGraphics :: [String] -> [Graphic]
runGraphics vars = initialGraphic : evalState (mapM createGraphics vars) (LState initialGraphic 0)

white :: (Double, Double, Double)
white = (1, 1, 1)

addColorToGraphic :: (Double, Double, Double) -> [Graphic] -> [((Double, Double, Double), (Double, Double, Double))]
addColorToGraphic color = map (\graphic -> (graphic, color))

main1 = LFunctions.drawLSystem (addColorToGraphic white (runGraphics $ LFunctions.createSentence LExamples.kochKurveStart LExamples.kochKurveRules 6))
main2 = LFunctions.drawLSystem (addColorToGraphic white (runGraphics $ LFunctions.createSentence LExamples.schneeFlockenStart LExamples.schneeFlockenRules 1))
main3 = LFunctions.drawLSystem (addColorToGraphic white (runGraphics $ LFunctions.createSentence LExamples.drachenKurveStart LExamples.drachenKurveRules 10))
