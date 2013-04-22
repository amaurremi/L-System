module TwoD where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects
import Data.IORef

type V3f = (GLfloat, GLfloat, GLfloat)
type Col3f = V3f

data Mode = Render3D | Render2D
data Vertex3f = Vertex3f V3f Col3f deriving (Show, Eq)

drawVerteces :: [((Double, Double, Double), (Double, Double, Double))] -> IO ()
drawVerteces = drawVerteces' . (map (\((x, y, z), (r, g, b)) -> 
                Vertex3f (realToFrac x, realToFrac y, realToFrac z)
                         (realToFrac r, realToFrac g, realToFrac b)))
    where 
        drawVerteces' :: [Vertex3f] -> IO ()
        drawVerteces' verteces = do
                initialDisplayMode    $= [DoubleBuffered, WithDepthBuffer]
                (progname, _)         <- getArgsAndInitialize
                initialWindowSize     $= Size 700 700
                createWindow "L-System"
                reshapeCallback       $= Just reshape
                keyboardMouseCallback $= Just keyboardMouse
                angle                 <- newIORef 0.0
                displayCallback $= let mode = Render2D
                                   in case mode of
                                        Render2D -> (display2D angle verteces)
                                        _        -> (display angle)
                idleCallback        $= Just (idle angle)
                actionOnWindowClose $= ContinueExectuion 
                mainLoop

aspect :: Size -> GLdouble
aspect (Size w h) = let x = fromIntegral w
                        y = fromIntegral h
                    in x / y

keyboardMouse :: (Monad m) => t -> t1 -> t2 -> t3 -> m ()
keyboardMouse key state modifiers position = return ()

idle :: (HasGetter g, Fractional a, HasSetter g) => g a -> IO ()
idle angle = do
        a     <- get angle
        angle $= a + 0.1
        postRedisplay Nothing

reshape :: Size -> IO ()
reshape s@(Size w h) = do
        viewport $= (Position 0 0, s)
        postRedisplay Nothing

display2D :: IORef GLfloat -> [Vertex3f] -> IO ()
display2D angle graphics = 
        let (xs, ys) = unzip $ map (\(Vertex3f (x, y, _) _) -> (x, y)) graphics
            indent = 0.1
            mini = realToFrac . minimum
            maxi = realToFrac . maximum
            xMin = mini xs - indent
            yMin = mini ys - indent
            xMax = maxi xs + indent
            yMax = maxi ys + indent
        in do
                clear [ColorBuffer, DepthBuffer]
                matrixMode $= Projection
                loadIdentity
                ortho xMin xMax yMin yMax (-1) 1
                matrixMode $= Modelview 0
                renderPrimitive LineStrip $ do
                        color $ (Color3 (1.0 :: GLfloat) 0 0)
                        mapM (\(Vertex3f (x, y, z) col) -> 
                                vertex (Vertex3 x y z)) graphics
                swapBuffers

display :: (HasGetter g) => g GLfloat -> IO ()
display angle = do 
        clear [ColorBuffer, DepthBuffer]
        loadIdentity  
        light (Light 0) $= Enabled
        light (Light 1) $= Enabled
        lighting        $= Enabled
        depthFunc       $= Just Less
        s               <- get windowSize
        matrixMode      $= Projection
        loadIdentity
        perspective 60.0 (aspect s) 0.1 20
        matrixMode      $= Modelview 0
        a               <- get angle
        rotate (a) (Vector3 (0::GLfloat) 1 0) 
        renderObject Solid (Teapot 1)
        swapBuffers
