module AronOpenGL where

import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL as GL 
import qualified Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL.GLU.Matrix as GM  
import System.Exit
import System.IO
import Control.Monad
import Data.Set(Set) 
import Data.IORef
import qualified Data.Map as DM
import qualified Data.Set as S 

import Data.Word
import Foreign.Ptr
import qualified Data.Vector.Storable as STO
import qualified Data.Vector.Storable.Mutable as STOM
import qualified Data.Vector.Mutable as DVM
import Foreign.ForeignPtr
import Codec.Picture.Types -- from the JuicyPixels librar
import Codec.Picture.Png
import Codec.Picture


-- import Graphics.UI.GLUT
-- import Graphics.UI.GLUT.Callbacks.Global
import AronModule  hiding (rw)
import AronGraphic hiding (dist)

{-| 
    === Vertex3 List, 25 vertices on x-y-plane only
-} 
vertexList = [
            Vertex3 0.9 0.8 0,
            Vertex3 0.5 0.1 0,
            Vertex3 0.7 0.1 0,
            Vertex3 0.2 0.4 0,
            Vertex3 0.4 0.29 0,

            Vertex3 0.9 0.7 0,
            Vertex3 0.71 0.1 0,
            Vertex3 0.2 0.9 0,
            Vertex3 0.23 0.3 0,
            Vertex3 0.5 0.5 0,

            Vertex3 0.1 0.9 0,
            Vertex3 0.2 0.31 0,
            Vertex3 0.471 0.21 0,
            Vertex3 0.442 0.34 0,
            Vertex3 0.2333 0.6 0,
            
            Vertex3 0.555 0.245 0,
            Vertex3 0.111 0.399 0,
            Vertex3 0.222 0.231 0,
            Vertex3 0.89 0.33 0,
            Vertex3 0.21 0.31 0,

            Vertex3 0.69 0.13 0,
            Vertex3 0.121 0.51 0,
            Vertex3 0.49 0.43 0,
            Vertex3 0.44 0.66 0,
            Vertex3 0.49 0.26 0 
            ]::[Vertex3 GLfloat]

{-| 
    === GLfloat pts
-} 
pts = [0.3, 
      0.1,
      0.4,
      0.2,
      0.32,

      0.19,
      0.21,
      0.39,
      0.19,
      0.09,
      0.29,
      0.239
      ]::[GLfloat]

_STEP = 1.0
  
data Cam = Cam{alpha::Double, beta::Double, gramma::Double, dist::Double} deriving(Show)
data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double}          deriving(Show)
data XYZAxis = XYZAxis{xa::Bool, ya::Bool, za::Bool}                      deriving(Show)
-- data GlobalRef = GlobalRef{str_::String, cursor_::(Int, Int), xyzAxis_::XYZAxis} deriving (Show)
-- data GlobalRef = GlobalRef{str_::String, cursor_::(GLfloat, GLfloat), xyzAxis_::XYZAxis} deriving (Show)
data GlobalRef = GlobalRef{
                    str_::String
                    ,windowSize_::(Int, Int)
                    ,cursor_::(GLfloat, GLfloat) 
                    ,xyzAxis_::XYZAxis 
                    ,mousePressed_::(Bool, (GLfloat, GLfloat))
                    ,drawRectX_ :: (Vertex3 GLfloat, Vertex3 GLfloat)
                    ,tranDrawRectX_ :: (Vector3 GLdouble)
                    ,fovDegree_ :: GLdouble
                    ,drawPts_ :: [[Vertex3 GLfloat]]
                    ,randomPts_ :: [Vertex3 GLfloat]
                    ,randomWalk_ :: [Vertex3 GLfloat]
                    ,randomWalkInt_ :: [(Int, Int)]
                    ,boardMap_ :: DM.Map (Int, Int) ((Vertex3 GLfloat), Color3 GLdouble)
                } deriving (Show)

initCam = Cam{alpha=0.0, beta=0.0, gramma=0.0, dist = 0.0}
  
initStep = Step{xx=0.0, yy=0.0, zz=0.0, ww = 0.01}


{-| 
    === keyboard call back function

    >data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)

    TODO: combine Cam{..} and Step{..} here
         do all the calculation here, remove keyboardRot
-} 
keyBoardCallBack::IORef Step -> G.KeyCallback
keyBoardCallBack ref window key scanCode keyState modKeys = do
    ps "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
    putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
    case keyState of
        G.KeyState'Pressed -> do 
            -- write Step{...} to ref
            case key of
              k | k == G.Key'Right -> writeIORef ref Step{xx=_STEP,    yy =0.0,      zz = 0.0,    ww = 0.0}
                | k == G.Key'Right -> writeIORef ref Step{xx=_STEP,    yy =0.0,      zz = 0.0,    ww = 0.0}
                | k == G.Key'Left  -> writeIORef ref Step{xx=(-_STEP), yy =0.0,      zz = 0.0,    ww = 0.0}
                | k == G.Key'Up    -> writeIORef ref Step{xx=0.0,      yy =_STEP,    zz = 0.0,    ww = 0.0}
                | k == G.Key'Down  -> writeIORef ref Step{xx=0.0,      yy =(-_STEP), zz = 0.0,    ww = 0.0}
                | k == G.Key'9     -> writeIORef ref Step{xx=0.0,      yy =0.0,      zz = _STEP,  ww = 0.0}
                | k == G.Key'0     -> writeIORef ref Step{xx=0.0,      yy =0.0,      zz = -_STEP, ww = 0.0}
                | k == G.Key'8     -> writeIORef ref Step{xx=0.0,      yy =0.0,      zz = 0.0,    ww = _STEP}
                | k == G.Key'7     -> writeIORef ref Step{xx=0.0,      yy =0.0,      zz = 0.0,    ww = -_STEP}
  
                -- TODO: In orthogonal projective status, 
                --   | k == G.Key'F1    -> writeIORef ref Step{xx=0.0,      yy =0.0,      zz = 0.0,    ww = 0.1}
                | k == G.Key'Space -> writeIORef ref initStep
                | otherwise -> pp "Unknown Key Press"
        G.KeyState'Released -> do
            if key == G.Key'Right then pp "Release Key => Right" else pp "Press No Right"
            if key == G.Key'Left  then pp "Release Key => left"  else pp "Press No Right"
            if key == G.Key'Up    then pp "Release Key => up"    else pp "Release No Up"
            if key == G.Key'Down  then pp "Release Key => Down"  else pp "Release No Down"
    when (key == G.Key'Escape && keyState == G.KeyState'Pressed)
        (G.setWindowShouldClose window True)


  
{-| 
    === Camera rotates around x y z axis

    @

    * Press a key, trigger keyBoardCallBack to UPDATE Ref Step
    * Increase/decrease _STEP (degree), modify the ref Step
                                                        ↑
    *                               +-------------------+
                                    ↓
    * keyboardRot IORef Cam -> IORef Step -> Double -> Double -> IO()
    * Update Cam::{alpha, beta, gramma, dist}
 
    @
    @
    data Cam = Cam{alpha::Double, beta::Double, gramma::Double, dist::Double} deriving(Show)

    data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)

    IORef Cam  => ref
    IORef Step => refStep
    keyboardRot ref refStep (fromIntegral width) (fromIntegral height)
    @
-} 
keyboardRot::IORef Cam -> IORef Step -> Double -> Double -> IO()
keyboardRot ref refStep w h = do
    cam  <- readIORef ref           -- data Cam = {...}
    step <- readIORef refStep       -- data Step = {...} refStep is not modified actually.
    modifyIORef ref (rx (xx step))  -- Update Camera x-axis 
    modifyIORef ref (ry (yy step))  -- Update Camera y-axis
    modifyIORef ref (rz (zz step))  -- Update Camera z-axis
    modifyIORef ref (rw (ww step))  -- Not been used for now

  
    -- Tuesday, 09 November 2021 12:39 PST
    -- TODO: fix the rotation issue
    -- ERROR: after the rotation of x-axis, y-axis and z-axis are not longer standard axes any more
    -- M-x gx
    -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html#v:rotate
    rotate ( alpha  cam) ( Vector3 1 0 0 :: Vector3 GLdouble)  -- rotate x-axix alpha degree/radian
                                                               --                ↑→ degree
    rotate ( beta   cam) ( Vector3 0 1 0 :: Vector3 GLdouble)  -- rotate y-axis beta  degree/radian
                                                               --                ↑→ degree
    rotate ( gramma cam) ( Vector3 0 0 1 :: Vector3 GLdouble)  -- rotate z-axis gamma degree/radian
                                                               --                ↑→ degree
    -- rotate ( beta   cam) $ ( Vector3 0 1 0 :: Vector3 GLdouble)  -- rotate y-axis beta  degree/radian
    
    -- y' = y cos alpha
    -- z' = z cos gramma
    -- lookAt (Vertex3 1.1 + (gamma cam) 0 0) (Vertex3 0 0 0)  (Vector3 0 1 0)
    -- get viewport width and height for ratio
    -- let ratio = width/height
    -- perspective 60.0 ratio 0.5 4.0
    -- perspective (field of view) width/height zNear zFar
    -- lookAt eye_vertex center_vertex lookat_vector
    -- perspective 65.0 (w/h) 1.0 4.0
        where
            rx::Double -> Cam -> Cam
            rx d (Cam x y z w) = Cam{alpha = x + d, beta = y,     gramma = z,     dist = w}

            ry::Double -> Cam -> Cam
            ry d (Cam x y z w) = Cam{alpha = x,     beta = y + d, gramma = z,     dist = w}

            rz::Double -> Cam -> Cam
            rz d (Cam x y z w) = Cam{alpha = x,     beta = y,     gramma = z + d, dist = w}

            rw::Double -> Cam -> Cam
            rw d (Cam x y z w) = Cam{alpha = x,     beta = y,     gramma = z,     dist = w}

{-| 
    === Draw Triangle


                   
                0.5 ← v₀ 
                 |
                 |
                 |
        - 0.25 - + - 0.25
           ↑           ↑
           v₁          v₂

-} 
drawTriangle::IO()
drawTriangle = 
    preservingMatrix $ do
        translate (Vector3 0 0 0 :: Vector3 GLdouble)
        renderPrimitive Triangles $ do
            color  (Color3  1         0    0 :: Color3 GLdouble)
            vertex (Vertex3 0         0.5  0 :: Vertex3 GLdouble)  -- v₀
            color  (Color3  0         1    0 :: Color3 GLdouble)
            vertex (Vertex3 (ne 0.25) 0    0 :: Vertex3 GLdouble)  -- v₁
            color  (Color3  0         0    1 :: Color3 GLdouble)
            vertex (Vertex3 0.25      0    0 :: Vertex3 GLdouble)  -- v₂
  where
    ne = negate
  
{-| 
    === Draw Triangle specified location and size
                             ↑                ↑
                             v               0.1
    >let v = Vector3 0.0 0 0
    >drawTriangle' v 0.1
-} 
drawTriangle'::Vector3 GLdouble -> GLdouble ->IO()
drawTriangle' v s = 
    preservingMatrix $ do
        translate v 
        renderPrimitive Triangles $ do
            color  (Color3 1 0 0 :: Color3 GLdouble)
            vertex (Vertex3 (negate s) (negate s) 0 :: Vertex3 GLdouble)
            color  (Color3 0 1 0 :: Color3 GLdouble)
            vertex (Vertex3 s (negate s) 0 :: Vertex3 GLdouble)
            color  (Color3 0 0 1 :: Color3 GLdouble)
            vertex (Vertex3 0 s 0 :: Vertex3 GLdouble)

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description

printGLVersion::G.Window -> IO()
printGLVersion window = do
      major <- G.getWindowContextVersionMajor    window
      minor <- G.getWindowContextVersionMinor    window
      rev   <- G.getWindowContextVersionRevision window
      putStrLn $ "OpenGL context version = " ++ show major ++ "." ++ show minor ++ "." ++ show rev  

{-| 
    === Torus paremater equation center at (0, 0, 0)

    Torus equation: <http://xfido.com/html/indexThebeautyofTorus.html Torus> 

    * drawPrimitive Points red torus 
-} 
torus::[(GLfloat, GLfloat, GLfloat)]
torus= [ ( fx i k, 
           fy i k, 
           fz i k ) | i <- [1..n], k <-[1..n]]
        where 
            del = rf(2*pi/(n-1))
            n = 100 
            r = 0.2
            br = 0.3

            fx = \i k -> (br + r**cos(del*i))*cos(del*k)
            fy = \i k -> sin(rf del*i)
            fz = \i k -> (br + r*cos(rf del*i))*sin(rf del*k)

{-| 
    === Draw torus with small and large radius

    >mapM_ (\lo -> drawPrimitive LineLoop red lo) $ torusR 0.1 0.2 
-} 
torusR::GLfloat -> GLfloat -> [[(GLfloat, GLfloat, GLfloat)]]
torusR r br = [[((br + r*cos(del*i))*cos(del*k), 
          sin(rf del*i), 
          (br + r*cos(rf del*i))*sin(rf del*k) ) | i <- [1..n]] | k <-[1..n]]
            where 
                del = rf(2*pi/(n-1))
                n = 100 


{-| 
    === sphere rotates around y-axis

    === cut the sphere alone the xz-plane

    * The radius of the circle is \( r \cos \alpha \)

    * The circle on the xz-plane is 

    \[
        \begin{equation}
        \begin{aligned}
        x &= r \cos \alpha \sin \beta \\
        z &= r \cos \alpha \cos \beta \\
        \end{aligned}
        \end{equation}
    \]

    * The center of the circle from the center of sphere is \( r \cos \alpha \sin \beta \) 

    * Put all together
    \[
        \begin{equation}
        \begin{aligned}
        x &= r \cos \alpha \sin \beta \\
        y &= r \sin \beta \\
        z &= r \cos \alpha \cos \beta \\
        \end{aligned}
        \end{equation}
    \]
-} 
sphere::GLfloat -> [[(GLfloat, GLfloat, GLfloat)]]
sphere r = [[((r*cos(del*i))*cos(del*k), 
               sin(rf del*i), 
              (r*cos(rf del*i))*sin(rf del*k) ) | i <- [1..n]] | k <-[1..n]]
            where 
                del = rf(2*pi/(n-1))
                n = 40 

torus'::[[(GLfloat, GLfloat, GLfloat)]]
torus'= [[((br + r*cos(del*i))*cos(del*k), 
          sin(rf del*i), 
          (br + r*cos(rf del*i))*sin(rf del*k) ) | i <- [1..n]] | k <-[1..n]]
            where 
                del = rf(2*pi/(n-1))
                n = 100 
                r = 0.3
                br = 0.3

parabolic::[[(GLfloat, GLfloat, GLfloat)]]
parabolic =[[(x'*i, y'*j, 2*(x'*i)^2 + 3*(y'*j)^2) | i <- [-n..n]] | j <-[-n..n]]
            where 
                n = 20 
                del = rf(1/n);
                x' = del
                y' = del

-- x^4 + y^2 + z^6 = 6
surface1::[[(GLfloat, GLfloat, GLfloat)]]
surface1 =[[(u*d, v*d, (c - (u*d)^4 - (v*d)^2)**(1/6)) | u <- [-n..n]] | v <-[-n..n]]
            where 
                n = 160 
                d = rf(1/n);
                c = 0.1

surface2::[[(GLfloat, GLfloat, GLfloat)]]
surface2 =[[(u*d, v*d, -(c - (u*d)^4 - (v*d)^2)**(1/6)) | u <- [-n..n]] | v <-[-n..n]]
            where 
                n = 100 
                d = rf(1/n);
                c = 0.1

surface3::[[(GLfloat, GLfloat, GLfloat)]]
surface3 =[[let x' = x*d; y' = y*d in (x', y', 1 - (1-x')^2 - 100*(y' - x'^2)^2 ) | y <- [-n..n]] | x <-[-n..n]]
            where 
                n = 100 
                d = rf(1/n);
                c = 0.1

lightingInfo::IO()
lightingInfo  = do
    diffuse  (Light 0) $= lightDiffuse
    ambient  (Light 0) $= lightAmbient
    specular (Light 0) $= lightSpecular
    position (Light 0) $= lightPosition
    light    (Light 0) $= Enabled
    -- lighting           $= Enabled
    depthFunc          $= Just Lequal
    blend              $= Enabled
    lineSmooth         $= Enabled

{-| 
    === Generate random Vertex3 in xy-plan, Vertex3 x y 0
-} 
randomVertex::Integer -> IO [Vertex3 GLfloat]
randomVertex n = do 
                    ranl <- randomDouble (fromIntegral n) 
                    let ranlist = map (\x -> 1.5*x) ranl 
                    let vexList = fmap (\x -> x - 0.5) $ fmap realToFrac ranlist 
                    let vexTuple = map(\x -> tripleToVertex3 x ) $ filter(\x -> length x == 3) $ partList 3 vexList 
                                where 
                                    tripleToVertex3::[GLfloat] -> Vertex3 GLfloat 
                                    tripleToVertex3 [a, b, c] = Vertex3 a b 0.0 
                    return vexTuple


{-|
    KEY: save image, save png, opengl save image, save png opengl

    DATE: Thursday, 20 April 2023 12:07 PDT
    NOTE: The image will be up side down
    TODO: Fix it

    https://www.reddit.com/r/haskell/comments/dee0iz/converting_opengl_window_to_a_png_image_in_haskell/
    read pixels, convert them to first a Vector then a JuicyPixel image
    and save the image as a PNG file


    Graphics.Rendering.OpenGL.GL.ReadCopyPixels readPixels :: Position -> Size -> PixelData a -> IO ()
    readPixels::(Position x y) -> (Size w h) -> PixelData a -> IO ()

    readPixels (Position x y) (Size w h) pd =
      withPixelData pd $ glReadPixels x y w h


    data PixelData a = PixelData PixelFormat DataType (Ptr a)

    https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadPixels.xhtml

    glReadPixels(GLint x       ,
                 GLint y       ,
                 GLsizei width ,
                 GLsizei height,
                 GLenum  format,
                 GLenum  type  ,
                 void * data);

-}
saveImageOpenGL :: G.Window -> FilePath -> IO ()
saveImageOpenGL window fp = do
  -- (Int, Int) <- G.getFramebufferSize window
  (w, h) <- G.getFramebufferSize window
  let w' = fromIntegral w
  let h' = fromIntegral h
  -- currentWindow $= Just window
  -- Size w h <- get windowSize
  let npixels = fromIntegral (w*h) :: Int
      nbytes  = 3*npixels
  fptr <- mallocForeignPtrArray nbytes :: IO (ForeignPtr Word8)
  -- withForeignPtr::ForeignPtr a -> (Ptr a -> IO b) -> IO b
  withForeignPtr fptr $ \ptr -> do
    let pdata = GL.PixelData GL.RGB GL.UnsignedByte ptr :: PixelData Word8
    readPixels (Position 0 0) (Size w' h') pdata
    -- readPixels (Position 0 0) (Size Int32 Int32) pdata
  let fptr' = castForeignPtr fptr :: ForeignPtr (PixelBaseComponent PixelRGB8)
  let vecImage = STO.unsafeFromForeignPtr0 fptr' npixels :: STO.Vector (PixelBaseComponent PixelRGB8)
  print $ "w=" ++ show w
  print $ "h=" ++ show h
  print $ "len=" ++ (show $ STO.length vecImage)
  let pix1 = vecImage STO.! 0
  let image = Image (fromIntegral w) (fromIntegral h) vecImage :: Image PixelRGB8
  writePng fp image
