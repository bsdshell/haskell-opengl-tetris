module AronOpenGL where

-- import Graphics.UI.GLUT
-- import Graphics.UI.GLUT.Callbacks.Global
import AronGraphic hiding (dist)
import AronModule hiding (rw)
import Codec.Picture
import Codec.Picture.Png
import Codec.Picture.Types
import Control.Lens hiding (pre, re)
import Control.Monad
import Control.Monad (unless, when)
import qualified Data.Array.IO as DAO
import Data.IORef
import qualified Data.Map as DM
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector.Mutable as DVM
import qualified Data.Vector.Storable as STO
import qualified Data.Vector.Storable.Mutable as STOM
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU.Matrix as GM
import qualified Graphics.UI.GLFW as G
import qualified Graphics.UI.GLUT as GLUT
import System.Exit
import System.IO
import qualified Text.Printf as PR


-- |
--    === Draw Triangle
--
--
--
--                0.5 ← v₀
--                 |
--                 |
--                 |
--        - 0.25 - + - 0.25
--           ↑           ↑
--           v₁          v₂
drawTriangle :: IO ()
drawTriangle =
  preservingMatrix $ do
    translate (Vector3 0 0 0 :: Vector3 GLdouble)
    renderPrimitive Triangles $ do
      color (Color3 1 0 0 :: Color3 GLdouble)
      vertex (Vertex3 0 0.5 0 :: Vertex3 GLdouble) -- v₀
      color (Color3 0 1 0 :: Color3 GLdouble)
      vertex (Vertex3 (ne 0.25) 0 0 :: Vertex3 GLdouble) -- v₁
      color (Color3 0 0 1 :: Color3 GLdouble)
      vertex (Vertex3 0.25 0 0 :: Vertex3 GLdouble) -- v₂
  where
    ne = negate

-- |
--    === Draw Triangle specified location and size
--                             ↑                ↑
--                             v               0.1
--    >let v = Vector3 0.0 0 0
--    >drawTriangle' v 0.1
drawTriangle' :: Vector3 GLdouble -> GLdouble -> IO ()
drawTriangle' v s =
  preservingMatrix $ do
    translate v
    renderPrimitive Triangles $ do
      color (Color3 1 0 0 :: Color3 GLdouble)
      vertex (Vertex3 (negate s) (negate s) 0 :: Vertex3 GLdouble)
      color (Color3 0 1 0 :: Color3 GLdouble)
      vertex (Vertex3 s (negate s) 0 :: Vertex3 GLdouble)
      color (Color3 0 0 1 :: Color3 GLdouble)
      vertex (Vertex3 0 s 0 :: Vertex3 GLdouble)

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
  b <- action
  unless b falseAction
  
{--
unlessX' :: Monad m => m Bool -> m Bool -> m () -> m ()
unlessX' action1 action2 falseAction = do
  b1 <- action1
  b2 <- action2
  unless (b1 || b2) falseAction
--}  

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
  Nothing -> nothingRes
  Just x -> f x

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description

printGLVersion :: G.Window -> IO ()
printGLVersion window = do
  major <- G.getWindowContextVersionMajor window
  minor <- G.getWindowContextVersionMinor window
  rev <- G.getWindowContextVersionRevision window
  putStrLn $ "OpenGL context version = " ++ show major ++ "." ++ show minor ++ "." ++ show rev

-- |
--    === Torus paremater equation center at (0, 0, 0)
--
--    Torus equation: <http://xfido.com/html/indexThebeautyofTorus.html Torus>
--
--    * drawPrimitive Points red torus
torus :: [(GLfloat, GLfloat, GLfloat)]
torus =
  [ ( fx i k,
      fy i k,
      fz i k
    )
    | i <- [1 .. n],
      k <- [1 .. n]
  ]
  where
    del = rf (2 * pi / (n -1))
    n = 100
    r = 0.2
    br = 0.3

    fx = \i k -> (br + r ** cos (del * i)) * cos (del * k)
    fy = \i k -> sin (rf del * i)
    fz = \i k -> (br + r * cos (rf del * i)) * sin (rf del * k)

-- |
--    === Draw torus with small and large radius
--
--    >mapM_ (\lo -> drawPrimitive LineLoop red lo) $ torusR 0.1 0.2
torusR :: GLfloat -> GLfloat -> [[(GLfloat, GLfloat, GLfloat)]]
torusR r br =
  [ [ ( (br + r * cos (del * i)) * cos (del * k),
        sin (rf del * i),
        (br + r * cos (rf del * i)) * sin (rf del * k)
      )
      | i <- [1 .. n]
    ]
    | k <- [1 .. n]
  ]
  where
    del = rf (2 * pi / (n -1))
    n = 100

-- |
--    === sphere rotates around y-axis
--
--    === cut the sphere alone the xz-plane
--
--    * The radius of the circle is \( r \cos \alpha \)
--
--    * The circle on the xz-plane is
--
--    \[
--        \begin{equation}
--        \begin{aligned}
--        x &= r \cos \alpha \sin \beta \\
--        z &= r \cos \alpha \cos \beta \\
--        \end{aligned}
--        \end{equation}
--    \]
--
--    * The center of the circle from the center of sphere is \( r \cos \alpha \sin \beta \)
--
--    * Put all together
--    \[
--        \begin{equation}
--        \begin{aligned}
--        x &= r \cos \alpha \sin \beta \\
--        y &= r \sin \beta \\
--        z &= r \cos \alpha \cos \beta \\
--        \end{aligned}
--        \end{equation}
--    \]
sphere :: GLfloat -> [[(GLfloat, GLfloat, GLfloat)]]
sphere r =
  [ [ ( (r * cos (del * i)) * cos (del * k),
        sin (rf del * i),
        (r * cos (rf del * i)) * sin (rf del * k)
      )
      | i <- [1 .. n]
    ]
    | k <- [1 .. n]
  ]
  where
    del = rf (2 * pi / (n -1))
    n = 40

torus' :: [[(GLfloat, GLfloat, GLfloat)]]
torus' =
  [ [ ( (br + r * cos (del * i)) * cos (del * k),
        sin (rf del * i),
        (br + r * cos (rf del * i)) * sin (rf del * k)
      )
      | i <- [1 .. n]
    ]
    | k <- [1 .. n]
  ]
  where
    del = rf (2 * pi / (n -1))
    n = 100
    r = 0.3
    br = 0.3

parabolic :: [[(GLfloat, GLfloat, GLfloat)]]
parabolic = [[(x' * i, y' * j, 2 * (x' * i) ^ 2 + 3 * (y' * j) ^ 2) | i <- [- n .. n]] | j <- [- n .. n]]
  where
    n = 20
    del = rf (1 / n)
    x' = del
    y' = del

-- x^4 + y^2 + z^6 = 6
surface1 :: [[(GLfloat, GLfloat, GLfloat)]]
surface1 = [[(u * d, v * d, (c - (u * d) ^ 4 - (v * d) ^ 2) ** (1 / 6)) | u <- [- n .. n]] | v <- [- n .. n]]
  where
    n = 160
    d = rf (1 / n)
    c = 0.1

surface2 :: [[(GLfloat, GLfloat, GLfloat)]]
surface2 = [[(u * d, v * d, - (c - (u * d) ^ 4 - (v * d) ^ 2) ** (1 / 6)) | u <- [- n .. n]] | v <- [- n .. n]]
  where
    n = 100
    d = rf (1 / n)
    c = 0.1

surface3 :: [[(GLfloat, GLfloat, GLfloat)]]
surface3 = [[let x' = x * d; y' = y * d in (x', y', 1 - (1 - x') ^ 2 - 100 * (y' - x' ^ 2) ^ 2) | y <- [- n .. n]] | x <- [- n .. n]]
  where
    n = 100
    d = rf (1 / n)
    c = 0.1

lightingInfo :: IO ()
lightingInfo = do
  diffuse (Light 0) $= lightDiffuse
  ambient (Light 0) $= lightAmbient
  specular (Light 0) $= lightSpecular
  position (Light 0) $= lightPosition
  light (Light 0) $= Enabled
  -- lighting           $= Enabled
  depthFunc $= Just Lequal
  blend $= Enabled
  lineSmooth $= Enabled

-- |
--    === Generate random Vertex3 in xy-plan, Vertex3 x y 0
randomVertex :: Integer -> IO [Vertex3 GLfloat]
randomVertex n = do
  ranl <- randomDouble (fromIntegral n)
  let ranlist = map (\x -> 1.5 * x) ranl
  let vexList = fmap (\x -> x - 0.5) $ fmap realToFrac ranlist
  let vexTuple = map (\x -> tripleToVertex3 x) $ filter (\x -> length x == 3) $ partList 3 vexList
        where
          tripleToVertex3 :: [GLfloat] -> Vertex3 GLfloat
          tripleToVertex3 [a, b, c] = Vertex3 a b 0.0
  return vexTuple

{--
 Sun  3 Mar 16:12:57 2024 
 Move to AronGraphic

renderText :: String -> IO ()
renderText str = do
  preservingMatrix $ do
    GL.scale (1 / scaleFont :: GL.GLdouble) (1 / scaleFont) 1
    GLUT.renderString GLUT.Roman str
--}

-- |
--    KEY: save image, save png, opengl save image, save png opengl
--
--    DATE: Thursday, 20 April 2023 12:07 PDT
--    NOTE: The image will be up side down
--    TODO: Fix it
--
--    https://www.reddit.com/r/haskell/comments/dee0iz/converting_opengl_window_to_a_png_image_in_haskell/
--    read pixels, convert them to first a Vector then a JuicyPixel image
--    and save the image as a PNG file
--
--
--    Graphics.Rendering.OpenGL.GL.ReadCopyPixels readPixels :: Position -> Size -> PixelData a -> IO ()
--    readPixels::(Position x y) -> (Size w h) -> PixelData a -> IO ()
--
--    readPixels (Position x y) (Size w h) pd =
--      withPixelData pd $ glReadPixels x y w h
--
--
--    data PixelData a = PixelData PixelFormat DataType (Ptr a)
--
--    https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadPixels.xhtml
--
--    glReadPixels(GLint x       ,
--                 GLint y       ,
--                 GLsizei width ,
--                 GLsizei height,
--                 GLenum  format,
--                 GLenum  type  ,
--                 void * data);
saveImageOpenGL :: G.Window -> FilePath -> IO ()
saveImageOpenGL window fp = do
  -- (Int, Int) <- G.getFramebufferSize window
  (w, h) <- G.getFramebufferSize window
  let w' = fromIntegral w
  let h' = fromIntegral h
  -- currentWindow $= Just window
  -- Size w h <- get windowSize
  let npixels = fromIntegral (w * h) :: Int
      nbytes = 3 * npixels
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

-- |
--
--  @
--    b = 0.3 :: GLfloat
--    p0 = Vertex3 b    b    (-b)
--    p1 = Vertex3 (-b) b    (-b)
--    p2 = Vertex3 (-b) b    b
--    p3 = Vertex3 b    b    b
--
--    let ls_top = [p0, p1, p2, p3]
--
--    let lc = [green, blue, yellow, cyan]
--
--    drawQuadColor lc ls_top
--  @
drawQuadColor :: [Color3 GLdouble] -> [Vertex3 GLfloat] -> IO ()
drawQuadColor lc ls = do
  let lt = zip lc ls
  renderPrimitive Quads $
    mapM_
      ( \(c, v) -> do
          color c
          vertex v
      )
      lt

-- |
--  @
--    b = 0.3 :: GLfloat
--    p0 = Vertex3 b    b    (-b)
--    p1 = Vertex3 (-b) b    (-b)
--    p2 = Vertex3 (-b) b    b
--    p3 = Vertex3 b    b    b
--
--    let ls_top = [p0, p1, p2, p3]
--    drawQuad green ls_top
--  @
drawQuad :: Color3 GLdouble -> [Vertex3 GLfloat] -> IO ()
drawQuad c ls = do
  renderPrimitive Quads $
    mapM_
      ( \v -> do
          color c
          vertex v
      )
      ls

{--
-- |
-- -
--  KEY: draw cube with quad
--  @
--  @
drawCubeQuad :: GLfloat -> IO ()
drawCubeQuad r =
  let nfaces = zip3 n [(green, gray), (cyan, blue), (yellow, green), (gray, blue), (white, yellow), (blue, cyan)] facesx
   in do
        mapM_
          ( \(n, (c1, c2), [v0, v1, v2, v3]) -> do
              renderPrimitive Quads $ do
                normal n
                color c1
                vertex v0
                color c2
                vertex v1
                vertex v2
                vertex v3
          )
          nfaces
  where
    n :: [Normal3 GLfloat]
    n =
      [ Normal3 (-1.0) 0.0 0.0,
        Normal3 0.0 1.0 0.0,
        Normal3 1.0 0.0 0.0,
        Normal3 0.0 (-1.0) 0.0,
        Normal3 0.0 0.0 1.0,
        Normal3 0.0 0.0 (-1.0)
      ]
    faces :: [[Vertex3 GLfloat]]
    faces =
      [ [ v 0, v 1, v 2, v 3],
        [ v 3, v 2, v 6, v 7],
        [ v 7, v 6, v 5, v 4],
        [ v 4, v 5, v 1, v 0],
        [ v 5, v 6, v 2, v 1],
        [ v 7, v 4, v 0, v 3]
      ]
    v :: Int -> Vertex3 GLfloat
    v x = Vertex3 v0 v1 v2
      where
        v0
          | x == 0 || x == 1 || x == 2 || x == 3 = - r
          | x == 4 || x == 5 || x == 6 || x == 7 = r
        v1
          | x == 0 || x == 1 || x == 4 || x == 5 = - r
          | x == 2 || x == 3 || x == 6 || x == 7 = r
        v2
          | x == 0 || x == 3 || x == 4 || x == 7 = r
          | x == 1 || x == 2 || x == 5 || x == 6 = - r
    facesx :: [[Vertex3 GLfloat]]
    facesx =
      [ [v0, v1, v2, v3],
        [v3, v2, v6, v7],
        [v7, v6, v5, v4],
        [v4, v5, v1, v0],
        [v5, v6, v2, v1],
        [v7, v4, v0, v3]
      ]
    v0 = Vertex3 (- r) (- r) r
    v1 = Vertex3 (- r) (- r) (- r)
    v2 = Vertex3 (- r) r (- r)
    v3 = Vertex3 (- r) r r
    v4 = Vertex3 r (- r) r
    v5 = Vertex3 r (- r) (- r)
    v6 = Vertex3 r r (- r)
    v7 = Vertex3 r r r
--}
