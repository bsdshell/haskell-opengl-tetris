module TetrisLib where

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
import qualified Graphics.Rendering.FTGL as FTGL

_STEP = 1.0

data RectGrid = RectGrid
  { minX_ :: Float,
    maxX_ :: Float,
    minY_ :: Float,
    maxY_ :: Float,
    minZ_ :: Float,
    maxZ_ :: Float,
    xCount_ :: Int,
    yCount_ :: Int,
    zCount_ :: Int,
    xEdge_ :: Float,
    yEdge_ :: Float,
    zEdge_ :: Float,
    rotStep :: Int
  }
  deriving (Show, Eq)

data Cam = Cam {alpha :: Double, beta :: Double, gramma :: Double, dist :: Double} deriving (Show)

data CameraRot = CameraRot
  { alpha_ :: Double,
    vecRotX_ :: Vector3 GLdouble,
    beta_ :: Double,
    vecRotY_ :: Vector3 GLdouble,
    gramma_ :: Double,
    vecRotZ_ :: Vector3 GLdouble,
    xyzRotDeg_ :: Double,
    xyzRotVec_ :: Vector3 GLdouble,
    currXYZ_ :: Int,  -- x-axis = 1, y-axis = 2, z-axis = 3
    
    dist_ :: Double,
    fovDeg_ :: Double,
    delta_ :: Step,
    persp_ :: Persp,
    modelview_ :: ModelView,

    
    rotX_ :: GLdouble,
    vecX_ :: Vector3 GLfloat,
    vecAxisX_ :: Vector3 GLfloat,
    rotY_ :: GLdouble,
    vecY_ :: Vector3 GLfloat,
    vecAxisY_ :: Vector3 GLfloat,
    rotZ_ :: GLdouble,
    vecZ_ :: Vector3 GLfloat,
    vecAxisZ_ :: Vector3 GLfloat,
    

    coordFrame_ :: (Vector3 GLfloat, Vector3 GLfloat, Vector3 GLfloat),
    coordFrameMat_ :: [[GLfloat]],
    rotSpeed_ :: GLfloat,
    isShownGrid_ :: Bool,
    isShownAxis_ :: Bool
  }
  deriving (Eq, Show)

data Persp = Persp
  { fov_ :: Double,
    aspect_ :: Double,
    zf_ :: Double,
    zn_ :: Double
  }
  deriving (Eq, Show)

data ModelView = ModelView
  { eye_ :: Vertex3 GLdouble,
    at_ :: Vertex3 GLdouble,
    up_ :: Vector3 GLdouble
  }
  deriving (Eq, Show)

data Step = Step {xx :: Double, yy :: Double, zz :: Double, ww :: Double} deriving (Eq, Show)

data XYZAxis = XYZAxis {xa :: Bool, ya :: Bool, za :: Bool} deriving (Eq, Show)

data BlockAttr = BlockAttr
  { isFilled_ :: Bool,
    typeId_ :: Int,
    tetrisCount_ :: Int,
    color_ :: Color3 GLdouble,
    vMat_ :: [[GLfloat]]
  }
  deriving (Show, Eq)

type BufferMap = DM.Map String (String, [String])

data GlobalRef = GlobalRef
  { 
    moveX_ :: Int,
    moveY_ :: Int,
    moveZ_ :: Int,
    rectGrid_ :: RectGrid,
    count1_ :: Integer,
    rotN_ :: Int,
    -- bk2_ :: [[(Int, Int, Color3 GLdouble)]],
    --           |    |
    --           |    |
    --           |  block type rep count
    --           |
    --        block type
    --                                      shape
    --                        color           |
    --            tetris type   |             |
    --       Global ID  |       |             |
    --             |    |       |             |
    currTetris_:: (BlockAttr, [[Int]]),
    currTetris3_:: (BlockAttr, [[[Int]]]),
    isPaused_ :: Bool,
    font_ :: FTGL.Font,
    nRow_ :: Int,
    rotAxis_ :: Int,
    rotDeg_ :: GLdouble,
    rotDegkk_ :: GLdouble,
    tetFrame_ :: (Vector3 GLfloat, Vector3 GLfloat, Vector3 GLfloat),
    tetFrameMat_ :: [[GLfloat]],
    flipFrame_ :: ((Int, GLfloat), (Int, GLfloat), (Int, GLfloat)),
    resetGame_ :: Bool,
    interval0_ :: Int,
    interval1_ :: Int,
    interval2_ :: Int,
    xxMat_ :: [[GLfloat]],
    dropY_ :: Int,
    dropHeight_ :: Int
  }
  deriving (Show)

initCam = Cam {alpha = 0.0, beta = 0.0, gramma = 0.0, dist = 0.0}

initModuleView :: ModelView 
initModuleView = ModelView {eye_ = Vertex3 0 0 1.0, at_ = Vertex3 0 0 0, up_ = Vector3 0 1.0 0}
initPersp :: Persp
initPersp = Persp {fov_ = 90, aspect_ = 1.0, zn_ = 0.2, zf_ = 4.5}

initCameraRot =
  CameraRot
    { alpha_ = 0.0,
      vecRotX_ = Vector3 1 0 0,
      beta_ = 0.0,
      vecRotY_ = Vector3 0 1 0,
      gramma_ = 0.0,
      vecRotZ_ = Vector3 0 0 1,
      xyzRotDeg_ = 0.0,
      xyzRotVec_ = Vector3 1 0 0,
      currXYZ_ = 1,
      
      dist_ = 0.0,
      fovDeg_ = 60.0,
      delta_ = Step {xx = 0, yy = 0, zz = 0, ww = 0},
      persp_ = initPersp,
      modelview_ = initModuleView,
       
      rotX_ = 0.0,
      vecX_ = Vector3 1 0 0,
      vecAxisX_ = Vector3 1 0 0,
      rotY_ = 0.0,
      vecY_ = Vector3 0 1 0,
      vecAxisY_ = Vector3 0 1 0,
      rotZ_ = 0.0,
      vecZ_ = Vector3 0 0 1,
      vecAxisZ_ = Vector3 0 0 1,
      
      coordFrame_ = (Vector3 1 0 0, Vector3 0 1 0, Vector3 0 0 1),
      coordFrameMat_ = matId 4,
      rotSpeed_ = 2.0,
      isShownGrid_ = True,
      isShownAxis_ = True
    }


initStep = Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = 0.01}

-- Thursday, 31 October 2024 11:35 PDT
-- keyBoardCallBack :: IORef Step -> G.KeyCallback
-- DELETE: /Users/cat/myfile/bitbucket/tmp/xx_722340.hs
  
-- mouseCallback :: IORef GlobalRef -> G.MouseButtonCallback
-- DELETE: /Users/cat/myfile/bitbucket/tmp/xx_937285.hs

flipAxis :: XYZAxis -> XYZAxis -> XYZAxis
flipAxis axisOld axisNew
  | x' = XYZAxis {xa = xor x x', ya = False, za = False}
  | y' = XYZAxis {xa = False, ya = xor y y', za = False}
  | z' = XYZAxis {xa = False, ya = False, za = xor z z'}
  | otherwise = XYZAxis {xa = False, ya = False, za = False}
  where
    x = xa axisOld
    y = ya axisOld
    z = za axisOld
    x' = xa axisNew
    y' = ya axisNew
    z' = za axisNew
    xor :: Bool -> Bool -> Bool
    xor True True = False
    xor True False = True
    xor False True = True
    xor False False = False

xAxis :: XYZAxis
xAxis = XYZAxis {xa = True, ya = False, za = False}

yAxis :: XYZAxis
yAxis = XYZAxis {xa = False, ya = True, za = False}

zAxis :: XYZAxis
zAxis = XYZAxis {xa = False, ya = False, za = True}
  

-- |
--    === Camera rotates around x y z axis
--
--    @
--
--    * Press a key, trigger keyBoardCallBack to UPDATE Ref Step
--    * Increase/decrease _STEP (degree), modify the ref Step
--                                                        ↑
--    *                               +-------------------+
--                                    ↓
--    * keyboardRot IORef Cam -> IORef Step -> Double -> Double -> IO()
--    * Update Cam::{alpha, beta, gramma, dist}
--
--    @
--    @
--    data Cam = Cam{alpha::Double, beta::Double, gramma::Double, dist::Double} deriving(Show)
--
--    data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)
--
--    IORef Cam  => ref
--    IORef Step => refStep
--    keyboardRot ref refStep (fromIntegral width) (fromIntegral height)
--    @
keyboardRot :: IORef Cam -> IORef Step -> Double -> Double -> IO ()
keyboardRot refCam refStep w h = do
  step <- readIORef refStep -- data Step = {...} refStep is not modified actually.
  modifyIORef refCam (\c -> c {alpha = alpha c + xx step})
  modifyIORef refCam (\c -> c {beta = beta c + yy step})
  modifyIORef refCam (\c -> c {gramma = gramma c + zz step})
  modifyIORef refCam (\c -> c {dist = ww step})
  cam <- readIORef refCam -- data Cam = {...}
  -- modifyIORef refCam (rx (xx step))  -- Update Camera x-axis
  -- modifyIORef refCam (ry (yy step))  -- Update Camera y-axis
  -- modifyIORef refCam (rz (zz step))  -- Update Camera z-axis
  -- modifyIORef refCam (rw (ww step))  -- Not been used for now
  -- Tuesday, 09 November 2021 12:39 PST
  -- TODO: fix the rotation issue
  -- ERROR: after the rotation of x-axis, y-axis and z-axis are not longer standard axes any more
  -- M-x gx
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html#v:rotate
  rotate (alpha cam) (Vector3 1 0 0 :: Vector3 GLdouble) -- rotate x-axix alpha degree/radian
  --                ↑→ degree
  rotate (beta cam) (Vector3 0 1 0 :: Vector3 GLdouble) -- rotate y-axis beta  degree/radian
  --                ↑→ degree
  rotate (gramma cam) (Vector3 0 0 1 :: Vector3 GLdouble) -- rotate z-axis gamma degree/radian
  --                ↑→ degree
  -- rotate ( beta   cam) $ ( Vector3 0 1 0 :: Vector3 GLdouble)  -- rotate y-axis beta  degree/radian

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
  b <- action
  unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
  Nothing -> nothingRes
  Just x -> f x

