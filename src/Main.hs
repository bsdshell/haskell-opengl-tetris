{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE CPP #-}

module Main where



-- import Graphics.GL.Types

-- Saturday, 07 October 2023 11:46 PDT
-- Complex number,
-- c = 1 :+ 2

-- Hidden modules
-- import Graphics.Rendering.OpenGL.GL.Capability
-- import Graphics.Rendering.OpenGL.GL.Exception
-- import Graphics.Rendering.OpenGL.GL.MatrixComponent
-- import Graphics.Rendering.OpenGL.GL.PeekPoke
-- import Graphics.Rendering.OpenGL.GL.QueryUtils
-- import Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit
-- import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
-- import Graphics.GL

-- BEG
-- KEY: ansi color, console color
-- import Rainbow
-- import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
-- https://hackage.haskell.org/package/ansi-terminal-0.10.3/docs/System-Console-ANSI.html
-- import System.IO (hFlush, stdout)
-- import qualified System.Console.ANSI as AN
-- END

-- import Graphics.UI.GLUT
-- import Graphics.UI.GLUT.Callbacks.Global
import AronGraphic
import AronModule
import AronAlias
import AronHtml2
import AronToken
import AronOpenGL (saveImageOpenGL)
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Lens
    ( Field1(_1), Field2(_2), Field3(_3), Field4(_4), (<&>), (^.) )
-- import Control.Monad
import Control.Monad (unless, when, join)
import qualified Control.Monad.State as CMS
-- import AronDevLib

import Data.Array.IO
import qualified Data.Array.IO as DAO
import Data.Complex
import Data.IORef
    ( modifyIORef, writeIORef, readIORef, newIORef, IORef )
import Data.Int
import qualified Data.List as DL
import qualified Data.Map as DM
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.StateVar
-- import Data.Typeable
import Data.Typeable (typeOf)
import qualified Text.Read as DT
import qualified Data.Vector as VU
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Float.RealFracMethods
import Graphics.Rendering.OpenGL
    ( viewport,
      perspective,
      loadIdentity,
      matrixMode,
      preservingMatrix,
      renderPrimitive,
      ComparisonFunction(Lequal),
      ClearBuffer(DepthBuffer, ColorBuffer),
      Size(Size),
      Position(Position),
      MatrixMode(Modelview, Projection),
      Matrix(getMatrixComponents, newMatrix),
      MatrixOrder(RowMajor, ColumnMajor),
      GLmatrix,
      MatrixComponent(rotate, translate),
      Vector3(..),
      Vertex(vertex),
      Vertex4(Vertex4),
      Normal(normal),
      Normal3(..),
      Color(color),
      PrimitiveMode(Triangles, TriangleStrip, LineLoop, Quads, TriangleFan, Points),
      GLdouble,
      Color3(..),
      Vertex3(..),
      Capability(Enabled),
      GLfloat )
import Graphics.Rendering.OpenGL as GL
  ( GLdouble,
    MatrixComponent (scale),
    clear,
    cullFace,
    depthFunc,
    lookAt,
    matrix,
    Capability(Enabled),
    blend,
    multMatrix,
  )
import qualified Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GLU.Matrix as GM
import qualified Graphics.UI.GLFW as G
import qualified Graphics.UI.GLUT as GLUT
import Language.Haskell.Interpreter
import System.Posix.Unistd
import System.Directory
import System.Process
import System.Environment
import System.Exit
import System.IO
import qualified Text.Printf as PR
import System.IO.Silently
import TetrisLib

-- |
--
--   | --------------------------------------------------------------------------------
--   | compile: run.sh
--   | ghc -i/Users/cat/myfile/bitbucket/haskelllib -o file file.hs
--   |
--   | KEY: keyboard example, keypress example, modifyIORef example,
--   |
--   | Tuesday, 09 November 2021 11:56 PST
--   |
--   | TODO: Combine Cam{..} and Step{..} in one function with Keyboard input
--   |     Current issue: try to implement orthogonal projective with key one press
--   |                    but Step{..} and Cam{..} are different type class.
--   |
--   | mainLoop w refCam refStep refCount lssVex
--   | keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)
--
--   @
--   data Cam = Cam{alpha::Double, beta::Double, gramma::Double, dist::Double} deriving(Show)
--
--   data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)
--   initCam = Cam{alpha=0.0, beta=0.0, gramma=0.0, dist = 0.0}
--   initStep = Step{xx=0.0, yy=0.0, zz=0.0, ww = 0.01}
--
--   SEE: Check the following code why the cube does not draw properly
--   NOTE: /Users/aaa/myfile/bitbucket/opengl/KeyBoardRotate/cube.c
--   @
tmpfile = "/tmp/tmpfile.txt"

convexPts :: IO [Vertex3 GLfloat]
convexPts = return cx
  where
    cx =
      [ Vertex3 0.1 0.1 0,
        Vertex3 0.2 0.6 0,
        Vertex3 0.88 0.9 0,
        Vertex3 0.25 0.34 0,
        Vertex3 0.12 0.8 0,
        Vertex3 1.3 0.12 0
      ]

helpme :: IO ()
helpme = do
  let (+) = (++)
  b <- en "b"
  -- AronModule.clear
  let ls =
        [ "file => " + b + "/tmp/draw.x",
          "PlotGeometry -h => help     ",
          "                            ",
          "point                       ",
          "0.1 0.1 0.1                 ",
          "endpoint                    ",
          "                            ",
          "Support primitives:         ",
          "point, segment and triangle "
        ]
  printBox 4 ls


-- |
--   KEY: random color, get color
randomColor :: IO (Color3 GLdouble)
-- randomColor = randomInt 0 (len ls - 1) >>= \x -> return $ ls !! x
randomColor = randomInt 0 (len lt - 1) >>= \x -> return $ lt !! x
  where
    lt =
      [ green,
        yellow,
        gray,
        blue,
        cyan,
        white
      ]

    ls =
      [ Color3 0.8 0.2 0.1,
        Color3 0.3 0.1 0.9,
        Color3 0.3 0.7 0.9,
        Color3 0.4 0.1 0.1,
        Color3 0.3 0.6 0.9,
        Color3 0.4 0.1 0.3,
        Color3 0.3 0.4 0.9,
        Color3 0.6 0.1 0.9,
        Color3 0.3 0.7 0.9,
        Color3 0.4 0.1 0.5,
        Color3 0.3 0.8 0.9,
        Color3 0.1 0.6 0.4
      ]

-- (a -> b -> c) => (a -> b -> a)
randomColorList :: Int -> IO [Color3 GLdouble]
randomColorList n = mapM (const randomColor) [1..n]
  
grid :: [[[Vertex3 GLfloat]]]
grid = [[[Vertex3 a b (a * a - b * b) | a <- aa] | b <- bb] | c <- cc]
  where
    n = 10
    fa = 1 / n
    aa = map (\x -> fa * x) [1 .. n]
    bb = map (\x -> fa * x) [1 .. n]
    cc = map (\x -> fa * x) [1 .. n]



maybeX' :: (Maybe a) -> b -> (a -> b) -> b
maybeX' m b f = case m of
                  Just x -> f x
                  Nothing -> error "err"

-- |
-- renderText :: GLUT.Font -> String -> IO ()
-- renderText font str = do
--     GL.scale (1/64 :: GL.GLdouble) (1/64) 1
--     GLUT.renderString GLUT.Roman str
mymain :: IO ()
mymain = do
  successfulInit <- G.init
  G.windowHint (G.WindowHint'DoubleBuffer True)
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
    mw3d <- G.createWindow 1000 1000 "PlotGeometry 3D" Nothing Nothing
    -- mw2d <- G.createWindow 1000 1000 "PlotGeometry 2D" Nothing Nothing
    -- maybe' :: Maybe a -> b -> (a -> b) -> b  
    -- maybe' mw (G.terminate >> exitFailure) $ \window -> do
    -- maybeX' (mw3d, mw2d) (G.terminate >> exitFailure)  $ \(window3d, window2d) -> do      
    maybeX' mw3d (G.terminate >> exitFailure)  $ \window3d -> do      
      -- ref <- newIORef initCam
      refCamRot <- newIORef initCameraRot
      refStep <- newIORef initStep

      let fontPath = "/Users/cat/Library/Fonts/FreeSans.ttf"
      font <- FTGL.createTextureFont fontPath 
      refGlobal <- newIORef (initGlobal font)
      globalRef <- readIORef refGlobal
      writeIORef refGlobal globalRef
      globalRef2 <- readIORef refGlobal
      refFrame <- timeNowMilli >>= \x -> newIORef FrameCount {frameTime = x, frameCount = 1, frameNewCount = 0, frameIndex = 0}

      ls <- randomIntList 10 (1, 4) >>= \cx -> return $ randomVexList (Vertex3 0.0 0.0 0.0) cx

      let rr = initRectGrid
      let nx = div (xCount_ rr) 2
      let ny = div (yCount_ rr) 2

      let blockAttr = BlockAttr {isFilled_ = False, typeId_ = 0, tetrisCount_ = 0, color_ = green}
      ioArray <- DAO.newArray ((- nx, - ny, 0), (nx - 1, ny - 1, 0)) blockAttr :: IO (IOArray (Int, Int, Int) BlockAttr)
      animaStateArr <- initAnimaState

      mainLoop window3d refCamRot refGlobal refFrame animaStateArr [] ioArray

      G.destroyWindow window3d
      G.terminate
      exitSuccess


  
  
multiModelviewMat :: [GLfloat] -> IO [GLfloat]
multiModelviewMat ls = do
  mat <- newMatrix RowMajor ls :: IO (GLmatrix GLfloat)
  GL.multMatrix mat
  getMatrixComponents RowMajor mat -- [GLfloat]



rotateN :: Int -> [[a]] -> [[a]]
rotateN n = foldl (\f g -> f . g) id $ take (abs n) $ repeat $ n >= 0 ? (reverse . DL.transpose) $ (DL.transpose . reverse)

--                                     shape
--                       color            |
--          tetris type     |             |
--     Global ID  |         |             |
--           |    |         |             |
mkTetris1 :: Int -> Int -> (Int, Int, Color3 GLdouble, [[Int]])
mkTetris1 bt bid = (bt, bid, white, bk)
  where
    bk =
      [ [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 1, 1, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
      ]
  
tetris0 :: [[Int]]
tetris0 =
        [ [0, 0, 0, 0, 0],
          [0, 0, 1, 0, 0],
          [0, 1, 1, 1, 0],
          [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0]
        ]
tetris1 :: [[Int]]
tetris1 =
        [ [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0],
          [1, 1, 1, 1, 1],
          [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0]
        ]
  
randomBlockX :: IORef GlobalRef -> IO (BlockAttr, [[Int]])
randomBlockX ref = do
  let tet1 =
        [ [0, 0, 0, 0, 0],
          [0, 0, 1, 0, 0],
          [0, 1, 1, 1, 0],
          [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0]
        ]
  let tet2 =
        [ [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0],
          [1, 1, 1, 1, 1],
          [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0]
        ]
  let tet3 =
        [ [0, 0, 0, 0, 0],
          [0, 1, 1, 1, 0],
          [0, 1, 1, 1, 0],
          [0, 1, 1, 1, 0],
          [0, 0, 0, 0, 0]
        ]
  tetrisCount <- readIORef ref <&> currTetris_ <&> fst <&> tetrisCount_
  let t1 = 1
  let t2 = 2
  let t3 = 3 
  let ls = [(tet1, white, t1), (tet2, cyan, t2), (tet3, green, t3)]
  inx <- randomInt 0 (len ls - 1)
  let br = ls !! inx
  let bb = (BlockAttr {isFilled_ = True, typeId_ = br ^. _3, tetrisCount_ = tetrisCount + 1, color_ = br ^. _2}, br ^. _1 )
  return bb

printCameraRot :: IORef CameraRot -> IO()
printCameraRot ioCameraRot = do
  readIORef ioCameraRot >>= print

printGlobalRef :: IORef GlobalRef -> IO()
printGlobalRef ioGlobalRef= do
  readIORef ioGlobalRef >>= print

drawFont:: FilePath ->  String -> GLdouble -> Color3 GLdouble -> Vector3 GLdouble -> IO()
drawFont fp s sz c v = do
    preservingMatrix $ do
        translate v 
        font <- FTGL.createTextureFont fp 
        FTGL.setFontFaceSize font 24 72 
        -- FTGL.setFontDepth font 1.0
        GL.scale (sz/scaleFont :: GL.GLdouble) (sz/scaleFont) 1 
        color c
        FTGL.renderFont font s FTGL.Front 



centerBrickX :: [[Int]] -> [[(Int, Int)]]
centerBrickX bk = map (\y -> map (\x -> (x - 2, y - 2)) [0 .. width - 1]) $ reverse [0 .. height - 1]
  where
    height = len bk
    width  = len $ head bk
  

-- /Library/WebServer/Documents/xfido/image/tetris0.png
-- tetrisCount_
-- BlockAttr
-- font_ :: FTGL.Font

initGlobal :: FTGL.Font -> GlobalRef
-- initGlobal :: GlobalRef
initGlobal font = 
  GlobalRef
    { 
      moveX_ = 0,
      moveY_ = initY,
      rectGrid_ = initRectGrid,
      count1_ = 10000000,
      rotN_ = 0,
      currTetris_ = (BlockAttr {isFilled_ = True, typeId_ = 1, tetrisCount_ = 1, color_ = blue}, tetris0),
      isPaused_ = False,
      font_ = font
    }

-- Friday, 01 November 2024 12:46 PDT
-- keyBoardCallBackNew :: IORef CameraRot -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
-- KEEP: /Users/cat/myfile/github/haskell-opengl-tetris/src/keyboardCallBackNew-2024-01-19-11-19-06.x

keyBoardCallBack3d :: IORef CameraRot -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
keyBoardCallBack3d refCamRot refGlobalRef ioArray window key scanCode keyState modKeys = do
  globalRef <- readIORef refGlobalRef
  cam <- readIORef refCamRot
  rr <- readIORef refGlobalRef <&> rectGrid_
  coordFrame <- readIORef refCamRot <&> coordFrame_
  rotSpeed <- readIORef refCamRot <&> rotSpeed_
  case keyState of
    ks
      | ks `elem` [G.KeyState'Pressed, G.KeyState'Repeating] -> do
        case key of
          k
            | k == G.Key'Right -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecXCF (rf $ (pi/180) * rotSpeed)
                         -- multiModelviewMat ls
                         let lsY = vecToList3 vecYCF ++ [0]
                         let lsZ = vecToList3 vecZCF ++ [0]
                         let vy3 = listToVec $ mm `multiVecL` lsY
                         let vz3 = listToVec $ mm `multiVecL` lsZ

                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vecXCF, vy3, vz3)})
                         mm' <- (cap . printMat) mm
                         logFileG ["mm_matrix"]
                         logFileG [mm']

                         logFileG ["vecXCF"]
                         logFileG [show vecXCF]  
                         logFileG ["n=1 vecvy3"]
                         logFileG [show vy3]
                         logFileG ["n=1 vecvz3"]
                         logFileG [show vz3]
                         logFileG ["lenn lsY lsZ"]
                         logFileG $ map show lsY
                         logFileG $ map show lsZ
  
                     | v == 2 -> do
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecYCF (rf $ (pi/180) * rotSpeed)
                         -- multiModelviewMat ls
                         let lsX = vecToList3 vecXCF ++ [0]
                         let lsZ = vecToList3 vecZCF ++ [0]
                         let vx3 = listToVec $ mm `multiVecL` lsX
                         let vz3 = listToVec $ mm `multiVecL` lsZ
                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vx3, vecYCF, vz3)})
                         mm' <- (cap . printMat) mm
                         logFileG ["mm_matrix"]
                         logFileG [mm']
  
                     | v == 3 -> do
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecZCF (rf $ (pi/180) * rotSpeed)
                         let lsX = vecToList3 vecXCF ++ [0]
                         let lsY = vecToList3 vecYCF ++ [0]
                         let vx3 = listToVec $ mm `multiVecL` lsX
                         let vy3 = listToVec $ mm `multiVecL` lsY
                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vx3, vy3, vecZCF)})
                     | v == 4 -> do
                         persp <- readIORef refCamRot <&> persp_
                         modifyIORef refCamRot (\s -> s{persp_ = persp{zn_ = zn_ persp + 0.04}})
                         persp' <- readIORef refCamRot <&> persp_
                         logFileGT "persp_1" [show persp']
  
                     | v == 5 -> return () 
                     | v == 6 -> do 
                         modifyIORef refCamRot (\s -> s{isShownGrid_ = not $ isShownGrid_ s})
                     | otherwise -> return ()  

            | k == G.Key'Left -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecXCF (rf $ (pi/180) * negate rotSpeed)
                         let lsY = vecToList3 vecYCF ++ [0]
                         let lsZ = vecToList3 vecZCF ++ [0]
                         let vy3 = listToVec $ mm `multiVecL` lsY
                         let vz3 = listToVec $ mm `multiVecL` lsZ
                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vecXCF, vy3, vz3)})
                     | v == 2 -> do
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecYCF (rf $ (pi/180) * negate rotSpeed)
                         let lsX = vecToList3 vecXCF ++ [0]
                         let lsZ = vecToList3 vecZCF ++ [0]
                         let vx3 = listToVec $ mm `multiVecL` lsX
                         let vz3 = listToVec $ mm `multiVecL` lsZ

                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vx3, vecYCF, vz3)})
  
                     | v == 3 -> do
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecZCF (rf $ (pi/180) * negate rotSpeed)
                         let lsX = vecToList3 vecXCF ++ [0]
                         let lsY = vecToList3 vecYCF ++ [0]
                         let vx3 = listToVec $ mm `multiVecL` lsX
                         let vy3 = listToVec $ mm `multiVecL` lsY
                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vx3, vy3, vecZCF)})
                     | v == 4 -> do
                         persp <- readIORef refCamRot <&> persp_
                         modifyIORef refCamRot (\s -> s{persp_ = persp{zn_ = zn_ persp - 0.04}})                             
                     | v == 5 -> return () 
                     | v == 6 -> do 
                         modifyIORef refCamRot (\s -> s{isShownAxis_ = not $ isShownAxis_ s})
                     | otherwise -> return () 

            | k == G.Key'L || k == G.Key'R -> do
              rotN <- readIORef refGlobalRef <&> rotN_
              tetX <- readIORef refGlobalRef <&> currTetris_
              let bk1'X = rotateN rotN (snd tetX)
              lsArr <- getAssocs ioArray
              let lsX = getShapeX bk1'X
              logFileG ["lsX"]
              logFileG $ map show lsX
              logFileG ["lsArr"]
              logFileG $ map show lsArr
              modifyIORef
                refGlobalRef
                ( \s ->
                    s
                      { moveX_ =
                          let mx = moveX_ s
                              my = moveY_ s
                              one = k == G.Key'L ? -1 $ 1
                              mX = map (\(a, b) -> (a + mx + one, b + my, 0)) lsX
                              b = checkMoveArr mX lsArr rr
                           in b ? mx + one $ mx
                      }
                )

              print "kk"

            | k == G.Key'D -> do
              runGame window refGlobalRef ioArray

            | k == G.Key'W -> do
              nowTime <- timeNowMilli
              modifyIORef refGlobalRef (\s -> s {count1_ = 0})
              fw "Rotate Block"
              pp "rotate me"
            | k == G.Key'P -> do
              modifyIORef refGlobalRef (\s -> s {isPaused_ = not $ isPaused_ s})
              isPaused <- readIORef refGlobalRef <&> isPaused_
              pauseTetris isPaused refGlobalRef initRectGrid ioArray
              pp "Pause"
            | k == G.Key'A -> do
              moveX <- readIORef refGlobalRef <&> moveX_
              moveY <- readIORef refGlobalRef <&> moveY_
              rotN <- readIORef refGlobalRef <&> rotN_
              tetX <- readIORef refGlobalRef <&> currTetris_
              array <- getAssocs ioArray
              let bk1'X = rotateN 0 (snd tetX)

              let currBrX = innerBrickX (moveX, moveY) bk1'X
              let currBrXX = map (\(x, y) -> (x, y, 0)) currBrX
              let bX = checkMoveArr currBrXX array rr
              modifyIORef refGlobalRef (\s -> s {count1_ = bX ? 0 $ count1_ s})
              modifyIORef refGlobalRef (\s -> s{rotN_ = bX ? (let n = (rotN_ s) + 1 in mod n 4 )$ rotN_ s})
              pp "rotateN 1"

            | k == G.Key'Up -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = _STEP, zz = 0}})
            | k == G.Key'Down -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = - _STEP, zz = 0}})
            | k == G.Key'X -> do
                modifyIORef refCamRot (\s -> let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 1.0 0}})
            | k == G.Key'1 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 1})
            | k == G.Key'2 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 2})
            | k == G.Key'3 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 3})
            | k == G.Key'4 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 4})
            | k == G.Key'5 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 5})
            | k == G.Key'6 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 6})
  
            | k == G.Key'Z -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         modifyIORef refCamRot(\s ->let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 0 1.0}})
                     | v == 2 -> do
                         modifyIORef refCamRot(\s ->let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 (-1) 0 0}})
                     | v == 3 -> do
                         modifyIORef refCamRot(\s ->let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 1.0 0 0}})
                     | v == 4 -> do
                         modifyIORef refCamRot(\s ->let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 0 (-1)}})
                     | otherwise -> return () 
  
            | k == G.Key'O -> do
                modifyIORef refCamRot (\s -> let p = persp_ s in s{persp_ = p{fov_ = fov_ p + 5.0}})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
                logFileG ["CameraRot_O"]
                readIORef refCamRot >>= \x -> logFileG [show x]
            -- zoom in
            | k == G.Key'I -> do
                modifyIORef refCamRot (\s -> let p = persp_ s in s{persp_ = p{fov_ = fov_ p - 5.0}})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
                logFileG ["CameraRot_I"]
                readIORef refCamRot >>= \x -> logFileG [show x]

            -- TODO: In orthogonal projective status,
            | k == G.Key'Space -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 || v == 2 || v == 3 || v == 4 -> do
                         modifyIORef refCamRot (\s -> s {coordFrame_ = (Vector3 1 0 0, Vector3 0 1 0, Vector3 0 0 1)})
                         modifyIORef refCamRot (\s -> s {coordFrameMat_ = matId 4})                           
                     | v == 5 -> do
                         modifyIORef refCamRot (\s -> s {coordFrame_ = (Vector3 1 0 0, Vector3 0 1 0, Vector3 0 0 1)})
                         modifyIORef refCamRot (\s -> s {coordFrameMat_ = matId 4})                           
                         modifyIORef refCamRot (\s -> s {modelview_ = initModuleView})                           
                         modifyIORef refCamRot (\s -> s {persp_ = initPersp})                           
                     | otherwise -> return () 
  
            | k == G.Key'K -> do
                pp "k"
            | otherwise -> return () 
      | ks == G.KeyState'Released -> do
        if key == G.Key'Right then do return () else return () 
        if key == G.Key'Left then do return () else return ()
        if key == G.Key'Up then do return () else return ()
        if key == G.Key'Down then do return () else return ()
      | otherwise -> return () 
  when
    (key == G.Key'Escape && keyState == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)

  
-- Friday, 01 November 2024 12:33 PDT
-- keyBoardCallBackNew2 :: IORef CameraRot -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
-- NOTE: replace with keyBoardCallBack3d
-- /Users/cat/myfile/bitbucket/tmp/xx_561634.hs

vecdTovecf :: Vector3 GLdouble -> Vector3 GLfloat
vecdTovecf (Vector3 x y z) = Vector3 x' y' z'
  where
    x' = rf x
    y' = rf y
    z' = rf z

-- |
--   === KEY: points set for sphere
--   The function from AronGraphic.hs spherePts
spherePtsX :: [[Vertex3 GLfloat]]
spherePtsX = geneParamSurface fx fy fz n
  where
    n = 20 :: Int
    δ = (2 * pi) / (rf (n -1)) :: Float
    r = 0.1
    br = 0.2
    σ = 1 / rf (n -1)

    fx :: Int -> Int -> GLfloat
    fx i j =
      let i' = rf i
          j' = rf j
          α = δ * i'
          β = δ * j'
       in r * cos (α) * cos (β)
    fy :: Int -> Int -> GLfloat
    fy i j =
      let i' = rf i
          j' = rf j
          α = δ * i'
          β = δ * j'
       in r * cos (α) * sin (β)

    fz :: Int -> Int -> GLfloat
    fz i j =
      let i' = rf i
          j' = rf j
          α = δ * i'
          β = δ * j'
       in r * sin α

-- |
--    KEY:
--
--    t0 pressed
--       (x0, y0)
--
--    t1 released
--       (x1, y1)
mouseCallbackX :: IORef GlobalRef -> G.MouseButtonCallback
mouseCallbackX globalRef window but butState mk = do
  case butState of
    G.MouseButtonState'Pressed -> do
      case but of
        v
          | v == G.MouseButton'1 -> do
            (fbw, fbh) <- G.getFramebufferSize window
            pos <- G.getCursorPos window >>= \(x, y) -> return (rf x, rf y) :: IO (GLfloat, GLfloat)
            ws <- G.getWindowSize window
            let str = PR.printf "cx=%.2f cy=%.2f wx=%d wy=%d bx=%d by=%d" (fst pos) (snd pos) (fst ws) (snd ws) fbw fbh :: String

            gRef <- readIORef globalRef                           

            pp str
          | otherwise -> pp "No button pressed"
    G.MouseButtonState'Released -> do
      -- pos <- G.getCursorPos window >>= \(x, y) -> return $ (rf x, rf y) :: IO (GLfloat, GLfloat)
      let pos = (0.0, 0.0)
      -- readIORef globalRef >>= \x -> writeIORef globalRef $ setMousePressed (False, pos) x
      pp "Button Released"

{--
             1
        -1   +(0 0)    1
            -1

--}
randomVexList :: Vertex3 GLfloat -> [Int] -> [Vertex3 GLfloat]
randomVexList _ [] = []
randomVexList v@(Vertex3 x y z) (c : cx) = case c of
  1 -> x < 1.0 ? (let u = Vertex3 (x + 0.1) y z in u : randomVexList u cx) $ randomVexList v cx
  2 -> x > -1.0 ? (let u = Vertex3 (x - 0.1) y z in u : randomVexList u cx) $ randomVexList v cx
  3 -> y < 1.0 ? (let u = Vertex3 x (y + 0.1) z in u : randomVexList u cx) $ randomVexList v cx
  4 -> y > -1.0 ? (let u = Vertex3 x (y - 0.1) z in u : randomVexList u cx) $ randomVexList v cx
  _ -> error "ERROR randomInt"

randomVexListInt :: (Int, Int) -> [Int] -> [(Int, Int)]
randomVexListInt _ [] = []
randomVexListInt v@(x, y) (c : cx) = case c of
  1 -> x < mx ? (let u = (x + 1, y) in u : randomVexListInt u cx) $ randomVexListInt v cx
  2 -> x > - mx ? (let u = (x - 1, y) in u : randomVexListInt u cx) $ randomVexListInt v cx
  3 -> y < my ? (let u = (x, y + 1) in u : randomVexListInt u cx) $ randomVexListInt v cx
  4 -> y > - my ? (let u = (x, y - 1) in u : randomVexListInt u cx) $ randomVexListInt v cx
  _ -> error "ERROR randomInt"
  where
    mx = 10
    my = 10

-- |
--                (x0, y0)
--
--                    +--
--                    |
--                         |
--                        -+ (-x0, -y0)
--
--
--                    boundary    init pt     random list   next move list
--                       |            |            |             |
randomVexListX :: (Int, Int) -> (Int, Int) -> [Int] -> [(Int, Int)]
randomVexListX _ _ [] = []
randomVexListX ix@(x0, y0) v@(x, y) (c : cx) = case c of
  1 -> x > x0 ? (let u = (x - 1, y) in u : randomVexListX ix u cx) $ randomVexListX ix v cx
  2 -> x < - x0 ? (let u = (x + 1, y) in u : randomVexListX ix u cx) $ randomVexListX ix v cx
  3 -> y < y0 ? (let u = (x, y + 1) in u : randomVexListX ix u cx) $ randomVexListX ix v cx
  4 -> y > - y0 ? (let u = (x, y - 1) in u : randomVexListX ix u cx) $ randomVexListX ix v cx
  _ -> error "ERROR randomInt"

getShapeX :: [[Int]] -> [(Int, Int)]
getShapeX tet = map fst $ join $ (map . filter) (\(_, n) -> n > 0) $ (zipWith . zipWith) (,) centerBrick tet 
  where
    centerBrick = map (\y -> map (\x -> (x - 2, y - 2)) [0 .. len (head tet) - 1]) $ reverse [0 .. len tet - 1]

getShape3 :: [[[(Int, Int, Int)]]] -> [[[Int]]] -> [(Int, Int, Int)]
getShape3 centerBrick bk = map fst $ join . join $ (map . map . filter) (\(_, n) -> n > 0) $ (zipWith . zipWith . zipWith) (,) centerBrick bk

innerBrickX :: (Int, Int) -> [[Int]] -> [(Int, Int)]
innerBrickX (moveX, moveY) tet = currBr
  where
    f x = map fst $ join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (,) centerBrick x
    r0 = tet 
    ((x0, x1), (y0, y1)) =
      let r1 = rotateN 1 r0
          r2 = rotateN 1 r1
          r3 = rotateN 1 r2
          r4 = rotateN 1 r3
          a1 = f r0
          a2 = f r1
          a3 = f r2
          a4 = f r3
          minmax x = (minimum x, maximum x)
          ls = a1 ++ a2 ++ a3 ++ a4
          tx = minmax $ map fst ls
          ty = minmax $ map snd ls
       in (tx, ty)
    cb0 = (map . filter) (\(x, _) -> x0 <= x && x <= x1) centerBrick
    cb1 = (map . filter) (\(_, y) -> y0 <= y && y <= y1) cb0
    currBr = join $ (map . map) (\(x, y) -> (x + moveX, y + moveY)) cb1
    centerBrick = map (\y -> map (\x -> (x - 2, y - 2)) [0 .. len (head tet) - 1]) $ reverse [0 .. len tet - 1]

-- |
--
--               minY_
--                |
--        minX_ - +  - -> maxX_
--                |
--               maxY_
--
--        |<-    xCount_    ->|
--                20
initRectGrid :: RectGrid
initRectGrid =
  RectGrid
    { minX_ = -1.0,
      maxX_ = 1.0,
      minY_ = -1.0,
      maxY_ = 1.0,
      xCount_ = 20,
      yCount_ = 20,
      xEdge_ = 0.1,
      yEdge_ = 0.01,
      rotStep = 20
    }

drawRectGridX :: RectGrid -> IO ()
drawRectGridX r = do
  let xc = [0 .. xCount_ r - 1]
  let yc = [0 .. yCount_ r - 1]
  mapM_ (\ny -> mapM_ (\nx -> drawBlock (nx, ny) r) xc) yc


drawBlock :: (Int, Int) -> RectGrid -> IO ()
drawBlock (nx, ny) r = do
  let wx = xEdge_ r; wy = yEdge_ r
  let v (x, y, z) = Vertex3 x y z
  -- top left corner
  let x0 = minX_ r; y0 = maxY_ r
  -- bottom right corner
  let x1 = maxY_ r; y1 = minY_ r
  let vx0 = v (x0, y0, 0)
  let vx1 = v (x1, y1, 0)
  let bWidth = (x1 - x0) / fi (xCount_ r)
  let bHeight = (y0 - y1) / fi (yCount_ r)
  -- drawRect (vx0, vx1)
  let ex0 = x0 + fi nx * bWidth + wx
      ey0 = y0 - fi ny * bHeight - wy
      ex1 = x0 + (fi nx + 1) * bWidth - wx
      ey1 = y0 - (fi ny + 1) * bHeight + wy
   in drawRectColor green (v (ex0, ey0, 0), v (ex1, ey1, 0))


-- |
--   Center Brick,
--
--        (0.1 0.1)
--    + - +
--    |   |
--    + - +
-- (0,0)
centerBlock00 :: (Int, Int) -> RectGrid -> Color3 GLdouble -> IO ()
centerBlock00 (nx, ny) r color = do
  preservingMatrix $ do
    let wx = xEdge_ r; wy = yEdge_ r
    let v (x, y, z) = Vertex3 x y z
    -- top left corner
    let x0 = minX_ r; y0 = maxY_ r
    -- bottom right corner
    -- let x1 = maxY_ r; y1 = minY_ r
    let x1 = maxX_ r; y1 = minY_ r
    let vx0 = v (x0, y0, 0)
    let vx1 = v (x1, y1, 0)
    let width = (x1 - x0) / fi (xCount_ r)
    let height = (y0 - y1) / fi (yCount_ r)
    let vo = Vertex3 0 0 0
    let vf = Vertex3 (fi nx * width) (fi ny * height) 0
    let vm = vo -: vf 
    translate vm
    preservingMatrix $ do
      let x = width / 2
      let y = height / 2
      let vf' = Vertex3 x y 0 :: (Vertex3 GLfloat)
      let ov' = Vertex3 0 0 0 :: (Vertex3 GLfloat)
      let vv = ov' -: vf' 
      translate vv
      drawRectFill2dX color (width, height)
  where
    vxz = Vertex3 0 0 0


drawRectGrid :: [Int] -> [Int] -> GLfloat -> IO ()
drawRectGrid sx sy e = do
  let wx = e; wy = e
  let v (x, y, z) = Vertex3 x y z
  -- top left corner
  let x0 = -1.0 :: GLfloat; y0 = 1.0 :: GLfloat
  -- bottom right corner
  let x1 = 1.0; y1 = -1.0
  let vx0 = v (x0, y0, 0)
  let vx1 = v (x1, y1, 0)
  let delx = (x1 - x0) / 20.0
  let dely = (y0 - y1) / 20.0
  -- drawRect (vx0, vx1)
  mapM_
    ( \ny ->
        mapM_
          ( \nx ->
              let ex0 = x0 + fi nx * delx + wx
                  ey0 = y0 - fi ny * dely - wy
                  ex1 = x0 + (fi nx + 1) * delx - wx
                  ey1 = y0 - (fi ny + 1) * dely + wy
               in drawRectColor green (v (ex0, ey0, 0), v (ex1, ey1, 0))
          )
          sx
    )
    sy

rotateBlock :: IORef GlobalRef -> RectGrid -> IO ()
rotateBlock refGlobal r = do
  preservingMatrix $ do
    count1 <- readIORef refGlobal <&> count1_
    mx <- readIORef refGlobal <&> moveX_
    my <- readIORef refGlobal <&> moveY_
    let mx' = fi mx
    let my' = fi my
    let r = initRectGrid
    let x0 = minX_ r; y0 = maxY_ r
    -- bottom right corner
    let x1 = maxX_ r; y1 = minY_ r
    let width = (x1 - x0) / fi (xCount_ r)
    let height = (y0 - y1) / fi (yCount_ r)
    let x0 = let w = width / 2 in w + mx' * width
    let y0 = let h = height / 2 in h + my' * height
    let vo = Vertex3 0 0 0
    let vf = Vertex3 x0 y0 0
    let vm = vo -: vf 

    let del = 90 / fi (rotStep r)
    let deln = del * fi count1
    rotate deln (Vector3 0 0 1 :: Vector3 GLdouble)

    rotN <- readIORef refGlobal <&> rotN_
    tetX <- readIORef refGlobal <&> currTetris_
    let bk1'X = rotateN rotN (snd tetX)
    let lsX = getShapeX bk1'X
    mapM_
      ( \(x, y) -> do
          preservingMatrix $ do
            centerBlock00 (x + mx, y + my) initRectGrid ((color_ . fst) tetX)
      )
      lsX
    pp "ok"

drawRectGridColor :: Color3 GLdouble -> [Int] -> [Int] -> GLfloat -> IO ()
drawRectGridColor color sx sy e = do
  let wx = e; wy = e
  let v (x, y, z) = Vertex3 x y z
  -- top left corner
  let x0 = -1.0 :: GLfloat; y0 = 1.0 :: GLfloat
  -- bottom right corner
  let x1 = 1.0; y1 = -1.0
  let vx0 = v (x0, y0, 0)
  let vx1 = v (x1, y1, 0)
  let delx = (x1 - x0) / 20.0
  let dely = (y0 - y1) / 20.0
  -- drawRect (vx0, vx1)
  mapM_
    ( \ny ->
        mapM_
          ( \nx ->
              let ex0 = x0 + fi nx * delx + wx
                  ey0 = y0 - fi ny * dely - wy
                  ex1 = x0 + (fi nx + 1) * delx - wx
                  ey1 = y0 - (fi ny + 1) * dely + wy
               in drawRectColor color (v (ex0, ey0, 0), v (ex1, ey1, 0))
          )
          sx
    )
    sy

-- ,boardMap_ :: DM.Map (Int, Int) ((Vertex3 GLfloat), Color3 GLdouble, Bool)
addBlock :: DM.Map (Int, Int) ((Vertex3 GLfloat), Color3 GLdouble, Bool) -> [((Int, Int), Color3 GLdouble)] -> DM.Map (Int, Int) ((Vertex3 GLfloat), Color3 GLdouble, Bool)
addBlock dm [] = dm
addBlock dm (s : cx) = DM.insert (fst s) (Vertex3 0 0 0, snd s, True) $ addBlock dm cx

addBlockX :: DM.Map (Int, Int) (Int, Int, Color3 GLdouble) -> [((Int, Int), (Int, Int, Color3 GLdouble))] -> DM.Map (Int, Int) (Int, Int, Color3 GLdouble)
addBlockX dm [] = dm
addBlockX dm (s : cx) = DM.insert (fst s) (snd s) $ addBlockX dm cx

-- DM.Map (Int, Int) ((Vertex3 GLfloat), Color3 GLdouble, Bool)
showCurrBoard :: DM.Map (Int, Int) ((Vertex3 GLfloat), Color3 GLdouble, Bool) -> IO ()
showCurrBoard bmap = do
  preservingMatrix $ do
    let lz = map (\x -> (x ^. _1, x ^. _2 ^. _2)) $ DM.toList bmap
    mapM_
      ( \((a, b), c) -> do
          preservingMatrix $ do
            centerBlock00 (a, b) initRectGrid c
      )
      lz
    pp "ok"

showCurrBoardX :: DM.Map (Int, Int) (Int, Int, Color3 GLdouble) -> IO ()
showCurrBoardX bmap = do
  preservingMatrix $ do
    let lz = map (\(xy, (_, _, c)) -> (xy, c)) $ DM.toList bmap
    mapM_
      ( \((a, b), c) -> do
          preservingMatrix $ do
            centerBlock00 (a, b) initRectGrid c
      )
      lz
    pp "ok"

showCurrBoardArr :: IOArray (Int, Int, Int) BlockAttr -> IO ()
showCurrBoardArr arr = do
  ls <- getAssocs arr
  preservingMatrix $ do
    mapM_
      ( \((x, y, _), b) ->
          preservingMatrix $ do
            when (isFilled_ b) $ do
              centerBlock00 (x, y) initRectGrid (color_ b)
              -- isFilled_ b ? centerBlock00 (x, y) initRectGrid (color_ b) $ pp "not filled"
      )
      ls
  pp "ok"

currBrickX :: IORef GlobalRef -> RectGrid -> IO ()
currBrickX refGlobal rr = do
  preservingMatrix $ do
    mx <- readIORef refGlobal <&> moveX_
    my <- readIORef refGlobal <&> moveY_
    rotN <- readIORef refGlobal <&> rotN_
    tetX <- readIORef refGlobal <&> currTetris_
    let bk1'X = rotateN rotN (snd tetX)
    let lsX = getShapeX bk1'X
    mapM_
      ( \(a, b) -> do
          preservingMatrix $ do
            centerBlock00 (a + mx, b + my) rr ((color_ . fst) tetX)
      )
      lsX
    pp "ok"

-- |
--   @
--   nx = 10
--   ny = 10
--   -10 <= x0 < 10
--   -10 <= y0 < 10
--   @
checkMoveX :: [(Int, Int)] -> DM.Map (Int, Int) (Int, Int, Color3 GLdouble) -> RectGrid -> Bool
checkMoveX [] _ _ = False
checkMoveX cx sm rr =
  all
    ( \x ->
        let x0 = fst x
            y0 = snd x
         in - nx <= x0 && x0 < nx && - ny <= y0 && y0 < ny && not (x `DM.member` sm)
    )
    cx
  where
    nx = div (xCount_ rr) 2
    ny = div (yCount_ rr) 2

checkMoveArr :: [(Int, Int, Int)] -> [((Int, Int, Int), BlockAttr)] -> RectGrid -> Bool
checkMoveArr cx bls rr = isInside && not anyBlock
  where
    sm = DM.fromList $ filter (\(_, b) -> isFilled_ b) bls
    anyBlock = any (`DM.member` sm) cx
    isInside = all (\(x, y, z) -> (- nx <= x && x < nx) && (- ny <= y && y < ny)) cx
    nx = div (xCount_ rr) 2
    ny = div (yCount_ rr) 2

getBottom :: (Int, Int, Int) -> [((Int, Int, Int), BlockAttr)] -> [((Int, Int, Int), BlockAttr)]
getBottom (x0, y0, z0) = filter (\((x, y, z), _) -> y == y0)

initBlockAttr :: BlockAttr
initBlockAttr =
  BlockAttr
    { isFilled_ = False,
      typeId_ = -1,
      tetrisCount_ = -1,
      color_ = red
    }

data AnimaState = AnimaState
  { animaTime_ :: Int,
    animaIndex_ :: Int,
    animaInterval_ :: Int,
    animaSlot_ :: Int
  }
  deriving (Show, Eq)

initAnimaState :: IO (IOArray Int AnimaState)
initAnimaState = do
  currTime <- timeNowMilli <&> fi
  let an = AnimaState {animaTime_ = currTime, animaIndex_ = 0, animaInterval_ = 1000, animaSlot_ = 0}
  -- let anx = AnimaState {animaTime_ = currTime, animaIndex_ = 0, animaInterval_ = 4000, animaSlot_ = 0}
  -- DAO.newArray (0, 5) an
  let ls = replicate 100 an
  DAO.newListArray (0, len ls - 1) ls

readAnimaState :: IOArray Int AnimaState -> Int -> Int -> IO (Bool, Int, AnimaState)
readAnimaState arr ix interval = do
  currTime <- timeNowMilli <&> fi
  an <- DAO.readArray arr ix
  DAO.writeArray arr ix an {animaInterval_ = interval}
  oldTime <- DAO.readArray arr ix <&> animaTime_
  interval <- DAO.readArray arr ix <&> animaInterval_
  oldIndex <- DAO.readArray arr ix <&> animaIndex_
  let newIndex = oldIndex + 1
  let isNext = currTime - oldTime >= interval
  if isNext
    then do
      return (isNext, newIndex, an {animaTime_ = currTime, animaIndex_ = newIndex, animaSlot_ = ix})
    else do
      return (isNext, oldIndex, an {animaSlot_ = ix})

writeAnimaState :: IOArray Int AnimaState -> AnimaState -> IO ()
writeAnimaState arr state = do
  let ix = animaSlot_ state
  DAO.writeArray arr ix state

flipIsNext :: IOArray Int AnimaState -> Int -> IO ()
flipIsNext arr ix = do
  an <- DAO.readArray arr ix
  currTime <- timeNowMilli <&> fi
  writeAnimaState arr an {animaTime_ = currTime}

pauseTetris :: Bool -> IORef GlobalRef -> RectGrid -> IOArray (Int, Int, Int) BlockAttr -> IO ()
pauseTetris isPaused refGlobal rr ioArray = do
  moveX <- readIORef refGlobal <&> moveX_
  moveY <- readIORef refGlobal <&> moveY_
  rotN <- readIORef refGlobal <&> rotN_
  tetX <- readIORef refGlobal <&> currTetris_
  ls <- getAssocs ioArray

  let rotTet = rotateN rotN (snd tetX)
  let tetris = getShapeX rotTet 

  let mX = map (\(a, b) -> (a + moveX, b + moveY - 1, 0)) tetris 

  let lastBlock = map (\(a, b) -> ((a + moveX, b + moveY, 0), fst tetX)) tetris
  let lastBlock' = map (\(a, b) -> ((a + moveX, b + moveY, 0), let s = fst tetX in s {isFilled_ = False})) tetris
  logFileG ["lsxx"]
  logFileG $ map show lastBlock
  if isPaused
    then do
      mapM_ (uncurry $ DAO.writeArray ioArray) lastBlock
    else do
      mapM_ (uncurry $ DAO.writeArray ioArray) lastBlock'

rotateTetries :: IORef GlobalRef -> RectGrid -> IOArray (Int, Int, Int) BlockAttr -> IO ()
rotateTetries refGlobal rr ioArray = do
  let stepN = fi $ rotStep initRectGrid
  count1 <- readIORef refGlobal <&> count1_
  case count1 of
    v
      | v < stepN - 1 -> do
        preservingMatrix $ do
          rotateBlock refGlobal rr
          modifyIORef refGlobal \s -> s {count1_ = count1_ s + 1}
      | v == stepN - 1 -> do
        let rr = initRectGrid
        moveX <- readIORef refGlobal <&> moveX_
        moveY <- readIORef refGlobal <&> moveY_

        rotN <- readIORef refGlobal <&> rotN_
        tetX <- readIORef refGlobal <&> currTetris_
        ls <- getAssocs ioArray
        let bk1'X = rotateN rotN (snd tetX)

        -- /Users/cat/myfile/bitbucket/tmp/xx_5248.x
        let currBrX = innerBrickX (moveX, moveY) bk1'X
        let currBrXX = map (\(x, y) -> (x, y, 0)) currBrX
        let bX = checkMoveArr currBrXX ls rr
        modifyIORef refGlobal \s -> s {rotN_ = bX ? (let n = rotN_ s + 1 in mod n 4) $ rotN_ s}
        rotateBlock refGlobal initRectGrid
        modifyIORef refGlobal \s -> s {count1_ = count1_ s + 1}
        pp "ok"
      | otherwise -> do
        modifyIORef refGlobal (\s -> s {count1_ = 1000000})

{-|

   URL: http://localhost/html/indexUnderstandOpenGL.html
   @

   @
-}
perspectiveModeView :: IORef CameraRot -> IO()
perspectiveModeView  refCamRot =  do
  fov <- readIORef refCamRot <&> persp_ <&> fov_
  aspect <- readIORef refCamRot <&> persp_ <&> aspect_
  zn <- readIORef refCamRot <&> persp_ <&> zn_
  zf <- readIORef refCamRot <&> persp_ <&> zf_
  eye <- readIORef refCamRot <&> modelview_ <&> eye_
  at <- readIORef refCamRot <&> modelview_ <&> at_
  up <- readIORef refCamRot <&> modelview_ <&> up_
  matrixMode $= Projection
  loadIdentity
  perspective fov aspect zn zf

  matrixMode $= Modelview 0
  loadIdentity
  GL.lookAt eye at up
  -- GL.lookAt (Vertex3 0 0 1.0 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
  
colorChange :: GLdouble -> Color3 GLdouble -> Color3 GLdouble
colorChange x (Color3 a b c) = Color3 (a*x) (b*x) (c*x)
  
{-|
   KEY: draw cylinder

   @
   let r = 0.5             -- radius of circle
   let l = 0.4             -- length of cylinder
   let leftClose = True    -- close the left end
   let rightClose = False  -- close the right end
   cylinder r l (leftClose, rightClose)
   @
-}
cylinder :: GLfloat -> GLfloat -> (Bool, Bool) -> [Color3 GLdouble]-> IO()
cylinder r leng (left, right) cl = do
  let d = 2.0*pi/10.0
  let lw = [0, 0 + d .. 2*pi]
  let lv = map (\x -> Vertex3 (r * cos x) leng (r * sin x)) lw
  let ls = map (\x -> Vertex3 (r * cos x) 0    (r * sin x)) lw
  let lt = join $ zipWith (\x y -> [x, y]) lv ls
  -- let lt' = zip lt $ join $ repeat [green, yellow, blue, cyan, gray, white]
  let lt' = zip lt $ (join . repeat) cl
  renderPrimitive TriangleStrip $ mapM_(\(v, c) -> do
      color c
      vertex v) lt'
  when left $ do
    renderPrimitive TriangleFan $ mapM_ (\v -> do
        color green
        vertex v) $ Vertex3 (r * cos 0) leng (r * sin 0) : lv
  when right $ do
    renderPrimitive TriangleFan $ mapM_ (\v -> do
        color yellow
        vertex v) $ Vertex3 (r * cos 0) 0 (r * sin 0) : ls

skewMatrix3 :: (Num a) => (a, a, a) -> [[a]]
skewMatrix3 (x, y, z) = [
                         [0,  -z, y],
                         [z,  0, -x],
                         [-y, x,  0]
                        ]
  
{-|

  KEY: rotate around arbitrary axis in 3d

   * Rodrigues's rotation formula

   <https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula Rodrigues_Rotation_Formula>


  @
   rotVec :: Vector3 GLdouble -> Vector3 GLdouble -> GLfloat -> Vector3 GLdouble

   -- k is unit vector rotation axis
   -- v is rotate around k in radian
   rotVec k v radius
  @
-}
rotVec :: (Floating a) => Vector3 a -> Vector3 a -> a -> Vector3 a
rotVec k v theta = ax + bx + cx
  where
    f (Vector3 x y z) = (x, y, z)
    toList (Vector3 x y z) = [x, y, z]
    vec x = Vector3 (head x) ((head . tail) x) (last x)
    skew = skewMatrix3 $ f k
    ax = v
    bx = let a = sin theta
             b = (vec . join) $ skew `multiVec` toList v
         in  a *: b
    cx = let a  = 1 - cos theta
             b = (vec $ join $  skew `multiVec` join (skew `multiVec` toList v))
         in a *: b

{-|

  KEY: rotate around arbitrary axis in 3d

   * Rodrigues's rotation formula

   <https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula Rodrigues_Rotation_Formula>

  @
   rotMat :: Vector3 GLdouble -> GLdouble -> [[GLdouble]]

   -- k is unit vector rotation axis
   -- φ is rotate around k in radius
   rotMat k φ radius
  @
-}
rotMat :: (Floating a) => Vector3 a -> a -> [[a]]
rotMat k θ = id3 + ax + bx
  where
    id3 = out (\a b -> a == b ? 1 $ 0) [1..3] [1..3]
    f (Vector3 x y z) = (x, y, z)
    m = skewMatrix3 $ f k
    ax = sin θ ×× m
    bx = (1 - cos θ) ×× (m `multiMat` m)
  
-- let m = (map . map) rf $ padMat3To4 $ rotMat k (rf $ del * fi i)  
rotMat4Tup :: (Floating a) => Vector3 a -> a -> ([[a]], [a])
rotMat4Tup k θ = (m4, join m4)
  where
    id3 = out (\a b -> a == b ? 1 $ 0) [1..3] [1..3]
    f (Vector3 x y z) = (x, y, z)
    m = skewMatrix3 $ f k
    ax = sin θ ×× m
    bx = (1 - cos θ) ×× (m `multiMat` m)
    m3 = id3 + ax + bx
    m4 = padMat3To4 m3

{-|
   === KEY: rotate vector to other vector

   * rotate v₀ to v₁ in angle θ radian around v₀ ⊗ v₁

   @
   let v0 = Vector3 1 0 0
   let v1 = Vector3 0 0 (-1)
   let θ  = pi/2
   rotToVecMat v₀ v₁ θ
   @
-}
rotToVecMat :: (Floating a, Eq a) => Vector3 a -> Vector3 a  -> [[a]]
rotToVecMat v₀ v₁ = rotMat vᵤ θ
  where
    vᵤ = case v₀ ⊗ v₁ of
              Nothing -> uv v₀
              -- Nothing -> error "ERROR: two vectors can not be parallel, ERROR123"
              Just v -> uv v
    θ = angle2Vector v₀ v₁

{-|
   === KEY: vector projects on plane
-}
projOnPlane :: (Num a, Eq a) => Vector3 a -> (Vector3 a, Vector3 a) -> Vector3 a
projOnPlane v (v0, v1) = v - vp
  where
    vc = case v0 ⊗ v1 of
              Nothing -> error "ERROR: two vectors can not be parallel, ERROR124"
              Just v -> v
    vp = (v `dot3ve` vc) *: vc

{-|

  @
  -- 3x3  to 4x4 matrix

    1 2 3
    4 5 6
    7 8 9

    1 2 3 0
    4 5 6 0
    7 8 9 0
    0 0 0 1
  @
-}
padMat3To4 :: (Num a) => [[a]] -> [[a]]
padMat3To4 m = tran mx
  where
    m' = m ++ [[0, 0, 0]]
    mt = tran m'
    mx = mt ++ [[0, 0, 0, 1]]
    
{-|
  
  @
  (Vertex3 0 0 0) ->  (Vertex3 1 0 0)

  cylinder + cone
  -- leng is the total length of cylinder and cone
  cylinderArrow leng [yellow, gray]
  @
-}
cylinderArrow :: GLfloat -> [Color3 GLdouble] -> IO()
cylinderArrow leng cl = do
  let cyRatio = 0.9 :: GLfloat
  let cyLen =   rf $leng * cyRatio
  let cyRadius = cyLen * 0.01
  let coneRadius = cyRadius * 2.0
  let coneHeigh = rf $ leng * (1.0 - cyRatio)
  rotate (-90) (Vector3 0 0 1 ::Vector3 GLdouble)
  preservingMatrix $ do
    cylinder cyRadius cyLen (True, True)  cl
    translate (Vector3 0 (rf cyLen) 0 :: Vector3 GLdouble)
    cone coneRadius coneHeigh 8 cl


  
coord :: IO()
coord = do
  preservingMatrix $ do
    let r = 0.02  -- radius of cone
    let clen = 0.95 :: GLdouble  -- length of cylinder
    let lo = rf $ 1.0 - clen     -- length of cone
    let nStep = 8                -- n-polygon, etc approximate circle
    
    -- +X-axis
    preservingMatrix $ do
      rotate (-90) (Vector3 0 0 1 :: Vector3 GLdouble)
      cylinder (r * 0.5) (rf $ clen - 0.01) (True, True) [red, colorChange 0.5 red]
    preservingMatrix $ do
      translate (Vector3 clen 0 0 :: Vector3 GLdouble)
      rotate (-90) (Vector3 0 0 1 :: Vector3 GLdouble)
      cone r lo nStep [red]
  
    -- +Y-axis
    preservingMatrix $ do
      cylinder (r * 0.5) (rf $ clen - 0.01) (True, True) [green, colorChange 0.5 green]
    preservingMatrix $ do
      translate (Vector3 0 clen 0 :: Vector3 GLdouble)
      cone r lo nStep [green]
  
    -- +Z-axis
    preservingMatrix $ do
      -- deg <- readAndParse "/tmp/kee.x" :: IO GLdouble
      let deg = 30
      rotate 90 (Vector3 1 0 0 :: Vector3 GLdouble)
      cylinder (r * 0.5) (rf $ clen - 0.01) (True, True) [blue, colorChange 0.5 blue]
    
    preservingMatrix $ do
      translate (Vector3 0 0 clen :: Vector3 GLdouble)
      rotate 90 (Vector3 1 0 0 :: Vector3 GLdouble)
      cone r lo nStep [blue]
    

  
{-|

   KEY: cone

   @
   let r = 0.05 -- radius of circle
   let l = 0.5  -- length of cylinder
   cone r l
   @
-}
cone :: GLfloat -> GLfloat -> Int -> [Color3 GLdouble] -> IO()
cone r leng n cl = do
  let d = 2.0*pi/fi n
  -- let lc = [green, yellow, blue, cyan, gray, white]
  let ld = [yellow, green, white, blue, gray, cyan]
  let lw = [0, 0 + d .. 2.0*pi]
  let lv = map (\x -> Vertex3 (r * cos x) leng (r * sin x)) lw
  let ls = map (\x -> Vertex3 (r * cos x) 0    (r * sin x)) lw
  let lp = zip ls $ join $ repeat cl
  let lt = join $ zipWith (\x y -> [x, y]) lv ls
  -- (x, y, z) <- readAndParse "/tmp/tu.x" :: IO(GLfloat, GLfloat, GLfloat)
  renderPrimitive TriangleFan $ mapM_ (\(v, c) -> do
      color c
      vertex v) $ (Vertex3 0 leng 0 :: Vertex3 GLfloat, white) : lp
  renderPrimitive TriangleFan $ mapM_ (\(v, c) -> do
      color c
      vertex v) $ (Vertex3 0 0 0 :: Vertex3 GLfloat, white) : lp
{--

{-|

   KEY: vector to vertex

   'Vector3' to 'Vertex3'
-}
vecToVex :: Vector3 a -> Vertex3 a
vecToVex (Vector3 x y z) = Vertex3 x y z

vexToVec :: Vertex3 a -> Vector3 a
vexToVec (Vertex3 x y z) = Vector3 x y z


vecToList :: Vector3 a -> [a]
vecToList (Vector3 x y z) = [x, y, z]

vexToList :: Vertex3 a -> [a]
vexToList (Vertex3 x y z) = [x, y, z]

listToVec :: [a] -> Vector3 a
listToVec ls = Vector3 (head lt) ((head . tail) lt) (last lt)
  where
    lt = take 3 ls

listToVex :: [a] -> Vertex3 a
listToVex ls = Vertex3 (head lt) ((head . tail) lt) (last lt)
  where
    lt = take 3 ls
{-|

  === KEY: point to a line, pt to a line, distance from a pt to a line
  
  <<http://localhost:8080/pdf/project_matrix.pdf project_matrix>>
-}
ptToLine3d :: Vertex3 GLfloat -> (Vertex3 GLfloat, Vertex3 GLfloat) -> GLfloat
ptToLine3d p0 (q1, q2) = nr vr
  where
    -- http://localhost:8080/pdf/project_matrix.pdf
    v0 = vexToList p0
    v12 = q1 -: q2
    u12 = uv v12
    ls = vecToList u12
    mx = out (*) ls ls  -- outer product two vector
    vp = join $ mx `multiVec` v0  -- p0 project onto v12
    vr = p0 -: listToVex vp     -- p0 reject onto  v12
{-|

  KEY: angle between two `Vector3 a` `Vertex3 a`

-}
angle2Vector :: (Floating a) => Vector3 a -> Vector3 a -> a
angle2Vector v0 v1 = acos $ (n0*n0 + n1*n1 - dx*dx) / (2 * n0 * n1)
  where 
    x0 = vecToVex v0
    x1 = vecToVex v1
    dx = distX x0 x1
    xz = Vertex3 0 0 0
    n0 = distX xz x0
    n1 = distX xz x1
--}

vecToList3 :: Vector3 a -> [a]
vecToList3 (Vector3 x y z) = [x, y, z]

  

{--
vecToM3x :: Vector3 GLdouble -> [[GLdouble]]
vecToM3x (Vector3 x y z) = [
                            [x, 0, 0],
                            [y, 0, 0],
                            [z, 0, 0]
                           ]

vecToM3y :: Vector3 GLdouble -> [[GLdouble]]
vecToM3y (Vector3 x y z) = [
                            [0, x, 0],
                            [0, y, 0],
                            [0, z, 0]
                          ]
  
vecToM3z :: Vector3 GLdouble -> [[GLdouble]]
vecToM3z (Vector3 x y z) = [
                            [0, 0, x],
                            [0, 0, y],
                            [0, 0, z]
                          ]
--}
  
vecToM4x :: Vector3 GLdouble -> [[GLdouble]]
vecToM4x (Vector3 x y z) = [
                            [x, 0, 0, 0],
                            [y, 0, 0, 0],
                            [z, 0, 0, 0],
                            [0, 0, 0, 0]
                          ]

vecToM4y :: Vector3 GLdouble -> [[GLdouble]]
vecToM4y (Vector3 x y z) = [
                            [0, x, 0, 0],
                            [0, y, 0, 0],
                            [0, z, 0, 0],
                            [0, 0, 0, 0]
                          ]

vecToM4z :: Vector3 GLdouble -> [[GLdouble]]
vecToM4z (Vector3 x y z) = [
                            [0, 0, x, 0],
                            [0, 0, y, 0],
                            [0, 0, z, 0],
                            [0, 0, 0, 0]
                          ]

fx (Vector3 x y z) = Vector3 (rf x) (rf y) (rf z)

rotateWorldX :: IORef CameraRot -> IO ()
rotateWorldX refCamRot = do
  currXYZ <- readIORef refCamRot <&> currXYZ_
  coordFrame <- readIORef refCamRot <&> coordFrame_
  case currXYZ of
    n | n == 1 -> do
          coordFrameMat <- readIORef refCamRot <&> coordFrameMat_
          let ls = join coordFrameMat
          multiModelviewMat ls
          return ()
      | n == 2 -> do
          coordFrameMat <- readIORef refCamRot <&> coordFrameMat_
          let ls = join coordFrameMat
          multiModelviewMat ls
          return ()
      | n == 3 -> do
          coordFrameMat <- readIORef refCamRot <&> coordFrameMat_
          let ls = join coordFrameMat
          multiModelviewMat ls
          return ()
      | n == 4 -> do
          coordFrameMat <- readIORef refCamRot <&> coordFrameMat_
          let ls = join coordFrameMat
          multiModelviewMat ls
          return ()
      | otherwise -> do
          error $ "currXYZ invalid Integer = " ++ show currXYZ
    
beginWindow3d :: G.Window -> IORef CameraRot -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> IO()
beginWindow3d w3d refCamRot refGlobal ioArray = do
  -- G.getWindowFocused w3d >>= \b -> when b $ G.setKeyCallback w3d (Just $ keyBoardCallBackNew2 refCamRot refGlobal ioArray)
  G.getWindowFocused w3d >>= \b -> when b $ G.setKeyCallback w3d (Just $ keyBoardCallBack3d refCamRot refGlobal ioArray)
  G.getWindowFocused w3d >>= \b -> when b $ G.setMouseButtonCallback w3d (Just $ mouseCallbackX refGlobal)

  (width, height) <- G.getFramebufferSize w3d
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  GL.clear [ColorBuffer, DepthBuffer]
  GL.depthFunc $= Just Lequal
  
  loadIdentity
  
  fov <- readIORef refCamRot <&> persp_ <&> fov_
  zf <- readIORef refCamRot <&> persp_ <&> zf_
  zn <- readIORef refCamRot <&> persp_ <&> zn_
  eye <- readIORef refCamRot <&> modelview_ <&> eye_
  at <- readIORef refCamRot <&> modelview_ <&> at_
  up <- readIORef refCamRot <&> modelview_ <&> up_
  pp "kk"

   
  matrixMode $= Projection
  loadIdentity
  perspective fov 1.0 zn zf

  matrixMode $= Modelview 0
  loadIdentity
  -- GL.lookAt (Vertex3 0 0 1 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
  GL.lookAt eye at up 
  
endWindow3d :: G.Window -> IO()
endWindow3d w = do
  G.makeContextCurrent $ Just w
  G.swapBuffers w
  G.pollEvents
  
saveImageFrame :: G.Window -> IOArray Int AnimaState -> IO()
saveImageFrame w animaStateArr = do
  let anima1 = 1
  let intervalx = 0 -- larger number is slower
  (isNext1, index1, animaState1) <- readAnimaState animaStateArr anima1 intervalx
  let fn = "/tmp/img_" ++ show (index1 + 1000) ++ ".png"
  saveImageOpenGL w fn
  writeAnimaState animaStateArr animaState1 {animaIndex_ = index1}
  
unlessX' :: IO Bool -> IO Bool -> IO () -> IO ()
unlessX' a b act = do 
  a' <- a
  b' <- b
  when (not a' || not b') act

unlessXX :: IO Bool -> IO () -> IO ()
unlessXX a act = do 
  a' <- a
  when (not a') act

{-|
  initial Y position before it falls
      
                            (0, 6)
      
      +  (moveX_, moveY_) = (0, 0)

-}
initY :: Int
initY = 6

runGame :: G.Window -> IORef GlobalRef -> DAO.IOArray (Int, Int, Int) BlockAttr -> IO()
runGame w2d refGlobal ioArray = do 
        rr <- readIORef refGlobal <&> rectGrid_
        mx <- readIORef refGlobal <&> moveX_
        my <- readIORef refGlobal <&> moveY_
        tetX <- readIORef refGlobal <&> currTetris_
        rotN <- readIORef refGlobal <&> rotN_
        ls <- getAssocs ioArray

        let rotTet = rotateN rotN (snd tetX)
        let tetris = getShapeX rotTet 

        let mX = map (\(a, b) -> (a + mx, b + my - 1, 0)) tetris 

        let lastBlock = map (\(a, b) -> ((a + mx, b + my, 0), fst tetX)) tetris 
        let isMovable = checkMoveArr mX ls rr
        if not isMovable
          then do
            mapM_ (uncurry $ DAO.writeArray ioArray) lastBlock

            -- KEY: remove bottom row
            let f x = isFilled_ x in removeBottomX w2d f ioArray

            logFileG ["writeArray"]
            logFileG $ map show ls

            newBlock <- randomBlockX refGlobal
            modifyIORef
              refGlobal
              ( \s ->
                  s
                    { moveY_ = initY,
                      moveX_ = 0,
                      currTetris_ = newBlock
                    }
              )
          else do 
            modifyIORef
              refGlobal
              ( \s ->
                  s
                    { moveY_ = my - 1,
                      moveX_ = moveX_ s,
                      currTetris_ = currTetris_ s 
                    }
              )


mainLoop ::
  G.Window ->
  IORef CameraRot ->
  IORef GlobalRef ->
  IORef FrameCount ->
  IOArray Int AnimaState ->
  [[Vertex3 GLfloat]] ->
  DAO.IOArray (Int, Int, Int) BlockAttr ->
  IO ()
mainLoop w2d refCamRot refGlobal refGlobalFrame animaStateArr lssVex ioArray = unlessXX (G.windowShouldClose w2d) $ do
  beginWindow3d w2d refCamRot refGlobal ioArray
  
-- /Users/cat/myfile/bitbucket/tmp/xx_9059.x
  rotateWorldX refCamRot
  
  drawRect (Vertex3 0 0.5 0, Vertex3 0.5 0 0)

  isPaused <- readIORef refGlobal <&> isPaused_
  unless isPaused $ do
    let slotNum0 = 0
    (isNext0, index, animaState) <- readAnimaState animaStateArr slotNum0 500000

    -- logFileG ["index=" ++ show index]
    logFileG ["isNextX=" ++ show isNext0 ++ " animaIndex_=" ++ show (animaIndex_ animaState)]
    -- my <- readIORef refGlobal >>= return . moveY_
    -- KEY: falling block, drop block
    when True $ do
      when isNext0 $ do
        runGame w2d refGlobal ioArray
        flipIsNext animaStateArr slotNum0

    when True $ do
      -- KEY: rotate brick, rotate tetris
      -- rotateTetries refGlobal initRectGrid ioArray
      -- show current tetris
      currBrickX refGlobal initRectGrid
      pp "Ok"
  -- KEY: show board, show grid, draw board
  -- saveImageFrame w2d animaStateArr

  drawFinal w2d ioArray initRectGrid

  endWindow3d w2d
  mainLoop w2d refCamRot refGlobal refGlobalFrame animaStateArr lssVex ioArray

drawFinal :: G.Window -> IOArray (Int, Int, Int) BlockAttr -> RectGrid -> IO ()
drawFinal w arr rr = do
  showCurrBoardArr arr
  drawRectGridX rr
  
bk1 :: [[Int]]
bk1 =
  [ [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
  ]



{--
data MyX = MyX {name00_ :: String, age00_ :: Int} deriving (Show, Eq)

main :: IO ()
main = do
    let x = MyX "x" 3
    let x1 = MyX "x1" 20
    let color = Color3 0.3 0.1 0.9 :: (Color3 GLdouble)
    let x2 = (9, blue)
    let x3 = (100, green)
    let grid = (-1, -1, white) :: (Int, Int, Color3 GLdouble)
    -- Create a mutable 2D array with dimensions 3x3
    -- arr <- DAO.newArray ((1,1), (3,3)) 0 :: IO (IOArray (Int, Int) Int)
    -- arr <- DAO.newArray ((1,1), (3,3)) x :: IO (IOArray (Int, Int) MyX)

    arr <- DAO.newArray ((1,1), (3,3)) x2 :: IO (IOArray (Int, Int) (Int, Color3 GLdouble))
    arx <- DAO.newArray ((1,1), (3,3)) x3 :: IO (IOArray (Int, Int) (Int, Color3 GLdouble))
    ary <- DAO.newArray ((1,1), (3,3)) grid :: IO (IOArray (Int, Int) (Int, Int, Color3 GLdouble))
    ark <- DAO.newArray ((-1,-1), (3,3)) 0 :: IO (IOArray (Int, Int) Int)
    ar3 <- DAO.newArray ((0, 0, 0), (2, 2, 2)) 0 :: IO (IOArray (Int, Int, Int) Int)

    let ls = [1..10] :: [Int]
    lsx <- DAO.newListArray (0, 10) ls :: IO (IOArray (Int) Int)

    when True $ do
      fw "Mutable Array"
      -- one dimension array
      -- index from [1 .. 3], init with 44
      brr <- DAO.newArray (1, 3) 44 :: IO (IOArray (Int) Int)

      -- two dimension array
      -- index from [0,0] - [3, 3], init with 2
      brk <- DAO.newArray ((0, 0), (3, 3)) 2 :: IO (IOArray (Int, Int) Int)

      -- write to array
      writeArray brk (0, 0) 400
      ls <- mapArray id brk >>= getAssocs >>= return . filter (\((x, y), a) -> 0 <= y && y <= 1)
      pre ls
      -- let lt = filter (\((x, y), a) -> 0 <= y && y <= 1) ls
      -- fw "lt"
      -- pre lt

    -- Read and print the initial 2D array
    elems <- getElems arr
    putStrLn $ "Initial array: " ++ show elems

    e3 <- getElems ar3
    fw "e3"
    putStrLn $ show e3
    fw "e3 3"
    printMat $ partList 3 e3

    ak3 <- getAssocs ar3
    fw "ak3"
    putStrLn $ show ak3

    -- mapArray ()

    -- Modify an element in the 2D array
    writeArray arr (2, 2) x2
    writeArray arr (2, 3) (10, green)
    writeArray arr (2, 3) (10, green)
    writeArray ark (0, 0) 4

    when True $ do
      let tt = zip [1..3] [1..3]
      fw "ark 1"
      lv <- mapArray id ark >>= getElems
      pre lv
      mapM_ (\t -> writeArray ark t 5) [(i, j) | i <- [0..2], j <- [0..2]]
      fw "ark 2"
      le <- mapArray id ark >>= getElems
      pp "ok"
      pre le
      bd <- getBounds ark
      fw "bd"
      pre bd

    when True $ do
      fw "a55 1"
      a55 <- mapIndices (0, 2) id lsx >>= getElems
      printMat $ partList 0 a55
    when True $ do
      fw "a55 2"
      a55 <- mapIndices (0, 2) (\x -> x + 1) lsx >>= getElems
      printMat $ partList 0 a55

    a44 <- mapArray id ark >>= getElems
    fw "a44"
    putStrLn $ show a44
    fw "a55"
    let a55 = partList 5 a44
    printMat $ a55
    fw "kk"

    -- apply lambda function on an array
    arr2 <- mapArray (\x -> (fst x + 1, snd x)) arr >>= getElems
    associatedArr <- mapArray (\x -> (fst x + 1, snd x)) arr >>= getAssocs
    fw "map array"
    putStrLn $ show arr2
    fw "getAssocs"
    putStrLn $ show associatedArr
    fw "print 2d"
    printMat $ partList 3 associatedArr

    -- Read and print the modified 2D array
    modifiedElems <- getElems arr
    putStrLn $ "Modified array: " ++ show modifiedElems
--}

--
-- (z,       y, x)
--  Layer
--          Col
--               Row
-- (l, c, r)
--

data D3 = D3
  { zVec_ :: Int,
    yVec_ :: Int,
    xVec_ :: Int
  }
  deriving (Show, Eq)

revIndex :: (Int, Int) -> Int -> Maybe Int
revIndex (a, b) x = DM.lookup x m
  where
    ls = [a .. b]
    rs = reverse ls
    m = DM.fromList $ zip ls rs

-- /Users/cat/myfile/bitbucket/tmp/xx_5686.x
-- /Users/cat/myfile/bitbucket/tmp/xx_3123.x

walkBlockXX :: (Int, Int, Int) -> (BlockAttr -> Bool) -> IOArray (Int, Int, Int) BlockAttr -> IO [(Int, Int, Int)]
walkBlockXX (a, b, c) f arr = do
  ls <- walkfun (a, b, c) f arr
  mapM_ (uncurry $ DAO.writeArray arr) ls
  return $ map fst ls
  where
    walkfun :: (Int, Int, Int) -> (BlockAttr -> Bool) -> IOArray (Int, Int, Int) BlockAttr -> IO [((Int, Int, Int), BlockAttr)]
    walkfun (a, b, c) f arr = do
      bound <- DAO.getBounds arr
      if DAO.inRange bound (a, b, c)
        then do
          x <- DAO.readArray arr (a, b, c)
          if f x
            then do
              DAO.writeArray arr (a, b, c) initBlockAttr
              walkfun (a + 1, b, c) f arr >>= \s1 ->
                walkfun (a - 1, b, c) f arr >>= \s2 ->
                  walkfun (a, b + 1, c) f arr >>= \s3 ->
                    walkfun (a, b - 1, c) f arr >>= \s4 ->
                      return (s1 ++ s2 ++ s3 ++ s4 ++ [((a, b, c), x)])
            else do
              return []
        else do
          return []

-- <&> = F a -> (a -> b) -> F b
-- y0, y1: -1  0  1  2  3
--               x  x   x
funx :: (Int, Int) -> (Int, Int) -> (Int -> Bool) -> IOArray (Int, Int, Int) Int -> IO ()
funx (y0, y1) (yy0, yy1) f arr = do
  when (y0 <= y1) $ do
    let z0 = 0
    ((a0, b0, c0), (a1, b1, c1)) <- getBounds arr

    -- ls <- DAO.mapArray id arr >>= getAssocs <&> filter (\((z, y, x), e) -> z == 0 && y == y1 && (b1 e || b2 e || b3 e || b4 e))
    ls <- DAO.mapArray id arr >>= getAssocs <&> filter (\((z, y, x), e) -> z == 0 && y == y1 && f e)
    if len ls == c1 - c0 + 1
      then do
        fw "Full Row"
        DAO.getAssocs arr
          >>= mapM_
            ( \((zDir, yDir, xDir), _) -> do
                let ms = revIndex (yy0, yy1) yDir
                let y' = case ms of
                      Just s -> s
                      Nothing -> error "ERROR: Invalid index"
                when (zDir == z0) $ do
                  if y0 < y' && y' <= y1
                    then do
                      s <- DAO.readArray arr (zDir, y' - 1, xDir)
                      DAO.writeArray arr (zDir, y', xDir) s
                    else do
                      when (y0 == y') $ do
                        DAO.writeArray arr (zDir, y', xDir) 0
            )
        funx (yy0, yy1) (yy0, yy1) f arr
      else do
        funx (y0, y1 - 1) (yy0, yy1) f arr
        pp "Not equal to len"
    fw "funx print arr"
    printMat3 arr

removeBottom :: G.Window -> (Int, Int) -> (Int, Int) -> (BlockAttr -> Bool) -> IOArray (Int, Int, Int) BlockAttr -> IO ()
removeBottom w (y0, y1) (yy0, yy1) f arr = do
  when (y0 <= y1) $ do
    let fstLayer = 0
    let z0 = 0
    bt@((a0, b0, c0), (a1, b1, c1)) <- getBounds arr
    let gridWidth = a1 - a0 + 1
    ls <- DAO.mapArray id arr >>= getAssocs <&> filter (\((z, y, x), e) -> x == fstLayer && y == y1 && f e)
    if len ls == gridWidth
      then do
        mapM_ (\x -> print $ "lsxx " ++ show x) ls
        {-      
        let lt = map (\(t, e) -> (t, e {color_ = white})) ls
        mapM_
          ( \(t, e) -> do
              DAO.writeArray arr t e
          )
          lt
        fw "Full Row"
        logFileG ["MyFullRow"]
        logFileG [show bt]
        logFileG $ map show ls
        logFileG ["EndFullRow"]
        logFileG ["myy1"]
        logFileG [show y1]
        -}
        DAO.getAssocs arr
          >>= mapM_
            ( \((zDir, yDir, xDir), _) -> do
                {-
                let ms = revIndex (yy0, yy1) yDir
                let y' = case ms of
                      Just s -> s
                      Nothing -> error "ERROR: Invalid index"
                -}
                -- KEY: two dimensions for now
                when (xDir == fstLayer) $ do
                  {-
                  logFileG ["CallMeMaybe0"]
                  logFileG ["yy0 y' yy1"]
                  logFileG $ map show [yy0, y', yy1]
                  logFileG ["y0, y1"]
                  logFileG $ map show [y0, y1]
                  logFileG ["zDir yDir xDir"]
                  logFileG $ map show [zDir, yDir, xDir]
                  -}

                  -- -10                   9
                  -- SEE: how to move row from top to bottom 
                  -- /Users/cat/myfile/try/bottom.png
                  if yy0 <= yDir && yDir < yy1 && y1 <= yDir
                    then do
                      logFileG ["(zDir yDir)" ++ (show (zDir, yDir + 1)) ++ " => " ++ show (zDir, yDir)]
                      -- KEY: move one row down, not at the lowest bottom yet
                      s <- DAO.readArray arr (zDir, yDir + 1, xDir)
                      DAO.writeArray arr (zDir, yDir, xDir) s
                    else do
                      -- The lowest bottom row
                      when (yy1 == yDir) $ do
                        DAO.writeArray arr (zDir, yDir, xDir) initBlockAttr
            )
        -- Remove the bottom row
        fallBlock w arr
        removeBottom w (yy0, yy1) (yy0, yy1) f arr
      else
      do
        removeBottom w (y0, y1 - 1) (yy0, yy1) f arr

-- pp "Not equal to len"
-- fw "funx print arr"
-- printMat3 arr

removeBottomX :: G.Window -> (BlockAttr -> Bool) -> IOArray (Int, Int, Int) BlockAttr -> IO ()
removeBottomX w f arr = do
  ((z0, y0, x0), (z1, y1, x1)) <- DAO.getBounds arr
  removeBottom w (y0, y1) (y0, y1) f arr

fallBlock :: G.Window -> IOArray (Int, Int, Int) BlockAttr -> IO ()
fallBlock w arr = do
  let rr = initRectGrid
  DAO.getAssocs arr
    >>= mapM_
      ( \((z, y, x), ax) -> do
          if isFilled_ ax
            then do
              lsArr <- getAssocs arr
              let f x = isFilled_ x && isFilled_ ax && tetrisCount_ x == tetrisCount_ ax
              lt <- walkBlockXX (z, y, x) f arr
              let lsArr' = filter (\(x, _) -> x `notElem` lt) lsArr
              let ls = map (\(z0, y0, x0) -> (z0, y0 - 1, x0)) lt

              logFileG ["fallBlock"]
              logFileG $ map show ls
              let b = checkMoveArr ls lsArr' rr
              when (len lt > 0 && b) $ do
                logFileG ["movedown"]
                let lv = map (,ax) ls
                logFileG $ map show lv

                mapM_ (uncurry $ DAO.writeArray arr) $ map (,initBlockAttr) lt
                mapM_ (uncurry $ DAO.writeArray arr) lv
                drawFinal w arr initRectGrid
                logFileG ["endmovedown"]
                fallBlock w arr
            else
            do
              pp "ee"
      )

{--
  11122
  ee12
  ex12
   x

--}

{--
main = do
       when True $ do
         let x0 = 0
         let x1 = 2
         let y0 = -1
         let y1 = 3
         let z0 = 0
         let z1 = 4
         arr <- DAO.newListArray ((x0, y0, z0) , (x1, y1, z1)) [1.. (x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1)] :: IO (IOArray (Int, Int, Int) Int)
         DAO.getAssocs arr >>= mapM_ (\((zDir, yDir, xDir), _) -> do
                   let ms = revIndex (y0, y1) yDir
                   let y' = case ms of
                                Just s -> s
                                Nothing -> error "ERROR: Invalid index"
                   when (zDir == z0) $ do
                     if y0 < y' && y' < y1 then do
                       s <- DAO.readArray arr  (zDir, y' - 1, xDir)
                       DAO.writeArray arr      (zDir, y', xDir) s
                     else do
                       when (y0 == y') $ do
                         DAO.writeArray arr (zDir, y', xDir) 0
               )
         fw "writeArray"
         a1 <- DAO.getAssocs arr
         printMat3 arr
         -- mapM_ (\x -> do printMat x; fw "";)  $ partList (y1 - y0 + 1) $ partList (z1 - z0 + 1) a1
         pp "ok"
       when True $ do
         let (a, b) = (-1, 2)
         let ls = map (revIndex (-1, 2)) [-1..2]
         pre ls
       when True $ do
         fw "printMat3"
         let a = 1; b = 2; c = 3
         ls <- DAO.newListArray ((0, 0, 0), (a, b, c)) [1..((a + 1) * (b + 1)) * (c + 1)] :: IO(IOArray (Int, Int, Int) Int)
         lt <- DAO.getAssocs ls
         printMat3 ls
       when True $ do
         pp "funx"
         let x0 = 0
         let x1 = 2
         let y0 = -1
         let y1 = 3
         let z0 = 0
         let z1 = 4
         ls <- DAO.newListArray ((x0, y0, z0), (x1, y1, z1)) [1..(x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1)] :: IO(IOArray (Int, Int, Int) Int)
         let b1 e = 21 <= e && e <= 25
         let b2 e = 16 <= e && e <= 20
         let b3 e = 11 <= e && e <= 15
         let b4 e = 6  <= e && e <= 10
         let b5 e = 1  <= e && e <= 5
         let f  e = 16 <= e && e <= 20
         funx (y0, y1) (y0, y1) f ls
         pp "kk"
       when True $ do
         fw "walkBlockXX"
         let a = 1; b = 2; c = 0
         arr <- DAO.newListArray ((0, 0, 0), (a, b, c)) [1..((a + 1) * (b + 1) * (c + 1))] :: IO(IOArray (Int, Int, Int) Int)
         lt <- DAO.getAssocs arr
         let f x = x == 1 || x == 2 || x == 4
         ls <- walkBlockXX (0, 0, 0) f arr
         printMat3 arr
         fw "ls"
         pre ls

       when True $ do
         pp "funx"
         let x0 = 0
         let x1 = 2
         let y0 = -1
         let y1 = 3
         let z0 = 0
         let z1 = 4
         let xx = x1 - x0 + 1
         let yy = y1 - y0 + 1
         let zz = z1 - z0 + 1
         arr <- DAO.newListArray ((x0, y0, z0), (x1, y1, z1)) $ replicate (xx * yy * zz) initBlockAttr  :: IO(IOArray (Int, Int, Int) BlockAttr)
         -- arr <- DAO.newListArray ((x0, y0, z0), (x1, y1, z1)) $ replicate (xx * yy * zz) 0  :: IO(IOArray (Int, Int, Int) Int)
         let b1 e = 21 <= e && e <= 25
         let b2 e = 16 <= e && e <= 20
         let b3 e = 11 <= e && e <= 15
         let b4 e = 6  <= e && e <= 10
         let b5 e = 1  <= e && e <= 5
         let f s = isFilled_ s

         removeBottom (y0, y1) (y0, y1) f arr
         fw "removeBottom arr"
         printMat3 arr
--}



mainX = do
       let bk1 =[
                [ [0, 0, 1, 0, 0],
                  [0, 0, 1, 0, 0],
                  [0, 1, 1, 1, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
              ]
       when True $ do
         let n1 = [0 .. len (head . head $ bk1) - 1]
         let n2 = reverse n1
         let n3 = n1
         let centerBrick = map (\z -> map (\y -> map (\x -> (x - 2, y - 2, z)) n1  ) n2) n3
         let ss = getShape3 centerBrick bk1
         fw "getShape3"
         pre ss
         fw "centerBrick"
         mapM_ (\x -> do printMat x; fw "") centerBrick
         pp "ok"

main = do
  argList <- getArgs
  if len argList > 0
    then do
      case head argList of
        "-h" -> helpme
        _ -> do
          print $ "Wrong option => " ++ head argList ++ ", -h => Help"
    else do
      mymain
