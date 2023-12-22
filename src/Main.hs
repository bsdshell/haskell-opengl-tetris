{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE CPP #-}

module Main where

-- import qualified Graphics.Rendering.FTGL as FTGL

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
import AronGraphic hiding (dist)
import AronModule
    ( printMat3,
      resetRefFrame,
      readRefFrame2,
      printMat,
      fi,
      unique,
      fw,
      logFileG,
      pre,
      fl,
      writeFileList,
      randomIntList,
      timeNowMilli,
      ρ,
      (?),
      partList,
      takeBetweenExc,
      trim,
      pp,
      rf,
      iterateList,
      mergeList,
      sqrtC',
      im,
      re,
      len,
      randomInt,
      printBox,
      en,
      FrameCount(..),
      C(C) )
import AronOpenGL
import Control.Arrow
import Control.Concurrent
import Control.Lens hiding (pre, re)
import Control.Monad
import Control.Monad (unless, when)
import qualified Control.Monad.State as CMS
import Data.Complex
import Data.IORef
import Data.Int
import qualified Data.List as DL
import qualified Data.Map as DM
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.StateVar
import Data.Typeable
import Data.Typeable (typeOf)
-- import AronDevLib

import qualified Data.Vector as VU
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Float.RealFracMethods
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GLU.Matrix as GM
import qualified Graphics.UI.GLFW as G
import qualified Graphics.UI.GLUT as GLUT
import Language.Haskell.Interpreter
import System.Directory
import System.Environment
import System.Exit
import System.IO
import qualified Text.Printf as PR
  
import qualified Data.Array.IO as DAO
import Data.Array.IO
  

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
--   @
tmpfile = "/tmp/tmpfile.txt"

mc :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
mc (a, b) (a', b') = (a * a' - b * b', a * b' + a' * b)

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

drawCylinderX :: [[Vertex3 GLfloat]] -> IO ()
drawCylinderX cx = drawSurfaceFromList cx

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

--   #if (1==0)

errorString :: InterpreterError -> String
errorString (WontCompile es) = DL.intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

emptyLine :: Interpreter ()
emptyLine = say ""

colorls =
  VU.fromList
    [ Color3 0.1 0.1 0.1,
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
      Color3 0.1 0.1 0.4
    ]

{-|
   KEY: random color, get color
-}
randomColor :: IO (Color3 GLdouble)
randomColor = randomInt 0 (len ls - 1) >>= \x -> return $ ls !! x
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

grid :: [[[Vertex3 GLfloat]]]
grid = [[[Vertex3 a b (a * a - b * b) | a <- aa] | b <- bb] | c <- cc]
  where
    n = 10
    fa = 1 / n
    aa = map (\x -> fa * x) [1 .. n]
    bb = map (\x -> fa * x) [1 .. n]
    cc = map (\x -> fa * x) [1 .. n]

-- f z = z*z
--
-- a    => x
-- b    => y
-- re c => z
-- im c => color
grid4 :: [[(Vertex3 GLfloat, GLfloat)]]
grid4 = [[let c = (C a b) * (C a b) in (Vertex3 a b (re c), im c) | a <- aa] | b <- bb]
  where
    ne = [[let c = sqrtC' (C a b) in (Vertex3 a b (re c), im c) | a <- aa] | b <- bb]
    n = 20
    fa = 1 / (1.5 * n)
    aa = map (\x -> fa * x) [- n .. n]
    bb = map (\x -> fa * x) [- n .. n]

grid4' = (map . map) (\x -> fst x) grid4

trig s1 s2 = map (\x -> foldr (++) [] x) $ (zipWith . zipWith) (\x y -> [x, y]) (init s1) (tail s2)

segment1 = zipWith (\x y -> [Vertex3 (-1) 0 0, y]) [1 ..] test_circle

test_circle :: [Vertex3 GLfloat]
test_circle =
  [ let x = (1 - t * t) / (1 + t * t)
        y = (2 * t) / (1 + t * t)
     in Vertex3 x y 0
    | t <- aa
  ]
  where
    n = 50
    fa = 1 / (0.1 * n)
    aa = map (\x -> fa * x) [- n .. n]

splitPt :: Int -> [(GLfloat, GLfloat, GLfloat)] -> [[(GLfloat, GLfloat, GLfloat)]]
splitPt _ [] = []
splitPt n xs = take n xs : (splitPt n $ drop n xs)

mergeChunk :: Int -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
mergeChunk n c = mergeList (take n c) (take n $ drop n c)

bigChunk :: Int -> [(GLfloat, GLfloat, GLfloat)] -> [[(GLfloat, GLfloat, GLfloat)]]
bigChunk n xs = splitPt n xs

renderSurface :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderSurface xs = do
  iterateList
    (bigChunk 80 xs)
    ( \chunk -> do
        let len = length chunk
        let n = div len 2
        let c = mergeChunk n chunk
        let cc = zipWith (\x y -> (x, y)) c [1 ..]
        renderPrimitive TriangleStrip $
          mapM_
            ( \((x, y, z), n) -> do
                case mod n 3 of
                  0 -> do
                    color (Color3 0.8 1 0 :: Color3 GLdouble)
                  1 -> do
                    color (Color3 0 0.5 1 :: Color3 GLdouble)
                  _ -> do
                    color (Color3 1 0 0.7 :: Color3 GLdouble)
                normal $ (Normal3 x y z :: Normal3 GLfloat)
                vertex $ Vertex4 x y z 0.8
            )
            cc
    )

-- |
--
--    translate rotate drawRect drawCircle
type Vex3 = Vertex3 GLfloat

type V3d = Vertex3 GLdouble

drawHistogram :: IO ()
drawHistogram = do
  preservingMatrix $ do
    translate (Vector3 0.0 0 0 :: Vector3 GLdouble)
    drawRect (Vertex3 (-0.1) (-0.1) 0, Vertex3 0.1 0.1 0)

drawHistogram2 :: [GLfloat] -> IO ()
drawHistogram2 cx = do
  let n = len cx
  let δ = 1 / rf n
  let w = δ - 0.002
  let zz = map (\(a, b) -> (rf a, b)) $ zip cx [0 ..]
  mapM
    ( \(h, c) -> do
        pp h
        pp c
        let off = rf $ c * δ
        preservingMatrix $ do
          translate (Vector3 off (h / 2) 0 :: Vector3 GLdouble)
          -- drawRect2d w (rf h)
          drawRectFill2d white (w, (rf h))

        -- translate (Vector3 off (-0.3) 0 :: Vector3 GLdouble)
        preservingMatrix $ do
          let strNum = PR.printf "%.1f" h :: String
          strWidth <- GLUT.stringWidth GLUT.Roman strNum
          -- strHeight <- GLUT.stringHeight GLUT.Roman str
          -- 1000 => 1000 pixel
          print $ "strWidth=" ++ (show $ rf strWidth / scaleFont)
          let cen = off - ((rf strWidth) / (scaleFont * 2.0))
          print $ "cen=" ++ (show cen)
          print $ "off=" ++ (show off)
          translate (Vector3 cen (-0.1) 0 :: Vector3 GLdouble)
          renderText strNum
        return ()
    )
    zz
  return ()

drawHis :: [GLfloat] -> IO ()
drawHis cx = do
  preservingMatrix $ do
    translate (Vector3 (-0.5) 0 0 :: Vector3 GLdouble)
    drawHistogram2 cx

drawTri :: [(Vertex3 GLfloat)] -> IO ()
drawTri cx = do
  drawPrimitiveVex LineLoop green cx

-- |
--   === Move object alone X-Axis
--
--   1. move drawRect2d to the right in x
--
--   @
--   moveToX drawRect2d (w, h) x    -- move to the right
--   moveToX drawRect2d (w, h) (-x) -- move to the left
--   @
--
--       drawRect2d w h
--
--           ↑ y
--           |
--         ⌜---⌝
--  ← -x   |   |  → x
--         | + |–--->      X-Axis
--         |   |
--         ⌞---⌟
moveToX :: ((GLfloat, GLfloat) -> IO ()) -> (GLfloat, GLfloat) -> GLdouble -> IO () -- moveToX drawRect2d (w, h) x  -- move to the (x) right, (-1) left
moveToX f (w, h) x = do
  preservingMatrix $ do
    translate (Vector3 x 0 0 :: Vector3 GLdouble)
    f (w, h)

moveToY :: ((GLfloat, GLfloat) -> IO ()) -> (GLfloat, GLfloat) -> GLdouble -> IO ()
moveToY f (w, h) y = do
  preservingMatrix $ do
    translate (Vector3 0 y 0 :: Vector3 GLdouble)
    f (w, h)

moveToZ :: ((GLfloat, GLfloat) -> IO ()) -> (GLfloat, GLfloat) -> GLdouble -> IO ()
moveToZ f (w, h) z = do
  preservingMatrix $ do
    translate (Vector3 0 0 z :: Vector3 GLdouble)
    f (w, h)

renderText :: String -> IO ()
renderText str = do
  preservingMatrix $ do
    -- rotate (60)$ (Vector3 0 0 1 :: Vector3 GLdouble)
    -- translate (Vector3 0 (-0.1) 0 ::Vector3 GLdouble)
    GL.scale (1 / scaleFont :: GL.GLdouble) (1 / scaleFont) 1
    GLUT.renderString GLUT.Roman str

-- KEY: string width, string height, font width, font height
-- strWidth <- GLUT.stringWidth GLUT.Roman str
-- strHeight <- GLUT.stringHeight GLUT.Roman str

-- |
--    === cylinder xz-plane, perpendicular to xz-plane
cylinder :: GLfloat -> IO ()
cylinder r = drawSurfaceFromList cylinderPt

cylinderPt :: [[Vertex3 GLfloat]]
cylinderPt = cm
  where
    cm =
      let n = 40 :: Int
          δ = (2 * pi) / (rf (n -1)) :: Float
          r = 0.1
          br = 0.2
          σ = 1 / rf (n -1)

          fx :: Int -> Int -> GLfloat
          fx i j =
            let i' = rf i
                j' = rf j
             in (1 / rf 40) * j'

          fy :: Int -> Int -> GLfloat
          fy i j =
            let i' = rf i
                j' = rf j
                n = 3
             in r * cos (δ * i')

          fz :: Int -> Int -> GLfloat
          fz i j =
            let i' = rf i
                j' = rf j
             in r * sin (δ * i')
       in [[Vertex3 (fx j j) (fy i i) (fz i i) | i <- [1 .. n]] | j <- [1 .. n]]

lsfun =
  [ "\\x -> x*x",
    "\\x -> 2*x*x",
    "\\x -> 3*x*x",
    "\\x -> 4*x*x",
    "\\x -> 5*x*x",
    "\\x -> 6*x*x",
    "\\x -> 7*x*x",
    "\\x -> 8*x*x",
    "\\x -> 9*x*x"
  ]

-- |
--    KEY: convert 'String' to 'Vector3' 'GLdouble'
--
--    @
--     Input:
--     vector3
--     0.3 0.4 0.0
--     0.2 0.3 0.0
--     endvector3
--
--     => [Vector3 0.3 0.4 0.0, Vector3 0.2 0.3 0.0]
--    @
takeVector3 :: [String] -> [Vector3 GLdouble]
takeVector3 [] = []
takeVector3 cx = cs
  where
    beg = "vector3"
    end = "endvector3"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map (\x -> strToVector3 x) ss

-- |
--    KEY: convert 'String' to 'Vertex3' 'GLfloat'
--
--    @
--     Input:
--     triangle
--     0.3 0.4 0.0
--     0.2 0.3 0.0
--     0.1 0.2 0.0
--     endtriangle
--
--     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0, Vertex3 0.1 0.2 0.0]
--    @
takeTriangleVex :: [String] -> [Vertex3 GLfloat]
takeTriangleVex [] = []
takeTriangleVex cx = xs
  where
    beg = "triangle"
    end = "endtriangle"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    xs = map (\x -> strToVertex3 x) ss

-- |
--    === KEY: vertex to tuple
vertex3Triple :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]
vertex3Triple cx = ts
  where
    ls = let s = partList 3 cx in if (len . last) s == 3 then s else init s
    ts = map (\(a : b : c : _) -> (a, b, c)) ls

-- |
--    === KEY: Convert a list vertices to tuple3 vertices
--
--    @
--     [Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 0.3, Vertex3 0.4 0.4 0.4]
--
--     => [(Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 03)]
--    @
listToTuple3Vex :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]
listToTuple3Vex cx =
  let lss = partList 3 cx
      lst =
        if len lss > 0
          then
            ( let s = last lss
               in len s == 3 ? lss $ init lss
            )
          else lss
   in map
        ( \x ->
            let a = x ! 0
                b = x ! 1
                c = x ! 2
                (!) = (!!)
             in (a, b, c)
        )
        lst ::
        [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]

listToTuple3VexD :: [Vertex3 GLdouble] -> [(Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble)]
listToTuple3VexD cx =
  let lss = partList 3 cx
      lst =
        if len lss > 0
          then (let s = last lss in len s == 3 ? lss $ init lss)
          else lss
   in map
        ( \x ->
            let a = x ! 0
                b = x ! 1
                c = x ! 2
                (!) = (!!)
             in (a, b, c)
        )
        lst ::
        [(Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble)]

-- |
--    === KEY: Convert a list vertices to tuple2 vertices
--
--    @
--     [Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 0.3, Vertex3 0.4 0.4 0.4]
--
--     => [(Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2),
--         (Vertex3 0.3 0.3 0.3,  Vertex3 0.4 0.4 0.4)
--        ]
--    @
listToTuple2Vex :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
listToTuple2Vex cx =
  let lss = partList num cx
      lst =
        if ρ lss > 0
          then
            ( let s = last lss
               in ρ s == num ? lss $ init lss
            )
          else lss
   in map
        ( \x ->
            let a = x ! 0
                b = x ! 1
                (!) = (!!)
             in (a, b)
        )
        lst ::
        [(Vertex3 GLfloat, Vertex3 GLfloat)]
  where
    num = 2

--   #endif

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
    mw <- G.createWindow 1000 1000 "PlotGeometry" Nothing Nothing
    maybe' mw (G.terminate >> exitFailure) $ \window -> do
      G.makeContextCurrent mw

      ref <- newIORef initCam
      refStep <- newIORef initStep
      refGlobal <- newIORef initGlobal
      globalRef <- readIORef refGlobal
      writeIORef refGlobal globalRef

      -- pre cylinderPt
      -- cylinder
      -- writeIORef refGlobal $ setDrawPts globalRef cylinderPt
      -- sphere
      -- randomPts <- randomVertex (10*3)
      randomPts <- convexPts

      -- writeIORef refGlobal $ setDrawPts   globalRef spherePtsX
      modifyIORef refGlobal (\x -> x {drawPts_ = spherePtsX})

      globalRef2 <- readIORef refGlobal
      -- writeIORef refGlobal $ setRandomPts globalRef2 randomPts
      modifyIORef refGlobal (\x -> x {randomPts_ = randomPts})

      refFrame <- timeNowMilli >>= \x -> newIORef FrameCount {frameTime = x, frameCount = 1, frameNewCount = 0, frameIndex = 0}

      let cx = circleNArc' (Vertex3 0.4 0 0) 0.4 40 (0, pi)
      let cy = curvePtK (const 0) (0, 0.8) 40
      let cx' = [cx, cy]

      ls <- randomIntList 10 (1, 4) >>= \cx -> return $ randomVexList (Vertex3 0.0 0.0 0.0) cx
      modifyIORef refGlobal (\x -> x {randomWalk_ = ls})

      lt <- randomIntList 10 (1, 4) >>= \cx -> return $ randomVexListInt (-8, 0) cx
      modifyIORef refGlobal (\x -> x {randomWalkInt_ = lt})

      let rr = initRectGrid
      let nx = div (xCount_ rr) 2
      let ny = div (yCount_ rr) 2

      let blockAttr = BlockAttr{isFilled_ = False, typeId_ = 0, tetrisNum_ = 0, color_ = green}
      ioArray <- DAO.newArray ((-nx, -ny, 0) , (nx - 1, ny - 1, 0)) blockAttr :: IO(IOArray (Int, Int, Int) BlockAttr)
      animaStateArr <- initAnimaState
      -- ioArray <- DAO.newArray ((0, -ny, -nx) , (0, ny - 1, nx - 1)) blockAttr :: IO(IOArray (Int, Int, Int) BlockAttr)
      {--
      ioArray <- let x0 = -nx
                     x1 =  nx - 1
                     y0 = -ny
                     y1 =  ny - 1
                     z0 =  0
                     z1 =  0
                     xx = x1 - x0 + 1
                     yy = y1 - y0 + 1
                     zz = z1 - z0 + 1
                 in DAO.newListArray ((0, -ny, -nx) , (0, ny - 1, nx - 1)) $ replicate (xx * yy * zz) blockAttr :: IO(IOArray (Int, Int, Int) BlockAttr)
      --}
      mainLoop window ref refStep refGlobal refFrame animaStateArr cx' ioArray
      G.destroyWindow window
      G.terminate
      exitSuccess
  
-- |
--    KEY: convert 'String' to 'Vertex3' 'GLfloat'
--
--    @
--     Input:
--     segment
--     0.3 0.4 0.0
--     0.2 0.3 0.0
--     0.1 0.2 0.0
--     endsegment
--
--     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0, Vertex3 0.1 0.2 0.0]
--    @
takeSegment :: [String] -> [Vertex3 GLfloat]
takeSegment [] = []
takeSegment cx = cs
  where
    beg = "segment"
    end = "endsegment"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map strToVertex3 ss

-- |
--    KEY: convert 'String' to 'Vertex3' 'GLfloat'
--
--    @
--     Input:
--     point
--     0.3 0.4 0.0
--     0.2 0.3 0.0
--     endpoint
--
--     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0]
--    @
takePoint :: [String] -> [Vertex3 GLfloat]
takePoint [] = []
takePoint cx = cs
  where
    beg = "point"
    end = "endpoint"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map strToVertex3 ss

circleNArc' :: Vertex3 GLfloat -> Double -> Integer -> (GLfloat, GLfloat) -> [Vertex3 GLfloat]
circleNArc' (Vertex3 x₀ y₀ z₀) r n (r₀, r₁) =
  [ let δ = (r₁ - r₀) / (rf n); r' = rf r
     in Vertex3 (r' * cos (r₀ + (rf x) * δ) + x₀) (r' * sin (r₀ + (rf x) * δ) + y₀) z₀
    | x <- [0 .. n]
  ]

-- |
--   [0, 1, 2]
--   x0 -- x1 -- x2
--
--   curvePtK::(GLfloat -> GLfloat)->(GLfloat,    GLfloat) -> Integer ->[Vertex3 GLfloat]
--                                     ↑             ↑          ↑
--                                     +-  interval  +          |
--                                                              + - n steps
--                                     |             |
--                                     x_0           x_1
--
--
--                                      (x_1 - x_0) / n
--                    f(x) = x^2
--
--
--    curvePtK f (0, 1.0) 10
--     f(x) = x^2
--    |-----+------|
--    |   x |  x^2 |
--    | 0.1 | 0.01 |
--    | 0.2 | 0.04 |
--    | 0.3 | 0.09 |
--    | 0.4 | 0.16 |
--    | 0.5 | 0.25 |
--    | 0.6 | 0.36 |
--    | 0.7 | 0.49 |
--    | 0.8 | 0.64 |
--    | 0.9 | 0.81 |
--    | 1.0 |  1.0 |
--    |-----+------|
curvePtK :: (GLfloat -> GLfloat) -> (GLfloat, GLfloat) -> Integer -> [Vertex3 GLfloat]
curvePtK f (x₀, x₁) n = [Vertex3 x (f x) 0 | x <- let δ = (x₁ - x₀) / (rf n) in map (\x -> x₀ + (rf x) * δ) [0 .. n]]

-- |
--   === KEY: use multMatrix, multiply matrix, multiply matrix with vector
--
--   https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
--
--  @
--      (cos α  + i sin α) ( cos β + i sin β)
--    = cos α  * cos β + sin α * cos β + i cos α * cos β + i cos α * sin β
--    = (cos α  * cos β + sin α * cos β) + i (cos α * cos β + cos α * sin β)
--    = cos (α + β) + i sin (α + β)
--
--     m 1    => 0
--       0       1
--
--     m 0    => -1
--       1       0
--
--    1  0      0 -1
--    0  1      1  0
--
--                     →  X -  \
--                              ↓
--                     |       Y
--                     \      /
--                       Z  ←
--
--                Right-hand Rule
--
--                      y
--                      ^
--                      |
--                      + - ->   x
--                     /
--                    /
--                   z
--
--                 x (x) y => Z
--             rotate around Z-axis
--             cos x   -sin x  0
--             sin x    cos x  0
--             0        0      1
--                   ↓
--             Swap row(2, 3) => M = -M
--                   ↓
--
--             rotate around X-axis
--             cos x   sin x  0
--             0        0      1
--             -sin x    cos x  0
--
--                    ↓
--             Swap row (1, 2) => M = -M
--                    ↓
--
--             rotate around Y-axis
--             0        0      1
--             cos x   -sin x  0
--             sin x    cos x  0
--  @
testmultMatrix :: IO ()
testmultMatrix = do
  let x = 1.0 :: GLfloat
  let y = 0.0 :: GLfloat
  let z = 0.0 :: GLfloat
  mat <-
    newMatrix
      RowMajor
      [ 1,
        0,
        0,
        x,
        0,
        1,
        0,
        y,
        0,
        0,
        1,
        z,
        0,
        0,
        0,
        1
      ] ::
      IO (GLmatrix GLfloat)

  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
  -- multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
  GL.multMatrix mat

  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
  -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
  ls <- getMatrixComponents RowMajor mat -- [GLfloat]
  -- pre ls
  writeFileList "/tmp/m.x" $ map show ls

multModelviewVec :: Vertex3 GLfloat -> IO [GLfloat]
multModelviewVec (Vertex3 x y z) = do
  --                      let x = 1.0::GLfloat
  --                      let y = 0.0::GLfloat
  --                      let z = 0.0::GLfloat
  mat <-
    newMatrix
      RowMajor
      [ 1,
        0,
        0,
        x,
        0,
        1,
        0,
        y,
        0,
        0,
        1,
        z,
        0,
        0,
        0,
        1
      ] ::
      IO (GLmatrix GLfloat)

  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
  -- multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
  GL.multMatrix mat

  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
  -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
  ls <- getMatrixComponents RowMajor mat -- [GLfloat]
  -- pre ls
  writeFileList "/tmp/m.x" $ map show ls
  return ls

getMatrixTest :: IO ()
getMatrixTest = do
  -- let pnameMatrix = getMatrix $ matrixModeToGetMatrix Projection
  -- matrixModeToGetMatrix :: MatrixMode -> GetPName
  -- let m = matrixModeToGetMatrix Projection

  -- matrixModeToGetMatrix is from hidden module
  -- Use src/Graphics
  -- let m = matrixModeToGetMatrix $ Modelview 16
  -- getMatrixf :: p -> Ptr GLfloat -> IO ()
  --                 let ls = [ 0, 0, 0, 0,
  --                            0, 0, 0, 0,
  --                            0, 0, 0, 0,
  --                            0, 0, 0, 0
  --                          ]::[GLfloat]
  --
  --                 getMatrixf m (Ptr ls)
  fl
  fl
  -- let m2 = getMatrix m
  return ()

getModelviewMatrix :: IO [GLfloat]
getModelviewMatrix = do
  let stateVar = GL.matrix (Just $ Modelview 16) :: StateVar (GLmatrix GLfloat)
  m1 <- Data.StateVar.get stateVar
  pre m1
  -- ls <- getMatrixComponents RowMajor m1  -- [GLfloat]
  ls <- getMatrixComponents ColumnMajor m1 -- [GLfloat]
  pre ls
  writeFileList "/tmp/m1.x" $ map show ls
  return ls

matrixTest :: IO ()
matrixTest = do
  let stateVar = GL.matrix (Just $ Modelview 16) :: StateVar (GLmatrix GLfloat)
  m1 <- Data.StateVar.get stateVar
  pre m1
  ls <- getMatrixComponents RowMajor m1 -- [GLfloat]
  pre ls
  writeFileList "/tmp/m1.x" $ map show ls
  return ()

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

-- |
--   x  y  z
--   0  1  0
--   1  0  0   XOR
--  ---------
--   1  1  0
--
--   x  y  z
--   0  1  0
--   0  1  0   XOR
--  ---------
--   0  0  0
--
--   x  y  z
--   0  1  0
--   0  0  1   XOR
--  ---------
--   0  1  1
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

xAxis :: XYZAxis
xAxis = XYZAxis {xa = True, ya = False, za = False}

yAxis :: XYZAxis
yAxis = XYZAxis {xa = False, ya = True, za = False}

zAxis :: XYZAxis
zAxis = XYZAxis {xa = False, ya = False, za = True}

initXYZAxis :: XYZAxis
initXYZAxis = XYZAxis {xa = False, ya = False, za = False}

moveBrick :: (Int, Int) -> [[((Int, Int), Int)]] -> [[((Int, Int), Int)]]
moveBrick (x, y) cx = (map . map) (\(x0, y0) -> ((***) (+ x) (+ y) x0, y0)) cx

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

randomBlockX :: IORef GlobalRef -> IO (BlockAttr, [[Int]])
randomBlockX ref = do
            let tet1 =
                  [
                       [0, 0, 0, 0, 0],
                       [0, 0, 1, 0, 0],
                       [0, 1, 1, 1, 0],
                       [0, 0, 0, 0, 0],
                       [0, 0, 0, 0, 0]
                   ]
            let tet2 =
                  [
                       [0, 0, 0, 0, 0],
                       [0, 0, 0, 0, 0],
                       [1, 1, 1, 1, 1],
                       [0, 0, 0, 0, 0],
                       [0, 0, 0, 0, 0]
                   ]
            tetrisCount <- readIORef ref <&> tetrisCount_
            let t1 = 1
            let t2 = 2
            let br1 = BlockAttr{isFilled_ = True, typeId_ = t1, tetrisNum_ = 0, color_ = white}
            let ls = [(tet1, white, t1), (tet2, cyan, t2)]
            inx <- randomInt 0 (len ls - 1)
            let br = ls !! inx
            ranColor <- randomColor
            let bb = (br1{typeId_ = br ^._3,  tetrisNum_ = tetrisCount + 1, color_ = ranColor }, br ^._1)
            modifyIORef ref (\x -> x{tetrisCount_ = tetrisCount + 1})
            return bb
  
initGlobal :: GlobalRef
initGlobal =
  GlobalRef
    { str_ = "",
      cursor_ = (0.0, 0.0),
      xyzAxis_ = initXYZAxis,
      mousePressed_ = (False, (0.0, 0.0)),
      drawRectX_ = (Vertex3 (-0.2) (-0.2) (0.0 :: GLfloat), Vertex3 0.2 0.2 (0.0 :: GLfloat)),
      tranDrawRectX_ = Vector3 0.0 0.0 (0.0 :: GLdouble),
      fovDegree_ = 100.0,
      drawPts_ = [[Vertex3 0.0 0.0 0.0]],
      randomWalk_ = [Vertex3 0.0 0.0 0.0, Vertex3 0.1 0.1 0.1],
      boardMap_ = DM.empty,
      boardMap1_ = DM.empty,
      moveX_ = 0,
      moveY_ = 0,
      block1_ =
        [ ((0 - 2, 0 + 8), gray),
          ((1 - 2, 0 + 8), gray),
          ((2 - 2, 0 + 8), gray),
          ((3 - 2, 0 + 8), gray),
          ((4 - 2, 0 + 8), gray)
        ],
      rectGrid_ = initRectGrid,
      centerBrick_ = map (\y -> map (\x -> (x - 2, y - 2)) [0 .. (len $ head $ bk1_ initGlobal) - 1]) $ reverse [0 .. (len $ bk1_ initGlobal) - 1],
      bk1_ =
        [ [0, 0, 0, 0, 0],
          [0, 0, 1, 0, 0],
          [0, 1, 1, 1, 0],
          [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0]
        ],
      bk2_ =
        [ [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (1, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (1, 0, green), (1, 0, green), (1, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)]
        ],
      rot_ = False,
      rotDeg_ = 0,
      time1_ = 0,
      count1_ = 10000000,
      rotN_ = 0,
      blockCount_ = 1,
      tetrisCount_ = 1,
      tetris1_ = mkTetris1 (blockCount_ initGlobal) 0,
      tetris1X_ = (BlockAttr {isFilled_ = True, typeId_ = 1, tetrisNum_ = (blockCount_ initGlobal), color_ = blue}, bk1_ initGlobal) 
    }

  
{--

```
data MyRec = {a_ :: Int, b_ :: Int} deriving (Show)

let r = MyRec = {a_ = 3, b_ = a_ + 4}
```

  ---------------------------------------m1---------------------------------------
  bk1_ =
  0 0 0 0 0
  0 0 0 0 0
  1 1 1 1 1
  0 0 0 0 0
  0 0 0 0 0
  ---------------------------------------sk---------------------------------------
  centerBrick_ =
  ((-2,2),0)  ((-1,2),0)  ((0,2),0)  ((1,2),0)  ((2,2),0)
  ((-2,1),0)  ((-1,1),0)  ((0,1),0)  ((1,1),0)  ((2,1),0)
  ((-2,0),1)  ((-1,0),1)  ((0,0),1)  ((1,0),1)  ((2,0),1)
  ((-2,-1),0) ((-1,-1),0) ((0,-1),0) ((1,-1),0) ((2,-1),0)
  ((-2,-2),0) ((-1,-2),0) ((0,-2),0) ((1,-2),0) ((2,-2),0)
--}

-- |
--    KEY:
--    NOTE: USED
keyBoardCallBack2 :: IORef Step -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
keyBoardCallBack2 refStep refGlobalRef ioArray window key scanCode keyState modKeys = do
  pp "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
  putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
  globalRef <- readIORef refGlobalRef
  let axisOld = xyzAxis_ globalRef
  let fovOld = fovDegree_ globalRef
  logFileG ["fovOld=" ++ show fovOld]
  rr <- readIORef refGlobalRef <&> rectGrid_
  case keyState of
    ks
      | ks `elem` [G.KeyState'Pressed, G.KeyState'Repeating] -> do
        -- G.KeyState'Pressed -> do
        -- write Step{...} to ref
        case key of
          k
            | k == G.Key'Right -> writeIORef refStep Step {xx = _STEP, yy = 0.0, zz = 0.0, ww = 0.0}
            | k == G.Key'Left -> writeIORef refStep Step {xx = -_STEP, yy = 0.0, zz = 0.0, ww = 0.0}
            | k == G.Key'Up -> writeIORef refStep Step {xx = 0.0, yy = _STEP, zz = 0.0, ww = 0.0}
            | k == G.Key'Down -> writeIORef refStep Step {xx = 0.0, yy = -_STEP, zz = 0.0, ww = 0.0}
            | k == G.Key'9 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = _STEP, ww = 0.0}
            | k == G.Key'0 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = - _STEP, ww = 0.0}
            | k == G.Key'8 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = _STEP}
            | k == G.Key'7 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = - _STEP}
            | k == G.Key'X -> modifyIORef refGlobalRef (\x -> x {xyzAxis_ = flipAxis (xyzAxis_ x) xAxis})
            --                                  ↑
            --                                  + -> Update Coord to YZ-plane

            | k == G.Key'Y -> modifyIORef refGlobalRef (\x -> x {xyzAxis_ = flipAxis (xyzAxis_ x) yAxis})
            --                                  ↑
            --                                  + -> Update Coord to XZ-plane

            | k == G.Key'Z -> modifyIORef refGlobalRef (\x -> x {xyzAxis_ = flipAxis (xyzAxis_ x) zAxis})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane

            -- zoom out
            | k == G.Key'O -> modifyIORef refGlobalRef (\x -> x {fovDegree_ = fovDegree_ x + 5.0})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
            -- zoom in
            | k == G.Key'I -> modifyIORef refGlobalRef (\x -> x {fovDegree_ = fovDegree_ x - 5.0})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane

            -- TODO: In orthogonal projective status,
            | k == G.Key'Space -> writeIORef refStep initStep
            {--
            | k == G.Key'R -> do
                              bmap <- readIORef refGlobalRef >>= return . boardMap_
                              block1 <- readIORef refGlobalRef >>= \x -> return $ map fst $ block1_ x
                              modifyIORef refGlobalRef (\s -> s{moveX_ =  let mx = moveX_ s
                                                                              my = moveY_ s
                                                                              m = map (\(a, b) -> (a + mx + 1, b + my)) block1
                                                                              b = checkMove m bmap rr
                                                                          in  b ? mx + 1 $ mx
                                                               }
                                                       )
                              print block1
            --}
  
            | k == G.Key'W -> do
              nowTime <- timeNowMilli
              modifyIORef refGlobalRef (\s -> s {count1_ = 0})
              modifyIORef refGlobalRef (\s -> s {time1_ = nowTime})
              modifyIORef refGlobalRef (\s -> s {rot_ = True})
              fw "Rotate Block"
              pp "rotate me"
            | k == G.Key'A -> do
              moveX <- readIORef refGlobalRef <&> moveX_
              moveY <- readIORef refGlobalRef <&> moveY_
              bmap <- readIORef refGlobalRef <&> boardMap_
              bmapX <- readIORef refGlobalRef <&> boardMap1_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              rotN <- readIORef refGlobalRef <&> rotN_
              bk1 <- readIORef refGlobalRef <&> bk1_
              tet <- readIORef refGlobalRef <&> tetris1_
              tetX <- readIORef refGlobalRef <&> tetris1X_
              array <- getAssocs ioArray
              -- let bk1' = rotateN 1 bk1
              let bk1' = rotateN 1 (snd tetX)
              -- let bk1'X = rotateN 0 (tet ^._4)
              let bk1'X = rotateN 0 (snd tetX)

              -- /Users/aaa/myfile/bitbucket/tmp/xx_5248.x
              let currBr = innerBrick (moveX, moveY) centerBrick bk1'
              let currBrX = innerBrick (moveX, moveY) centerBrick bk1'X
              let currBrXX = map (\(x, y) -> (x, y, 0)) currBrX
              let -- b = checkMove currBr bmap rr
              -- let bX = checkMoveX currBrX bmapX rr
              let bX = checkMoveArr currBrXX array rr
              -- modifyIORef refGlobalRef (\s -> s{bk1_ = b ? bk1' $ bk1})
              -- modifyIORef refGlobalRef (\s -> s {count1_ = b ? 0 $ count1_ s})
              modifyIORef refGlobalRef (\s -> s {count1_ = bX ? 0 $ count1_ s})
              -- modifyIORef refGlobalRef (\s -> s{rotN_ = bX ? (let n = (rotN_ s) + 1 in mod n 4 )$ rotN_ s})
              pp "rotateN 1"
            | k == G.Key'L || k == G.Key'R -> do
              bmap <- readIORef refGlobalRef <&> boardMap_
              bmapX <- readIORef refGlobalRef <&> boardMap1_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              rotN <- readIORef refGlobalRef <&> rotN_
              -- bk1 <- readIORef refGlobalRef >>= return . bk1_
              tet <- readIORef refGlobalRef <&> tetris1_
              tetX <- readIORef refGlobalRef <&> tetris1X_
              let bk1' = rotateN rotN bk1
              -- let bk1'X = rotateN rotN (tet ^._4)
              let bk1'X = rotateN rotN (snd tetX)
              lsArr <- getAssocs ioArray
              -- let mk = (join . join) $ (map . map) fst $ (map . filter) (\(_, n) -> n == 1) $ (map . zip) centerBrick bk1
              -- let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
              -- let ls = map fst lz
              -- let ls = getShape centerBrick bk1'
              let lsX = getShape centerBrick bk1'X
              logFileG ["lsX"]
              logFileG $ map show lsX
              logFileG ["lsArr"]
              logFileG $ map show lsArr
              -- block1 <- readIORef refGlobalRef >>= \x -> return $ map fst $ block1_ x
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
            | k == G.Key'U -> do
              bmap <- readIORef refGlobalRef <&> boardMap_
              bmapX <- readIORef refGlobalRef <&> boardMap1_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              rotN <- readIORef refGlobalRef <&> rotN_
              bk1 <- readIORef refGlobalRef <&> bk1_
              tet <- readIORef refGlobalRef <&> tetris1_
              let bk1' = rotateN rotN bk1
              let bk1'X = rotateN rotN (tet ^._4)

              -- let mk = (join . join) $ (map . map) fst $ (map . filter) (\(_, n) -> n == 1) $ (map . zip) centerBrick bk1
              let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
              let ls = map fst lz
              let lzX = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'X
              let lsX = map fst lzX
              -- block1 <- readIORef refGlobalRef >>= \x -> return $ map fst $ block1_ x
              {--
              modifyIORef
                refGlobalRef
                ( \s ->
                    s
                      { moveY_ =
                          let mx = moveX_ s
                              my = moveY_ s
                              m = map (\(a, b) -> (a + mx, b + my + 1)) lsX
                              b = checkMoveX m bmapX rr
                           in b ? my + 1 $ my
                      }
                )
              --}

              print "kk"
            | k == G.Key'D -> do
              mx <- readIORef refGlobalRef <&>  moveX_
              my <- readIORef refGlobalRef <&> moveY_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              tetX <- readIORef refGlobalRef <&> tetris1X_
              rotN <- readIORef refGlobalRef <&> rotN_
              bk1 <- readIORef refGlobalRef <&> bk1_
              lsArr <- getAssocs ioArray
              -- let bk1' = rotateN rotN bk1

              -- let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
              -- let ls = map fst lz
              -- let ls = getShape centerBrick bk1'
              -- let ls = getShape centerBrick $ rotateN rotN (tet ^._4)
              let ls = getShape centerBrick $ rotateN rotN (snd tetX)

              
              let mX = map (\(a, b) -> (a + mx, b + my - 1, 0)) ls
              
              let b = checkMoveArr mX lsArr rr
              
              when (not b) $ do
                let ls3 = map (\(a, b) -> ((a + mx, b + my, 0), fst tetX)) ls
                mapM_ (uncurry $ DAO.writeArray ioArray) ls3
  
              newBlock <- randomBlockX refGlobalRef
              modifyIORef
                refGlobalRef
                ( \s ->
                    s
                      { moveX_ = b ? moveX_ s $ 0,
                        moveY_ = b ? my - 1 $ 0,
                        count1_ = b ? count1_ s $ 10000000,
                        rotN_ = b ? rotN_ s $ 0,
                        tetris1X_ = b ? tetris1X_ s $ newBlock
                      }
                )
  
              print "kk"
            | otherwise -> pp $ "Unknown Key Press" ++ show key
      | ks == G.KeyState'Released -> do
        -- G.KeyState'Released -> do
        if key == G.Key'Right then pp "Release Key => Right" else pp "Press No Right"
        if key == G.Key'Left then pp "Release Key => left" else pp "Press No Right"
        if key == G.Key'Up then pp "Release Key => up" else pp "Release No Up"
        if key == G.Key'Down then pp "Release Key => Down" else pp "Release No Down"
      -- if key == G.Key'R  then modifyIORef refGlobalRef (\x -> x{moveX_ = 0}) else pp "Release No Down"
      -- if key == G.Key'L  then modifyIORef refGlobalRef (\x -> x{moveY_ = 0}) else pp "Release No Down"
      | otherwise -> pp "Unknow keyState"
  when
    (key == G.Key'Escape && keyState == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)

-- |
--
--                    | upLeftY
--         upLeftX -> v +----------+
--                                 |
--                                 |
--                      +          +
--                                 ^ <-  downRightX
--                                 |
--                              downRightY
--
--                                 + -> upLeft
--                                 |
--                                 |            + -> downRight
--                                 |            |                    + -> cursor pos
--                                 ↓            ↓                    ↓
drawRectX :: G.Window -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat) -> IO ()
drawRectX w (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1)) c@(x, y) = do
  -- NDC [-x, +x] = [-2, +2], [-y, +y] = [-2, +2]
  let ndcWidth = 4.0
  (width, height) <- G.getFramebufferSize w
  (winW, winH) <- G.getWindowSize w >>= \(w, h) -> return (rf w, rf h)
  -- let (winW, winH) = (rf winWidth, rf winHeight)
  let (w, h) = (rf width, rf height)
  let upLeftX = winW / 2 + x0 * (winW / ndcWidth)
  let upLeftY = winH / 2 + y0 * (winH / ndcWidth)
  let downRightX = winW / 2 + x1 * (winW / ndcWidth)
  let downRightY = winH / 2 + y1 * (winH / ndcWidth)
  logFileG ["upLeftX=" ++ sw upLeftX, "upLeftY=" ++ sw upLeftY, "downRightX=" ++ sw downRightX, "downRightY=" ++ sw downRightY]
  logFileG ["FrameBuffer.x=" ++ sw w, "FrameBuffer.y=" ++ sw h]
  logFileG ["WindowSize.x=" ++ sw winW, "WindowSize.y=" ++ sw winH]
  if upLeftX <= x && x <= downRightX && upLeftY <= y && y <= downRightY
    then do
      drawBox green
    else do
      drawBox red
  where
    sw = show
    drawSeg = drawSegmentNoEnd
    drawBox color = do
      drawSeg color (Vertex3 x0 y0 z0, Vertex3 x1 y0 z0) --  top horizontal
      drawSeg color (Vertex3 x0 y0 z0, Vertex3 x0 y1 z0) --  left vertical
      drawSeg color (Vertex3 x1 y0 z0, Vertex3 x1 y1 z1) --  right vertical
      drawSeg color (Vertex3 x0 y1 z0, Vertex3 x1 y1 z1) --  bottom horizontal

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

drawRectX2 :: G.Window -> IORef GlobalRef -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat) -> IO ()
drawRectX2 w ioGlobalRef (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1)) c@(x, y) = do
  preservingMatrix $ do
    tvec <- getTranVecDrawRectX ioGlobalRef
    (isPre, _) <- getMousePressed ioGlobalRef
    drawRectMouse w tvec (p0, p1) (x, y, 0.0) isPre

drawRectMouse :: G.Window -> (Vector3 GLdouble) -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat, GLfloat) -> Bool -> IO ()
drawRectMouse w (Vector3 vx vy vz) (p0, p1) (mx, my, mz) isPre = do
  let tvec = Vector3 vx vy vz
  logFileG [show tvec]
  translate tvec
  --
  --      tvec                                          tvec'
  --        ↓                                             ↓
  --   mouse movement coord-system
  --     ↓                                      ^    translate, coord-system
  --  (0, 0)                                    |
  --    + - - - ->                              | (0,0)
  --    |                 =>         ---------- + - - - - - ->
  --    |                                       |
  --    |                                       |
  --    v                                       |
  --                                            |
  --
  --    x-axis is the same direction
  --    y-axis is opposive direction
  --
  --  =>  (x, y, z) => (x, -y, z)
  --
  let tvec' = Vector3 vx (- vy) vz
  (winWidth, winHeight) <- G.getWindowSize w
  let (winW, winH) = (rf winWidth, rf winHeight)
  isIn <- isPtInsideRect w (p0 +: (d2f tvec'), p1 +: (d2f tvec')) (mx, my)
  drawRectColor (isPre && isIn ? green $ red) (p0, p1)

d2f :: Vector3 GLdouble -> Vector3 GLfloat
d2f (Vector3 x y z) = Vector3 x' y' z'
  where
    x' = rf x
    y' = rf y
    z' = rf z

-- |
--    === KEY: check whether point (x, y) is inside the rectangle
--
--    @
--                    | upLeftY
--         upLeftX -> v +----------+
--                                 |
--                                 |
--                      +          +
--                                 ^ <-  downRightX
--                                 |
--                              downRightY
--                                                                             +- - -> translate Vector3
--                                 + -> upLeft                                 |
--                                 |                                           |
--                                 |                     + -> downRight        |
--                                 |                     |                     |                    +-> cursor pos
--                                 ↓                     ↓                     ↓                    ↓
--    @
isPtInsideRectTran :: G.Window -> (Vertex3 GLfloat, Vertex3 GLfloat) -> Vector3 GLdouble -> IO Bool
isPtInsideRectTran w (p0, p1) (Vector3 a b c) = do
  cursorPos <- getCursorPosf w -- IO (GLfloat, GLfloat)
  let tvec = Vector3 a (- b) c
  isPtInsideRect w (p0 +: (d2f tvec), p1 +: (d2f tvec)) cursorPos

isPtInsideRect :: G.Window -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat) -> IO Bool
isPtInsideRect w (Vertex3 x0 y0 z0, Vertex3 x1 y1 z1) (x, y) = do
  let ndcWidth = 4.0
  (winWidth, winHeight) <- G.getWindowSize w
  let (winW, winH) = (rf winWidth, rf winHeight)
  -- let (w, h) = (rf width, rf height)
  --
  --                 + -> Shift to the Right in half window
  --                 ↓
  let upLeftX = winW / 2 + x0 * (winW / ndcWidth)
  let upLeftY = winH / 2 + y0 * (winH / ndcWidth)
  let downRightX = winW / 2 + x1 * (winW / ndcWidth)
  let downRightY = winH / 2 + y1 * (winH / ndcWidth)
  if upLeftX <= x && x <= downRightX && upLeftY <= y && y <= downRightY
    then return True
    else return False

-- |
--    KEY: getter for fovDegree_
--    NOTE: zoom in, zoom out
getFOVDegree :: IORef GlobalRef -> IO GLdouble
getFOVDegree ioGlobalRef = readIORef ioGlobalRef <&> fovDegree_

-- |
--    KEY: getter for str_
getStr :: IORef GlobalRef -> IO String
getStr ioGlobalRef = readIORef ioGlobalRef <&>str_

-- |
--    KEY: getter for xyzAxis_
getXYZAxis :: IORef GlobalRef -> IO XYZAxis
getXYZAxis ioGlobalRef = readIORef ioGlobalRef <&> xyzAxis_

-- |
--    KEY: getter for drawPts_
getDrawPts :: IORef GlobalRef -> IO [[Vertex3 GLfloat]]
getDrawPts ioGlobalRef = readIORef ioGlobalRef <&> drawPts_

getRandomPts :: IORef GlobalRef -> IO [Vertex3 GLfloat]
getRandomPts ioGlobalRef = readIORef ioGlobalRef <&> randomPts_

-- |
--    KEY:
getCursorPosf :: G.Window -> IO (GLfloat, GLfloat)
getCursorPosf w = G.getCursorPos w >>= \(x, y) -> return (rf x, rf y) :: IO (GLfloat, GLfloat)

-- |
--    KEY:
getMousePressed :: IORef GlobalRef -> IO (Bool, (GLfloat, GLfloat))
getMousePressed ioGlobalRef = readIORef ioGlobalRef <&>  mousePressed_

-- |
--    KEY:
getCursor :: IORef GlobalRef -> IO (GLfloat, GLfloat)
getCursor ioGlobalRef = readIORef ioGlobalRef <&> cursor_

-- |
--    KEY:
getDrawRectX :: IORef GlobalRef -> IO (Vertex3 GLfloat, Vertex3 GLfloat)
getDrawRectX ioGlobalRef = readIORef ioGlobalRef <&> drawRectX_

getTranVecDrawRectX :: IORef GlobalRef -> IO (Vector3 GLdouble)
getTranVecDrawRectX ioGlobalRef = readIORef ioGlobalRef <&> tranDrawRectX_

-- |
--    KEY:
--
--    t0 pressed
--       (x0, y0)
--
--    t1 released
--       (x1, y1)
mouseCallback :: IORef GlobalRef -> G.MouseButtonCallback
mouseCallback globalRef window but butState mk = do
  case butState of
    G.MouseButtonState'Pressed -> do
      case but of
        v
          | v == G.MouseButton'1 -> do
            (fbw, fbh) <- G.getFramebufferSize window
            pos <- G.getCursorPos window >>= \(x, y) -> return  (rf x, rf y) :: IO (GLfloat, GLfloat)
            ws <- G.getWindowSize window
            let str = PR.printf "cx=%.2f cy=%.2f wx=%d wy=%d bx=%d by=%d" (fst pos) (snd pos) (fst ws) (snd ws) fbw fbh :: String

            gRef <- readIORef globalRef
            -- ↑
            -- +---------------------------+
            --                             ↓
            -- writeIORef globalRef $ setStr gRef str
            modifyIORef globalRef (\x -> x {str_ = str})

            -- newGlobalRef <- readIORef globalRef >>= return . setCursor pos
            -- readIORef globalRef >>= return . setCursor pos >>= \x -> writeIORef globalRef $ setMousePressed (True, pos) x
            -- readIORef globalRef >>= return . setCursor pos >>= \x -> writeIORef globalRef $ setMousePressed (True, pos) x
            modifyIORef globalRef (\x -> x {cursor_ = pos})
            modifyIORef globalRef (\x -> x {mousePressed_ = (True, pos)})

            --  ↑
            --  +--------------------------------------------------+
            --                                                     ↓
            -- writeIORef globalRef $ setMousePressed (True, pos) newGlobalRef

            pp str
          | otherwise -> pp "No button pressed"
    G.MouseButtonState'Released -> do
      -- pos <- G.getCursorPos window >>= \(x, y) -> return $ (rf x, rf y) :: IO (GLfloat, GLfloat)
      let pos = (0.0, 0.0)
      -- readIORef globalRef >>= \x -> writeIORef globalRef $ setMousePressed (False, pos) x
      modifyIORef globalRef (\x -> x {mousePressed_ = (False, pos)})

      pp "Button Released"

{--
Fri 24 Mar 10:46:03 2023
DELETE IT
show3dStr::String -> Color3 GLdouble ->  GLdouble -> IO()
show3dStr str c u = do
    preservingMatrix $ do
        translate (Vector3 u 0 0 :: Vector3 GLdouble)
        rotate (90)$ (Vector3 0 0 1 :: Vector3 GLdouble)
         -- coordTip c
    preservingMatrix $ do
        translate (Vector3 (-u) (-u) 0 :: Vector3 GLdouble)
        GL.scale (1/4000 :: GL.GLdouble) (1/4000::GL.GLdouble) 1
        -- Ref: https://hackage.haskell.org/package/gloss-rendering-1.13.1.1/docs/src/Graphics.Gloss.Internals.Rendering.Picture.html#renderPicture
        -- text looks weird when we have got blend on
        GL.blend $= GL.Disabled
        preservingMatrix $ GLUT.renderString GLUT.Roman str
        GL.blend $= GL.Enabled
--}

-- |
--    === KEY: screen coordinates-system to graphic coordinates-system
--
--    @
--        (x, y) <- getCursorPosf w = G.getCursorPos w >>= \(x, y) -> return $ (rf x, rf y)
--        screen Coordinates-System
--
--        [winW = 1000, winH = 1000]
--        topLeft
--         ↓ (0,0)
--         + - - - ->  winW
--         |
--         |
--         |
--         v winH
--
--        Scale XY-axes to [0, 1.0]
--        (winW, winH) <- G.getWindowSize w >>= \(u, v) -> return (rf u, rf v)
--
--        (x/winW, y/winH)
--
--         topLeft
--         ↓ (0,0)
--         + - - - ->  1.0
--         |
--         |
--         |
--         v 1.0
--
--
--         Move (0,0) => (0, 0.5)
--         (x/winW, y/winH - 0.5)
--         topLeft
--         ↓
--         + -     ->  1.0
--         |
-- (0,0)   + - - - ->
--         |
--         v 1.0
--
--         Flip Y-Axis
--         (x/winW, (0.5 - y/winH))
--
--         ^
--         |
--         |
--         + - - - ->
--         |
--         |
--
--         ↑
--         bottomLeft
--
--         Move (0,0) to (0, 0.5), 0.5 on X-axis
--         (0.5, 0.5) => (0.0, 0.0)
--
--         (x/winW - 0.5,  0.5 - y/winH)
--
--                 ^
--                 |
--                 |
--                 | (0,0)
--          -------+------->
--                 |
--                 |
--                 |
--
--
--         Test 1, x = winW/2, y=winH/2 => (0, 0)       ✓
--         Test 2, x = winW,   y=winH   => (0.5, -0.5)  ✓
--         Test 3, x = (1/4)winW, y= (1/4) winH => (0.25 - 0.5, 0.5 - 0.25) = (-0.25, 0.25)  ✓
--
--    @
screenCSToGraphicCS :: G.Window -> (GLfloat, GLfloat) -> IO (Vertex3 GLfloat)
screenCSToGraphicCS ws (wpx, hpx) = do
  let ndcWidth = 4.0
  (winW, winH) <- G.getWindowSize ws >>= \(u, v) -> return (rf u, rf v)
  let cenx = ndcWidth / 2.0
  let ceny = ndcWidth / 2.0
  -- [0, ndcWidth] - (ndcWidth/2)
  -- x: [0, 2] - 1 => [-1, 1], move (0, 0) => (1, 0)
  -- y: [0, 2] - 1 => [-1, 1], flip Y-axis => -1*[-1, 1] => [1, -1]
  let x' = ndcWidth / winW
  let y' = ndcWidth / winH
  return $ Vertex3 (x' * wpx - cenx) (negate (y' * hpx - ceny)) 0.0

normDist :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat
normDist (x0, y0) (x1, y1) = (x1 - x0) ^ 2 + (y1 - y0) ^ 2

drawConvexHull :: [Vertex3 GLfloat] -> IO ()
drawConvexHull pts = do
  let n = len pts
  let ls = convexHull n pts
  mapM_ (\x -> drawDot x) pts
  mapM_ (\x -> drawSegmentWithEndPt red x) ls

-- listTupeToList::[(Vertex3 GLfloat, Vertex3 GLfloat)] -> [[Vertex3 GLfloat]]
-- listTupeToList cx = map (\x -> ((fromJust . head) cx, (fromJust . last) cx) ) cx

seg :: Vertex3 GLfloat -> Vertex3 GLfloat -> [Vertex3 GLfloat]
seg v0 v1 = cmpVex v0 v1 ? [v0, v1] $ [v1, v0]

findAllChildren :: Vertex3 GLfloat -> [(Vertex3 GLfloat, Vertex3 GLfloat)] -> [Vertex3 GLfloat]
findAllChildren v cx = unique $ map fromJust $ filter (/= Nothing) $ map (\(v0, v1) -> v == v0 ? Just v1 $ (v == v1 ? Just v0 $ Nothing)) cx

moveNext :: Vertex3 GLfloat -> IO (Vertex3 GLfloat)
moveNext (Vertex3 x y z) = do
  n <- randomInt 1 4
  let b = 0.8 :: GLfloat
  case n of
    1 -> if x < b then let v = Vertex3 (x + 0.1) y z in return v else moveNext (Vertex3 x y z)
    2 -> if x > - b then let v = Vertex3 (x - 0.1) y z in return v else moveNext (Vertex3 x y z)
    3 -> if y < b then let v = Vertex3 x (y + 0.1) z in return v else moveNext (Vertex3 x y z)
    4 -> if y > - b then let v = Vertex3 x (y - 0.1) z in return v else moveNext (Vertex3 x y z)
    _ -> error "ERROR randomInt"
  

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


getShape:: [[(Int, Int)]] -> [[Int]] -> [(Int, Int)]
getShape centerBrick bk = map fst $ join $ (map . filter) (\(_, n) -> n > 0) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk
  
getShapeX:: [[(Int, Int)]] -> [[Int]] -> [(Int, Int)]
getShapeX centerBrick bk = map fst $ join $ (map . filter) (\(_, n) -> n > 0) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk
  
innerBrick :: (Int, Int) -> [[(Int, Int)]] -> [[Int]] -> [(Int, Int)]
innerBrick (moveX, moveY) centerBrick bk1 = currBr
  where
    f x = map fst $ join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick x
    r0 = bk1
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

drawFirstRect :: IO ()
drawFirstRect = do
  let wx = 0.02; wy = 0.02
  let v (x, y, z) = Vertex3 x y z
  -- top left corner
  let x0 = -1.0; y0 = 1.0
  -- bottom right corner
  let x1 = 1.0; y1 = -1.0
  let vx0 = v (x0, y0, 0)
  let vx1 = v (x1, y1, 0)
  let delx = abs $ (x1 - x0) / 20
  let dely = abs $ (y1 - y0) / 20
  let cx = delx / 2
  let cy = dely / 2
  mapM_
    ( \ny ->
        mapM_
          ( \nx ->
              let ex0 = x0 + nx * delx + wx
                  ey0 = y0 - ny * dely - wy
                  ex1 = x0 + (nx + 1) * delx - wx
                  ey1 = y0 - (ny + 1) * dely + wy
               in drawRectColor yellow (v (ex0, ey0, 0), v (ex1, ey1, 0))
          )
          [0]
    )
    [0]

drawFirstRectColor :: Color3 GLdouble -> IO ()
drawFirstRectColor color = do
  let wx = 0.02; wy = 0.02
  let v (x, y, z) = Vertex3 x y z
  -- top left corner
  let x0 = -1.0; y0 = 1.0
  -- bottom right corner
  let x1 = 1.0; y1 = -1.0
  let vx0 = v (x0, y0, 0)
  let vx1 = v (x1, y1, 0)
  let delx = abs $ (x1 - x0) / 20
  let dely = abs $ (y1 - y0) / 20
  let cx = delx / 2
  let cy = dely / 2
  mapM_
    ( \ny ->
        mapM_
          ( \nx ->
              let ex0 = x0 + nx * delx + wx
                  ey0 = y0 - ny * dely - wy
                  ex1 = x0 + (nx + 1) * delx - wx
                  ey1 = y0 - (ny + 1) * dely + wy
               in drawRectColor color (v (ex0, ey0, 0), v (ex1, ey1, 0))
          )
          [0]
    )
    [0]


{-|

               minY_
                |
        minX_ - +  - -> maxX_
                |
               maxY_

        |<-    xCount_    ->|
                20
-}
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

{--
let sx = [0.. (xCount_ r) - 1] :: [Int]
let sy = [0.. (yCount_ r) - 1] :: [Int]
let wx = xEdge_ r; wy = yEdge_ r
let v (x, y, z) = Vertex3 x y z
-- top left corner
let x0 = minX_ r; y0 = maxY_ r
-- bottom right corner
let x1 = maxY_ r; y1 = minY_ r
let vx0 = v(x0, y0, 0)
let vx1 = v(x1, y1, 0)
let bWidth = (x1 - x0) / (fi $ xCount_ r)
let bHeight = (y0 - y1) / (fi $ yCount_ r)
-- drawRect (vx0, vx1)
mapM_(\ny -> mapM_(\nx -> let ex0 = x0 + (fi $ nx) * bWidth  + wx
                              ey0 = y0 - (fi $ ny) * bHeight - wy
                              ex1 = x0 + (fi $ nx + 1) * bWidth  - wx
                              ey1 = y0 - (fi $ ny + 1) * bHeight + wy
                          in  drawRectColor green (v(ex0, ey0, 0), v(ex1, ey1, 0))
                  ) sx ) sy
--}

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
  let bWidth = (x1 - x0) / (fi $ xCount_ r)
  let bHeight = (y0 - y1) / (fi $ yCount_ r)
  -- drawRect (vx0, vx1)
  let ex0 = x0 + (fi $ nx) * bWidth + wx
      ey0 = y0 - (fi $ ny) * bHeight - wy
      ex1 = x0 + (fi $ nx + 1) * bWidth - wx
      ey1 = y0 - (fi $ ny + 1) * bHeight + wy
   in drawRectColor green (v (ex0, ey0, 0), v (ex1, ey1, 0))

-- |
--      centerBlockMove (2, 1) (0.1, 0.1) green
--
--             | +
--             + -
--
--                 | +
--             |   + -
--             + - - -
centerBlockMove :: (Int, Int) -> [((Int, Int), Bool)] -> (GLfloat, GLfloat) -> Color3 GLdouble -> IO ()
centerBlockMove c@(nx, ny) sx (cx, cy) color = do
  preservingMatrix $ do
    let vo = Vertex3 0 0 0
    let v = Vertex3 (cx * (fi nx)) (cy * (fi ny)) 0
    let ve = v -: vo
    translate ve

    preservingMatrix $ do
      let w = 0.1
      let h = 0.1
      let x = 0.1 / 2
      let y = 0.1 / 2
      let vf = Vertex3 x y 0.0 :: (Vertex3 GLfloat)
      let ov = Vertex3 0.0 0.0 0.0
      let vv = vf -: ov
      translate vv
      -- drawRectGridColor color [0] [0] 0.02
      drawRectFill2d color (0.1, 0.1)

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
    let x1 = maxY_ r; y1 = minY_ r
    let vx0 = v (x0, y0, 0)
    let vx1 = v (x1, y1, 0)
    let width = (x1 - x0) / fi (xCount_ r)
    let height = (y0 - y1) / fi (yCount_ r)
    let vo = Vertex3 0 0 0
    let vf = Vertex3 (fi nx * width) (fi ny * height) 0
    let vm = vf -: vo
    translate vm
    preservingMatrix $ do
      let x = width / 2
      let y = height / 2
      let vf = Vertex3 x y 0 :: (Vertex3 GLfloat)
      let ov = Vertex3 0 0 0 :: (Vertex3 GLfloat)
      let vv = vf -: ov
      translate vv
      drawRectFill2d color (width, height)

-- |
--        x
--        |
--        |- - +  (0.1 0.1)
--        |    |
--        + - - - - >
centerBlock :: Color3 GLdouble -> IO ()
centerBlock color = do
  preservingMatrix $ do
    let w = 0.1
    let h = 0.1
    let x = 0.1 / 2
    let y = 0.1 / 2
    let vf = Vertex3 x (- y) 0.0 :: (Vertex3 GLfloat)
    let ov = Vertex3 0.0 0.0 0.0
    let vv = vf -: ov
    translate vv
    -- drawRectGridColor color [0] [0] 0.02
    drawRectFill2d color (0.1, 0.1)

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
    let x1 = maxY_ r; y1 = minY_ r
    let width = (x1 - x0) / fi (xCount_ r)
    let height = (y0 - y1) / fi (yCount_ r)
    let x0 = let w = width / 2 in w + mx' * width
    let y0 = let h = height / 2 in h + my' * height
    let vo = Vertex3 0 0 0
    let vf = Vertex3 x0 y0 0
    let vm = vf -: vo
    let del = 90 / fi (rotStep r)
    let deln = del * fi count1
    translate vm
    rotate deln (Vector3 0 0 1 :: Vector3 GLdouble)
    translate $ negate vm

    centerBrick <- readIORef refGlobal <&> centerBrick_
    rotN <- readIORef refGlobal <&> rotN_
    -- bk1 <- readIORef refGlobal >>= return . bk1_
    -- tet <- readIORef refGlobal >>= return . tetris1_
    tetX <- readIORef refGlobal <&> tetris1X_
    -- let bk1' = rotateN rotN bk1
    -- let bk1'X = rotateN rotN (tet ^._4)
    let bk1'X = rotateN rotN (snd tetX)

    -- let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
    -- let ls = map fst lz
    -- let ls = getShape centerBrick bk1'
    let lsX = getShape centerBrick bk1'X
    
    bmX <- readIORef refGlobal <&> boardMap1_

    mapM_
      ( \(a, b) -> do
          preservingMatrix $ do
            -- centerBlock00 (a + mx, b + my) initRectGrid (tet ^._3)
            centerBlock00 (a + mx, b + my) initRectGrid ((color_ . fst) tetX)
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

moveSqure :: IO ()
moveSqure = do
  preservingMatrix $ do
    let vf = Vertex3 (-1.0) 1.0 0.0 :: (Vertex3 GLfloat)
    let vv = Vertex3 0.0 0.0 0.0 -: vf
    translate vv
    let nr = initRectGrid {xCount_ = 1, yCount_ = 1, xEdge_ = 0.02, yEdge_ = 0.02}
    -- drawRectGrid [0] [0] 0.02
    drawRectGridX nr

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
      ) ls
  pp "ok"

currBrickX :: IORef GlobalRef -> RectGrid -> IO ()
currBrickX refGlobal rr = do
  preservingMatrix $ do
    mx <- readIORef refGlobal <&> moveX_
    my <- readIORef refGlobal <&> moveY_
    centerBrick <- readIORef refGlobal <&> centerBrick_
    rotN <- readIORef refGlobal <&> rotN_
    bk1 <- readIORef refGlobal <&> bk1_
    tet <- readIORef refGlobal <&> tetris1_
    tetX <- readIORef refGlobal <&> tetris1X_
    let bk1' = rotateN rotN bk1
    -- let bk1'X = rotateN rotN (tet ^._4)
    let bk1'X = rotateN rotN (snd tetX)
    
    -- let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
    -- let ls = map fst lz
    let ls = getShape centerBrick bk1'
    let lsX = getShape centerBrick bk1'X
    
    mapM_
      ( \(a, b) -> do
          preservingMatrix $ do
            centerBlock00 (a + mx, b + my) rr ((color_ . fst) tetX)
      )
      lsX
    pp "ok"
  



{-|
   @
   nx = 10
   ny = 10
   -10 <= x0 < 10
   -10 <= y0 < 10
   @
-}
checkMoveX :: [(Int, Int)] -> DM.Map (Int, Int) (Int, Int, Color3 GLdouble) -> RectGrid -> Bool
checkMoveX [] _ _ = False
checkMoveX cx sm rr =
      all 
        ( \x ->
            let x0 = fst x
                y0 = snd x
            in -nx <= x0 && x0 < nx && - ny <= y0 && y0 < ny  &&  not (x `DM.member` sm)
        )
        cx
  where
    nx = div (xCount_ rr) 2
    ny = div (yCount_ rr) 2
  
checkMoveArr :: [(Int, Int, Int)] -> [((Int, Int, Int) , BlockAttr)] -> RectGrid -> Bool
checkMoveArr cx bls rr = isInside && not anyBlock
  where
    sm = DM.fromList $ filter (\(_, b) -> isFilled_ b) bls
    anyBlock = any (`DM.member` sm) cx
    isInside = all (\(x, y, z) -> (-nx <= x && x < nx) && (-ny <= y && y < ny)) cx
    nx = div (xCount_ rr) 2
    ny = div (yCount_ rr) 2

getBottom :: (Int, Int, Int) ->[((Int, Int, Int), BlockAttr)] -> [((Int, Int, Int), BlockAttr)]
getBottom (x0, y0, z0) = filter (\((x, y, z), _) -> y == y0)

initBlockAttr :: BlockAttr
initBlockAttr = BlockAttr {
   isFilled_ = False
  ,typeId_ = -1
  ,tetrisNum_ = -1
  ,color_ = red
                          }

  
data AnimaState = AnimaState {
  animaTime_ :: Int,
  animaIndex_ :: Int,
  animaInterval_ :: Int
                             } deriving (Show, Eq)

initAnimaState :: IO(IOArray Int AnimaState)
initAnimaState = do
  currTime <- timeNowMilli <&> fi
  let an = AnimaState{animaTime_ = currTime, animaIndex_ = 0, animaInterval_ = 1000}
  let anx = AnimaState{animaTime_ = currTime, animaIndex_ = 0, animaInterval_ = 4000}
  -- DAO.newArray (0, 5) an
  let ls = [an, anx, an, an, an, an]
  DAO.newListArray (0, 5) ls

readAnimaState :: IOArray Int AnimaState -> Int -> IO (Bool, AnimaState)
readAnimaState arr ix = do
  currTime <- timeNowMilli <&> fi
  an <- DAO.readArray arr ix
  oldTime <- DAO.readArray arr ix <&> animaTime_
  interval <- DAO.readArray arr ix <&> animaInterval_
  oldIndex <- DAO.readArray arr ix <&> animaIndex_
  let newIndex = oldIndex + 1
  let isNext = currTime - oldTime >= interval
  if isNext then do
    return (isNext, an{animaTime_ = currTime, animaIndex_ = newIndex})
    else do
    return (isNext, an)

writeAnimaState :: IOArray Int AnimaState -> Int -> AnimaState -> IO()
writeAnimaState arr ix an = do
  DAO.writeArray arr ix an

flipIsNext :: IOArray Int AnimaState -> Int -> IO()
flipIsNext arr ix = do
  an <- DAO.readArray arr ix
  currTime <- timeNowMilli <&> fi
  writeAnimaState arr ix an{animaTime_ = currTime}
  
rotateTetries :: IORef GlobalRef -> RectGrid -> IOArray (Int, Int, Int) BlockAttr -> IO()
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
        bmap <- readIORef refGlobal <&> boardMap_
        bmapX <- readIORef refGlobal <&> boardMap1_
        centerBrick <- readIORef refGlobal <&> centerBrick_

        rotN <- readIORef refGlobal <&> rotN_
        bk1 <- readIORef refGlobal <&> bk1_
        tet <- readIORef refGlobal <&> tetris1_
        tetX <- readIORef refGlobal <&> tetris1X_
        ls <- getAssocs ioArray
        let bk1'X = rotateN rotN (snd tetX)

        -- /Users/aaa/myfile/bitbucket/tmp/xx_5248.x
        let currBrX = innerBrick (moveX, moveY) centerBrick bk1'X
        let currBrXX = map (\(x, y) -> (x, y, 0)) currBrX
        -- let bX = checkMoveX currBrX bmapX rr
        let bX = checkMoveArr currBrXX ls rr
        modifyIORef refGlobal \s -> s {rotN_ = bX ? (let n = rotN_ s + 1 in mod n 4) $ rotN_ s}
        rotateBlock refGlobal initRectGrid
        modifyIORef refGlobal \s -> s {count1_ = count1_ s + 1}
        pp "ok"
      | otherwise -> do
        modifyIORef refGlobal (\s -> s {count1_ = 1000000})


  

mainLoop ::
  G.Window ->
  IORef Cam ->
  IORef Step ->
  IORef GlobalRef ->
  IORef FrameCount ->
  IOArray Int AnimaState -> 
  [[Vertex3 GLfloat]] ->
  DAO.IOArray (Int, Int, Int) BlockAttr ->
  IO ()
mainLoop w refCam refStep refGlobal refGlobalFrame animaStateArr lssVex ioArray = unless' (G.windowShouldClose w) $ do
  (width, height) <- G.getFramebufferSize w
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  GL.clear [ColorBuffer, DepthBuffer]

  G.setKeyCallback w (Just $ keyBoardCallBack2 refStep refGlobal ioArray) -- AronOpenGL
  G.setMouseButtonCallback w (Just $ mouseCallback refGlobal) -- mouse event
  -- lightingInfo
  loadIdentity -- glLoadIdentity
  -- view matrix: http://xfido.com/html/indexUnderstandOpenGL.html
  -- matrixMode $= Modelview 0

  -- A matrix stack.
  --
  --data
  -- MatrixMode =
  --     Modelview GLsizei  -- ^ The modelview matrix stack of the specified vertex unit.
  --   | Projection         -- ^ The projection matrix stack.
  --   | Texture            -- ^ The texture matrix stack.
  --   | Color              -- ^ The color matrix stack.
  --   | MatrixPalette      -- ^ The matrix palette stack.
  --   deriving ( Eq, Ord, Show )

  -- projection matrix stack
  -- glMatrixModel(GL_PROJECTION)

  -- matrixMode $= Modelview 0
  --    let fovy = 80.0
  --        aspect = 1.0
  --        zNear = 2.0
  --        zFar = (-2)
  --        in GM.perspective fovy aspect zNear zFar

  -- matrixMode $= Modelview 0
  -- loadIdentity

  {--
      KeyBoard Character
      URL: https://hackage.haskell.org/package/GLFW-b-3.3.0.0/docs/Graphics-UI-GLFW.html

      @
      lookAt :: Vertex3 GLdouble -> Vertex3 GLdouble -> Vector3 GLdouble -> IO ()
      lookAt (Vertex3 eyeX    eyeY    eyeZ)
         (Vertex3 centerX centerY centerZ)
         (Vector3 upX     upY     upZ) =
      gluLookAt eyeX eyeY eyeZ centerX centerY centerZ upX upY upZx

      data XYZAxis = XYZAxis{xa::Bool, ya::Bool, za::Bool}                      deriving(Show)
      @
  --}
  -- modelview matrix stack
  -- matrixMode $= Modelview 0

  step <- readIORef refStep
  xyzAxis <- getXYZAxis refGlobal
  fovNew <- getFOVDegree refGlobal
  case xyzAxis of
    --                                 +---> YZ-plane
    --                                 ↓
    var
      | xa var -> GL.lookAt (Vertex3 1.0 0 0 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
      --                               +---> XZ-plane
      --                               ↓
      | ya var -> GL.lookAt (Vertex3 0 1.0 0 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 1 0 0 :: Vector3 GLdouble)
      --                                 +---> XY-plane
      --                                 ↓
      | za var -> do
        -- Graphics.Rendering.OpenGL.GLU.Matrix perspective :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

        -- glFrustum describes a perspective matrix that produces a perspective projection. (left,bottom,-near) and (right,top,-near) specify the points on the near clipping plane that
        -- are mapped to the lower left and upper right corners of the window, respectively, assuming that the eye is located at (0, 0, 0). -far specifies the location of the far clipping
        -- plane. Both near and far must be positive. The corresponding matrix is

        let zf = 0.5
        -- perspective (field of view) width/height zNear zFar
        -- perspective 90 1.0 zf (zf + 4.0)
        -- perspective 126.934 1.0 zf (zf + 4.0)
        -- zoom in, zoom out
        perspective fovNew 1.0 zf (zf + 4.0)
        -- GL.lookAt (Vertex3 0 4 1.0::Vertex3 GLdouble) (Vertex3 0 4 0::Vertex3 GLdouble) (Vector3 0 1 0::Vector3 GLdouble)
        GL.lookAt (Vertex3 0 0 1.0 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
      | otherwise -> do
        GM.lookAt (Vertex3 0.2 0.2 0.2 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
        keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)

        preservingMatrix $ do
          loadIdentity
          fw "loadIdentity"
          ls <- getModelviewMatrix
          let lss = partList 4 ls
          printMat lss
          GM.lookAt (Vertex3 0 2 3 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
          fw "getModelviewMatrix"
          lr <- getModelviewMatrix
          let lt = partList 4 lr
          printMat lt

  -- AronOpenGL.hs
  -- delta refStep to modify Cam{xx, yy, zz} in degree
  -- keyboardRot => rotate around {x-axis, y-axis, y-axis} in some degrees
  -- keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)
  renderCoordinates
  -- show3dStr "1234" red 0.8

  curStr <- getStr refGlobal
  show3dStr curStr red 0.8
  logFileG ["str_=" ++ show curStr]

  when True $ do
    (index, isNext, currFrame) <- readRefFrame2 refGlobalFrame 1000
    --                                                  |
    --                                                  + -> speed, larger = slower
    movSphere <- getDrawPts refGlobal
  
    let anima0 = 0
    (isNext0, animaState) <- readAnimaState animaStateArr anima0
    -- pre movSphere

    -- logFileG ["index=" ++ show index]
    logFileG ["isNextX=" ++ show isNext0 ++ " animaIndex_=" ++ show (animaIndex_ animaState)]
    -- my <- readIORef refGlobal >>= return . moveY_
    -- KEY: falling block, drop block
    when True $ do
      when isNext0 $ do
        rr <- readIORef refGlobal <&> rectGrid_
        bmap <- readIORef refGlobal <&> boardMap_
        bmapX <- readIORef refGlobal <&> boardMap1_
        mx <- readIORef refGlobal <&> moveX_
        my <- readIORef refGlobal <&> moveY_
        centerBrick <- readIORef refGlobal <&> centerBrick_
        tet <- readIORef refGlobal <&> tetris1_
        tetX <- readIORef refGlobal <&> tetris1X_
        rotN <- readIORef refGlobal <&> rotN_
        ls <- getAssocs ioArray
  
        let lz1X = join $ (map . filter) (\(_, n) -> n > 0) $ (zipWith . zipWith) (,) centerBrick  $ rotateN rotN (snd tetX)
        -- let currBlock1 = map fst lz1 -- [(x, y)] => [(1, 2), (3, 4)]
        let currTetris = map fst lz1X -- [(x, y)] => [(1, 2), (3, 4)]
       
        -- let m = map (\(a, b) -> (a + mx, b + my - 1)) currBlock1
        let mX = map (\(a, b) -> (a + mx, b + my - 1, 0)) currTetris
        
        let lastBlock = map (\(a, b) -> ((a + mx, b + my, 0), fst tetX)) currTetris
        -- let bX = checkMoveX m bmapX rr
        let isMovable = checkMoveArr mX ls rr
        -- KEY: new block
        newBlock <- randomBlockX refGlobal
        modifyIORef
          refGlobal
          ( \s ->
              s
                { moveY_ = isMovable ? my - 1 $ 0,
                  moveX_ = isMovable ? moveX_ s $ 0,
                  tetris1X_ = isMovable ? tetris1X_ s $ newBlock
                }
          )
  
        if not isMovable then do
          -- let br = BlockAttr {isFilled_ = True, typeId_ = 2, blockNum_ = 0, color_ = yellow}
          -- mapM_ (\(t, b) -> DAO.writeArray ioArray t b) lastBlock
          mapM_ (uncurry $ DAO.writeArray ioArray) lastBlock
          
          -- KEY: remove bottom row
          let f x = isFilled_ x in removeBottomX f ioArray

          logFileG ["writeArray"]
          logFileG $ map show ls
          else pp "not write"
  
        flipIsNext animaStateArr anima0
      pp "kk"
  
    -- KEY: rotate brick, rotate block
    when True $ do
      rotateTetries refGlobal initRectGrid ioArray
      {--
      let stepN = fi $ rotStep initRectGrid
      count1 <- readIORef refGlobal <&> count1_
      case count1 of
        v
          | v < stepN - 1 -> do
            preservingMatrix $ do
              rotateBlock refGlobal initRectGrid
              modifyIORef refGlobal \s -> s {count1_ = count1_ s + 1}
          | v == stepN - 1 -> do
            let rr = initRectGrid
            moveX <- readIORef refGlobal <&> moveX_
            moveY <- readIORef refGlobal <&> moveY_
            bmap <- readIORef refGlobal <&> boardMap_
            bmapX <- readIORef refGlobal <&> boardMap1_
            centerBrick <- readIORef refGlobal <&> centerBrick_

            rotN <- readIORef refGlobal <&> rotN_
            bk1 <- readIORef refGlobal <&> bk1_
            tet <- readIORef refGlobal <&> tetris1_
            tetX <- readIORef refGlobal <&> tetris1X_
            ls <- getAssocs ioArray
            let bk1'X = rotateN rotN (snd tetX)

            -- /Users/aaa/myfile/bitbucket/tmp/xx_5248.x
            let currBrX = innerBrick (moveX, moveY) centerBrick bk1'X
            let currBrXX = map (\(x, y) -> (x, y, 0)) currBrX
            -- let bX = checkMoveX currBrX bmapX rr
            let bX = checkMoveArr currBrXX ls rr
            modifyIORef refGlobal \s -> s {rotN_ = bX ? (let n = rotN_ s + 1 in mod n 4) $ rotN_ s}
            rotateBlock refGlobal initRectGrid
            modifyIORef refGlobal \s -> s {count1_ = count1_ s + 1}
            pp "ok"
          | otherwise -> do
            modifyIORef refGlobal (\s -> s {count1_ = 1000000})
       --}
-- /Users/aaa/myfile/bitbucket/tmp/xx_5948.x

    -- show current tetris
    when True $ do
      currBrickX refGlobal initRectGrid
    -- KEY: show board, show grid, draw board
    when True $ do
      showCurrBoardArr ioArray

      -- showCurrBoardArr ioArray
  -- C-; BACKUP, insertContent /Users/aaa/myfile/bitbucket/tmp/xx_6507.x

  -- xxx
  -- draw 20 x 20 grid
  when True $ do
    -- drawRectGrid [0..19] [0..19] 0.01
    drawRectGridX initRectGrid

  -- END_triangulation

  -- G.swapBuffers w
    
  G.swapBuffers w
  
  G.pollEvents
  mainLoop w refCam refStep refGlobal refGlobalFrame animaStateArr lssVex ioArray


main = do
  argList <- getArgs
  if len argList > 0
    then do
      case head argList of
        "-h" -> helpme
        _ -> do
          print $ "Wrong option => " ++ head argList ++ ", -h => Help"
    else mymain


bk1 :: [[Int]]
bk1 =
  [ [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
  ]

{--
main = do
       when True $ do
         let ls = [(1, 2)]
         let m = DM.empty
         let m1 = DM.insert (1, 2) (Vertex3 0.1 0.1 0.1, green, True) m
         print $ checkMove ls m1 initRectGrid == False
       when True $ do
         let ls = [(1, 2), (1, 3)]
         let m = DM.empty
         let m1 = DM.insert (1, 2) (Vertex3 0.1 0.1 0.1, green, False) m
         print $ checkMove ls m1 initRectGrid == True
       when True $ do
         let ls = [(1, 2), (1, 3)]
         let m = DM.empty
         let m1 = DM.insert (1, 5) (Vertex3 0.1 0.1 0.1, green, True) m
         print $ checkMove ls m1 initRectGrid == True
       when True $ do
         let ls = [(1, 2), (1, 3)]
         let m = DM.empty
         let m1 = DM.insert (1, 5) (Vertex3 0.1 0.1 0.1, green, False) m
         print $ checkMove ls m1 initRectGrid == True
       when True $ do
         let ls = [(1, 2), (1, 3)]
         let m = DM.empty
         let m1 = DM.insert (1, 5) (Vertex3 0.1 0.1 0.1, green, False) m
         let m2 = DM.insert (1, 3) (Vertex3 0.1 0.1 0.1, green, True) m1
         print $ checkMove ls m2 initRectGrid == False
       when True $ do
         let ls = [(1, 2), (1, 3)]
         let m = DM.empty
         let m1 = DM.insert (1, 5) (Vertex3 0.1 0.1 0.1, green, False) m
         let m2 = DM.insert (1, 3) (Vertex3 0.1 0.1 0.1, green, False) m1
         print $ checkMove ls m1 initRectGrid == True
       when True $ do
         let f x = join (***) x
         let s  = map (\y -> map (\x -> (x, y)) [0..4]) $ reverse [0..4]
         let s' = map (\y -> map (\x -> (x - 2, y - 2)) [0..4]) $ reverse [0..4]

         let sk = (zipWith . zipWith) (\x y -> (x, y)) s' bk1

         let ska = (map . map) (\(x, y) -> ((***) (+1) (+2) x, y)) sk
         let skr = (zipWith . zipWith) (\x y -> (x, y)) s' $ rotateN 1 bk1
         let sks = (map . filter) (\(x, y) -> y == 1) skr
         let bk1 :: [[Int]]
             bk1 = [ [0, 0, 0, 0, 0]
                 ,[0, 0, 0, 0, 0]
                 ,[1, 1, 1, 1, 1]
                 ,[0, 0, 0, 0, 0]
                 ,[0, 0, 0, 0, 0]
                 ]

         let ss = (zipWith . zipWith) (\x y -> (x, y)) s' s
         let st = (map . map) (\x -> let n = (snd . snd) x in n == 2 ? (x, 1) $ (x, 0)) ss
         let m1 = (map . map) snd st
         let rs = rotateN 1 st
         let m2 = (map . map) snd rs
         printMat s
         fw "s'"
         printMat s'
         pp "ok"
         fw "triple"
         printMat ss
         fw "triple 1"
         printMat st
         fw "rotateN 1"
         printMat rs
         fw "m1"
         printMat m1
         fw "m2"
         printMat m2
         fw "sk"
         printMat sk
         fw "skr"
         printMat skr
         fw "ska"
         printMat ska
         fw "sks"
         printMat sks

--}
{--
main = do
       fw "mat"
       printMat mat
       fw "mat 0"
       printMat $ rotateN 0 mat
       fw "mat 1"
       printMat $ rotateN 1 mat
       fw "mat 2"
       printMat $ rotateN 2 mat
--}


-- import Data.Array.IO
-- import Control.Monad

data MyX = MyX {name00_ :: String, age00_ :: Int} deriving (Show, Eq)


{--
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

data D3 = D3{zVec_ :: Int,
             yVec_ :: Int,
             xVec_ :: Int} deriving (Show, Eq)

revIndex :: (Int, Int) -> Int -> Maybe Int
revIndex (a, b) x = DM.lookup x m
  where
    ls = [a .. b]
    rs = reverse ls
    m = DM.fromList $ zip ls rs

-- /Users/aaa/myfile/bitbucket/tmp/xx_5686.x
-- /Users/aaa/myfile/bitbucket/tmp/xx_3123.x
  
walkBlockXX :: (Int, Int, Int) -> (BlockAttr -> Bool) -> IOArray (Int, Int, Int) BlockAttr -> IO [(Int, Int, Int)]
walkBlockXX (a, b, c) f arr = do
  ls <- walkfun (a, b, c) f arr
  mapM_ (uncurry $ DAO.writeArray arr) ls
  return $ map fst ls
  where
   walkfun :: (Int, Int, Int) -> (BlockAttr -> Bool) -> IOArray (Int, Int, Int) BlockAttr -> IO [((Int, Int, Int), BlockAttr)]
   walkfun (a, b, c) f arr = do
     bound <- DAO.getBounds arr
     if DAO.inRange bound (a, b, c) then do
       x <- DAO.readArray arr (a, b, c)
       if f x then do
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
funx :: (Int, Int) -> (Int, Int) -> (Int -> Bool) -> IOArray (Int, Int, Int) Int -> IO()
funx (y0, y1) (yy0, yy1) f arr = do
  when (y0 <= y1) $ do
    let z0 = 0
    ((a0, b0, c0), (a1, b1, c1)) <- getBounds arr

    -- ls <- DAO.mapArray id arr >>= getAssocs <&> filter (\((z, y, x), e) -> z == 0 && y == y1 && (b1 e || b2 e || b3 e || b4 e))
    ls <- DAO.mapArray id arr >>= getAssocs <&> filter (\((z, y, x), e) -> z == 0 && y == y1 && f e)
    if len ls == c1 - c0 + 1 then do
      fw "Full Row"
      DAO.getAssocs arr >>= mapM_ (\((zDir, yDir, xDir), _) -> do
            let ms = revIndex (yy0, yy1) yDir
            let y' = case ms of
                         Just s -> s
                         Nothing -> error "ERROR: Invalid index"
            when (zDir == z0) $ do
              if y0 < y' && y' <= y1 then do
                s <- DAO.readArray arr  (zDir, y' - 1, xDir)
                DAO.writeArray arr      (zDir, y',     xDir) s
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

removeBottom :: (Int, Int) -> (Int, Int) -> (BlockAttr -> Bool) -> IOArray (Int, Int, Int) BlockAttr -> IO()
removeBottom (y0, y1) (yy0, yy1) f arr = do
  when (y0 <= y1) $ do
    let z0 = 0
    bt@((a0, b0, c0), (a1, b1, c1)) <- getBounds arr
    -- ls <- DAO.mapArray id arr >>= getAssocs <&> filter (\((z, y, x), e) -> z == 0 && y == y1 && (b1 e || b2 e || b3 e || b4 e))
    ls <- DAO.mapArray id arr >>= getAssocs <&> filter (\((a, b, c), e) -> c == 0 && b == y1 && f e)
    if len ls == a1 - a0 + 1 then do
      fw "Full Row"
      logFileG ["MyFullRow"]
      logFileG [show bt]
      logFileG $ map show ls
      logFileG ["EndFullRow"]
      logFileG ["myy1"]
      logFileG [show y1]
      DAO.getAssocs arr >>= mapM_ (\((zDir, yDir, xDir), _) -> do
            let ms = revIndex (yy0, yy1) yDir
            let y' = case ms of
                         Just s -> s
                         Nothing -> error "ERROR: Invalid index"
            when (xDir == z0) $ do
              logFileG ["CallMyMaybe0"]
              logFileG ["yy0 y' yy1"]
              logFileG $ map show [yy0, y', yy1]
              logFileG ["y0, y1"]
              logFileG $ map show [y0, y1]
              logFileG ["zDir yDir xDir"]
              logFileG $ map show [zDir, yDir, xDir]
              -- -10                   9
              if yy0 <= yDir && yDir < yy1 && y1 <= yDir then do
                logFileG ["CallMyMaybe1"]
                s <- DAO.readArray arr  (zDir, yDir + 1, xDir)
                DAO.writeArray arr      (zDir, yDir,     xDir) s
              else do
                when (yy1 == yDir) $ do
                  DAO.writeArray arr (zDir, yDir, xDir) initBlockAttr
        )
      -- Remove the bottom row
      -- Display all the new position of blocks
      -- showCurrBoardArr arr
      -- threadDelay 2000000
      -- XXX here
      fallBlock arr
      -- showCurrBoardArr arr
      -- threadDelay 200000
      removeBottom (yy0, yy1) (yy0, yy1) f arr
      else do
      removeBottom (y0, y1 - 1) (yy0, yy1) f arr
      -- pp "Not equal to len"
    -- fw "funx print arr"
    -- printMat3 arr


fallBlock :: IOArray (Int, Int, Int) BlockAttr -> IO()
fallBlock arr = do
  let rr = initRectGrid
  DAO.getAssocs arr >>= mapM_ (\((z, y, x), ax) -> do
                                -- ax <- DAO.readArray arr (z, y, x)
                                if isFilled_ ax then do
                                  lsArr <- getAssocs arr
                                  let f x = isFilled_ x && isFilled_ ax && tetrisNum_ x == tetrisNum_ ax
                                  lt <- walkBlockXX (z, y, x) f arr
                                  let lsArr' = filter (\(x,_) -> x `notElem` lt) lsArr
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
                                    logFileG ["endmovedown"]
                                    -- showCurrBoardArr arr
                                    -- threadDelay 2000000
                                  else do
                                  pp "ee"
                                pp "ok"
                               )

removeBottomX :: (BlockAttr -> Bool) -> IOArray (Int, Int, Int) BlockAttr -> IO()
removeBottomX f arr = do
  ((z0, y0, x0), (z1, y1, x1)) <- DAO.getBounds arr
  removeBottom (y0, y1) (y0, y1) f arr

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
