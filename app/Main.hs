{-# LANGUAGE OverloadedStrings #-}

import SDL
import SDL.Image (load)
import Linear (V2(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Control.Monad (unless, void)
import Data.Bits (testBit)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.Types (CInt)
import Foreign.Ptr (nullPtr)
import System.Environment (getArgs)
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

main :: IO ()
main = do
    args <- getArgs
    case args of
        [n, m] -> do
            let rows = read n :: CInt
            let cols = read m :: CInt
            let cellSize' = 50  -- Tamanho dos quadrados
            putStrLn $ "Número de linhas: " ++ show rows
            putStrLn $ "Número de colunas: " ++ show cols
            putStrLn $ "Tamanho da célula: " ++ show cellSize'
            initializeAll
            initialize [InitVideo]
            let windowSize = V2 (cellSize' * cols) (cellSize' * rows)
            putStrLn $ "Tamanho da janela: " ++ show windowSize
            window <- createWindow "Campo Minado" defaultWindow
                { windowInitialSize = windowSize }
            renderer <- createRenderer window (-1) defaultRenderer

            -- Carregar a imagem
            texture <- loadTexture renderer "resources/bomb.png"

            appLoop renderer (rows, cols) cellSize' texture

            destroyTexture texture
            destroyRenderer renderer
            destroyWindow window
            quit
        _ -> putStrLn "Uso: cabal exec main -- <número de linhas> <número de colunas>"

appLoop :: Renderer -> (CInt, CInt) -> CInt -> Texture -> IO ()
appLoop renderer (rows, cols) cellSize' texture = do
    events <- pollEvents
    let quit = any eventIsQuit events

    -- Desenhar tabuleiro
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    drawBoard renderer (rows, cols) cellSize' texture
    present renderer

    -- Verificar cliques do mouse
    handleMouseClick renderer (rows, cols) cellSize' events

    unless quit (appLoop renderer (rows, cols) cellSize' texture)

-- Função para obter a localização do mouse retirada de https://hackage.haskell.org/package/sdl2-2.0.0/docs/src/SDL-Input-Mouse.html#getMouseLocation
getMouseLocation :: MonadIO m => m (Point V2 CInt)
getMouseLocation = liftIO $
  alloca $ \x ->
  alloca $ \y -> do
    _ <- Raw.getMouseState x y -- We don't deal with button states here
    P <$> (V2 <$> peek x <*> peek y)

-- Função para verificar o estado dos botões do mouse retirada de https://hackage.haskell.org/package/sdl2-2.0.0/docs/src/SDL-Input-Mouse.html#getMouseLocation
getMouseButtons :: MonadIO m => m (MouseButton -> Bool)
getMouseButtons = liftIO $
  convert <$> Raw.getMouseState nullPtr nullPtr
  where
    convert w b = w `testBit` index
      where
      index = case b of
                ButtonLeft    -> 0
                ButtonMiddle  -> 1
                ButtonRight   -> 2
                ButtonX1      -> 3
                ButtonX2      -> 4
                ButtonExtra i -> i

-- Atualização da função handleMouseClick
handleMouseClick :: Renderer -> (CInt, CInt) -> CInt -> [Event] -> IO ()
handleMouseClick renderer (rows, cols) cellSize' events = do
    let mouseClickEvents = filter isMouseButtonDown events
    mapM_ (processMouseClick renderer (rows, cols) cellSize') mouseClickEvents

isMouseButtonDown :: Event -> Bool
isMouseButtonDown event =
    case eventPayload event of
        KeyboardEvent _ -> False
        MouseButtonEvent mouseEvent -> mouseButtonEventMotion mouseEvent == Pressed
        _ -> False

-- Função atualizada para processar o clique do mouse e identificar o botão pressionado
processMouseClick :: Renderer -> (CInt, CInt) -> CInt -> Event -> IO ()
processMouseClick renderer (rows, cols) cellSize' event = do
    mousePos <- getMouseLocation
    let (P (V2 x y)) = mousePos
    let col = x `div` cellSize'
    let row = y `div` cellSize'
    
    let mouseButton = case eventPayload event of
                        MouseButtonEvent mbe -> mouseButtonEventButton mbe
                        _ -> ButtonLeft  -- Valor padrão para garantir que algo seja impresso

    putStrLn $ "Célula clicada: Linha " ++ show row ++ ", Coluna " ++ show col
    putStrLn $ "Botão pressionado: " ++ show mouseButton

drawBoard :: Renderer -> (CInt, CInt) -> CInt -> Texture -> IO ()
drawBoard renderer (rows, cols) cellSize' texture = do
    rendererDrawColor renderer $= V4 0 0 0 255
    mapM_ (drawLineCustom renderer) [((i * cellSize', 0), (i * cellSize', rows * cellSize')) | i <- [1 .. cols-1]]
    mapM_ (drawLineCustom renderer) [((0, i * cellSize'), (cols * cellSize', i * cellSize')) | i <- [1 .. rows-1]]

    -- Desenhar a imagem na coordenada (1,1)
    let x = 1 * cellSize'  -- Ajuste conforme necessário
    let y = 1 * cellSize'  -- Ajuste conforme necessário
    let destRect = Rectangle (P (V2 x y)) (V2 cellSize' cellSize')
    copy renderer texture Nothing (Just destRect)

drawLineCustom :: Renderer -> ((CInt, CInt), (CInt, CInt)) -> IO ()
drawLineCustom renderer ((x1, y1), (x2, y2)) =
    drawLine renderer (P (V2 x1 y1)) (P (V2 x2 y2))

eventIsQuit :: Event -> Bool
eventIsQuit event =
    case eventPayload event of
        KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
        WindowClosedEvent _ -> True
        _ -> False

loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture renderer filePath = do
    surface <- SDL.Image.load filePath
    texture <- createTextureFromSurface renderer surface
    freeSurface surface
    return texture
