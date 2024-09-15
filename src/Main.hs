import Control.Monad.State
import Game

main :: IO ()
main = evalStateT playShogi createGame