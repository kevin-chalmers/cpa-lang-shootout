import Data.Char
import Control.Concurrent
import Control.Concurrent.Chan

main :: IO ()
main = do
        a <- newChan :: IO (Chan Int)
        b <- newChan :: IO (Chan Int)
        c <- newChan :: IO (Chan Int)
        d <- newChan :: IO (Chan Int)
        forkIO (prefix 0 a b)
        forkIO (delta b c d)
        forkIO (succ c a)
        printer d
        where
            prefix n input output = do
                writeChan output (n)
                id input output
                where
                    id input output = forever $ do
                        n <- readChan input
                        writeChan output n
            delta input out0 out1 = forever $ do
                n <- readChan input
                writeChan out0 n
                writeChan out1 n
            succ input output = forever $ do
                n <- readChan input
                writeChan output (n + 1)
            printer input = forever $ do
                n <- readChan input
                print n

forever a = a >> forever a