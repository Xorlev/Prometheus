module Main
        ( main
        ) where

import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch)
import UrlExtender

server = "irc.darklordpotter.net"
port   = 6667
channel = "#programming"
nick   = "HaskellBot"

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where
        disconnect = hClose . socket
        loop st    = catch (runReaderT run st) (\(SomeException _) -> return ())
 

connect = notify $ do
        h <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering h NoBuffering
        return (Bot h)
  where
        notify a = bracket_
                (printf "Connecting to %s ... " server >> hFlush stdout)
                (putStrLn "done.")
                a

run :: Net ()
run = do
        write "NICK" nick
        write "USER" (nick++" 0 * :raven's bot")
        write "JOIN" channel
        asks socket >>= listen

write :: String -> String -> Net ()
write s t = do
        h <- asks socket
        io $ hPrintf h "%s %s\r\n" s t
        io $ printf "> %s %s\r\n" s t

listen :: Handle -> Net ()
listen h = forever $ do
                s <- init `fmap` io (hGetLine h)
                io $ putStrLn s
                if ping s then pong s else eval (clean s)
        where
                forever a = a >> forever a
                ping x = "PING :" `isPrefixOf` x
                pong x = write "PONG" (':' : drop 6 x)
                clean = drop 1 . dropWhile (/= ':') . drop 1

eval :: String -> Net ()
eval            "!quit"                         = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x          | "!id " `isPrefixOf` x         = privmsg (drop 4 x)
eval x          | "!ex" `isPrefixOf` x          = (io $ expandUrlEasy (drop 4 x)) >>= privmsg 
eval _                                          = return () -- ignore

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" $ channel ++ " :" ++ s

io :: IO a -> Net a
io = liftIO