module Main

import FS.Chunk as C
import FS.Posix
import IO.Async.Loop.Posix
import System

%default total

covering
runProg :  AsyncStream Poll [Errno] Void
        -> IO ()
runProg prog =
  simpleApp $
    mpull ( handle [stderrLn . interpolate] prog
          )

toCelsius :  ByteString
          -> Double
toCelsius bs =
  case parseDouble bs of
    Nothing => 0.0
    Just f  => (f - 32.0) * (5.0/9.0)

fahrenheit :  ChunkSize
           => String
           -> AsyncStream Poll [Errno] Void
fahrenheit fp =
     readBytes fp
  |> lines
  |> C.mapOutput (fromString . show . toCelsius)
  |> unlines
  |> writeTo Stdout

prog : List String -> AsyncStream Poll [Errno] Void
prog []         =
  throw EINVAL
prog (_::size) =
  case size of
    ["small"]       =>
      fahrenheit @{4096} "fahrenheit/resources/fahrenheit_small.txt"
    ["medium"]      =>
      fahrenheit @{4096} "fahrenheit/resources/fahrenheit_medium.txt"
    ["large"]       =>
      fahrenheit @{4096} "fahrenheit/resources/fahrenheit_large.txt"
    ["extra_large"] =>
      fahrenheit @{4096} "fahrenheit/resources/fahrenheit_extra_large.txt"
    _               =>
      stderrLn "Usage: pack run idris2-streams-fahrenheit.ipkg [small|medium|large|extra_large]"

covering
main : IO ()
main = getArgs >>= runProg . prog
