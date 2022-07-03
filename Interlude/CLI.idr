module Interlude.CLI
import Interlude.Monad {- until -}

import Control.Monad.Either {- runEitherT -}
import Control.Monad.Trans {- lift -}
import Control.Monad.Writer {- execWriterT -}
import Data.String {- fastConcat -}
import Data.Vect {- exactLength, fromList -}
import System {- getArgs -}
import System.File {- fEOF, fGetLine, fPutStr, stderr -}

export total
putError : List String -> IO ()
putError es = do
   ignore$ fPutStr stderr (fastConcat es)
   ignore$ fPutStr stderr "\n"

export total
putProcError : String -> List String -> IO ()
putProcError procname error_parts = putError( procname :: ": error: " :: error_parts )

export partial
interpretLines: Monoid a => HasIO m =>
   File -> (String -> m a) -> m (Either FileError a)
interpretLines file interpret = runEitherT $execWriterT
   $until( fEOF file ) $do
      line <- liftEither !(fGetLine file)
      tell !(lift $lift $interpret line)

||| Definitions to simplify creation of program entry points.
namespace Main

    public export
    ExactArgsMain: Nat -> Type
    ExactArgsMain n_args = Vect n_args String -> (putError: List String -> IO ()) -> IO ()

    export total
    exactArgs: {argc: Nat} -> String -> ExactArgsMain argc -> IO ()
    exactArgs usage main = do

        procname :: args <- getArgs
            | Nil => putError[ "bad command line parameter list (empty)" ]

        let putError = putProcError procname

        case exactLength argc (fromList args) of
            Nothing   => putError[ "usage: ", procname, usage ]
            Just args => main args putError
