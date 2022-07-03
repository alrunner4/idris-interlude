module Interlude.Monad
import Data.List1

public export partial
while: Monad f => f Bool -> Lazy( f () ) -> f ()
while cond op = if !cond then op *> while cond op else pure ()

public export partial
until: Monad f => f Bool -> Lazy( f () ) -> f ()
until cond op = if !cond then pure () else op *> until cond op

public export total
foldl1M: Monad m => (a -> a -> m a) -> List1 a -> m a
foldl1M f (x ::: []) = pure x
foldl1M f (x ::: (y::ys)) =
   foldl1M f (assert_smaller (x ::: (y :: ys)) (y ::: ys)) >>= f x
