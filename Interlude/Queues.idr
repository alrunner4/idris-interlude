module Interlude.Queues
import Control.Monad.STT
import Data.IOArray
import Data.IORef

%hide Prelude.Stream.(::)


----------------
-- Interfaces --

public export
interface FromList (t: Type -> Type) where
   fromList: forall a. List a -> t a

public export
interface IStack (m: Type -> Type) where
   Stack: Type -> Type
   (.push): Stack a -> a -> m ()
   (.pop): Stack a -> m (Maybe a)

public export
interface IQueue (m: Type -> Type) where
   Queue: Type -> Type
   (.enqueue): Queue a -> a -> m ()
   (.dequeue): Queue a -> m (Maybe a)

public export
interface IDeque (m: Type -> Type) where
   Deque: Type -> Type
   (.enqueue_back):  Deque a -> a -> m ()
   (.enqueue_front): Deque a -> a -> m ()
   (.dequeue_back):  Deque a ->      m (Maybe a)
   (.dequeue_front): Deque a ->      m (Maybe a)

namespace IORef
   prefix 9 *:
   infixr 3 =:
   public export (*:): IORef a ->             IO a
   public export (=:): IORef a ->       a  -> IO ()
   public export (=$): IORef a -> (a -> a) -> IO ()
   (*:) = readIORef
   (=:) = writeIORef
   (=$) = modifyIORef

namespace STT
   prefix 9 *:
   infixr 3 =:
   infixr 3 =$
   public export (*:): MonadST m s => VarRef {m} {s} a ->             m s a
   public export (=:): MonadST m s => VarRef {m} {s} a ->       a  -> m s ()
   public export (=$): MonadST m s => VarRef {m} {s} a -> (a -> a) -> m s ()
   (*:)  = readSTTRef
   (=:) = writeSTTRef
   (=$) ref f = readSTTRef ref >>= writeSTTRef ref . f

assert: Monad m => m Bool -> m ()
assert p = if !p then pure () else assert_total (idris_crash "assertion failed")

----------------
-- ArrayDeque --

export
record ArrayDeque (a: Type) where
   constructor MkArrayDeque
   array: IORef (IOArray a)
   back:  IORef Int
   front: IORef Int

export
mkArrayDeque: IO (ArrayDeque a)
mkArrayDeque = MkArrayDeque
   <$> newIORef !(newArray 10)
   <*> newIORef 0
   <*> newIORef 0

export
empty: ArrayDeque a -> IO Bool
empty q = pure$ !(*:q.back) == 0

export
full: ArrayDeque a -> IO Bool
full q = pure$ !(*:q.back) == max !(*:q.array)

namespace ArrayDeque
   private
   grow: ArrayDeque a -> IO ()
   grow current_deque = do
      current_array <- *:current_deque.array
      current_front <- *:current_deque.front
      grown_array <- newArray (max current_array * 2)
      let grown_deque = MkArrayDeque !(newIORef grown_array) !(newIORef 0) !(newIORef 0)
      -- TODO: need better copyFrom for circular buffer
      copyFrom (current_front, current_array) (0, grown_array)
      copyFrom (0, current_array) (max current_array - current_front, grown_array)
      current_deque.array =: grown_array
    where
      copyFrom: (Int, IOArray a) -> (Int, IOArray a) -> IO ()
      copyFrom (from_index, from_array) (to_index, to_array) = do
         if any (\(i,a) => i >= max a) [(from_index, from_array), (to_index, to_array)]
            then pure ()
            else do
               readArray from_array from_index >>= \case
                  Just val => ignore$ writeArray to_array to_index val
                  Nothing  => assert_total$ idris_crash "impossible"
               assert_total$ copyFrom (from_index+1, from_array) (to_index+1, to_array)

public export
[IDequeArray] IDeque IO where

   Deque = ArrayDeque

   (.enqueue_back) q x = do
      when !(full q) (grow q)
      arr <- *:q.array
      let size = max arr
      let new_back = (!(*:q.back)+1) `mod` size
      assert$ writeArray arr new_back x
      q.back =: new_back

   (.enqueue_front) = ?ad_ef

   (.dequeue_back) = ?ad_eb --do
      --arr <- *:q.array
      --new_back <- *:q.back <&> (-1)

   (.dequeue_front) = ?ad_df

export
[IQueueArray] IQueue IO where
   Queue = ArrayDeque
   (.enqueue) = (.enqueue_back) @{IDequeArray}
   (.dequeue) = (.dequeue_back) @{IDequeArray}

export
[IStackArray] IStack IO where
   Stack = ArrayDeque
   (.push) = (.enqueue_back) @{IDequeArray}
   (.pop)  = (.dequeue_back) @{IDequeArray}

-----------------
-- LinkedDeque --

export
record LinkedDeque
   (m: (0 _: STThread) -> Type -> Type)
   (0 s: STThread)
   {auto st: MonadST m s}
   (a: Type)
 where
   constructor MkLinkedDeque
   back:  VarRef (List a) @{st}
   front: VarRef (List a) @{st}

namespace LinkedDeque

   export
   empty: MonadST m s => LinkedDeque m s _ -> m s Bool
   empty q = pure$ null !(*:q.back) && null !(*:q.front)

   export
   dequeue_back: MonadST m s => LinkedDeque m s a -> m s (Maybe a)
   dequeue_back {a} q = try_back !(*:q.back) where
      try_back, try_front: List a -> m s (Maybe a)
      try_back = \case
         (x :: xs) => do
            q.back =: xs
            pure (Just x)
         [] => try_front (reverse !(*:q.front))
      try_front = \case
         [] => pure Nothing
         (x :: xs) => do
            q.back =: xs
            pure (Just x)

   export
   dequeue_front: MonadST m s => LinkedDeque m s a -> m s (Maybe a)
   dequeue_front {a} q = try_front !(*:q.front) where
      try_front, try_back: List a -> m s (Maybe a)
      try_front = \case
         (x :: xs) => do
            q.front =: xs
            pure (Just x)
         [] => try_back (reverse !(*:q.back))
      try_back = \case
         [] => pure Nothing
         (x :: xs) => do
            q.front =: xs
            pure (Just x)

   export
   enqueue_back: MonadST m s => LinkedDeque m s a -> a -> m s ()
   enqueue_back q x = q.back =$ (x ::)

   export
   enqueue_front: MonadST m s => LinkedDeque m s a -> a -> m s ()
   enqueue_front q x = q.front =$ (x ::)

   export
   {m: _} -> Monad m => FromList (STT m s . LinkedDeque (STT m) s) where
      fromList l = MkLinkedDeque <$> newSTTRef Nil <*> newSTTRef l

   export
   {m: _} -> MonadST m s => FromList (\a => m s (LinkedDeque m s a)) where
      fromList l = MkLinkedDeque <$> newSTTRef Nil <*> newSTTRef l


export
mkLinkedDeque: Monad m => MonadST (STT m) s => STT m s (LinkedDeque (STT m) s a)
mkLinkedDeque = MkLinkedDeque <$> newSTTRef Nil <*> newSTTRef Nil

[ListDeque] {m: _} -> {0 s: _} -> (st: MonadST m s) => IDeque (m s) where
   Deque = LinkedDeque m s
   (.enqueue_back)  = enqueue_back
   (.enqueue_front) = enqueue_front
   (.dequeue_back)  = dequeue_back
   (.dequeue_front) = dequeue_front

export
[ListQueue] {m: _} -> {0 s: _} -> (st: MonadST m s) => IQueue (m s) where
   Queue = LinkedDeque m s
   (.enqueue) = enqueue_back
   (.dequeue) = dequeue_front


---------------------------------
-- CircularArrayFixedSizeQueue --

