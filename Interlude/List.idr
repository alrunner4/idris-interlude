module Interlude.List
import Data.List {- length, splitAt -}
import Data.List1
import Data.Vect


namespace ListLike

   public export
   interface Cons (t: Type -> Type) where
      cons: a -> t a -> t a
   public export
   interface Uncons (t: Type -> Type) where
      uncons: t a -> Maybe (a, t a)

   public export
   reverseOnto: {from, to: Type -> Type} -> Cons to => Uncons from => to a -> from a -> to a
   reverseOnto xs ys = case uncons ys of
      Nothing     => xs
      Just (y,ys) => reverseOnto( cons y xs ) ys

   %inline public export
   Cons List where cons = (::)
   public export
   Uncons List where uncons = \case
      Nil   => Nothing
      x::xs => Just( x, xs )
   public export
   Cons List1 where cons x (y ::: ys) = x ::: (y :: ys)
   %inline public export
   Cons Stream where cons x xs = x :: xs
   %inline public export
   Uncons Stream where uncons (x :: xs) = Just (x, xs)
   %inline public export
   Cons SnocList where cons x xs = xs :< x
   public export
   Uncons SnocList where uncons = \case
      Lin     => Nothing
      xs :< x => Just( x, xs )


public export
reverseOnto: List a -> Vect n a -> List a
reverseOnto xs Nil = xs
reverseOnto xs (y::ys) = reverseOnto (y::xs) ys

export
data Zipper: {a: Type} -> List a -> Type where
   ||| The `Locus` constructor preserves proof that `pos` many elements have been reversed from
   ||| `List` `xs` onto `Vect` `previous`
    Locus: (pos: Nat) -> (previous: Vect pos a) -> (xs: List a) ->
        Zipper( reverseOnto xs previous )

export
position: Zipper{a} l -> Nat
position( Locus pos _ _ ) = pos

||| Create a "fully-zipped" `Zipper`.
export
headZipper: (z: List a) -> Zipper z
headZipper xs = Locus Z Nil xs

export
peek: Zipper{a} z -> Maybe a
peek( Locus _ _ Nil ) = Nothing
peek( Locus _ _ (x::_) ) = Just x

||| The key operation of a `Zipper`
export
unzip: Zipper{a} z -> Maybe( a, Zipper z )
unzip( Locus _ _ Nil ) = Nothing
unzip( Locus pos previous (x::xs) ) = Just( x, Locus( S pos )( x :: previous ) xs )

export
zip: Zipper z -> Maybe( Zipper z )
zip( Locus Z _ _ ) = Nothing
zip( Locus( S n )( x :: previous ) xs ) = Just( Locus n previous (x::xs) )

||| Finds all non-overlapping occurrences of the `target` `List` within the `reference` `List`
export
find: Eq a => List a -> (reference: List a) -> List( Zipper reference )
find target reference =
   let zipperStart = headZipper reference
   in matching zipperStart zipperStart target
   where
      matching: Zipper reference -> Zipper reference -> List a -> List( Zipper reference )
      -- a match concludes
      matching current_focus match_focus Nil =
         match_focus :: matching current_focus current_focus target
      -- unmatched elements remain
      matching current_focus match_focus ( subH :: subT ) = case unzip current_focus of
         Nothing => Nil                              -- end of input is reached
         Just( seqH, new_focus ) => if subH == seqH
            then matching new_focus match_focus subT -- a match starts or continues
            else matching new_focus new_focus target -- the current_focus doesn't match

