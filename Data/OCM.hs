module OCM where

import Generics.Pointless.Combinators

data Form a = Fm a | FmNull
          deriving (Eq, Show)
instance Functor Form where
   fmap f (Fm x) = Fm (f x)
   fmap _ FmNull  = FmNull

data Content a = Ct a | CtNull
               deriving (Eq, Show)
instance Functor Content where
   fmap f (Ct x) = Ct (f x)
   fmap _ CtNull  = CtNull

type FMat = [(String, Int)]
type Construct = (Form String, Content FMat)
type Construction = Construct -> Construct

type FormOp = Form String -> Form String
type ContentOp = Content FMat -> Content FMat

construction :: FormOp -> ContentOp -> Construction
construction = (><)

validCx :: (Form a, Content b) -> Bool
validCx (FmNull, _) = False
validCx (_, CtNull) = False
validCx _ = True

addPrefix :: String -> FormOp
addPrefix prefix = fmap (prefix ++)

addSuffix :: String -> FormOp
addSuffix suffix = fmap (++suffix)

setFeature :: String -> Int -> ContentOp
setFeature feature value = fmap $ ((feature, value):) . filter ((/=feature) . fst)

subcatForm :: (a -> Bool) -> Form a -> Form a
subcatForm _ FmNull = FmNull
subcatForm p (Fm x) = if p x then (Fm x) else FmNull

cxNull = (FmNull, CtNull)

root :: String -> FMat -> Construction
root fm ct (FmNull, CtNull) = (Fm fm, Ct ct)

cxWalk :: Construction
cxWalk = root "walk" []

cxPastEd :: Construction
cxPastEd = addSuffix "ed" . subcatForm ((`elem` "bdg") . last)
           >< 
           setFeature "Past" 1 . setFeature "Fut" 0