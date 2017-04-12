{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module TypeLevel.Bits where

data Bit = O | I deriving (Eq, Ord, Show, Read)

type family Sum (cin :: Bit) (x :: Bit) (y :: Bit) :: Bit where
    Sum O O O = O
    Sum O O I = I
    Sum O I O = I
    Sum O I I = O
    Sum I O O = I
    Sum I O I = O
    Sum I I O = O
    Sum I I I = I

type family CarryOut (cin :: Bit) (x :: Bit) (y :: Bit) :: Bit where
    CarryOut O O O = O
    CarryOut O O I = O
    CarryOut O I O = O
    CarryOut O I I = I
    CarryOut I O O = O
    CarryOut I O I = I
    CarryOut I I O = I
    CarryOut I I I = I

type family AddWithCarry (cin :: Bit) (xs :: [Bit]) (ys :: [Bit]) :: [Bit] where
    AddWithCarry c (x ': xs) (y : ys) = Sum c x y : AddWithCarry (CarryOut c x y) xs ys

    AddWithCarry O '[] ys = ys
    AddWithCarry O xs '[] = xs
