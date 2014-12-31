{-# OPTIONS_GHC -Wall -Werror #-}

class MyPrettyPrint a where
        prettyPrint :: a -> String

data TrafficLight = Red | Yellow | Green deriving (Eq, Show, Ord, Bounded, Read, Enum)

instance MyPrettyPrint TrafficLight where 
    prettyPrint = lightPrinter'

lightPrinter :: TrafficLight -> String
lightPrinter Red = "red"
lightPrinter Yellow = "yellow"
lightPrinter _ = "green"

lightPrinter' :: TrafficLight -> String
lightPrinter' x = case x of Red      -> "rrred"
                            Yellow   -> "yyyeeelllow"
                            Green    -> "gggreeen"

data OS = Android Int | IOS Int
