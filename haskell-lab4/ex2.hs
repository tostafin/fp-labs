-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y


data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}


xCoord'' :: Cart2DVec'' a -> a
xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

yCoord'' :: Cart2DVec'' a -> a
yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y


-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"


-- 1.

data Cart3DVec a = Cart3DVec a a a

xCoord''' :: Cart3DVec a -> a
xCoord''' (Cart3DVec x _ _) = x

yCoord''' :: Cart3DVec a -> a
yCoord''' (Cart3DVec _ y _) = y

zCoord''' :: Cart3DVec a -> a
zCoord''' (Cart3DVec _ _ z) = z


-- 2.

data Cart3DVec' a = Cart3DVec' {x'::a, y'::a, z'::a}

--xCoord'''' :: Cart3DVec' a -> a
--xCoord'''' (Cart3DVec' {x' = xVal, y' = _, z' = _}) = xVal

--yCoord'''' :: Cart3DVec' a -> a
--yCoord'''' (Cart3DVec' {x' = _, y' = yVal, z' = _}) = yVal

--zCoord'''' :: Cart3DVec' a -> a
--zCoord'''' (Cart3DVec' {x' = _, y' = _, z' = zVal}) = zVal


-- 6.

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b


-- 7.

data TrafficLights = Green' |
                     Yellow' |
                     Red'

actionFor :: TrafficLights -> String
actionFor Green' = "GO!"
actionFor Yellow' = "Either get ready to go, or stop, because the red light is coming!"
actionFor Red' = "STOP!"



