module Lib.Types where


data RobotPart = RobotPart
  { name :: String
  , description :: String
  , cost :: Double
  , count :: Int } deriving (Show)

instance Eq RobotPart where
  (==) (RobotPart _ _ c1 _) (RobotPart _ _ c2 _) = c1 == c2

instance Ord RobotPart where
  compare (RobotPart _ _ c1 _) (RobotPart _ _ c2 _) = compare c1 c2
  

type Html = String

data Box a = Box a deriving Show

instance Functor Box where
  fmap func (Box val) = Box (func val)

  
