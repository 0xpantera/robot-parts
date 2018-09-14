module Lib
    ( partsDB
    , minPart
    ) where

import Lib.Types
import qualified Data.Map as Map


leftArm :: RobotPart
leftArm = RobotPart
  { name = "left arm"
  , description = "left arm for face punching"
  , cost = 1000.0
  , count = 3 }

rightArm :: RobotPart
rightArm = RobotPart 
  { name = "right arm"
  , description = "right arm for kind hand gestures"
  , cost = 1025.00
  , count = 5 }

robotHead :: RobotPart
robotHead = RobotPart
  { name = "robot head"
  , description = "this head looks mad"
  , cost = 5092.25
  , count = 2 }

leftLeg :: RobotPart
leftLeg = RobotPart 
  { name = "left leg"
  , description = "left leg for kicking"
  , cost = 2100.50
  , count = 5}

rightLeg :: RobotPart
rightLeg = RobotPart
  { name = "right leg"
  , description = "right leg for footsies"
  , cost = 2575.99
  , count = 4 }


renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>", partName, "</h2>",
                           "<p><h3>desc</h3>",partDesc,
                           "</p><p><h3>cost</h3>",
                           partCost,
                           "</p><p><h3>count</h3>",
                           partCount,"</p>"]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)


partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1,2,3,4,5]
    vals = [robotHead,leftArm,rightArm,leftLeg,rightLeg]
    keyVals = zip keys vals

minPart :: RobotPart -> RobotPart -> RobotPart
minPart p1 p2
  | p1 < p2 = p1
  | p1 > p2 = p2
  | otherwise = p1

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal


allParts :: [RobotPart]
allParts = snd <$> (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts


htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB


leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO



myBox :: Box Int
myBox = Box 1

testBox :: Box [Int]
testBox = Box [1,2,3]

morePresents :: Box a -> Int -> Box [a]
morePresents present n = (take n . repeat) <$> present

wrapped :: Box (Box Int)
wrapped = Box <$> myBox

unwrap :: Box a -> a
unwrap (Box val) = val
