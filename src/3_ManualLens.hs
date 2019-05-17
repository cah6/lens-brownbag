{-# LANGUAGE RankNTypes #-}
module ManualLens where

import Control.Lens

s1 = TestData 1 "s1" (TestDataNested 100 "s2")

-- Define data types and setters
data TestData = TestData
  { _intField :: Int
  , _stringField :: String
  , _nestedField :: TestDataNested
  } deriving Show

data TestDataNested = TestDataNested
  { _innerIntField :: Int
  , _innerStringField :: String
  } deriving Show

-- this says: given TestData, I know how to focus on intField, which is an Int
intField :: Lens' TestData Int
intField f testData = 
  let 
    setter newVal = testData { _intField = newVal } 
    getter = _intField testData
  in 
    setter <$> f getter

nestedField :: Lens' TestData TestDataNested
nestedField f testData = 
  let 
    setter newVal = testData { _nestedField = newVal } 
    getter = _nestedField testData
  in 
    setter <$> f getter

innerIntField :: Lens' TestDataNested Int
innerIntField f testDataNested = 
  let 
    setter newVal = testDataNested { _innerIntField = newVal } 
    getter = _innerIntField testDataNested
  in 
    setter <$> f getter

-- whew, done defining lenses and data types. let's play with them

-- Lens library gives us "view" and "set", which can use the Lens' to define either
getInt :: TestData -> Int
getInt input = view intField input

setInt :: Int -> TestData -> TestData
setInt i input = set intField i input

-- Since Lens' knows how to set and get, the library also gives us methods that can do
-- combinations of these, like modify (over)
incrementInt :: TestData -> TestData
incrementInt input = over intField (+1) input

-- even better, lenses are composable, in the sense of "function composition"
-- (f ∘ g) x = f (g x)
-- (f . g) x = f (g x)
aToC :: Lens' a b -> Lens' b c -> Lens' a c
aToC lensAB lensBC = lensAB . lensBC
-- aToC lensAB lensBC = (.) lensAB lensBC
-- aToC = (.)

directToInnerInt :: Lens' TestData Int
directToInnerInt = nestedField . innerIntField

-- why is this nice?
-- our nested get/set/modify becomes really easy!
getInnerInt :: TestData -> Int
getInnerInt input = view directToInnerInt input

setInnerInt :: Int -> TestData -> TestData
setInnerInt i input = set directToInnerInt i input

incrementInnerInt :: TestData -> TestData
incrementInnerInt input = over directToInnerInt (+1) input

incrementInnerInt' :: TestData -> TestData
incrementInnerInt' input = directToInnerInt +~ 1 $ input