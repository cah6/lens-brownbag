module NoRedundantFields where

-- Define data types and setters
data TestData = TestData
  { intField :: Int
  , stringField :: String
  , nestedField :: TestDataNested
  }

setInt :: Int -> TestData -> TestData
setInt i original = original { intField = i }

data TestDataNested = TestDataNested
  { innerIntField :: Int
  , innerStringField :: String
  }

setInnerInt :: Int -> TestDataNested -> TestDataNested
setInnerInt i original = original { innerIntField = i }

-- Using it...
s1 = TestData 1 "s1" (TestDataNested 100 "s2")

-- intField :: TestData -> Int
intOuterIncrement = setInt (intField s1 + 1) s1

intInnerIncrement =
  let s1nested = nestedField s1
  in  s1 { nestedField = setInnerInt (innerIntField s1nested + 1) s1nested }