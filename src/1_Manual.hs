module Manual where 

-- Define data types and setters
data TestData = TestData
  { intField :: Int
  , stringField :: String
  , nestedField :: TestDataNested
  }

setInt :: Int -> TestData -> TestData
setInt newVal (TestData i s n) = TestData newVal s n

data TestDataNested = TestDataNested
  { innerIntField :: Int
  , innerStringField :: String
  }

setInnerInt :: Int -> TestDataNested -> TestDataNested
setInnerInt newVal (TestDataNested i s) = TestDataNested newVal s

-- Using it...
s1 = TestData 1 "s1" (TestDataNested 100 "s2")

-- intField :: TestData -> Int
intOuterIncrement = setInt (intField s1 + 1) s1

intInnerIncrement =
  let s1nested = nestedField s1
  in  TestData (intField s1) (stringField s1) (setInnerInt (innerIntField s1nested + 1) s1nested)