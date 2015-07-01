


-- `a' does NOT need to be (Show a)
data One a where
  OneA  :: Int -> One Int
  OneB  :: Int -> One Double

-- `a' DOES need to be (Show a)
data Two a where
  TwoA  :: Int -> Two Int
  TwoB  :: Int -> Two Double
  TwoC  :: a   -> Two a

-- `a' does NOT need to be (Show a)
data Three a where
  ThreeA  :: Int   -> Three Int
  ThreeB  :: Int   -> Three Double
  ThreeC  :: One a -> Three a

-- `a' DOES need to be (Show a)
data Four a where
  FourA  :: Int   -> Four Int
  FourB  :: Int   -> Four Double
  FourC  :: Two a -> Four a

-- `a' does NOT to be (Show a)
-- AND SixC needs to be included
-- in the Read (Maybe Int) instance,
-- but NOT in the [Double] instance.
data Five a where
  FiveA  :: Int   -> Five (Maybe Int)
  FiveB  :: Int   -> Five [Double]
  FiveC  :: One a -> Five (Maybe a)

-- `a' DOES need to be (Show a)
-- AND SixC needs to be included
-- in the Read (Maybe Int) instance,
-- but NOT in the [Double] instance.

-- XXX: this one causes problems with the Show strategy
-- data Six a where
--   SixA  :: Int   -> Six (Maybe Int)
--   SixB  :: Int   -> Six [Double]
--   SixC  :: Two a -> Six (Maybe a)


