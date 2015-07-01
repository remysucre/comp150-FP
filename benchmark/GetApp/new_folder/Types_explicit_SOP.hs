
-------------------------------------------------------------------------------

-- (See leaky.hs for comments.)

-------------------------------------------------------------------------------

  -- XXX Note that -dcore-lint exposes a bug in GHC with this module,
  -- so we cannot have it on when this module recompiles.
  -- Incidentally, this error happens with -O2 but not -O0...
  {-  OPTIONS_GHC -dno-core-lint #-}  -- nonesuch, unfortunately

  {-  OPTIONS_GHC -O2 #-}
  {-# OPTIONS_GHC -O0 #-}

-------------------------------------------------------------------------------

  {-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------

-- For NFDataP (which perforce includes NFDataN and NFData):
  {-# LANGUAGE TemplateHaskell #-}
  {-  LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  {-  LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE GADTs #-}  -- for GHC 7.6.3
  {-# LANGUAGE DeriveGeneric #-}
  {-# LANGUAGE DeriveDataTypeable #-}
  {-# LANGUAGE StandaloneDeriving #-}

-------------------------------------------------------------------------------

  -- RankNTypes wanted since some injected type signatures,
  -- due to imported types, may require it.
  -- The user currently has to add this themselves; it would
  -- be nice if SOME available form of automatic injection
  -- could do this!  (A text-based pre-processor might be able...).
  -- (So far as I know, no Haskell library or GHC feature will
  -- allow auto-injection of pragmas, but it's quite trivial
  -- as a text pre-process.)
  {-# LANGUAGE RankNTypes #-}

-------------------------------------------------------------------------------

  module Types_explicit_SOP where

  import Control.DeepSeq.Bounded
  import Control.DeepSeq.Generics

  import Generics.SOP.TH
  import GHC.Generics ( Generic )
  import Data.Typeable ( Typeable )
  import Data.Data ( Data )

--import Control.Concurrent ( threadDelay )
  import Control.Monad ( when )
--import Control.Exception ( evaluate )

  import System.Environment ( getArgs )
  import System.IO ( stdout, hFlush )

  import System.Random

  import Debug.Trace ( trace )

#if 0
#if FORCING_STRATEGY == 4
  import Seqaid.Runtime ( seqaid )
#endif
#if FORCING_STRATEGY >= 5
--import Seqaid.Runtime  -- comes in with Seqaid.TH
  import Seqaid.TH
#endif
#endif

-------------------------------------------------------------------------------

  type State = TA

  -- This is behaviourally a "strict blob", within the diagnostic
  -- purposes of leaky.  It incurs a large, constant cost when
  -- the head is evaluated.  This is also a reasonable model
  -- for function application (except then the cost is usually
  -- a function of argument values). [?]
  data Blob a = Blob [a]  deriving (Show,Generic,Typeable,Data)
  instance (NFData a,Num a) => NFData (Blob a) where rnf x = force (doWork x) `seq` ()
  instance (NFData a,Num a) => NFDataN (Blob a) where rnfn n x = force (doWork x) `seq` ()
  instance (NFData a,Num a,Typeable a) => NFDataP (Blob a) where rnfp p x = force (doWork x) `seq` ()
  doWork :: Num a => Blob a -> a
  doWork (Blob lst) = sum lst
--doWork (Blob lst) = sum lst :: Num a => a

-- XXX Why exactly is this needed, when it wasn't needed when
-- everything was in one module??...
#if 0
  {-# NOINLINE hackblah #-}
  hackblah :: IO StdGen
  hackblah = getStdGen
#else
  deriving instance Typeable StdGen
  instance NFData StdGen where rnf x = ()
  instance NFDataN StdGen where rnfn n x = ()
  instance NFDataP StdGen where rnfp p x = ()
#endif

#if 1

  -- (No strict fields.)
  data TA = A1 Int | A2 TB Int TC
  data TB = B1 | B2 TC | B3 Int TA TB
#if USE_STRICT_BLOB
  -- Note: I think Blob must NOT have a strictness bang (!).
  -- Later: I doubt it matters, the way doing Blob's now...
  data TC = C1 Int TC | C2 Int | C3 TC (Blob Int) ![Int] TC
#else
  data TC = C1 Int TC | C2 Int | C3 TC ![Int] TC
#endif

#else

  -- All Int fields strict (!):
  data TA = A1 !Int | A2 TB !Int TC
  data TB = B1 | B2 TC | B3 !Int TA TB
#if USE_STRICT_BLOB
  -- Note: I think Blob must NOT have a strictness bang (!).
  -- Later: I doubt it matters, the way doing Blob's now...
  data TC = C1 !Int TC | C2 !Int | C3 TC (Blob Int) ![Int] TC
#else
  data TC = C1 !Int TC | C2 !Int | C3 TC ![Int] TC
#endif

#endif

  deriving instance Show TA
  deriving instance Generic TA
  deriving instance Typeable TA
  deriving instance Data TA
  deriving instance Show TB
  deriving instance Generic TB
  deriving instance Typeable TB
  deriving instance Data TB
  deriving instance Show TC
  deriving instance Generic TC
  deriving instance Typeable TC
  deriving instance Data TC

  instance NFDataP TA where rnfp = grnfp
  instance NFDataN TA where rnfn = grnfn
  instance NFData  TA where rnf  = genericRnf
  instance NFDataP TB where rnfp = grnfp
  instance NFDataN TB where rnfn = grnfn
  instance NFData  TB where rnf  = genericRnf
  instance NFDataP TC where rnfp = grnfp
  instance NFDataN TC where rnfn = grnfn
  instance NFData  TC where rnf  = genericRnf

  deriveGeneric ''TA
  deriveGeneric ''TB
  deriveGeneric ''TC

-------------------------------------------------------------------------------

