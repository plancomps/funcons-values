
module Funcons.Operations (
  module Funcons.Operations.Atoms,
  module Funcons.Operations.ADTs,
  module Funcons.Operations.Expr,
  module Funcons.Operations.Values,
  module Funcons.Operations.Eval,
  module Funcons.Operations.Lists,
  module Funcons.Operations.Tuples,
  module Funcons.Operations.Booleans,
  module Funcons.Operations.Optionals,
  module Funcons.Operations.Types,
  module Funcons.Operations.NonGroundValues,
  module Funcons.Operations.Integers,
  module Funcons.Operations.Floats,
  module Funcons.Operations.Strings,
--  module Funcons.Operations.Rationals,
  module Funcons.Operations.Graphs,
  module Funcons.Operations.Sets,
  module Funcons.Operations.Multisets,
  module Funcons.Operations.Bits,
  module Funcons.Operations.Characters,
  module Funcons.Operations.Maps,
  libApp, libAppWith, libFromList, libUnite, Funcons.Operations.library, Library
  ) where

import Funcons.Operations.Expr 
import Funcons.Operations.Libraries 
import Funcons.Operations.Values hiding (showArgs, set_)
import Funcons.Operations.Eval hiding (showArgs)
import Funcons.Operations.Atoms hiding (library)
import qualified Funcons.Operations.Atoms (library)
import Funcons.Operations.Booleans hiding (library)
import qualified Funcons.Operations.Booleans (library)
import Funcons.Operations.Optionals hiding (library)
import qualified Funcons.Operations.Optionals (library)
import Funcons.Operations.Lists hiding (library)
import qualified Funcons.Operations.Lists (library)
import Funcons.Operations.Tuples hiding (library)
import qualified Funcons.Operations.Tuples (library)
import Funcons.Operations.Types hiding (library)
import qualified Funcons.Operations.Types (library)
import Funcons.Operations.NonGroundValues hiding (library, isGround)
import qualified Funcons.Operations.NonGroundValues (library)
import Funcons.Operations.Integers hiding (isInt, unInt, library)
import qualified Funcons.Operations.Integers (library)
import Funcons.Operations.Floats hiding (isInt, unInt, library)
import qualified Funcons.Operations.Floats (library)
import Funcons.Operations.Strings hiding (library)
import qualified Funcons.Operations.Strings (library)
--import Funcons.Operations.Rationals hiding (library)
--import qualified Funcons.Operations.Rationals (library)
import Funcons.Operations.Graphs hiding (library)
import qualified Funcons.Operations.Graphs (library)
import Funcons.Operations.Sets hiding (library, sets)
import qualified Funcons.Operations.Sets (library)
import Funcons.Operations.Multisets hiding (library, multisets)
import qualified Funcons.Operations.Multisets (library)
import Funcons.Operations.Bits hiding (library)
import qualified Funcons.Operations.Bits (library)
import Funcons.Operations.Characters hiding (library)
import qualified Funcons.Operations.Characters (library)
import Funcons.Operations.Maps hiding (library, maps)
import qualified Funcons.Operations.Maps (library)
import Funcons.Operations.ADTs hiding (library)
import qualified Funcons.Operations.ADTs

library :: (HasValues t, Ord t) => Library t
library = libUnite [
    Funcons.Operations.Atoms.library
  , Funcons.Operations.ADTs.library
  , Funcons.Operations.Booleans.library
  , Funcons.Operations.Optionals.library
  , Funcons.Operations.Lists.library
  , Funcons.Operations.Tuples.library
  , Funcons.Operations.Types.library
  , Funcons.Operations.NonGroundValues.library
  , Funcons.Operations.Integers.library
  , Funcons.Operations.Floats.library
  , Funcons.Operations.Strings.library
--  , Funcons.Operations.Rationals.library
  , Funcons.Operations.Graphs.library
  , Funcons.Operations.Sets.library
  , Funcons.Operations.Multisets.library
  , Funcons.Operations.Bits.library
  , Funcons.Operations.Characters.library
  , Funcons.Operations.Maps.library
  ]

libApp :: (HasValues t, Ord t) => OP -> [OpExpr t] -> Maybe (OpExpr t)
libApp = libAppWith library

libAppWith :: (HasValues t, Ord t) => Library t -> OP -> [OpExpr t] -> Maybe (OpExpr t)
libAppWith lib op args = do 
  valop <- libLookup op lib
  case (args, valop) of
    ([], NullaryExpr op)      -> Just op
    ([x], UnaryExpr op)       -> Just (op x)
    ([x,y], BinaryExpr op)    -> Just (op x y)
    ([x,y,z], TernaryExpr op) -> Just (op x y z)
    (_, NaryExpr op)          -> Just (op args)
    _                         -> Nothing

