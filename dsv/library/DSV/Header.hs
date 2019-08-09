{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.Header
  ( zipHeader, zipHeader', zipHeaderPipe
  , zipHeaderWith, zipHeaderWith', zipHeaderWithPipe
  , applyHeaderPipe, applyHeaderPipeM
  ) where

import DSV.Pipes
import DSV.Prelude
import DSV.Vector

-- pipes
import qualified Pipes.Prelude as P

{- |

=== Example

>>> :set -XOverloadedLists
>>> zipHeader [["A","B"],["1","2"],["3","4"]]
[[("A","1"),("B","2")],[("A","3"),("B","4")]]

-}

zipHeader :: forall a . [Vector a] -> [Vector (a, a)]
zipHeader [] = []
zipHeader (names : rows) = zipHeader' names rows

{- |

=== Example

>>> :set -XOverloadedLists
>>> zipHeader' ["A","B"] [["1","2"],["3","4"]]
[[("A","1"),("B","2")],[("A","3"),("B","4")]]

-}

zipHeader' :: forall a b . Vector a -> [Vector b] -> [Vector (a, b)]
zipHeader' names rows = map (vectorZip names) rows

{- |

=== Example

>>> :set -XOverloadedLists
>>> zipHeaderWith (<>) [["A","B"],["1","2"],["3","4"]]
[["A1","B2"],["A3","B4"]]

-}

zipHeaderWith :: forall a b . (a -> a -> b) -> [Vector a] -> [Vector b]
zipHeaderWith _ [] = []
zipHeaderWith f (names : rows) = zipHeaderWith' f names rows

{- |

=== Example

>>> :set -XOverloadedLists
>>> zipHeaderWith' (<>) ["A","B"] [["1","2"],["3","4"]]
[["A1","B2"],["A3","B4"]]

-}

zipHeaderWith' :: forall a b c . (a -> b -> c)
    -> Vector a -> [Vector b] -> [Vector c]
zipHeaderWith' f names rows = map (vectorZipWith f names) rows

applyHeaderPipe ::
    forall a b m r .
    Monad m
    => (a -> a -> b)
    -> Pipe a b m r

applyHeaderPipe f =
  do
    header <- await
    P.map (f header)

applyHeaderPipeM ::
    forall a b m r .
    Monad m
    => (a -> m (a -> m b))
    -> Pipe a b m r

applyHeaderPipeM f =
  do
    header <- await
    applyHeader <- lift (f header)
    P.mapM applyHeader

{- |

=== Example

>>> import qualified Pipes.Prelude as P
>>> r1 = listToVector ["A","B"]
>>> r2 = listToVector ["1","2"]
>>> r3 = listToVector ["3","4"]
>>> p = do { yield r1; yield r2; yield r3 }
>>> runEffect (p >-> zipHeaderPipe >-> P.print)
[("A","1"),("B","2")]
[("A","3"),("B","4")]

-}

zipHeaderPipe ::
    forall a m r .
    Monad m
    => Pipe (Vector a) (Vector (a, a)) m r

zipHeaderPipe = applyHeaderPipe vectorZip

{- |

=== Example

>>> import qualified Pipes.Prelude as P
>>> r1 = listToVector ["A","B"]
>>> r2 = listToVector ["1","2"]
>>> r3 = listToVector ["3","4"]
>>> p = do { yield r1; yield r2; yield r3 }
>>> runEffect (p >-> zipHeaderWithPipe (<>) >-> P.print)
["A1","B2"]
["A3","B4"]

-}

zipHeaderWithPipe ::
    forall a b m r .
    Monad m
    => (a -> a -> b)
    -> Pipe (Vector a) (Vector b) m r

zipHeaderWithPipe f = applyHeaderPipe (vectorZipWith f)
