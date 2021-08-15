module Symbolic where

import VM
import Data.Word (Word8)

-- SymbolicExpression
data SE = BytesConstant [Word8] | Uint64Constant Integer | UnknownBytes | UnknownUint64


add :: SE -> SE -> SVM SE SE
add (BytesConstant _) _ = fail "attempt to add bytes"
add _ (BytesConstant _) = fail "attempt to add bytes"
add UnknownBytes _ = fail "attempt to add bytes"
add _ UnknownBytes = fail "attempt to add bytes"
add UnknownUint64 _ = mplus (fail "+ overflow") (return UnknownUint64)
add _ UnknownUint64 = mplus (fail "+ overflow") (return UnknownUint64)
add (Uint64Constant x) (Uint64Constant y) = let z = x + y in
  if | z >= 2^64 -> fail "+ overflow"
     | otherwise -> return $ Uint64Constant z
