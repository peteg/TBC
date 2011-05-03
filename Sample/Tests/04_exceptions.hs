module T where

-- We need deepseq to get at exceptions buried in lazy structures.
deep_exception_Just_exception :: Maybe Int
deep_exception_Just_exception = Just (error "Without deepSeq we don't see this.")
