module EitherUtils where

fromMaybe :: a -> Maybe b -> Either a b
fromMaybe _ (Just x) = Right x
fromMaybe x Nothing  = Left x

withLeft :: a -> Either a b -> Either a b
withLeft x = either (const $ Left x) Right
