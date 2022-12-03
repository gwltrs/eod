module Type.LiveDay where

import Prelude

-- bulkDayFromJSON :: String -> Maybe BulkDay
-- bulkDayFromJSON jsonStr = 
--   let 
--     obj = (rightToMaybe $ jsonParser jsonStr) >>= toObject
--     co = obj >>= lookup "code" >>= toString
--     d = obj >>= lookup "date" >>= toString
--     o = obj >>= lookup "open" >>= toNumber
--     h = obj >>= lookup "high" >>= toNumber
--     l = obj >>= lookup "low" >>= toNumber
--     cl = obj >>= lookup "close" >>= toNumber
--     v = obj >>= lookup "volume" >>= toNumber
--   in bulkDay <$> co <*> d <*> o <*> h <*> l <*> cl <*> v
