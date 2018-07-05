
module GoatGuardian.Password where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Scrypt (EncryptedPass(EncryptedPass), Pass(Pass), encryptPassIO', getEncryptedPass, verifyPass')
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)

hashPass
  :: MonadIO m
  => Text -- ^ password
  -> m Text -- ^ hashed password
hashPass pass = do
  hashedPassBS <- liftIO $ encryptPassIO' (Pass $ encodeUtf8 pass)
  pure $ decodeUtf8With lenientDecode (getEncryptedPass hashedPassBS)

checkPass
  :: Text -- ^ password
  -> Text -- ^ hashed password
  -> Bool
checkPass pass hash =
  verifyPass'
    (Pass $ encodeUtf8 pass)
    (EncryptedPass $ encodeUtf8 hash)
