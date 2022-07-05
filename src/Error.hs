module Error where
import           Data.Text ( Text )
import           UnliftIO  ( Exception )

data AppError
  = ValidationError Text
  | NotFound Text
  | Unauthorized Text
  | Conflict Text
  deriving stock (Show)

instance Exception AppError
