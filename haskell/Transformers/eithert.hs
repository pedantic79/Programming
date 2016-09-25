import Control.Exception (IOException, try)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)

safeLoadFile  :: FilePath -> IO (Either IOException String)
-- safeLoadFile f  = (Right <$> readFile f) `catch` (pure . Left)
safeLoadFile  = try . readFile

safeLoadFileT :: FilePath -> ExceptT IOException IO String
safeLoadFileT =  ExceptT . try . readFile

safeLoadFileU :: FilePath -> EitherT IOException IO String
safeLoadFileU =  EitherT . try . readFile

fileChars  :: FilePath -> IO (Either IOException Int)
fileChars  = fmap (fmap length) . safeLoadFile

fileCharsT :: FilePath -> ExceptT IOException IO Int
fileCharsT = fmap length . safeLoadFileT

fileCharsU :: FilePath -> EitherT IOException IO Int
fileCharsU = fmap length . safeLoadFileU

safePrintFile  :: FilePath -> IO (Either IOException ())
safePrintFile  = (traverse putStrLn =<<) . safeLoadFile

safePrintFileT :: FilePath -> ExceptT IOException IO ()
safePrintFileT = (lift . putStrLn =<<) . safeLoadFileT

safePrintFileU :: FilePath -> EitherT IOException IO ()
safePrintFileU = (lift . putStrLn =<<) . safeLoadFileU
