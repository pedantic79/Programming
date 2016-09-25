> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> import Control.Exception (IOException, try)
> import Control.Monad (liftM2)
> import Control.Monad.Trans (MonadIO, lift)
> import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
> import Control.Monad.Trans.Reader (Reader, ReaderT(..), ask, runReader, runReaderT)

> import Control.Monad.Reader (MonadReader)
> import Control.Monad.Except (MonadError)



Reader Int stores an Int, and can be retrieved via `ask`.
In the function `test`, we're passing an Int in and then
adding it with the value stored in the Reader Monad.

We use liftM2 to lift (+) to the monad. The value from
Int already lifted into the monad. We just use pointfree
notation and pure to lift the parameter up into the monad.

To write this in non-pointfree

test v = liftM2 (+) ask (pure v)

> test :: Int -> Reader Int Int
> test = liftM2 (+) ask . pure

To run test, we need to use the runReader function
runReader :: Reader r a -> r -> a

The first parameter is stored in the monad, and the second
is passed to the function.

runTest u v = runReader (test v) u

> runTest :: Int -> Int -> Int
> runTest = runReader . test

Here we are going to combine IO and Reader with the use of
the monad transformer, ReaderT. ReaderT r m a

The `r` type is the value stored in the ReaderT monad, the
m is the next monad in the stack. In our case it will be
IO, since IO must be at the bottom of the stack. The last
type is the return type of the overall monad stack.

testIO takes a filename and adds the size of that to the
value stored in the Reader monad. testIO is how it would look
in do notation. We use `lift` to lift the value from
the IO operation into the ReaderT

The value from v is already lifted.

> testIO :: FilePath -> ReaderT Int IO Int
> testIO f = do
>   l <- lift . fmap length . readFile $ f
>   v <- ask
>   return $ v + l

testIO2 is how you would write testIO without the do in pointfree notation

testIO2 f =  liftM2 (+) ask (lift . fmap length . readFile f)

> testIO2 :: FilePath -> ReaderT Int IO Int
> testIO2 = liftM2 (+) ask . lift . fmap length . readFile

Here's how to run the ReaderT. It's very similar to before.

runTestIO f v = runReaderT (testIO f) v

> runTestIO :: FilePath -> Int -> IO Int
> runTestIO = runReaderT . testIO

The problem with testIO is that it will get an exception if we run it with
a file that doesn't exist.
ghci> runTestIO "bad" 0
*** Exception: bad: openFile: does not exist (No such file or directory)

To handle this, we can use the ExceptT transformer. In testIOSafe, we
replace the type IO with (ExceptT IOException IO)

The code is similar to before, except we use `ExceptT . try` to automatically
wrap our value into a Right or a Left if an exception occurred.

> testIOSafe :: FilePath -> ReaderT Int (ExceptT IOException IO) Int
> testIOSafe f = do
>   l <- lift . fmap length . ExceptT . try . readFile $ f
>   v <- ask
>   return $ v + l

testIOSafe2 f =  liftM2 (+) ask (lift . fmap length . ExceptT . try . readFile f)

> testIOSafe2 :: FilePath -> ReaderT Int (ExceptT IOException IO) Int
> testIOSafe2 = liftM2 (+) ask . lift . fmap length . ExceptT . try . readFile

Here we are returning an (Either IOException Int)

runTestIOSafe f v = runExceptT (runReaderT (testIOSafe f) v)

> runTestIOSafe :: FilePath -> Int -> IO (Either IOException Int)
> runTestIOSafe = (runExceptT .) . runReaderT . testIOSafe
