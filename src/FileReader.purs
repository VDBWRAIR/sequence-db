module FileReader where
foreign import data FS :: !

type ErrorCode = String
type FilePath = String 
--newtype ContT r m a = ContT ((a -> m r) -> m r)
type Async eff = ContT Unit (Eff eff)


import Data.Either 
import Control.Monad.Eff
import DOM -- needed for type signatures
--import Control.Monad.Eff.Console (print)
import Control.Monad.Cont.Trans
import Data.Function 
import Prelude
import DOM.File.Types (File())
foreign import readFileImpl ::
                 forall eff. Fn3 File
                   (String -> Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

--foreign import firstFile :: forall eff. J.JQuery ->  File
--foreign import firstFile :: forall eff. J.JQuery ->  Eff ( dom :: DOM | eff) File

---foreign import files :: forall eff. J.JQuery ->  Eff ( dom :: DOM | eff) (Array File)
--main = runContT 
--  (readFileCont "README.md") 

--  print

readFile' path = runContT (readFileCont path)
  
readFileCont :: forall eff. 
  File -> 
  Async (fs :: FS | eff) (Either ErrorCode String)
readFileCont path = ContT $ readFile path


readFile :: forall eff. 
  File -> 
  (Either ErrorCode String -> Eff (fs :: FS | eff) Unit) -> 
  Eff (fs :: FS | eff) Unit
readFile path k = 
  runFn3 readFileImpl 
         path 
         (k <<< Right) 
         (k <<< Left)

foreign import readFileBlocking ::
  forall e. String -> Eff (fs :: FS | e ) String
