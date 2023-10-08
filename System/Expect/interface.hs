{-# LANGUAGE ForeignFunctionInterface #-}

module System.Expect.ExpectInterface 

   ExpectType(ExpExact,ExpRegex,ExpGlob,ExpNull)

  ,ExpectCase(ExpectCase,expectPattern,expectType,expectValue)
  ,ExpectProc(ExpectProc,expectHandle,expectFilePtr)
  ,muteExpect,unmuteExpect
  ,spawnExpect
  ,expectCases,expectSingle,expectExact,expectRegex,expectMultiple
  ,sendLine)
where

import System.Expect.ExpectBindings as EB

import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.IO.Handle.FD
import System.IO

foreign import ccall "stdio.h fileno" fileno :: Ptr CFile -> IO CInt

data ExpectType = ExpExact  
                | ExpRegex 
                | ExpGlob   
                | ExpNull

data ExpectCase = ExpectCase {
                               expectPattern ::String 
                          
     		  	     , expectType :: ExpectType 
          
			     , expectValue :: Int     
                             }
data ExpectProc = ExpectProc {
                               expectFilePtr :: Ptr CFile
     		  	     , expectHandle :: Handle }

muteExpect :: IO ()
muteExpect = poke exp_loguser 0

unmuteExpect :: IO ()
unmuteExpect = poke exp_loguser 1

spawnExpect :: String 
            -> IO ExpectProc 
spawnExpect cmd = do
  cstr <- newCString cmd
  cfileptr <- EB.exp_popen cstr
  cfileno <- fileno cfileptr
  handle <- fdToHandle $ fromIntegral cfileno
  return $ ExpectProc cfileptr handle


expectCases :: ExpectProc
            -> [ExpectCase] 
            -> IO (Maybe Int) 

expectCases proc cases = do
  scases <- mapM toStorableCase cases
  sarray <- newArray (scases ++ [endStorableCase])
  cval <- EB.exp_fexpectv (expectFilePtr proc) sarray
  nlist <- peekArray (length scases + 1) sarray
  mapM_ freeStorableCase nlist
  if cval < 0 then return Nothing
     	      else return $ Just $ fromEnum cval

expectSingle :: ExpectProc 
             -> String     
             -> ExpectType
             -> IO (Maybe Int) 
expectSingle proc str ec = expectCases proc [ExpectCase str ec 1]

--processo geral da interface

expectExact :: ExpectProc -- atvio
            -> String -- ^padrao c91
            -> IO (Maybe Int)
expectExact proc exact = expectSingle proc exact ExpExact


expectRegex :: ExpectProc
            -> String
            -> IO (Maybe Int) 
expectRegex proc reg = expectSingle proc reg ExpRegex

expectMultiple :: ExpectProc 
               -> [String]
               -> ExpectType 
               -> IO (Maybe Int) 
expectMultiple proc ss ec = expectCases proc cases
    where cases = map (\(x,y) -> ExpectCase x ec y) (zip ss [1..])

sendLine :: ExpectProc 
         -> String
         -> IO ()
sendLine proc line = hPutStrLn (expectHandle proc) line

toStorableCase :: ExpectCase
               -> IO ExpCase
toStorableCase cs = do
  cstr <- newCString $ expectPattern cs
  cval <- (return . toEnum . expectValue) cs
  return $ ExpCase cstr nullPtr (expectTypeToExpType $ expectType cs) cval

endStorableCase :: ExpCase
endStorableCase = ExpCase nullPtr nullPtr expEnd 0

freeStorableCase :: ExpCase
                 -> IO ()
freeStorableCase cs = do
  if (regexp cs) == nullPtr then free (regexp cs)
     	     	    	    else return ()

expectTypeToExpType :: ExpectType -> ExpType
expectTypeToExpType ExpRegex = expRegexp
expectTypeToExpType ExpExact = expExact
expectTypeToExpType ExpGlob = expGlob
expectTypeToExpType ExpNull = expNull
