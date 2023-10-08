module System.Expect.ExpectCombinators
       ( ExpectM (ExpectM), ExpectOption
       , spawn, send, switch, wait, check, readStr
       , mute, unmute
       , runExpect, runExpectIO )
       where

import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Expect
import System.IO

data ExpectM a = ExpectM { expectMProc :: (Maybe ExpectProc -> IO (a, Maybe ExpectProc)) }
data ExpectOption a = ExpectOption { optionType :: ExpectType
                                   , optionPattern :: String
                                   , optionAction :: (ExpectM a) }

instance Monad ExpectM where
  return a = ExpectM (\x -> return (a, x))
  (ExpectM f) >>= g = ExpectM (\x -> do { (a, x') <- f x; expectMProc (g a) x'})

spawn :: String -- ^ gerar
      -> ExpectM ()
spawn cmd = ExpectM (\_ -> do { p <- spawnExpect cmd; return ((), Just p) })

-- | Envie uma linha para o processo local.
send :: String -- ^ Linha a ser enviada para processo
     -> ExpectM ()
send line = ExpectM (\x -> if isJust x then do {sendLine (fromJust x) line; return ((), x) } else return ((), x))

-- | Leia N caracteres do terminal. Observe que isso inclui caracteres ecoados nas ações de envio.
readStr :: Int -- ^ Número de caracteres a serem lidos dentro do terminal
     -> ExpectM String
readStr count = ExpectM (\x -> if isJust x then do {a <- replicateM count (hGetChar (expectHandle (fromJust x))); return (a, x) } else return ("", x))

-- | Pega uma lista de casos e execute a ação do caso correspondente,
-- ou retorne um valor de falha no caso de não haver correspondências.
switch :: [ExpectOption a] -- ^ lisa
       -> a 
       -> ExpectM a
switch options failVal =  ExpectM (switch')
  where switch' p@(Just proc) =
          do let numOptions = zip [1..] options
                 cases = map (\(n,o) -> ExpectCase (optionPattern o) (optionType o) n) numOptions
             result <- expectCases proc cases
             if isJust result 
                then let opt = lookup (fromJust result) numOptions in
                     if isJust opt
                        then (expectMProc $ optionAction $ fromJust opt) p
                        else return (failVal, p)
                else return (failVal, p)
        switch' p = return (failVal, p)

wait :: ExpectType -- ^ padrão C90
     -> String -- ^ padrao txt
     -> ExpectM ()
wait expType pattern = ExpectM (\x -> do { _ <- expectCases (fromJust x) [ExpectCase pattern expType 1]; return ((), x) })

check :: ExpectType  -- ^ como interpreta
      -> String -- ^ padrao
      -> ExpectM a
      -> ExpectOption a
check expType pattern act = ExpectOption expType pattern act

unmute :: ExpectM ()
unmute = ExpectM (\x -> do { unmuteExpect; return ((),x) })

mute :: ExpectM ()
mute = ExpectM (\x -> do { muteExpect; return ((),x) })

runExpect :: ExpectM a 
          -> IO a
runExpect expAction = fst <$> expectMProc expAction Nothing

runExpectIO :: ExpectM (IO a) -- ^ Ação ExpectM a ser executada
            -> IO a
runExpectIO = join . runExpect
