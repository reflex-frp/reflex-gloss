{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module Reflex.Gloss
  ( playReflex
  , InputEvent )
  where

import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (liftIO)
import           Data.Dependent.Sum (DSum ((:=>)))
import           Data.IORef             (readIORef)


import           Graphics.Gloss                   (Color, Display, Picture)
import           Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G

import           Reflex
import           Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)

type InputEvent = G.Event

type GlossApp t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Event t Float
                  -> Event t InputEvent
                  -> m (Behavior t Picture)

playReflex
  :: Display
  -> Color
  -> Int
  -> (forall t m. GlossApp t m)
  -> IO ()
playReflex display color frequency network =
  runSpiderHost $ do
    (tickEvent,  tickTriggerRef)  <- newEventWithTriggerRef
    (inputEvent, inputTriggerRef) <- newEventWithTriggerRef

    picture <- runHostFrame $ network tickEvent inputEvent

    liftIO $ playIO display
           color
           frequency
           ()
           (\_    -> runSpiderHost $ runHostFrame (sample picture))
           (\ge _ -> runSpiderHost $ handleTrigger ge inputTriggerRef)
           (\fl _ -> runSpiderHost $ handleTrigger fl tickTriggerRef)

  where
    handleTrigger e trigger = do
      mETrigger <- liftIO $ readIORef trigger
      case mETrigger of
        Nothing       -> return ()
        Just eTrigger -> fireEvents [eTrigger :=> e]
