{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Reflex.Gloss
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- A Gloss interface for Reflex.
--
-------------------------------------------------------------------------------

module Reflex.Gloss
  ( playReflex
  , InputEvent
  , GlossApp )
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

-- | Synonym for a Gloss Event to prevent name clashes.
type InputEvent = G.Event

-- | Convert the refresh and input events to a Behavior t Picture.
type GlossApp t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Event t Float
                  -> Event t InputEvent
                  -> m (Behavior t Picture)
-- | Play the 'GlossApp' in a window, updating when the Behavior t Picture
--   changes.
playReflex
  :: Display                    -- ^ Display mode.
  -> Color                      -- ^ Background color.
  -> Int                        -- ^ Maximum frames per second.
  -> (forall t m. GlossApp t m) -- ^ A reflex function that returns a 'Behavior t Picture'
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
