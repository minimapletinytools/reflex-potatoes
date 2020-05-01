{-# LANGUAGE RecursiveDo #-}

module Reflex.Potato.HelpersSpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (last)

import           Reflex
import           Reflex.Potato.Helpers

import           Reflex.Test.Host

warning_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t ()) -> PerformEventT t m (Event t ())
warning_network ev = do
  -- ensure leftmostwarn gives a warning
  let
    ev1 = leftmostwarn "expected" [ev, ev]
  -- ensure fmapMaybeWarn gives a warning
    ev2 = fmapMaybeWarn "expected" (const False) ev
    ev2Failed = assertEvent "must not happen" (const False) ev2
  -- ensure fmapMaybeWarn does not give a warning
    ev3 = fmapMaybeWarn "expected" (const True) ev
  -- ensure assertEvent gives error (uncomment to test)
    --ev4 = assertEvent "must crash" (const False) ev
    ev4 = never
  -- force all events by collecting them
  return $ leftmost [ev1, ev2, ev2Failed, ev3, ev4]

test_warning :: Test
test_warning = TestLabel "delayEvent" $ TestCase $ do
  let
    bs = [()]
    run :: IO [[Maybe ()]]
    run = runAppSimple warning_network bs
  _ <- liftIO run
  return ()




delayEvent_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t Int -> PerformEventT t m (Event t Int))
delayEvent_network ev = mdo
  delayedEv <- delayEvent ev
  return $ leftmostwarn "delayEvent" [ev, delayedEv]

test_delayEvent :: Test
test_delayEvent = TestLabel "delayEvent" $ TestCase $ do
  let
    n = 100
    bs = [0..n] :: [Int]
    run :: IO [[Maybe Int]]
    run = runAppSimple delayEvent_network bs
  v <- liftIO run
  join v @?= [Just (x `div` 2) | x <- [0..(n*2+1)]]


sequenceEvents_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t (Int, Int) -> PerformEventT t m (Event t Int))
sequenceEvents_network ev = mdo
  let fstEv = fmap fst ev
      sndEv = fmap snd ev
  delayedSndEv <- sequenceEvents fstEv sndEv
  return $ leftmostwarn "sequenceEvents" [fstEv, delayedSndEv]

test_sequenceEvents :: Test
test_sequenceEvents = TestLabel "sequenceEvents" $ TestCase $ do
  let bs = [(0, 1)] :: [(Int, Int)]
      run :: IO [[Maybe Int]]
      run = runAppSimple sequenceEvents_network bs
  v <- liftIO run
  join v @?= [Just 0, Just 1]


stepEventsAndCollectOutput_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t [Int] -> PerformEventT t m (Event t [Int]))
stepEventsAndCollectOutput_network ev = mdo
  (repeated, collected) <- stepEventsAndCollectOutput ev repeated
  return collected

test_stepEventsAndCollectOutput :: Test
test_stepEventsAndCollectOutput =
  TestLabel "stepEventsAndCollectOutput" $ TestCase $ do
    let bs = [[0], [], [1 .. 5], [], [], [1, 2], [1 .. 10], []] :: [[Int]]
        run :: IO [[Maybe [Int]]]
        run = runAppSimple stepEventsAndCollectOutput_network bs
    v <- liftIO run
    fmap L.last v @?= fmap Just bs

stepEvents_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t [Int] -> PerformEventT t m (Event t Int))
stepEvents_network = stepEvents

test_stepEvents :: Test
test_stepEvents = TestLabel "stepEvents" $ TestCase $ do
  let bs = [[1 .. 10], [0], [], [1 .. 5], [], [], [1, 2]] :: [[Int]]
      run :: IO [[Maybe Int]]
      run = runAppSimple stepEvents_network bs
  v <- liftIO run
  --print v
  return ()
  L.last v @?= [Just 1, Just 2]

spec :: Spec
spec = do
  describe "Potato" $ do
    fromHUnitTest test_warning
    fromHUnitTest test_stepEvents
    fromHUnitTest test_stepEventsAndCollectOutput
    fromHUnitTest test_sequenceEvents
    fromHUnitTest test_delayEvent
