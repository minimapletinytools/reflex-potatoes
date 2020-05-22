{-# LANGUAGE RecursiveDo #-}

module Reflex.Potato.TestingSpec
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



switchtest_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t Int -> TestGuestT t m (Event t Int))
switchtest_network ev = mdo
  let
    ev1 = fmapMaybe (\x -> if x == 1 then Just 1 else Nothing) ev
  outEvDyn <- foldDyn (\x _ -> if x == 1 then ev1 else never) ev ev
  --return $ switchPromptlyDyn outEvDyn
  return $ switchDyn outEvDyn

test_switchtest :: Test
test_switchtest = TestLabel "switchtest" $ TestCase $ do
  let
    bs = [0,1] :: [Int]
    run :: IO [[Maybe Int]]
    run = runAppSimple switchtest_network bs
  v <- liftIO run
  print v


spec :: Spec
spec = do
  describe "Reflex" $ do
    fromHUnitTest test_switchtest
