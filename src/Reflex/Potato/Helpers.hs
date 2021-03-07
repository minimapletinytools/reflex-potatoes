--TODO move some of this stuff into it's own lib when it's properly tested

{-# LANGUAGE RecursiveDo #-}




module Reflex.Potato.Helpers
  (
  -- other helpers
    dsum_to_dmap


  -- reflex helpers
  , simultaneous
  , assertEvent
  , assertEventWith
  , fmapMaybeWarn
  , fmapMaybeWarnWith
  , traceEventSimple
  , leftmostWarn
  , leftmostAssert
  , leftmostWarnWithIndex
  , alignWarn
  , alignAssert
  , foldDynMergeWith
  , foldDynMerge
  , fanDSum
  , pushAlwaysDyn

  , delayEvent
  , sequenceEvents
  , stepEvents
  , stepEventsAndCollectOutput
  , stepEventsAndSequenceCollectOutput
  )
where

import           Prelude            (error)
import           Relude             hiding (error)

import           Reflex

import           Control.Monad.Fix

import qualified Data.Dependent.Map as DM
import qualified Data.Dependent.Sum as DS
import           Data.These

-- | fires only when both events fire
simultaneous :: (Reflex t) => Event t a -> Event t b -> Event t (a,b)
simultaneous eva evb = alignEventWithMaybe
  (\case
    These a b -> Just (a,b)
    _ -> Nothing)
  eva
  evb

dsum_to_dmap :: DM.GCompare k => DS.DSum k f -> DM.DMap k f
dsum_to_dmap ds = DM.fromList [ds]


{- TODO can't get -fno-ignore-asserts to work
-- | assert that a predicate is true each time the event triggers
-- internall calls assert, which can be disabled via compiler options
-- enable explicitly with {-# OPTIONS_GHC -fno-ignore-asserts #-}
assertEvent :: (Reflex t, Show a)
  => String -- ^ assert message
  -> (a -> Bool) -- ^ predicate to check
  -> Event t a
  -> Event t a
assertEvent s p = fmap (\x -> byPred assert s p x x)

-- | assert that a predicate is true each time the event triggers
-- internall calls assert, which can be disabled via compiler options
-- enable explicitly with {-# OPTIONS_GHC -fno-ignore-asserts #-}
assertEventWith :: (Reflex t)
  => (a -> String) -- ^ assert message
  -> (a -> Bool) -- ^ predicate to check
  -> Event t a
  -> Event t a
assertEventWith sf p = fmap (\x -> byPred assert (sf x) id (p x) x)
-}

-- | assert that a predicate is true each time the event triggers
assertEvent :: (Reflex t, Show a)
  => String -- ^ assert message
  -> (a -> Bool) -- ^ predicate to check
  -> Event t a
  -> Event t a
assertEvent s p = fmap (\x -> if not (p x) then error $ s <> " " <> show x else x)

-- | assert that a predicate is true each time the event triggers
assertEventWith :: (Reflex t)
  => (a -> String) -- ^ assert message
  -> (a -> Bool) -- ^ predicate to check
  -> Event t a
  -> Event t a
assertEventWith sf p = fmap (\x -> if not (p x) then error $ sf x else x)

-- | same as fmapMaybe except outputs a warning if predicate fails
fmapMaybeWarn :: (Reflex t, Show a)
  => String -- ^ warning message
  -> (a -> Bool) -- ^ predicate to check
  -> Event t a
  -> Event t a
fmapMaybeWarn s p ev = r where
  ev' = fmap (\x -> (p x, x)) ev
  good = fmapMaybe (\(a,x) -> if a then Just x else Nothing) ev'
  bad =  fmapMaybe (\(a,x) -> if not a then Just x else Nothing) ev'
  r = leftmost [good, fmapMaybe (const Nothing) $ traceEvent s bad]

-- | same as fmapMaybe except outputs a warning if predicate fails
fmapMaybeWarnWith :: (Reflex t)
  => (a -> String) -- ^ warning message
  -> (a -> Bool) -- ^ predicate to check
  -> Event t a
  -> Event t a
fmapMaybeWarnWith sf p ev = r where
  ev' = fmap (\x -> (p x, x)) ev
  good = fmapMaybe (\(a,x) -> if a then Just x else Nothing) ev'
  bad =  fmapMaybe (\(a,x) -> if not a then Just x else Nothing) ev'
  r = leftmost [good, fmapMaybe (const Nothing) $ traceEventWith sf bad]

traceEventSimple :: (Reflex t) => String -> Event t a -> Event t a
traceEventSimple s = traceEventWith (const s)

-- | same as leftmost but outputs a warning if more than one event fires at once
leftmostWarn :: (Reflex t) => String -> [Event t a] -> Event t a
leftmostWarn label evs = r where
  combine = mergeList evs
  nowarn =
    fmapMaybe (\x -> if length x == 1 then Just (head x) else Nothing) combine
  warn =
    traceEventWith
        (const ("WARNING: multiple " <> label <> " events triggered"))
      $ fmapMaybe (\x -> if length x > 1 then Just (head x) else Nothing)
                  combine
  r = leftmost [nowarn, warn]

-- | same as leftmost but asserts if more than one event fires at once
leftmostAssert :: (Reflex t) => String -> [Event t a] -> Event t a
leftmostAssert label evs = r where
  combine = mergeList evs
  nowarn =
    fmapMaybe (\x -> if length x == 1 then Just (head x) else Nothing) combine
  warn =
    assertEventWith (const ("ASSERT: multiple " <> label <> " events triggered")) (const False)
      $ fmapMaybe (\x -> if length x > 1 then Just (head x) else Nothing)
                  combine
  r = leftmost [nowarn, warn]

-- | same as leftmostWarn but also adds an index for debugging
leftmostWarnWithIndex :: (Reflex t) => String -> [Event t a] -> Event t a
leftmostWarnWithIndex label evs = r where
  evsWithIndex = zipWith (\i -> fmap (i,)) [0..] evs
  combine = mergeList evsWithIndex
  nowarn =
    fmapMaybe (\x -> if length x == 1 then Just (head x) else Nothing) combine
  warn =
    traceEventWith
        (const ("WARNING: multiple " <> label <> " events triggered"))
      $ fmapMaybe (\x -> if length x > 1 then Just (head x) else Nothing)
                  combine
  r = fmap snd $ leftmost [nowarn, warn]



-- | same as align but only returns left event if both events fire
-- prints a warning if both events fire
alignWarn
  :: (Reflex t) => String -> Event t a -> Event t b -> Event t (Either a b)
alignWarn label ev1 ev2 =
  leftmostWarn label [Left <$> ev1, Right <$> ev2]


-- | same as align but returns an either and asserts if both events fire at once
alignAssert :: (Reflex t) => String -> Event t a -> Event t b -> Event t (Either a b)
alignAssert label = alignEventWithMaybe alignfn where
  alignfn (This a) = Just $ Left a
  alignfn (That b) = Just $ Right b
  alignfn _        = error $ "both events fired when aligning " <> label

foldDynMergeWith
  :: (Reflex t, MonadHold t m, MonadFix m)
  => b -- ^ initial value of dynamic
  -> [Event t (b -> b)]  -- ^ list of events producing a reducing method
  -> m (Dynamic t b)  -- ^ final output after all folding methods applied
foldDynMergeWith acc = foldDyn ($) acc . mergeWith (.)

foldDynMerge
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> b) -- ^ folding method
  -> b -- ^ initial value of dynamic
  -> [Event t a] -- ^ list of events
  -> m (Dynamic t b) -- ^ final output
foldDynMerge f acc evs = foldDynMergeWith acc (f <<$>> evs)

fanDSum
  :: forall t k
   . (Reflex t, DM.GCompare k)
  => Event t (DS.DSum k Identity)
  -> EventSelector t k
fanDSum ds = fan $ DM.fromAscList . (: []) <$> ds

-- TODO test
pushAlwaysDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> PushM t b)
  -> Dynamic t a
  -> m (Dynamic t b)
pushAlwaysDyn f da = do
  da0 <- sample . current $ da
  buildDynamic (f da0) $ pushAlways f (updated da)


selectNext :: [a] -> Maybe a
selectNext []      = Nothing
selectNext (x : _) = Just x
selectRest :: [a] -> Maybe [a]
selectRest []       = Nothing
selectRest (_ : []) = Nothing
selectRest (_ : xs) = Just xs

-- | delays an event by 1 tick
delayEvent
  :: forall t m a
   . (Adjustable t m)
  => Event t a
  -> m (Event t a)
delayEvent ev = do
  (_, evDelayed) <- runWithReplace (return ()) (fmap return ev)
  return evDelayed

-- | This takes two possibly simultaneous events to and sequences them to fire on different frames.
-- If both events fire at the same time, this functions returns an event with the second event's results that fires one frame after the first event fires.
sequenceEvents
  :: forall t m a b
   . (Adjustable t m, MonadFix m)
  => Event t a
  -> Event t b
  -> m (Event t b)
sequenceEvents ev1 ev2 = mdo
  let makeEv2Delayed :: m (Event t b)
      makeEv2Delayed = do
        let
          -- filters for when BOTH ev1 and ev2 triggered in the previous frame
            fmapfn = \case
              These _ v2 -> Just v2
              _           -> Nothing
            delayed = fmapMaybe fmapfn redo
        -- if ev1 does not trigger, delay does not trigger and this gives ev2
        -- if ev1 did trigger, and ev2 did not, this gives ev2
        -- if ev1 and ev2 both triggered, this gives previous value of evl2
        -- * note that it's possible for ev1 or ev2 to trigger in the second frame for outside reasons
        -- if this is the case, you really should not use this function
        return $ leftmost [delayed, difference ev2 ev1]
  (ev2Delayed, redo) <- runWithReplace
    makeEv2Delayed
    (alignEventWithMaybe (Just . return) ev1 ev2)
  return ev2Delayed

-- | Creates an output event that fires once for each input in the list.
-- Each output event runs in a different consecutive frame.
-- If an output event triggers the input event, they get appended to the end of the list of events to be triggered
stepEvents
  :: forall t m a
   . (Adjustable t m, MonadFix m)
  => Event t [a]
  -> m (Event t a)
stepEvents evin = mdo
  let
    -- if input event fires in subsequent ticks, append to end
    -- obviously, be mindful of infinite loops
      evin' :: Event t [a]
      evin' = mergeWith (\rev' ev' -> rev' <> ev') [rev, evin]
      next  = fmapMaybe selectNext evin'
      rest  = fmapMaybe selectRest evin'

  -- TODO this implementation is better but I can't figure out how to properly wrap request and response types
  --rev <- requestingIdentity (Identity <$> rest)
  --requestingIdentity (Identity <$> next)

  (_, rev) <- runWithReplace (return ()) (return <$> rest)
  return next

-- | Same as stepEvents but collects results for each event firing.
stepEventsAndCollectOutput
  :: forall t m a b
   . (Adjustable t m, MonadHold t m, MonadFix m)
  => Event t [a] -- ^ event to repeat
  -> Event t b -- ^ event to collect results from, only collects if event fires
  -> m (Event t a, Event t [b]) -- ^ (repeated event, collected results once event is done repeating)
stepEventsAndCollectOutput evin collectEv = mdo
  let
    -- if input event fires in subsequent ticks, append to end
    -- obviously, be mindful of infinite loops
    evin' :: Event t [a]
    evin' = mergeWith (\rev' ev' -> rev' <> ev') [rev, evin]
    next  = fmapMaybe selectNext evin'
    rest  = fmapMaybe selectRest evin'
    -- nothing left, this means we fired the last event
    stop  = fmapMaybe
      (\x -> if isNothing (selectRest x) then Just () else Nothing)
      evin'
    collected = tagPromptlyDyn (reverse <$> collector) stop

    -- collect events in reverse order
    -- reset when given the signal
    foldfn :: These Bool b -> [b] -> [b]
    foldfn (This True    ) _  = []
    foldfn (That b       ) bs = b : bs
    foldfn (These True  b) _  = [b]
    foldfn (These False b) bs = b : bs
    foldfn _               bs = bs

  -- we use the trick 'tag (current resetState) evin''
  -- which causes it to use resetState from previous iterations.
  collector <- foldDyn
    foldfn
    []
    (alignEventWithMaybe Just (tag (current resetState) evin') collectEv)

  resetState <- foldDyn
    const
    True
    (leftmost [const True <$> stop, const False <$> evin'])

  (_, rev) <- runWithReplace (return ()) (return <$> rest)
  return (next, collected)

-- | Same as stepEventsAndCollectOutput but the collected event fires one frame
-- AFTER the last input event fires
stepEventsAndSequenceCollectOutput
  :: forall t m a b
   . (Adjustable t m, MonadHold t m, MonadFix m)
  => Event t [a] -- ^ event to step
  -> Event t b -- ^ event to collect results from, only collects if event fires
  -> m (Event t a, Event t [b]) -- ^ (repeated event, collected results once event is done repeating)
stepEventsAndSequenceCollectOutput evin collectEv = mdo
  let
    -- if input event fires in subsequent ticks, append to end
    -- obviously, be mindful of infinite loops
    evin' :: Event t [a]
    evin' = mergeWith (\rev' ev' -> rev' <> ev') [rev, evin]
    next  = fmapMaybe selectNext evin'
    rest  = fmapMaybe selectRest evin'
    -- nothing left, this means we fired the last event
    stop  = fmapMaybe
      (\x -> if isNothing (selectRest x) then Just () else Nothing)
      evin'
    collected = tag (current (reverse <$> collector)) stop

    -- collect events in reverse order
    -- reset when given the signal
    foldfn :: These Bool b -> [b] -> [b]
    foldfn (This True    ) _  = []
    foldfn (That b       ) bs = b : bs
    foldfn (These True  b) _  = [b]
    foldfn (These False b) bs = b : bs
    foldfn _               bs = bs

  -- we use the trick 'tag (current resetState) evin''
  -- which causes it to use resetState from previous iterations.
  collector <- foldDyn
    foldfn
    []
    (alignEventWithMaybe Just (updated resetState) collectEv)

  resetState <- foldDyn
    const
    True
    (leftmost [const True <$> stop, const False <$> evin'])

  (_, rev) <- runWithReplace (return ()) (return <$> rest)
  return (next, collected)
