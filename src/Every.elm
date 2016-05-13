module Every exposing
  ( Every
  , initEvery
  , EveryMsg (Start, Stop)
  , EveryResults
  , handleEveryResults
  , updateEvery
  )

{-|

This component will continually issue / poll an action / message
for some given amount of time, determined by a function you provide
the system.


## Polling State

@docs Every

@docs initEvery


## Polling Invocation

@docs EveryMsg


## Polling Enactment

@docs updateEvery

@docs EveryResults

@docs handleEveryResults

-}

import Process
import Time exposing (Time)
import Task



type alias Elapsed =
  { soFar  : Time
  , toWait : Time
  }

{-| The state of the poller
-}
type alias Every b =
  { elapsed : Maybe (Elapsed, b)
  }

{-| Initial state of the poller
-}
initEvery : Every b
initEvery =
  { elapsed = Nothing
  }

{-| The type of messages you can send to the poller -
    either initialization (or new input data for the action
    to be dispatched), or a cease-and-desist call.
-}
type EveryMsg b
  = Start b
  | Invoke
  | Stop

{-| The type of results during update - either another `EveryMsg`,
    or an actual action to be dispatched. This is all handled under-the-hood
    with `handleEveryResults`.
-}
type EveryResults b a
  = More (EveryMsg b)
  | Issue a

{-| Given a way to integrate an `EveryMsg` into your action type,
    you can turn a `EveryResults` into your action type.
-}
handleEveryResults : (EveryMsg b -> a) -> EveryResults b a -> a
handleEveryResults f m =
  case m of
    More x  -> f x
    Issue x -> x

{-| Given a method to compute the duration to wait until the next action is issued
    (calculated from the total time elapsed `total -> delay`), and the main action to
    issue, build an updating component.
-}
updateEvery : (Time -> Time)
           -> (b -> a)
           -> EveryMsg b
           -> Every b
           -> (Every b, Cmd (EveryResults b a))
updateEvery duration mainAction action model =
  case action of
    Start x ->
      case model.elapsed of
        Nothing ->
          ( { model | elapsed = Just ( { soFar = 0
                                       , toWait = duration 0
                                       }
                                     , x
                                     )
            }
          , Task.perform Debug.crash (\_ -> More Invoke)
              <| Process.sleep (duration 0)
          )
        Just (elap, _) ->
          ( { model | elapsed = Just (elap, x) } -- update data
          , Cmd.none
          )
    Invoke ->
      case model.elapsed of
        Nothing ->
          ( initEvery -- was stopped before completion
          , Cmd.none
          )
        Just (elap,x) ->
          let total = elap.soFar + elap.toWait
              newDuration = duration total
          in  ( { model | elapsed = Just ( { soFar = total, toWait = newDuration }
                                         , x
                                         )
                }
              , Cmd.batch
                  [ Task.perform Debug.crash (\x -> x)
                      <| Task.succeed <| Issue <| mainAction x
                  , Task.perform Debug.crash (\_ -> More Invoke)
                      <| Process.sleep newDuration
                  ]
              )
    Stop ->
      ( initEvery
      , Cmd.none
      )
