module Every exposing
  ( Model
  , init
  , Msg (Start, Stop)
  , ModifyData (..)
  , Modify
  , update
  )

{-|

This component will continually issue / poll an action / message
for some given amount of time, determined by a function you provide
the system.


## Polling State

@docs Model, init


## Polling Invocation

@docs Msg, ModifyData, Modify


## Polling Enactment

@docs update

-}

import Process
import Time exposing (Time)
import Task



type alias Elapsed b =
  { waitTil : Time
  , soFar   : Time
  , toWait  : Time
  , state   : Maybe b
  }

{-| The state of the poller
-}
type alias Model b =
  { elapsed : Maybe (Elapsed b)
  }

{-| Initial state of the poller, where `b` is the type of message
    to send on completion.
-}
init : Model b
init =
  { elapsed = Nothing
  }

{-| Either adjust the potentially stored data, or just assign some. -}
type ModifyData b
  = Update (Maybe b -> Maybe b)
  | Assign b

{-| We can modify the stored data, and also reset the accrued time so far. -}
type alias Modify b =
  { resetSoFar : Bool
  , modifyData : Maybe (ModifyData b)
  }

{-| The type of messages you can send to the poller:
- start a new thread, all with shared data
- adjust the data stored
- stop all threads

The api is a bit weird for now; I just can't manage a clean one. Expect changes in
the next version!
-}
type Msg b
  = Start (Modify b)
  | Adjust (Modify b)
  | SetWait Time
  | Invoke Time
  | Stop


{-| Given a method to compute the duration to wait until the next action is issued
    (calculated from the total time elapsed `total -> delay`), and the main action to
    issue, build an updating component.
-}
update : (Maybe b -> Time -> Time)
      -> (Maybe b -> Cmd a)
      -> Msg b
      -> Model b
      -> (Model b, Cmd (Result a (Msg b)))
update duration actions action model =
  case action of
    Start modifier ->
      case model.elapsed of
        Nothing ->
          let newState = case modifier.modifyData of
                           Nothing -> Nothing
                           Just md ->
                             case md of
                               Update addState -> addState Nothing
                               Assign state    -> Just state
              newDuration = duration newState 0
          in  ( { model | elapsed = Just { waitTil = 0 -- overwrite immediately
                                         , soFar = 0
                                         , toWait = newDuration
                                         , state = newState
                                         }
                }
              , Cmd.batch
                  [ Task.perform Debug.crash (Ok << SetWait) Time.now
                  , Task.perform Debug.crash (Ok << Invoke)
                      <| Process.sleep newDuration `Task.andThen`
                         \_ -> Time.now
                  ]
              )
        Just elap ->
          let newState = case modifier.modifyData of
                           Nothing -> elap.state
                           Just md ->
                             case md of
                               Update addState -> addState elap.state
                               Assign state    -> Just state
              newDuration = duration newState elap.soFar
          in  ( { model | elapsed = Just { elap | soFar = if modifier.resetSoFar
                                                          then 0
                                                          else elap.soFar
                                                , toWait = newDuration
                                                , state = newState
                                         }
                } -- update data
              , Cmd.batch
                  [ Task.perform Debug.crash (Ok << SetWait) Time.now
                  , Task.perform Debug.crash (Ok << Invoke)
                      <| Process.sleep newDuration `Task.andThen`
                         \_ -> Time.now
                  ]
              )
    Adjust modifier ->
      case model.elapsed of
        Nothing -> (model, Cmd.none)
        Just elap ->
          let newState = case modifier.modifyData of
                           Nothing -> elap.state
                           Just md ->
                             case md of
                               Update addState -> addState elap.state
                               Assign state    -> Just state
          in  ( { model | elapsed = Just { elap | soFar = if modifier.resetSoFar
                                                          then 0
                                                          else elap.soFar
                                                , state = newState
                                         }
                } -- update data
              , Cmd.none
              )
    SetWait now ->
      ( case model.elapsed of
          Nothing -> model -- stopped before last issued
          Just elap ->
            { model | elapsed = Just { elap | waitTil = now + elap.toWait }
            }
      , Cmd.none
      )
    Invoke now ->
      case model.elapsed of
        Nothing ->
          ( init -- was stopped before completion
          , Cmd.none
          )
        Just elap ->
          if now >= elap.waitTil
          then let newSoFar = elap.soFar + elap.toWait
                   newDuration = duration elap.state newSoFar
               in  ( { model | elapsed = Just { elap | soFar = newSoFar
                                                     , toWait = newDuration
                                              }
                     }
                   , Cmd.batch
                       [ Cmd.map Err <| actions elap.state
                       , Task.perform Debug.crash (Ok << Invoke)
                           <| Process.sleep newDuration `Task.andThen`
                                \_ -> Time.now
                       ]
                   )
          else ( model -- debounce
               , Cmd.none
               )
    Stop ->
      ( init
      , Cmd.none
      )
