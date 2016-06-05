module Every exposing
  ( Model
  , init
  , Msg (Start, Stop)
  , update
  )

{-|

This component will continually issue / poll an action / message
for some given amount of time, determined by a function you provide
the system.


## Polling State

@docs Model

@docs init


## Polling Invocation

@docs Msg


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

{-| The type of messages you can send to the poller -
    either initialization (or new input data for the action
    to be dispatched), or a cease-and-desist call.
-}
type Msg b
  = Start (Maybe b -> Maybe b)
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
    Start addState ->
      case model.elapsed of
        Nothing ->
          let newState = addState Nothing
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
          let newState = addState elap.state
              newDuration = duration newState 0
          in  ( { model | elapsed = Just { elap | soFar = 0
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
