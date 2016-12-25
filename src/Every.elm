module Every exposing
  ( Model
  , init
  , Msg (Start, Adjust, Stop)
  , update
  , waitingFor
  )

{-|

This component will continually issue / poll an action / message
for some given amount of time, determined by a function you provide
the system.


## Polling State

@docs Model, init


## Polling Invocation

@docs Msg


## Polling Enactment

@docs update, waitingFor

-}

import Process
import Time exposing (Time)
import Task



{-|
The state of the loop. Note that `soFar` is a block-like metric: it doesn't
change dynamically over the duration of time, but is rather the last-known
input to the new duration function.
-}
type alias Model b =
  { threadId : Int
  , data     : b
  , stop     : Bool
  , soFar    : Time
  }

{-| -}
init : b -> Model b
init initB =
  { threadId = 0
  , data     = initB
  , stop     = False
  , soFar    = 0
  }

{-| The type of messages you can send to poll:
- start again, debouncing and adjusting the data
- adjust the data stored and optionally debounce
- stop all threads
-}
type Msg b
  = Start (b -> b)
  | Adjust { modify : b -> b
           , reset  : Bool
           }
  | Invoke Int Time
  | Stop


freshThreadId : Model b -> (Int, Model b)
freshThreadId model =
  (model.threadId, { model | threadId = model.threadId + 1 })

{-|
Given a method to compute the duration to wait until the next action is issued
(calculated from the total time elapsed `total -> delay`), and the main action to
issue, build an updating component.

Also notice that this does not fire immediately.
-}
update : (b -> Time -> Time)
      -> (b -> Cmd a)
      -> Msg b
      -> Model b
      -> (Model b, Cmd (Result a (Msg b)))
update duration actions action model =
  let (threadId, model_) = freshThreadId model
  in case action of
    Start modifier ->
      let firstDuration = duration model.data 0
      in  ( { model_ | data  = modifier model_.data
                     , stop  = False
                     , soFar = 0
            }
          , Task.perform (\_ -> Ok <| Invoke threadId firstDuration)
              <| Process.sleep firstDuration
          )
    Adjust modifier ->
      let newData = modifier.modify model.data
      in if modifier.reset
      then let firstDuration  = duration model.data 0
      in   ( { model_ | data  = newData
                      , stop  = False
                      , soFar = 0
             }
           , Task.perform (\_ -> Ok <| Invoke threadId firstDuration)
               <| Process.sleep firstDuration
           )
      else ( { model | data = newData }
           , Cmd.none
           )
    Invoke threadId_ soFar ->
      if model.stop
      then (model, Cmd.none)
      else if model.threadId - 1 == threadId_ -- wouldve been the last id before I added one
      then let newDuration = duration model.data soFar
      in   ( { model_ | soFar = soFar }
           , Cmd.batch
               [ Cmd.map Err <| actions model.data
               , Task.perform (\_ -> Ok <| Invoke threadId newDuration)
                   <| Process.sleep newDuration
               ]
           )
      else ( model -- debounce
           , Cmd.none
           )
    Stop ->
      ( { model | stop = True }
      , Cmd.none
      )

{-|
Given the duration function and the current state, give the total time that is currently being
waited. Note that this is assuming there hasn't been a manipulation of the data stored since the
last issued tick.
-}
waitingFor : (b -> Time -> Time) -> Model b -> Time
waitingFor duration model =
  duration model.data model.soFar
