module Every exposing
  ( Model
  , init
  , Msg (Start, Adjust, Stop)
  , update
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

@docs update

-}

import Process
import Time exposing (Time)
import Task



{-| The state of the poller
-}
type alias Model b =
  { threadId : Int
  , data     : b
  , stop     : Bool
  }

{-| Initial state of the poller, where `b` is the type of message
    to send on completion.
-}
init : b -> Model b
init initB =
  { threadId = 0
  , data = initB
  , stop = False
  }

{-| The type of messages you can send to the poller:
- start a new thread, all with shared data
- adjust the data stored
- stop all threads

The api is a bit weird for now; I just can't manage a clean one. Expect changes in
the next version!
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

{-| Given a method to compute the duration to wait until the next action is issued
    (calculated from the total time elapsed `total -> delay`), and the main action to
    issue, build an updating component.
-}
update : (b -> Time -> Time)
      -> (b -> Cmd a)
      -> Msg b
      -> Model b
      -> (Model b, Cmd (Result a (Msg b)))
update duration actions action model =
  let (threadId, model') = freshThreadId model
  in case action of
    Start modifier ->
      let firstDuration = duration model.data 0
      in  ( { model' | data = modifier model'.data
                     , stop = False
            }
          , Task.perform (Debug.crash << toString) (\_ -> Ok <| Invoke threadId firstDuration)
              <| Process.sleep firstDuration
          )
    Adjust modifier ->
      let newData = modifier.modify model.data
      in if modifier.reset
      then let firstDuration = duration model.data 0
      in   ( { model' | data = newData
                      , stop = False
             }
           , Task.perform (Debug.crash << toString) (\_ -> Ok <| Invoke threadId firstDuration)
               <| Process.sleep firstDuration
           )
      else ( { model | data = newData }
           , Cmd.none
           )
    Invoke threadId' soFar ->
      if model.stop
      then (model, Cmd.none)
      else if model.threadId - 1 == threadId' -- wouldve been the last id before I added one
      then let newDuration = duration model.data soFar
      in   ( model'
           , Cmd.batch
               [ Cmd.map Err <| actions model.data
               , Task.perform Debug.crash (\_ -> Ok <| Invoke threadId newDuration)
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
