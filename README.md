# Elm-Every

Repeatidly issue an action, with some time frame in-between.

It's pretty simple to integrate, all you need is:

- a function to determine how much time to wait until the next issue, based on
  the total time waited so far: `Time -> Time`
- an action to issue, with some data it would expect to apply with

Then, to start the poller, you issue `Start` with some data you would want the
action to use. To update this data, just make another call to `Start`. Then,
to stop the poller, just issue `Stop`.


```elm
type alias MyModel =
  { mySession : SessionModel
  , poller : Every
  }

initMyModel : MyModel
initMyModel =
  { mySession = initSession
  , poller = initEvery
  }

type MyAction
  = SessionMsg SessionMsg
  | EveryMsg EveryMsg

-- we're calculating the time to add to the total, based on the current total.
fibbDelay : Time -> Time
fibbDelay total = total

updateModel : MyAction
           -> MyModel
           -> (MyModel, Cmd MyAction)
updateModel action model =
  case action of
    SessionMsg a ->
      let (newSession, eff) = updateSession a model.mySession
      in  ( { model | mySession = newSession }
          , Cmd.map SessionMsg eff
          )
    EveryMsg a ->
      let (newEvery, eff) = updateEvery fibbDelay (\v -> SessionMsg <| UpdateSession v)
                              a model.poller
      in  ( { model | poller = newEvery }
          , Cmd.map (handleEveryResults EveryMsg) eff
          )
```

Then somewhere, you would _set_ the `v` value with `EveryMsg <| Start v`, and
logout or something similar with `EveryMsg Stop`. This isn't a very realistic
example though - why would we wait for session tokens at a Fibbonacci scale?
Who knows :) It would probably be more realistic with something like `\_ -> second`
or something, and _inside_ the `updateSession` routine, we pack in `Start v` with
a new session token to check or something.
