[@bs.val] external setTimeout: (unit => unit, int) => float = "setTimeout";
open Belt.Option;
open Js.Promise;

module Reffect = {
  type reducerFunc('s, 'a, 'e) = ('s, 'a) => ('s, option('e));
  type dispatchFunc('a) = 'a => unit;

  let makeDispatch =
      (
        state: 's,
        reducer: reducerFunc('s, 'a, 'e),
        interpreter: ('e, dispatchFunc('a)) => unit,
        onNextState: 's => unit,
      )
      : dispatchFunc('a) => {
    let rec dispatch = (action: 'a): unit => {
      let (nextState, nextEffect) = reducer(state, action);
      onNextState(nextState);
      switch (nextEffect) {
      | Some(effect) => interpreter(effect, dispatch)
      | None => ()
      };
    };

    dispatch;
  };  
};

type route = {
  origin: string,
  destination: string,
};

[@bs.deriving accessors]
type action =
  | SetOrigin(string)
  | SetDestination(string)
  | SetMinutes(int)
  | FetchRoute
  | FetchedRoute(int)
  | Noop;

type routeFetchAbility =
  | CanFetch
  | CannotFetch;

type dataLoadingState =
  | Loading
  | NotLoading;

type state = {
  origin: option(string),
  destination: option(string),
  minutes: option(int),
  routeFetchAbility,
  dataLoadingState,
  routeDuration: option(int),
};

let initialState = {
  origin: None,
  destination: None,
  minutes: None,
  routeFetchAbility: CannotFetch,
  dataLoadingState: NotLoading,
  routeDuration: None,
};

let applyFetchAbility = stateEffect => {
  let state = fst(stateEffect);
  let routeFetchAbility =
    switch (state.origin, state.destination, state.minutes) {
    | (Some(_), Some(_), Some(_)) => CanFetch
    | _ => CannotFetch
    };

  ({...state, routeFetchAbility}, snd(stateEffect));
};

let displayString = ostr => {
  mapWithDefault(ostr, "nada", s => s);
};

let displayInt = i => {
  mapWithDefault(i, "nada", n => string_of_int(n));
};

let directionsApi = (origin, destination) => {
  "https://maps.googleapis.com/maps/api/directions/json?origin="
  ++ origin
  ++ "&destination="
  ++ destination
  ++ "&key=AIzaSyC6AfIwElNGcfmzz-XyBHUb3ftWb2SL2vU";
};

let canFetch = state =>
  switch (state.routeFetchAbility) {
  | CanFetch => true
  | CannotFetch => false
  };

type googleDuration = {value: int};

type googleLeg = {duration: googleDuration};

type googleRoute = {legs: array(googleLeg)};

type googleDirections = {routes: array(googleRoute)};

// Reffect model


type effect('a) =
  | CalculateRoute(string, string, int => 'a);

let reducer = (state, action) => {
  let res =
    switch (action) {
    | SetOrigin(point) => ({...state, origin: Some(point)}, None)
    | SetDestination(dest) => ({...state, destination: Some(dest)}, None)
    | SetMinutes(minutes) => ({...state, minutes: Some(minutes)}, None)
    | FetchRoute => (
        {...state, dataLoadingState: Loading},
        Some(
          CalculateRoute(
            state.origin->getExn,
            state.destination->getExn,
            fetchedRoute,
          ),
        ),
      )
    | FetchedRoute(i) =>
      ({...state, routeDuration: Some(i)}, None);
    | Noop => (state, None)
    };

  applyFetchAbility(res);
};
