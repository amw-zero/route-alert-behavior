open Belt.Option;

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
      let (nextState, effect) = reducer(state, action);
      onNextState(nextState);
      forEach(effect, e => interpreter(e, dispatch));
    };

    dispatch;
  };
};

type route = {
  origin: string,
  destination: string,
};

type routeFetchAbility =
  | CanFetch
  | CannotFetch;

type dataLoadingState =
  | Loading
  | NotLoading;

// Server interaction

let directionsApi = (origin, destination) => {
  "https://maps.googleapis.com/maps/api/directions/json?origin="
  ++ origin
  ++ "&destination="
  ++ destination
  ++ "&key=AIzaSyC6AfIwElNGcfmzz-XyBHUb3ftWb2SL2vU"
  ++ "&departure_time=now";
};

type googleDuration = {value: int};

type googleLeg = {duration: googleDuration};

type googleRoute = {legs: list(googleLeg)};

type googleDirections = {routes: list(googleRoute)};

type routeAlert = {
  origin: string,
  destination: string,
  durationMinutes: int,
};

type errorResponse = {message: string};

let errorResponseDecoder = json => {
  Json.Decode.{message: json |> field("message", string)};
};

let errorResponseEncoder = errorResponse =>
  Json.Encode.(
    {
      object_([("message", errorResponse.message |> string)]);
    }
  );

let routeAlertDecoder = json =>
  Json.Decode.{
    origin: json |> field("origin", string),
    destination: json |> field("destination", string),
    durationMinutes: json |> field("durationMinutes", int),
  };

let routeAlertEncoder = routeAlert =>
  Json.Encode.(
    {
      object_([
        ("origin", routeAlert.origin |> string),
        ("destination", routeAlert.destination |> string),
        ("durationMinutes", routeAlert.durationMinutes |> int),
      ]);
    }
  );

let googleDurationDecoder = json =>
  Json.Decode.{value: json |> field("value", int)};

let googleLegDecoder = json =>
  Json.Decode.{
    duration: json |> field("duration_in_traffic", googleDurationDecoder),
  };

let googleRouteDecoder = json =>
  Json.Decode.{legs: json |> field("legs", list(googleLegDecoder))};

let googleDirectionsDecoder = json =>
  Json.Decode.{routes: json |> field("routes", list(googleRouteDecoder))};

type httpMethod =
  | Get
  | Post;

type serverRequest = {
  method: httpMethod,
  path: string,
  body: option(Js.Json.t),
};

let createRouteAlertEffectHandler =
    (routeAlertJson, networkBridge, onComplete) => {
  let routeAlert = routeAlertDecoder(routeAlertJson);
  let api = directionsApi(routeAlert.origin, routeAlert.destination);
  let request = {method: Get, path: api, body: None};

  networkBridge(request, onComplete);
};

// Reffect model

type state = {
  origin: option(string),
  destination: option(string),
  minutes: option(int),
  routeFetchAbility,
  dataLoadingState,
  routeDuration: option(int),
};

[@bs.deriving accessors]
type action =
  | SetOrigin(string)
  | SetDestination(string)
  | SetMinutes(int)
  | FetchRoute
  | FetchedRoute(int)
  | Noop;

let string_of_action = a => {
  switch (a) {
  | SetOrigin(o) => "SetOrigin(" ++ o ++ ")"
  | SetDestination(d) => "SetDetination(" ++ d ++ ")"
  | SetMinutes(m) => "SetMinutes(" ++ string_of_int(m) ++ ")"
  | FetchRoute => "FetchRoute"
  | FetchedRoute(d) => "FetchedRoute(" ++ string_of_int(d) ++ ")"
  | Noop => "Noop"
  };
};

type effect('a) =
  | CreateRouteAlert(string, string, int, int => 'a);

type respondFunc = Js.Json.t => unit;
let behaviorInterpreter =
    (networkBridge: (serverRequest, respondFunc) => unit, effect, dispatch) => {
  switch (effect) {
  | CreateRouteAlert(origin, destination, durationMinutes, actionCtor) =>
    // I don't feel that this provides a way to ensure that the endpoint URL is formed correctly
    let request = {
      method: Post,
      path: "/route_alerts",
      body: Some(routeAlertEncoder({origin, destination, durationMinutes})),
    };
    networkBridge(request, response => {
      googleDirectionsDecoder(response).routes->List.nth(0).legs
      ->List.nth(0).
        duration.
        value
      |> actionCtor
      |> dispatch
    })
    |> ignore;
  };
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

let reducer = (state: state, action) => {
  // Js.log("Processing action: " ++ string_of_action(action));
  let res =
    switch (action) {
    | SetOrigin(point) => ({...state, origin: Some(point)}, None)
    | SetDestination(dest) => ({...state, destination: Some(dest)}, None)
    | SetMinutes(minutes) => ({...state, minutes: Some(minutes)}, None)
    | FetchRoute => (
        {...state, dataLoadingState: Loading},
        Some(
          CreateRouteAlert(
            state.origin->getExn,
            state.destination->getExn,
            state.minutes->getExn,
            fetchedRoute,
          ),
        ),
      )
    | FetchedRoute(i) => ({...state, routeDuration: Some(i)}, None)
    | Noop => (state, None)
    };

  applyFetchAbility(res);
};

// View Helpers

let initialState = {
  origin: None,
  destination: None,
  minutes: None,
  routeFetchAbility: CannotFetch,
  dataLoadingState: NotLoading,
  routeDuration: None,
};

let canFetch = state =>
  switch (state.routeFetchAbility) {
  | CanFetch => true
  | CannotFetch => false
  };
