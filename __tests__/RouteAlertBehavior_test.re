open Jest;
open Expect;
open Belt.List;
open RouteAlertBehavior;

let googleDurationEncoder = googleDuration =>
  Json.Encode.(object_([("value", googleDuration.value |> int)]));

let googleLegEncoder = googleLeg =>
  Json.Encode.(
    object_([
      ("duration_in_traffic", googleLeg.duration |> googleDurationEncoder),
    ])
  );

let googleRouteEncoder = googleRoute =>
  Json.Encode.(
    object_([("legs", googleRoute.legs |> list(googleLegEncoder))])
  );

let googleDirectionsEncoder = googleDirections =>
  Json.Encode.(
    object_([
      ("routes", googleDirections.routes |> list(googleRouteEncoder)),
    ])
  );

let serverNetworkBridge = (request: serverRequest, respond) => {
  let pathString = Js.String.make(request.path);
  if (Js.String.includes("maps.googleapis.com", pathString)) {
    let googleDirections = {routes: [{legs: [{
                                               duration: {
                                                 value: 6,
                                               },
                                             }]}]};
    respond(googleDirectionsEncoder(googleDirections));
  };
};

// Need the ability for the network bridge to respond with multiple response types

let clientNetworkBridge = (request: serverRequest, respond) => {
  switch (request.path) {
  | "/route_alerts" =>
    switch (request.body) {
    | Some(json) =>
      createRouteAlertEffectHandler(
        json, serverNetworkBridge, googleDirections =>
        respond(googleDirections)
      )
    | None => errorResponseEncoder({message: "bad body"})->respond
    }
  | _ => errorResponseEncoder({message: "bad route"})->respond
  };
};

let testEnv = {networkBridge: clientNetworkBridge};

let reduceActions = actions => {
  let state = ref(initialState);

  reduce(
    actions,
    initialState,
    (_, action) => {
      Reffect.makeDispatch(state^, reducer, testEnv, s => state := s, action);

      state^;
    },
  );
};

let canFetch = state => {
  switch (state) {
  | CanFetch => true
  | CannotFetch => false
  };
};

let createRouteAlert = (~minutes=5, ~origin="origin", ~dest="dest", ()) => [
  SetOrigin(origin),
  SetDestination(dest),
  SetMinutes(minutes),
];

describe("Route Alert Behavior", () => {
  test("preventing alert creation when all data is not present", () => {
    let finalState =
      reduceActions([SetOrigin("origin"), SetDestination("dest")]);

    expect(canFetch(finalState.routeFetchAbility)) |> toBe(false);
  });

  test("preventing alert creation when all data is present", () => {
    let finalState = reduceActions(createRouteAlert());

    expect(canFetch(finalState.routeFetchAbility)) |> toBe(true);
  });

  test("calculating route duration when calculation is successful", () => {
    let finalState =
      reduceActions(List.concat([createRouteAlert(), [FetchRoute]]));

    let passed =
      fun
      | Some(6) => true
      | _ => false;

    expect(passed(finalState.routeDuration)) |> toBe(true);
  });

  test(
    "creating link to route in Google maps when there are no spaces in the stops",
    () => {
    let finalState =
      reduceActions([SetOrigin("origin"), SetDestination("dest")]);

    expect(finalState.routeLinkGoogle)
    |> toBe(Some("https://google.com/maps/dir/origin/dest"));
  });

  test("creating link to route in Google maps when the stops have spaces", () => {
    let finalState =
      reduceActions([SetOrigin("new york"), SetDestination("new jersey")]);

    expect(finalState.routeLinkGoogle)
    |> toBe(Some("https://google.com/maps/dir/new+york/new+jersey"));
  });
});
