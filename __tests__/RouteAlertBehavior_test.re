open Jest;
open Expect;
open Belt.List;
open RouteAlertBehavior;

let testNetworkBridge = (request, respond) => {
  switch (request.path) {
  | "/route_alerts" =>
    switch (request.body) {
    | Some(routeAlertJson) =>
      createRouteAlertEffectHandler(routeAlertJson)
      ->respond
    | None => errorResponseEncoder({message: "bad body"})->respond
    }

  | _ => errorResponseEncoder({message: "bad route"})->respond
  } |> ignore;
};

let testInterpreter = behaviorInterpreter(testNetworkBridge);

let reduceActions = actions => {
  let state = ref(initialState);
  actions->reduce(
    initialState,
    (_, action) => {
      Reffect.makeDispatch(
        state^,
        reducer,
        testInterpreter,
        s => state := s,
        action,
      );

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

describe("Route Alert Behavior", () => {
  test("preventing alert creation when all data is not present", () => {
    let finalState =
      reduceActions([SetOrigin("origin"), SetDestination("dest")]);

    expect(canFetch(finalState.routeFetchAbility)) |> toBe(false);
  });

  test("preventing alert creation when all data is present", () => {
    let finalState =
      reduceActions([
        SetOrigin("origin"),
        SetDestination("dest"),
        SetMinutes(5),
      ]);

    expect(canFetch(finalState.routeFetchAbility)) |> toBe(true);
  });

  test("calculating route duration", () => {
    let finalState =
      reduceActions([
        SetOrigin("origin"),
        SetDestination("dest"),
        SetMinutes(5),
        FetchRoute,
      ]);

    let passed =
      switch (finalState.routeDuration) {
      | Some(5) => true
      | _ => false
      };

    expect(passed) |> toBe(true);
  });
});
