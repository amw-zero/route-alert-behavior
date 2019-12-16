open Jest;
open Expect;
open Belt.List;
open RouteAlertBehavior;

let testInterpreter = (effect, dispatch) => {
  switch (effect) {
    | CalculateRoute(_, _, actionCtor) => dispatch(actionCtor(90))
  };
};

let reduceActions = (actions) => {
  let state = ref(initialState);
  actions->reduce(
    initialState,
    (_, action) => {
      Reffect.makeDispatch(state^, reducer, testInterpreter, s => state:= s)(action);

      state^
    }
  );
};

describe("Route Alert Behavior", () => {
  test("preventing alert creation when all data is not present", () => {
    let finalState = reduceActions([
      SetOrigin("origin"), 
      SetDestination("dest")
    ])

    let canFetch = switch (finalState.routeFetchAbility) {
      | CanFetch => true
      | CannotFetch => false
    };

    expect(canFetch) |> toBe(false);
  });

  test("preventing alert creation when all data is present", () => {
    let finalState = reduceActions([
      SetOrigin("origin"), 
      SetDestination("dest"), 
      SetMinutes(5)
    ]);

    let canFetch = switch(finalState.routeFetchAbility) {
      | CanFetch => true
      | CannotFetch => false
    };

    expect(canFetch) |> toBe(true);  
  });

  test("calculating route duration", () => {
    let finalState = reduceActions([
      SetOrigin("origin"), 
      SetDestination("dest"), 
      SetMinutes(5),
      FetchRoute
    ]);

    let passed = switch(finalState.routeDuration) {
      | Some(90) => true
      | _ => false
    };

    expect(passed) |> toBe(true);
  });
});