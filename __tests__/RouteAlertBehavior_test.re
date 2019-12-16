open Jest;
open Expect;
open Belt.List;
open RouteAlertBehavior;

let testInterpreter = behaviorInterpreter((api, respond) => {
  respond({ duration: 90 });
});

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

let canFetch = state => {
  switch (state) {
    | CanFetch => true
    | CannotFetch => false
  };
}

describe("Route Alert Behavior", () => {
  test("preventing alert creation when all data is not present", () => {
    let finalState = reduceActions([
      SetOrigin("origin"), 
      SetDestination("dest")
    ])

    expect(canFetch(finalState.routeFetchAbility)) |> toBe(false);
  });

  test("preventing alert creation when all data is present", () => {
    let finalState = reduceActions([
      SetOrigin("origin"), 
      SetDestination("dest"), 
      SetMinutes(5)
    ]);

    expect(canFetch(finalState.routeFetchAbility)) |> toBe(true);  
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