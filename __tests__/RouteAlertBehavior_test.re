open Jest;
open Expect;

describe("Expect", () => {
  test("toBe", () =>
    expect(1 + 2) |> toBe(3))

  test("toBe2", () =>
    expect(1 + 4) |> toBe(5))
});