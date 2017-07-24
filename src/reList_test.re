open Jest;
open Expect;

let _ =

describe "List.append" (fun () => {
  test "should append two lists" (fun () =>
    expect (ReList.append [1, 2, 3] [4, 5, 6]) |> toEqual [1, 2, 3, 4, 5, 6]);
});

describe "List.all" (fun () => {
  test "should return true when all are true" (fun () =>
    expect (ReList.all (fun x => x == 1) [1, 1, 1]) |> toBe true);
  test "should return false when some are false" (fun () =>
    expect (ReList.all (fun x => x == 1) [1, 2, 1]) |> toBe false);
});

describe "List.some" (fun () => {
  test "should return true when some are true" (fun () =>
    expect (ReList.some (fun x => x == 1) [1, 2, 1]) |> toBe true);
  test "should return false whesome are false" (fun () =>
    expect (ReList.some (fun x => x == 1) [2, 2, 3]) |> toBe false)
});

describe "List.reverse" (fun () => {
  test "should reverse a list" (fun () =>
    expect (ReList.reverse  [1, 2, 3]) |> toEqual [3, 2, 1]);
});

describe "List.filter" (fun () => {
  test "should filter a list by its predicate" (fun () =>
    expect (ReList.filter (fun x => x > 1)  [1, 2, 3, 4]) |> toEqual [2, 3, 4]);
});

describe "List.reject" (fun () => {
  test "should reject a list by its predicate" (fun () =>
    expect (ReList.reject (fun x => x == 1)  [1, 2, 3]) |> toEqual [2, 3]);
});

describe "List.duplicate" (fun () => {
  test "should duplicate an thing by n times into a list" (fun () =>
    expect (ReList.duplicate 3 "thing") |> toEqual ["thing", "thing", "thing"]);
});

describe "List.flatten" (fun () => {
  test "should flatten a list of lists into a list of items" (fun () =>
    expect (ReList.flatten [[1, 2], [3, 4]]) |> toEqual [1, 2, 3, 4]);
});

describe "List.map" (fun () => {
  test "should return a new list where each item is the function return of prev list's item" (fun () =>
    expect (ReList.map (fun x => x * 2) [1, 2, 3]) |> toEqual [2, 4, 6]);
});

describe "List.foldLeft" (fun () => {
  test "should reduce a list to a value from the left starting from the acc" (fun () =>
    expect (ReList.foldLeft (fun acc x => acc ^ x) "1" ["2", "3", "4"]) |> toEqual "1234");
});

describe "List.foldRight" (fun () => {
  test "should reduce a list to a value from the left starting from the right" (fun () =>
    expect (ReList.foldRight (fun acc x => acc ^ x) "0" ["1", "2", "3"]) |> toEqual "0321");
});

describe "List.wrap" (fun () => {
  test "should wrap a value into a list" (fun () =>
    expect (ReList.wrap 1) |> toEqual [1]);
});

describe "List.length" (fun () => {
  test "should return the length of a list" (fun () =>
    expect (ReList.length [1, 2, 3]) |> toEqual 3);
});

describe "List.sum" (fun () => {
  test "should return the sum of a list of integers" (fun () =>
    expect (ReList.sum [1, 2, 3]) |> toEqual 6);
});








