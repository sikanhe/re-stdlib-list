let rec doReverse list acc =>
  switch list {
  | [] => []
  | [hd, ...tail] => doReverse tail [hd, ...acc]
  };
let reverse list => doReverse list [];

let rec doAppend list1 list2 =>
  switch list1 {
  | [] => list2
  | [hd, ...tail] => doAppend tail [hd, ...list2]
  };
let append list1 list2 =>
  doAppend (reverse list1) list2;

let rec filter f item list =>
  switch list {
  | [] => []
  | [hd, ...tail] when f(hd) =>
    [hd, ...(filter f item tail)]
  | [_hd, ...tail] =>
    filter f item tail
  };

let rec reject f item list =>
  switch list {
  | [] => []
  | [hd, ...tail] when f(hd) =>
    reject f item tail
  | [hd, ...tail] =>
    [hd, ...(reject f item tail)]
  };

let rec duplicate item count =>
  switch count {
  | n when n <= 0 => []
  | n => [item, ...duplicate item (n - 1)]
  };

let rec doFlatten list acc =>
  switch list {
  | [] =>
    acc
  | [[], ...tailOuter] =>
    doFlatten tailOuter acc
  | [[hd, ...tailInner], ...tailOuter] =>
    doFlatten [tailInner, ...tailOuter] [hd, ...acc]
  };

let rec map f list =>
  switch list {
  | [] => []
  | [hd, ...tail] => [(f hd), ...(map f tail)]
  };

let flatten list => doFlatten list [];

let rec foldl f acc list =>
  switch list {
  | [] => acc
  | [hd, ...tail] => foldl f (f acc hd) tail
  };

let foldr f acc list =>
  foldl f acc (reverse list);

let wrap = fun item => [item];

let rec doLength acc list =>
  switch list {
  | [] => acc
  | [_hd, ...tail] => doLength (acc + 1) tail
  };

let length list => doLength 0 list;

let rec insertAt index item list =>
  switch (list, index) {
  | (list, index) when index < 0 =>
    insertAt item ((length list) + index) list
  | ([], _index) =>
    [item]
  | (list, 0)  =>
    [item, ...list]
  | ([hd, ...tail], index) =>
    [hd, ...(insertAt item index tail)]
  };

let rec updateAt f index list =>
  switch (list, index) {
  | ([], _index) => []
  | (list, index) when index < 0 =>
    updateAt f ((length list) + index) list
  | ([hd, ...tail], 0) =>
    [(f hd), ...tail]
  | (list, index) => updateAt f (index - 1) list
  };

let rec popAt ::default=? list index =>
  switch (list, index) {
  | ([], _index) => (default, [])
  | (list, index) when index < 0 =>
    popAt list ((length list) + index) ::?default
  | ([hd, ...tail], 0) => (Some hd, tail)
  | (list, index) => popAt list (index - 1) ::?default
  };

let rec replaceAt newItem index list =>
  switch (list, index) {
  | ([], _index) => []
  | (list, index) when index < 0 =>
    replaceAt newItem ((length list) + index) list
  | ([_replaced, ...tail], 0) =>
    [newItem, ...tail]
  | (list, index) =>
    replaceAt newItem (index - 1) list
  };