let rec all pred =>fun
| [] => true
| [hd, ...tail] => (pred hd) && (all pred tail);


let reverse list => {
  let rec doReverse acc => fun
  | [] => acc
  | [hd, ...tail] => doReverse [hd, ...acc] tail;

  doReverse [] list
};

let append l1 l2 => {
  let rec doAppend l1 l2 =>
    switch l1 {
    | [] => l2
    | [hd, ...tail] => doAppend tail [hd, ...l2]
    };

  doAppend (reverse l1) l2;
};

let rec filter pred => fun
| [] => []
| [hd, ...tail] when (pred hd) =>
  [hd, ...(filter pred tail)]
| [_hd, ...tail] =>
  filter pred tail;

let rec reject pred  => fun
| [] => []
| [hd, ...tail] when (pred hd) =>
  reject pred tail
| [hd, ...tail] =>
  [hd, ...(reject pred tail)];

let duplicate n x => {
  let rec doDuplicate acc n =>
    n <= 0
      ? acc
      : doDuplicate [x, ...acc] (n - 1);

  doDuplicate [] n;
};

let flatten list => {
  let rec doFlatten acc => fun
  | [] =>
    acc
  | [[], ...tailOuter] =>
    doFlatten acc tailOuter
  | [[hd, ...tailInner], ...tailOuter] =>
    doFlatten [hd, ...acc] [tailInner, ...tailOuter];

  doFlatten [] list;
};

let map f list => {
  let rec doMap acc list =>
    switch list {
      | [] => acc
      | [hd, ...tail] => doMap [(f hd), ...acc] tail
    };

  doMap [] list;
};

let rec foldLeft f acc => fun
| [] => acc
| [hd, ...tail] => foldLeft f (f acc hd) tail;

let foldRight f acc list =>
  foldLeft f acc (reverse list);

let wrap = fun item => [item];

let length list => {
  let rec doLength acc => fun
  | [] => acc
  | [_hd, ...tail] => doLength (acc + 1) tail;

  doLength 0 list;
};

let sum list => {
  let rec doSum acc => fun
  | [] => acc
  | [hd, ...tail] => doSum (hd + acc) tail;

  doSum 0 list
};