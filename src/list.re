let rec all pred => fun
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

let filter pred list => {
  let rec doFilter acc => fun
  | [] => acc
  | [hd, ...tail] when (pred hd) =>
    doFilter [hd, ...acc] tail
  | [_hd, ...tail] =>
    doFilter acc tail;

  doFilter [] list;
};

let reject pred list => {
  let rec doReject acc => fun
  | [] => acc
  | [hd, ...tail] when not (pred hd) =>
    doReject [hd, ...acc] tail
  | [_hd, ...tail] =>
    doReject acc tail;

  doReject [] list;
};


let duplicate n x => {
  let rec doDuplicate acc => fun
  | n when n <= 0 => acc
  | n => doDuplicate [x, ...acc] (n - 1);

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