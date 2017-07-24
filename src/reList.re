let rec all pred =>
  fun
  | [] => true
  | [hd, ...tail] => (pred hd) && (all pred tail);

let rec some pred =>
  fun
  | [] => false
  | [hd, ...tail] => (pred hd) || (some pred tail);

let reverse list => {
  let rec aux acc =>
    fun
    | [] => acc
    | [hd, ...tail] => aux [hd, ...acc] tail;

  aux [] list
};

let append l1 l2 => {
  let rec aux l1 l2 =>
    switch l1 {
    | [] => l2
    | [hd, ...tail] => aux tail [hd, ...l2]
    };

  aux (reverse l1) l2;
};

let rec each f =>
  fun
  | [] => ()
  | [hd, ...tail] => {f hd; each f tail};

let rec find pred =>
  fun
  | [] => None
  | [hd, ..._tail] when pred hd => Some hd
  | [_hd, ...tail] => find pred tail ;

let rec filter pred =>
  fun
  | [] => []
  | [hd, ...tail] when (pred hd) =>
    [hd, ...(filter pred tail)]
  | [_hd, ...tail] =>
    filter pred tail;

let rec reject pred =>
  fun
  | [] => []
  | [hd, ...tail] when (pred hd) =>
    reject pred tail
  | [hd, ...tail] =>
    [hd, ...(reject pred tail)];

let rec duplicate n x =>
  switch n {
    | n when n <= 0 => []
    | n => [x, ...(duplicate (n - 1) x)];
  };

let flatten list => {
  let rec aux acc =>
    fun
    | [] =>
      acc
    | [[], ...tailOuter] =>
      aux acc tailOuter
    | [[hd, ...tailInner], ...tailOuter] =>
      aux [hd, ...acc] [tailInner, ...tailOuter];

  reverse(aux [] list);
};

let rec map f =>
  fun
  | [] => []
  | [hd, ...tail] => [(f hd), ...(map f tail)];

let rec foldLeft f acc =>
  fun
  | [] => acc
  | [hd, ...tail] => foldLeft f (f acc hd) tail;

let foldRight f acc list =>
  foldLeft f acc (reverse list);

let wrap = fun item => [item];

let length list => {
  let rec aux acc =>
    fun
    | [] => acc
    | [_hd, ...tail] => aux (acc + 1) tail;

  aux 0 list;
};

let sum list => {
  let rec aux acc =>
    fun
    | [] => acc
    | [hd, ...tail] => aux (hd + acc) tail;

  aux 0 list
};