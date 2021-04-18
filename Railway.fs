module Railway

type Result<'a, 'b> =
  | Success of 'a
  | Failure of 'b

let bind f = function
  | Success succ -> f succ
  | Failure fail -> Failure fail