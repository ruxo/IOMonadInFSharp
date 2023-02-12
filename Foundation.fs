module RZ.Foundation.FSharp

exception UnwrapError of obj
  
type Option<'a> with
  member inline my.isNone() :bool = my |> Option.isNone
  member inline my.unwrap() :'a =
    match my with
    | Some v -> v
    | None -> raise <| UnwrapError(exn $"Unwrap None value of type {typeof<'a>} option")
 
type Result<'a, 'err> with
  member inline my.map(f: 'a -> 'b) :Result<'b, 'err> = my |> Result.map f
  member inline my.bind(f: 'a -> Result<'b,'err>) :Result<'b, 'err> = my |> Result.bind f
  
  member my.unwrap() :'a =
    match my with
    | Ok v -> v
    | Error e -> raise <| UnwrapError(e)