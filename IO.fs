module FSharp.IOMonad
#nowarn "46" // no warning for using reserved word "pure"

open System.Runtime.CompilerServices
open RZ.Foundation.FSharp

type IOError<'T> = Result<'T, exn>
type IO<'T> = unit -> IOError<'T>
  
module IO =
  let inline pure (x: 'a) :IO<'a> = fun () -> Ok x
  let inline map ([<InlineIfLambda>] f: 'a -> 'b) ([<InlineIfLambda>] x: IO<'a>) = fun() -> x().map(f)
  let run (x: IO<'a>) :IOError<'a> =
    try
      x()
    with
    | e -> Error e
 
  let inline bind ([<InlineIfLambda>] f: 'a -> IO<'b>) ([<InlineIfLambda>] m: IO<'a>) :IO<'b> =
    fun() -> m().bind(fun x -> (f x)())
    
  let inline retry([<InlineIfLambda>] ma: IO<'a>) :IO<'a> =
    fun() -> let mutable result = None
             while result.isNone() do
               let r = ma()
               match r with
               | Ok _ -> result <- Some r
               | Error _ -> ()
             result.unwrap()
 
  type IOBuilder() =
    member inline _.Return(x: 'a) :IO<'a> = pure x
    member inline _.ReturnFrom([<InlineIfLambda>] x: IO<'a>) :IO<'a> = x
    member inline _.Bind([<InlineIfLambda>] m: IO<'a>, [<InlineIfLambda>] f: 'a -> IO<'b>) :IO<'b> = bind f m
    member inline _.Yield(r: IOError<'a>) :IO<'a> = fun() -> r
    member inline _.Zero() :IO<unit> = pure ()
    
    member inline _.Delay([<InlineIfLambda>] w: unit -> IO<'a>) :IO<'a> = fun() -> w()()
 
let io = IO.IOBuilder()

[<Extension>]
type IOExtension() =
  [<Extension>] static member inline map([<InlineIfLambda>] my,[<InlineIfLambda>] f) = my |> IO.map f
  [<Extension>] static member inline run([<InlineIfLambda>] my) = IO.run my