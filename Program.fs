open System
open RZ.Foundation.FSharp
open FSharp.IOMonad

let get_random(max: int) :IO<int> =
  io {
    let randomizer = Random()
    return randomizer.Next max
  }
  
let read_line() :IO<string> = io { return Console.ReadLine() }
let write_text(text: string) :IO<unit> = io { printf $"%s{text}" }
let writeln_text(text: string) :IO<unit> = io { printfn $"%s{text}" }
           
let play(target: int) (round: int) :IO<unit> =
  io {
    do! write_text($"#%d{round} Guess: ")
    let! guess = read_line().map(Int32.Parse)
    
    if guess = target then
      do! writeln_text($"Congrat, it's %d{guess}")
    else
      do! if guess < target
          then writeln_text("Too small")
          else writeln_text("Too big")
      yield Error(exn "Guess incorrect")
  }
  
let program =
  io {
    let! target = get_random 10
    let mutable round = 0
    do! IO.retry(io {
      round <- round + 1
      do! play target round
    })
  }

printfn "To run!"
program.run().unwrap()