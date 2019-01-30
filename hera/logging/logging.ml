open Async
open Async_extended
module Http = Log.Make_global ()
module Module = Log.Make_global ()
module Main = Log.Make_global ()

let () =
  Main.set_output [Extended_log.Console.output ~debug:[`Dim] ~info:[`Bright] (Lazy.force Writer.stderr)];
  Module.set_output [Extended_log.Console.output ~debug:[`Dim] (Lazy.force Writer.stderr)];
  Http.set_output [Extended_log.Console.output ~debug:[`Dim] (Lazy.force Writer.stderr)];
  Http.set_level `Debug
;;
