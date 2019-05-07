open Async
module Http = Log.Make_global ()
module Module = Log.Make_global ()
module Main = Log.Make_global ()

let () =
  Main.set_output [Log_extended.Console.output ~debug:[`Dim] ~info:[`Bright] (Lazy.force Writer.stderr)];
  Module.set_output [Log_extended.Console.output ~debug:[`Dim] (Lazy.force Writer.stderr)];
  Http.set_output [Log_extended.Console.output ~debug:[`Dim] (Lazy.force Writer.stderr)];
  Http.set_level `Debug
;;
