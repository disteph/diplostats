open Diplostats.Diplolib
   
let args = ref []
let description = "QE in Yices"

let options = [
  ("-verb", Arg.Int(fun i -> verbosity := i), "Verbosity level (default is 0)");
  ("-finished", Arg.Set finished, "Look at all finished games (default is false)");
  ("-nono_messaging", Arg.Clear no_messaging, "Look at games without no messaging");
  ("-norm_messaging", Arg.Set norm_messaging, "Look at games with regular messaging");
  ("-rule_messaging", Arg.Set rule_messaging, "Look at games with rulebook messaging");
  ("-pub_messaging", Arg.Set pub_messaging, "Look at games with rulebook messaging");
  ("-no-anonymity", Arg.Clear anonymity, "Look at games without anonymity (default is look at games with anonymity)");
  ("-novdiplo", Arg.Clear vdiplo, "Do not look at games on vdiplomacy.com");
  ("-nowebdiplo", Arg.Clear webdiplo, "Do not look at games on webdiplomacy.net");
];;

Arg.parse options (fun a->args := a::!args) description;;

match !args with
| [variant]        -> Lwt_main.run (parse variant) |> print_endline
| [older; variant] -> Lwt_main.run (parse ~older variant) |> print_endline
| [] -> failwith "Too few arguments in the command"
| _ -> failwith "Too many arguments in the command"
