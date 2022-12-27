open Containers
open Lwt
open Cohttp
open Cohttp_lwt_unix

open Lwt

let print _i fs = Format.(fprintf stderr) fs

let run url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  print 0 "@[Got something.@]@,%!";
  let code = resp |> Response.status |> Cohttp.Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Cohttp.Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let args = ref [];;

Arg.parse [] (fun a->args := a::!args) "";;

match !args with
| [url]        ->
   let body = Lwt_main.run (run url) in
   print_endline ("Received body\n" ^ body)
| [] -> failwith "Too few arguments in the command"
| _ -> failwith "Too many arguments in the command"

