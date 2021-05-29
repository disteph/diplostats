open Containers
   
open Lwt.Infix
open Cohttp_lwt
open Cohttp_lwt_unix

open Diplostats.Diplolib

(* Apply the [Webmachine.Make] functor to the Async-based IO module exported by
 * cohttp. For added convenience, include the [Rd] module as well so you don't
 * have to go reaching into multiple modules to access request-related
 * information. *)
module Wm = struct
  module Rd = Webmachine.Rd
  module UnixClock = struct
    let now = fun () -> int_of_float (Unix.gettimeofday ())
  end
  include Webmachine.Make(Cohttp_lwt_unix__Io)(UnixClock)
end

(* Create a new class that inherits from [Wm.resource] and provides
 * implementations for its two virtual methods, and overrides some of its default methods.
 *)
class hello = object(self)
  inherit [Body.t] Wm.resource

  (* Only allow POST requests to this resource *)
  method allowed_methods rd =
    Wm.continue [`GET] rd

  (* Setup the resource to handle multiple content-types. Webmachine will
   * perform content negotiation as described in RFC 7231:
   *
   *   https://tools.ietf.org/html/rfc7231#section-5.3.2
   *
   * Content negotiation can be a complex process. Hoever for simple Accept
   * headers its fairly straightforward. Here's what content negotiation will
   * produce in some of these simple cases:
   *
   *     Accept             | Called method
   *   ---------------------+----------------
   *     "text/plain"       | self#to_text
   *     "text/html"        | self#to_html
   *     "text/*"           | self#to_html
   *     "application/json" | self#to_json
   *     "application/*"    | self#to_json
   *     "*/*"              | self#to_html
   *)
  method content_types_provided rd =
    Wm.continue [
      ("text/html"       , self#to_html);
      ("text/plain"      , self#to_text);
      ("application/json", self#to_json);
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd
    
  method private answer rd =
    let module SM = Map.Make(String) in
    let options = match Wm.Rd.lookup_path_info "options" rd with
      | Some v ->
         print_endline v;
         let l = String.split_on_char '&' v in
         let aux sofar option = match String.split_on_char '=' option with
           | key::tail ->
              let value = String.concat "=" tail in
              if not(String.is_empty value)
              then
                begin
                  print_endline ("Registering key "^key^" with value "^value);
                  SM.add key value sofar
                end
              else sofar 
           | [] -> sofar
         in
         List.fold_left aux SM.empty l
      | None -> SM.empty
    in
    print_endline "Finished constructing options";
    let treat key f = match SM.get key options with
      | Some v ->
         print_endline ("Setting option "^key^" to "^v);
         f v
      | None -> ()
    in
    match SM.get "variant" options with
    | None -> Lwt.return "Variant is missing"
    | Some variant ->
       print_endline("Variant is "^variant);
       treat "verb"           (fun v -> verbosity := int_of_string v);
       treat "finished"       (fun v -> finished := bool_of_string v);
       treat "nono_messaging" (fun v -> no_messaging := not(bool_of_string v));
       treat "norm_messaging" (fun v -> norm_messaging := bool_of_string v);
       treat "rule_messaging" (fun v -> rule_messaging := bool_of_string v);
       treat "pub_messaging"  (fun v -> pub_messaging := bool_of_string v);
       treat "no_anonymity"   (fun v -> anonymity := not(bool_of_string v));
       treat "novdiplo"       (fun v -> vdiplo := not(bool_of_string v));
       treat "nowebdiplo"     (fun v -> webdiplo := not(bool_of_string v));
       print_endline("Options processed");
       let older = SM.get "older" options in
       parse ?older variant >>= fun result ->
       reset();
       Lwt.return result

  (* Returns an html-based representation of the resource *)
  method private to_html rd =
    self#answer rd >>= fun answer ->
    let body = Printf.sprintf "<html><body>%s</body></html>" answer in
    Wm.continue (`String body) rd

  (* Returns a plaintext representation of the resource *)
  method private to_text rd =
    self#answer rd >>= fun answer ->
    let text = answer in
    Wm.continue (`String text) rd

  (* Returns a json representation of the resource *)
  method private to_json rd =
    self#answer rd >>= fun answer ->
    let json = Printf.sprintf "{\"output\" : \"%s\"}" answer in
    Wm.continue (`String json) rd

end

let main ~port =
  (* The route table. Both routes use the [hello] resource defined above.
   * However, the second one containes the [:what] wildcard in the path. The
   * value of that wildcard can be accessed in the resource by calling
   *
   *   [Wm.Rd.lookup_path_info "what" rd]
  *)
  let routes = [
    ("/:options", fun () -> new hello);
    ]
  in
  let callback (_ch,_conn) request body =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Wm.dispatch' routes ~body ~request
    >|= begin function
      | None        -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result
    end
    >>= fun (status, headers, body, path) ->
    (* If you'd like to see the path that the request took through the
     * decision diagram, then run this example with the [DEBUG_PATH]
     * environment variable set. This should suffice:
     *
     *  [$ DEBUG_PATH= ./hello_async.native]
     *
    *)
    let path =
      match Sys.getenv "DEBUG_PATH" with
      | _ -> Printf.sprintf " - %s" (String.concat ", " path)
      | exception Not_found   -> ""
    in
    Printf.eprintf "%d - %s %s%s"
      (Code.code_of_status status)
      (Code.string_of_method (Request.meth request))
      (Uri.path (Request.uri request))
      path;
    (* Finally, send the response to the client *)
    Server.respond ~headers ~body ~status ()
  in
  (* Create the server and handle requests with the function defined above. Try
   * it out with some of these curl commands:
   *
   *   [curl -H"Accept:text/html" "http://localhost:8080"]
   *   [curl -H"Accept:text/plain" "http://localhost:8080"]
   *   [curl -H"Accept:application/json" "http://localhost:8080"]
  *)
  let conn_closed (ch,_conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let config = Server.make ~callback ~conn_closed () in
  Printf.eprintf "hello_lwt: listening on localhost:%d\n%!" port;
  Server.create ~mode:(`TCP(`Port port)) config

let port = ref 8080

let args = ref []
let description = "Diplostats server"

let options = [
  ("-port", Arg.Int(fun i -> port := i), "port (default is 8080)");
];;

Arg.parse options (fun a->args := a::!args) description;;

match !args with
| [] -> Lwt_main.run (main ~port:!port)
| _  -> failwith "Too many arguments in the command"
