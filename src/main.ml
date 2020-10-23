open Containers
open Cohttp
open Cohttp_lwt_unix

let verbosity = ref 0
let anonymity = ref true
let no_messaging = ref true
let norm_messaging = ref false
let rule_messaging = ref false
let pub_messaging = ref false
let finished = ref false

let bogus_games = ["Caucasia - Random" ]

let print i fs = Format.((if !verbosity >= i then fprintf else ifprintf) stderr) fs
               
let url variant page = 
  let base = "https://vdiplomacy.com/gamelistings.php?" in
  let options = [
      "gamelistType", Some "Search";
      "status", Some (if !finished then "Finished" else "Won");
      "userGames", Some "All";
      "seeJoinable", Some "All";
      "privacy", Some "All";
      "potType", Some "All";
      "drawVotes", Some "All";
      "variant", Some variant;
      "excusedTurns", Some "All";
      "anonymity", if !anonymity then Some "yes" else None;
      "phaseLengthMin", Some "All";
      "rrMin", Some "All";
      "rrMax", Some "All";
      "betMin", Some "";
      "betMax", Some "";
      "messageNon", (if !no_messaging then Some "Yes" else None);
      "messageNorm", (if !norm_messaging then Some "Yes" else None);
      "messageRule", (if !rule_messaging then Some "Yes" else None);
      "messagePub", (if !pub_messaging then Some "Yes" else None);
      "sortCol", Some "id";
      "sortType", Some "desc";
      "Submit", Some "Search";
      "pagenum", Some (string_of_int page)
    ]
  in
  let rec aux1 = function
    | [] -> []
    | (o,Some v)::tail -> (o^"="^v)::aux1 tail
    | (o,None)::tail -> aux1 tail
  in
  let rec aux2 = function
    | [] -> ""
    | [o] -> o
    | o::tail -> o^"&"^aux2 tail
  in
  base^(options |> aux1 |> aux2)
               
let get variant page =
  let url = url variant page in
  let open Lwt in
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  (* let code = resp |> Response.status |> Code.code_of_status in
   * Printf.printf "Response code: %d\n" code;
   * Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string); *)
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  (* Printf.printf "Body of length: %d\n" (String.length body); *)
  body

let get_name country =
  let open Soup in
  country $? ".memberCountryName" |> Option.map R.leaf_text

let get_centers_units country =
  let open Soup in
  match country $? ".memberSCCount" with
  | None -> None
  | Some block ->
     let is_em node =
       match element node with
       | Some node -> String.equal_caseless (name node) "em"
       | None -> false
     in 
     let below = descendants block |> to_list
                 |> List.filter is_em
                 |> List.map texts
     in
     match below with
     | [[centers]; [units]] ->
        (try
           Some(int_of_string centers, int_of_string units)
         with _ -> None)
     | _ -> None

type quant =
  | Survived of int*int
  | Defeated of int [@@deriving show { with_path = false }]

type result = {
    country : string;
    centers_units: quant
  } [@@deriving show { with_path = false }]

type record = {
    result : result;
    ranking : float ref [@printer fun fmt r -> fprintf fmt "%f" !r];
    victorious : bool
  } [@@deriving show { with_path = false }]

let compare_result { centers_units = cu1 } { centers_units = cu2 } =
  match cu1, cu2 with
  | Survived(c1,u1), Survived(c2,u2) -> Ord.pair Int.compare Int.compare (c1,u1) (c2,u2)
  | Survived _, Defeated _ -> 1
  | Defeated _, Survived _ -> -1
  | Defeated d1, Defeated d2 -> Ord.opp Int.compare d1 d2

module HT = Hashtbl.Make(String)

let (/) n d = float_of_int n /. float_of_int d

let game_name game =
  let open Soup in
  let r = game $ ".gameName" |> R.leaf_text in
  print 0 "@[%s@]@," r;
  r
  
let compute_stats l =
  let rec aux i rankings victories centers units = function
    | [] -> rankings /. float_of_int i, victories / i, centers / i, units / i
    | { ranking; result = { centers_units = Survived(c,u) }; victorious }::l ->
       let victories = if victorious then victories + 1 else victories in
       aux (i+1) (rankings +. !ranking) victories (centers + c) (units + u) l
    | { ranking; result = { centers_units = Defeated r }}::l ->
       aux (i+1) (rankings +. !ranking) victories (centers - r) (units - r) l
  in
  aux 0 0. 0 0 0 l

let close_ranking last_ranking index =
  let nb = float_of_int index -. !last_ranking in
  let h = (nb -. 1.) /. 2. in
  last_ranking := !last_ranking +. h

  
let compute_ranking last_record index result =
  let index = index+1 in
  match last_record with
  | None -> let ranking = ref 1. in
            let record = { ranking; victorious = true; result } in
            (Some record), record
  | Some r ->
     if compare_result r.result result = 0
     then last_record, { ranking = r.ranking; victorious = r.victorious; result }
     else
       begin
         close_ranking r.ranking index;
         let ranking = ref (float_of_int index) in
         let record = { ranking; victorious = false; result } in
         (Some record), record
       end
    
let parse ?older variant =
  print 0 "@[<v>@[<v>";
  let open Soup in
  let rec collect page games =
    print 0 "@[Collecting page %i@]%!@ " page;
    let parsed = Lwt_main.run (get variant page) |> parse in
    match parsed $$ ".gamePanel" |> to_list with
    | [] -> games
    | page_games -> collect (page+1) (List.rev_append page_games games)
  in
  print 0 "@]@,@[<v>";
  let games = collect 1 [] |> List.rev in
  let tbl = HT.create 10 in
  let per_country (i,d,l) member =
    match get_name member with
    | Some country ->
       let centers_units, d =
         match get_centers_units member with
         | Some(a,b) -> Survived(a,b), d
         | None -> Defeated d, d+1
       in
       let record = { country; centers_units }
       in
       (* print_endline (show_result record); *)
       i+1, d, (record::l)
    | None -> i, d, l
  in
  let per_game (older,nb) game =
    match older with
    | Some g ->
       (if String.equal (game_name game) g then None else older), nb
    | None ->
       if List.mem (game_name game) bogus_games then (None, nb)
       else
         let _, _, results = game $$ ".member" |> to_list |> List.fold_left per_country (1,1,[]) in
         let results = List.sort (Ord.opp compare_result) results in
         let last_record, results = List.fold_map_i compute_ranking None results in
         (match last_record with
          | Some r -> close_ranking r.ranking (List.length results + 1);
          | None -> assert false);
         print 1 "@[<v>%a@,@]@," (List.pp pp_record) results;
         let fill_up record =
           let result = record.result in
           let base = if HT.mem tbl result.country then HT.find tbl result.country else [] in
           HT.replace tbl result.country (record::base)
         in
         List.iter fill_up results;
         None, nb+1
  in
  let _, nb_games = List.fold_left per_game (older,0) games in
  let upto = match older with
    | Some g -> " before game "^g
    | None -> ""
  in
  print_endline (variant^"; "^string_of_int nb_games ^"; games played"^ upto ^"; "^ url variant 1);
  print_endline "Rémi; Country ; Average ranking; Victories; Average centers; Average units";
  let total = ref 0. in
  let aux country l =
    let ranking, victories, centers, units = compute_stats l in
    total := ranking +. !total;
    print_endline ("; "^ country
                   ^"; "^ string_of_float ranking
                   ^"; "^ string_of_float victories
                   ^"; "^ string_of_float centers
                   ^"; "^ string_of_float units)
  in
  HT.iter aux tbl;
  print 1 "@[<v>%f@,@]@," !total;
  print 0 "@]@]%!"

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
];;

Arg.parse options (fun a->args := a::!args) description;;

match !args with
| [variant]        -> parse variant
| [older; variant] -> parse ~older variant
| [] -> failwith "Too few arguments in the command"
| _ -> failwith "Too many arguments in the command"
