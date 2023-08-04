open Containers
open Cohttp_lwt_unix

let verbosity = ref 0
let anonymity = ref true
let no_messaging = ref true
let norm_messaging = ref false
let rule_messaging = ref false
let pub_messaging = ref false
let finished = ref false

let vdiplo   = ref true
let webdiplo = ref true

let reset () =
  verbosity := 0;
  anonymity := true;
  no_messaging := true;
  norm_messaging := false;
  rule_messaging := false;
  pub_messaging := false;
  finished := false
  
let bogus_games = ["Caucasia - Random" ]

let print i fs = Format.((if !verbosity >= i then fprintf else ifprintf) stderr) fs
(* let print_stdout s = Format.(fprintf stdout "%s@," s) *)
             
type base = VDiplo | WebDiplo [@@deriving show]

let url base variant page = 
  let base = match base with
    | VDiplo -> "https://vdiplomacy.com/gamelistings.php?"
    | WebDiplo -> "https://webdiplomacy.net/gamelistings.php?"
  in
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
    | (_o,None)::tail -> aux1 tail
  in
  let rec aux2 = function
    | [] -> ""
    | [o] -> o
    | o::tail -> o^"&"^aux2 tail
  in
  base^(options |> aux1 |> aux2)
               
let get base variant page =
  let url = url base variant page in
  let open Lwt in
  Client.get (Uri.of_string url) >>= fun (_resp, body) ->
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
    victorious : float ref [@printer fun fmt r -> fprintf fmt "%f" !r];
  } [@@deriving show { with_path = false }]

let compare_result { centers_units = cu1; _ } { centers_units = cu2; _ } =
  match cu1, cu2 with
  (* | Survived(c1,u1), Survived(c2,u2) -> Ord.pair Int.compare Int.compare (c1,u1) (c2,u2) *)
  | Survived(c1,_), Survived(c2,_) -> Int.compare c1 c2
  | Survived _, Defeated _ -> 1
  | Defeated _, Survived _ -> -1
  | Defeated _d1, Defeated _d2 -> 0
  (* | Defeated d1, Defeated d2 -> Ord.opp Int.compare d1 d2 *)

module HT = Hashtbl.Make(String)

let (/) n d = float_of_int n /. float_of_int d

let game_name game =
  let open Soup in
  game $ ".gameName" |> R.leaf_text

let game_duration game =
  let open Soup in
  let txt  = game $ ".gameDate" |> R.leaf_text in
  let year = String.(sub txt (length txt - 4) 4) |> int_of_string in
  year
  (* print 0 "@[YEAR: %i@]@,%!" year; *)
  (* let spring = String.(equal (sub txt 0 6) "Spring") in *)
  (* 2*(year - 1901 + 1) + if spring then 0 else 1 *)

let compute_stats l =
  let rec aux i rankings victories centers units = function
    | [] -> rankings /. float_of_int i, victories /. float_of_int  i, centers / i, units / i
    | { ranking; result = { centers_units = Survived(c,u); _ }; victorious }::l ->
       let victories = victories +. !victorious in
       aux (i+1) (rankings +. !ranking) victories (centers + c) (units + u) l
    | { ranking; result = { centers_units = Defeated _r ; _ } ; _}::l ->
       aux (i+1) (rankings +. !ranking) victories centers units l
  in
  aux 0 0. 0. 0 0 l

let close_ranking last_ranking index =
  let nb = float_of_int index -. !last_ranking in
  let h = (nb -. 1.) /. 2. in
  last_ranking := !last_ranking +. h

  
let compute_ranking last_record index result =
  let index = index+1 in
  match last_record with
  | None -> let ranking = ref 1. in
            let victorious = ref 1. in
            let record = { ranking; victorious; result } in
            (Some record), record
  | Some r ->
     if compare_result r.result result = 0
     then last_record, { ranking = r.ranking; victorious = r.victorious; result }
     else
       begin
         close_ranking r.ranking index;
         r.victorious := !(r.victorious) /. float_of_int (index - 1);
         let ranking = ref (float_of_int index) in
         let victorious = ref 0. in
         let record = { ranking; victorious; result } in
         (Some record), record
       end
    
let parse ?(html=true) ?older variant =
  Format.(fprintf stdout "@[<v>");
  let (let*) = Lwt.bind in
  let rec collect base page games =
    print 0 "@[Collecting page %i from %a@]@,%!" page pp_base base;
    let* p = get base variant page in
    let parsed = Soup.parse p in
    match Soup.(parsed $$ ".gamePanel" |> to_list) with
    | [] -> Lwt.return games
    | page_games -> collect base (page+1) (List.rev_append page_games games)
  in
  print 0 "\n%!";
  let* webgames = if !webdiplo then collect WebDiplo 1 [] else Lwt.return [] in
  let* vgames   = if !vdiplo   then collect VDiplo 1 [] else Lwt.return [] in
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
       (* print_stdout (show_result record); *)
       i+1, d, (record::l)
    | None -> i, d, l
  in
  let per_game older ((stop,nb,duration_sum) as sofar) game =
    if stop then sofar
    else
      try
        let name = game_name game in
        let duration = game_duration game in
        print 0 "@[%s - duration %i@]@,%!" name duration;
        match older with
        | Some g when String.equal name g -> true, nb, duration_sum
        | _ ->
           Option.iter (print 0 "@[not the same as %s@]@,%!") older;
           if List.mem name bogus_games then sofar
           else
             let _, _, results = Soup.(game $$ ".member" |> to_list) |> List.fold_left per_country (1,1,[]) in
             let results = List.sort (Ord.opp compare_result) results in
             let last_record, results = List.fold_map_i compute_ranking None results in
             (match last_record with
              | Some r ->
                 close_ranking r.ranking (List.length results + 1);
                 r.victorious := !(r.victorious) /. float_of_int (List.length results)
              | None -> assert false);
             print 1 "@[<v>%a@,@]@,%!" (List.pp pp_record) results;
             let fill_up record =
               let result = record.result in
               let base = if HT.mem tbl result.country then HT.find tbl result.country else [] in
               HT.replace tbl result.country (record::base)
             in
             List.iter fill_up results;
             false, nb+1, duration_sum+duration
      with _ -> sofar
  in
  let sofar = List.fold_left (per_game older) (false,0,0) webgames in
  let _, nb_games, duration = List.fold_left (per_game older) sofar vgames in
  print 0 "@[Duration moyenne %f@]@," (duration/nb_games);
  let upto = match older with
    | Some g -> "games played before game "^g
    | None -> "games played"
  in
  let wrap_cell arg fmt =
    if html then Format.fprintf fmt "<td>%t</td>" arg
    else Format.fprintf fmt "%t;" arg
  in
  let wrap_line arg fmt =
    if html then Format.fprintf fmt "<tr>%t</tr>@," arg
    else Format.fprintf fmt "%t@," arg
  in
  let string fmt s = wrap_cell(fun fmt -> Format.fprintf fmt "%s" s) fmt in
  let int fmt i    = wrap_cell(fun fmt -> Format.fprintf fmt "%i" i) fmt in
  let float fmt f  = wrap_cell(fun fmt -> Format.fprintf fmt "%f" f) fmt in
  let pp fmt =
    wrap_line (fun fmt ->
        Format.fprintf fmt "%a %a %a %a %a %a"
          string "Rémi"
          string "Country"
          string "Average ranking"
          string "Victories"
          string "Average centers"
          string "Average units") fmt;
    let total = ref 0. in
    let aux country l =
      let ranking, victories, centers, units = compute_stats l in
      total := ranking +. !total;
      wrap_line (fun fmt ->
          Format.fprintf fmt "%a %a %a %a %a %a"
            string ""
            string country
            float ranking
            float victories
            float centers
            float units) fmt;
    in
    HT.iter aux tbl;
    wrap_line (fun fmt ->
        Format.fprintf fmt "%a %a %a %a %a %a %a %a %a %a"
          string ""
          string ""
          string ""
          string ""
          string ""
          string ""
          string variant
          int nb_games
          string upto
          string (url VDiplo variant 1)) fmt;
    wrap_line (fun fmt ->
        Format.fprintf fmt "%a %a %a %a %a %a %a %a"
          string ""
          string ""
          string ""
          string ""
          string ""
          string ""
          string "Fin de partie moyenne"
          float (duration/nb_games)) fmt;
    print 1 "@[<v>%f@,@]@,%!" !total
  in
  Lwt.return(
      if html
      then Format.sprintf
             "@[<v><meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\" />@,<table border=1>@,%t@,</table>@,@]%!" pp
      else Format.sprintf "@[<v>%t@]%!" pp
  ) 
