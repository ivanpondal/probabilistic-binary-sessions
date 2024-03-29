open Ast

let need_parentheses ctor s = priority_of_constructor ctor >= priority_of_type s

let string_of_polarity = function
  | `Receive -> "?"
  | `Send -> "!"
  | `Branch -> "&"
  | `Choice -> "⊕"
  | `AcceptSequence when !Configuration.sequence_polarity -> "&"
  | `SelectSequence when !Configuration.sequence_polarity -> "⊕"
  | `AcceptSequence | `SelectSequence -> ""

let all_vars t0 =
  let tvars = ref [] in
  let rvars = ref [] in
  let stvars = ref [] in
  let add x xs = if not (List.mem x !xs) then xs := x :: !xs in
  let add_session_type_var x y =
    if (not (List.mem (x, y) !stvars)) && not (List.mem (y, x) !stvars) then
      stvars := (x, y) :: !stvars
  in
  let rec aux = function
    | Var x -> add x tvars
    | RecVar x -> add x rvars
    | SessionTypeVar (x, y) -> add_session_type_var x y
    | Rec (x, t) ->
        add x rvars;
        aux t
    | Constructor (_, ts) -> List.iter aux ts
    | Tagged (_, tags) -> List.iter aux_tag tags
  and aux_tag (_, t) = aux t in
  aux t0;
  (List.rev !tvars, List.rev !rvars, List.rev !stvars)

let occurs x =
  let rec aux = function
    | Var y -> x = y
    | RecVar y -> x = y
    | SessionTypeVar _ -> false
    | Rec (y, _) when x = y -> false
    | Rec (_, t) -> aux t
    | Constructor (_, ts) -> List.exists aux ts
    | Tagged (_, tags) -> List.exists aux_tag tags
  and aux_tag (_, t) = aux t in
  aux

let round_float_2_dec f = Float.round (f *. 100.) /. 100.

let convex_sum p q r = (p *. q) +. ((1. -. p) *. r)

let pp_convex_sum p q r =
  let pp_prob_var p =
    match p with
    | Constructor (`Prob p, []) -> string_of_float (round_float_2_dec p)
    | Var p -> p
    | _ -> raise (Invalid_argument "Was expecting prob or var type")
  in
  match (p, q, r) with
  | ( Constructor (`Prob p, []),
      Constructor (`Prob q, []),
      Constructor (`Prob r, []) ) ->
      string_of_float (round_float_2_dec (convex_sum p q r))
  | _ ->
      let pp_p = pp_prob_var p
      and pp_q = pp_prob_var q
      and pp_r = pp_prob_var r in
      "conv_sum(" ^ pp_p ^ ", " ^ pp_q ^ ", " ^ pp_r ^ ")"

let pp t0 =
  let rec aux = function
    | Var x -> Format.print_as 1 x
    | RecVar x -> Format.print_as 1 x
    | SessionTypeVar (x, y) -> Format.print_string ("\"" ^ x ^ "'" ^ y)
    | Rec (x, t) ->
        Format.open_box 0;
        Format.print_string ("rec " ^ x ^ ".");
        Format.print_cut ();
        aux t;
        Format.close_box ()
    | Constructor (`Arrow, [ t; s ]) ->
        Format.open_hovbox 0;
        aux_priority `Arrow t;
        Format.print_as 2 " →";
        Format.print_space ();
        aux s;
        Format.close_box ()
    | Constructor (`Tuple, []) -> assert false (* impossible *)
    | Constructor (`Tuple, t :: ts) ->
        Format.open_hvbox 0;
        aux t;
        List.iter
          (fun s ->
            Format.print_as 2 " ×";
            Format.print_break 1 1;
            aux s)
          ts;
        Format.close_box ()
    | Constructor (`Apply sl, []) -> Format.print_string (aux_name sl)
    | Constructor (`Apply sl, [ t ]) ->
        Format.open_hovbox 0;
        aux t;
        Format.print_space ();
        Format.print_string (aux_name sl);
        Format.close_box ()
    | Constructor (`Apply sl, t :: ts) ->
        Format.open_hovbox 1;
        Format.print_string "(";
        aux t;
        List.iter
          (fun s ->
            Format.print_string ",";
            Format.print_space ();
            aux s)
          ts;
        Format.print_string ")";
        Format.print_space ();
        Format.print_string (aux_name sl);
        Format.close_box ()
    | Constructor (`Empty, []) -> Format.print_string "0"
    | Constructor (`Channel, [ t; s ]) ->
        Format.open_hovbox 1;
        Format.print_string "<";
        aux t;
        Format.print_string ",";
        Format.print_space ();
        aux s;
        Format.print_string ">";
        Format.close_box ()
    | Constructor (`ClosedChannel, [ t ]) ->
        Format.open_hovbox 1;
        Format.print_string "<";
        aux t;
        Format.print_string ">";
        Format.close_box ()
    | Constructor (`End, []) -> Format.print_string "idle"
    | Constructor (`Done, []) -> Format.print_string "done"
    | Constructor (`Nat n, []) -> Format.print_string (string_of_int n ^ " nat")
    | Constructor (`Frac f, []) ->
        Format.print_string (string_of_float (round_float_2_dec f) ^ " frac")
    | Constructor (`Prob p, []) ->
        Format.print_string (string_of_float (round_float_2_dec p))
    | Constructor (`ConvexSum, [ p; q; r ]) ->
        Format.print_string (pp_convex_sum p q r)
    | Constructor (((`Send | `Receive) as pol), [ t; ct ]) ->
        Format.open_hvbox 0;
        Format.print_as 1 (string_of_polarity pol);
        aux_priority pol t;
        if !Configuration.show_end || ct <> t_End then (
          Format.print_string ".";
          Format.print_break 0 0;
          aux ct);
        Format.close_box ()
    | Tagged (`Variant, tags) -> aux_tags tags
    | Tagged (((`Choice | `Branch) as pol), tags) -> aux_tags_prob pol tags
    | Constructor (((`SelectSequence | `AcceptSequence) as pol), [ t; s ]) ->
        Format.open_hovbox 0;
        Format.print_as 1 (string_of_polarity pol);
        Format.print_string "{";
        aux t;
        Format.print_string "}";
        if !Configuration.show_end || s <> t_End then (
          Format.print_string ".";
          Format.print_break 0 0;
          aux s);
        Format.close_box ()
    | Constructor (`As _, _) -> assert false
    | Constructor _ -> assert false
  (* impossible? *)
  and aux_priority ctor t =
    if need_parentheses ctor t then aux_protected t else aux t
  and aux_protected t =
    Format.open_box 1;
    Format.print_string "(";
    aux t;
    Format.print_string ")";
    Format.close_box ()
  and aux_name sl = String.concat "." sl
  and aux_tags = function
    | [] -> Format.print_string "[ ]"
    | tag :: tags ->
        Format.open_hvbox 0;
        Format.print_string "[ ";
        aux_tag tag;
        List.iter
          (fun tag ->
            Format.print_break 1 0;
            Format.print_string "| ";
            aux_tag tag)
          tags;
        Format.print_string " ]";
        Format.close_box ()
  and aux_tags_prob pol = function
    | [] -> Format.print_string "[ ]"
    | prob :: tag :: tags ->
        Format.open_hvbox 0;
        aux_tag_prob prob;
        Format.print_as 1 (string_of_polarity pol);
        Format.print_string "[ ";
        aux_tag tag;
        List.iter
          (fun tag ->
            Format.print_break 1 0;
            Format.print_string "| ";
            aux_tag tag)
          tags;
        Format.print_string " ]";
        Format.close_box ()
    | _ -> assert false
  and aux_tag_prob (_, t) =
    Format.open_hvbox 0;
    aux t;
    Format.close_box ()
  and aux_tag (name, t) =
    Format.open_hvbox 0;
    Format.print_string (name ^ ":");
    Format.print_break 1 2;
    aux t;
    Format.close_box ()
  in
  aux t0

let unfold s x =
  let rec aux = function
    | RecVar y when x = y -> s
    | (Var _ | RecVar _ | SessionTypeVar _) as t -> t
    | Rec (y, _) as t when x = y -> t
    | Rec (y, t) -> Rec (y, aux t)
    | Tagged (ctor, tags) -> Tagged (ctor, List.map aux_tag tags)
    | Constructor (ctor, ts) -> Constructor (ctor, List.map aux ts)
  and aux_tag (tag, t) = (tag, aux t) in
  aux

let rec find = function Rec (x, s) as t -> find (unfold t x s) | t -> t

let eq =
  let rec aux env t s =
    if List.mem (t, s) env then true
    else
      let env = (t, s) :: env in
      match (find t, find s) with
      | Var x, Var y -> x = y
      | Tagged (ctor1, tags1), Tagged (ctor2, tags2) ->
          ctor1 = ctor2 && aux_tags env tags1 tags2
      | SessionTypeVar (x1, x2), SessionTypeVar (y1, y2) -> x1 = y1 && x2 = y2
      | Constructor (ctor1, ts), Constructor (ctor2, ss) ->
          ctor1 = ctor2 && List.for_all2 (aux env) ts ss
      | Rec _, _ | _, Rec _ | RecVar _, _ | _, RecVar _ ->
          assert false (* impossible *)
      | _ -> false
  and aux_tags env tm sm =
    List.length tm = List.length sm
    && List.for_all
         (fun (tag, t') ->
           try aux env t' (List.assoc tag sm) with Not_found -> false)
         tm
  in
  aux []

let rec parse_nat n =
  let t_suc = List.append Configuration.get_natural_prefix [ "suc" ]
  and t_zero = List.append Configuration.get_natural_prefix [ "zero" ] in
  match n with
  | Constructor (`Apply cs, [ suc ]) when cs = t_suc -> parse_nat suc + 1
  | Constructor (`Apply cs, []) when cs = t_zero -> 0
  | _ -> raise (Invalid_argument "Was expecting only natural types")

let parse_frac f =
  match f with
  | Constructor (`Tuple, [ Constructor (_, [ n ]); Constructor (_, [ d ]) ]) ->
      float_of_int (parse_nat n) /. float_of_int (parse_nat d)
  | _ -> raise (Invalid_argument "Was expecting only fraction types")

let parse_prob n d = float_of_int (parse_nat n) /. float_of_int (parse_nat d + 1)

let phase_one t0 =
  let ( ++ ) = List.append in
  let t_0 = Configuration.get_prefix () ++ [ "_0" ]
  and t_1 = Configuration.get_prefix () ++ [ "_1" ]
  and t_p0 = Configuration.get_prefix () ++ [ "_p_0" ]
  and t_p1 = Configuration.get_prefix () ++ [ "_p_1" ]
  and t_st = Configuration.get_prefix () ++ [ !Configuration.session_type ]
  and t_cst = Configuration.get_prefix () ++ [ "cpst" ]
  and t_it = Configuration.get_prefix () ++ [ "it" ]
  and t_ot = Configuration.get_prefix () ++ [ "ot" ]
  and t_et = Configuration.get_prefix () ++ [ "et" ]
  and t_seq = Configuration.get_prefix () ++ [ "seq" ]
  and t_pchoice = Configuration.get_prefix () ++ [ "pchoice" ]
  and t_prob = Configuration.get_prefix () ++ [ "prob" ]
  and t_conv_sum = Configuration.get_prefix () ++ [ "conv_sum" ]
  and t_math_nat = Configuration.get_natural_prefix ++ [ "nat" ]
  and t_math_frac = Configuration.get_rational_prefix ++ [ "frac" ] in
  let rec aux = function
    | (Var _ | RecVar _) as t -> t
    | Constructor (`Apply cs, []) when cs = t_0 -> t_Empty
    | Constructor (`Apply cs, []) when cs = t_1 -> t_Done
    | Constructor (`Apply cs, []) when cs = t_p0 -> t_Prob 0.0
    | Constructor (`Apply cs, []) when cs = t_p1 -> t_Prob 1.0
    | Constructor (`Apply cs, [ p; q; r ]) when cs = t_conv_sum ->
        t_ConvexSum (aux p) (aux q) (aux r)
    | Constructor (`Apply cs, [ nat ]) when cs = t_math_nat ->
        t_Nat (parse_nat nat)
    | Constructor (`Apply cs, [ frac ]) when cs = t_math_frac ->
        t_Frac (parse_frac frac)
    | Constructor (`Apply cs, [ it; ot ]) when cs = t_cst ->
        Constructor
          (`ClosedChannel, [ Constructor (`Channel, [ aux it; aux ot ]) ])
    | Constructor (`Apply cs, [ it; ot ]) when cs = t_st ->
        Constructor (`Channel, [ aux it; aux ot ])
    | Constructor (`Apply cs, [ it ]) when cs = t_it ->
        Constructor (`Channel, [ aux it; t_Empty ])
    | Constructor (`Apply cs, [ ot ]) when cs = t_ot ->
        Constructor (`Channel, [ t_Empty; aux ot ])
    | Constructor (`Apply cs, []) when cs = t_et ->
        Constructor (`Channel, [ t_Empty; t_Empty ])
    | Constructor (`Apply cs, [ t; s ]) when cs = t_seq ->
        Constructor (`Sequence, [ aux t; aux s ])
    | Constructor (`Apply cs, [ n; d ]) when cs = t_prob ->
        t_Prob (parse_prob n d)
    | Constructor (`Apply cs, [ t; f; p ]) when cs = t_pchoice ->
        Tagged (`Variant, [ ("Prob", aux p); ("True", aux t); ("False", aux f) ])
    | Tagged (`Variant, tags) -> Tagged (`Variant, List.map aux_tag tags)
    | Constructor (ctor, ts) -> Constructor (ctor, List.map aux ts)
    | Rec (x, t) -> Rec (x, aux t)
    | t ->
        print_endline "the impossible has happened";
        pp t;
        assert false
  (* impossible *)
  and aux_tag (tag, t) = (tag, aux t) in
  aux t0

let rec assoc_eq eq x = function
  | [] -> raise Not_found
  | (y, v) :: _ when eq x y -> v
  | _ :: l -> assoc_eq eq x l

let possibly_rec x t = if occurs x t then Rec (x, t) else t

let refold =
  let rec aux next tmap t =
    try RecVar (assoc_eq eq t tmap)
    with Not_found ->
      let id = "X" ^ string_of_int next in
      let next' = next + 1 in
      let tmap' = (t, id) :: tmap in
      possibly_rec id (aux0 next' tmap' (find t))
  and aux0 next tmap = function
    | (Var _ | SessionTypeVar _) as t -> t
    | Tagged (ctor, tags) -> Tagged (ctor, List.map (aux_tag next tmap) tags)
    | Constructor (ctor, ts) -> Constructor (ctor, List.map (aux next tmap) ts)
    | _ -> assert false
  (* not implemented, not useful? *)
  and aux_tag next tmap (tag, t) = (tag, aux next tmap t) in
  aux 0 []

let phase_two =
  let next = ref 0 in
  let tmap = ref [] in
  let get_next_var () =
    let n = !next in
    incr next;
    "X" ^ string_of_int n
  in
  let rec aux pol t =
    let key = (pol, t) in
    try assoc_eq (fun (p, t) (q, s) -> p = q && eq t s) key !tmap
    with Not_found ->
      let x = get_next_var () in
      tmap := (key, RecVar x) :: !tmap;
      let t' = aux_unguarded pol (find t) in
      tmap := List.tl !tmap;
      possibly_rec x t'
  and aux_unguarded pol = function
    | Var _ as t -> t
    | Constructor (`Channel, [ it; ot ]) when pol = `In ->
        aux_session_type it ot
    | Constructor (`Channel, [ it; ot ]) -> aux_session_type ot it
    | Constructor (ctor, ts) -> Constructor (ctor, List.map (aux `In) ts)
    | Tagged (ctor, tags) -> Tagged (ctor, List.map (aux_tag `In) tags)
    | t ->
        print_endline "cannot decode:";
        pp t;
        print_newline ();
        assert false
  and aux_session_type it ot =
    match (find it, find ot) with
    | Constructor (`Empty, []), Constructor (`Empty, []) ->
        Constructor (`End, [])
    | Constructor (`Done, []), Constructor (`Done, []) -> Constructor (`Done, [])
    | Constructor (`Tuple, [ t; ct ]), Constructor (`Empty, []) ->
        Constructor (`Receive, [ aux `In t; aux `In ct ])
    | Constructor (`Empty, []), Constructor (`Tuple, [ t; ct ]) ->
        Constructor (`Send, [ aux `In t; aux `Out ct ])
    | Tagged (`Variant, tags), Constructor (`Empty, []) ->
        Tagged (`Branch, List.map (aux_tag `In) tags)
    | Constructor (`Empty, []), Tagged (`Variant, tags) ->
        Tagged (`Choice, List.map (aux_tag `Out) tags)
    | Var x, Var y -> SessionTypeVar (x, y)
    | Constructor (`Sequence, [ t; s ]), _ ->
        Constructor (`AcceptSequence, [ aux `In t; aux `In s ])
    | Constructor (`Empty, []), Constructor (`Sequence, [ t; s ]) ->
        Constructor (`SelectSequence, [ aux `Out t; aux `Out s ])
    | it, ot -> Constructor (`Channel, [ aux `In it; aux `In ot ])
  and aux_tag pol (tag, t) = (tag, aux pol t) in
  aux `In

let subscript_of_int =
  let numbers = [ "₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉" ] in
  let rec aux n =
    (if n < 10 then "" else aux (n / 10)) ^ List.nth numbers (n mod 10)
  in
  aux

let var_of_int letters n =
  let m = List.length letters in
  let i = n mod m in
  let j = n / m in
  List.nth letters i ^ if j = 0 then "" else subscript_of_int j

let type_var_of_int = var_of_int [ "α"; "β"; "γ"; "δ"; "ε" ]

let rec_var_of_int = var_of_int [ "X"; "Y"; "Z" ]

let session_type_var_of_int = var_of_int [ "A"; "B"; "C" ]

let rename_vars t =
  let tvars, rvars, stvars = all_vars t in
  let tmap = List.mapi (fun n x -> (x, type_var_of_int n)) tvars in
  let rmap = List.mapi (fun n x -> (x, rec_var_of_int n)) rvars in
  let stmap =
    List.mapi (fun n (x, y) -> ((x, y), session_type_var_of_int n)) stvars
  in
  let rec aux = function
    | Var x -> Var (List.assoc x tmap)
    | RecVar x -> RecVar (List.assoc x rmap)
    | SessionTypeVar (x, y) when List.mem_assoc (x, y) stmap ->
        Var (List.assoc (x, y) stmap)
    | SessionTypeVar (x, y) when List.mem_assoc (y, x) stmap ->
        Var (List.assoc (y, x) stmap ^ " dual")
    | SessionTypeVar _ -> assert false (* impossible *)
    | Rec (x, t) -> Rec (List.assoc x rmap, aux t)
    | Tagged (ctor, tags) -> Tagged (ctor, List.map aux_tag tags)
    | Constructor (ctor, ts) -> Constructor (ctor, List.map aux ts)
  and aux_tag (name, t) = (name, aux t) in
  aux t

let calculate_success_prob t =
  let q, r = Mapping.(compute_adj_list t |> map_adj_list) in
  let b = Math.Markov.absortion_matrix q r in
  Owl.Mat.get b 0 0

let rec fill_success_prob = function
  | Constructor (`ClosedChannel, [ t ]) ->
      Constructor (`ClosedChannel, [ t_Prob (calculate_success_prob t) ])
  | Constructor (ctor, ts) -> Constructor (ctor, List.map fill_success_prob ts)
  | ts -> ts

let decode t =
  phase_one t |> phase_two |> refold |> rename_vars |> fill_success_prob
