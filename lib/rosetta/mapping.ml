open Ast
open Owl

type state = State of int | Done | Idle

let new_state adj_list =
  let inc_state = function
    | State s -> State (s + 1)
    | _ -> raise (Invalid_argument "Can't be final state")
  in

  match adj_list with
  | (state, _) :: _ -> inc_state state
  | _ -> raise (Invalid_argument "Can't get next from empty list")

let rec compute_adj prev_state p rec_states adj_list t =
  let add_transition current_state =
    List.map (fun (state, children) ->
        if prev_state = state then (prev_state, (current_state, p) :: children)
        else (state, children))
  in

  let current_state = new_state adj_list in

  match t with
  | Constructor (`Done, []) -> add_transition Done adj_list
  | Constructor (`End, []) -> add_transition Idle adj_list
  | Rec (rec_var, t) ->
      compute_adj current_state 1.0
        ((rec_var, current_state) :: rec_states)
        ((current_state, []) :: add_transition current_state adj_list)
        t
  | RecVar rec_var -> add_transition (List.assoc rec_var rec_states) adj_list
  | Constructor ((`Send | `Receive), [ _; t ]) ->
      compute_adj current_state 1.0 rec_states
        ((current_state, []) :: add_transition current_state adj_list)
        t
  | Tagged
      ( (`Choice | `Branch),
        [
          ("Prob", Constructor (`Prob p, []));
          ("True", true_branch);
          ("False", false_branch);
        ] ) ->
      let true_adj_list =
        compute_adj current_state p rec_states
          ((current_state, []) :: add_transition current_state adj_list)
          true_branch
      in
      compute_adj current_state (1.0 -. p) rec_states true_adj_list false_branch
  | _ -> adj_list

let compute_adj_list = compute_adj (State 0) 1.0 [] [ (State 0, []) ]

let map_adj_list adj_list =
  let done_col_idx = 0 in
  let idle_col_idx = 1 in
  let size = List.length adj_list in
  let q = Mat.zeros size size in
  let r = Mat.zeros size 2 in
  List.iter
    (fun (state, children) ->
      match state with
      | State i ->
          List.iter
            (fun (next_state, p) ->
              match next_state with
              | Done -> Mat.set r i done_col_idx p
              | Idle -> Mat.set r i idle_col_idx p
              | State j -> Mat.set q i j p)
            children
      | _ -> ())
    adj_list;
  (q, r)
