module type OrderedType = sig
  type t
  [@@deriving yojson]

  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
end

module Int = struct
  type t = int
  [@@deriving yojson]

  let pp = Format.pp_print_int
  let compare = compare
end

module type SetS = sig
  include Set.S

  val pp_gen : open_:string -> close:string -> empty:string -> Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) Result.result
end

module MakeSet (Ord : OrderedType) = struct
  include Set.Make(Ord)

  let pp_gen ~open_ ~close ~empty fmt fs =
    let fpf = Format.fprintf in
    match elements fs with
    | [] ->
      fpf fmt "%s" empty
    | f :: fs ->
      fpf fmt "%s" open_;
      Ord.pp fmt f;
      List.iter (fpf fmt ", %a" Ord.pp) fs;
      fpf fmt "%s" close
  (* FIXME: this is here because of Atom.pp that uses @@deriving. But this should move. *)

  let pp fmt fs =
    pp_gen ~open_:"{" ~close:"}" ~empty:"∅" fmt fs

  (* FIXME: yojson deriving. This should probably move. *)

  type tmp_list = Ord.t list [@@deriving yojson]
  let to_tmp_list s = fold List.cons s []
  let of_tmp_list j = of_list j

  let to_yojson s = s |> to_tmp_list |> tmp_list_to_yojson
  let of_yojson j = j |> tmp_list_of_yojson |> (fun l -> Ppx_deriving_yojson_runtime.(>|=) l of_tmp_list)
end

module type MapS = sig
  include Map.S

  val map_filter : ('a -> 'b option) -> 'a t -> 'b t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (* FIXME: for < 4.06 compatibility *)

  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
  val of_yojson : (Yojson.Safe.t -> ('a, string) Result.result) -> Yojson.Safe.t -> ('a t, string) Result.result
end

module MakeMap (Ord : OrderedType) = struct
  include Map.Make(Ord)

  let map_filter f m =
    m
    |> map f
    |> filter (fun _ -> (<>) None)
    |> map (function Some x -> x | None -> assert false)

  let update x f m = (* FIXME: for < 4.06 compatibility *)
    match f (find_opt x m) with
    | None -> remove x m
    | Some y -> add x y m

  type 'a tmp_list = (Ord.t * 'a) list [@@deriving yojson]
  let to_tmp_list s = fold (fun k v -> List.cons (k, v)) s []
  let of_tmp_list j = List.fold_left (fun m (k, v) -> add k v m) empty j

  let to_yojson t s = s |> to_tmp_list |> tmp_list_to_yojson t
  let of_yojson f j = j |> tmp_list_of_yojson f |> (fun l -> Ppx_deriving_yojson_runtime.(>|=) l of_tmp_list)
end
