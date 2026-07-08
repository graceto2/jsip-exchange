open! Core

module List_seq = struct
  (* TODO: replace the definition of type t and the implementations of
     create, set, and get *)
  type t = int list ref

  let create () = ref []

  let set t ~key ~data =
    let length = List.length !t in
    t
    := if key = length
       then List.append !t [ data ]
       else if key < length
       then List.mapi !t ~f:(fun i x -> if i = key then data else x)
       else failwith "index out of bounds"
  ;;

  let get t key = List.nth !t key
end

module Dynarray_seq = struct
  (* TODO: replace the definition of type t and the implementations of
     create, set, and get *)
  type t = int Dynarray.t

  let create () = Dynarray.create ()

  let set t ~key ~data =
    let length = Dynarray.length t in
    if key = length
    then Dynarray.add_last t data
    else Dynarray.set t key data
  ;;

  let get t key =
    match Dynarray.get t key with
    | data -> Some data
    | exception Invalid_argument _ -> None
  ;;
end
