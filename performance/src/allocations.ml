open! Core

module Build_list = struct
  (* [acc @ [ x ]] copies the whole accumulator each step -> O(n^2)
     allocation. *)
  let silly xs =
    let acc = ref [] in
    List.iter xs ~f:(fun x -> acc := !acc @ [ x ]);
    !acc
  ;;

  (* Prepend (O(1) per step) then reverse once -> O(n) allocation. Same
     result. *)
  let non_silly xs =
    let acc = ref [] in
    List.iter xs ~f:(fun x -> acc := x :: !acc);
    List.rev !acc
  ;;
end

module First_match = struct
  (* Allocate a fresh list of *every* match, then throw all but the head
     away. *)
  let silly xs ~f =
    let filtered = List.filter xs ~f in
    List.nth filtered 0
  ;;

  (* Stop at the first match; allocate nothing but the returned [Some]. *)
  let non_silly xs ~f = List.find xs ~f
end
