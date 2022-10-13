open! Core.Core_stable
open Bidirectional_map_interfaces
open Bidirectional_map_interfaces_stable

module V1 = struct
  type ('l, 'lc, 'r, 'rc) t = ('l, 'lc, 'r, 'rc) Bidirectional_multimap.t

  module M = Bidirectional_multimap.M

  (* Comparisons, etc.: provided for deriving, do not actually have to be stable *)

  let compare_m__t = Bidirectional_multimap.compare_m__t
  let equal_m__t = Bidirectional_multimap.equal_m__t
  let hash_fold_m__t = Bidirectional_multimap.hash_fold_m__t
  let hash_m__t = Bidirectional_multimap.hash_m__t

  (* Serializations: implement via explicit round-trip to/from association list *)

  let sexp_of_m__t
        (type l r)
        (module Left : With_sexp_of with type t = l)
        (module Right : With_sexp_of with type t = r)
        t
    =
    [%sexp_of: (Left.t * Right.t) list] (Bidirectional_multimap.to_alist t)
  ;;

  let m__t_of_sexp
        (type l lc r rc)
        (module Left : With_of_sexp with type t = l and type comparator_witness = lc)
        (module Right : With_of_sexp with type t = r and type comparator_witness = rc)
        sexp
    =
    Bidirectional_multimap.of_alist
      (module Left)
      (module Right)
      ([%of_sexp: (Left.t * Right.t) list] sexp)
  ;;

  module Provide_bin_io (Left : With_bin_io) (Right : With_bin_io) :
    Core.Binable.S with type t := M(Left)(Right).t =
    Binable.Of_binable.V2
      (struct
        type t = (Left.t * Right.t) list [@@deriving bin_io]
      end)
      (struct
        type t = M(Left)(Right).t

        let to_binable = Bidirectional_multimap.to_alist

        let of_binable alist =
          Bidirectional_multimap.of_alist (module Left) (module Right) alist
        ;;

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "75a4dad2-5fee-42ee-bb74-0b30343e7766"
        ;;
      end)
end
