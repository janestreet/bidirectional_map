open! Core
open Bidirectional_map_interfaces

module type With_bin_io = sig
  type t

  include Binable.S with type t := t
  include Comparator.S with type t := t
end

module type S = sig
  type ('l, 'lc, 'r, 'rc) t

  include Deriving_shared with type ('l, 'lc, 'r, 'rc) t := ('l, 'lc, 'r, 'rc) t

  module Provide_bin_io (Left : With_bin_io) (Right : With_bin_io) :
    Binable.S with type t := M(Left)(Right).t
end
