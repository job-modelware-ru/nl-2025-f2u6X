let mutable_x = ref 10;;
mutable_x := !mutable_x + 1;;
!mutable_x;; (* int = 11 *)
