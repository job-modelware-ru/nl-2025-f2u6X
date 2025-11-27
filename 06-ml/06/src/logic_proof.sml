datatype prop =
         Var of string
       | And of prop * prop
       | Imp of prop * prop

datatype theorem = Theorem of prop list * prop

datatype proof =
         Assume of prop
       | AndIntro of proof * proof
       | AndElimLeft of proof
       | AndElimRight of proof
       | ImpIntro of prop * proof
       | ImpElim of proof * proof

fun add_to_ctx (p : prop) [] = [p]
  | add_to_ctx p (h :: t) =
        if h = p then h :: t else h :: add_to_ctx p t

fun union_ctx (ctx : prop list) (additions : prop list) =
    foldl (fn (p, acc) =>
              if List.exists (fn q => q = p) acc
              then acc
              else p :: acc) ctx additions

fun remove_once (target : prop) (ctx : prop list) =
    case ctx of
        [] => raise Fail "assumption not found in context"
      | h :: t =>
            if h = target then t else h :: remove_once target t

fun infer (Assume p) = Theorem ([p], p)
  | infer (AndIntro (left_pf, right_pf)) =
        let
            val Theorem (ctx_l, left_prop) = infer left_pf
            val Theorem (ctx_r, right_prop) = infer right_pf
            val new_ctx = union_ctx ctx_l ctx_r
        in
            Theorem (new_ctx, And (left_prop, right_prop))
        end
  | infer (AndElimLeft pf) =
        let
            val Theorem (ctx, concl) = infer pf
        in
            case concl of
                And (p, _) => Theorem (ctx, p)
              | _ => raise Fail "ожидалась конъюнкция для левого устранения"
        end
  | infer (AndElimRight pf) =
        let
            val Theorem (ctx, concl) = infer pf
        in
            case concl of
                And (_, q) => Theorem (ctx, q)
              | _ => raise Fail "ожидалась конъюнкция для правого устранения"
        end
  | infer (ImpIntro (assumption, pf)) =
        let
            val Theorem (ctx, conclusion) = infer pf
            val remaining_ctx =
                if List.exists (fn p => p = assumption) ctx
                then remove_once assumption ctx
                else raise Fail "attempted to discharge missing assumption"
        in
            Theorem (remaining_ctx, Imp (assumption, conclusion))
        end
  | infer (ImpElim (imp_pf, arg_pf)) =
        let
            val Theorem (ctx_imp, imp_concl) = infer imp_pf
            val (premise, result) =
                case imp_concl of
                    Imp pair => pair
                  | _ => raise Fail "ожидалась импликация для Modus Ponens"
            val Theorem (ctx_arg, arg_prop) = infer arg_pf
            val _ =
                if arg_prop = premise
                then ()
                else raise Fail "modus ponens mismatch"
            val combined_ctx = union_ctx ctx_imp ctx_arg
        in
            Theorem (combined_ctx, result)
        end

fun prop_to_string (Var name) = name
  | prop_to_string (And (p, q)) =
        "(" ^ prop_to_string p ^ " ∧ " ^ prop_to_string q ^ ")"
  | prop_to_string (Imp (p, q)) =
        "(" ^ prop_to_string p ^ " → " ^ prop_to_string q ^ ")"

fun ctx_to_string [] = "∅"
  | ctx_to_string ctx =
        String.concatWith ", " (List.map prop_to_string ctx)

fun theorem_to_string (Theorem (ctx, conclusion)) =
        ctx_to_string ctx ^ " ⊢ " ^ prop_to_string conclusion

val a = Var "A"
val b = Var "B"
val conjunction = And (a, b)
val swapped = And (b, a)
val target_theorem = Imp (conjunction, swapped)

val proof_swap =
    ImpIntro (conjunction,
      let
          val hyp = Assume conjunction
          val proj_a = AndElimLeft hyp
          val proj_b = AndElimRight hyp
          val reordered = AndIntro (proj_b, proj_a)
      in
          reordered
      end)

val derived = infer proof_swap

val _ =
    case derived of
        Theorem ([], conclusion) =>
            print ("Теорема доказана: " ^ prop_to_string conclusion ^ "\n")
      | th =>
            print ("Доказательство содержит незакрытые допущения: "
                   ^ theorem_to_string th ^ "\n")

