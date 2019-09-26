(* University of Vermont CSPF *)
(* Author: Lindsey Stuntz *)
(* Author John H. Ring IV *)
(* PCNC - NAL Implementation *)
(* In Progress *)

open Util
open StringSetMap
open Core
open Yojson
open Yojson.Basic.Util

(********************
 * Syntax for types *
 ********************)

(* X ∈ prog ≈ 𝕊
*)
type prog = string
[@@deriving show {with_path = false}, sexp]

(* X ∈ auta ≈ 𝕊
*)
type auta = string
[@@deriving show {with_path = false}, sexp]

(* X ∈ tvar ≈ 𝕊
*)
type tvar = string
[@@deriving show {with_path = false}, sexp]

(* A ∈ pals ≈ 𝕊
*)
type pals = string
[@@deriving show {with_path = false}, sexp]


(* τ ∈ type ⩴ unit | τ + τ | τ × τ | τ → τ | X | ∀X.τ | ∃X.τ | A says τ | install(τ) | p
 *)

module Ty = struct
  type t
    = Unit
    | Sum of t * t
    | Prod of t * t
    | Fun of t * t
    | TVar of tvar
    | Forall of tvar * t
    | Exists of tvar * t
    | Says of t * t
    | Install of t
    | Reconfig of t
    | Extend of t
    | Program of prog
    | Principal of pals
  [@@deriving show {with_path = false}, sexp]

  let of_string s =
    Sexp.of_string s |> t_of_sexp

  let to_string t =
    sexp_of_t t |> Sexp.to_string

end

type ty = Ty.t
[@@deriving show {with_path = false}, sexp]

(**************************
 * Syntax for expressions *
 **************************)

(* x ∈ var ≈ 𝕊
 *)
type var = string
[@@deriving show {with_path = false}, sexp]

(* e ∈ exp ⩴ Bullet
 *         | inl(e) as . + τ | inr(e) as . + τ | case(e){x.e}{x.e}
 *         | ⟨e,e⟩ | projl(e) | projr(e)
 *         | x | let x ≔ e in e | λ(x:τ).e | e(e)
 *         | ΛX.e | e[τ]
 *         | ⟨*τ,e⟩ as ∃X.τ | let ⟨*X,x⟩ = e in e
 *         | ret^A(e) | x ← e ; e
 *         | install(p)
 *)
module Exp = struct
  type t
    = Bullet
    | Inl of t * Ty.t
    | Inr of t * Ty.t
    | Case of t * var * t * var * t
    | Pair of t * t
    | Projl of t
    | Projr of t
    | Var of var
    | Let of var * t * t
    | Lambda of var * Ty.t * t
    | Apply of t * t
    | BigLambda of tvar * t
    | TyApply of t * Ty.t
    | Pack of Ty.t * t * tvar * Ty.t
    | Unpack of tvar * var * t * t
    | Ret of pals * t
    | Bind of var * t * t
  [@@deriving show {with_path = false}, sexp]

  let of_string s =
    Sexp.of_string s |> t_of_sexp

  let to_string t =
    sexp_of_t t |> Sexp.to_string

end

type exp = Exp.t
[@@deriving show {with_path = false}, sexp]


(***********************************
 * Syntax for type system contexts *
 ***********************************)

(* Γ ∈ tenv ≔ var ⇀ type
*)
type tenv = ty string_map
[@@deriving show {with_path = false}]

(* S ∈ scope ≔ ℘(tvar)
*)
type tscope = string_set
[@@deriving show {with_path = false}]

(****************************
 * Free variables for types *
 ****************************)

(* FV ∈ type → ℘(tvar)
 * tfree_vars τ ≡ FV(τ)
 *)
let rec tfree_vars (t0 : ty) : string_set = match t0 with
  | Unit -> StringSet.empty
  | Sum(t1,t2) -> StringSet.union (tfree_vars t1) (tfree_vars t2)
  | Prod(t1,t2) -> StringSet.union (tfree_vars t1) (tfree_vars t2)
  | Fun(t1,t2) -> StringSet.union (tfree_vars t1) (tfree_vars t2)
  | TVar(xt) -> StringSet.of_list [xt]
  | Forall(xt,t) -> StringSet.remove xt (tfree_vars t)
  | Exists(xt,t) -> StringSet.remove xt (tfree_vars t)
  | Says(t1,t2) -> StringSet.union (tfree_vars t1) (tfree_vars t2)
  | Install(t) -> StringSet.empty
  | Reconfig(t) -> StringSet.empty
  | Extend(t) -> StringSet.empty
  | Program(p) -> StringSet.empty
  | Principal(p) -> StringSet.empty

(**************************
 * Substitution for types *
 **************************)

(* An auxiliary function:
 *
 *   trename X X′ τ
 *
 * for renaming X to X′ in type τ, i.e., [X↦X′]τ
 *)
let rec trename (xt : tvar) (xt' : tvar) (t0 : ty) : ty = match t0 with
  | Unit -> Unit
  | Sum(t1,t2) -> Sum(trename xt xt' t1,trename xt xt' t2)
  | Prod(t1,t2) -> Prod(trename xt xt' t1,trename xt xt' t2)
  | Fun(t1,t2) -> Fun(trename xt xt' t1,trename xt xt' t2)
  | TVar(yt) -> if xt = yt then TVar(xt') else TVar(yt)
  | Forall(yt,t) -> if xt = yt then Forall(yt,t) else Forall(yt,trename xt xt' t)
  | Exists(yt,t) -> if xt = yt then Exists(yt,t) else Forall(yt,trename xt xt' t)
  | Says(t1,t2) -> Says(trename xt xt' t1,trename xt xt' t2)
  | Install(t) -> Install(t)
  | Reconfig(t) -> Reconfig(t)
  | Extend(t) -> Extend(t)
  | Program(p) -> Program(p)
  | Principal(a) -> Principal(a)

(* An auxiliary function:
 *
 *   tfresh X τ′ τ = ⟨X′,τ″⟩
 *
 * which returns X′ and τ″ such that
 *
 *   X′ ∉ FV(τ′) ∪ (FV(τ) ∖ ❴X❵)
 *   τ″ = [X↦X′]τ
 *
 * we first define an iterator helper function:
 *
 *   tfresh_i X X′ τ′ τ
 *
 * which tries the current candidate X′, and iterates to try X′ with a prime
 * appended to the variable symbol if X′ isn't satisfactory.
 *
 * a call to
 *
 *   tfresh X τ′ τ
 *
 * runs the iterator with X as the current guess.
 *)
let rec tfresh_i (xt : tvar) (xt' : tvar) (t' : ty) (t : ty) : tvar * ty =
  if StringSet.mem xt' (StringSet.union (tfree_vars t') (tfree_vars (Forall(xt,t))))
  then tfresh_i xt (xt' ^ "'") t' t
  else (xt',trename xt xt' t)

let tfresh (xt : tvar) (t' : ty) (t : ty) : tvar * ty = tfresh_i xt xt t' t

(* The capture-avoiding substitution function for types.
 *
 * [_↦_]_ ∈ tvar × type × type → type
 * tsubst X τ′ τ ≡ [X↦τ′]τ
 *)
let rec tsubst (xt : tvar) (t' : ty) (t0 : ty) : ty = match t0 with
  | Unit -> Unit
  | Sum(t1,t2) -> Sum(tsubst xt t' t1,tsubst xt t' t2)
  | Prod(t1,t2) -> Prod(tsubst xt t' t1,tsubst xt t' t2)
  | Fun(t1,t2) -> Fun(tsubst xt t' t1,tsubst xt t' t2)
  | TVar(yt) -> if xt = yt then t' else TVar(yt)
  | Forall(yt,t) ->
      if xt = yt then Forall(xt,t) else
      let (yt'',t'') = tfresh yt t' t in
      Forall(yt'',tsubst xt t' t'')
  | Exists(yt,t) ->
      if xt = yt then Exists(xt,t) else
      let (yt'',t'') = tfresh yt t' t in
      Exists(yt'',tsubst xt t' t'')
  | Says(t1,t2) -> Says(tsubst xt t' t1,tsubst xt t' t2)
  | Install(t) -> Install(tsubst xt t' t)
  | Reconfig(t) -> Reconfig(tsubst xt t' t)
  | Extend(t) -> Extend(tsubst xt t' t)
  | Program(p) -> Program(p)
  | Principal(a) -> Principal(a)

(**********************************
 * Free variables for expressions *
 **********************************)

(* FV ∈ exp → ℘(var)
 * efree_vars e ≡ FV(e)
 *)
let rec efree_vars (e0 : exp) : string_set = match e0 with
  | Bullet -> StringSet.empty
  | Inl(e,t) -> efree_vars e
  | Inr(e,t) -> efree_vars e
  | Pair(e1,e2) -> StringSet.union (efree_vars e1) (efree_vars e2)
  | Case(e1,x2,e2,x3,e3) ->
    StringSet.union
      (efree_vars e1)
      (StringSet.union
         (StringSet.remove x2 (efree_vars e2))
         (StringSet.remove x3 (efree_vars e3)))
  | Projl(e) -> efree_vars e
  | Projr(e) -> efree_vars e
  | Var(x) -> StringSet.of_list [x]
  | Let(x,e1,e2) ->
      StringSet.union
        (efree_vars e1)
        (StringSet.remove x (efree_vars e2))
  | Lambda(x,t,e) ->
      StringSet.remove x (efree_vars e)
  | Apply(e1,e2) -> StringSet.union (efree_vars e1) (efree_vars e2)
  | BigLambda(xt,e) -> efree_vars e
  | TyApply(e,t) -> efree_vars e
  | Pack(t1,e,xt,t2) -> efree_vars e
  | Unpack(xt,x,e1,e2) ->
      StringSet.union
        (efree_vars e1)
        (StringSet.remove x (efree_vars e2))
  | Ret(a, e) -> efree_vars e
  | Bind(x, e1, e2) -> StringSet.union (efree_vars e1) (efree_vars e2)

(***********************************************
 * Substitution for expressions in expressions *
 ***********************************************)

(* Non-capture-avoiding substitution for expressions in expressions. Because
 * this is non-capture-avoiding, it assumes that the expression being
 * substituted is closed.
 *
 *   esubst_e_i x e′ e
 *
 * Assumption: e′ is closed
 *
 * Do not use this function directly. Instead, use esubst_e which checks the
 * invariant.
 *)
let rec esubst_e_i (x : var) (e' : exp) (e0 : exp) : exp = match e0 with
  | Bullet -> Bullet
  | Inl(e,t) -> Projl(esubst_e_i x e' e)
  | Inr(e,t) -> Projr(esubst_e_i x e' e)
  | Case(e1,y2,e2,y3,e3) -> Case(esubst_e_i x e' e1,y2,esubst_e_i x e' e2,y3,esubst_e_i x e' e3)
  | Pair(e1,e2) -> Pair(esubst_e_i x e' e1,esubst_e_i x e' e2)
  | Projl(e) -> Projl(esubst_e_i x e' e)
  | Projr(e) -> Projr(esubst_e_i x e' e)
  | Var(y) -> if x = y then e' else Var(y)
  | Let(y,e1,e2) ->
      if x = y
      then Let(x,esubst_e_i x e' e1,e2)
      else Let(y,esubst_e_i x e' e1,esubst_e_i x e' e2)
  | Lambda(y,t,e) ->
      if x = y
      then Lambda(x,t,e)
      else Lambda(y,t,esubst_e_i x e' e)
  | Apply(e1,e2) -> Apply(esubst_e_i x e' e1,esubst_e_i x e' e2)
  | BigLambda(xt,e) -> BigLambda(xt,esubst_e_i x e' e)
  | TyApply(e,t) -> TyApply(esubst_e_i x e' e,t)
  | Pack(t1,e,xt,t2) -> Pack(t1,esubst_e_i x e' e,xt,t2)
  | Unpack(xt,y,e1,e2) ->
      if x = y
      then Unpack(xt,x,esubst_e_i x e' e1,e2)
      else Unpack(xt,y,esubst_e_i x e' e1,esubst_e_i x e' e2)
  | Ret(a,e) -> Ret(a,esubst_e_i x e' e)
  | Bind(y,e1,e2) -> Bind(y,esubst_e_i x e' e1,esubst_e_i x e' e2)

exception NOT_CLOSED_ERROR

(* A version of non-capture-avoiding substitution that raises an exception if
 * its required assumptions are not satisfied.
 *
 * [_↦_]_ ∈ var × exp × exp → exp
 * esubst_e x e′ e ≡ [x↦e′]e
 *
 * Raises exception if e′ is not closed
 *)
let esubst_e (x : var) (e' : exp) (e : exp) : exp =
  if StringSet.equal (efree_vars e') StringSet.empty
  then esubst_e_i x e' e
  else raise NOT_CLOSED_ERROR

(*****************************************
 * Substitution for types in expressions *
 *****************************************)

(* Non-capture-avoiding substitution for types in expressions. Because this is
 * non-capture-avoiding, it assumes that the type being substituted is closed.
 *
 *   esubst_t_i X τ′ e
 *
 * Assumption: τ′ is closed
 *
 * Do not use this function directly. Instead, use esubst_t which checks the
 * invariant.
 *)
let rec esubst_t_i (xt : tvar) (t' : ty) (e0 : exp) : exp = match e0 with
  | Bullet -> Bullet
  | Inl(e,t) -> Projl(esubst_t_i xt t' e)
  | Inr(e,t) -> Projr(esubst_t_i xt t' e)
  | Case(e1,y2,e2,y3,e3) -> Case(esubst_t_i xt t' e1,y2,esubst_t_i xt t' e2,y3,esubst_t_i xt t' e3)
  | Pair(e1,e2) -> Pair(esubst_t_i xt t' e1,esubst_t_i xt t' e2)
  | Projl(e) -> Projl(esubst_t_i xt t' e)
  | Projr(e) -> Projr(esubst_t_i xt t' e)
  | Var(x) -> Var(x)
  | Let(x,e1,e2) -> Let(x,esubst_t_i xt t' e1,esubst_t_i xt t' e2)
  | Lambda(x,t,e) -> Lambda(x,tsubst xt t' t,esubst_t_i xt t' e)
  | Apply(e1,e2) -> Apply(esubst_t_i xt t' e1,esubst_t_i xt t' e2)
  | BigLambda(yt,e) ->
      if xt = yt
      then BigLambda(xt,e)
      else BigLambda(yt,esubst_t_i xt t' e)
  | TyApply(e,t) -> TyApply(esubst_t_i xt t' e,tsubst xt t' t)
  | Pack(t1,e,yt,t2) ->
      if xt = yt
      then Pack(tsubst xt t' t1,esubst_t_i xt t' e,xt,t2)
      else Pack(tsubst xt t' t1,esubst_t_i xt t' e,yt,tsubst xt t' t2)
  | Unpack(yt,x,e1,e2) ->
      if xt = yt
      then Unpack(xt,x,esubst_t_i xt t' e1,e2)
      else Unpack(xt,x,esubst_t_i xt t' e1,esubst_t_i xt t' e2)
  | Ret(a,e) -> Ret(a,esubst_t_i xt t' e)
  | Bind(y,e1,e2) -> Bind(y,esubst_t_i xt t' e1,esubst_t_i xt t' e2)

(* A version of non-capture-avoiding substitution that raises an exception if
 * its required assumptions are not satisfied.
 *
 * [_↦_]_ ∈ tvar × type × exp → exp
 * esubst_t X τ′ e ≡ [X↦τ′]e
 *
 * Raises exception if τ′ is not closed
 *)
let esubst_t (x : var) (t' : ty) (e : exp) : exp =
  if StringSet.equal (tfree_vars t') StringSet.empty
  then esubst_t_i x t' e
  else raise NOT_CLOSED_ERROR

(**********************
 * Well-scoped relation
 **********************)

(* The relation:
 *
 *   S ⊢ τ
 *
 * scope_ok S τ = true ⟺  S ⊢ τ
 *)
let rec scope_ok (s : tscope) (t0 : ty) : bool = match t0 with
  (* [Unit]
   * S ⊢ unit *)
  | Unit -> true
  (* [Sum]
   * S ⊢ τ₁
   * S ⊢ τ₂
   * ⟹
   * S ⊢ τ₁ + τ₂ *)
  | Sum(t1,t2) ->
    if scope_ok s t1 && scope_ok s t2 then true else false
  (* [Prod]
   * S ⊢ τ₁
   * S ⊢ τ₂
   * ⟹
   * S ⊢ τ₁ × τ₂ *)
  | Prod(t1,t2) ->
    if scope_ok s t1 && scope_ok s t2 then true else false
  (* [Fun]
   * S ⊢ τ₁
   * S ⊢ τ₂
   * ⟹
   * S ⊢ τ₁ → τ₂ *)
  | Fun(t1,t2) ->
    if scope_ok s t1 && scope_ok s t2 then true else false
  (* [TVar]
   * X ∈ S
   * ⟹
   * S ⊢ X *)
  | TVar(xt) ->
    if StringSet.mem xt s then true else false
  (* [Forall]
   * S∪{X} ⊢ τ
   * ⟹
   * S ⊢ ∀X.τ *)
  | Forall(xt,t) ->
    if scope_ok (StringSet.union s (StringSet.of_list [xt])) t then true else false
  (* [Exists]
   * S∪{X} ⊢ τ
   * ⟹
   * S ⊢ ∃X.τ *)
  | Exists(xt,t) ->
    if scope_ok (StringSet.union s (StringSet.of_list [xt])) t then true else false
  (* [Says]
   * S ⊢ τ
   * ⟹
   * S ⊢ A says τ *)
  | Says(t1,t2) ->
    if scope_ok s t1 && scope_ok s t2 then true else false
  (* [Install]
   * S ⊢ τ
   * ⟹
   * S ⊢ install(τ) *)
  | Install(t) ->
    if scope_ok s t then true else false
  | Extend(t) ->
    if scope_ok s t then true else false
  | Reconfig(t) ->
    if scope_ok s t then true else false
  (* [Program]
   * S ⊢ Program(p) *)
  | Program(p) -> true
  | Principal(a) -> true

(***********************
 * Well-typed relation *
 ***********************)

(* An auxiliary function for testing the equality of two types, modulo alpha
 * conversion.
 *
 * First, a helper function tequal_r which keeps track of which bindings are
 * equal by assigning them to unique numbers.
 *)
let rec tequal_r (l : int) (t1e : int string_map) (t2e : int string_map) (t1 : ty) (t2 : ty) : bool = match t1 , t2 with
  | Unit , Unit -> true
  | Sum(t11,t12) , Sum(t21,t22) -> tequal_r l t1e t2e t11 t21 && tequal_r l t1e t2e t12 t22
  | Prod(t11,t12) , Prod(t21,t22) -> tequal_r l t1e t2e t11 t21 && tequal_r l t1e t2e t12 t22
  | Fun(t11,t12) , Fun(t21,t22) -> tequal_r l t1e t2e t11 t21 && tequal_r l t1e t2e t12 t22
  | TVar(x) , TVar(y) ->
      if StringMap.mem x t1e && StringMap.mem y t2e
      then StringMap.find x t1e = StringMap.find y t2e
      else x = y
  | Forall(xt,t1) , Forall(yt,t2) -> tequal_r (l+1) (StringMap.add xt l t1e) (StringMap.add yt l t2e) t1 t2
  | Forall(_) , _ -> false | _ , Forall(_) -> false
  | Exists(xt,t1) , Exists(yt,t2) -> tequal_r (l+1) (StringMap.add xt l t1e) (StringMap.add yt l t2e) t1 t2
  | Says(t11,t12), Says(t21,t22) ->  tequal_r l t1e t2e t11 t21 && tequal_r l t1e t2e t12 t22
  | Install(p1), Install(p2) -> p1 = p2
  | Extend(p1), Extend(p2) -> p1 = p2
  | Reconfig(p1), Reconfig(p2) -> p1 = p2
  | Principal(a1), Principal(a2) -> a1 = a2
  | _ , _ -> false

(* tequal τ₁ τ₂ = true ⟺  τ₁ ≈ᵅ τ₂
 *
 * !! use tequal in your implementation of infer anytime you need to compare
 * two types for equality
 *)
let tequal (t1 : ty) (t2 : ty) : bool = tequal_r 1 StringMap.empty StringMap.empty t1 t2

exception TYPE_ERROR

(* The relation:
 *
 *   S , Γ ⊢ e : τ
 *
 * infer S Γ e = τ ⟺  S , Γ ⊢ : τ
 *)
let rec infer (s : tscope) (g : tenv) (e0 : exp) : ty = match e0 with
  (* [Bullet]
   * S , Γ ⊢ . : unit *)
  | Bullet -> Unit
  (* [Inl]
   * S ⊢ τ₂
   * S , Γ ⊢ e : τ₁
   * ⟹
   * S , Γ ⊢ inl(e) as . + τ₂ : τ₁ + τ₂ *)
  | Inl(e,t2) ->
    if not (scope_ok s t2) then raise TYPE_ERROR else
    let t1 = infer s g e in
    Sum(t1,t2)
  (* [Inr]
   * S ⊢ τ₁
   * S , Γ ⊢ e : τ₂
   * ⟹
   * S , Γ ⊢ inr(e) as τ₁ + . : τ₁ + τ₂ *)
  | Inr(e,t1) ->
    if not (scope_ok s t1) then raise TYPE_ERROR else
      let t2 = infer s g e in
      Sum(t1,t2)
  (* [Case]
   * S , Γ ⊢ e₁ : τ₁ + τ₂
   * S , Γ[x↦τ₁] ⊢ e₂ : τ
   * S , Γ[y↦τ₂] ⊢ e₃ : τ
   * ⟹
   * S , Γ ⊢ case(e₁){x.e₂}{y.e₃} : τ *)
  | Case(e1,x2,e2,x3,e3) ->
      let t = infer s g e1 in
      begin match t with
      | Sum(t1,t2) ->
          let t1' = infer s (StringMap.add x2 t1 g) e2 in
          let t2' = infer s (StringMap.add x3 t2 g) e3 in
          if not (t1' = t2') then raise TYPE_ERROR else
          t2'
      | _ -> raise TYPE_ERROR
      end
  (* [Pair]
   * S , Γ ⊢ e₁ : τ₁
   * S , Γ ⊢ e₂ : τ₂
   * ⟹
   * S , Γ ⊢ ⟨e₁,e₂⟩ : τ₁ × τ₂ *)
  | Pair(e1,e2) ->
      let t1 = infer s g e1 in
      let t2 = infer s g e2 in
      Prod(t1,t2)
  (* [Projl]
   * S , Γ ⊢ e : τ₁ × τ₂
   * ⟹
   * S , Γ ⊢ projl(e) : τ₁ *)
  | Projl(e) ->
      let t = infer s g e in
      begin match t with
      | Prod(t1,_) -> t1
      | _ -> raise TYPE_ERROR
      end
  (* [Projr]
   * S , Γ ⊢ e : τ₁ × τ₂
   * ⟹
   * S , Γ ⊢ projr(e) : τ₂ *)
  | Projr(e) ->
      let t = infer s g e in
      begin match t with
      | Prod(_,t2) -> t2
      | _ -> raise TYPE_ERROR
      end
  (* [Var]
   * Γ(x) = τ
   * ⟹
   * S , Γ ⊢ x : τ *)
  | Var(x) ->
    StringMap.find x g
  (* [Let]
   * S , Γ ⊢ e₁ : τ₁
   * S , Γ[x↦τ₁] ⊢ e₂ : τ₂
   * ⟹
   * S , Γ ⊢ let x ≔ e₁ in e₂ : τ₂ *)
  | Let(x,e1,e2) ->
    let t1 = infer s g e1 in
    let t2 = infer s (StringMap.add x t1 g) e2 in
    t2
  (* [Lambda]
   * S ⊢ τ₁
   * S , Γ[x↦τ₁] ⊢ e : τ₂
   * ⟹
   * S , Γ ⊢ λ(x:τ₁).e : τ₁ → τ₂ *)
  | Lambda(x,t1,e) ->
    if not (scope_ok s t1) then raise TYPE_ERROR else
    let t2 = infer s (StringMap.add x t1 g) e in
    Fun(t1,t2)
  (* [Apply]
   * S , Γ ⊢ e₁ : τ₁ → τ₂
   * S , Γ ⊢ e₂ : τ₁
   * ⟹
   * S , Γ ⊢ e₁(e₂) : τ₂ *)
  | Apply(e1,e2) ->
    let t' = infer s g e1 in
    let t1 = infer s g e2 in
    begin match t' with
      | Fun(t1',t2) -> if not (tequal t1 t1') then raise TYPE_ERROR else t2
      | _ -> raise TYPE_ERROR
      end
  (* [TypeLambda]
   * S∪{X} , Γ ⊢ e : τ
   * ⟹
   * S , Γ ⊢ ΛX.e : ∀X.τ *)
  | BigLambda(xt,e) ->
    let t = infer (StringSet.union s (StringSet.of_list [xt])) g e in
    Forall(xt,t)
  (* [TypeApply]
   * S ⊢ τ′
   * S , Γ ⊢ e : ∀X.τ
   * ⟹
   * S , Γ ⊢ e[τ′] : [X↦τ′]τ *)
  | TyApply(e,t') ->
    if not (scope_ok s t') then (printf "gackkk"; raise TYPE_ERROR) else
    let t1 = infer s g e in
    begin match t1 with
      | Forall(xt,t) -> tsubst xt t' t
      | _ -> raise TYPE_ERROR
      end
  (* [Pack]
   * S , Γ ⊢ e : [X↦τ′]τ
   * ⟹
   * S , Γ ⊢ ⟨*τ′,e⟩ as ∃ X.τ : ∃X.τ *)
  | Pack(t1,e,xt,t2) ->
    let t = infer s g e in
    let t' = tsubst xt t1 t2 in
    if not (tequal t t') then raise TYPE_ERROR else
      Exists(xt,t2)
  (* [Unpack-Alt]
   * S ⊢ τ₂
   * S , Γ ⊢ e₁ : ∃Y.τ₁
   * S∪{X} , Γ[x↦[Y↦X]τ₁] ⊢ e₂ : τ₂
   * ⟹
   * let ⟨*X,x⟩ ≔ e₁ in e₂ : τ₂ *)
  | Unpack(xt,x,e1,e2) ->
    let t = infer s g e1 in
    begin match t with
      | Exists(yt,t1) ->
        let t2 = infer (StringSet.union s (StringSet.of_list [xt])) (StringMap.add x (trename yt xt t1) g) e2 in
        if not (scope_ok s t2) then raise TYPE_ERROR else
          t2
      | _ -> raise TYPE_ERROR
    end
  (* [Ret]
   * S , Γ ⊢ e : τ
   * ⟹
   * S , Γ ⊢ ret^A(e) : A says τ *)
  | Ret(a,e) ->
    let t = infer s g e in
    Says((Principal a),t)
  (* [Bind]
   * S , Γ ⊢ e₁ : A says τ₁
   * S , Γ[x↦τ₁] ⊢ e₂ : A says τ₂
   * ⟹
   * S , Γ ⊢ x ← e₁ ; e₂ : A says τ₂ *)
  | Bind(x,e1,e2) ->
    let t = infer s g e1 in
    begin match t with
      | Says(a,t1) ->
        let t' = infer s (StringMap.add x t1 g) e2 in
        begin match t' with
          | Says(b, t1) -> if a = b then t' else raise TYPE_ERROR
          | _ -> raise TYPE_ERROR
	end
      | _ -> raise TYPE_ERROR
    end

(******************
 * Proof Validity *
 ******************)

let check (s : tscope) (g : tenv) (e : exp) (t: ty) : bool = tequal (infer s g e) t

(***********
 * Printing *
 ***********)

 let strip_outer_parens (s : string) : string =
   if String.length s <= 2 then s else
     let s1 = String.slice s 0 1 in
     let s2 = String.slice s ((String.length s) - 1) (String.length s) in
     if String.equal s1 "(" && String.equal s2 ")" then String.slice s 1 ((String.length s) - 1) else s

let rec ty_to_str (t : ty) : string = match t with
  | Unit -> "unit"
  | Sum(t1, t2) -> "(" ^ ty_to_str t1 ^ " + " ^ ty_to_str t2 ^ ")"
  | Prod(t1, t2) -> "(" ^ ty_to_str t1 ^ " × " ^ ty_to_str t2 ^ ")"
  | Fun(t1, t2) -> "(" ^ ty_to_str t1 ^ " → " ^ ty_to_str t2 ^ ")"
  | TVar(xt) -> "(" ^ xt ^ ")"
  | Forall(xt, t) -> "(∀ " ^ xt ^ ". " ^ ty_to_str t ^ ")"
  | Exists(xt, t) -> "(∃ " ^ xt ^ ". " ^ ty_to_str t ^ ")"
  | Says(t1, t2) -> "(" ^  ty_to_str t1 ^ " says " ^ ty_to_str t2 ^ ")"
  | Install(t) -> "(install(" ^ strip_outer_parens (ty_to_str t) ^ "))"
  | Reconfig(t) -> "(reconfig(" ^ strip_outer_parens (ty_to_str t) ^ "))"
  | Extend(t) -> "(extend(" ^ strip_outer_parens (ty_to_str t) ^ "))"
  | Program(p) -> "(" ^ p ^ ")"
  | Principal(a) -> "(" ^ a ^ ")"

let rec exp_to_str (e : exp) : string = match e with
| Bullet -> "bullet"
| Inl(e, t) -> "(inl(" ^ exp_to_str e ^ ") as . + " ^ ty_to_str t ^ ")"
| Inr(e, t) -> "(inr(" ^ exp_to_str e ^ ") as . + " ^ ty_to_str t ^ ")"
| Case(e, x1, e1, x2, e2) -> "(case(" ^ strip_outer_parens (exp_to_str e) ^ "){" ^ x1 ^ "." ^ exp_to_str e1 ^ "}{" ^ x2 ^ "." ^ exp_to_str e2 ^ "})"
| Pair(e1, e2) -> "(⟨" ^ exp_to_str e1 ^ ", " ^ exp_to_str e2 ^ "⟩)"
| Projl(e) -> "(projl(" ^ strip_outer_parens (exp_to_str e) ^ "))"
| Projr(e) -> "(projr(" ^ strip_outer_parens (exp_to_str e) ^ "))"
| Var(x) -> "(" ^ x ^ ")"
| Let(x, e1, e2) -> "(let " ^ x ^ " ≔ " ^ exp_to_str e1 ^ " in " ^ exp_to_str e2 ^ ")"
| Lambda(x, t, e) -> "(λ(" ^ x ^ ": " ^ ty_to_str t ^ "). " ^ exp_to_str e ^ ")"
| Apply(e1, e2) -> "(" ^ exp_to_str e1 ^ "(" ^ exp_to_str e2 ^ "))"
| BigLambda(xt, e) -> "(Λ" ^ xt ^ ". " ^ exp_to_str e ^ ")"
| TyApply(e, t) -> "(" ^ exp_to_str e ^ "[" ^ ty_to_str t ^ "])"
| Pack(t1, e, xt, t2) -> "(⟨*" ^ ty_to_str t1 ^ ", " ^ exp_to_str e ^ "⟩ as ∃" ^ xt ^ ". " ^ ty_to_str t2 ^ ")"
| Unpack(xt, x, e1, e2) -> "(let ⟨*" ^ xt ^ ", " ^ x ^ "⟩ = " ^ exp_to_str e1 ^ " in " ^ exp_to_str e2 ^ ")"
| Ret(a, e) -> "(ret^" ^ a ^ "(" ^ strip_outer_parens (exp_to_str e) ^ "))"
| Bind(x, e1, e2) -> "(" ^ x ^ " ← " ^ exp_to_str e1 ^ " ; " ^ exp_to_str e2 ^ ")"

let print_ty (t : ty) (newline : bool) : unit = if newline then print_endline(strip_outer_parens (ty_to_str t)) else print_string(strip_outer_parens (ty_to_str t))
let print_exp (e : exp) (newline : bool) : unit = if newline then print_endline(strip_outer_parens (exp_to_str e)) else print_string(strip_outer_parens (exp_to_str e))

(***********
 * IO *
 **********)

(* Container for easy storage of relvant nal variables *)
type nal_obj =
  { m_ty : Ty.t;
    m_exp : Exp.t;
    m_tscope : tscope;
    m_tenv : tenv;
    m_prog : string;
  }

(* Load NAL types from JSON file *)
let load_nal (f : string) =
  let rec load_tenv (m : tenv) l1 l2 =
  match l1, l2 with
  | [], _ -> m
  | _, [] -> m
  | hd1 :: tl1, hd2 :: tl2 -> load_tenv (StringMap.add hd1 hd2 m) tl1 tl2 in

  let json = Yojson.Basic.from_file f in
  let m_ty = json |> member "ty" |> to_string |> Ty.of_string in
  let m_exp = json |> member "exp" |> to_string |> Exp.of_string in
  let m_tscope = json |> member "tscope" |> to_list |> filter_string |> StringSet.of_list in
  let tenv_tmp = json |> member "tenv" |> to_list in
  let tenv_keys = List.map tenv_tmp ~f:(fun json -> member "key" json |> to_string) in
  let tenv_types = List.map tenv_tmp ~f:(fun json -> member "type" json |> to_string |>Ty.of_string) in
  let m_tenv = load_tenv StringMap.empty tenv_keys tenv_types in
  let m_prog = json |> member "prog" |> to_string in
{ m_ty; m_exp; m_tscope; m_tenv; m_prog };;

let read_process command =
  let open Unix in
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = String.create buffer_size in
  let in_channel = Unix.open_process_in command in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input in_channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  ignore (Unix.close_process_in in_channel);
  Buffer.contents buffer


module Checker = struct
  let spec = Command.Spec.(
      empty
      +> anon ("file" %: file)
  )

  let run file () =
    let obj = load_nal file in
    let open Core.Printf in
    try if check obj.m_tscope obj.m_tenv obj.m_exp obj.m_ty then printf("true\n") else printf("false\n")
    with TYPE_ERROR -> printf("false\n")
end

module Encrypt = struct
  let spec = Command.Spec.(
    empty
    +> anon ("file" %: file)
    +> anon ("out"  %: file)
  )

  let write_nal (obj : nal_obj) (dir : string) =
    let make_assoc key value =
      print_string ("Please enter the path to the private key for: " ^ (Ty.to_string value) ^ "\n");
      let key_path = read_line() in
      let prog = ("./sign.sh " ^ key_path ^ " " ^ "\"" ^ (Core.String.escaped ( Ty.to_string value)) ^ "\"") in
      let enc = read_process prog in
      `Assoc [ ( "key", `String key); ( "type", `String (Ty.to_string value)); ( "enc", `String (enc)) ] in

    let j_ty = `Assoc [ ("ty", `String (Ty.to_string obj.m_ty))] in
    let j_exp = `Assoc [ ("exp", `String (Exp.to_string obj.m_exp))] in
    (*let test = [1; 2; 3] in*)
    let j_tscope = `Assoc [("tscope", `List [])] in 
    let l = List.rev (StringMap.fold (fun k v z -> (make_assoc k v) :: z) obj.m_tenv []) in
    let j_tenv = `Assoc [ ("tenv", `List l)] in
    let prog = `Assoc [ ("prog", `String obj.m_prog)] in
    let open Yojson.Basic.Util in
    let out = combine (combine (combine (combine j_ty j_exp) j_tenv) j_tscope) prog in
    let oc = Core.open_out dir in
    let _ = Core.Printf.fprintf oc "%s\n" (Yojson.Basic.pretty_to_string out) in
    Core.close_out oc;;

  let run file out () =
    let obj = load_nal file in
    write_nal obj out
end

module Verify = struct
  let spec = Command.Spec.(
    empty
    +> anon ("file" %: file)
  )

  let check_sigs (f : string) =
    let rec check_sign l1 l2 =
      match l1, l2 with
      | [], _ -> true
      | _, [] -> true
      | hd1 :: tl1, hd2 :: tl2 -> let check = read_process ("./verify.sh " ^ "\"" ^ (Core.String.escaped (Ty.to_string hd1)) ^ "\"" ^ " " ^ hd2) in
        if (check = "true\n") then check_sign tl1 tl2 else (print_string check; false) in

    let json = Yojson.Basic.from_file f in
    let tenv_tmp = json |> member "tenv" |> to_list in
    let tenv_enc = List.map tenv_tmp ~f:(fun json -> member "enc" json |> to_string) in
    let tenv_types = List.map tenv_tmp ~f:(fun json -> member "type" json |> to_string |>Ty.of_string) in
    check_sign tenv_types tenv_enc;;


    let run file () = 
      let open Core.Printf in
      if check_sigs file then printf("true\n") else printf("false\n")
end

module Extract = struct
  let spec = Command.Spec.(
    empty
    +> anon ("file" %: file)
    +> anon ("progfile" %: file)
  )

  (* bob is network admin *)
  let bob = "FcC3bAPSaKshn7sJKyy6hyk5T1u5mthkmqx6tPJ81ug="

  let run file progfile () = 
    let json = Yojson.Basic.from_file file in
    let prog1_str = json |> member "prog" |> to_string in
    let prog1 : ty = Says((Principal bob), Install(Program(prog1_str))) in
    let prog2 : ty = Says((Principal bob), Extend(Program(prog1_str))) in
    let prog3 : ty = Says((Principal bob), Reconfig(Program(prog1_str))) in
    let ty = json |> member "ty" |> to_string |> Ty.of_string in
    let oc = Core.open_out progfile in
    let open Core.Printf in
    if (prog1 = ty || prog2 = ty || prog3 = ty) then (fprintf oc "%s\n" prog1_str; printf("true\n")) else printf("Error: Programs do not match");;
end

module ClientExtend = struct
    let spec = Command.Spec.(
        empty
        +> anon ("file" %: file)
    )

    let run outfile () = 
        let write_nal (obj : nal_obj) (dir : string) =
          let make_assoc key value =
            `Assoc [ ( "key", `String key); ( "type", `String (Ty.to_string value)) ] in
          let j_ty = `Assoc [ ("ty", `String (Ty.to_string obj.m_ty))] in
          let j_exp = `Assoc [ ("exp", `String (Exp.to_string obj.m_exp))] in
          let j_tscope = `Assoc [("tscope", `List [])] in 
          let l = List.rev (StringMap.fold (fun k v z -> (make_assoc k v) :: z) obj.m_tenv []) in
          let j_tenv = `Assoc [ ("tenv", `List l)] in
          let prog = `Assoc [ ("prog", `String obj.m_prog)] in
          let open Yojson.Basic.Util in
          let out = combine (combine (combine (combine j_ty j_exp) j_tenv) j_tscope) prog in
          let oc = Core.open_out dir in
          let _ = Core.Printf.fprintf oc "%s\n" (Yojson.Basic.pretty_to_string out) in
          Core.close_out oc in


      let m_prog = "(filter (not (switch = 346653522121)) or (switch = 346653522121 and ((vlanId=1000 and ip4Dst=104.16.65.50 and tcpDstPort=443) or (vlanId=1000 and ip4Dst=104.16.66.50 and tcpDstPort=443) or (vlanId=1001 and ip4Dst=89.30.121.150 and tcpDstPort=443) or (vlanId=1001 and ip4Dst=151.101.146.217 and tcpDstPort=443) or ipProto = 17)))" in

      let fedmem = "fgL5t8ra/C8tw00fIPslt51jbl3kNKQ/nejKTQJocZY=" in
      let bob =   "FcC3bAPSaKshn7sJKyy6hyk5T1u5mthkmqx6tPJ81ug=" in
      let alice = "qMtiPo/TjSKg6R3zPv+Q3EQTKVVf4Y3WyNyMywLeQ/Y=" in

      let t1 : ty = Says((Principal alice), Extend(Program(m_prog))) in

      let t2 : ty = Says((Principal bob),
		   Forall("X", Fun(Says((Principal fedmem), Extend(TVar("X"))),
				   Says((Principal bob), Extend(TVar("X")))))) in

      let t3 : ty = Says((Principal fedmem),
		   Forall("X", Fun(Says((Principal alice), TVar("X")),
				   Says((Principal fedmem), TVar("X"))))) in


      let m_exp : exp = Bind("bobconfig", Var("fedmem_cred"),
		       Apply(TyApply(Var("bobconfig"), Program(m_prog)),
			     Bind("config", Var("alice_cred"),
				  Apply(TyApply(Var("config"), Extend(Program(m_prog))),
					Var("extend_cred"))))) in

      let m_ty : ty = Says((Principal bob), Extend(Program(m_prog))) in

      let m_tscope : tscope = StringSet.empty in

      let m_tenv : tenv =
         StringMap.add "extend_cred" t1
          (StringMap.add "fedmem_cred" t2
            (StringMap.add "alice_cred" t3	       
              (StringMap.empty))) in
      let obj : nal_obj = {m_ty; m_exp; m_tscope; m_tenv; m_prog} in
      let _ = write_nal obj outfile in
      let open Core.Printf in
      printf "Stage1 written to %s\n" outfile;;		   
end

module ClientReconfig = struct
    let spec = Command.Spec.(
        empty
        +> anon ("file" %: file)
    )

    let run outfile () = 
        let write_nal (obj : nal_obj) (dir : string) =
          let make_assoc key value =
            `Assoc [ ( "key", `String key); ( "type", `String (Ty.to_string value)) ] in
          let j_ty = `Assoc [ ("ty", `String (Ty.to_string obj.m_ty))] in
          let j_exp = `Assoc [ ("exp", `String (Exp.to_string obj.m_exp))] in
          let j_tscope = `Assoc [("tscope", `List [])] in 
          let l = List.rev (StringMap.fold (fun k v z -> (make_assoc k v) :: z) obj.m_tenv []) in
          let j_tenv = `Assoc [ ("tenv", `List l)] in
          let prog = `Assoc [ ("prog", `String obj.m_prog)] in
          let open Yojson.Basic.Util in
          let out = combine (combine (combine (combine j_ty j_exp) j_tenv) j_tscope) prog in
          let oc = Core.open_out dir in
          let _ = Core.Printf.fprintf oc "%s\n" (Yojson.Basic.pretty_to_string out) in
          Core.close_out oc in

      
      let m_prog = "(filter (not (switch = 346653522121)) or (switch = 346653522121 and ((vlanId=1000 and ip4Dst=104.16.65.50 and tcpDstPort=443) or (vlanId=1000 and ip4Dst=104.16.66.50 and tcpDstPort=443) or ipProto = 17)))" in

		   
      let netco = "ADgUfXBPW5nfzj3CK6Qyqdvzi4HlTU3IaBXPYqINOa4=" in
      let bob =   "FcC3bAPSaKshn7sJKyy6hyk5T1u5mthkmqx6tPJ81ug=" in

      let t1 : ty = Says((Principal netco), Reconfig(Program(m_prog))) in

      let t2 : ty = Says((Principal bob),
		   Prod(Forall("X", Fun(Says((Principal netco), Reconfig(TVar("X"))),
					Says((Principal bob), Reconfig(TVar("X"))))),
		        Forall("X", Fun(Says((Principal netco), Extend(TVar("X"))),
					Says((Principal bob), Extend(TVar("X")))))))
		   in

      let m_exp : exp =  Bind("config", Var("delegate_cred"),
		           Apply(TyApply(Projl(Var("config")), Program(m_prog)), Var("reconfig_cred")))  in
      let m_ty : ty =  Says((Principal bob), Reconfig(Program(m_prog))) in

      let m_tscope : tscope = StringSet.empty in

      let m_tenv : tenv = StringMap.add "reconfig_cred" t1 (StringMap.add "delegate_cred" t2 (StringMap.empty)) in
      let obj : nal_obj = {m_ty; m_exp; m_tscope; m_tenv; m_prog} in
      let _ = write_nal obj outfile in
      let open Core.Printf in
      printf "Stage1 written to %s\n" outfile;;
end

module Test = struct
  let spec = Command.Spec.(
    empty
  )

  let run () =

      (* Alice says Hi example *)
      let t1 : ty = Forall("X", Fun(Says((Principal "Bob"), TVar("X")), Says((Principal "Alice"), TVar("X")))) in
      let t2 : ty = Says((Principal "Alice"), t1) in
      let t3 : ty = Says((Principal "Bob"), TVar("Hi")) in

      let tv : tvar = "Hi" in
      let s : tscope = StringSet.of_list [tv] in
      let g : tenv = StringMap.add "b_says" t3 (StringMap.add "a_says" t2 (StringMap.empty)) in

      let e : exp = Bind("x", Var("a_says"), Apply(TyApply(Var("x"), TVar("Hi")), Var("b_says"))) in

      let t : ty = Says((Principal "Alice"), TVar("Hi")) in

      (* Example from F says syntax paper *)
      let t1' : ty = Says((Principal "CEMS"), Forall("X", Fun(Says((Principal "Raymond"), Install(TVar("X"))), Says((Principal "CEMS"), Install(TVar("X")))))) in   (* changed Install("X") to TVar("X") *)
      let t2' : ty = Says((Principal "UIT"), Forall("X", Fun(Says((Principal "CEMS"), Install(TVar("X"))), Says((Principal "UIT"), Install(TVar("X")))))) in        (* changed Install("X") to TVar("X") *)
      let t3' : ty = Says((Principal "Raymond"), Install(Program("p"))) in
      let t4' : ty = Forall("X", Fun(Says((Principal "UIT"), TVar("X")), TVar("X"))) in

      let s' : tscope = StringSet.empty in
      let g' : tenv = StringMap.add "Perm_r" t1' (StringMap.add "Perm_u" t2' (StringMap.add "action" t3' (StringMap.add "run" t4' (StringMap.empty)))) in

      let e1 : exp = TyApply(Var("run"), Install(Program("p"))) in
      let e2 : exp = Apply(TyApply(Var("perm_u"), Program("p")), Var("action")) in
      let e3 : exp = Bind("perm_u", Var("Perm_r"), e2) in
      let e4 : exp = Apply(TyApply(Var("perm_r"), Program("p")), e3) in
      let e5 : exp = Bind("perm_r", Var("Perm_u"), e4) in
      let e6 : exp = Apply(e1, e5) in

      let t' : ty = Install(Program("p")) in

      (* IDEMPOTENCE *)
      let idem_t : ty = Forall("A", Forall("X", Fun(Says((Principal "A"), Says((Principal "A"), TVar("X"))), Says((Principal "A"), TVar("X"))))) in
      let idem : exp = BigLambda("A", BigLambda("X", Lambda("x", Says((Principal "A"), Says((Principal "A"), TVar("X"))), Bind("y", Var("x"), Var("y"))))) in

      (* HANDOFF *)
      let ba_speaksfor : ty = Forall("x", Fun(Says((Principal "B"), TVar("x")), Says((Principal "A"), TVar("x")))) in
       let handoff_t : ty = 
         Forall("A", Forall("B", Forall("X", 
            Fun((Says((Principal "B"), 
                  Fun(Says((Principal "A"), TVar("X")), Says((Principal "B"), TVar("X"))))),
                Fun(Says((Principal "A"), TVar("X")), Says((Principal "B"), TVar("X")))))))
      in 
      let handoff : exp = 
         BigLambda("A", BigLambda("B", BigLambda("X", 
            Lambda("y", Says((Principal "B"), Fun(Says((Principal "A"), TVar("X")), Says((Principal "B"), TVar("X")))), 
                Lambda("z", Says((Principal "A"), TVar("X")), Bind("w", Var("y"), Apply(Var("w"), Var("z")))))))) in 

      (* Alice says Hi *)
      print_endline("Alice says hi example:");
      if (check s g e t) then print_endline("\tthe proof holds :)") else print_endline("\tthe proof does not hold :(");
      print_ty (infer s g e) true;

      (* F says paper *)
      print_endline("F says paper example:");
      if (check s' g' e6 t') then print_endline("\tthe proof holds :)") else print_endline("\tthe proof does not hold :(");
      print_ty (infer s' g' e6) true;

      print_endline("idempotence:");
      if (check StringSet.empty StringMap.empty idem idem_t) then print_endline("\tthe idempotence term holds :)") else print_endline("\tthe idempotence term does not hold :(");
      print_ty (infer StringSet.empty StringMap.empty idem) true;

      print_endline("handoff:");
      if (check StringSet.empty StringMap.empty handoff handoff_t) then print_endline("\tthe handoff term holds :)") else print_endline("\tthe handoff term does not hold :(");
      print_ty (infer StringSet.empty StringMap.empty handoff) true;
end

let checker : Command.t = 
  Command.basic_spec
    ~summary:"Runs the NAL typechecker"
    Checker.spec
    Checker.run

let test : Command.t =
  Command.basic_spec
    ~summary:"Run Nal tests"
    Test.spec
    Test.run

let encrypt : Command.t =
  Command.basic_spec
    ~summary:"Signs elements in gamma"
    Encrypt.spec
    Encrypt.run

let verify : Command.t =
  Command.basic_spec
    ~summary:"Verifies all signatures in Gamma"
    Verify.spec
    Verify.run

let extract : Command.t =
  Command.basic_spec
    ~summary:"extracts the Netkat program from the wire format"
    Extract.spec
    Extract.run

    
let clientExtend : Command.t =
  Command.basic_spec
    ~summary:"Outputs stage1 example"
    ClientExtend.spec
    ClientExtend.run

let clientReconfig : Command.t =
    Command.basic_spec
      ~summary:"todo"
      ClientReconfig.spec
      ClientReconfig.run
    
let main : Command.t =
  Command.group
    ~summary:"todo"
[("checker", checker); ("test", test); ("encrypt", encrypt); ("verify", verify); ("extract", extract);
("clientExtend", clientExtend); ("clientReconfig", clientReconfig)]

let () = Command.run main
