{-
// example from the C23 draft standard N3088 page 78.
// this valid C23 procedure 'f' returns 1. thus, it is fully equivalent
// to this function: int f_(void) { return 1; }
// which is because a compound literal is only created once, as the
// standard itself says.

struct s {
  int i;
};
int f(void) {
  struct s *p = 0, *q;
  int j = 0;
again:
  q = p, p = &((struct s){j++});
  if (j < 2) goto again;
  return p == q && q->i == 1;
}
-}

-- x y sto <-> storage (place) for a value of type x with the
--  substructural rule y (here ordered logic 'ord' used), requiring
--  terms to be used allocated and deallocated in LIFO order, and
--  used exactly once in its lifetime.
--
-- the type void:
--   void <-> true 'void' type as in the value cannot be constructed.
--            also denoted as '!'.
--
-- polymorphism:
--  a b c ... x y z t -> type 't' has parameters a b c ... x y z.
--
-- some primops:
--  al, de -- (de)allocate stack, bytes
--  al :: forall t. nat -> t ord sto
--  de :: forall t. t ord sto -> (() -> void) -> void
--
--  rvalue -- materialize a constant
--  rvalue :: forall t r. t -> (t r sto -> void) -> void
--
--  move -- copy a value, destination, then source.
--  move :: forall t r. t r sto -> t r sto -> (t r sto -> t r sto -> !) -> !
--
--  address -- dereference a place (param 2) and store address into place (param 1).
--  address :: forall t r s. (t s ptr) r sto -> t s sto -> ((t s ptr) r sto -> t s sto -> !) -> !
--
--  equal, inc, less -- arithmetic, control flow.
--  equal :: forall t r. t r sto -> t r sto -> (t r sto -> t r sto -> !)
--                                          -> (t r sto -> t r sto -> !) -> !
--  (etc.; notation should be enhanced to represent that each 'sto' term
--   can only be used exactly once in either branch. currently defective.)
--
--  cast :: forall a b. a -> b -- very unsafe primop.
--
-- general rules:
--  - functions are first-class; values are second-class.
--  - functions are represented at runtime by their addresses.
--  - no term has a representation. all terms are phantom and zero-sized.
--  - thus, any immediate value must be given a place using the primop
--    'rvalue.' a trait exists for all applicative types to overload 'rvalue.'
--    but it's notably missing for 'void.'
--  - a term of type 'void' can be formally produced and handled as usual
--    but its *value* cannot be constructed (no function can store it).
--  - integral literals are generally int but they are cast as needed.
--    in this cleaned-up dump the casts are omitted.
--  - here 'deref_s_i' can be assumed to be compiler generated and exists
--    somewhere else.
f k = al (cast 4) \p -> al (cast 4) \q -> al (cast 4) \j ->
  rvalue (cast 0) \zero ->
  move p zero \p zero ->
  move j (cast zero) \j zero ->
  de zero \() -> f' p q j k
f' p q j k =
  -- C23 standard says: a compound literal is created only once.
  al (cast 4) \s ->
  f'' p q j s (\p q j s ->
    de s \() ->
    let joinpoint1 p q j n = de j \() -> de q \() -> de p \() -> k n in
    equal p q
      (\p q -> deref_s_i q \q -> rvalue (cast 1) \one ->
        equal q one
          -- (rvalue (cast 1) \ret -> joinpoint1 p q j ret), but eta-reduced.
          (rvalue (cast 1) (joinpoint p q j))
          (rvalue (cast 0) (joinpoint p q j)))
      (\p q -> rvalue (cast 0) (joinpoint1 p q j)))
f'' p q j s k =
  move q p \q p ->
  address p s \p s ->
  move s j \s j ->
  inc j \j ->
  rvalue (cast 2) \two ->
  less j two
    (\j two -> de two \() -> f'' p q j s k)
    (\j two -> de two \() -> k p q j s)

-- goal: find optimization rules that help reduce f into:
--  f = rvalue 1
-- in other words,
--  f k = rvalue 1 \ret -> k ret
