
f k = al (cast 4) \p -> al (cast 4) \q -> al (cast 4) \j ->
  rvalue (cast 0) \zero ->
  move p zero \p zero ->
  move j (cast zero) \j zero ->
  de zero \() ->
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
  move q p \q p ->      -- we know that q = p now.
  address p s \p s ->
  move s j \s j ->
  inc j \j ->
  rvalue (cast 2) \two ->
  less j two
    (\j two -> de two \() -> f'' p q j s k)
    (\j two -> de two \() -> k p q j s)

-- evaluate f'' p q j s k:
f'' p q j s k =
  move q p \q p -> -- now we know that q and p store the same value of zero.
                   -- but p is used independently, let's see.
  address p s \p s -> -- ok, the same value of s will be used for all iterations.
  move s j \s j -> -- because 'move' may not alter the identity of s;
                   -- nothing can. also, now s holds 0.
  inc j \j ->      -- so previously this stored 0; now it stores 1.
  rvalue (cast 2) \two ->
    less j two     -- comparing 1 and 2, 1 is indeed less.
    (\j two -> de two \() -> f'' p q j s k) -- now q has 0, p has &s, j has 1.

f'' p q j s k =
  move q p \p q -> -- now p and q both store &s.
  address p s \p s -> -- p still stores &s, as s cannot be altered.
  move s j \s j -> -- so s now stores 1.
  inc j \j ->      -- so j now stores 2.
  rvalue (cast 2) \two ->
    less j two     -- this is false now.
    (\j two -> ...) -- skip.
    (\j two -> de two \() -> k p q j s) -- ok, jump.

-- now we can replace f'' p q j s k with
f'' p q j s k =
  rvalue (cast 1) \one -> move s one \s one -> de one \() ->
  rvalue (cast 2) \two -> move j two \j two -> de two \() ->
  address p s \p s ->
  move q p \q p ->
  k p q j s

-- at this point, it might make more sense to alter 'rvalue' so that
-- it requires a previously existing place, and it merely moves the
-- constant to that place.

-- revised program:
-- also let's remove all the annoying casts,
-- and let al be called without the size.

f k = al \p -> al \q -> al \j ->
  rvalue p 0 \p ->
  rvalue j 0 \j ->
  al \s ->
  f'' p q j s \p q j s ->
    de s \() ->
    let join1 p q j n = de j \() -> de q \() -> de p \() -> k n in
    equal p q
      (\p q -> deref_s_i q \q -> al \t -> rvalue t 1 \t ->
        equal q t
          (cast t (join1 p q j))
          (move t 0 (join1 p q j)))
      (\p q -> move t 0 (join1 p q j))
f'' p q j s k =
  move q p \q p ->
  address p s \p s ->
  move s j \s j ->
  inc j \j ->
  al \t -> rvalue 2 t \t ->
  less j t
    (\j t -> de t \() -> f'' p q j s k)
    (\j t -> de t \() -> k p q j s)

-- here's a problem with 'move', it clobbers the storage and prevents
-- optimization. we need to convert it to SSA form. for this, we can
-- create a copy.

f'' p q j s k =
  al \r -> move r p \r p ->
  address p s \p s -> -- this one too.
    ...

-- define this helper function
copy v k = al \w -> move w v \w v -> de v \() -> k w

-- now,

f'' p q j s k =
  copy p \q ->
  al \t -> address t s -- etc.

-- getting a feel. i think we should prohibit assigning to a place
-- that's been already used. so
--  forall t.
--    t r fresh sto
--  turns into
--    t r used sto
--  which can then be deallocated.

f'' p q j s k =
  de q \() ->
  copy p \q ->
  al \t ->
  address t s \t s -> -- t used, q fresh.
  move s j \s j -> -- can't easily be unclobbered.
  -- ... about to rediscover phi nodes.
