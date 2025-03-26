// volatile int n = 1.
let "n": (volatile; int) obj = int 1;
let "p": (int ptr) obj = cast (int ptr) (ref "n");
let "val": int obj = deref "p"; // undefined behavior.

// struct s {int i; const int ci;} s;
// volatile struct s vs;

let struct "s": struct type = record {
  "i" : int obj,
  "ci" : (const; int) obj,
}, and "s" : (struct "s") obj;
let "vs": (volatile; struct "s") obj;

// void f(double x[volatile], const double y[volatile]);
let "f" : fn("x" : (volatile; double obj;) array,
             "y" : (volatile; (volatile; double) obj;) array);

/*
struct v
{
   union // anonymous union
   {
       struct { int i, j; }; // anonymous structure
       struct { long k, l; } w;
   };
   int m;
} v1;
*/

// AST ppr format experiment #1.
let struct "v": struct type =
  let @0: union type =
    let @1: struct type = record {
      "i" : int obj,
      "j" : int obj,
    } in
    let @2: struct type = record {
      "k" : long obj,
      "l" : long obj,
    } in
    record {
      @3 : (struct @1) obj,
      @4 : (struct @2) obj,
    }
  in
  record {
    @5 : @0 obj,
    "m" : int obj,
  }, and "v1" : (struct "v") obj;

// AST ppr format experiment #2.
// This time, let-in inside record.
let struct "v": struct type = record {
  let @0: union type = record {
    let @1: struct type = record {
      "i" : int obj,
      "j" : int obj,
    } in @2 : @1 obj,
    let @3: struct type = record {
      "k" : long obj,
      "l" : long obj,
    } in "w" : @3 obj,
  } in @1: @0 obj,
  "m" : int obj,
}, and "v1" : (struct "v") obj;
