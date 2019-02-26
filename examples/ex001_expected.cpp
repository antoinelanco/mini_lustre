/* Include this file by default */
#include <tuple>

/*  Base minilustre code

node check (x: bool) returns (OK: bool);
var n1, n2: int;
let
  n1 = 0 -> pre n1 + 1;
  n2 = 1 -> pre n2 + 1;
  OK = (n1 + 1) = n2;
tel

*/

std::tuple<bool> check (std::tuple<bool> const& in_var)
{
  /* Generated out_var declaration */
  std::tuple<bool> out_var;

  /* Generated structured bindings */
  auto & [OK] = out_var;
  auto & [x]  = in_var;

  int n1, n2;

  static int pre_n1 = 0;  /* Generated to manage followed_by ("->") and pre */
  n1 = pre_n1 + 1;
  static int pre_n2 = 1;  /* Generated to manage followed_by ("->") and pre */
  n2 = pre_n2 + 1;

  OK = (n1 + 1) == n2;

  /* Generated pre_x */
  pre_n1 = n1;
  pre_n2 = n2;

  return out_var;
}
