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

auto check (std::tuple<bool> const& in_var)
{
  /* Generated out_var declaration */
  std::tuple<bool> out_var;

  /* Generated structured bindings */
  auto & [OK] = out_var;
  auto & [x]  = in_var;

  /* Generated pre_x declarations */
  static int pre_n1 = 0;
  static int pre_n2 = 0;

  int n1, n2;

  //n1 = 0 -> pre n1 + 1;
  //n2 = 1 -> pre n2 + 1;
  OK = (n1 + 1) == n2;

  /* Generated pre_x */
  pre_n1 = n1;
  pre_n2 = n2;

  return out_var;
}
