/* Include this file by default */
#include <tuple>
#include <memory>

#include <lustre.hpp>

/*  Base minilustre code

node check (x: bool) returns (OK: bool);
var n1, n2: int;
let
  n1 = 0 -> pre(n1) + 1;
  n2 = 1 -> pre(n2) + 1;
  OK = (n1 + 1) = n2;
tel

*/


auto get_check()
{
  using namespace std;
  using namespace lustre;

  int n1, n2;

  auto n1_ref = reference_wrapper(n1);
  auto n2_ref = reference_wrapper(n2);

  auto get_n1 = fby(0, Op_add(pre(n1_ref), 1));   //  n1 = 0 -> pre(n1) + 1;
  auto get_n2 = fby(1, Op_add(pre(n2_ref), 1));   //  n2 = 1 -> pre(n2) + 1;
  auto get_OK = Op_eq(Op_add(n1_ref, 1), n2_ref); //  OK = (n1 + 1) = n2;

  auto lam =  [ = //  Capturing variables
              //  Refreshing references
              , n1_ref = reference_wrapper(n1)
              , n2_ref = reference_wrapper(n2)
              ] (std::tuple<bool> const& in_var) mutable
  {
    /* Generated out_var declaration */
    std::tuple<bool> out_var;

    /* Generated structured bindings */
    auto & [OK] = out_var;
    auto & [x]  = in_var;

    n1 = get_n1();
    n2 = get_n2();
    OK = get_OK();

    return out_var;
  };

  return lam;
}

int main(int, char const *[])
{
  auto check = get_check();
  (void)check;
  return 0;
}
