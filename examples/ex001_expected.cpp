/* Include this file by default */
#include <tuple>
#include <memory>

/*  Base minilustre code

node check (x: bool) returns (OK: bool);
var n1, n2: int;
let
  n1 = 0 -> pre(n1) + 1;
  n2 = 1 -> pre(n2) + 1;
  OK = (n1 + 1) = n2;
tel

*/

auto var = [](auto& var) { return [=]() mutable { return var; }; };
auto con = [](auto  val) { return [=]() { return val; }; };

/* TODO : complete this one */
auto add = [](auto e1, auto e2) { return e1() + e2(); };
auto sub = [](auto e1, auto e2) { return e1() + e2(); };

auto pre = [](auto exp)
{
  auto val = exp();
  return [=]() mutable
  {
    auto ret = val;
    val = exp();
    return ret;
  };
};

auto fby = [](auto first, auto then)
{
  auto val = first();
  return [=]() mutable
  {
    auto ret = val;
    val = then();
    return ret;
  };
};

auto get_check ()
{
  int n1, n2;

  auto n1_var = var(n1);
  auto n2_var = var(n2);

  //  Generating functions & captures here
  auto get_n1 = fby(con(0), add(pre(n1_var), con(1)));
  auto get_n2 = fby(con(1), add(pre(n2_var), con(1)));

  auto lam = [n1 = std::move(n1), n2](std::tuple<bool> const& in_var) mutable -> std::tuple<bool>
  {
    /* Generated out_var declaration */
    std::tuple<bool> out_var;

    /* Generated structured bindings */
    auto & [OK] = out_var;
    auto & [x]  = in_var;

    n1 = get_n1();
    n2 = get_n2();
    OK = (n1 + 1) == n2;

    return out_var;
  };

  return lam;
}

int main(int, char const *[])
{
  return 0;
}
