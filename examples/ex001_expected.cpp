/* Include this file by default */
#include <tuple>
#include <memory>

//#include <lustre.hpp>

/*  Base minilustre code

node check (x: bool) returns (OK: bool);
var n1, n2: int;
let
  n1 = 0 -> pre(n1) + 1;
  n2 = 1 -> pre(n2) + 1;
  OK = (n1 + 1) = n2;
tel

*/

template<typename T>
inline auto pre(T & sto, T const& nval)
{
  auto ret = sto;
  sto = nval;
  return ret;
}

template<typename T>
inline auto fby(bool & branched, T const& first, T const& then)
{
  return branched ? then : (branched = true, first);
}

auto get_check()
{
  auto lam =
    //  Local variables
    [ n1 = int{0}
    , n2 = int{0}

    //  followed_by
    //
    //  Les noms sont générés par le compilateur, ils doivent être *uniques* et
    //  ne pas être en conflits avec les noms des variables existantes.
    , _fby_1 = false
    , _fby_2 = false

    //  pre
    //
    //  Pareil que pour followed_by
    , _pre_1 = int{0}
    , _pre_2 = int{1}

    ] (std::tuple<bool> const& in_var)
    //  Le mot-clé 'mutable' permet d'attacher un environnement aux lambdas
    //  retournées par get_<node>.
    mutable
  {
    /* Generated 'out' declaration */
    std::tuple<bool> out;

    /* Generated structured bindings */
    auto & [OK] = out;
    auto & [x] = in_var;
    (void)x;

    //  n1 = 0 -> pre(n1) + 1;
    n1 = fby(_fby_1, 0, pre(_pre_1, n1) + 1);
    //  n2 = 1 -> pre(n2) + 1;
    n2 = fby(_fby_2, 1, pre(_pre_2, n2) + 1);
    //  OK = (n1 + 1) = n2;
    OK = (n1 + 1) == n2;

    return out;
  };

  return lam;
}

int main(int, char const *[])
{
  auto check = get_check();
  (void)check;
  return 0;
}
