#pragma once

#include <type_traits>

namespace lustre {  //  lustre

//  Conditionnal invoke
template<typename T>
auto ci(T t)
{
  if constexpr (std::is_invocable<T>()) return t();
  else return t;
}

auto Op_add = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) + ci(e2); };
};

auto Op_sub = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) - ci(e2); };
};


auto Op_mul = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) * ci(e2); };
};

auto Op_div = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) / ci(e2); };
};

auto Op_mod = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) % ci(e2); };
};


auto Op_eq  = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) == ci(e2); };
};

auto Op_neq = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) != ci(e2); };
};

auto Op_lt  = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) <  ci(e2); };
};

auto Op_le  = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) <= ci(e2); };
};

auto Op_gt  = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) >  ci(e2); };
};

auto Op_ge  = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) >= ci(e2); };
};


auto Op_not = [](auto const& e)
{
  return [=](){ return !ci(e); };
};


auto Op_and  = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) && ci(e2); };
};

auto Op_or   = [](auto const& e1, auto const& e2)
{
  return [=](){ return ci(e1) || ci(e2); };
};

auto Op_impl = [](auto const& e1, auto const& e2)
{
  return [=](){ return !ci(e1) || ci(e2); };
};

auto Op_if = [](auto const& c, auto const& t, auto const& f)
{ return [=](){ return ci(c) ? ci(t) : ci(f); }; };

template<typename Exp>
auto pre(Exp exp)
{
  auto val = ci(exp);
  return [=]() mutable
  {
    auto ret = val;
    val = ci(exp);
    return ret;
  };
}

template<typename First, typename Then>
auto fby(First first, Then then)
{
  auto val = ci(first);
  return [=]() mutable
  {
    auto ret = val;
    val = ci(then);
    return ret;
  };
}

} //  ! lustre
