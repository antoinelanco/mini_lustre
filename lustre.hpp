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

template<typename E1, typename E2> auto Op_add(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) + ci(e2); }; };
template<typename E1, typename E2> auto Op_sub(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) - ci(e2); }; };
template<typename E1, typename E2> auto Op_mul(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) * ci(e2); }; };
template<typename E1, typename E2> auto Op_div(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) / ci(e2); }; };
template<typename E1, typename E2> auto Op_mod(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) % ci(e2); }; };

template<typename E1, typename E2> auto Op_eq(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) == ci(e2); }; };
template<typename E1, typename E2> auto Op_neq(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) != ci(e2); }; };
template<typename E1, typename E2> auto Op_lt(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) <  ci(e2); }; };
template<typename E1, typename E2> auto Op_le(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) <= ci(e2); }; };
template<typename E1, typename E2> auto Op_gt(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) >  ci(e2); }; };
template<typename E1, typename E2> auto Op_ge(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) >= ci(e2); }; };

template<typename E> auto Op_not(E const& e)
{ return [=](){ return !ci(e); }; };

template<typename E1, typename E2> auto Op_and(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) && ci(e2); }; };
template<typename E1, typename E2> auto Op_or(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) || ci(e2); }; };
template<typename E1, typename E2> auto Op_impl(E1 const& e1, E2 const& e2)
{ return [=](){ return !ci(e1) || ci(e2); }; };

template<typename C, typename R>
auto Op_if(C const& c, R const& t, R const& f)
{ return [=](){ return ci(c) ? ci(t) : ci(f); }; };

template<typename E> auto pre(E e)
{
  auto val = ci(e);
  return [=]() mutable
  {
    auto ret = val;
    val = ci(e);
    return ret;
  };
}

template<typename First, typename Then> auto fby(First first, Then then)
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
