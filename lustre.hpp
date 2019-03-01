#pragma once

#include <type_traits>

namespace lustre {  //  lustre

//  Conditionnal invoke

template<typename T>
inline auto ci(T t)
{
  if constexpr (std::is_invocable<T>()) return t();
  else return t;
}

//  Lustre operators

////  Arithmetic

template<typename E1, typename E2>
inline auto Op_add(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) + ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_sub(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) - ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_mul(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) * ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_div(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) / ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_mod(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) % ci(e2); }; };

////  Logic

template<typename E1, typename E2>
inline auto Op_eq(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) == ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_neq(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) != ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_lt(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) <  ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_le(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) <= ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_gt(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) >  ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_ge(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) >= ci(e2); }; };

template<typename E>
inline auto Op_not(E const& e)
{ return [=](){ return !ci(e); }; };

template<typename E1, typename E2>
inline auto Op_and(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) && ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_or(E1 const& e1, E2 const& e2)
{ return [=](){ return ci(e1) || ci(e2); }; };
template<typename E1, typename E2>
inline auto Op_impl(E1 const& e1, E2 const& e2)
{ return [=](){ return !ci(e1) || ci(e2); }; };

////  Branching

template<typename C, typename R>
inline auto Op_if(C const& c, R const& t, R const& f)
{ return [=](){ return ci(c) ? ci(t) : ci(f); }; };

////  Previous state operators

template<typename E>
inline auto pre(E e)
{
  auto val = ci(e);
  return [=]() mutable
  {
    auto ret = val;
    val = ci(e);
    return ret;
  };
}

template<typename First, typename Then>
inline auto fby(First first, Then then)
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
