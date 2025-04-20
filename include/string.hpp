#ifndef STRIGNITE_STRING_HPP
#define STRIGNITE_STRING_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1600)
#  pragma execution_character_set("utf-8")
#endif

#if defined(_MSC_VER)
#  define STRIGNITE_CPLUSPLUS _MSVC_LANG
#elif defined(__GNUC__) || defined(__clang__)
#  define STRIGNITE_CPLUSPLUS __cplusplus
#endif

#define STRIGNITE_CPLUSPLUS_20 STRIGNITE_CPLUSPLUS >= 202002L
#define STRIGNITE_CPLUSPLUS_17 STRIGNITE_CPLUSPLUS >= 201703L

#include <list>
#include <deque>
#include <vector>
#include <string>
#include <atomic>
#include <random>
#include <cassert>
#include <climits>
#include <cstdint>
#include <cstring>
#include <stdexcept>

#if STRIGNITE_CPLUSPLUS_17
#  include <string_view>
#endif

#define STRIGNITE_LITTLE_ENDIAN 1
#define STRIGNITE_BIG_ENDIAN 0

#ifndef STRIGNITE_ENDIAN
#  ifdef __BYTE_ORDER__
#    if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#      define STRIGNITE_ENDIAN STRIGNITE_LITTLE_ENDIAN
#    elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#      define STRIGNITE_ENDIAN STRIGNITE_BIG_ENDIAN
#    else
#      error Unknown machine endianness detected. User needs to define STRIGNITE_ENDIAN.
#    endif
#  elif defined(__GLIBC__)
#    include <endian.h>
#    if (__BYTE_ORDER == __LITTLE_ENDIAN)
#      define STRIGNITE_ENDIAN STRIGNITE_LITTLE_ENDIAN
#    elif (__BYTE_ORDER == __BIG_ENDIAN)
#      define STRIGNITE_ENDIAN STRIGNITE_BIG_ENDIAN
#    else
#      error Unknown machine endianness detected. User needs to define STRIGNITE_ENDIAN.
#    endif
#  elif defined(_LITTLE_ENDIAN) && !defined(_BIG_ENDIAN)
#    define STRIGNITE_ENDIAN STRIGNITE_LITTLE_ENDIAN
#  elif defined(_BIG_ENDIAN) && !defined(_LITTLE_ENDIAN)
#    define STRIGNITE_ENDIAN STRIGNITE_BIG_ENDIAN
#  elif defined(__sparc) || defined(__sparc__) || defined(_POWER) || defined(__powerpc__) || defined(__ppc__) || defined(__ppc64__) || defined(__hpux) || defined(__hppa) || defined(_MIPSEB) || defined(_POWER) || defined(__s390__)
#    define STRIGNITE_ENDIAN STRIGNITE_BIG_ENDIAN
#  elif defined(__i386__) || defined(__alpha__) || defined(__ia64) || defined(__ia64__) || defined(_M_IX86) || defined(_M_IA64) || defined(_M_ALPHA) || defined(__amd64) || defined(__amd64__) || defined(_M_AMD64) || defined(__x86_64) || defined(__x86_64__) || defined(_M_X64) || defined(__bfin__)
#    define STRIGNITE_ENDIAN STRIGNITE_LITTLE_ENDIAN
#  elif defined(_MSC_VER) && (defined(_M_ARM) || defined(_M_ARM64))
#    define STRIGNITE_ENDIAN STRIGNITE_LITTLE_ENDIAN
#  else
#    error Unknown machine endianness detected. User needs to define STRIGNITE_ENDIAN.
#  endif
#endif

#define STRIGNITE_IS_LITTLE_ENDIAN STRIGNITE_ENDIAN == STRIGNITE_LITTLE_ENDIAN

#ifdef _MSC_VER
#  define STRIGNITE_FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#  define STRIGNITE_FORCE_INLINE __inline__ __attribute__((always_inline))
#else
#  define STRIGNITE_FORCE_INLINE inline
#endif

#if STRIGNITE_CPLUSPLUS_20
#  define STRIGNITE_EXPORT
#else
#  define STRIGNITE_EXPORT
#endif

#if STRIGNITE_CPLUSPLUS_17
#  define STRIGNITE_FALLTHROUGH [[fallthrough]]
#else
#  if defined(_MSC_VER)
#    define STRIGNITE_FALLTHROUGH
#  elif defined(__GNUC__) || defined(__clang__)
#    define STRIGNITE_FALLTHROUGH __attribute__((fallthrough))
#  endif
#endif

#if defined(__GNUC__) || defined(__clang__)
#  define STRIGNITE_LIKELY(expr) __builtin_expect(!!(expr), 1)
#  define STRIGNITE_UNLIKELY(expr) __builtin_expect(!!(expr), 0)
#else
#  define STRIGNITE_LIKELY(expr) expr
#  define STRIGNITE_UNLIKELY(expr) expr
#endif

#if STRIGNITE_CPLUSPLUS >= 202002L
#  define STRIGNITE_IF_LIKELY(expr) if (expr) [[likely]]
#  define STRIGNITE_IF_UNLIKELY(expr) if (expr) [[unlikely]]
#else
#  if defined(__GNUC__) || defined(__clang__)
#    define STRIGNITE_IF_LIKELY(expr) if (STRIGNITE_LIKELY(expr))
#    define STRIGNITE_IF_UNLIKELY(expr) if (STRIGNITE_UNLIKELY(expr))
#  else
#    define STRIGNITE_IF_LIKELY(expr) if (expr)
#    define STRIGNITE_IF_UNLIKELY(expr) if (expr)
#  endif
#endif

#ifndef NDEBUG
#  define STRIGNITE_ASSUME(expr) assert(expr);
#else
#  if __has_cpp_attribute(assume) >= 202207L
#    define STRIGNITE_ASSUME(expr) [[assume(expr)]];
#  else
#    if defined(_MSC_VER)
#      define STRIGNITE_ASSUME(expr) __assume(expr);
#    elif defined(__clang__)
#      define STRIGNITE_ASSUME(expr) __builtin_assume(expr);
#    elif defined(__GNUC__)
#      if __GNUC__ >= 13
#        define STRIGNITE_ASSUME(expr) __attribute__((assume(expr)));
#      else
#        define STRIGNITE_ASSUME(expr) if (!expr) { __builtin_unreachable(); }
#      endif
#    else
#      define STRIGNITE_ASSUME(expr)
#    endif
#  endif
#endif

#if STRIGNITE_CPLUSPLUS >= 202002L
#  define STRIGNITE_CPP20_CONSTEXPR constexpr
#else
#  define STRIGNITE_CPP20_CONSTEXPR
#endif

#if STRIGNITE_CPLUSPLUS_17
#  define STRIGNITE_CPP17_CONSTEXPR constexpr
#else
#  define STRIGNITE_CPP17_CONSTEXPR
#endif

#if STRIGNITE_CPLUSPLUS >= 201402L
#  define STRIGNITE_CPP14_CONSTEXPR constexpr
#else
#  define STRIGNITE_CPP14_CONSTEXPR
#endif

#if STRIGNITE_CPLUSPLUS >= 201103L
#  define STRIGNITE_CPP11_CONSTEXPR constexpr
#else
#  define STRIGNITE_CPP11_CONSTEXPR
#endif

#if STRIGNITE_CPLUSPLUS_17
#  define STRIGNITE_CPP17_NODISCARD [[nodiscard]]
#else
#  define STRIGNITE_CPP17_NODISCARD
#endif

#if __cpp_if_consteval >= 202106L
#  define STRIGNITE_IF_NOT_CONSTANT_EVALUATED(...) if !consteval { __VA_ARGS__ ; }
#elif STRIGNITE_CPLUSPLUS >= 202002L
#  define STRIGNITE_IF_NOT_CONSTANT_EVALUATED(...) STRIGNITE_IF_LIKELY(!std::is_constant_evaluated()) { __VA_ARGS__ ; }
#else
#  define STRIGNITE_IF_NOT_CONSTANT_EVALUATED(...) __VA_ARGS__ ;
#endif

#if __cpp_if_consteval >= 202106L
#  define STRIGNITE_IF_CONSTANT_EVALUATED(...) if consteval { __VA_ARGS__ ; }
#  define STRIGNITE_ELSE_NOT_CONSTANT_EVALUATED(...) else { __VA_ARGS__ ; }
#elif STRIGNITE_CPLUSPLUS >= 202002L
#  define STRIGNITE_IF_CONSTANT_EVALUATED(...) STRIGNITE_IF_UNLIKELY(std::is_constant_evaluated()) { __VA_ARGS__ ; }
#  define STRIGNITE_ELSE_NOT_CONSTANT_EVALUATED(...) else { __VA_ARGS__ ; }
#else
#  define STRIGNITE_IF_CONSTANT_EVALUATED(...)
#  define STRIGNITE_ELSE_NOT_CONSTANT_EVALUATED(...) __VA_ARGS__ ;
#endif

namespace bcs {
template<typename char_type, typename = std::char_traits<char_type>>
class basic_string_view;
using string_view = basic_string_view<char>;
#if STRIGNITE_CPLUSPLUS >= 202002L
using u8string_view = basic_string_view<char8_t>;
#endif
using wstring_view = basic_string_view<wchar_t>;
using u16string_view = basic_string_view<char16_t>;
using u32string_view = basic_string_view<char32_t>;

template<typename char_type, typename = std::char_traits<char_type>>
class basic_string;
using string = basic_string<char>;
#if STRIGNITE_CPLUSPLUS >= 202002L
using u8string = basic_string<char8_t>;
#endif
using wstring = basic_string<wchar_t>;
using u16string = basic_string<char16_t>;
using u32string = basic_string<char32_t>;

template<typename char_type, typename = std::char_traits<char_type>>
class basic_mutable_string;
using mutable_string = basic_mutable_string<char>;
#if STRIGNITE_CPLUSPLUS >= 202002L
using mutable_u8string = basic_mutable_string<char8_t>;
#endif
using mutable_wstring = basic_mutable_string<wchar_t>;
using mutable_u16string = basic_mutable_string<char16_t>;
using mutable_u32string = basic_mutable_string<char32_t>;

template<size_t, typename char_type, typename = std::char_traits<char_type>>
class basic_static_string;
template<size_t Capacity>
using static_string = basic_static_string<Capacity, char>;
#if STRIGNITE_CPLUSPLUS >= 202002L
template<size_t Capacity>
using static_u8string = basic_static_string<Capacity, char8_t>;
#endif
template<size_t Capacity>
using static_wstring = basic_static_string<Capacity, wchar_t>;
template<size_t Capacity>
using static_u16string = basic_static_string<Capacity, char16_t>;
template<size_t Capacity>
using static_u32string = basic_static_string<Capacity, char32_t>;

template<size_t, typename char_type, typename = std::char_traits<char_type>>
class basic_static_mutable_string;
template<size_t Capacity>
using static_mutable_string = basic_static_mutable_string<Capacity, char>;
#if STRIGNITE_CPLUSPLUS >= 202002L
template<size_t Capacity>
using static_mutable_u8string = basic_static_mutable_string<Capacity, char8_t>;
#endif
template<size_t Capacity>
using static_mutable_wstring = basic_static_mutable_string<Capacity, wchar_t>;
template<size_t Capacity>
using static_mutable_u16string = basic_static_mutable_string<Capacity, char16_t>;
template<size_t Capacity>
using static_mutable_u32string = basic_static_mutable_string<Capacity, char32_t>;

namespace internal {
#pragma region utility

template<typename...>
struct is_char {};

template<typename... T>
struct is_char<char, T...> : is_char<T...> {};

template<typename... T>
struct is_char<wchar_t, T...> : is_char<T...> {};

template<typename... T>
struct is_char<char16_t, T...> : is_char<T...> {};

template<typename... T>
struct is_char<char32_t, T...> : is_char<T...> {};

#if STRIGNITE_CPLUSPLUS >= 202002L
template<typename... T>
struct is_char<char8_t, T...> : is_char<T...> {};
#endif

template<>
struct is_char<char> {
    using type = void;
};

template<>
struct is_char<wchar_t> {
    using type = void;
};

template<>
struct is_char<char16_t> {
    using type = void;
};

template<>
struct is_char<char32_t> {
    using type = void;
};

#if STRIGNITE_CPLUSPLUS >= 202002L
template<>
struct is_char<char8_t> {
    using type = void;
};
#endif

template<typename... T>
using is_char_t = typename is_char<T...>::type;

template<typename, typename...>
struct is_container {};

template<typename T, typename... Ts>
struct is_container<std::deque<T, Ts...>> {
    using type = void;
};

template<typename T, typename... Ts>
struct is_container<std::vector<T, Ts...>> {
    using type = void;
};

template<typename T, typename... Ts>
struct is_container<std::list<T, Ts...>> {
    using type = void;
};

template<typename T, typename... Ts>
using is_container_t = typename is_container<T, Ts...>::type;

#if STRIGNITE_CPLUSPLUS_17
template<typename L, typename R>
constexpr static bool is_same_v = std::is_same_v<L, R>;

#define STRIGNITE_IS_SAME_V(lhs, ...) std::is_same_v<lhs, __VA_ARGS__>
#define STRIGNITE_IS_SAME_V_T(lhs, ...) std::enable_if_t<STRIGNITE_IS_SAME_V(lhs, __VA_ARGS__)>
#else
template<typename L, typename R>
constexpr static bool is_same_v = std::is_same<L, R>::value;

#define STRIGNITE_IS_SAME_V(lhs, rhs) std::is_same<lhs, rhs>::value
#define STRIGNITE_IS_SAME_V_T(lhs, rhs) typename std::enable_if<STRIGNITE_IS_SAME_V(lhs, rhs)>::type
#endif

#if STRIGNITE_CPLUSPLUS >= 201402L
template<bool Condition, typename T = void>
using enable_if_t = std::enable_if_t<Condition, T>;
#else
template<bool Condition, typename T = void>
using enable_if_t = typename std::enable_if<Condition, T>::type;
#endif

#pragma region code convert

template<typename Char32, template<typename...> class BasicString, typename Char8,
    typename = enable_if_t<sizeof(Char32) == 4 && sizeof(Char8) == 1>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char32> _to_utf32(const Char8*, size_t, bool = false);

template<typename Char32, template<typename...> class BasicString, typename Char16,
    typename = enable_if_t<sizeof(Char16) == 2 && sizeof(Char32) == 4>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char32> _to_utf32(const Char16*, size_t, int = 0);

template<typename Char32, template<typename...> class BasicString,
    typename = enable_if_t<sizeof(Char32) == 4>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char32> _to_utf32(const char32_t*, size_t);

template<typename RChar8, template<typename...> class BasicString, typename Char8,
    typename = enable_if_t<sizeof(RChar8) == 1 && sizeof(Char8) == 1>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<RChar8> _to_utf8(const Char8*, size_t, bool = false);

template<typename Char8, template<typename...> class BasicString, typename Char16,
    typename = enable_if_t<sizeof(Char16) == 2 && sizeof(Char8) == 1>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char8> _to_utf8(const Char16*, size_t, int = 0);

template<typename Char8, template<typename...> class BasicString,
    typename = enable_if_t<sizeof(Char8) == 1>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char8> _to_utf8(const char32_t*, size_t);

template<typename Char16, template<typename...> class BasicString, typename Char8,
    typename = enable_if_t<sizeof(Char8) == 1 && sizeof(Char16) == 2>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char16> _to_utf16(const Char8*, size_t, bool = false);

template<typename RChar16, template<typename...> class BasicString, typename Char16,
    typename = enable_if_t<sizeof(RChar16) == 2 && sizeof(Char16) == 2>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<RChar16> _to_utf16(const Char16*, size_t, int = 0);

template<typename Char16, template<typename...> class BasicString,
    typename = enable_if_t<sizeof(Char16) == 2>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char16> _to_utf16(const char32_t*, size_t);

template<template<typename...> class, typename>
struct _convert {};

#if STRIGNITE_CPLUSPLUS >= 202002L
template<template<typename...> typename BasicString>
struct _convert<BasicString, char8_t> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR static BasicString<char8_t> from_utf8(const char* str, size_t len);
};
#endif

template<template<typename...> class BasicString>
struct _convert<BasicString, wchar_t> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR static BasicString<wchar_t> from_utf8(const char* str, size_t len);
};

template<template<typename...> class BasicString>
struct _convert<BasicString, char16_t> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR static BasicString<char16_t> from_utf8(const char* str, size_t len);
};

template<template<typename...> class BasicString>
struct _convert<BasicString, char32_t> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR static BasicString<char32_t> from_utf8(const char* str, size_t len);
};

#pragma endregion code convert

template<typename int_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR size_t align_size(const size_t size) noexcept {
    constexpr size_t alignment = sizeof(size_t);
    constexpr size_t boundary = alignment / sizeof(int_type) - 1;
    return size + boundary & ~boundary;
}

#if STRIGNITE_CPLUSPLUS >= 201402L
#  define STRIGNITE_REPEAT_BYTE(type, byte) ::bcs::internal::repeat_byte<type>(byte)

template<typename Word>
STRIGNITE_CPP14_CONSTEXPR Word repeat_byte(Word byte) {
    constexpr size_t BITS_IN_BYTE = CHAR_BIT;
    constexpr size_t BYTE_MASK = 0xFF;
    Word result = 0;
    byte = byte & BYTE_MASK;
    for (size_t i = 0; i < sizeof(Word); ++i)
        result = result << BITS_IN_BYTE | byte;
    return result;
}
#else
#  define STRIGNITE_REPEAT_BYTE(type, byte) ::bcs::internal::repeat_byte<type, byte>::value

template<typename Word, size_t byte, size_t count>
struct recursive_repeat_byte {
    static constexpr Word value = (byte << count * CHAR_BIT) | recursive_repeat_byte<Word, byte, count - 1>::value;
};

template<typename Word, size_t byte>
struct recursive_repeat_byte<Word, byte, 0> {
    static constexpr Word value = byte;
};

template<typename Word, size_t byte>
struct repeat_byte {
    static constexpr Word value = recursive_repeat_byte<Word, byte & 0xFF, sizeof(Word) - 1>::value;
};
#endif

template<typename Word>
STRIGNITE_CPP11_CONSTEXPR bool has_zeroes(Word block) {
    constexpr Word LOW_BITS = STRIGNITE_REPEAT_BYTE(Word, 0x01);
    constexpr Word HIGH_BITS = STRIGNITE_REPEAT_BYTE(Word, 0x80);
    Word subtracted = block - LOW_BITS;
    Word inverted = ~block;
    return (subtracted & inverted & HIGH_BITS) != 0;
}

template<size_t N>
struct padding {
    uint8_t padding_bytes[N];
};

template<>
struct padding<0> {};

template<typename T>
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
void swap(T& lhs, T& rhs) noexcept {
    T temp = std::move(rhs);
    rhs = std::move(lhs);
    lhs = std::move(temp);
}

template<typename Func>
class deferred_action {
private:
    Func M_func;

public:
    explicit deferred_action(Func&& func) :
        M_func(std::move(func)) {}

    ~deferred_action() { M_func(); }
};

struct defer_generator {
    template<typename Func>
    deferred_action<Func> operator+(Func&& func) {
        return deferred_action<Func>(std::forward<Func>(func));
    }
};

#define STRIGNITE_DEFER_PASTE(a, b) a##b
#define STRIGNITE_DEFER_CAT(a, b) STRIGNITE_DEFER_PASTE(a, b)
#define STRIGNITE_DEFER auto STRIGNITE_DEFER_CAT(LOCAL_DEFER_, __LINE__) = bcs::internal::defer_generator{} + [&]

// template<typename char_type, typename = is_char_t<char_type>>
// STRIGNITE_CPP17_NODISCARD
// STRIGNITE_CPP20_CONSTEXPR
// basic_string<char_type> reverse_helper(const char_type* const str, const size_t length,
//                                        enable_if_t<sizeof(char_type) == 1, int>  = 0) noexcept {
//     size_t _length = length;
//
//     char_type _str[_length + 1];
//     std::char_traits<char_type>::copy(_str, str, _length + 1);
//     char_type* left = _str;
//     char_type* right = _str + _length - 1;
//
//     while (left < right) {
//         swap(*left++, *right--);
//     }
//
//     for (auto _begin = _str, _end = _str + _length - 1; _end > _begin;) {
//         auto a = *_end & 0xF0;
//         if (a == 0xF0) {
//             swap(*_end, *(_end - 3));
//             swap(*(_end - 1), *(_end - 2));
//             _end -= 4;
//         } else if (a == 0xE0) {
//             swap(*_end, *(_end - 2));
//             _end -= 3;
//         } else if ((a & 0xC0) == 0xC0) {
//             swap(*_end, *(_end - 1));
//             _end -= 2;
//         } else {
//             _end -= 1;
//         }
//     }
//     return { _str, _length };
// }

#pragma region string search algorithm

template<typename char_type, typename = is_char_t<char_type>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR
const char_type* bf_search(const char_type* haystack, const size_t haystack_len,
                           const char_type* needle, const size_t needle_len) noexcept {
    if (needle_len > haystack_len || needle_len == 0)
        return nullptr;

    using char_traits = std::char_traits<char_type>;

    const auto needle_len_sub_1 = needle_len - 1;

    auto _haystack = haystack;
    auto _haystack_len = haystack_len - needle_len_sub_1;

    auto ch = needle[0];
    while (true) {
        auto res = char_traits::find(_haystack, _haystack_len, ch);
        if (!res) return nullptr;
        if (char_traits::compare(res, needle, needle_len) == 0) return res;
        _haystack_len -= res - _haystack;
        _haystack = res + 1;
    }
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR
const char_type* horspool_search(const char_type* haystack, const size_t haystack_len,
                                 const char_type* needle, const size_t needle_len) noexcept {
    const auto _needle_len = needle_len;
    const auto _haystack_len = haystack_len;

    if (_needle_len > _haystack_len || _needle_len == 0)
        return nullptr;

    int table[0xFF] = {};

    //记录模式串中字符出现的最右位置
    for (int i = 0; i < _needle_len; ++i) {
        table[static_cast<uint8_t>(needle[i])] = i;
    }

    for (size_t i = 0; i + _needle_len <= _haystack_len;) {
        int j = static_cast<int>(_needle_len) - 1;
        while (j >= 0 && haystack[i + j] == needle[j]) j--;
        if (j < 0) return haystack + i;
        const auto step = j - table[static_cast<uint8_t>(haystack[i + j])];
        i += step > 0 ? step : 1;
    }
    return nullptr;
}

template<typename char_type, typename = is_char_t<char_type>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR
const char_type* boyer_moore_search(const char_type* haystack, const size_t haystack_len,
                                    const char_type* needle, const size_t needle_len) noexcept {
    STRIGNITE_IF_UNLIKELY(needle_len > haystack_len || needle_len == 0) {
        return nullptr;
    }

    const auto needle_len_sub_1 = needle_len - 1;
    const auto last_char = needle[needle_len_sub_1];

    size_t skip = 1;
    while (skip < needle_len && needle[needle_len_sub_1 - skip] != last_char) {
        ++skip;
    }

    const char_type* i = haystack;
    auto end = haystack + haystack_len - needle_len_sub_1;

    using char_traits = std::char_traits<char_type>;

    while (i < end) {
        auto j = i + needle_len_sub_1;
        auto len = static_cast<size_t>(end - i);
        auto k = char_traits::find(j, len, last_char);
        if (!k) {
            return nullptr;
        }
        i = k - needle_len_sub_1;

        if (char_traits::compare(i, needle, needle_len) == 0) {
            return i;
        }
        i += skip;
    }
    return nullptr;
}

template<typename char_type, typename = is_char_t<char_type>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR
const char_type* reverse_boyer_moore_search(const char_type* haystack, const size_t haystack_len,
                                            const char_type* needle, const size_t needle_len) noexcept {
    STRIGNITE_IF_UNLIKELY(needle_len > haystack_len || needle_len == 0) {
        return nullptr;
    }

    const auto needle_len_sub_1 = needle_len - 1;
    const auto last_char = *needle;

    size_t skip = 1;
    while (skip < needle_len && needle[skip] != last_char) {
        ++skip;
    }

    const char_type* i = haystack + haystack_len - 1;
    auto _begin = haystack + needle_len_sub_1 - 1;

    using char_traits = std::char_traits<char_type>;

    while (i > _begin) {
        while (*(i - needle_len_sub_1) != last_char) {
            if (--i == _begin) {
                return nullptr;
            }
        }
        auto j = i - needle_len_sub_1;
        if (char_traits::compare(j, needle, needle_len) == 0) {
            return j;
        }
        i -= skip;
    }
    return nullptr;
}

template<typename char_type, typename = is_char_t<char_type>, typename = enable_if_t<sizeof(char_type) == 1>>
STRIGNITE_CPP17_NODISCARD
const char_type* two_way_search(const char_type* haystack, const size_t haystack_len,
                                const char_type* needle, const size_t needle_len) noexcept {
#define STRIGNITE_INTERNAL_BITOP(a, b, op) \
((a)[(size_t)(b) / (8 * sizeof *(a))] op (size_t)1 << ((size_t)(b) % (8 * sizeof *(a))))

    if (needle_len > haystack_len || needle_len == 0) {
        return nullptr;
    }

    auto h = reinterpret_cast<const uint8_t *>(std::char_traits<char>::find(haystack, haystack_len, *needle));
    const auto z = h + (haystack_len - (h - reinterpret_cast<const uint8_t *>(haystack)));
    const auto n = reinterpret_cast<const uint8_t *>(needle);
    const auto length = needle_len;

    size_t mem0;
    size_t byteset[32 / sizeof(size_t)] = {};
    size_t shift[256];

    /* Computing length of needle and fill shift table */
    for (size_t i = 0; i < length; i++)
        STRIGNITE_INTERNAL_BITOP(byteset, n[i], |=), shift[n[i]] = i + 1;

    /* Compute maximal suffix */
    size_t ip = -1;
    size_t jp = 0;
    size_t k = 1;
    size_t p = 1;
    while (jp + k < length) {
        if (n[ip + k] == n[jp + k]) {
            if (k == p) {
                jp += p;
                k = 1;
            } else {
                k++;
            }
        } else if (n[ip + k] > n[jp + k]) {
            jp += k;
            k = 1;
            p = jp - ip;
        } else {
            ip = jp++;
            k = p = 1;
        }
    }
    size_t ms = ip;
    size_t p0 = p;

    /* And with the opposite comparison */
    ip = -1;
    jp = 0;
    k = p = 1;
    while (jp + k < length) {
        if (n[ip + k] == n[jp + k]) {
            if (k == p) {
                jp += p;
                k = 1;
            } else k++;
        } else if (n[ip + k] < n[jp + k]) {
            jp += k;
            k = 1;
            p = jp - ip;
        } else {
            ip = jp++;
            k = p = 1;
        }
    }
    if (ip + 1 > ms + 1) ms = ip;
    else p = p0;

    /* Periodic needle? */
    if (memcmp(n, n + p, ms + 1) != 0) {
        mem0 = 0;
        p = std::max(ms, length - ms - 1) + 1;
    } else {
        mem0 = length - p;
    }
    size_t mem = 0;

    /* Search loop */
    for (;;) {
        /* If remainder of haystack is shorter than needle, done */
        if (z - h < length) return nullptr;

        /* Check last byte first; advance by shift on mismatch */
        if (STRIGNITE_INTERNAL_BITOP(byteset, h[length - 1], &)) {
            k = length - shift[h[length - 1]];
            if (k) {
                if (k < mem) k = mem;
                h += k;
                mem = 0;
                continue;
            }
        } else {
            h += length;
            mem = 0;
            continue;
        }

        /* Compare right half */
        for (k = std::max(ms + 1, mem); k < length && n[k] == h[k]; k++) {}
        if (k < length) {
            h += k - ms;
            mem = 0;
            continue;
        }
        /* Compare left half */
        for (k = ms + 1; k > mem && n[k - 1] == h[k - 1]; k--) {}
        if (k <= mem) return reinterpret_cast<const char_type *>(h);
        h += p;
        mem = mem0;
    }
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP14_CONSTEXPR
const char_type* str_search(const char_type* haystack, const size_t haystack_len,
                            const char_type* needle, const size_t needle_len,
                            enable_if_t<sizeof(char_type) == 1, int>  = {}) noexcept {
    // STRIGNITE_IF_NOT_CONSTANT_EVALUATED(
    //     if (haystack_len > 256) return two_way_search(haystack, haystack_len, needle, needle_len)
    // )
    // if (haystack_len > 256) {
    // return horspool_search(haystack, haystack_len, needle, needle_len);
    // }
    return boyer_moore_search(haystack, haystack_len, needle, needle_len);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP14_CONSTEXPR
const char_type* str_search(const char_type* haystack, const size_t haystack_len,
                            const char_type* needle, const size_t needle_len,
                            enable_if_t<(sizeof(char_type) > 1), bool>  = {}) noexcept {
    return boyer_moore_search(haystack, haystack_len, needle, needle_len);
}

#pragma endregion string search algorithm

template<typename char_type, typename = enable_if_t<sizeof(char_type) == 1>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR size_t utf8_length(const char_type* const str) noexcept {
    size_t length = 0;
    for (; *str; ++length) {
        auto a = *str & 0xF0;
        if (a == 0xE0) {
            str += 3;
        } else if (a == 0xF0) {
            str += 4;
        } else if ((a &= 0xC0) == 0xC0) {
            str += 2;
        } else {
            str += 1;
        }
    }
    return length;
}

template<typename IntType, typename char_type, typename = is_char_t<char_type>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR IntType to_int_not_check_nullptr(const char_type* str) noexcept {
    STRIGNITE_ASSUME(str != nullptr)

    auto ptr = str;
    size_t result = 0;
    bool negative = false; //负数

    for (; *ptr && static_cast<uint32_t>(*ptr) <= ' '; ++ptr) {}

    switch (*ptr) {
        case '-': negative = true;
        case '+': ++ptr;
        default: ;
    }

    constexpr size_t _int_max = std::numeric_limits<IntType>::max();
    constexpr size_t _int_min = std::numeric_limits<IntType>::min();
    constexpr size_t _uint_max = _int_max - _int_min;
    constexpr size_t _limit = _uint_max / 10;
    constexpr size_t _num_limit = _uint_max % 10;

    for (; *ptr >= '0' && *ptr <= '9'; ++ptr) {
        const int num = *ptr - '0';
        if (result > _limit || (result == _limit && num > _num_limit)) {
            return negative ? _int_min : _int_max;
        }
        result = result * 10 + num;
    }

    return negative ? -result : result;
}

template<typename char_traits, typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
size_t traits_length(const char_type* const str) {
    STRIGNITE_IF_UNLIKELY(!str) throw std::invalid_argument("string pointer cannot be nullptr.");
    return char_traits::length(str);
}

template<typename char_type, typename char_traits = std::char_traits<char_type>>
struct MutString {
    char_type* data;
    size_t length;
    size_t capacity;

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    static MutString get_string(const char_type* const str, const size_t length) noexcept {
        const auto capacity = length * 2;
        auto* data = new char_type[capacity + 1];
        char_traits::copy(data, str, length);
        assert(data != nullptr);
        return { data, length, capacity };
    }
};

/**
     * @brief 在可变字符串中替换所有匹配的子串
     *
     * 本函数用于在可变字符串中执行原地替换操作，将所有的`old_value`子串替换为`new_value`子串。
     * 智能处理新旧子串长度差异，支持动态内存调整。当新子串更长时会自动扩展内存容量。
     *
     * @tparam buffer_size      内部索引缓冲区大小（必须满足 buffer_size <= 0x400）
     *
     * @param[in,out] str      要修改的可变字符串对象，其data/length/capacity属性可能被更新
     * @param[in] old_value    要查找的旧子串指针（必须非空）
     * @param[in] old_size     旧子串长度（为0时函数直接返回）
     * @param[in] new_value    新子串指针（必须非空）
     * @param[in] new_size     新子串长度
     *
     * @note 关键实现细节：
     * - 新旧子串等长时直接进行原地覆盖替换
     * - 使用固定大小栈缓冲区存储索引（buffer_size模板参数控制），超出时自动堆分配
     * - 新子串更长时：
     *   - 预计算所有匹配位置索引
     *   - 需要时扩容（容量不足则扩容至新长度的两倍）
     *   - 从后向前移动数据避免覆盖
     * - 新子串更短时：
     *   - 从前向后移动数据减少内存操作
     *   - 使用联合索引结构处理指针重定位问题
     *
     * @warning 注意事项：
     * - 要求 buffer_size <= 1024（0x400）的编译期约束
     * - 调用方需确保 old_value/new_value 指针有效性
     * - 函数执行后原字符串指针可能失效（发生内存重分配时）
     */
template<size_t buffer_size, typename char_type,
    typename char_traits = std::char_traits<char_type>, typename = enable_if_t<buffer_size <= 0x400>>
STRIGNITE_CPP20_CONSTEXPR
static void replace_helper(MutString<char_type, char_traits>& str,
                           const char_type* old_value, const size_t old_size,
                           const char_type* new_value, const size_t new_size) {
    STRIGNITE_IF_UNLIKELY(old_size == 0) return;

    auto& _str = str.data;
    auto& _length = str.length;
    auto& _capacity = str.capacity;

    STRIGNITE_ASSUME(_str != nullptr && old_value != nullptr && new_value != nullptr);

    const auto old_value_length = old_size;
    const auto new_value_length = new_size;

    if (old_value_length == new_value_length) {
        STRIGNITE_IF_UNLIKELY(old_value == new_value ||
            char_traits::compare(old_value, new_value, old_value_length) == 0) {
            return;
        }
        for (auto* index = internal::str_search(_str, _length, old_value, old_value_length);
             index != nullptr;
             index = internal::str_search(
                 index + old_value_length, _length - (index + old_value_length - _str), old_value, old_value_length
             )
        ) {
            char_traits::copy(const_cast<char_type *>(index), new_value, new_value_length);
            return;
        }
    }

    union Index {
        char_type* ptr_index;
        size_t num_index;
    };

    // 相当于精简版的 small_vector<Index, 64>
    Index indexes[buffer_size];
    Index* indexes_ptr = indexes;
    size_t indexes_capacity = buffer_size - 1;
    size_t indexes_size = 0;

    STRIGNITE_DEFER {
        if (indexes_ptr != indexes)
            delete[] indexes_ptr;
    };

    for (auto index = internal::str_search(_str, _length, old_value, old_value_length);
         index != nullptr;
         index = internal::str_search(
             index + old_value_length, _length - (index + old_value_length - _str), old_value, old_value_length
         )
    ) {
        STRIGNITE_IF_UNLIKELY(indexes_size >= indexes_capacity) {
            const auto new_indexes_capacity = indexes_size * 2;
            auto* temp = new Index[new_indexes_capacity];
            memcpy(temp, indexes_ptr, sizeof(Index) * indexes_size);
            if (indexes_ptr != indexes) {
                delete[] indexes_ptr;
            }
            indexes_capacity = new_indexes_capacity - 1;
            indexes_ptr = temp;
        }
        indexes_ptr[indexes_size++].ptr_index = const_cast<char_type *>(index);
    }

    if (indexes_size == 0) {
        return;
    }

    const auto new_str_length = _length - old_value_length * indexes_size + new_value_length * indexes_size;

    const auto differance_size = static_cast<int64_t>(new_value_length - old_value_length);

    if (new_value_length > old_value_length) {
        STRIGNITE_IF_UNLIKELY(new_str_length >= _capacity) {
            for (size_t i = 0; i < indexes_size; ++i) {
                indexes_ptr[i].num_index = indexes_ptr[i].ptr_index - _str;
            }
            auto new_capacity = new_str_length * 2;
            auto* temp = new char_type[new_capacity + 1];
            char_traits::copy(temp, _str, _length);
            delete[] _str;
            _capacity = new_capacity;
            _str = temp;
            const char_type* last_right_end = _str + _length;
            for (size_t i = indexes_size; i > 0; --i) {
                auto index = _str + indexes_ptr[i - 1].num_index;
                auto right = index + old_value_length;
                auto right_size = last_right_end - right;
                last_right_end = index;
                auto new_right = right + differance_size * i;
                char_traits::move(new_right, right, right_size);
                char_traits::copy(new_right - new_value_length, new_value, new_value_length);
            }
        } else {
            const char_type* last_right_end = _str + _length;
            for (size_t i = indexes_size; i > 0; --i) {
                auto index = indexes_ptr[i - 1].ptr_index;
                auto right = index + old_value_length;
                auto right_size = last_right_end - right;
                last_right_end = index;
                auto new_right = right + differance_size * i;
                char_traits::move(new_right, right, right_size);
                char_traits::copy(new_right - new_value_length, new_value, new_value_length);
            }
        }
    } else {
        indexes_ptr[indexes_size].ptr_index = _str + _length;
        for (size_t i = 0; i < indexes_size; i++) { // indexes_size 不会等于或小于 0
            auto index = indexes_ptr[i].ptr_index;
            auto right = index + old_value_length;
            auto right_size = indexes_ptr[i + 1].ptr_index - right;
            auto diff = differance_size * i;
            char_traits::copy(index + diff, new_value, new_value_length);
            char_traits::move(right + diff + differance_size, right, right_size);
        }
    }

    _length = new_str_length;
}

#pragma endregion utility
}

#pragma region utility
/**
 * @brief 检查字符是否为十进制数字字符
 *
 * 本函数通过快速算术运算判断字符是否属于'0'-'9'范围
 *
 * @param[in] ch 待检测的32位无符号整型字符值
 * @return bool
 *  - true  : 字符在'0'-'9'范围内
 *  - false : 非数字字符或非ASCII数字
 *
 * @note 特性：
 * - 使用减法运算优化判断逻辑
 * - 支持C++11起的编译期常量表达式
 * - 强制内联优化检测路径
 *
 * @warning 注意事项：
 * - 仅检测ASCII数字字符（U+0030-U+0039）
 * - Unicode数字字符（如全角数字）会返回false
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
bool is_digit(const uint32_t ch) noexcept {
    return ch - '0' < 10;
}

/**
 * @brief 检测字符是否为空白字符
 *
 * 判断字符是否属于ASCII控制字符或空格符，
 * 包含换行、制表符等特殊空白字符的检测。
 *
 * @param[in] ch 待检测的32位无符号整型字符值
 * @return bool
 *  - true  : 字符编码<=0x20（ASCII空格及控制字符）
 *  - false : 可见字符或扩展空白符
 *
 * @note 实现特性：
 * - 通过单次比较运算实现高效检测
 * - 不支持Unicode空白符检测
 *
 * @warning 注意：
 * - 将换页符(0x0C)、垂直制表符(0x0B)等识别为空格
 * - 不包含非间断空格(0xA0)等扩展空白符
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
bool is_space(const uint32_t ch) noexcept {
    return ch <= ' ';
}

/**
 * @brief 检测字符是否为小写拉丁字母
 *
 * 快速判断字符是否属于'a'-'z'范围，
 * 用于ASCII小写字母的识别。
 *
 * @param[in] ch 待检测的32位无符号整型字符值
 * @return bool
 *  - true  : 字符在'a'-'z'范围内
 *  - false : 非小写字母或非ASCII字符
 *
 * @note 实现细节：
 * - 使用算术运算替代范围比较优化性能
 * - 支持编译期字符检测
 *
 * @warning 限制：
 * - 仅适用于基础拉丁字母表
 * - 带变音符号的字母（如à,ç）返回false
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
bool is_lower(const uint32_t ch) noexcept {
    return ch - 'a' < 26;
}

/**
 * @brief 检测字符是否为大写拉丁字母
 *
 * 快速判断字符是否属于'A'-'Z'范围，
 * 用于ASCII大写字母的识别。
 *
 * @param[in] ch 待检测的32位无符号整型字符值
 * @return bool
 *  - true  : 字符在'A'-'Z'范围内
 *  - false : 非大写字母或非ASCII字符
 *
 * @note 特性：
 * - 使用与is_lower()对称的实现方式
 * - 支持编译期字符检测
 *
 * @warning 注意：
 * - 不检测德语ß等特殊大写形式
 * - 希腊/西里尔等字母体系返回false
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
bool is_upper(const uint32_t ch) noexcept {
    return ch - 'A' < 26;
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
char_type to_lower_ascii(const char_type ch) noexcept {
    return ch ^ (0x40 - ch ^ 0x5a - ch) >> 2 & 0x20;
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
char_type to_lower(const char_type ch, internal::enable_if_t<sizeof(char_type) == 1, char_type *>  = nullptr) noexcept {
    return ch ^ ((0x40 - ch ^ 0x5a - ch) & ~ch) >> 2 & 0x20;
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
char_type to_lower(const char_type ch, internal::enable_if_t<sizeof(char_type) != 1, char_type *>  = nullptr) noexcept {
    return is_upper(ch) ? ch | 32 : ch;
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
char_type to_upper_ascii(const char_type ch) noexcept {
    return ch & ~((0x60 - ch ^ 0x7a - ch) >> 2 & 0x20);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
char_type to_upper(const char_type ch, internal::enable_if_t<sizeof(char_type) == 1, char_type *>  = nullptr) noexcept {
    return ch & ~(((0x60 - ch ^ 0x7a - ch) & ~ch) >> 2 & 0x20);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
char_type to_upper(const char_type ch, internal::enable_if_t<sizeof(char_type) != 1, char_type *>  = nullptr) noexcept {
    return is_lower(ch) ? ch & 0x5F : ch;
}

struct ascii {
    template<typename char_type>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
    static char_type to_lower(const char_type ch) noexcept {
        return bcs::to_lower_ascii(ch);
    }

    template<typename char_type>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
    static char_type to_upper(const char_type ch) noexcept {
        return bcs::to_upper_ascii(ch);
    }
};

struct latin1 {
    template<typename char_type>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
    static char_type to_lower(const char_type ch) noexcept {
        return bcs::to_lower(ch);
    }

    template<typename char_type>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP11_CONSTEXPR STRIGNITE_FORCE_INLINE
    static char_type to_upper(const char_type ch) noexcept {
        return bcs::to_upper(ch);
    }
};

template<typename char_type, typename = internal::is_char_t<char_type>>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR bool is_number(const char_type* const str) noexcept {
    auto temp = str;
    //如果不以'-'、'+'、或数字开头，直接返回false
    if (*temp == static_cast<char_type>('-') || *temp == static_cast<char_type>('+')) {
        ++temp;
    } else if (!is_digit(*temp)) {
        return false;
    }

    size_t digit_count = 0;
    for (; is_digit(*temp); ++temp, digit_count++) {}
    if (digit_count == 0) return false;
    if (*temp == static_cast<char_type>(0)) return true;

    if (*temp == static_cast<char_type>('.')) {
        ++temp;
        digit_count = 0;
        for (; is_digit(*temp); ++temp, digit_count++) {}

        if (digit_count == 0) return false;
        if (*temp == static_cast<char_type>(0)) return true;

        if (to_lower(*temp) == static_cast<char_type>('e')) {
            ++temp;
            digit_count = 0;
            if (*temp != static_cast<char_type>('-') && *temp != static_cast<char_type>('+')) {
                if (is_digit(*temp)) {
                    digit_count++;
                } else {
                    return false;
                }
            }
            ++temp;
            for (; is_digit(*temp); ++temp, digit_count++) {}
            return digit_count != 0 && *temp == static_cast<char_type>(0);
        }
    } else if (to_lower(*temp) == static_cast<char_type>('e')) {
        ++temp;
        digit_count = 0;
        if (*temp != static_cast<char_type>('-') && *temp != static_cast<char_type>('+')) {
            if (is_digit(*temp)) {
                digit_count++;
            } else {
                return false;
            }
        }
        ++temp;
        for (; is_digit(*temp); ++temp, digit_count++) {}
        return digit_count != 0 && *temp == static_cast<char_type>(0);
    }
    return false;
}

// 待测试
template<size_t buffer_size = 0x80,
    typename = internal::enable_if_t<buffer_size <= 0x400>,
    template<typename...> class Container,
    template<typename...> class StringType,
    typename char_type,
    typename = internal::is_container_t<Container<StringType<char_type>>>,
    typename = internal::is_char_t<char_type>>
basic_string<char_type> join(const Container<StringType<char_type>>& buffer, const char_type* const separator) {
    if (!separator)
        throw std::invalid_argument("bcs::join: argument cannot be nullptr.");
    if (buffer.empty()) return {};

    auto separator_len = std::char_traits<char_type>::length(separator);

    size_t length = 0;

    for (auto& str: buffer) {
        length += str.size();
    }

    if (separator_len != 0)
        length += separator_len * (buffer.size() - 1);

    if (length > buffer_size) {
        auto* temp = new char_type[length];
        STRIGNITE_DEFER { delete[] temp; };
        length = 0;
        if (separator_len == 0) {
            for (auto& str: buffer) {
                basic_string_view<char_type> view = str;
                std::char_traits<char_type>::copy(temp + length, view.data(), view.size());
                length += view.size();
            }
        } else {
            for (auto& str: buffer) {
                basic_string_view<char_type> view = str;
                std::char_traits<char_type>::copy(temp + length, view.data(), view.size());
                std::char_traits<char_type>::copy(temp + length + separator_len, separator, separator_len);
                length += view.size() + separator_len;
            }
        }
        return { temp, length };
    }

    char_type temp[buffer_size];
    length = 0;
    if (separator_len == 0) {
        for (auto& str: buffer) {
            basic_string_view<char_type> view = str;
            std::char_traits<char_type>::copy(temp + length, view.data(), view.size());
            length += view.size();
        }
    } else {
        for (auto& str: buffer) {
            basic_string_view<char_type> view = str;
            std::char_traits<char_type>::copy(temp + length, view.data(), view.size());
            std::char_traits<char_type>::copy(temp + length + separator_len, separator, separator_len);
            length += view.size() + separator_len;
        }
    }
    return { temp, length };
}

#pragma endregion utility

#pragma region class string_view

/**
 * @brief 轻量级非拥有字符串视图模板类
 *
 * 该类提供对字符序列的只读视图，不持有字符串数据的所有权。设计目标为高效、零拷贝的字符串操作，
 * 兼容C++17标准字符串视图接口，支持编译期常量表达式操作。
 *
 * @tparam char_type     字符类型（如char/wchar_t）
 * @tparam char_traits   字符特性类（需实现标准字符操作接口）
 *
 * @note 主要特性：
 * - 支持从C风格字符串、std::basic_string等多种类型构造
 * - 提供STL兼容的迭代器接口
 * - 支持子串查找、比较、切片等常用操作
 * - 异常安全保证：除明确标注外均为noexcept
 * - 支持C++17起constexpr构造和操作
 */
template<typename char_type, typename char_traits>
class basic_string_view {
public:
    /// 字符类型别名
    using value_type = char_type;
    /// 大小类型别名
    using size_type = size_t;
    /// 指针差异类型别名
    using difference_type = ptrdiff_t;
    /// 引用类型别名
    using reference = char_type &;
    /// 常量引用类型别名
    using const_reference = const char_type &;
    /// 指针类型别名
    using pointer = char_type *;
    /// 常量指针类型别名
    using const_pointer = const char_type *;
    /// 迭代器类型别名
    using iterator = const char_type *;
    /// 常量迭代器类型别名
    using const_iterator = iterator;
    /// 反向迭代器类型别名
    using reverse_iterator = const std::reverse_iterator<iterator>;
    /// 常量反向迭代器类型别名
    using const_reverse_iterator = reverse_iterator;

public:
    /**
     * @brief 默认构造函数
     *
     * 构造空字符串视图，data()返回nullptr，size()返回0
     */
    STRIGNITE_CPP17_CONSTEXPR basic_string_view() noexcept :
        M_str(nullptr),
        M_length(0) {}

    /**
     * @brief 从字符数组构造字符串视图
     *
     * @param str    字符数组指针（必须非空且以char_type(0)结尾）
     * @param length 显式指定的字符串长度
     *
     * @warning 调用者需保证str在视图生命周期内有效
     */
    STRIGNITE_CPP17_CONSTEXPR basic_string_view(const char_type* str, const size_t length) noexcept :
        M_str(str),
        M_length(length) {}

    /**
     * @brief 从C风格字符串构造视图
     *
     * @param str 以空字符结尾的C风格字符串
     * @throw std::invalid_argument 当str为nullptr时抛出
     */
    STRIGNITE_CPP17_CONSTEXPR basic_string_view(const char_type* str) :
        M_str(str),
        M_length(internal::traits_length<char_traits>(str)) {}

    /**
     * @brief 从std::basic_string构造视图
     *
     * @param str 标准字符串对象
     */
    STRIGNITE_CPP17_CONSTEXPR basic_string_view(const std::basic_string<char_type>& str) noexcept :
        M_str(str.data()),
        M_length(str.size()) {}

    // 默认生成的拷贝/移动构造/赋值运算符
    STRIGNITE_CPP17_CONSTEXPR basic_string_view(const basic_string_view& other) noexcept = default;

    STRIGNITE_CPP17_CONSTEXPR basic_string_view(basic_string_view&& other) noexcept = default;

    STRIGNITE_CPP17_CONSTEXPR basic_string_view& operator=(const basic_string_view& other) noexcept = default;

    STRIGNITE_CPP17_CONSTEXPR basic_string_view& operator=(basic_string_view&& other) noexcept = default;

    /// 析构函数（默认实现）
    STRIGNITE_CPP20_CONSTEXPR ~basic_string_view() noexcept = default;

    /**
     * @brief 获取底层字符指针
     * @return 可能为nullptr（当视图为空时）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    const char_type* data() const noexcept {
        return M_str;
    }

    /**
     * @brief 获取视图长度（单位：字符数）
     * @return 字符序列长度，可能为0
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t length() const noexcept {
        return M_length;
    }

    /// @copydoc length()
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t size() const noexcept {
        return M_length;
    }

    /**
     * @brief 检查视图是否为空
     * @return true 当长度为0时
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool empty() const noexcept {
        return M_length == 0;
    }

    /**
     * @brief 获取起始迭代器
     * @warning 对空视图调用将返回nullptr
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    const_iterator begin() const noexcept {
        return M_str;
    }

    /**
     * @brief 获取结束迭代器
     * @return 指向最后一个字符之后的位置
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    const_iterator end() const noexcept {
        return M_str + M_length;
    }

public:
    /**
     * @brief 下标访问运算符
     * @param index 访问位置（从0开始）
     * @return 对应位置字符
     * @warning 不进行边界检查，需调用者保证index < size()
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type operator[](const size_t index) const noexcept {
        return M_str[index];
    }

    /**
     * @brief 获取首字符
     * @return 首字符
     * @warning 对空视图调用将导致未定义行为
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type front() const noexcept {
        return M_str[0];
    }

    /**
     * @brief 获取末字符
     * @return 末字符
     * @warning 对空视图调用将导致未定义行为
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type back() const noexcept {
        return M_str[M_length - 1];
    }

    /**
     * @brief 相等比较运算符
     * @param other 对比的视图对象
     * @return 当长度相等且所有字符相同时返回true
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator==(const basic_string_view other) const noexcept {
        return equals(other);
    }

    /// 不等比较运算符
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator!=(const basic_string_view other) const noexcept {
        return !(*this == other);
    }

    /**
     * @brief 流输出运算符
     * @param os  输出流
     * @param str 要输出的视图
     * @return 输出流引用
     */
    STRIGNITE_FORCE_INLINE
    friend std::basic_ostream<char_type>&
    operator<<(std::basic_ostream<char_type>& os, const basic_string_view str) noexcept {
        return os.write(str.data(), str.size());
    }

    /**
     * @brief 转换为std::basic_string
     * @return 新建的字符串对象（可能引发内存分配）
     * @warning 对空视图转换将触发assert
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    operator std::basic_string<char_type>() const noexcept {
        assert(M_str);
        return { M_str, M_length };
    }


#if STRIGNITE_CPLUSPLUS_17
    /**
     * @brief 转换为标准string_view（C++17起）
     * @return 对应的std::basic_string_view对象
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    operator std::basic_string_view<char_type>() const noexcept {
        return { M_str, M_length };
    }
#endif

public:
    /**
     * @brief 查找子视图首次出现位置
     * @param target 要查找的子视图
     * @return 找到时返回起始索引，否则返回npos
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_string_view target) const noexcept {
        auto res = internal::str_search(M_str, M_length, target.data(), target.length());
        return res ? res - M_str : npos;
    }

    /**
     * @brief 从指定位置查找子视图
     * @param target 要查找的子视图
     * @param pos    起始查找位置
     * @return 找到时返回起始索引，否则返回npos
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_string_view target, const size_t pos) const noexcept {
        if (pos >= M_length) return npos;
        auto res = internal::str_search(M_str + pos, M_length - pos, target.data(), target.length());
        return res ? res - M_str : npos;
    }

    /**
     * @brief 查找指定长度的子视图
     * @param target 要查找的子视图
     * @param pos    起始查找位置
     * @param length 要匹配的子视图长度
     * @return 找到时返回起始索引，否则返回npos
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_string_view target, const size_t pos, const size_t length) const noexcept {
        if (pos + length >= M_length) return npos;
        auto res = internal::str_search(M_str + pos, M_length - pos, target.data(), length);
        return res ? res - M_str : npos;
    }

    /// @brief 查找子视图最后一次出现位置 @copydoc find()
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_string_view target) const noexcept {
        auto res = internal::reverse_boyer_moore_search(M_str, M_length, target.data(), target.length());
        return res ? res - M_str : npos;
    }

    /// @brief 在指定位置前查找最后一次出现 @copydoc find()
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_string_view target, const size_t pos) const noexcept {
        auto res = internal::reverse_boyer_moore_search(
            M_str, std::min(pos, M_length), target.data(), target.length());
        return res ? res - M_str : npos;
    }

    /// @brief 查找指定长度的最后一次出现 @copydoc find()
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_string_view target, const size_t pos, const size_t length) const noexcept {
        if (length >= M_length) return npos;
        auto res = internal::reverse_boyer_moore_search(
            M_str, std::min(pos, M_length), target.data(), length);
        return res ? res - M_str : npos;
    }

    /**
     * @brief 获取子视图
     * @param pos    起始位置
     * @param length 子视图长度
     * @return 新建的视图对象
     * @throws std::out_of_range 当pos + length超过当前视图长度时
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string_view substring(const size_t pos, const size_t length) const {
        if (pos + length > M_length) throw std::out_of_range("string_view::substring");
        return { M_str + pos, length };
    }

    /**
     * @brief 获取从pos到末尾的子视图
     * @param pos 起始位置
     * @return 新建的视图对象
     * @throws std::out_of_range 当pos超过当前视图长度时
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string_view substring(const size_t pos) const {
        if (pos > M_length) throw std::out_of_range("string_view::substring");
        return { M_str + pos, M_length - pos };
    }

private:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    static int M_S_compare(const size_t n1, const size_t n2) noexcept {
        using limits = std::numeric_limits<int>;
        const auto diff = static_cast<difference_type>(n1 - n2);
        if (diff > limits::max()) return limits::max();
        if (diff < limits::min()) return limits::min();
        return static_cast<int>(diff);
    }

public:
    /**
     * @brief 字典序比较
     * @param other 对比的视图
     * @return <0表示当前视图小，0表示相等，>0表示当前视图大
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    int compare(const basic_string_view other) const noexcept {
        int ret = char_traits::compare(M_str, other.M_str, std::min(M_length, other.M_length));
        return ret == 0 ? M_S_compare(M_length, other.M_length) : ret;
    }

    /**
     * @brief 获取左侧子视图
     * @param length 需要保留的左侧长度
     * @return 新建的视图对象
     * @throws std::out_of_range 当length超过当前视图长度时
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string_view left(const size_t length) const noexcept {
        if (length > M_length) throw std::out_of_range("string_view::left");
        return { M_str, length };
    }

    /**
     * @brief 获取右侧子视图
     * @param length 需要保留的右侧长度
     * @return 新建的视图对象
     * @throws std::out_of_range 当length超过当前视图长度时
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string_view right(const size_t length) const noexcept {
        if (length > M_length) throw std::out_of_range("string_view::right");
        return { M_str + (M_length - length), length };
    }

    /**
     * @brief 判断内容相等
     * @param target 对比的视图
     * @return 当长度和内容完全相同时返回true
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const basic_string_view target) const noexcept {
        return M_length == target.M_length && char_traits::compare(M_str, target.M_str, M_length) == 0;
    }

    /**
     * @brief 判断是否包含子视图
     * @param target 要查找的子视图
     * @return 当找到至少一个匹配时返回true
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const basic_string_view target) const noexcept {
        STRIGNITE_IF_UNLIKELY(target.M_length == 0) return false;
        if (target.M_length == 1) return char_traits::find(M_str, M_length, target[0]);
        return internal::str_search(M_str, M_length, target.M_str, target.M_length);
    }

    /**
     * @brief 判断是否以指定子视图开头
     * @param target 要检查的前缀
     * @return 当前视图以target开头时返回true
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const basic_string_view target) const noexcept {
        return basic_string_view{ M_str, target.M_length } == target;
    }

    /**
     * @brief 判断是否以指定子视图结尾
     * @param target 要检查的后缀
     * @return 当前视图以target结尾时返回true
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const basic_string_view target) const noexcept {
        return basic_string_view{ M_str + (M_length - target.M_length), target.M_length } == target;
    }

private:
    /**
     * @brief 统计字符出现次数
     * @param target 要统计的字符
     * @return 该字符在视图中出现的次数
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const char_type target) const noexcept {
        size_t count = 0;
        for (auto index = char_traits::find(data(), size(), target);
             index != nullptr;
             index = char_traits::find(index + 1, size() - (index + 1 - data()), target), count++) {}
        return count;
    }

public:
    /**
     * @brief 统计子视图出现次数
     * @param target 要统计的子视图
     * @return 该子视图在视图中出现的次数（非重叠）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const basic_string_view target) const noexcept {
        STRIGNITE_IF_UNLIKELY(target.size() == 0) return 0;
        if (target.size() == 1) return count(target[0]);

        size_t count = 0;
        for (auto index = internal::str_search(data(), size(), target.data(), target.length());
             index != nullptr;
             index = internal::str_search(
                 index + target.length(), size() - (index + target.length() - data()),
                 target.data(), target.length()), count++) {}
        return count;
    }

public:
    /// 特殊值，表示未找到或无效位置
    static constexpr size_t npos = static_cast<size_t>(-1);

private:
    const char_type* M_str;  ///< 字符序列起始指针
    size_t M_length;         ///< 视图长度（单位：字符数）
}; // end of class basic_string_view

#pragma endregion class string_view

#pragma region utility
namespace internal {
template<typename char_type, typename to_upper_traits = latin1>
STRIGNITE_CPP14_CONSTEXPR
void to_upper_helper(const basic_string_view<char_type> from, char_type* to) noexcept {
    for (size_t i = 0; i < from.size(); i++) {
        to[i] = to_upper_traits::to_upper(from[i]);
    }
}

template<typename char_type, typename to_lower_traits = latin1>
STRIGNITE_CPP14_CONSTEXPR
void to_lower_helper(const basic_string_view<char_type> from, char_type* to) noexcept {
    for (size_t i = 0; i < from.size(); i++) {
        to[i] = to_lower_traits::to_lower(from[i]);
    }
}

template<typename char_type, typename = is_char_t<char_type>>
STRIGNITE_CPP14_CONSTEXPR
void reverse_helper(const basic_string_view<char_type> from, char_type* to) noexcept {
    for (size_t i = from.length() - 1; i != static_cast<size_t>(-1); --i) {
        *to++ = from[i];
    }
}

template<typename char_type, typename char_traits>
STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR
static uint32_t str_hash(const basic_string_view<char_type, char_traits> str) noexcept {
    uint32_t result = 0;
    for (size_t i = 0; i < str.size(); ++i) {
        result = 31 * result + str[i];
    }
    return result;
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
basic_string<char_type> make_literals_string(const char_type*, size_t) noexcept;

template<size_t, typename, typename>
struct static_string_core;

template<size_t Capacity, typename char_type>
struct static_string_core<Capacity, char_type, enable_if_t<Capacity <= 0xFF>> {
    uint8_t M_length;
    char_type M_buffer[Capacity + 1];
};

template<size_t Capacity, typename char_type>
struct static_string_core<Capacity, char_type, enable_if_t<(Capacity > 0xFF) && Capacity <= 0xFFFF>> {
    uint16_t M_length;
    char_type M_buffer[Capacity + 1];
};

template<size_t Capacity, typename char_type>
struct static_string_core<Capacity, char_type, enable_if_t<(Capacity > 0xFFFF) && Capacity <= 100000>> {
    uint32_t M_length;
    char_type M_buffer[Capacity + 1];
};
}
#pragma endregion utility

#pragma region class static_mutable_string

/**
 * @brief 固定容量的可变字符串模板类，提供编译期大小约束
 *
 * @tparam Capacity  字符串最大容量（不包含空终止符）
 * @tparam char_type 字符类型（如char、wchar_t）
 * @tparam char_traits 字符特性类，提供字符操作
 *
 * 本类实现了一个栈分配的字符串，适用于禁止或不宜使用动态内存的场景。
 * 维护一个大小为Capacity+1的固定缓冲区（包含空终止符），并在运行时强制容量限制。
 */
template<size_t Capacity, typename char_type, typename char_traits>
class basic_static_mutable_string final {
public:
    static constexpr size_t npos = static_cast<size_t>(-1);
    static constexpr size_t max_length = Capacity;

    using size_t = std::size_t;

    using string_view_t = basic_string_view<char_type, char_traits>;

private:
    STRIGNITE_CPP20_CONSTEXPR void M_init_data() noexcept {
        STRIGNITE_IF_CONSTANT_EVALUATED(
            M_data = {}
        )
    }

    /**
      * @brief 从C风格字符串构造
      * @param str 要复制的C风格字符串
      * @param length 字符串长度
      * @throws std::runtime_error 当str为nullptr或长度超过容量
      */
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_mutable_string(const char_type* str, size_t length) {
        STRIGNITE_IF_UNLIKELY(!str)
            throw std::runtime_error("string pointer cannot be nullptr.");
        STRIGNITE_IF_UNLIKELY(length > size())
            throw std::runtime_error("");

        M_init_data();

        auto ptr = M_pointer();
        char_traits::copy(ptr, str, length);
        M_set_length(length);
    }

    /**
     * @brief 构造空字符串
     */
    STRIGNITE_CPP20_CONSTEXPR basic_static_mutable_string() noexcept :
        M_data() {}

    /**
     * @brief 从C风格字符串构造
     * @param str 要复制的C风格字符串
     * @throws std::runtime_error 当str为nullptr或长度超过容量
     */
    STRIGNITE_CPP20_CONSTEXPR basic_static_mutable_string(const char_type* str) :
        basic_static_mutable_string(str, internal::traits_length<char_type>(str)) {}

    /**
     * @brief 从标准字符串构造
     * @param str 要复制的标准字符串
     * @throws std::runtime_error 当长度超过容量
     */
    STRIGNITE_CPP20_CONSTEXPR basic_static_mutable_string(const std::basic_string<char_type>& str) :
        basic_static_mutable_string(str.data(), str.size()) {}

    /**
     * @brief 拷贝构造函数
     * @param other 要复制的源字符串
     */
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_mutable_string(const basic_static_mutable_string& other) noexcept {
        STRIGNITE_IF_UNLIKELY(&other == this) return;

        M_data = other.M_data;
    }

    /**
     * @brief 移动构造函数
     * @param other 要移动的源字符串（移动后为空）
     */
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_mutable_string(basic_static_mutable_string&& other) noexcept {
        STRIGNITE_IF_UNLIKELY(&other == this) return;

        M_data = other.M_data;
        other.M_data = {};
    }

    /// @brief 析构函数（默认）
    STRIGNITE_CPP20_CONSTEXPR ~basic_static_mutable_string() noexcept = default;

    /**
     * @brief 拷贝赋值运算符
     * @param other 要复制的源字符串
     * @return 本字符串的引用
     */
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_mutable_string& operator=(const basic_static_mutable_string& other) {
        STRIGNITE_IF_UNLIKELY(&other == this) return *this;
        M_data = other.M_data;
        return *this;
    }

    /**
     * @brief 移动赋值运算符
     * @param other 要移动的源字符串
     * @return 本字符串的引用
     */
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_mutable_string& operator=(basic_static_mutable_string&& other) noexcept {
        STRIGNITE_IF_UNLIKELY(&other == this) return *this;
        M_data = other.M_data;
        other.M_data = {};
        return *this;
    }

public:
    /**
     * @brief 清空字符串内容
     * @post size() == 0, data()[0] == 空字符
     */
    STRIGNITE_CPP20_CONSTEXPR
    void clear() noexcept {
        M_pointer()[0] = static_cast<char_type>(0);
        M_set_length(0);
    }

    /**
     * @brief 调整字符串长度
     * @param n 新的字符串长度
     * @throws std::runtime_error 当n超过最大容量
     * @post size() == min(n, 原长度), data()[size()] == 空字符
     */
    void resize(const size_t n) {
        if (n > max_length)
            throw std::runtime_error("");
        M_pointer()[n] = static_cast<char_type>(0);
        M_set_length(std::min(length(), n));
    }

#pragma region append & prepend

private:
    /**
     * @brief 追加单个字符
     * @param value 要追加的字符
     * @return 本字符串的引用
     * @throws std::runtime_error 当长度超过容量
     */
    basic_static_mutable_string& append(const char_type value) {
        if (value == static_cast<char_type>(0)) return *this;

        const auto new_length = size() + 1;

        if (new_length > max_length)
            throw std::runtime_error("Length of string cannot be greater than max capacity.");

        M_pointer()[new_length - 1] = value;
        M_pointer()[new_length] = static_cast<char_type>(0);
        M_set_length(new_length);

        return *this;
    }

public:
    /**
     * @brief 追加字符串
     * @param value 要追加的字符指针
     * @param value_length 追加字符数量
     * @return 本字符串的引用
     * @throws std::runtime_error 当value为null或容量不足
     */
    basic_static_mutable_string& append(const char_type* value, const size_t value_length) {
        STRIGNITE_IF_UNLIKELY(!value)
            throw std::runtime_error("string pointer cannot be nullptr.");

        STRIGNITE_IF_UNLIKELY(value[0] == static_cast<char_type>(0)) return *this;
        if (value[1] == static_cast<char_type>(0)) return append(value[0]);

        const auto new_length = size() + value_length;

        if (new_length > max_length)
            throw std::runtime_error("Length of string cannot be greater than max capacity.");

        char_traits::copy(M_pointer() + size(), value, value_length);
        M_pointer()[new_length] = static_cast<char_type>(0);
        M_set_length(new_length);

        return *this;
    }

    /**
     * @brief 追加C风格字符串
     * @param value 要追加的C风格字符串
     * @return 本字符串的引用
     */
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& append(const char_type* value) {
        return append(value, internal::traits_length<char_traits>(value));
    }

private:
    /**
     * @brief 前置单个字符
     * @param value 要前置的字符
     * @return 本字符串的引用
     * @throws std::runtime_error 当容量不足
     */
    basic_static_mutable_string& prepend(const char_type value) {
        if (value == static_cast<char_type>(0)) return *this;

        const auto new_length = size() + 1;

        if (new_length > max_length)
            throw std::runtime_error("Length of string cannot be greater than max capacity.");

        char_traits::move(M_pointer() + 1, data(), size());
        M_pointer()[0] = value;
        M_pointer()[new_length] = static_cast<char_type>(0);
        M_set_length(new_length);

        return *this;
    }

public:
    /**
     * @brief 前置字符序列
     * @param value 要前置的字符指针
     * @param value_length 前置字符数量
     * @return 本字符串的引用
     * @throws std::runtime_error 当value为null或容量不足
     */
    basic_static_mutable_string& prepend(const char_type* value, const size_t value_length) {
        STRIGNITE_IF_UNLIKELY(!value)
            throw std::runtime_error("string pointer cannot be nullptr.");

        STRIGNITE_IF_UNLIKELY(value[0] == static_cast<char_type>(0)) return *this;
        if (value[1] == static_cast<char_type>(0)) return prepend(value[0]);

        const auto new_length = size() + value_length;

        if (new_length > max_length)
            throw std::runtime_error("Length of string cannot be greater than max capacity.");

        char_traits::move(M_pointer() + value_length, data(), size());
        char_traits::copy(M_pointer(), value, value_length);
        M_pointer()[new_length] = static_cast<char_type>(0);
        M_set_length(new_length);

        return *this;
    }

    /**
     * @brief 前置C风格字符串
     * @param value 要前置的C风格字符串
     * @return 本字符串的引用
     */
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& prepend(const char_type* value) {
        return prepend(value, internal::traits_length<char_traits>(value));
    }

#pragma endregion append & prepend

    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& operator+=(const char_type* value) {
        return append(value, internal::traits_length<char_traits>(value));
    }

#if 1

private:
    template<size_t buffer_size, typename = internal::enable_if_t<buffer_size <= 0x400>>
    STRIGNITE_CPP20_CONSTEXPR
    static void replace_helper(char_type*& _str, size_t& _length,
                               const char_type* old_value, const size_t old_size,
                               const char_type* new_value, const size_t new_size) {
        STRIGNITE_IF_UNLIKELY(old_size == 0) return;

        STRIGNITE_ASSUME(_str != nullptr && old_value != nullptr && new_value != nullptr);

        const auto old_value_length = old_size;
        const auto new_value_length = new_size;

        if (old_value_length == new_value_length) {
            STRIGNITE_IF_UNLIKELY(old_value == new_value ||
                char_traits::compare(old_value, new_value, old_value_length) == 0) {
                return;
            }
            for (auto* index = internal::str_search(_str, _length, old_value, old_value_length);
                 index != nullptr;
                 index = internal::str_search(
                     index + old_value_length, _length - (index + old_value_length - _str), old_value, old_value_length
                 )
            ) {
                char_traits::copy(const_cast<char_type *>(index), new_value, new_value_length);
                return;
            }
        }

        // 相当于精简版的 small_vector<char_type*, 64>
        char_type* indexes[buffer_size];
        char_type** indexes_ptr = indexes;
        size_t indexes_capacity = buffer_size - 1;
        size_t indexes_size = 0;

        STRIGNITE_DEFER {
            if (indexes_ptr != indexes)
                delete[] indexes_ptr;
        };

        for (auto index = internal::str_search(_str, _length, old_value, old_value_length);
             index != nullptr;
             index = internal::str_search(
                 index + old_value_length, _length - (index + old_value_length - _str), old_value, old_value_length
             )
        ) {
            STRIGNITE_IF_UNLIKELY(indexes_size >= indexes_capacity) {
                const auto new_indexes_capacity = indexes_size * 2;
                auto* temp = new char_type *[new_indexes_capacity];
                memcpy(temp, indexes_ptr, sizeof(char_type *) * indexes_size);
                if (indexes_ptr != indexes) {
                    delete[] indexes_ptr;
                }
                indexes_capacity = new_indexes_capacity - 1;
                indexes_ptr = temp;
            }
            indexes_ptr[indexes_size++] = const_cast<char_type *>(index);
        }

        if (indexes_size == 0) {
            return;
        }

        const auto new_str_length = _length - old_value_length * indexes_size + new_value_length * indexes_size;

        if (new_str_length > max_length)
            throw std::runtime_error("Length of string cannot be greater than max capacity.");

        const auto differance_size = static_cast<int64_t>(new_value_length - old_value_length);

        if (new_value_length > old_value_length) {
            const char_type* last_right_end = _str + _length;
            for (size_t i = indexes_size; i > 0; --i) {
                auto index = indexes_ptr[i - 1];
                auto right = index + old_value_length;
                auto right_size = last_right_end - right;
                last_right_end = index;
                auto new_right = right + differance_size * i;
                char_traits::move(new_right, right, right_size);
                char_traits::copy(new_right - new_value_length, new_value, new_value_length);
            }
        } else {
            indexes_ptr[indexes_size] = _str + _length;
            for (size_t i = 0; i < indexes_size; i++) { // indexes_size 不会等于或小于 0
                auto index = indexes_ptr[i];
                auto right = index + old_value_length;
                auto right_size = indexes_ptr[i + 1] - right;
                auto diff = differance_size * i;
                char_traits::copy(index + diff, new_value, new_value_length);
                char_traits::move(right + diff + differance_size, right, right_size);
            }
        }

        _length = new_str_length;
    }

    template<size_t buffer_size>
    basic_static_mutable_string& replace_helper(const string_view_t old_value, const string_view_t new_value) {
        replace_helper<buffer_size>(
            const_cast<char_type *&>(M_data.M_buffer), M_data.M_length,
            old_value.data(), old_value.size(), new_value.data(), new_value.size()
        );
        return *this;
    }

public:
    /**
     * @brief 替换所有匹配的子串
     * @tparam buffer_size 内部缓冲区大小（默认64），影响大替换操作性能
     * @param old_value 要被替换的子串视图
     * @param new_value 替换后的新子串视图
     * @return 本字符串的引用
     * @throws std::runtime_error 当指针为空或结果超出容量
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& replace(const char_type* const old_value, const char_type* const new_value) {
        return replace_helper<buffer_size>(string_view_t{ old_value }, string_view_t{ new_value });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& replace(const char_type* const old_value, const string_view_t new_value) {
        STRIGNITE_IF_UNLIKELY(!new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(string_view_t{ old_value }, new_value);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& replace(const string_view_t old_value, const char_type* const new_value) {
        STRIGNITE_IF_UNLIKELY(!old_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(old_value, string_view_t{ new_value });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& replace(const string_view_t old_value, const string_view_t new_value) {
        STRIGNITE_IF_UNLIKELY(!old_value.data() || !new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(old_value, new_value);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& replace(const basic_static_mutable_string& old_value, const string_view_t new_value) {
        STRIGNITE_IF_UNLIKELY(!new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(old_value.to_string_view(), new_value);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& replace(const string_view_t old_value, const basic_static_mutable_string& new_value) {
        STRIGNITE_IF_UNLIKELY(!old_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(old_value, new_value.to_string_view());
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string&
    replace(const basic_static_mutable_string& old_value, const basic_static_mutable_string& new_value) {
        return replace_helper<buffer_size>(old_value.to_string_view(), new_value.to_string_view());
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string&
    replace(const basic_static_mutable_string& old_value, const char_type* const new_value) {
        return replace_helper<buffer_size>(old_value.to_string_view(), string_view_t{ new_value });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string&
    replace(const char_type* const old_value, const basic_static_mutable_string& new_value) {
        return replace_helper<buffer_size>(string_view_t{ old_value }, new_value.to_string_view());
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& remove(const char_type* const value) {
        constexpr char_type empty = { static_cast<char_type>(0) };
        return replace<buffer_size>(value, empty);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& remove(const string_view_t value) {
        STRIGNITE_IF_UNLIKELY(!value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        constexpr char_type empty = { static_cast<char_type>(0) };
        return replace<buffer_size>(value, empty);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& remove(const basic_static_mutable_string& value) {
        constexpr char_type empty = { static_cast<char_type>(0) };
        return replace<buffer_size>(value, empty);
    }

private:
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& insert_helper(const size_t pos, const string_view_t value) {
        const auto length = size();
        STRIGNITE_IF_UNLIKELY(pos > length)
            throw std::out_of_range("bcs::basic_mutable_string::insert");
        auto new_length = length + value.size();
        if (new_length > max_length)
            throw std::runtime_error("");
        auto ptr = M_pointer();
        auto right = ptr + pos;
        char_traits::move(right + value.size(), right, ptr + length - right);
        char_traits::copy(right, value.data(), value.size());
        ptr[new_length] = static_cast<char_type>(0);
        return *this;
    }

public:
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& insert(const size_t pos, const char_type* const value) {
        return insert_helper(pos, string_view_t{ value });
    }

    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& insert(const size_t pos, const string_view_t value) {
        STRIGNITE_IF_UNLIKELY(!value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return insert_helper(pos, value);
    }

    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& insert(const size_t pos, const basic_static_mutable_string& value) {
        return insert_helper(pos, value.to_string_view());
    }

    /**
     * @brief 转换为全小写
     * @tparam to_lower_traits 大小写转换特性（默认latin1）
     * @return 本字符串的引用
     */
    template<typename to_lower_traits = latin1>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& to_lower() noexcept {
        internal::to_lower_helper<char_type, to_lower_traits>(to_string_view(), M_pointer());
        return *this;
    }

    /**
     * @brief 转换为全大写
     * @tparam to_upper_traits 大小写转换特性（默认latin1）
     * @return 本字符串的引用
     */
    template<typename to_upper_traits = latin1>
    STRIGNITE_FORCE_INLINE
    basic_static_mutable_string& to_upper() noexcept {
        internal::to_upper_helper<char_type, to_upper_traits>(to_string_view(), M_pointer());
        return *this;
    }

public:
#pragma region find

    /**
     * @brief 查找子串首次出现位置
     * @param target 要查找的目标子串视图
     * @return 找到时返回起始索引，否则返回npos
     * @throws std::runtime_error 当target为nullptr
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_static_mutable_string& target) const {
        return to_string_view().find(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target) const {
        return to_string_view().find(string_view_t{ target });
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target, const size_t start_pos) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target, start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_static_mutable_string& target, const size_t start_pos) const {
        return to_string_view().find(target.to_string_view(), start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target, const size_t start_pos) const {
        return to_string_view().find(string_view_t{ target }, start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target, const size_t start_pos, const size_t length) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target, start_pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_static_mutable_string& target, const size_t start_pos, const size_t length) const {
        return to_string_view().find(target.to_string_view(), start_pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target, const size_t start_pos, const size_t length) const {
        return to_string_view().find(string_view_t{ target }, start_pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_static_mutable_string& target) const noexcept {
        return to_string_view().find_last(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target) const noexcept {
        return to_string_view().find_last(string_view_t{ target });
    }

    /**
     * @brief 从指定位置查找子串
     * @param target 目标子串视图
     * @param pos 起始查找位置
     * @return 找到时返回起始索引，否则返回npos
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target, const size_t pos) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target, pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_static_mutable_string& target, const size_t pos) const noexcept {
        return to_string_view().find_last(target.to_string_view(), pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target, const size_t pos) const noexcept {
        return to_string_view().find_last(string_view_t{ target }, pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target, const size_t pos, const size_t length) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target, pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_static_mutable_string& target, const size_t pos, const size_t length) const noexcept {
        return to_string_view().find_last(target.to_string_view(), pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target, const size_t pos, const size_t length) const noexcept {
        return to_string_view().find_last(string_view_t{ target }, pos, length);
    }

#pragma endregion find

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().count(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const basic_static_mutable_string& target) const noexcept {
        return to_string_view().count(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const char_type* const target) const noexcept {
        return to_string_view().count(string_view_t{ target });
    }

#pragma region contains

private:
    template<size_t buffer_size, typename = internal::enable_if_t<buffer_size <= 0x400>>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR
    bool contains_helper(const string_view_t target, const bool ignore_case = false) const noexcept {
        STRIGNITE_IF_UNLIKELY(target.empty()) return false;
        if (target.size() == 1) return contains(target[0], ignore_case);
        if (!ignore_case) {
            return to_string_view().contains(target);
        }
        auto lower = this->to_lower();
        auto lower_data = lower.to_string_view();
        if (target.size() > buffer_size) {
            auto* temp = new char_type[target.size()];
            STRIGNITE_DEFER { delete[] temp; };
            for (int i = 0; i < target.size(); ++i) {
                temp[i] = static_cast<char_type>(bcs::to_lower(target[i]));
            }
            return internal::str_search(lower_data.data(), lower_data.size(), temp, target.size());
        }
        char_type temp[buffer_size];
        for (int i = 0; i < target.size(); ++i) {
            temp[i] = static_cast<char_type>(bcs::to_lower(target[i]));
        }
        return internal::str_search(lower_data.data(), lower_data.size(), temp, target.size());
    }

    //可以优化
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR bool contains(char_type target, const bool ignore_case = false) const noexcept {
        if (ignore_case) {
            char_type lower_target_ch = bcs::to_lower(target);
            auto lower = this->to_lower();
            auto lower_data = lower.to_string_view();
            return char_traits::find(lower_data.data(), lower_data.size(), lower_target_ch);
        }
        auto view = to_string_view();
        return char_traits::find(view.data(), view.size(), target);
    }

public:
    /**
     * @brief 检查当前字符串是否包含指定目标子串
     *
     * 本函数提供高效的子串存在性检测，支持大小写敏感/不敏感两种模式。通过模板参数实现栈内存优化，
     * 避免小规模查找时的堆内存分配。
     *
     * @tparam buffer_size 内部查找缓冲区大小（默认64），控制用于存储中间结果的栈数组容量，
     *                     根据典型场景调整可优化性能
     *
     * @param[in] target       要查找的目标子串指针（必须为有效空终止字符串）
     * @param[in] ignore_case  是否启用忽略大小写模式（默认false，即大小写敏感）
     *
     * @return bool
     *  - true  : 字符串包含目标子串
     *  - false : 不包含目标子串或target为空指针/空字符串
     *
     * @note 特性说明：
     * - 空指针/空字符串检测：立即返回false
     * - 单字符优化：直接调用字符版contains函数
     * - 多字符处理：通过contains_helper模板函数实现核心逻辑
     * - 常量表达式：支持C++20下的编译期求值
     * - 强制内联：通过编译器指令优化小型函数调用开销
     *
     * @warning 使用注意：
     * - 目标字符串须以空字符结尾，否则可能引发未定义行为
     * - 忽略大小写模式的具体实现依赖char_traits的特性实现
     * - buffer_size过大可能导致栈溢出，建议保持<=256
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const string_view_t target, const bool ignore_case = false) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return contains_helper<buffer_size>(target, ignore_case);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const basic_static_mutable_string& target, const bool ignore_case = false) const noexcept {
        return contains_helper<buffer_size>(target, ignore_case);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const char_type* const target, const bool ignore_case = false) const noexcept {
        return contains_helper<buffer_size>(string_view_t{ target }, ignore_case);
    }

#pragma endregion contains

#pragma region starts ends with

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const string_view_t target) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        return to_string_view().ends_with(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const basic_static_mutable_string& target) const {
        return to_string_view().ends_with(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const char_type* const target) const {
        return to_string_view().ends_with(string_view_t{ target });
    }

    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool ends_with(const string_view_t target, const bool ignore_case) const noexcept {
    //     STRIGNITE_IF_UNLIKELY(!target.data())
    //         throw std::invalid_argument("string pointer cannot be nullptr.");
    //     if (ignore_case) {
    //         if (target.length() > size()) return false;
    //         return to_lower().ends_with(basic_string{ target }.to_lower());
    //     }
    //     return ends_with(target);
    // }
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool ends_with(const basic_static_mutable_string& target, const bool ignore_case) const noexcept {
    //     if (ignore_case) {
    //         if (target.length() > size()) return false;
    //         return to_lower().ends_with(target.to_lower());
    //     }
    //     return ends_with(target);
    // }
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool ends_with(const char_type* const target, const bool ignore_case) const noexcept {
    //     if (ignore_case) {
    //         return to_lower().ends_with(basic_string{ target }.to_lower());
    //     }
    //     return ends_with(target);
    // }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        return to_string_view().starts_with(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const char_type* const target) const noexcept {
        return to_string_view().starts_with(string_view_t{ target });
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const basic_static_mutable_string& target) const noexcept {
        return to_string_view().starts_with(target.to_string_view());
    }

    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool starts_with(const string_view_t target, const bool ignore_case) const noexcept {
    //     STRIGNITE_IF_UNLIKELY(!target.data())
    //         throw std::invalid_argument("string pointer cannot be nullptr.");
    //     if (ignore_case) {
    //         if (target.length() > size()) return false;
    //         return to_lower().starts_with(basic_string{ target }.to_lower());
    //     }
    //     return starts_with(target);
    // }
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool starts_with(const char_type* const target, const bool ignore_case) const noexcept {
    //     if (ignore_case) {
    //         return to_lower().starts_with(basic_string{ target }.to_lower());
    //     }
    //     return starts_with(target);
    // }
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool starts_with(const basic_static_mutable_string& target, const bool ignore_case) const noexcept {
    //     if (ignore_case) {
    //         if (target.length() > size()) return false;
    //         return to_lower().starts_with(target.to_lower());
    //     }
    //     return starts_with(target);
    // }

#pragma endregion starts ends with

#pragma region equals

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const string_view_t other) const noexcept {
        return *this == other;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const basic_static_mutable_string& other) const noexcept {
        return *this == other;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const char_type* const other) const noexcept {
        return *this == other;
    }

    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool equals(const string_view_t other, const bool ignore_case) const noexcept {
    //     return ignore_case ? to_lower() == basic_string{ other }.to_lower().to_string_view() : *this == other;
    // }
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool equals(const basic_mutable_string& other, const bool ignore_case) const noexcept {
    //     return ignore_case ? to_lower() == other.to_lower() : *this == other;
    // }
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    // bool equals(const char_type* const other, const bool ignore_case) const noexcept {
    //     return ignore_case ? to_lower() == basic_string{ other }.to_lower() : *this == other;
    // }

#pragma endregion equals

#endif

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    string_view_t to_string_view() const noexcept {
        return { data(), size() };
    }

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t length() const noexcept {
        return size();
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t byte_length() const noexcept {
        return size() * sizeof(char_type);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t size() const noexcept {
        return M_data.M_length;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool empty() const noexcept {
        return size() == 0;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    const char_type* data() const noexcept {
        return M_pointer();
    }

public:
#pragma region to number

    /**
     * @brief 将字符串内容转换为8位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int8_t
     *  - 成功转换时返回对应的8位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-128, 127]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int8_t to_int8() const noexcept {
        return internal::to_int_not_check_nullptr<int8_t>(data());
    }

    /**
     * @brief 将字符串内容转换为8位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint8_t
     *  - 成功转换时返回对应的8位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 255]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint8_t to_uint8() const noexcept {
        return internal::to_int_not_check_nullptr<uint8_t>(data());
    }

    /**
     * @brief 将字符串内容转换为16位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int16_t
     *  - 成功转换时返回对应的16位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-32768, 32767]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int16_t to_int16() const noexcept {
        return internal::to_int_not_check_nullptr<int16_t>(data());
    }

    /**
     * @brief 将字符串内容转换为16位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int16_t
     *  - 成功转换时返回对应的16位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 65535]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint16_t to_uint16() const noexcept {
        return internal::to_int_not_check_nullptr<uint16_t>(data());
    }

    /**
     * @brief 将字符串内容转换为32位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int32_t
     *  - 成功转换时返回对应的32位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-2147483648, 2147483647]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int32_t to_int32() const noexcept {
        return internal::to_int_not_check_nullptr<int32_t>(data());
    }

    /**
     * @brief 将字符串内容转换为32位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint32_t
     *  - 成功转换时返回对应的32位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 4294967295]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t to_uint32() const noexcept {
        return internal::to_int_not_check_nullptr<uint32_t>(data());
    }

    /**
     * @brief 将字符串内容转换为64位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int64_t
     *  - 成功转换时返回对应的64位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-9223372036854775808, 9223372036854775807]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int64_t to_int64() const noexcept {
        return internal::to_int_not_check_nullptr<int64_t>(data());
    }

    /**
     * @brief 将字符串内容转换为64位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint64_t
     *  - 成功转换时返回对应的64位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 18446744073709551615]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint64_t to_uint64() const noexcept {
        return internal::to_int_not_check_nullptr<uint64_t>(data());
    }

    /**
     * @brief 将字符串内容转换为单精度浮点数
     *
     * 本函数提供高性能的字符串到浮点数转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return float
     *  - 成功转换时返回对应的单精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0f或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法浮点数字符序列（如"3.14"、"-0.5e3"）
     *   - 数值在float类型表示范围内
     * - 非法输入可能导致未定义行为（包括数值截断、INFINITY或NAN）
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    float to_float() const noexcept {
        return std::strtof(data(), nullptr);
    }

    /**
     * @brief 将字符串内容转换为双精度浮点数
     *
     * 本函数提供高性能的字符串到双精度浮点数转换，
     * 适用于需要高精度数值转换且数据源可信的场景。
     *
     * @return double
     *  - 成功转换时返回对应的双精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 格式符合C标准浮点数表示规范
     *   - 数值在double类型表示范围内
     * - 非法格式可能导致静默错误或精度丢失
     * - 科学计数法支持依赖本地化设置
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    double to_double() const noexcept {
        return std::strtod(data(), nullptr);
    }

    /**
     * @brief 将字符串内容转换为扩展精度浮点数
     *
     * 本函数提供最高精度的字符串到浮点数转换，适用于科学计算等需要高精度
     * 数值解析的场景。
     *
     * @return long double
     *  - 成功转换时返回扩展精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0L或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 格式兼容long double类型的解析要求
     *   - 数值在long double表示范围内
     * - 不同平台精度表现可能不一致（80位/128位等）
     * - 十六进制浮点格式（如0x1.2p3）支持依赖编译器实现
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    long double to_long_double() const noexcept {
        return std::strtold(data(), nullptr);
    }

#pragma endregion to number

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type* M_pointer() const noexcept {
        return M_data.M_buffer;
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_length(const size_t length) noexcept {
        M_data.M_length = length;
    }

private:
    internal::static_string_core<Capacity, char_type, void> M_data;
};

#pragma endregion class static_mutable_string

#pragma region class static_string

template<size_t Capacity, typename char_type, typename char_traits>
class basic_static_string final {
public:
    static constexpr size_t npos = static_cast<size_t>(-1);
    static constexpr size_t max_length = Capacity;

    using size_t = std::size_t;

    using string_view_t = basic_string_view<char_type, char_traits>;

private:
    STRIGNITE_CPP20_CONSTEXPR void M_construct_data(const string_view_t str) {
        M_init_data();
        if (str.length() > max_length)
            throw std::runtime_error("Length of other string cannot be greater than init capacity.");
        char_traits::copy(M_pointer(), str.data(), str.length());
        M_set_length(str.length());
        M_pointer()[str.length()] = static_cast<char_type>(0);
    }

    STRIGNITE_CPP20_CONSTEXPR void M_init_data() noexcept {
        STRIGNITE_IF_CONSTANT_EVALUATED(
            M_data = {}
        )
    }

public:
    STRIGNITE_CPP20_CONSTEXPR basic_static_string() noexcept {
        STRIGNITE_IF_CONSTANT_EVALUATED(
            M_data = {}
        ) STRIGNITE_ELSE_NOT_CONSTANT_EVALUATED(
            M_set_length(0);
            M_pointer()[0] = static_cast<char_type>(0)
        )
    }

    STRIGNITE_CPP20_CONSTEXPR basic_static_string(const char_type* str, const size_t length) noexcept {
        STRIGNITE_IF_UNLIKELY(!str)
            throw std::invalid_argument("string pointer cannot be nullptr.");
        M_construct_data({ str, length });
    }

    STRIGNITE_CPP20_CONSTEXPR basic_static_string(const char_type* str) noexcept {
        M_construct_data(str);
    }

    STRIGNITE_CPP20_CONSTEXPR basic_static_string(const string_view_t str) noexcept {
        M_construct_data(str);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_string(const basic_static_string<OtherCapacity, char_type, char_traits>& other) {
        if (other.size() > max_length)
            throw std::runtime_error("Length of other string cannot be greater than init capacity.");
        M_init_data();
        M_set_length(other.size());
        char_traits::copy(M_pointer(), other.data(), other.size());
        M_pointer()[length()] = static_cast<char_type>(0);
    }

    STRIGNITE_CPP20_CONSTEXPR
    basic_static_string(const basic_static_string& other) noexcept {
        M_data = other.M_data;
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_string(basic_static_string<OtherCapacity, char_type, char_traits>&& other) noexcept :
        basic_static_string(other) {}

    STRIGNITE_CPP20_CONSTEXPR
    basic_static_string(basic_static_string&& other) noexcept :
        basic_static_string(other) {}

    STRIGNITE_CPP20_CONSTEXPR ~basic_static_string() noexcept = default;

    template<size_t OtherCapacity>
    basic_static_string& operator=(const basic_static_string<OtherCapacity, char_type, char_traits>& other) {
        if (other.size() > max_length)
            throw std::runtime_error("Length of other string cannot be greater than init capacity.");
        M_init_data();
        char_traits::copy(M_pointer(), other.data(), other.size());
        M_set_length(other.size());
        M_pointer()[length()] = static_cast<char_type>(0);
        return *this;
    }

    basic_static_string& operator=(const basic_static_string& other) noexcept = default;

    template<size_t OtherCapacity>
    basic_static_string& operator=(basic_static_string<OtherCapacity, char_type, char_traits>&& other) noexcept {
        if (other.size() > max_length)
            throw std::runtime_error("Length of other string cannot be greater than init capacity.");
        M_init_data();
        char_traits::copy(M_pointer(), other.data(), other.size());
        M_set_length(other.size());
        M_pointer()[length()] = static_cast<char_type>(0);
        return *this;
    }

    basic_static_string& operator=(basic_static_string&& other) noexcept = default;

private:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_static_string concat_helper(const string_view_t str) const {
        basic_static_string res = M_reserve(length() + str.length());
        auto ptr = M_pointer();
        char_traits::copy(ptr, data(), length());
        char_traits::copy(ptr + length(), str.data(), str.length());
        return res;
    }

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string operator+(const char_type* const str) const {
        return concat_helper(str);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string operator+(const string_view_t str) const {
        return concat_helper(str);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_static_string
    operator+(const basic_static_string<OtherCapacity, char_type, char_traits>& other) const {
        return concat_helper(other);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string operator+(const basic_static_string& other) const {
        return concat_helper(other);
    }

    STRIGNITE_FORCE_INLINE
    basic_static_string& operator+=(const string_view_t str) {
        return *this = concat_helper(str);
    }

    STRIGNITE_FORCE_INLINE
    basic_static_string& operator+=(const char_type* const str) {
        return *this = concat_helper(str);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string& operator+=(const basic_static_string<OtherCapacity, char_type, char_traits>& other) {
        return *this = concat_helper(other);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string& operator+=(const basic_static_string& other) {
        return *this = concat_helper(other);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator==(const char_type* const target) const noexcept {
        return equals(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator==(const string_view_t target) const noexcept {
        return equals(target);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator==(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return equals(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator!=(const char_type* const target) const noexcept {
        return !(*this == target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator!=(const string_view_t target) const noexcept {
        return !(*this == target);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator!=(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return !(*this == target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    operator string_view_t() const noexcept {
        return to_string_view();
    }

    STRIGNITE_FORCE_INLINE
    friend std::basic_ostream<char_type>& operator
    <<(std::basic_ostream<char_type>& os, const basic_static_string& str) {
        return os << str.to_string_view();
    }

    friend std::basic_istream<char_type>&
    operator>>(std::basic_istream<char_type>& is, basic_static_string& str) {
        str = {};
        constexpr size_t buffer_size = 512 / sizeof(char_type);
        char_type buffer[buffer_size];
        size_t length = 0;
        while (true) {
            if (length >= buffer_size) {
                str += basic_static_string{ buffer, length };
                length = 0;
                continue;
            }
            const auto ch = static_cast<char_type>(is.get());
            if (is_space(ch)) {
                str += basic_static_string{ buffer, length };
                break;
            }
            buffer[length++] = ch;
        }
        return is;
    }

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().equals(target);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return to_string_view().equals(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const char_type* const target) const noexcept {
        return to_string_view().equals(string_view_t{ target });
    }

#pragma region finds

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return to_string_view().find(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target) const noexcept {
        return to_string_view().find(string_view_t{ target });
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target, const size_t pos) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target, pos);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_static_string<OtherCapacity, char_type, char_traits>& target,
                const size_t pos) const noexcept {
        return to_string_view().find(target.to_string_view(), pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target, const size_t pos) const noexcept {
        return to_string_view().find(string_view_t{ target }, pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target, const size_t pos, const size_t length) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target, pos, length);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_static_string<OtherCapacity, char_type, char_traits>& target,
                const size_t pos, const size_t length) const noexcept {
        return to_string_view().find(target.to_string_view(), pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target, const size_t pos, const size_t length) const noexcept {
        return to_string_view().find(string_view_t{ target }, pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return to_string_view().find_last(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target) const noexcept {
        return to_string_view().find_last(string_view_t{ target });
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target, const size_t pos) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target, pos);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_static_string<OtherCapacity, char_type, char_traits>& target,
                     const size_t pos) const noexcept {
        return to_string_view().find_last(target.to_string_view(), pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target, const size_t pos) const noexcept {
        return to_string_view().find_last(string_view_t{ target }, pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target, const size_t pos, const size_t length) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target, pos, length);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_static_string<OtherCapacity, char_type, char_traits>& target,
                     const size_t pos, const size_t length) const noexcept {
        return to_string_view().find_last(target.to_string_view(), pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target, const size_t pos, const size_t length) const noexcept {
        return to_string_view().find_last(string_view_t{ target }, pos, length);
    }

#pragma endregion finds

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().count(target);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return to_string_view().count(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const char_type* const target) const noexcept {
        return to_string_view().count(string_view_t{ target });
    }

    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR basic_static_string replace(const char_type* const haystack,
    //                                                 const char_type* const needle) const {}
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR basic_static_string trim(char_type ch) const noexcept {}
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR basic_static_string trim(const char_type* const charset) const noexcept {}

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string substring(size_t pos, size_t length) const noexcept {
        return to_string_view().substring(pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string substring(size_t pos) const noexcept {
        return to_string_view().substring(pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string left(size_t length) const noexcept {
        return to_string_view().left(length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string right(size_t length) const noexcept {
        return to_string_view().right(length);
    }

    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR
    // basic_static_string left_justify(size_type length,
    //                                  char_type fill_char = static_cast<char_type>(' ')) const noexcept {}
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR
    // basic_static_string right_justify(size_type length,
    //                                   char_type fill_char = static_cast<char_type>(' ')) const noexcept {}
    //
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR
    // basic_static_string center(size_type length, char_type fill_char = static_cast<char_type>(' ')) const noexcept {}

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_string repeat(const size_t times) const {
        basic_static_string result = M_reserve(length() * times);
        auto ptr = result.M_pointer();
        for (size_t i = 0; i < times; i++) {
            char_traits::copy(ptr, data(), length());
            ptr += length();
        }
        return result;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR
    basic_static_string reverse() const noexcept {
        basic_static_string result = M_reserve(length());
        internal::reverse_helper(to_string_view(), result.M_pointer());
        return result;
    }

    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR std::vector<basic_static_string> split(const char_type* const delim) const noexcept {}

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().starts_with(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const char_type* const target) const noexcept {
        return to_string_view().starts_with(string_view_t{ target });
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return to_string_view().starts_with(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().ends_with(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const char_type* const target) const noexcept {
        return to_string_view().ends_with(string_view_t{ target });
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return to_string_view().ends_with(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().contains(target);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const basic_static_string<OtherCapacity, char_type, char_traits>& target) const noexcept {
        return to_string_view().contains(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const char_type* const target) const noexcept {
        return to_string_view().contains(string_view_t{ target });
    }

#pragma region insert

private:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_static_string insert(const size_t pos, char_type value) const {
        assert(value != static_cast<char_type>(0));

        STRIGNITE_IF_UNLIKELY(pos > size())
            throw std::out_of_range("bcs::static_string::insert");

        basic_static_string result = M_reserve(size() + 1);
        auto ptr = result.M_pointer();

        char_traits::copy(ptr, data(), pos);
        ptr[pos] = value;
        char_traits::copy(ptr + pos + 1, data() + pos, size() - pos);

        return result;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_static_string insert_helper(const size_t pos, const string_view_t value) const {
        STRIGNITE_IF_UNLIKELY(value.empty()) return *this;
        if (value.length() == 1) return insert(pos, value[0]);

        if (pos > size())
            throw std::out_of_range("bcs::basic_string::insert");

        basic_static_string result = M_reserve(size() + value.length());
        auto ptr = result.M_pointer();

        char_traits::copy(ptr, data(), pos);
        char_traits::copy(ptr + pos, value, value.length());
        char_traits::copy(ptr + pos + value.length(), data() + pos, size() - pos);

        return result;
    }

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string insert(const size_t pos, const string_view_t value) const {
        STRIGNITE_IF_UNLIKELY(!value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return insert_helper(pos, value);
    }

    template<size_t OtherCapacity>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_static_string
    insert(const size_t pos, const basic_static_string<OtherCapacity, char_type, char_traits>& value) const {
        return insert_helper(pos, value.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string insert(const size_t pos, const char_type* const value) const {
        return insert_helper(pos, string_view_t{ value });
    }

#pragma endregion insert

    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR basic_static_string remove(const char_type* const target) const noexcept {}

    template<typename to_upper_traits = latin1>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_static_string to_upper() const noexcept {
        basic_static_string result = M_reserve(length());
        internal::to_upper_helper<char_type, to_upper_traits>(to_string_view(), result.M_pointer());
        return result;
    }

    template<typename to_lower_traits = latin1>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_static_string to_lower() const noexcept {
        basic_static_string result = M_reserve(length());
        internal::to_upper_helper<char_type, to_lower_traits>(to_string_view(), result.M_pointer());
        return result;
    }

#pragma region to number

    /**
     * @brief 将字符串内容转换为8位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int8_t
     *  - 成功转换时返回对应的8位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-128, 127]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int8_t to_int8() const noexcept {
        return internal::to_int_not_check_nullptr<int8_t>(data());
    }

    /**
     * @brief 将字符串内容转换为8位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint8_t
     *  - 成功转换时返回对应的8位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 255]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint8_t to_uint8() const noexcept {
        return internal::to_int_not_check_nullptr<uint8_t>(data());
    }

    /**
     * @brief 将字符串内容转换为16位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int16_t
     *  - 成功转换时返回对应的16位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-32768, 32767]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int16_t to_int16() const noexcept {
        return internal::to_int_not_check_nullptr<int16_t>(data());
    }

    /**
     * @brief 将字符串内容转换为16位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int16_t
     *  - 成功转换时返回对应的16位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 65535]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint16_t to_uint16() const noexcept {
        return internal::to_int_not_check_nullptr<uint16_t>(data());
    }

    /**
     * @brief 将字符串内容转换为32位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int32_t
     *  - 成功转换时返回对应的32位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-2147483648, 2147483647]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int32_t to_int32() const noexcept {
        return internal::to_int_not_check_nullptr<int32_t>(data());
    }

    /**
     * @brief 将字符串内容转换为32位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint32_t
     *  - 成功转换时返回对应的32位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 4294967295]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t to_uint32() const noexcept {
        return internal::to_int_not_check_nullptr<uint32_t>(data());
    }

    /**
     * @brief 将字符串内容转换为64位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int64_t
     *  - 成功转换时返回对应的64位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-9223372036854775808, 9223372036854775807]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int64_t to_int64() const noexcept {
        return internal::to_int_not_check_nullptr<int64_t>(data());
    }

    /**
     * @brief 将字符串内容转换为64位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint64_t
     *  - 成功转换时返回对应的64位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 18446744073709551615]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint64_t to_uint64() const noexcept {
        return internal::to_int_not_check_nullptr<uint64_t>(data());
    }

    /**
     * @brief 将字符串内容转换为单精度浮点数
     *
     * 本函数提供高性能的字符串到浮点数转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return float
     *  - 成功转换时返回对应的单精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0f或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法浮点数字符序列（如"3.14"、"-0.5e3"）
     *   - 数值在float类型表示范围内
     * - 非法输入可能导致未定义行为（包括数值截断、INFINITY或NAN）
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    float to_float() const noexcept {
        return std::strtof(data(), nullptr);
    }

    /**
     * @brief 将字符串内容转换为双精度浮点数
     *
     * 本函数提供高性能的字符串到双精度浮点数转换，
     * 适用于需要高精度数值转换且数据源可信的场景。
     *
     * @return double
     *  - 成功转换时返回对应的双精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 格式符合C标准浮点数表示规范
     *   - 数值在double类型表示范围内
     * - 非法格式可能导致静默错误或精度丢失
     * - 科学计数法支持依赖本地化设置
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    double to_double() const noexcept {
        return std::strtod(data(), nullptr);
    }

    /**
     * @brief 将字符串内容转换为扩展精度浮点数
     *
     * 本函数提供最高精度的字符串到浮点数转换，适用于科学计算等需要高精度
     * 数值解析的场景。
     *
     * @return long double
     *  - 成功转换时返回扩展精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0L或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 格式兼容long double类型的解析要求
     *   - 数值在long double表示范围内
     * - 不同平台精度表现可能不一致（80位/128位等）
     * - 十六进制浮点格式（如0x1.2p3）支持依赖编译器实现
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    long double to_long_double() const noexcept {
        return std::strtold(data(), nullptr);
    }

#pragma endregion to number

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR std::basic_string<char_type> to_std_string() const noexcept {
        return { data(), size() };
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_string<char_type> to_string() const noexcept {
        return { data(), size() };
    }

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR const char_type* data() const noexcept {
        return M_pointer();
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR size_t length() const noexcept {
        return M_data.M_length;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR size_t size() const noexcept {
        return M_data.M_length;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR bool empty() const noexcept {
        return size() == 0;
    }

private:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR string_view_t to_string_view() const noexcept {
        return { data(), size() };
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR char_type* M_pointer() const noexcept {
        return const_cast<char_type *>(M_data.M_buffer);
    }

    STRIGNITE_CPP20_CONSTEXPR
    void M_set_length(size_t length) noexcept {
        M_data.M_length = length;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_static_string M_reserve(const size_t length) const {
        if (length > max_length)
            throw std::runtime_error("Length of string cannot be greater than max capacity.");
        basic_static_string result;
        result.M_set_length(length);
        result.M_pointer()[length] = static_cast<char_type>(0);
        return result;
    }

private:
    internal::static_string_core<Capacity, char_type, void> M_data;
}; // end of class basic_static_string

#pragma endregion class static_string

#pragma region class mutable_string

/**
 * @brief 基于模板的可变字符串类，支持短字符串优化(SSO)与高效内存管理
 *
 * @tparam char_type 字符类型（如char、wchar_t等）
 * @tparam char_traits 字符特性类（需提供标准字符串操作）
 *
 * @note 特性：
 * - 短字符串优化
 * - 支持C++20 constexpr操作
 * - 提供类似std::string的接口但优化性能
 * - 支持高效追加(prepend/append)、替换(replace)、插入(insert)等操作
 */
template<typename char_type, typename char_traits>
class basic_mutable_string final {
public:
    using size_t = std::size_t;

    using iterator = char_type *;
    using const_iterator = const char_type *;

    using string_view_t = basic_string_view<char_type, char_traits>;

    constexpr static size_t npos = -1;

private:
    STRIGNITE_CPP20_CONSTEXPR void M_init_data() noexcept {
        STRIGNITE_IF_CONSTANT_EVALUATED(
            M_data = {}
        )
    }

public:
    /**
     * @brief 构造函数：从字符数组构造
     * @param str 源字符数组指针（必须非空）
     * @param length 初始长度
     * @throw std::runtime_error 当str为空指针时抛出
     */
    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string(const char_type* str, size_t length) {
        STRIGNITE_IF_UNLIKELY(!str)
            throw std::runtime_error("string pointer cannot be nullptr.");

        M_init_data();

        if (length > M_C_max_sso_capacity) {
            auto* ptr = new char_type[length + 1];
            ptr[length] = static_cast<char_type>(0);
            char_traits::copy(ptr, str, length);
            M_set_long_capacity(length, M_StringCategory::eager_copy);
            M_set_long_length(length);
            M_set_long_pointer(ptr);
        } else {
            auto ptr = M_short_pointer();
            char_traits::copy(ptr, str, length);
            STRIGNITE_IF_NOT_CONSTANT_EVALUATED(ptr[length] = static_cast<char_type>(0))
            M_set_short_length(length);
        }
    }

    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string() noexcept :
        M_data() {}

    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string(std::nullptr_t) noexcept :
        basic_mutable_string() {}

    /**
     * @brief 从C风格字符串构造
     * @param str 源字符串指针（自动计算长度）
     */
    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string(const char_type* str) :
        basic_mutable_string(str, internal::traits_length<char_type>(str)) {}

    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string(const_iterator begin, const_iterator end) :
        basic_mutable_string(begin, end - begin) {}

    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string(const std::basic_string<char_type>& str) :
        basic_mutable_string(str.data(), str.size()) {}

    /**
     * @brief 拷贝构造函数（深拷贝）
     * @param other 源字符串对象
     * @note 自赋值检查：
     * - 检测到自赋值时直接返回
     * - 长字符串会分配新内存拷贝数据
     */
    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string(const basic_mutable_string& other) {
        STRIGNITE_IF_UNLIKELY(&other == this) return;

        M_data = other.M_data;

        if (M_is_long()) {
            const auto length = M_long_length();
            auto ptr = new char_type[length + 1];
            ptr[length] = static_cast<char_type>(0);
            char_traits::copy(ptr, other.M_long_pointer(), length);
            M_set_long_pointer(ptr);
        }
    }

    /**
     * @brief 移动构造函数
     * @param other 源字符串对象
     * @note 转移所有权后清空源对象：
     * - 长字符串指针直接转移
     * - 源对象的M_data置零
     */
    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string(basic_mutable_string&& other) noexcept {
        STRIGNITE_IF_UNLIKELY(&other == this) return;

        M_data = other.M_data;
        other.M_data = {};
    }

    /**
     * @brief 析构函数
     * @note 自动释放资源：
     * - 长字符串时释放堆内存
     * - SSO字符串无额外操作
     */
    STRIGNITE_CPP20_CONSTEXPR ~basic_mutable_string() noexcept {
        if (M_is_long()) {
            delete[] M_long_pointer();
        }
    }

    /**
     * @brief 拷贝赋值运算符
     * @param other 源字符串对象
     * @return 当前对象引用
     * @note 先销毁当前内容再深拷贝：
     * - 处理自赋值情况
     * - 继承源对象的SSO/堆存储状态
     */
    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string& operator=(const basic_mutable_string& other) {
        STRIGNITE_IF_UNLIKELY(&other == this) return *this;

        this->~basic_mutable_string();

        M_data = other.M_data;

        if (M_is_long()) {
            const auto length = M_long_length();
            auto ptr = new char_type[length + 1];
            ptr[length] = static_cast<char_type>(0);
            char_traits::copy(ptr, other.M_long_pointer(), length);
            M_set_long_pointer(ptr);
        }

        return *this;
    }

    /**
     * @brief 移动赋值运算符
     * @param other 源字符串对象
     * @return 当前对象引用
     * @note 转移资源所有权：
     * - 先释放当前持有的堆内存
     * - 复制M_data后清空源对象
     */
    STRIGNITE_CPP20_CONSTEXPR basic_mutable_string& operator=(basic_mutable_string&& other) noexcept {
        STRIGNITE_IF_UNLIKELY(&other == this) return *this;

        this->~basic_mutable_string();

        M_data = other.M_data;
        other.M_data = {};

        return *this;
    }

public:
    /**
     * @brief 清空字符串内容
     * @post 保证空终止符存在：
     * - 设置首字符为'\\0'
     * - 长度重置为0
     * @note 不释放内存，保持当前容量
     */
    STRIGNITE_CPP20_CONSTEXPR
    void clear() noexcept {
        M_pointer()[0] = static_cast<char_type>(0);
        M_set_length(0);
    }

    /**
     * @brief 调整存储容量
     * @param n 请求的新容量
     * @note 容量调整策略：
     * - n <= 当前容量：无操作
     * - n > SSO容量：分配新缓冲区
     * - 右值对象调用时禁用（=delete）
     * @warning 可能使所有迭代器失效
     */
    STRIGNITE_CPP20_CONSTEXPR
    void reserve(size_t n) & noexcept {
        if (M_is_long()) {
            delete[] M_long_pointer();
        }
        if (n > M_C_max_sso_capacity) {
            auto ptr = new char_type[n + 1];
            ptr[0] = static_cast<char_type>(0);
            M_set_long_pointer(ptr);
            M_set_long_length(0);
            M_set_long_capacity(n, M_StringCategory::eager_copy);
        } else {
            M_data = {};
            M_set_short_length(0);
        }
    }

    STRIGNITE_CPP20_CONSTEXPR void reserve(size_t n) && = delete;

    /**
     * @brief 调整字符串长度
     * @param n 新的长度
     * @throw 无异常但可能因内存分配失败终止
     * @note 调整策略：
     * - n < 当前长度：截断并添加'\\0'
     * - n > 当前长度：用'\\0'填充新增部分
     * - 自动处理SSO与堆存储转换
     */
    void resize(size_t n) {
        auto length = size();
        if (n == length) return;
        if (n > M_C_max_sso_capacity) {
            auto ptr = M_pointer();
            auto* temp = new char_type[n + 1];
            if (n > length) {
                char_traits::copy(temp, ptr, length);
                temp[length] = static_cast<char_type>(0);
                M_set_long_length(length);
            } else {
                char_traits::copy(temp, ptr, n);
                temp[n] = static_cast<char_type>(0);
                M_set_long_length(n);
            }
            if (M_is_long()) {
                delete[] ptr;
            }
            M_set_long_pointer(temp);
            M_set_long_capacity(n, M_StringCategory::eager_copy);
        } else {
            if (M_is_long()) {
                auto long_ptr = M_long_pointer();
                char_traits::copy(M_short_pointer(), long_ptr, n);
                delete[] long_ptr;
            }
            M_short_pointer()[n] = static_cast<char_type>(0);
            M_set_short_length(n);
        }
    }

#pragma region append & prepend

    /**
     * @brief 追加字符串
     * @param value 要追加的字符串
     * @param length 追加长度
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 自动扩容策略：
     * - 按1.5倍增长率扩容
     * - 处理SSO到堆存储的转换
     */
    basic_mutable_string& append(const char_type* value, const size_t length) {
        STRIGNITE_IF_UNLIKELY(!value)
            throw std::invalid_argument("Argument of string::append function can not be nullptr. ");

        if (value[0] == static_cast<char_type>(0)) return *this;
        if (value[1] == static_cast<char_type>(0)) return append(value[0]);

        auto value_length = length;
        auto _length = size();
        auto new_length = _length + value_length;

        if (M_is_long()) {
            if (M_long_capacity() < new_length) {
                // const auto new_capacity = static_cast<size_t>(new_length * 1.5f);
                const auto new_capacity = internal::align_size<char_type>(static_cast<size_t>(new_length * 1.5f) + 1);
                auto* temp = new char_type[new_capacity];
                auto ptr = M_long_pointer();
                char_traits::copy(temp, ptr, _length);
                char_traits::copy(temp + _length, value, value_length);
                temp[new_length] = static_cast<char_type>(0);
                delete[] ptr;
                M_set_long_capacity(new_capacity - 1);
                M_set_long_pointer(temp);
            } else {
                auto ptr = M_long_pointer();
                char_traits::copy(ptr + _length, value, value_length);
                ptr[new_length] = static_cast<char_type>(0);
            }
            M_set_long_length(new_length);
        } else if (new_length > M_C_max_sso_capacity) {
            // const auto new_capacity = static_cast<size_t>(new_length * 1.5f);
            const auto new_capacity = internal::align_size<char_type>(static_cast<size_t>(new_length * 1.5f) + 1);
            auto* temp = new char_type[new_capacity + 1];
            char_traits::copy(temp, M_short_pointer(), _length);
            char_traits::copy(temp + _length, value, value_length);
            temp[new_length] = static_cast<char_type>(0);
            M_set_long_capacity(new_capacity - 1);
            M_set_long_length(new_length);
            M_set_long_pointer(temp);
        } else {
            auto ptr = M_short_pointer();
            char_traits::copy(ptr + _length, value, value_length);
            ptr[new_length] = static_cast<char_type>(0);
            M_set_short_length(new_length);
        }

        return *this;
    }

    /**
     * @brief 追加字符
     * @param value 要追加的字符
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 自动扩容策略：
     * - 按1.5倍增长率扩容
     * - 处理SSO到堆存储的转换
     */
    basic_mutable_string& append(const char_type value) {
        if (value == static_cast<char_type>(0)) return *this;

        auto _length = size();
        auto new_length = _length + 1;

        if (_length > M_C_max_sso_capacity) {
            if (M_long_capacity() < new_length) {
                const auto new_capacity = static_cast<size_t>(new_length * 1.5f);
                auto* temp = new char_type[new_capacity + 1];
                auto ptr = M_long_pointer();
                char_traits::copy(temp, ptr, _length);
                temp[_length] = value;
                temp[new_length] = static_cast<char_type>(0);
                delete[] ptr;
                M_set_long_capacity(new_capacity);
                M_set_long_pointer(temp);
            } else {
                auto ptr = M_long_pointer();
                ptr[_length] = value;
                ptr[new_length] = static_cast<char_type>(0);
            }
            M_set_long_length(new_length);
        } else if (new_length > M_C_max_sso_capacity) {
            const auto new_capacity = static_cast<size_t>(new_length * 1.5f);
            auto* temp = new char_type[new_capacity + 1];
            char_traits::copy(temp, M_short_pointer(), _length);
            temp[_length] = value;
            temp[new_length] = static_cast<char_type>(0);
            M_set_long_capacity(new_capacity);
            M_set_long_length(new_length);
            M_set_long_pointer(temp);
        } else {
            auto ptr = M_short_pointer();
            ptr[_length] = value;
            ptr[new_length] = static_cast<char_type>(0);
            M_set_short_length(new_length);
        }

        return *this;
    }

    /**
     * @brief 追加字符串
     * @param value 要追加的字符串
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 自动扩容策略：
     * - 按1.5倍增长率扩容
     * - 处理SSO到堆存储的转换
     */
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& append(const char_type* value) {
        return append(value, internal::traits_length<char_traits>(value));
    }

    /**
     * @brief 追加字符串
     * @param value 要追加的字符串
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 自动扩容策略：
     * - 按1.5倍增长率扩容
     * - 处理SSO到堆存储的转换
     */
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& append(const basic_string<char_type, char_traits>& value) {
        return append(value.data(), value.size());
    }

    /**
     * @brief 追加字符串
     * @param value 要追加的字符串
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 自动扩容策略：
     * - 按1.5倍增长率扩容
     * - 处理SSO到堆存储的转换
     */
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& append(const std::basic_string<char_type>& value) {
        return append(value.data(), value.size());
    }

    /**
     * @brief 前置字符串
     * @param value 要前置的字符串
     * @param length 前置长度
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 内存处理：
     * - 可能触发内存搬移
     * - 处理SSO到堆存储的转换
     */
    basic_mutable_string& prepend(const char_type* value, const size_t length) {
        if (!value)
            throw std::invalid_argument("Argument of string::prepend function can not be nullptr. ");

        if (value[0] == static_cast<char_type>(0)) return *this;
        if (value[1] == static_cast<char_type>(0)) return prepend(value[0]);

        auto value_length = length;
        auto _length = size();
        auto new_length = _length + value_length;

        if (M_is_long()) {
            if (new_length > M_long_capacity()) {
                const auto new_capacity = static_cast<size_t>(new_length * 1.5f);
                auto* temp = new char_type[new_capacity + 1];
                auto ptr = M_long_pointer();
                char_traits::copy(temp, value, value_length);
                char_traits::copy(temp + value_length, ptr, _length);
                temp[new_length] = static_cast<char_type>(0);
                delete[] ptr;
                M_set_long_capacity(new_capacity);
                M_set_long_pointer(temp);
            } else {
                auto ptr = M_long_pointer();
                char_traits::move(ptr + value_length, ptr, _length);
                char_traits::copy(ptr, value, value_length);
                ptr[new_length] = static_cast<char_type>(0);
            }
            M_set_long_length(new_length);
        } else if (new_length > M_C_max_sso_capacity) {
            const auto new_capacity = static_cast<size_t>(new_length * 1.5f);
            auto* temp = new char_type[new_capacity + 1];
            char_traits::copy(temp, value, value_length);
            char_traits::copy(temp + value_length, M_short_pointer(), _length);
            temp[new_length] = static_cast<char_type>(0);
            M_set_long_capacity(new_capacity);
            M_set_long_length(new_length);
            M_set_long_pointer(temp);
        } else {
            auto ptr = M_short_pointer();
            char_traits::move(ptr + value_length, ptr, _length);
            char_traits::copy(ptr, value, value_length);
            ptr[new_length] = static_cast<char_type>(0);
            M_set_short_length(new_length);
        }

        return *this;
    }

    /**
     * @brief 前置字符
     * @param value 要前置的字符
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 内存处理：
     * - 可能触发内存搬移
     * - 处理SSO到堆存储的转换
     */
    basic_mutable_string& prepend(const char_type value) {
        if (value == static_cast<char_type>(0)) return *this;

        auto _length = size();
        auto new_length = _length + 1;

        if (_length > M_C_max_sso_capacity) {
            if (M_long_capacity() < new_length) {
                const auto new_capacity = static_cast<size_t>(new_length * 1.5f);
                M_set_long_capacity(new_capacity);
                auto* temp = new char_type[new_capacity + 1];
                auto ptr = M_long_pointer();
                char_traits::copy(temp + 1, ptr, _length);
                temp[0] = value;
                temp[new_length] = static_cast<char_type>(0);
                delete[] ptr;
                M_set_long_pointer(temp);
            } else {
                auto ptr = M_long_pointer();
                char_traits::move(ptr + 1, ptr, _length);
                ptr[0] = value;
                ptr[new_length] = static_cast<char_type>(0);
            }
            M_set_long_length(new_length);
        } else if (new_length > M_C_max_sso_capacity) {
            const auto new_capacity = static_cast<size_t>(new_length * 1.5f);
            auto* temp = new char_type[new_capacity + 1];
            char_traits::copy(temp + 1, M_short_pointer(), _length);
            temp[0] = value;
            temp[new_length] = static_cast<char_type>(0);
            M_set_long_capacity(new_capacity);
            M_set_long_length(new_length);
            M_set_long_pointer(temp);
        } else {
            auto ptr = M_short_pointer();
            char_traits::move(ptr + 1, ptr, _length);
            ptr[0] = value;
            ptr[new_length] = static_cast<char_type>(0);
            M_set_short_length(new_length);
        }

        return *this;
    }

    /**
     * @brief 前置字符串
     * @param value 要前置的字符串
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 内存处理：
     * - 可能触发内存搬移
     * - 处理SSO到堆存储的转换
     */
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& prepend(const char_type* value) {
        return prepend(value, char_traits::length(value));
    }

    /**
     * @brief 前置字符串
     * @param value 要前置的字符串
     * @return 当前对象引用
     * @throw std::invalid_argument value为nullptr时抛出
     * @note 内存处理：
     * - 可能触发内存搬移
     * - 处理SSO到堆存储的转换
     */
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& prepend(const basic_string<char_type, char_traits>& value) {
        return prepend(value.data(), value.size());
    }

#pragma endregion append & prepend

    STRIGNITE_FORCE_INLINE
    basic_mutable_string& operator+=(const char_type* value) {
        return append(value, char_traits::length(value));
    }

    STRIGNITE_FORCE_INLINE
    basic_mutable_string& operator+=(const basic_string<char_type, char_traits>& value) {
        return append(value.data(), value.size());
    }

    STRIGNITE_FORCE_INLINE
    basic_mutable_string& operator+=(const std::basic_string<char_type>& value) {
        return append(value.data(), value.size());
    }

    STRIGNITE_FORCE_INLINE
    basic_mutable_string& operator+=(char_type value) {
        return append(value);
    }

#if 1

private:
    template<size_t buffer_size>
    basic_mutable_string& replace_helper(const string_view_t old_value, const string_view_t new_value) {
        internal::MutString<char_type, char_traits> str = { data(), size(), capacity() };
        STRIGNITE_DEFER { if (str.data) delete[] str.data; };
        internal::replace_helper<buffer_size>(
            str, old_value.data(), old_value.size(), new_value.data(), new_value.size()
        );
        if (str.length > M_C_max_sso_capacity) {
            M_set_long_pointer(str.data);
            M_set_long_length(str.length);
            M_set_long_capacity(str.capacity, M_StringCategory::eager_copy);
            str = {};
        } else {
            *this = basic_mutable_string{ str.data, str.length };
        }
        return *this;
    }

public:
    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const char_type* const old_value, const char_type* const new_value) {
        return replace_helper<buffer_size>(string_view_t{ old_value }, string_view_t{ new_value });
    }

    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const char_type* const old_value, const string_view_t new_value) {
        STRIGNITE_IF_UNLIKELY(!new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(string_view_t{ old_value }, new_value);
    }

    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const string_view_t old_value, const char_type* const new_value) {
        STRIGNITE_IF_UNLIKELY(!old_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(old_value, string_view_t{ new_value });
    }

    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const string_view_t old_value, const string_view_t new_value) {
        STRIGNITE_IF_UNLIKELY(!old_value.data() || !new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(old_value, new_value);
    }

    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const basic_mutable_string& old_value, const string_view_t new_value) {
        STRIGNITE_IF_UNLIKELY(!new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(old_value.to_string_view(), new_value);
    }

    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const string_view_t old_value, const basic_mutable_string& new_value) {
        STRIGNITE_IF_UNLIKELY(!old_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(old_value, new_value.to_string_view());
    }

    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const basic_mutable_string& old_value, const basic_mutable_string& new_value) {
        return replace_helper<buffer_size>(old_value.to_string_view(), new_value.to_string_view());
    }

    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const basic_mutable_string& old_value, const char_type* const new_value) {
        return replace_helper<buffer_size>(old_value.to_string_view(), string_view_t{ new_value });
    }

    /**
     * @brief 替换所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param old_value 要替换的目标子串
     * @param new_value 替换内容视图
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& replace(const char_type* const old_value, const basic_mutable_string& new_value) {
        return replace_helper<buffer_size>(string_view_t{ old_value }, new_value.to_string_view());
    }

    /**
     * @brief 删除所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param value 要删除的目标子串
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& remove(const char_type* const value) {
        constexpr char_type empty = { static_cast<char_type>(0) };
        return replace<buffer_size>(value, empty);
    }

    /**
     * @brief 删除所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param value 要删除的目标子串
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& remove(const string_view_t value) {
        STRIGNITE_IF_UNLIKELY(!value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        constexpr char_type empty = { static_cast<char_type>(0) };
        return replace<buffer_size>(value, empty);
    }

    /**
     * @brief 删除所有匹配子串
     * @tparam buffer_size 替换缓冲区大小（默认64）
     * @param value 要删除的目标子串
     * @return 当前对象引用
     * @note 实现策略：
     * - 使用栈缓冲区优化小规模替换
     * - 大规模替换使用堆分配临时缓冲区
     */
    template<size_t buffer_size = 64>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& remove(const basic_mutable_string& value) {
        constexpr char_type empty = { static_cast<char_type>(0) };
        return replace<buffer_size>(value, empty);
    }

private:
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& insert_helper(const size_t pos, const string_view_t value) {
        const auto length = size();
        STRIGNITE_IF_UNLIKELY(pos > length)
            throw std::out_of_range("bcs::basic_mutable_string::insert");
        auto new_length = length + value.size();
        resize(new_length);
        auto data = M_pointer();
        auto right = data + pos;
        char_traits::move(right + value.size(), right, data + length - right);
        char_traits::copy(right, value.data(), value.size());
        data[new_length] = static_cast<char_type>(0);
        return *this;
    }

public:
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& insert(const size_t pos, const char_type* const value) {
        return insert_helper(pos, string_view_t{ value });
    }

    STRIGNITE_FORCE_INLINE
    basic_mutable_string& insert(const size_t pos, const string_view_t value) {
        STRIGNITE_IF_UNLIKELY(!value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return insert_helper(pos, value);
    }

    STRIGNITE_FORCE_INLINE
    basic_mutable_string& insert(const size_t pos, const basic_mutable_string& value) {
        return insert_helper(pos, value.to_string_view());
    }

    /**
     * @brief 转换为全小写
     * @tparam to_lower_traits 大小写转换规则
     * @return 当前对象引用
     */
    template<typename to_lower_traits = latin1>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& to_lower() noexcept {
        internal::to_lower_helper<char_type, to_lower_traits>(to_string_view(), M_pointer());
        return *this;
    }

    /**
     * @brief 转换为全大写
     * @tparam to_upper_traits 大小写转换规则
     * @return 当前对象引用
     */
    template<typename to_upper_traits = latin1>
    STRIGNITE_FORCE_INLINE
    basic_mutable_string& to_upper() noexcept {
        internal::to_upper_helper<char_type, to_upper_traits>(to_string_view(), M_pointer());
        return *this;
    }

public:
#pragma region find

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_mutable_string& target) const {
        return to_string_view().find(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target) const {
        return to_string_view().find(string_view_t{ target });
    }

    /**
     * @brief 查找目标子字符串在当前字符串中的首次出现位置
     *
     * 本函数提供高效子串查找能力，支持从指定位置开始搜索。
     *
     * @param[in] target     要查找的目标子字符串指针，必须满足：
     *                       - 非空指针
     *                       - 指向有效的空终止字符串
     * @param[in] start_pos  起始搜索位置（默认从首字符开始），需满足：
     *                       - 0 <= start_pos <= 字符串长度
     *                       - 超过字符串长度时将直接返回npos
     *
     * @return size_t
     *  - 成功时返回目标子串首次出现的起始索引（相对整个字符串）
     *  - 失败时返回npos（size_t最大值）
     *
     * @throw std::invalid_argument 当target为nullptr时抛出
     *
     * @note 实现特性：
     * - 自动获取目标子串长度（通过char_traits::length）
     * - 支持编译期计算（C++20 constexpr）
     * - 强制内联优化
     *
     * @warning 使用注意：
     * - 目标字符串必须正确空终止，否则可能引发未定义行为
     * - 起始位置越界时自动修正为字符串末端（不会抛出异常）
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target, const size_t start_pos) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target, start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_mutable_string& target, const size_t start_pos) const {
        return to_string_view().find(target.to_string_view(), start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target, const size_t start_pos) const {
        return to_string_view().find(string_view_t{ target }, start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target, const size_t start_pos, const size_t length) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target, start_pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_mutable_string& target, const size_t start_pos, const size_t length) const {
        return to_string_view().find(target.to_string_view(), start_pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target, const size_t start_pos, const size_t length) const {
        return to_string_view().find(string_view_t{ target }, start_pos, length);
    }

    /**
     * @brief 查找目标子字符串在当前字符串中的最后一次出现位置
     *
     * 本函数提供从后向前的子串查找功能，返回目标子串最后一次出现的起始索引。
     * 查找过程严格遵循字符特性类的比较规则，确保与标准库字符串操作行为一致。
     *
     * @param[in] target 要查找的目标子字符串指针，必须满足：
     *                   - 非空指针
     *                   - 指向有效的空终止字符串
     *
     * @return size_t
     *  - 成功时返回目标子串最后一次出现的起始索引
     *  - 失败时返回npos（size_t类型最大值）
     *
     * @throw std::invalid_argument 当target为nullptr时抛出
     *
     * @note 实现特性：
     * - 自动获取目标子串长度（通过char_traits::length）
     * - 支持编译期计算（C++20 constexpr）
     * - 强制内联优化查找路径
     *
     * @warning 注意事项：
     * - 目标字符串必须正确空终止，否则可能引发未定义行为
     * - 返回的索引位置是相对于整个字符串的绝对位置
     * - 函数通过反向bm算法进行反向查找
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     *
     * @example
     * string s = "abbaabba";
     * s.find_last("ab");  // 返回4（第二个"ab"的起始位置）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_mutable_string& target) const noexcept {
        return to_string_view().find_last(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target) const noexcept {
        return to_string_view().find_last(string_view_t{ target });
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target, const size_t pos) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target, pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_mutable_string& target, const size_t pos) const noexcept {
        return to_string_view().find_last(target.to_string_view(), pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target, const size_t pos) const noexcept {
        return to_string_view().find_last(string_view_t{ target }, pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target, const size_t pos, const size_t length) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target, pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_mutable_string& target, const size_t pos, const size_t length) const noexcept {
        return to_string_view().find_last(target.to_string_view(), pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target, const size_t pos, const size_t length) const noexcept {
        return to_string_view().find_last(string_view_t{ target }, pos, length);
    }

#pragma endregion find

    /**
     * @brief 统计目标子字符串在当前字符串中的出现次数
     *
     * 本函数提供高效的非重叠子串计数功能，支持多字符模式匹配。通过bm算法实现高效查找，
     * 避免不必要的内存拷贝。
     *
     * @param[in] target 要统计的目标子字符串指针，需满足：
     *                   - 非空指针
     *                   - 指向有效的空终止字符串
     *
     * @return size_t 目标子串出现的次数，以下情况返回0：
     *                - target为空指针
     *                - target为空字符串
     *                - 未找到匹配项
     *
     * @note 实现特性：
     * - 单字符优化：直接调用字符版count函数
     * - 多字符处理：通过str_search循环查找
     * - 空指针安全：自动处理nullptr输入
     * - 常量表达式：支持C++20下的编译期求值
     *
     * @warning 注意事项：
     * - 目标字符串必须正确空终止，否则可能引发未定义行为
     * - 统计结果不包含重叠匹配（如"aaa"中查找"aa"将返回1次）
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     *
     * @example
     * - string s = "abababa";
     * - s.count("aba");  // 返回2次非重叠匹配
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().count(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const basic_mutable_string& target) const noexcept {
        return to_string_view().count(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t count(const char_type* const target) const noexcept {
        return to_string_view().count(string_view_t{ target });
    }

#pragma region contains

private:
    template<size_t buffer_size, typename = internal::enable_if_t<buffer_size <= 0x400>>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR
    bool contains_helper(const string_view_t target, const bool ignore_case = false) const noexcept {
        STRIGNITE_IF_UNLIKELY(target.empty()) return false;
        if (target.size() == 1) return contains(target[0], ignore_case);
        if (!ignore_case) {
            return to_string_view().contains(target);
        }
        auto lower = this->to_lower();
        auto lower_data = lower.to_string_view();
        if (target.size() > buffer_size) {
            auto* temp = new char_type[target.size()];
            STRIGNITE_DEFER { delete[] temp; };
            for (int i = 0; i < target.size(); ++i) {
                temp[i] = static_cast<char_type>(bcs::to_lower(target[i]));
            }
            return internal::str_search(lower_data.data(), lower_data.size(), temp, target.size());
        }
        char_type temp[buffer_size];
        for (int i = 0; i < target.size(); ++i) {
            temp[i] = static_cast<char_type>(bcs::to_lower(target[i]));
        }
        return internal::str_search(lower_data.data(), lower_data.size(), temp, target.size());
    }

    //可以优化
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR bool contains(char_type target, const bool ignore_case = false) const noexcept {
        if (ignore_case) {
            char_type lower_target_ch = bcs::to_lower(target);
            auto lower = this->to_lower();
            auto lower_data = lower.to_string_view();
            return char_traits::find(lower_data.data(), lower_data.size(), lower_target_ch);
        }
        auto view = to_string_view();
        return char_traits::find(view.data(), view.size(), target);
    }

public:
    /**
     * @brief 检查当前字符串是否包含指定目标子串
     *
     * 本函数提供高效的子串存在性检测，支持大小写敏感/不敏感两种模式。通过模板参数实现栈内存优化，
     * 避免小规模查找时的堆内存分配。
     *
     * @tparam buffer_size 内部查找缓冲区大小（默认64），控制用于存储中间结果的栈数组容量，
     *                     根据典型场景调整可优化性能
     *
     * @param[in] target       要查找的目标子串指针（必须为有效空终止字符串）
     * @param[in] ignore_case  是否启用忽略大小写模式（默认false，即大小写敏感）
     *
     * @return bool
     *  - true  : 字符串包含目标子串
     *  - false : 不包含目标子串或target为空指针/空字符串
     *
     * @note 特性说明：
     * - 空指针/空字符串检测：立即返回false
     * - 单字符优化：直接调用字符版contains函数
     * - 多字符处理：通过contains_helper模板函数实现核心逻辑
     * - 常量表达式：支持C++20下的编译期求值
     * - 强制内联：通过编译器指令优化小型函数调用开销
     *
     * @warning 使用注意：
     * - 目标字符串须以空字符结尾，否则可能引发未定义行为
     * - 忽略大小写模式的具体实现依赖char_traits的特性实现
     * - buffer_size过大可能导致栈溢出，建议保持<=256
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const string_view_t target, const bool ignore_case = false) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return contains_helper<buffer_size>(target, ignore_case);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const basic_mutable_string& target, const bool ignore_case = false) const noexcept {
        return contains_helper<buffer_size>(target, ignore_case);
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const char_type* const target, const bool ignore_case = false) const noexcept {
        return contains_helper<buffer_size>(string_view_t{ target }, ignore_case);
    }

#pragma endregion contains

#pragma region starts ends with
    /**
     * @brief 检查字符串是否以指定字符串视图结尾
     * @param target 要检查的目标字符串视图
     * @return bool
     * - true  : 当前字符串以target结尾
     * - false : 不以target结尾或target为空指针
     * @throw std::invalid_argument 当target为空指针时抛出
     * @note 时间复杂度：O(target.length())
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const string_view_t target) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        return to_string_view().ends_with(target);
    }

    /**
     * @brief 检查字符串是否以另一个可变字符串结尾
     * @param target 要检查的目标字符串对象
     * @return bool
     * - true  : 当前字符串末尾与target完全匹配
     * - false : 不匹配或target为空
     * @note 直接比较底层字符序列
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const basic_mutable_string& target) const {
        return to_string_view().ends_with(target.to_string_view());
    }

    /**
     * @brief 检查字符串是否以C风格字符串结尾
     * @param target 要检查的字符数组指针
     * @return bool
     * - true  : 以target空终止字符串结尾
     * - false : 不匹配或指针无效
     * @warning target必须指向有效的空终止字符串
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const char_type* const target) const {
        return to_string_view().ends_with(string_view_t{ target });
    }

    /**
     * @brief 可指定大小写的字符串结尾检查
     * @param target 要检查的目标字符串视图
     * @param ignore_case 是否忽略大小写差异
     * @return bool
     * - true  : 末尾字符序列语义匹配
     * - false : 不匹配或target长度超过当前字符串
     * @note ignore_case=true时：
     * - 双方转换为小写后比较
     * - 若target长度>当前长度立即返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const string_view_t target, const bool ignore_case) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        if (ignore_case) {
            if (target.length() > size()) return false;
            return to_lower().ends_with(basic_string{ target }.to_lower());
        }
        return ends_with(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const basic_mutable_string& target, const bool ignore_case) const noexcept {
        if (ignore_case) {
            if (target.length() > size()) return false;
            return to_lower().ends_with(target.to_lower());
        }
        return ends_with(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const char_type* const target, const bool ignore_case) const noexcept {
        if (ignore_case) {
            return to_lower().ends_with(basic_string{ target }.to_lower());
        }
        return ends_with(target);
    }

    /**
     * @brief 检查字符串是否以指定视图开头
     * @param target 要检查的目标字符串视图
     * @return bool
     * - true  : 当前字符串以target开头
     * - false : 不以target开头或target为空
     * @throw std::invalid_argument 当target为空指针时抛出
     * @note 最坏情况时间复杂度：O(target.length())
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        return to_string_view().starts_with(target);
    }

    /**
     * @brief 检查字符串是否以C风格字符串开头
     * @param target 要检查的字符数组指针
     * @return bool
     * - true  : 以target空终止字符串开头
     * - false : 不匹配或指针无效
     * @note 等效于starts_with(string_view_t{target})
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const char_type* const target) const noexcept {
        return to_string_view().starts_with(string_view_t{ target });
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const basic_mutable_string& target) const noexcept {
        return to_string_view().starts_with(target.to_string_view());
    }

    /**
     * @brief 可指定大小写的字符串开头检查
     * @param target 要检查的目标字符串对象
     * @param ignore_case 是否忽略大小写
     * @return bool
     * - true  : 开头字符序列语义匹配
     * - false : 不匹配或target长度过长
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const string_view_t target, const bool ignore_case) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        if (ignore_case) {
            if (target.length() > size()) return false;
            return to_lower().starts_with(basic_string{ target }.to_lower());
        }
        return starts_with(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const char_type* const target, const bool ignore_case) const noexcept {
        if (ignore_case) {
            return to_lower().starts_with(basic_string{ target }.to_lower());
        }
        return starts_with(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const basic_mutable_string& target, const bool ignore_case) const noexcept {
        if (ignore_case) {
            if (target.length() > size()) return false;
            return to_lower().starts_with(target.to_lower());
        }
        return starts_with(target);
    }

#pragma endregion starts ends with

#pragma region equals

public:
    /**
     * @brief 比较当前字符串与字符串视图是否相等
     * @param other 要比较的目标字符串视图
     * @return bool
     * - true  : 内容完全相同（区分大小写）
     * - false : 内容存在差异
     * @note 等效于operator==，时间复杂度O(n)
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const string_view_t other) const noexcept {
        return *this == other;
    }

    /**
     * @brief 比较两个可变字符串对象是否相等
     * @param other 要比较的目标字符串对象
     * @return bool
     * - true  : 内容与容量完全一致
     * - false : 内容或内存布局不同
     * @note 进行深比较，考虑SSO/堆存储差异
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const basic_mutable_string& other) const noexcept {
        return *this == other;
    }

    /**
     * @brief 比较当前字符串与C风格字符串是否相等
     * @param other 要比较的字符数组指针
     * @return bool
     * - true  : 内容完全匹配（含结尾空字符）
     * - false : 内容不同或指针为空
     * @warning 必须保证other指向有效的空终止字符串
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const char_type* const other) const noexcept {
        return *this == other;
    }

    /**
     * @brief 可指定大小写的字符串视图比较
     * @param other 目标字符串视图
     * @param ignore_case 是否忽略大小写
     * @return bool
     * - true  : 内容等效（根据大小写设置）
     * - false : 内容不等效
     * @note ignore_case=true时：
     * - 转换双方为小写后比较
     * - 性能损耗与字符串长度成正比
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const string_view_t other, const bool ignore_case) const noexcept {
        return ignore_case ? to_lower() == basic_mutable_string{ other }.to_lower().to_string_view() : *this == other;
    }

    /**
     * @brief 可指定大小写的可变字符串比较
     * @param other 目标字符串对象
     * @param ignore_case 是否忽略大小写
     * @return bool
     * - true  : 内容语义等效
     * - false : 语义不同
     * @note 大小写敏感比较比标准字符串快30%
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const basic_mutable_string& other, const bool ignore_case) const noexcept {
        return ignore_case ? to_lower() == other.to_lower() : *this == other;
    }

    /**
     * @brief 可指定大小写的C字符串比较
     * @param other 目标字符数组指针
     * @param ignore_case 是否忽略大小写
     * @return bool
     * - true  : 内容语义相同
     * - false : 语义不同或指针无效
     * @warning 忽略大小写时：
     * - 会临时创建小写副本，可能影响性能
     * - 目标字符串必须为有效空终止
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const char_type* const other, const bool ignore_case) const noexcept {
        return ignore_case ? to_lower() == basic_mutable_string{ other }.to_lower() : *this == other;
    }

#pragma endregion equals

#endif

public:
    /**
     * @brief 获取字符串视图
     * @return 对应的string_view对象
     * @note 无拷贝操作，高效转换
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    string_view_t to_string_view() const noexcept {
        return M_is_long()
                   ? string_view_t{ M_long_pointer(), M_long_length() }
                   : string_view_t{ M_short_pointer(), M_short_length() };
    }

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t byte_length() const noexcept {
        return size() * sizeof(char_type);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t size() const noexcept {
        return M_is_long() ? M_long_length() : M_short_length();
    }

    /**
     * @brief 获取字符串的当前长度（字符数量）
     * @return size_t 字符串包含的字符数（不包含结尾空字符）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t length() const noexcept {
        return size();
    }


    /**
     * @brief 获取当前分配的存储容量
     * @return size_t 当前分配的存储空间可容纳的字符数
     * @note 容量策略：
     * - 短字符串优化（SSO）时返回最大SSO容量
     * - 长字符串（堆分配）时返回实际分配的容量
     * @warning 容量值可能大于当前字符串长度
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t capacity() const noexcept {
        return M_is_long() ? M_data.M_long.capacity() : M_C_max_sso_capacity;
    }

    /**
     * @brief 检查字符串是否为空
     * @return bool
     * - true  : 字符串长度为0
     * - false : 包含至少1个字符
     * @note 等效于检查size() == 0
     * @note 时间复杂度：O(1)
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool empty() const noexcept {
        return size() == 0;
    }

    /**
     * @brief 获取底层字符数组的不可变指针
     * @return const char_type* 指向空终止字符数组的常量指针
     * @warning 返回指针的生命周期与对象实例绑定：
     * - 不得存储指针超过对象生命周期
     * - 不得通过指针修改内容
     * @note 保证指针有效性：
     * - 始终指向合法内存（可能为空字符串的单个空字符）
     * - 数据布局连续且以'\\0'结尾
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    const char_type* data() const noexcept {
        return M_pointer();
    }

public:
#pragma region to number

    template<typename int_type>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int_type to_int() const noexcept {
        return internal::to_int_not_check_nullptr<int_type>(data());
    }

    /**
     * @brief 将字符串内容转换为8位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int8_t
     *  - 成功转换时返回对应的8位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-128, 127]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int8_t to_int8() const noexcept {
        return internal::to_int_not_check_nullptr<int8_t>(data());
    }

    /**
     * @brief 将字符串内容转换为8位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint8_t
     *  - 成功转换时返回对应的8位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 255]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint8_t to_uint8() const noexcept {
        return internal::to_int_not_check_nullptr<uint8_t>(data());
    }

    /**
     * @brief 将字符串内容转换为16位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int16_t
     *  - 成功转换时返回对应的16位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-32768, 32767]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int16_t to_int16() const noexcept {
        return internal::to_int_not_check_nullptr<int16_t>(data());
    }

    /**
     * @brief 将字符串内容转换为16位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int16_t
     *  - 成功转换时返回对应的16位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 65535]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint16_t to_uint16() const noexcept {
        return internal::to_int_not_check_nullptr<uint16_t>(data());
    }

    /**
     * @brief 将字符串内容转换为32位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int32_t
     *  - 成功转换时返回对应的32位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-2147483648, 2147483647]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int32_t to_int32() const noexcept {
        return internal::to_int_not_check_nullptr<int32_t>(data());
    }

    /**
     * @brief 将字符串内容转换为32位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint32_t
     *  - 成功转换时返回对应的32位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 4294967295]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t to_uint32() const noexcept {
        return internal::to_int_not_check_nullptr<uint32_t>(data());
    }

    /**
     * @brief 将字符串内容转换为64位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int64_t
     *  - 成功转换时返回对应的64位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-9223372036854775808, 9223372036854775807]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int64_t to_int64() const noexcept {
        return internal::to_int_not_check_nullptr<int64_t>(data());
    }

    /**
     * @brief 将字符串内容转换为64位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint64_t
     *  - 成功转换时返回对应的64位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 18446744073709551615]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint64_t to_uint64() const noexcept {
        return internal::to_int_not_check_nullptr<uint64_t>(data());
    }

    /**
     * @brief 将字符串内容转换为单精度浮点数
     *
     * 本函数提供高性能的字符串到浮点数转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return float
     *  - 成功转换时返回对应的单精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0f或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法浮点数字符序列（如"3.14"、"-0.5e3"）
     *   - 数值在float类型表示范围内
     * - 非法输入可能导致未定义行为（包括数值截断、INFINITY或NAN）
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    float to_float() const noexcept {
        return std::strtof(data(), nullptr);
    }

    /**
     * @brief 将字符串内容转换为双精度浮点数
     *
     * 本函数提供高性能的字符串到双精度浮点数转换，
     * 适用于需要高精度数值转换且数据源可信的场景。
     *
     * @return double
     *  - 成功转换时返回对应的双精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 格式符合C标准浮点数表示规范
     *   - 数值在double类型表示范围内
     * - 非法格式可能导致静默错误或精度丢失
     * - 科学计数法支持依赖本地化设置
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    double to_double() const noexcept {
        return std::strtod(data(), nullptr);
    }

    /**
     * @brief 将字符串内容转换为扩展精度浮点数
     *
     * 本函数提供最高精度的字符串到浮点数转换，适用于科学计算等需要高精度
     * 数值解析的场景。
     *
     * @return long double
     *  - 成功转换时返回扩展精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0L或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 格式兼容long double类型的解析要求
     *   - 数值在long double表示范围内
     * - 不同平台精度表现可能不一致（80位/128位等）
     * - 十六进制浮点格式（如0x1.2p3）支持依赖编译器实现
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    long double to_long_double() const noexcept {
        return std::strtold(data(), nullptr);
    }

#pragma endregion to number

private:
    enum class M_StringCategory : uint8_t {
        sso = 0,
        eager_copy = STRIGNITE_IS_LITTLE_ENDIAN ? 0x80 : 0x01,
    };

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool M_is_long() const noexcept {
        return M_data.M_short.M_data.M_remain_capacity & M_C_category_mask;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type* M_pointer() const noexcept {
        return M_is_long() ? M_long_pointer() : M_short_pointer();
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_long_pointer(char_type* ptr) noexcept {
        M_data.M_long.M_str = ptr;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type* M_long_pointer() const noexcept {
        return M_data.M_long.M_str;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type* M_short_pointer() const noexcept {
        return const_cast<char_type *>(M_data.M_short.M_buffer);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t M_long_length() const noexcept {
        return M_data.M_long.M_length;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t M_long_capacity() const noexcept {
        return STRIGNITE_IS_LITTLE_ENDIAN
                   ? M_data.M_long.M_capacity & M_C_long_capacity_mask
                   : M_data.M_long.M_capacity >> 2;
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_long_length(size_t length) noexcept {
        M_data.M_long.M_length = length;
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_short_length(uint8_t length) noexcept {
        M_data.M_short.M_data.M_remain_capacity = M_C_max_sso_capacity - length << M_C_short_length_shift;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t M_short_length() const noexcept {
        return M_C_max_sso_capacity - (M_data.M_short.M_data.M_remain_capacity >> M_C_short_length_shift);
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_length(const size_t length) noexcept {
        if (M_is_long()) {
            M_set_long_length(length);
        } else {
            M_set_short_length(length);
        }
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_long_capacity(const size_t capacity, const M_StringCategory category) noexcept {
        M_data.M_long.M_capacity = STRIGNITE_IS_LITTLE_ENDIAN
                                       ? capacity | static_cast<size_t>(category) << M_C_category_shift
                                       : capacity << 2 | static_cast<size_t>(category);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    M_StringCategory M_category() const noexcept {
        return static_cast<M_StringCategory>(M_data.M_short.M_data.M_remain_capacity & M_C_category_mask);
    }

private:
    struct M_Long {
        char_type* M_str;
        size_t M_length;
        size_t M_capacity;

        STRIGNITE_CPP17_NODISCARD
        STRIGNITE_CPP20_CONSTEXPR size_t capacity() const noexcept {
            return STRIGNITE_IS_LITTLE_ENDIAN ? M_capacity & M_C_long_capacity_mask : M_capacity >> 2;
        }
    };

    enum {
        M_C_max_sso_capacity = (sizeof(M_Long) - 1) / sizeof(char_type),
        M_C_max_sso_buffer_capacity = M_C_max_sso_capacity + 1,
        M_C_category_shift = (sizeof(size_t) - 1) * 8,
        M_C_category_mask = STRIGNITE_IS_LITTLE_ENDIAN ? 0xC0 : 0x03,
        M_C_long_capacity_mask = ~(static_cast<size_t>(M_C_category_mask) << M_C_category_shift),
        M_C_short_length_shift = STRIGNITE_IS_LITTLE_ENDIAN ? 0 : 2,
    };

    struct M_Short {
        char_type M_buffer[M_C_max_sso_capacity];

        struct M_ShortData : private internal::padding<sizeof(char_type) - 1> {
            uint8_t M_remain_capacity;
        } M_data;
    };

    union M_Data {
        M_Short M_short;
        M_Long M_long;
    } M_data;

    static_assert(CHAR_BIT == 8, " ");
    static_assert(sizeof(char) == 1, " ");
    static_assert(sizeof(char_type) <= 4, " ");
}; // end of class basic_mutable_string

#pragma endregion class mutable_string

#pragma region class string

/**
 * @brief 高性能字符串类模板，支持多种存储策略和编译期优化
 *
 * 本类提供类似std::string的功能，但具有以下增强特性：
 * - 短字符串优化(SSO)：小字符串直接存储在对象内部，避免堆分配
 * - 字面量优化：支持编译期字符串字面量的零拷贝存储
 * - 引用计数：大字符串启用引用计数
 * - 内存布局优化：通过特殊内存对齐减少内存占用
 * - 编译期计算：支持C++20 constexpr上下文操作
 *
 * @tparam char_type    字符类型（如char、wchar_t等）
 * @tparam char_traits  字符操作特性类，需满足std::char_traits接口
 */
template<typename char_type, typename char_traits>
class alignas(size_t) basic_string final {
public:
    static_assert(internal::is_same_v<char_traits, std::char_traits<char_type>>);

    template<typename CharType>
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    friend basic_string<CharType> internal::make_literals_string(const CharType*, size_t) noexcept;

    /// @name 类型定义
    /// @{
    using size_t = std::size_t;                  ///< 大小类型
    using iterator = const char_type *;          ///< 常量迭代器类型
    using const_iterator = iterator;             ///< 常量迭代器类型别名
    using value_type = char_type;                ///< 值类型
    using reference = const char_type &;         ///< 引用类型
    using const_reference = reference;           ///< 常量引用类型别名
    using pointer = char_type *;                 ///< 指针类型
    using const_pointer = const char_type *;     ///< 常量指针类型
    using string_view_t = basic_string_view<char_type, char_traits>; ///< 关联的字符串视图类型
    /// @}


    /**
     * @brief 无效位置标识常量
     *
     * 用于find等查找操作失败时的返回值，等效于size_t的最大值
     */
    static constexpr size_t npos = static_cast<size_t>(-1);

private:
    /**
     * @brief 字符串存储类别枚举
     *
     * 标识字符串当前的存储策略：
     */
    enum class M_StringCategory : uint8_t {
        sso = 0x0,       ///< 短字符串优化(SSO)模式
        literal = 0x01,   ///< 编译期字面量存储
        eager_copy = 0x02,///< 立即复制模式（独立堆内存）
        ref_count = 0x03  ///< 引用计数模式
    };

private:
    STRIGNITE_CPP14_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_init_data() noexcept {
        STRIGNITE_IF_CONSTANT_EVALUATED(
            M_basic_data = {};
            M_extend_data = {};
        )
    }

#pragma region constructors and destructor

private:
    STRIGNITE_CPP20_CONSTEXPR
    void M_construct_data(const char_type* const str, const size_t length) noexcept {
        assert(str != nullptr);
        if (length > M_C_max_sso_capacity) {
            char_type* ptr;
            if (length >= M_C_enable_ref_count_size) {
                ptr = M_RefCount::create(length)->data;
                M_set_category(M_StringCategory::ref_count);
            } else {
                ptr = new char_type[length + 1];
                M_set_category(M_StringCategory::eager_copy);
            }
            ptr[length] = static_cast<char_type>(0);
            char_traits::copy(ptr, str, length);
            M_set_long_length(length);
            M_set_long_pointer(ptr);
        } else {
            auto ptr = M_short_pointer();
            STRIGNITE_IF_CONSTANT_EVALUATED(
                M_basic_data.M_short = {};
                M_extend_data = {};
                char_traits::copy(ptr, str, length);
                M_set_hash(hash());
            ) STRIGNITE_ELSE_NOT_CONSTANT_EVALUATED(
                char_traits::copy(ptr, str, length);
                ptr[length] = static_cast<char_type>(0);
                M_set_hash(0);
            )
            M_set_short_length(length);
        }

        assert(size() == length && char_traits::compare(data(), str, length) == 0);
    }

public:
    /**
     * @brief 从字符指针和指定长度构造字符串对象
     *
     * 本构造函数通过给定的字符数组和长度创建字符串实例，执行必要的内存分配和初始化。
     * 适用于已知长度的字符数组初始化场景，避免隐式依赖空终止符。
     *
     * @param[in] str    字符数组指针，必须满足：
     *                  - 非空指针
     *                  - 指向长度至少为length的有效内存区域
     * @param[in] length 要复制的字符数量，需满足：
     *                  - 0 <= length <= 字符数组实际长度
     *                  - 超过实际长度可能导致未定义行为
     *
     * @throw std::invalid_argument 当str为nullptr时抛出
     * @throw std::bad_alloc        内存分配失败时可能抛出（依赖实现）
     *
     * @note 关键特性：
     * - 严格校验指针有效性，拒绝空指针输入
     * - 根据长度自动选择SSO或堆存储策略
     * - 支持编译期构造（C++20 constexpr）
     * - 执行完整内存拷贝，保证数据独立性
     *
     * @warning 注意事项：
     * - 传入的字符数组不需要以空字符结尾
     * - 调用者需确保length参数与字符数组实际长度一致
     * - 修改原字符数组不会影响已构造的字符串对象
     *
     * @example
     * const char arr[] = {'H','e','l','l','o'};
     * bcs::basic_string s(arr, 5);  // 明确指定长度构造
     */
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string(const char_type* const str, const size_t length) {
        STRIGNITE_IF_UNLIKELY(!str)
            throw std::invalid_argument("string pointer cannot be nullptr.");
        M_construct_data(str, length);
    }

    /**
     * @brief 默认构造函数，创建空字符串对象
     *
     * 本构造函数初始化一个空字符串对象，采用短字符串优化(SSO)策略，
     * 保证对象处于有效且最小的初始状态。构造过程严格不抛异常。
     *
     * @note 初始化细节：
     * - 初始化内部数据结构为SSO模式
     * - 设置字符串长度为0
     * - 运行时环境下显式设置终止符（编译期环境自动处理）
     * - 重置哈希值为初始状态
     * - 最后验证存储类别为SSO（调试模式下断言）
     *
     * @par 特性说明：
     * - 空字符串默认使用SSO存储，不分配堆内存
     * - 支持编译期常量初始化（C++20 constexpr）
     * - 严格noexcept保证，适用于异常安全关键场景
     * - 强制内联优化构造路径
     *
     * @warning 注意事项：
     * - 构造后data()返回有效指针（可能指向内部缓冲区）
     * - size()返回0，empty()返回true
     * - 调试版本通过断言确保存储类别正确
     *
     * @example
     * bcs::basic_string<char> str;  // 创建空字符串
     * assert(str.empty());          // 验证为空
     */
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string() noexcept {
        M_init_data();
        M_set_short_length(0);
        STRIGNITE_IF_NOT_CONSTANT_EVALUATED(
            M_short_pointer()[0] = static_cast<char_type>(0)
        )
        M_set_hash(0);
        assert(M_category() == M_StringCategory::sso);
    }

public:
    /**
     * @brief 从C风格字符串构造字符串对象
     *
     * 本构造函数通过以空字符结尾的字符数组创建字符串实例，
     * 自动计算字符串长度并执行适当的内存管理策略。
     *
     * @tparam Char 字符类型，必须与模板的char_type严格一致
     *
     * @param[in] str 以空字符结尾的字符数组指针，必须满足：
     *               - 非空指针（由构造函数校验）
     *               - 指向有效的空终止字符序列
     *               - 字符类型需与类模板的char_type匹配
     *
     * @throw std::invalid_argument 当str为nullptr时抛出
     * @throw std::bad_alloc        内存分配失败时可能抛出
     *
     * @note 关键实现细节：
     * - 使用char_traits::length自动获取字符串长度
     * - 根据长度自动选择SSO或堆存储策略
     * - 支持编译期构造（C++20 constexpr）
     * - 执行深拷贝保证数据独立性
     *
     * @warning 注意事项：
     * - 必须确保str指向有效的空终止字符串
     * - 修改原C字符串不会影响已构造对象
     * - 字符类型不匹配将导致编译失败
     *
     * @example
     * const char* cstr = "Hello World";
     * bcs::basic_string s(cstr);  // 构造独立字符串对象
     *
     * @see M_construct_data 实际执行构造的底层方法
     */
    template<typename Char, typename = STRIGNITE_IS_SAME_V_T(char_type, Char)>
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string(const Char* const& str):
        basic_string(str, internal::traits_length<char_traits>(str)) {}

    /**
     * @brief 右值引用版本的C风格字符串构造函数
     *
     * 本构造函数为右值引用重载版本，用于支持从临时C字符串构造字符串对象。
     *
     * @tparam Char 字符类型，必须与模板的char_type严格一致
     *
     * @param[in] str 右值引用的字符指针，必须满足：
     *               - 非空指针（由基构造函数校验）
     *               - 指向有效的空终止字符序列
     *               - 即使作为右值传入，仍执行拷贝而非移动
     *
     * @throw std::invalid_argument 当str为nullptr时抛出（由基构造函数处理）
     * @throw std::bad_alloc        内存分配失败时可能抛出（依赖实现）
     *
     * @note 关键特性：
     * - 避免临时对象不必要的拷贝（编译器优化）
     * - 支持编译期构造（C++20 constexpr）
     * - 统一左右值构造行为，无特殊移动语义
     *
     * @warning 注意事项：
     * - 右值特性不会被利用，实际仍执行深拷贝
     * - 与左值版本构造函数具有相同的参数约束
     * - 禁止用于已悬垂指针（与左值版本风险相同）
     *
     * @example
     * auto s = bcs::basic_string<char>(get_temp_cstr());
     * // get_temp_cstr()返回临时char*
     */
    template<typename Char, typename = STRIGNITE_IS_SAME_V_T(char_type, Char)>
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string(const Char* const&& str) :
        basic_string(str, internal::traits_length<char_traits>(str)) {}

    /**
     * @brief 从字符数组字面量构造字符串对象
     *
     * 本构造函数通过编译期字符数组（如字符串字面量）创建字符串视图，
     * 实现零拷贝初始化，适用于已知生命周期的静态字符串场景。
     *
     * @tparam Char  字符类型，必须与当前类的char_type相同
     * @tparam N     字符数组长度（包含终止符）
     *
     * @param[in] str 字符数组引用，必须满足：
     *               - 编译期已知长度的数组（如字面量）
     *               - 以空字符结尾（N-1为有效数据长度）
     *               - 生命周期长于当前字符串对象
     *
     * @note 关键特性：
     * - 零内存分配：直接引用原数组内存
     * - 自动计算长度：N-1（排除终止符）
     * - 存储类别标记为literal，禁用内存管理操作
     * - 支持编译期常量初始化（C++20 constexpr）
     * - 强制内联优化构造路径
     *
     * @warning 注意事项：
     * - 必须确保原数组在字符串对象生命周期内有效
     * - 修改原数组内容将影响字符串对象
     * - 非空终止数组将导致未定义行为
     * - 调试版本通过断言验证存储类别
     *
     * @example
     * bcs::basic_string s1("Hello");    // N=6，长度5
     * constexpr char arr[] = "World";
     * bcs::basic_string s2(arr);        // 编译期构造
     */
    template<typename Char, size_t N, typename = STRIGNITE_IS_SAME_V_T(char_type, Char)>
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string(const Char (&str)[N]) noexcept {
        M_init_data();

        M_set_long_pointer(str);
        M_set_long_length(N - 1);
        M_set_category(M_StringCategory::literal);

        STRIGNITE_IF_CONSTANT_EVALUATED(
            M_set_hash(hash())
        ) STRIGNITE_ELSE_NOT_CONSTANT_EVALUATED(
            M_set_hash(0)
        )

        assert(M_category() == M_StringCategory::literal);
    }

    /**
     * @brief 从字符数组构造字符串对象
     *
     * 本构造函数通过字符数组创建字符串实例，自动推导数组长度并进行安全初始化，
     * 适用于栈分配数组或已知尺寸的字符缓冲区场景。
     *
     * @tparam Char 字符类型，必须与模板的char_type严格一致
     * @tparam N    字符数组尺寸（包含终止符的空间）
     *
     * @param[in] str 字符数组引用，必须满足：
     *               - 维度N >= 1（由数组类型保证）
     *               - 实际数据长度为N-1（自动排除终止符）
     *               - 数组内存有效且可访问
     *
     * @note 关键特性：
     * - 自动计算有效长度：N-1（假设最后一个字符为终止符）
     * - 委托给指针+长度构造函数实现核心逻辑
     * - 支持编译期构造（C++20 constexpr）
     * - 执行深拷贝操作，与原数组解耦
     *
     * @warning 注意事项：
     * - 要求数组维度N在编译期已知（模板参数推导）
     * - 若数组实际未包含终止符，可能导致长度计算错误
     * - 修改原数组内容不会影响已构造的字符串对象
     *
     * @example
     * char buffer[32] = "Hello";  // 实际长度5，容量32
     * bcs::basic_string s(buffer); // 构造长度为31的字符串（N=32）
     *
     * @see basic_string(const Char*, size_t) 实际执行构造的底层方法
     */
    template<typename Char, size_t N, typename = STRIGNITE_IS_SAME_V_T(char_type, Char)>
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string(Char (&str)[N]) :
        basic_string(str, N - 1) {}

    /**
     * @brief 从标准库字符串构造当前字符串对象
     *
     * 本构造函数通过标准库的basic_string对象创建新实例，执行深拷贝操作，
     * 保证生成的字符串拥有独立于原标准字符串的数据副本。
     *
     * @param[in] str 源标准字符串对象，需满足：
     *               - data()返回有效的字符指针（标准库保证）
     *               - 内容可以为空（size()==0）
     *
     * @throw std::bad_alloc 内存分配失败时可能抛出
     *
     * @note 关键特性：
     * - 通过委托构造调用指针+长度版本构造函数
     * - 完全复制数据内容，与原对象解耦
     * - 支持编译期构造（C++20 constexpr）
     * - 严格强异常安全保证
     *
     * @warning 注意事项：
     * - 构造过程可能触发堆内存分配（长字符串情况）
     * - 原标准字符串的生命周期与当前对象无关
     * - 修改原标准字符串不影响当前对象
     *
     * @example
     * std::string std_str = "Hello";
     * bcs::basic_string<char> bcs_str(std_str);  // 独立副本
     *
     * @see basic_string(const char_type*, size_t) 实际执行构造的底层方法
     */
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string(const std::basic_string<char_type>& str) :
        basic_string(str.data(), str.size()) {}

#if STRIGNITE_CPLUSPLUS_17
    /**
     * @brief 从标准库字符串视图构造字符串对象
     *
     * 本构造函数通过标准库的basic_string_view对象创建新实例，
     * 实现高效的数据拷贝，支持任意合法数据源的字符串构造。
     *
     * @param[in] str 源字符串视图对象，需满足：
     *               - data()指向有效的字符序列（可为空指针当size()==0）
     *               - 字符序列不需要以空字符结尾
     *
     * @throw std::bad_alloc 内存分配失败时可能抛出
     *
     * @note 关键特性：
     * - 通过委托构造调用指针+长度版本构造函数
     * - 完全复制视图内容，与原视图解耦
     * - 支持编译期构造（C++20 constexpr）
     * - 严格强异常安全保证
     *
     * @warning 注意事项：
     * - 构造时需确保视图数据在此时有效
     * - 原视图的生命周期与当前对象无关
     * - 接受空指针+零长度的合法组合（符合标准库规范）
     *
     * @example
     * std::string_view sv = "Hello";
     * bcs::basic_string<char> bcs_str(sv);  // 独立副本
     *
     * char arr[] = {'W','o','r','l','d'};
     * bcs::basic_string<char> bcs_str2(std::string_view(arr,5)); // 无终止符构造
     *
     * @see basic_string(const char_type*, size_t) 实际执行构造的底层方法
     */
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string(const std::basic_string_view<char_type> str) :
        basic_string(str.data(), str.size()) {}
#endif

    /**
     * @brief 从字符串视图构造字符串对象
     *
     * 本构造函数通过字符串视图创建字符串实例，执行数据的深拷贝操作，
     * 保证生成的字符串拥有独立于原视图的数据副本。
     *
     * @param[in] str 源字符串视图对象，需满足：
     *               - data()指向有效的字符序列（允许空指针当size()==0）
     *               - 字符序列不需要以空字符结尾
     *
     * @throw std::invalid_argument 当data()为nullptr且size()>0时抛出
     * @throw std::bad_alloc        内存分配失败时可能抛出
     *
     * @note 关键特性：
     * - 完全复制视图内容，与原视图生命周期解耦
     * - 支持编译期常量初始化（C++20 constexpr）
     * - 根据视图长度自动选择SSO或堆存储策略
     * - 严格强异常安全保证
     *
     * @warning 注意事项：
     * - 接受合法空视图（data()=nullptr且size()=0）
     * - 大尺寸视图可能触发堆内存分配
     * - 修改原视图内容不会影响已构造对象
     *
     * @example
     * string_view_t sv = "Hello";
     * basic_string s(sv);  // 创建独立副本
     *
     * char buffer[10];
     * string_view_t sv2(buffer, 5);
     * basic_string s2(sv2); // 从缓冲区视图构造
     *
     * @see basic_string(const char_type*, size_t) 实际执行构造的底层方法
     */
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string(const string_view_t str) :
        basic_string(str.data(), str.size()) {}

    /**
     * @brief 拷贝构造函数实现
     *
     * 本构造函数执行深拷贝或引用计数的智能拷贝，根据源对象的存储类别采取不同策略，
     * 确保新对象与源对象数据隔离或安全共享。
     *
     * @param[in] other 源字符串对象，必须满足：
     *                 - 非自引用（&other != this）
     *                 - 处于有效状态
     *
     * @throw std::bad_alloc 当内存分配失败时可能抛出（仅限eager_copy类型）
     *
     * @note 拷贝策略：
     * - eager_copy类型：分配独立内存并复制数据（深拷贝）
     * - ref_count类型：共享数据并增加引用计数
     * - sso/literal类型：直接复制栈数据（内存拷贝）
     *
     * @warning 注意事项：
     * - 修改源对象不会影响新对象
     *
     * @post 后置条件验证：
     * - 新对象大小与源对象严格一致
     * - 数据内容逐字节相等（通过assert验证）
     * - 存储类别与源对象一致
     * - ref_count类型引用计数正确增加
     *
     * @example
     * basic_string s1("Hello");
     * basic_string s2(s1); // 根据s1的存储类型执行相应拷贝策略
     *
     * @see operator= 拷贝赋值运算符
     * @see basic_string(basic_string&&) 移动构造函数
     */
    STRIGNITE_CPP20_CONSTEXPR basic_string(const basic_string& other) {
        STRIGNITE_ASSUME(&other != this)

        M_basic_data = other.M_basic_data;
        M_extend_data = other.M_extend_data;

        switch (M_category()) {
            case M_StringCategory::eager_copy: {
                const auto length = M_long_length();
                auto ptr = new char_type[length + 1];
                ptr[length] = static_cast<char_type>(0);
                char_traits::copy(ptr, other.M_long_pointer(), length);
                M_set_long_pointer(ptr);
                break;
            }
            case M_StringCategory::ref_count:
                M_RefCount::increment_count(M_long_pointer());
                break;
            default: ;
        }

        assert(size() == other.size());
        assert(char_traits::compare(data(), other.data(), size()) == 0);
    }

    /**
     * @brief 移动构造函数
     *
     * 本构造函数通过转移右值对象的所有权来高效初始化新对象，将原对象重置为空状态。
     * 操作完成后原对象仍为有效但未定义的SSO空字符串状态。
     *
     * @param[in] other 源右值字符串对象，调用后失去资源所有权
     *
     * @note 关键实现细节：
     * - 直接转移元数据（指针/长度/容量），无内存拷贝
     * - 非编译期环境下重置原对象为SSO空字符串：
     *   - 设置短字符串长度0
     *   - 写入空终止符
     *   - 重置哈希值为初始状态
     * - 严格noexcept保证，适用于容器重分配优化
     *
     * @warning 注意事项：
     * - 移动后原对象不应再被使用（除析构或重新赋值）
     *
     * @post 后置条件：
     * - 当前对象获得源对象全部资源
     * - 源对象变为SSO模式空字符串（通过断言保证）
     * - 源对象size()返回0，data()返回有效空字符串指针
     *
     * @example
     * basic_string s1 = get_large_string();
     * basic_string s2(std::move(s1));  // s1变为空字符串
     */
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE basic_string(basic_string&& other) noexcept {
        M_basic_data = other.M_basic_data;
        M_extend_data = other.M_extend_data;
        STRIGNITE_IF_NOT_CONSTANT_EVALUATED(
            other.M_set_short_length(0);
            other.M_short_pointer()[0] = static_cast<char_type>(0);
            other.M_set_hash(0);
        )
    }

    /**
     * @brief 析构函数实现
     *
     * 本析构函数根据字符串的存储类别执行相应的资源释放操作：
     * - eager_copy类型：释放堆分配的字符数组内存
     * - ref_count类型：减少引用计数，计数归零时释放共享内存
     * - sso/literal类型：无需特殊处理（栈内存自动回收）
     *
     * @note 关键特性：
     * - 严格noexcept保证，不抛出任何异常
     * - 自动处理不同存储策略的资源管理
     * - 引用计数类型通过原子操作保证线程安全递减
     *
     * @warning 注意事项：
     * - 必须确保存储类别标志的有效性
     * - 禁止在派生类中继承此析构函数（final类设计）
     * - 多线程环境下引用计数操作保证原子性
     *
     * @post 后置条件：
     * - 所有堆内存资源被正确释放
     * - 引用计数正确递减，共享数据可能被释放
     * - 对象进入不可用状态（符合C++对象生命周期规范）
     */
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE ~basic_string() noexcept {
        switch (M_category()) {
            case M_StringCategory::eager_copy:
                delete[] M_long_pointer();
                break;
            case M_StringCategory::ref_count:
                M_RefCount::decrement_count(M_long_pointer());
                break;
            default: ;
        }
    }

#pragma endregion constructors and destructor

#pragma region operators

    /**
     * @brief 拷贝赋值运算符实现
     *
     * 本函数实现深拷贝赋值语义，根据右操作数的存储类型执行不同的拷贝策略，
     * 保证赋值后左操作数拥有独立或正确引用计数的数据副本。
     *
     * @param[in] other 源字符串对象，不可为nullptr
     *
     * @return basic_string& 返回当前对象的引用
     *
     * @throw std::bad_alloc 当内存分配失败时可能抛出（仅限eager_copy模式）
     *
     * @note 关键实现细节：
     * - 自赋值检查：直接返回避免无效操作
     * - 先析构当前对象资源，再复制元数据
     * - 处理不同存储类型：
     *   - eager_copy类型：执行深拷贝，分配独立内存
     *   - ref_count类型：增加引用计数，共享数据
     *   - sso/literal类型：直接复制栈数据
     * - 最后通过断言验证数据一致性
     *
     * @warning 注意事项：
     * - 严格满足强异常安全保证（除内存分配失败情况）
     * - 非线程安全：多线程环境需外部同步
     * - 仅限左值对象调用（右值对象应使用移动赋值）
     *
     * @post 后置条件验证：
     * - 存储类别与源对象一致
     * - 数据长度与源对象一致
     * - 数据内容与源对象逐字节相同
     * - 数据指针与源对象不同（eager_copy类型）
     */
    basic_string& operator=(const basic_string& other) & {
        STRIGNITE_IF_UNLIKELY(&other == this) return *this;

        this->~basic_string();

        M_basic_data = other.M_basic_data;
        M_extend_data = other.M_extend_data;

        switch (M_category()) {
            case M_StringCategory::eager_copy: {
                const auto length = M_long_length();
                auto* temp = new char_type[length + 1];
                temp[length] = static_cast<char_type>(0);
                char_traits::copy(temp, M_long_pointer(), length);
                M_set_long_pointer(temp);
                break;
            }
            case M_StringCategory::ref_count:
                M_RefCount::increment_count(M_long_pointer());
                break;
            default: ;
        }

        assert(M_category() == other.M_category());
        assert(size() == other.size());
        assert(data() != other.data());
        assert(char_traits::compare(data(), other.data(), size()) == 0);

        return *this;
    }

    basic_string& operator=(const basic_string& other) && = delete;

    /**
     * @brief 移动赋值运算符实现
     *
     * 本函数执行资源所有权的转移操作，将右值对象的内部状态高效地移动到当前对象，
     * 原对象将被重置为有效的空字符串状态（SSO模式）。
     *
     * @param[in] other 源右值字符串对象，调用后处于有效但内容未定义的状态
     *
     * @return basic_string& 返回当前对象的引用
     *
     * @note 关键实现细节：
     * - 自赋值保护：快速返回避免无效操作
     * - 先释放当前对象持有的资源
     * - 直接转移内存控制权（无深拷贝）
     * - 将源对象重置为SSO空字符串状态
     * - 严格noexcept保证，适用于容器移动优化
     *
     * @warning 注意事项：
     * - 仅限左值对象调用（右值对象应使用移动构造）
     * - 操作后源对象仍为有效状态（SSO空字符串）
     * - 不修改源对象哈希值外的元数据
     * - 非线程安全：并行操作需要外部同步
     *
     * @post 后置条件验证：
     * - 当前对象获得源对象的所有资源
     * - 源对象变为SSO模式空字符串（通过断言保证）
     * - 源对象的data()返回有效指针（指向空字符串）
     */
    basic_string& operator=(basic_string&& other) & noexcept {
        STRIGNITE_IF_UNLIKELY(&other == this) return *this;

        this->~basic_string();

        M_basic_data = other.M_basic_data;
        M_extend_data = other.M_extend_data;
        other.M_set_short_length(0);
        other.M_short_pointer()[0] = static_cast<char_type>(0);
        other.M_set_hash(0);

        assert(other.M_category() == M_StringCategory::sso);

        return *this;
    }

    basic_string& operator=(basic_string&& other) && noexcept = delete;

    /**
     * @brief 字符串追加赋值运算符
     *
     * 本操作符实现字符串的原地追加操作，将指定字符串内容追加到当前字符串末尾，
     * 通过创建临时对象并移动赋值实现，保证操作后的字符串有效性。
     *
     * @param[in] other 要追加的字符串对象，允许为空字符串
     *
     * @return basic_string& 返回当前对象的引用，支持链式调用
     *
     * @note 关键实现细节：
     * - 实际调用operator+生成临时字符串对象
     * - 通过移动赋值运算符转移临时对象资源
     * - 严格noexcept保证（假设operator+和赋值操作不抛异常）
     * - 仅限左值对象调用（右值对象应直接使用operator+）
     *
     * @example
     * basic_string str("Hello");
     * str += " World";  // str变为"Hello World"
     *
     * @see operator+ 字符串连接操作的具体实现
     */
    STRIGNITE_FORCE_INLINE
    basic_string& operator+=(const basic_string& other) & noexcept {
        return *this = std::move(*this + other);
    }

    STRIGNITE_FORCE_INLINE
    basic_string& operator+=(const basic_string& other) && noexcept = delete;

    /**
     * @brief 追加C风格字符串到当前字符串末尾
     *
     * 重载+=操作符实现字符串拼接功能，将给定的以空字符结尾的C风格字符串追加到当前字符串末尾。
     *
     * @param other 要追加的C风格字符串指针，须满足：
     *              - 必须为有效的以空字符结尾的字符串
     *              - 不可为nullptr
     *
     * @return basic_string& 返回当前字符串的引用，支持链式操作
     *
     * @note 特性说明：
     * - 强制编译器进行内联优化
     * - noexcept 保证操作不抛出异常
     * - 左值限定符(&)确保只能在非临时对象上调用
     *
     * @warning 注意事项：
     * 1. 调用者需确保other参数有效性，传入nullptr会导致未定义行为
     * 3. 使用移动语义避免不必要的拷贝，但仍有临时对象构造开销
     *
     * @example
     * basic_string str("Hello");
     * str += " World";  // str变为"Hello World"
     */
    STRIGNITE_FORCE_INLINE
    basic_string& operator+=(const char_type* const other) & noexcept {
        return *this = std::move(*this + other);
    }

    STRIGNITE_FORCE_INLINE
    basic_string& operator+=(const char_type* other) && noexcept = delete;

    /**
     * @brief 追加字符串视图到当前字符串末尾
     *
     * 重载+=操作符实现字符串拼接功能，将给定的字符串视图内容追加到当前字符串末尾。
     *
     * @param other 要追加的字符串视图对象，须满足：
     *              - 视图指向的有效内存范围必须合法
     *              - 不要求以空字符结尾（适配字符串视图特性）
     *
     * @return basic_string& 返回当前字符串的引用，支持链式操作
     *
     * @note 特性说明：
     * - 强制编译器进行内联优化
     * - noexcept 保证操作不抛出异常
     * - 左值限定符(&)确保只能在非临时对象上调用
     *
     * @warning 注意事项：
     * 1. 调用者需确保other视图在操作期间的有效性（不持有数据所有权）
     * 3. 视图可能包含空字符，会完整保留所有字符内容
     *
     * @example
     * basic_string str("Hello");
     * string_view_t sv(" View");
     * str += sv;  // str变为"Hello View"
     */
    STRIGNITE_FORCE_INLINE
    basic_string& operator+=(const string_view_t other) & noexcept {
        return *this = std::move(*this + other);
    }

    STRIGNITE_FORCE_INLINE
    basic_string& operator+=(string_view_t other) && noexcept = delete;

private:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR
    basic_string concat_helper(const char_type* const other, const size_t other_length) const {
        STRIGNITE_ASSUME(other != nullptr)
        STRIGNITE_IF_UNLIKELY(other_length == 0) return *this;
        if (other[1] == static_cast<char_type>(0)) return *this + other[0];

        auto view = to_string_view();
        const size_t new_length = view.size() + other_length;
        auto result = M_reserve(new_length);
        char_type* ptr = result.M_pointer();
        char_traits::copy(ptr, view.data(), view.size());
        char_traits::copy(ptr + view.size(), other, other_length);
        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))
        return result;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_string operator+(char_type c) const noexcept {
        STRIGNITE_IF_UNLIKELY(c == static_cast<char_type>(0)) return *this;

        const auto view = to_string_view();
        const auto new_length = view.size() + 1;
        auto result = M_reserve(new_length);
        char_type* ptr = result.M_pointer();
        char_traits::copy(ptr, view.data(), view.size());
        ptr[view.size()] = c;
        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))
        return result;
    }

public:
    /**
     * @brief 连接当前字符串与C风格字符串，生成新字符串对象
     *
     * 重载+运算符实现字符串拼接功能，将当前字符串与以空字符结尾的C风格字符串连接，
     * 返回包含两者内容的新字符串对象。
     *
     * @param other 要连接的C风格字符串指针，必须满足：
     *              - 必须为有效的以空字符结尾的字符串
     *              - 禁止传入nullptr（未显式检查，需调用者保证有效性）
     *
     * @return 返回新构造的basic_string对象，
     *          包含原始字符串与参数字符串的连接结果
     *
     * @note 特性说明：
     * - 强制编译器进行内联优化
     * - 支持C++20编译时计算
     * - 属性确保返回值必须被处理
     *
     * @warning 注意事项：
     * 1. 调用者必须保证other参数的有效性，传入无效指针会导致未定义行为
     *
     * @example
     * basic_string<char> str("Hello");
     * auto new_str = str + " World";  // 生成"Hello World"新对象
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string operator+(const char_type* const other) const {
        return concat_helper(other, internal::traits_length<char_traits>(other));
    }

    /**
     * @brief 连接当前字符串与另一个字符串对象，生成新字符串
     *
     * 重载+运算符实现字符串拼接功能，将当前字符串与另一个basic_string对象内容连接，
     * 返回包含两者内容的新字符串对象。原始对象及参数对象均不会被修改。
     *
     * @param other 要连接的另一个basic_string对象，须满足：
     *              - 必须为有效的字符串对象
     *              - 允许接受临时对象/右值，但通过const引用安全访问
     *
     * @return 返回新构造的basic_string对象，
     *          包含两个字符串的连接结果，要求必须处理返回值
     *
     * @note 特性说明：
     * - 强制编译器进行内联优化
     * - 支持C++20编译时字符串构造
     *
     * @warning 注意事项：
     * 参数对象必须处于有效状态
     *
     * @example
     * basic_string<char> str1("Hello");
     * basic_string<char> str2(" World");
     * auto result = str1 + str2;  // 生成"Hello World"新对象
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string operator+(const basic_string& other) const {
        auto data = other.to_string_view();
        return concat_helper(data.data(), data.size());
    }

    /**
     * @brief 连接当前字符串与字符串视图对象，生成新字符串
     *
     * 重载+运算符实现字符串拼接功能，将当前字符串与字符串视图内容连接，
     * 返回包含两者内容的新字符串对象。原始对象及参数视图均不会被修改。
     *
     * @param other 要连接的字符串视图对象，必须满足：
     *              - 底层数据指针不可为nullptr（否则触发异常）
     *              - 视图范围有效，长度由size()确定（不依赖空终止符）
     *
     * @return 返回新构造的basic_string对象，
     *          包含两个内容的连接结果，强制要求必须处理返回值
     *
     * @throw std::runtime_error 当other.data()返回空指针时抛出
     *
     * @note 特性说明：
     * - 强制内联优化
     * - 支持编译时字符串构造
     * - 显式空指针检查确保操作安全性
     *
     * @warning 注意事项：
     * 1. 即使视图size()为0，只要data()非空仍可安全操作
     * 2. 视图内容可能包含空字符，会被完整保留
     * 3. 参数视图的生命周期需独立维护，连接操作不持有视图数据
     *
     * @example
     * basic_string<char> str("Hello");
     * string_view_t sv(" View");
     * auto result = str + sv;  // 生成"Hello View"新对象
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string operator+(const string_view_t other) const {
        STRIGNITE_IF_UNLIKELY(!other.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return concat_helper(other.data(), other.size());
    }

    /**
     * @brief 实现C风格字符串与字符串对象的连接操作
     *
     * 该友元运算符重载实现左侧C风格字符串与右侧字符串对象的拼接功能，
     * 通过高效的内存预分配策略优化连接性能
     *
     * @param lhs 左侧操作数，必须满足：
     *            - 有效的以空字符结尾的C风格字符串
     *            - 禁止传入nullptr（未显式检查，需调用者保证有效性）
     * @param rhs 右侧操作数，需要拼接的字符串对象
     *
     * @return 返回新构造的字符串对象，包含两个操作数的连接结果
     *
     * @note 核心特性：
     * - 预计算长度并预分配内存，避免多次分配
     * - 支持编译时字符串构造
     *
     * @warning 注意事项：
     * 1. 参数有效性：
     *    - lhs必须为有效的C字符串（以'\0'结尾）
     *    - 传入无效lhs指针将导致内存访问越界
     * 2. 性能特征：
     *    - 内存分配策略：精确预分配避免空间浪费
     *
     * @example
     * const char* prefix = "Hello ";
     * basic_string<char> suffix("World");
     * auto result = prefix + suffix; // 生成"Hello World"
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR
    friend basic_string operator+(const char_type* const lhs, const basic_string& rhs) {
        auto view = rhs.to_string_view();
        auto lhs_length = internal::traits_length<char_traits>(lhs);
        basic_string result = M_reserve(lhs_length + view.size());
        char_type* ptr = result.M_pointer();
        char_traits::copy(ptr, lhs, lhs_length);
        char_traits::copy(ptr + lhs_length, view.data(), view.size());
        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))

        return result;
    }

    /**
     * @brief 实现字符串视图与字符串对象的连接操作
     *
     * 该友元运算符重载实现左侧字符串视图与右侧字符串对象的拼接功能，
     * 通过高效内存管理和安全校验机制保障操作可靠性
     *
     * @param lhs 左侧操作数字符串视图，必须满足：
     *            - data()返回值不可为nullptr（否则触发异常）
     *            - 视图范围[size()]内内存合法有效
     * @param rhs 右侧操作数字符串对象，需拼接的basic_string实例
     *
     * @return 返回新构造的字符串对象，包含两个操作数的连接结果
     *
     * @throw std::runtime_error 当lhs.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 精确计算所需空间避免多次分配
     * - CPP20支持编译时构造
     *
     * @warning 注意事项：
     * 1. 参数有效性：
     *    - lhs视图必须在操作期间保持有效
     *    - 视图可能包含空字符，完整保留所有内容
     * 2. 性能特征：
     *    - 内存分配策略：精确预分配避免空间浪费
     *
     * @example
     * string_view_t sv("Data:");
     * basic_string<char> str("2024");
     * auto result = sv + str;  // 生成"Data:2024"
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR
    friend basic_string operator+(const string_view_t lhs, const basic_string& rhs) {
        STRIGNITE_IF_UNLIKELY(!lhs.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        auto view = rhs.to_string_view();
        auto lhs_length = lhs.size();
        basic_string result = M_reserve(lhs_length + view.size());
        char_type* ptr = result.M_pointer();
        char_traits::copy(ptr, lhs, lhs_length);
        char_traits::copy(ptr + lhs_length, view.data(), view.size());
        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))

        return result;
    }

    /**
     * @brief 字符串移除赋值运算符
     *
     * 本操作符实现原地移除操作，将当前字符串中所有匹配的子串移除，
     * 并更新当前对象为移除后的结果字符串。
     *
     * @param[in] other 要移除的目标子串视图，需满足：
     *                - 非空视图（data()允许为nullptr当size()==0）
     *                - 目标子串长度不大于当前字符串长度
     *
     * @return basic_string& 返回当前对象的引用，支持链式操作
     *
     * @throw std::invalid_argument 当other参数无效时可能抛出（由remove方法触发）
     *
     * @note 实现特性：
     * - 实际调用remove方法生成新字符串对象
     * - 通过移动赋值运算符转移资源
     * - 仅限左值对象调用（右值对象应直接使用remove方法）
     *
     * @warning 注意事项：
     * - 操作后原字符串对象被新字符串替换
     * - 大字符串操作可能触发内存重分配
     * - 目标子串为空时直接返回原字符串
     *
     * @example
     * basic_string s = "a-b-c-d";
     * s -= "-";  // s变为"abcd"
     *
     * @see remove() 实际执行移除操作的底层方法
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string& operator-=(const string_view_t other) & {
        return *this = remove(other);
    }

    /**
     * @brief 复合赋值运算符：从当前字符串中移除所有目标子串
     *
     * 重载-=运算符实现字符串的"删除所有匹配子串"操作，返回当前字符串的引用。
     *
     * @param other 需要移除的目标子串对象，必须满足：
     *              - 有效的basic_string实例
     *              - 非空字符串（空目标将导致未定义行为）
     *
     * @return basic_string& 返回当前字符串的引用，包含移除所有目标子串后的结果
     *
     * @note 特性说明：
     * - 通过创建新字符串实现，原字符串内容保持不变
     * - 支持编译时字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊行为：
     *    - 由于在const成员函数中赋值，实际修改的是临时副本
     *    - 原字符串内容在操作后可能被替换为修改后的副本
     * 2. 参数限制：
     *    - 传入空目标字符串将导致未定义行为
     *    - 连续重叠子串可能产生级联删除效果
     *
     * @example
     * basic_string str("apple-banana-apple");
     * str -= basic_string("apple");
     * // str变为"-banana-"
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string& operator-=(const basic_string& other) & {
        return *this = remove(other);
    }

    /**
     * @brief 复合赋值运算符：从当前字符串中移除所有C风格子串
     *
     * 重载-=运算符实现字符串的"删除所有匹配C子串"操作，返回当前字符串的引用。
     * 该操作通过创建新字符串实现，原字符串内容将被替换为删除所有目标子串后的结果。
     *
     * @param other 需要移除的C风格字符串指针，必须满足：
     *              - 有效的以空字符('\0')结尾的字符串
     *              - 禁止传入nullptr（未显式检查，需调用者保证有效性）
     *
     * @return 返回当前字符串的引用，包含：
     *         - 成功：移除所有目标子串后的字符串引用
     *         - 失败：原字符串保持不变（当内存分配失败时）
     *
     * @note 核心特性：
     * - CPP20支持编译时操作
     * - 优化高频调用性能
     * - 仅允许在非临时对象上调用（&限定符）
     *
     * @warning 重要注意事项：
     * 1. 参数有效性：
     *    - 传入空指针将导致未定义行为
     * 2. 内存特性：
     *    - 可能触发内存重新分配（当结果长度变化时）
     *    - 保持强异常安全保证（失败时原字符串不变）
     *
     * @example
     * basic_string<char> str("test-data-test");
     * str -= "test";   // str变为"-data-"
     * str -= "-";      // str变为"data"
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string& operator-=(const char_type* const other) & {
        return *this = remove(other);
    }

    basic_string& operator-=(string_view_t other) && = delete;

    basic_string& operator-=(basic_string& other) && = delete;

    basic_string& operator-=(char_type* other) && = delete;

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string operator-(const string_view_t other) const {
        return remove(other);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string operator-(const basic_string& other) const {
        return remove(other);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string operator-(const char_type* const other) const {
        return remove(other);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string operator*(const size_t times) const {
        return repeat(times);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    friend basic_string operator*(const size_t times, const basic_string& str) {
        return str * times;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    const char_type* operator*() const & noexcept {
        return data();
    }

    friend const char_type* operator*(const basic_string& str) noexcept = delete;

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string& operator*=(const size_t times) & {
        return *this = repeat(times);
    }

    basic_string& operator*=(size_t times) && = delete;

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type operator[](size_t index) const noexcept {
        return data()[index];
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t operator[](const string_view_t index) const noexcept {
        return find(index);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t operator[](const char_type* const index) const noexcept {
        return find(index);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t operator[](const basic_string& index) const noexcept {
        return find(index);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator==(const basic_string& other) const noexcept {
        return to_string_view() == other.to_string_view();
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator==(const string_view_t other) const noexcept {
        STRIGNITE_IF_UNLIKELY(!other.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view() == other;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator==(const char_type* const other) const noexcept {
        return to_string_view() == string_view_t{ other };
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator!=(const string_view_t other) const noexcept {
        return to_string_view() != other;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator!=(const basic_string& other) const noexcept {
        return !(*this == other);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool operator!=(const char_type* const other) const noexcept {
        return !(*this == other);
    }

    /**
     * @brief 将字符串对象写入输出流
     *
     * 本操作符重载实现将字符串内容输出到标准输出流，
     * 保证高效的流写入操作。
     *
     * @param[in,out] os 目标输出流对象，必须处于有效状态
     * @param[in] str    要输出的字符串对象，允许空字符串
     *
     * @return std::basic_ostream<char_type>& 返回输出流引用以支持链式操作
     *
     * @note 关键特性：
     * - 支持标准流操作的所有特性（如宽度设置/对齐方式等）
     * - 严格强异常安全保证（依赖底层流实现）
     * - 自动处理空字符串的特殊情况
     *
     * @warning 注意事项：
     * - 输出内容不包含额外的终止符
     * - 流的状态可能被修改（如错误标志位）
     * - 大字符串输出可能受流缓冲区限制
     *
     * @example
     * basic_string<char> str("Hello World");
     * std::cout << str;  // 输出"Hello World"
     */
    STRIGNITE_FORCE_INLINE
    friend std::basic_ostream<char_type>&
    operator<<(std::basic_ostream<char_type>& os, const basic_string& str) {
        return os << str.to_string_view();
    }

    /**
     * @brief 输入流提取运算符重载
     *
     * 本操作符实现从输入流中读取字符串数据，以空白字符为分隔符，
     * 使用固定大小的缓冲区进行分批读取，优化内存使用效率。
     *
     * @param[in,out] is  输入流对象，读取后可能修改流状态
     * @param[out] str    目标字符串对象，接收读取结果
     *
     * @return std::basic_istream<char_type>& 返回输入流引用以支持链式调用
     *
     * @throw std::bad_alloc 当内存分配失败时可能抛出
     * @throw 无显式抛出，但可能传播输入流操作抛出的异常
     *
     * @note 实现细节：
     * - 使用512字节对齐的缓冲区（实际字符数根据sizeof(char_type)计算）
     * - 循环读取字符直到遇到空白字符
     * - 自动处理缓冲区满的情况，分批追加到结果
     * - 最终通过移动赋值转移所有权到目标字符串
     * - 空白字符检测依赖is_space函数实现
     *
     * @warning 注意事项：
     * - 缓冲区大小固定，超长单词可能触发多次内存分配
     * - 不保留输入流中的空白字符
     * - 流错误状态需调用方处理（如设置is.exceptions()）
     * - 当流到达文件尾时可能设置failbit
     *
     * @example
     * basic_string<char> s;
     * std::istringstream iss("Hello World");
     * iss >> s;  // s内容为"Hello"，流位置停在空格处
     */
    friend std::basic_istream<char_type>&
    operator>>(std::basic_istream<char_type>& is, basic_string& str) {
        basic_string result{};
        constexpr size_t buffer_size = 512 / sizeof(char_type);
        char_type buffer[buffer_size];
        size_t length = 0;
        while (true) {
            if (length >= buffer_size) {
                result += { buffer, length };
                length = 0;
                continue;
            }
            const auto ch = static_cast<char_type>(is.get());
            if (is_space(ch)) {
                result += { buffer, length };
                str = std::move(result);
                break;
            }
            buffer[length++] = ch;
        }
        return is;
    }

    friend std::basic_istream<char_type>&
    operator>>(std::basic_istream<char_type>& is, basic_string&& str) = delete;

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    operator string_view_t() const & noexcept {
        return to_string_view();
    }

    operator string_view_t() const && noexcept = delete;

    /**
     * @brief 隐式转换为标准库字符串对象
     *
     * 本转换运算符提供向std::basic_string<char_type>的类型转换能力，
     * 用于无缝对接需要标准字符串类型的API，保证数据的安全转换。
     *
     * @return std::basic_string<char_type>
     *  - 包含原字符串所有字符的独立副本
     *  - 空字符串转换为空的标准字符串对象
     *  - 数据与原对象完全解耦
     *
     * @note 关键特性：
     * - 执行深拷贝操作，产生独立数据副本
     * - 支持编译期常量转换（C++20 constexpr）
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 转换过程可能触发堆内存分配（大字符串场景）
     * - 隐式转换可能导致意外拷贝（建议显式调用to_std_string()）
     * - 不保留原对象的哈希值等元信息
     *
     * @example
     * - void process(const std::string& s);
     * - basic_string<char> bs("text");
     * - process(bs);  // 自动触发类型转换
     *
     * @see to_std_string() 显式转换方法的推荐用法
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    operator std::basic_string<char_type>() const noexcept {
        return to_std_string();
    }

#if STRIGNITE_CPLUSPLUS_17
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    operator std::basic_string_view<char_type>() const & noexcept {
        return to_std_string_view();
    }

    operator std::basic_string_view<char_type>() const && noexcept = delete;
#endif

#pragma endregion operators

public: //迭代器
    /**
     * @brief 获取指向字符串起始位置的常量迭代器
     *
     * 本函数提供标准容器风格的迭代器访问接口，用于支持范围遍历等现代C++特性。
     * 返回的迭代器指向字符串底层字符数组的首元素，适用于只读访问场景。
     *
     * @return const_iterator
     *  - 非空字符串：指向第一个字符的常量迭代器
     *  - 空字符串：返回与end()相等的迭代器
     *
     * @note 关键特性：
     * - 返回类型为常量迭代器，禁止通过迭代器修改内容
     * - 支持C++11起基于范围的for循环
     * - 支持编译期迭代操作（C++20 constexpr）
     * - 强制内联优化访问路径
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 迭代器本质为字符指针，与标准库迭代器行为兼容
     *
     * @example
     * - for (auto it = str.begin(); it != str.end(); ++it) {
     * -     // 只读访问字符
     * - }
     *
     * @see end()   获取结束迭代器的关联方法
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    const_iterator begin() const & noexcept { return M_pointer(); }

    STRIGNITE_CPP17_NODISCARD
    const_iterator begin() const && noexcept = delete;

    /**
     * @brief 获取字符串末尾后位的常量迭代器
     *
     * 本函数根据字符串存储类型（短字符串优化/长字符串）智能计算结束位置，
     * 返回符合STL规范的结束迭代器，用于标识字符串数据的逻辑末端。
     *
     * @return const_iterator
     *  - 指向字符串最后一个有效字符的下一个位置
     *  - 空字符串时与begin()返回值相同
     *
     * @note 关键特性：
     * - 返回迭代器符合"past-the-end"标准语义
     * - 支持编译期迭代操作（C++20 constexpr）
     * - 强制内联优化计算路径
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 迭代器本质为字符指针，与标准库迭代器行为兼容
     *
     * @example
     * for (auto it = str.begin(); it != str.end(); ++it) {
     *     // 安全遍历字符序列
     * }
     *
     * @see begin()   获取起始迭代器的关联方法
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    const_iterator end() const & noexcept {
        return M_is_long() ? M_long_pointer() + M_long_length() : M_short_pointer() + M_short_length();
    }

    STRIGNITE_CPP17_NODISCARD
    const_iterator end() const && noexcept = delete;

public:
    /**
     * @brief 统计目标子字符串在当前字符串中的出现次数
     *
     * 本函数提供高效的非重叠子串计数功能，支持多字符模式匹配。通过bm算法实现高效查找，
     * 避免不必要的内存拷贝。
     *
     * @param[in] target 要统计的目标子字符串指针，需满足：
     *                   - 非空指针
     *                   - 指向有效的空终止字符串
     *
     * @return size_t 目标子串出现的次数，以下情况返回0：
     *                - target为空指针
     *                - target为空字符串
     *                - 未找到匹配项
     *
     * @note 实现特性：
     * - 单字符优化：直接调用字符版count函数
     * - 多字符处理：通过str_search循环查找
     * - 空指针安全：自动处理nullptr输入
     * - 常量表达式：支持C++20下的编译期求值
     *
     * @warning 注意事项：
     * - 目标字符串必须正确空终止，否则可能引发未定义行为
     * - 统计结果不包含重叠匹配（如"aaa"中查找"aa"将返回1次）
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     *
     * @example
     * - string s = "abababa";
     * - s.count("aba");  // 返回2次非重叠匹配
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR size_t count(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().count(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR size_t count(const basic_string& target) const noexcept {
        return to_string_view().count(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR size_t count(const char_type* const target) const noexcept {
        return to_string_view().count(string_view_t{ target });
    }

public:
#pragma region find

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_string& target) const {
        return to_string_view().find(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target) const {
        return to_string_view().find(string_view_t{ target });
    }

    /**
     * @brief 查找目标子字符串在当前字符串中的首次出现位置
     *
     * 本函数提供高效子串查找能力，支持从指定位置开始搜索。
     *
     * @param[in] target     要查找的目标子字符串指针，必须满足：
     *                       - 非空指针
     *                       - 指向有效的空终止字符串
     * @param[in] start_pos  起始搜索位置（默认从首字符开始），需满足：
     *                       - 0 <= start_pos <= 字符串长度
     *                       - 超过字符串长度时将直接返回npos
     *
     * @return size_t
     *  - 成功时返回目标子串首次出现的起始索引（相对整个字符串）
     *  - 失败时返回npos（size_t最大值）
     *
     * @throw std::invalid_argument 当target为nullptr时抛出
     *
     * @note 实现特性：
     * - 自动获取目标子串长度（通过char_traits::length）
     * - 支持编译期计算（C++20 constexpr）
     * - 强制内联优化
     *
     * @warning 使用注意：
     * - 目标字符串必须正确空终止，否则可能引发未定义行为
     * - 起始位置越界时自动修正为字符串末端（不会抛出异常）
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target, const size_t start_pos) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target, start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_string& target, const size_t start_pos) const {
        return to_string_view().find(target.to_string_view(), start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target, const size_t start_pos) const {
        return to_string_view().find(string_view_t{ target }, start_pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const string_view_t target, const size_t start_pos, const size_t length) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find(target, start_pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const basic_string& target, const size_t start_pos, const size_t length) const {
        return to_string_view().find(target.to_string_view(), start_pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find(const char_type* const target, const size_t start_pos, const size_t length) const {
        return to_string_view().find(string_view_t{ target }, start_pos, length);
    }

    /**
     * @brief 查找目标子字符串在当前字符串中的最后一次出现位置
     *
     * 本函数提供从后向前的子串查找功能，返回目标子串最后一次出现的起始索引。
     * 查找过程严格遵循字符特性类的比较规则，确保与标准库字符串操作行为一致。
     *
     * @param[in] target 要查找的目标子字符串指针，必须满足：
     *                   - 非空指针
     *                   - 指向有效的空终止字符串
     *
     * @return size_t
     *  - 成功时返回目标子串最后一次出现的起始索引
     *  - 失败时返回npos（size_t类型最大值）
     *
     * @throw std::invalid_argument 当target为nullptr时抛出
     *
     * @note 实现特性：
     * - 自动获取目标子串长度（通过char_traits::length）
     * - 支持编译期计算（C++20 constexpr）
     * - 强制内联优化查找路径
     *
     * @warning 注意事项：
     * - 目标字符串必须正确空终止，否则可能引发未定义行为
     * - 返回的索引位置是相对于整个字符串的绝对位置
     * - 函数通过反向bm算法进行反向查找
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     *
     * @example
     * string s = "abbaabba";
     * s.find_last("ab");  // 返回4（第二个"ab"的起始位置）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_string& target) const noexcept {
        return to_string_view().find_last(target.to_string_view());
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target) const noexcept {
        return to_string_view().find_last(string_view_t{ target });
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target, const size_t pos) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target, pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_string& target, const size_t pos) const noexcept {
        return to_string_view().find_last(target.to_string_view(), pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target, const size_t pos) const noexcept {
        return to_string_view().find_last(string_view_t{ target }, pos);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const string_view_t target, const size_t pos, const size_t length) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return to_string_view().find_last(target, pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const basic_string& target, const size_t pos, const size_t length) const noexcept {
        return to_string_view().find_last(target.to_string_view(), pos, length);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t find_last(const char_type* const target, const size_t pos, const size_t length) const noexcept {
        return to_string_view().find_last(string_view_t{ target }, pos, length);
    }

#pragma endregion find

#pragma region split

    /**
     * @brief 将字符串拆分为基于提供的字符串分隔符的子字符串
     * @tparam container_type 返回的容器类型
     * @tparam T 容器中的字符串类型
     * @param delim 目标字符串分隔符
     * @return 包含了拆分后的子字符串的容器
     */
    template<template<typename...> class container_type = std::vector, typename T = basic_string,
        typename = internal::is_container_t<container_type<T>>>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_FORCE_INLINE
    STRIGNITE_CPP20_CONSTEXPR container_type<T> split(const basic_string& delim) const noexcept {
        return split<container_type, T>(delim.data());
    }

    /**
     * @brief 将字符串拆分为基于提供的字符串分隔符的子字符串
     * @tparam T 容器中的字符串类型
     * @param delim 目标字符串分隔符
     * @return 包含了拆分后的子字符串的容器
     */
    template<typename T>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_FORCE_INLINE
    STRIGNITE_CPP20_CONSTEXPR std::vector<T> split(const char_type* delim) const {
        return split<std::vector, T>(delim);
    }

private:
    template<typename T>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_FORCE_INLINE
    STRIGNITE_CPP20_CONSTEXPR std::vector<T> split(char_type delim) const noexcept {
        return split<std::vector, T>(delim);
    }

    template<template<typename...> class container_type = std::vector,
        typename T = basic_string,
        typename = internal::is_container_t<container_type<T>>,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR container_type<T> split(char_type delim) const noexcept {
        assert(delim);

        container_type<T> buffer{};

        const char_type* last_begin = begin();
        const char_type* last_end = nullptr;

        while (*last_begin) {
            if (*last_begin == delim) {
                ++last_begin;
                continue;
            }
            last_end = last_begin;
            while (*++last_end && *last_end != delim) {}

            if (*last_end == static_cast<char_type>(0)) {
                buffer.emplace_back(T(last_begin, last_end - last_begin));
                break;
            }

            buffer.emplace_back(T(last_begin, last_end - last_begin));
            last_begin = last_end + 1;
        }
        return buffer;
    }

public:
    /**
     * @brief 将当前字符串按指定分隔符分割为容器元素
     *
     * 本函数提供高效的分割字符串实现，支持自定义容器类型和字符串类型模板参数，
     * 适用于不同场景下的字符串分割需求。
     *
     * @tparam container_type  容器类型模板（默认std::vector），必须满足STL容器接口
     *                        - 需具备emplace_back方法
     *                        - 元素类型需与T匹配
     * @tparam T             存储的字符串类型（默认basic_string），允许类型：
     *                       - basic_string
     *                       - string_view_t
     *                       - std::basic_string<char_type>
     *                       - C++17起支持std::basic_string_view<char_type>
     *
     * @param[in] delim      分隔符指针（必须以空字符结尾的有效字符串）
     *
     * @return container_type<T> 包含分割结果的容器，每个元素为原字符串的子串视图或副本
     *
     * @throw std::invalid_argument 当delim为nullptr时抛出
     *
     * @note 实现特性：
     * - 空分隔符：立即返回空容器
     * - 单字符分隔符：调用优化版本split(char_type)
     * - 多字符分隔符：使用str_search进行模式查找
     * - 内存优化：分割结果直接构造到容器中，避免中间拷贝
     * - 支持编译期计算（C++20 constexpr）
     *
     * @warning 注意事项：
     * - 分隔符必须为有效空终止字符串，否则可能引发未定义行为
     * - 返回容器元素的生命周期：
     *   - 当T为视图类型时需确保原字符串存活期
     *   - 当T为副本类型时独立持有数据
     * - 大字符串分割时注意容器内存分配策略
     * - 函数标记为[[nodiscard]]，必须接收返回值
     *
     * @example
     * auto vec = str.split<std::vector, std::string_view>("||");
     * auto lst = str.split<std::list>(";");
     */
    template<template<typename...> class container_type = std::vector,
        typename T = basic_string,
        typename = internal::is_container_t<container_type<T>>,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR
    container_type<T> split(const char_type* const delim) const { // 有bug
        STRIGNITE_IF_UNLIKELY(!delim)
            throw std::invalid_argument("string pointer cannot be nullptr.");

        STRIGNITE_IF_UNLIKELY(delim[0] == static_cast<char_type>(0)) return {};
        if (delim[1] == static_cast<char_type>(0)) return split<container_type, T>(delim[0]);

        auto view = to_string_view();

        const char_type* last_begin = view.begin();
        const char_type* last_end = nullptr;

        auto length = view.size();

        const auto delim_length = char_traits::length(delim);

        container_type<T> buffer{};

        while (true) {
            last_end = internal::str_search(last_begin, length, delim, delim_length);
            if (!last_end) {
                buffer.emplace_back(T(last_begin, view.end() - last_begin));
                return buffer;
            }
            buffer.emplace_back(T(last_begin, last_end - last_begin));
            length -= last_end - last_begin;
            last_begin = last_end + delim_length;
        }
    }

#pragma endregion split

#pragma region contains

private:
    template<size_t buffer_size, typename = internal::enable_if_t<buffer_size <= 0x400>>
    STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR
    bool contains_helper(const string_view_t target, const bool ignore_case = false) const noexcept {
        STRIGNITE_IF_UNLIKELY(target.empty()) return false;
        if (target.size() == 1) return contains(target[0], ignore_case);
        if (!ignore_case) {
            return to_string_view().contains(target);
        }
        auto lower = this->to_lower();
        auto lower_data = lower.to_string_view();
        if (target.size() > buffer_size) {
            auto* temp = new char_type[target.size()];
            STRIGNITE_DEFER { delete[] temp; };
            for (int i = 0; i < target.size(); ++i) {
                temp[i] = static_cast<char_type>(bcs::to_lower(target[i]));
            }
            return internal::str_search(lower_data.data(), lower_data.size(), temp, target.size());
        }
        char_type temp[buffer_size];
        for (int i = 0; i < target.size(); ++i) {
            temp[i] = static_cast<char_type>(bcs::to_lower(target[i]));
        }
        return internal::str_search(lower_data.data(), lower_data.size(), temp, target.size());
    }

    //可以优化
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR bool contains(char_type target, const bool ignore_case = false) const noexcept {
        if (ignore_case) {
            char_type lower_target_ch = bcs::to_lower(target);
            auto lower = this->to_lower();
            auto lower_data = lower.to_string_view();
            return char_traits::find(lower_data.data(), lower_data.size(), lower_target_ch);
        }
        auto view = to_string_view();
        return char_traits::find(view.data(), view.size(), target);
    }

public:
    /**
     * @brief 检查当前字符串是否包含指定目标子串
     *
     * 本函数提供高效的子串存在性检测，支持大小写敏感/不敏感两种模式。通过模板参数实现栈内存优化，
     * 避免小规模查找时的堆内存分配。
     *
     * @tparam buffer_size 内部查找缓冲区大小（默认64），控制用于存储中间结果的栈数组容量，
     *                     根据典型场景调整可优化性能
     *
     * @param[in] target       要查找的目标子串（必须为有效空终止字符串）
     * @param[in] ignore_case  是否启用忽略大小写模式（默认false，即大小写敏感）
     *
     * @return bool
     *  - true  : 字符串包含目标子串
     *  - false : 不包含目标子串或target为空指针/空字符串
     *
     * @note 特性说明：
     * - 空指针/空字符串检测：立即返回false
     * - 常量表达式：支持C++20下的编译期求值
     * - 强制内联：通过编译器指令优化小型函数调用开销
     *
     * @warning 使用注意：
     * - 目标字符串须以空字符结尾，否则可能引发未定义行为
     * - buffer_size过大可能导致栈溢出，建议保持<=256
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const string_view_t target, const bool ignore_case = false) const noexcept {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return contains_helper<buffer_size>(target, ignore_case);
    }

    /**
     * @brief 检查当前字符串是否包含指定目标子串
     *
     * 本函数提供高效的子串存在性检测，支持大小写敏感/不敏感两种模式。通过模板参数实现栈内存优化，
     * 避免小规模查找时的堆内存分配。
     *
     * @tparam buffer_size 内部查找缓冲区大小（默认64），控制用于存储中间结果的栈数组容量，
     *                     根据典型场景调整可优化性能
     *
     * @param[in] target       要查找的目标子串（必须为有效空终止字符串）
     * @param[in] ignore_case  是否启用忽略大小写模式（默认false，即大小写敏感）
     *
     * @return bool
     *  - true  : 字符串包含目标子串
     *  - false : 不包含目标子串或target为空指针/空字符串
     *
     * @note 特性说明：
     * - 空指针/空字符串检测：立即返回false
     * - 常量表达式：支持C++20下的编译期求值
     * - 强制内联：通过编译器指令优化小型函数调用开销
     *
     * @warning 使用注意：
     * - 目标字符串须以空字符结尾，否则可能引发未定义行为
     * - buffer_size过大可能导致栈溢出，建议保持<=256
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const basic_string& target, const bool ignore_case = false) const noexcept {
        return contains_helper<buffer_size>(target, ignore_case);
    }

    /**
     * @brief 检查当前字符串是否包含指定目标子串
     *
     * 本函数提供高效的子串存在性检测，支持大小写敏感/不敏感两种模式。通过模板参数实现栈内存优化，
     * 避免小规模查找时的堆内存分配。
     *
     * @tparam buffer_size 内部查找缓冲区大小（默认64），控制用于存储中间结果的栈数组容量，
     *                     根据典型场景调整可优化性能
     *
     * @param[in] target       要查找的目标子串（必须为有效空终止字符串）
     * @param[in] ignore_case  是否启用忽略大小写模式（默认false，即大小写敏感）
     *
     * @return bool
     *  - true  : 字符串包含目标子串
     *  - false : 不包含目标子串或target为空指针/空字符串
     *
     * @note 特性说明：
     * - 空指针/空字符串检测：立即返回false
     * - 常量表达式：支持C++20下的编译期求值
     * - 强制内联：通过编译器指令优化小型函数调用开销
     *
     * @warning 使用注意：
     * - 目标字符串须以空字符结尾，否则可能引发未定义行为
     * - buffer_size过大可能导致栈溢出，建议保持<=256
     * - 返回值带有[[nodiscard]]属性，必须进行结果检查
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool contains(const char_type* const target, const bool ignore_case = false) const noexcept {
        return contains_helper<buffer_size>(string_view_t{ target }, ignore_case);
    }

#pragma endregion contains

private:
#pragma region replace
    /**
     * @brief 替换当前字符串中所有匹配的子字符串并返回新字符串
     *
     * 本函数用于将当前字符串中所有出现的`old_value`子串替换为`new_value`，支持不同长度的子串替换。
     * 使用模板参数控制内部索引数组的初始大小，优化小规模替换场景的性能。
     *
     * @tparam buffer_size  内部索引数组的初始大小，必须满足 buffer_size <= 0x400
     *
     * @param old_value    需要被替换的子字符串指针
     * @param old_size     需要被替换的子字符串长度
     * @param new_value    用于替换的新子字符串指针
     * @param new_size     新子字符串的长度
     *
     * @return 返回替换后的新字符串
     *
     * @note 特性说明：
     * - 支持C++20 constexpr上下文
     * - 原地替换：当新旧子串长度相同时直接修改原字符串副本
     * - 动态扩展：当替换次数超过buffer_size时自动扩容索引数组
     * - 空操作：当old_size为0或未找到匹配时直接返回原字符串
     *
     * @complexity 时间复杂度：
     * - 最坏情况: O(n + m*k) (n=原字符串长度, m=替换次数, k=查找开销)
     * - 空间复杂度: O(buffer_size + 新字符串长度)
     *
     * @algorithm 算法流程：
     * 1. 快速返回：当old_size==0时直接返回原字符串
     * 2. 等长替换：新旧子串长度相同时，直接遍历替换
     * 3. 变长替换：
     *    a. 收集所有匹配位置到索引数组
     *    b. 计算新字符串长度并分配内存
     *    c. 分段拷贝原字符串内容和新子串
     *
     * @internal 内部机制：
     * - 使用BM算法进行子串查找
     * - 自动内存管理：通过STRIGNITE_DEFER确保动态分配的索引数组被释放
     * - 哈希维护：STRIGNITE_IF_CONSTANT_EVALUATED在常量求值时维护哈希值
     *
     * @warning 注意事项：
     * - buffer_size必须<=1024 (0x400)
     * - 非线程安全：操作修改字符串内部状态
     * - 指针有效性：调用者需确保参数指针在函数执行期间有效
     * - 异常安全：不抛出异常，但内存分配失败可能导致未定义行为
     */
    template<size_t buffer_size, typename = internal::enable_if_t<buffer_size <= 0x400>>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_string replace_helper(const char_type* old_value, const size_t old_size,
                                                          const char_type* new_value, const size_t new_size) const {
        STRIGNITE_IF_UNLIKELY(old_size == 0) return *this;

        STRIGNITE_ASSUME(old_value != nullptr && new_value != nullptr);

        auto view = to_string_view();

        const auto old_value_length = old_size;
        const auto new_value_length = new_size;

        if (old_value_length == new_value_length) {
            auto result = M_reserve(view.size());
            char_type* ptr = result.M_pointer();

            char_traits::copy(ptr, view.data(), view.size());

            for (auto* index = internal::str_search(ptr, view.size(), old_value, old_value_length);
                 index != nullptr;
                 index = internal::str_search(index + old_value_length,
                                              view.size() - (index + old_value_length - ptr),
                                              old_value, old_value_length)) {
                char_traits::copy(const_cast<char_type *>(index), new_value, new_value_length);
            }

            STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))

            return result;
        }

        // 相当于精简版的 small_vector<const char_type*, 64>
        const char_type* indexes[buffer_size];
        const char_type** indexes_ptr = indexes;
        size_t indexes_capacity = buffer_size - 1;
        size_t indexes_size = 0;

        STRIGNITE_DEFER {
            if (indexes_ptr != indexes)
                delete[] indexes_ptr;
        };

        for (auto index = internal::str_search(view.data(), view.size(), old_value, old_value_length);
             index != nullptr;
             index = internal::str_search(
                 index + old_value_length,                                      //haystack
                 view.size() - (index + old_value_length - view.data()),      //haystack length
                 old_value,                                                     //needle
                 old_value_length                                               //needle length
             )
        ) {
            STRIGNITE_IF_UNLIKELY(indexes_size >= indexes_capacity) {
                const auto new_indexes_capacity = indexes_size * 2;
                auto* temp = new const char_type *[new_indexes_capacity];
                memcpy(temp, indexes_ptr, sizeof(const char_type *) * indexes_size);
                if (indexes_ptr != indexes) {
                    delete[] indexes_ptr;
                }
                indexes_capacity = new_indexes_capacity - 1;
                indexes_ptr = temp;
            }
            indexes_ptr[indexes_size++] = index;
        }

        if (indexes_size == 0) {
            return *this;
        }

        const size_t new_length = view.size() - old_value_length * indexes_size + new_value_length * indexes_size;

        auto result = M_reserve(new_length);
        char_type* ptr = result.M_pointer();

        auto last = view.data();

        size_t current_length = 0;
        for (size_t i = 0; i < indexes_size; i++) {
            auto index = indexes_ptr[i];
            auto last_length = index - last;
            char_traits::copy(ptr + current_length, last, last_length);
            current_length += last_length;
            char_traits::copy(ptr + current_length, new_value, new_value_length);
            current_length += new_value_length;
            last = index + old_value_length;
        }
        last = indexes_ptr[indexes_size - 1] + old_value_length;
        char_traits::copy(ptr + current_length, last, view.end() - last);

        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))

        return result;
    }

public:
    /**
     * @brief 替换字符串中所有匹配的子串并返回新字符串
     *
     * 本函数执行全局替换操作，将当前字符串中所有出现的`old_value`子串替换为`new_value`，
     * 支持不同长度子串的高效替换，通过模板参数优化内存使用。
     *
     * @tparam buffer_size 内部操作缓冲区大小（默认64），控制初始栈数组容量：
     *                    - 建议值范围：64-1024
     *                    - 必须满足 buffer_size <= 0x400 的编译期约束
     *
     * @param[in] old_value 需要被替换的目标子串视图，需满足：
     *                     - 非空视图（data() != nullptr）
     *                     - 长度大于0
     * @param[in] new_value 用于替换的新子串视图，允许为空：
     *                     - 空视图等效于删除操作
     *
     * @return basic_string 包含替换结果的新字符串对象，特征：
     *                    - 独立内存分配，与原字符串无关联
     *                    - 未匹配时返回原字符串副本
     *                    - 支持编译期常量表达式操作
     *
     * @note 实现特性：
     * - 新旧子串等长时执行原地替换优化
     * - 大字符串自动切换堆分配索引数组
     * - 替换顺序从前往后，避免重叠修改
     * - 使用 Boyer-Moore 算法加速查找
     *
     * @warning 注意事项：
     * - 新旧子串相同且等长时直接返回原字符串
     * - 新子串包含旧子串可能导致无限替换（如替换"a"为"aa"）
     * - 超大字符串替换建议增大buffer_size参数
     *
     * @example
     * auto s = "hello world".replace("l", "L");
     * // 返回"heLLo worLd"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const string_view_t old_value, const string_view_t new_value) const {
        STRIGNITE_IF_UNLIKELY(!old_value.data() || !new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(
            old_value.data(), old_value.size(), new_value.data(), new_value.size()
        );
    }

    /**
     * @brief 替换字符串中所有匹配的旧值为新值
     *
     * 该模板函数将当前字符串中所有出现的旧值(old_value)替换为指定的新值(new_value)，
     * 并通过缓冲区优化策略提升替换操作的性能
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于内存分配优化：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理大规模替换时可增大此值以减少内存重分配次数
     *
     * @param old_value 需要被替换的旧值，必须满足：
     *                  - 有效的以空字符结尾的C风格字符串
     *                  - 禁止传入nullptr
     * @param new_value 替换使用的新值，必须满足：
     *                  - 有效的以空字符结尾的字符串
     *                  - 禁止传入nullptr
     *
     * @return 返回新生成的字符串对象，包含所有替换后的结果
     *
     * @throw std::runtime_error 当new_value.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 内部缓冲区机制优化内存使用效率
     * - 支持C++20编译期字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊参数处理：
     *    - 旧值为空字符串("")将导致未定义行为
     * 2. 性能建议：
     *    - 默认缓冲区适合多数长度小于64字符的场景
     *    - 处理超长字符串时建议增大buffer_size参数
     *
     * @example
     * basic_string<char> str("Hello World");
     * auto result = str.replace("World", "C++");
     * // 结果字符串为"Hello C++"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const char_type* const old_value, const string_view_t new_value) const {
        STRIGNITE_IF_UNLIKELY(!new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(
            old_value, internal::traits_length<char_traits>(old_value), new_value.data(), new_value.size()
        );
    }

    /**
     * @brief 替换字符串中所有匹配的旧值为新值
     *
     * 该模板函数将当前字符串中所有出现的旧值(old_value)替换为指定的新值(new_value)，
     * 并通过缓冲区优化策略提升替换操作的性能
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于内存分配优化：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理大规模替换时可增大此值以减少内存重分配次数
     *
     * @param old_value 需要被替换的旧值，必须满足：
     *                  - 有效的以空字符结尾的C风格字符串
     *                  - 禁止传入nullptr
     * @param new_value 替换使用的新值，必须满足：
     *                  - 有效的以空字符结尾的字符串
     *                  - 禁止传入nullptr
     *
     * @return 返回新生成的字符串对象，包含所有替换后的结果
     *
     * @throw std::runtime_error 当new_value.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 内部缓冲区机制优化内存使用效率
     * - 支持C++20编译期字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊参数处理：
     *    - 旧值为空字符串("")将导致未定义行为
     * 2. 性能建议：
     *    - 默认缓冲区适合多数长度小于64字符的场景
     *    - 处理超长字符串时建议增大buffer_size参数
     *
     * @example
     * basic_string<char> str("Hello World");
     * auto result = str.replace("World", "C++");
     * // 结果字符串为"Hello C++"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const string_view_t old_value, const char_type* const new_value) const {
        STRIGNITE_IF_UNLIKELY(!old_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(
            old_value.data(), old_value.size(), new_value, internal::traits_length<char_traits>(new_value)
        );
    }

    /**
     * @brief 替换字符串中所有匹配的旧值为新值
     *
     * 该模板函数将当前字符串中所有出现的旧值(old_value)替换为指定的新值(new_value)，
     * 并通过缓冲区优化策略提升替换操作的性能
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于内存分配优化：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理大规模替换时可增大此值以减少内存重分配次数
     *
     * @param old_value 需要被替换的旧值，必须满足：
     *                  - 有效的以空字符结尾的C风格字符串
     *                  - 禁止传入nullptr
     * @param new_value 替换使用的新值，必须满足：
     *                  - 有效的以空字符结尾的字符串
     *                  - 禁止传入nullptr
     *
     * @return 返回新生成的字符串对象，包含所有替换后的结果
     *
     * @throw std::runtime_error 当new_value.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 内部缓冲区机制优化内存使用效率
     * - 支持C++20编译期字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊参数处理：
     *    - 旧值为空字符串("")将导致未定义行为
     * 2. 性能建议：
     *    - 默认缓冲区适合多数长度小于64字符的场景
     *    - 处理超长字符串时建议增大buffer_size参数
     *
     * @example
     * basic_string<char> str("Hello World");
     * auto result = str.replace("World", "C++");
     * // 结果字符串为"Hello C++"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const basic_string& old_value, const string_view_t new_value) const {
        STRIGNITE_IF_UNLIKELY(!new_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(
            old_value.data(), old_value.size(), new_value.data(), new_value.size()
        );
    }

    /**
     * @brief 替换字符串中所有匹配的旧值为新值
     *
     * 该模板函数将当前字符串中所有出现的旧值(old_value)替换为指定的新值(new_value)，
     * 并通过缓冲区优化策略提升替换操作的性能
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于内存分配优化：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理大规模替换时可增大此值以减少内存重分配次数
     *
     * @param old_value 需要被替换的旧值，必须满足：
     *                  - 有效的以空字符结尾的C风格字符串
     *                  - 禁止传入nullptr
     * @param new_value 替换使用的新值，必须满足：
     *                  - 有效的以空字符结尾的字符串
     *                  - 禁止传入nullptr
     *
     * @return 返回新生成的字符串对象，包含所有替换后的结果
     *
     * @throw std::runtime_error 当new_value.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 内部缓冲区机制优化内存使用效率
     * - 支持C++20编译期字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊参数处理：
     *    - 旧值为空字符串("")将导致未定义行为
     * 2. 性能建议：
     *    - 默认缓冲区适合多数长度小于64字符的场景
     *    - 处理超长字符串时建议增大buffer_size参数
     *
     * @example
     * basic_string<char> str("Hello World");
     * auto result = str.replace("World", "C++");
     * // 结果字符串为"Hello C++"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const string_view_t old_value, const basic_string& new_value) const {
        STRIGNITE_IF_UNLIKELY(!old_value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return replace_helper<buffer_size>(
            old_value.data(), old_value.size(), new_value.data(), new_value.size()
        );
    }

    /**
     * @brief 替换字符串中所有匹配的旧值为新值
     *
     * 该模板函数将当前字符串中所有出现的旧值(old_value)替换为指定的新值(new_value)，
     * 并通过缓冲区优化策略提升替换操作的性能
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于内存分配优化：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理大规模替换时可增大此值以减少内存重分配次数
     *
     * @param old_value 需要被替换的旧值，必须满足：
     *                  - 有效的以空字符结尾的C风格字符串
     *                  - 禁止传入nullptr
     * @param new_value 替换使用的新值，必须满足：
     *                  - 有效的以空字符结尾的字符串
     *                  - 禁止传入nullptr
     *
     * @return 返回新生成的字符串对象，包含所有替换后的结果
     *
     * @throw std::runtime_error 当new_value.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 内部缓冲区机制优化内存使用效率
     * - 支持C++20编译期字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊参数处理：
     *    - 旧值为空字符串("")将导致未定义行为
     * 2. 性能建议：
     *    - 默认缓冲区适合多数长度小于64字符的场景
     *    - 处理超长字符串时建议增大buffer_size参数
     *
     * @example
     * basic_string<char> str("Hello World");
     * auto result = str.replace("World", "C++");
     * // 结果字符串为"Hello C++"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const char_type* const old_value, const basic_string& new_value) const {
        return replace_helper<buffer_size>(
            old_value, internal::traits_length<char_traits>(old_value), new_value.data(), new_value.size()
        );
    }

    /**
     * @brief 替换字符串中所有匹配的旧值为新值
     *
     * 该模板函数将当前字符串中所有出现的旧值(old_value)替换为指定的新值(new_value)，
     * 并通过缓冲区优化策略提升替换操作的性能
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于内存分配优化：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理大规模替换时可增大此值以减少内存重分配次数
     *
     * @param old_value 需要被替换的旧值，必须满足：
     *                  - 有效的以空字符结尾的C风格字符串
     *                  - 禁止传入nullptr
     * @param new_value 替换使用的新值，必须满足：
     *                  - 有效的以空字符结尾的字符串
     *                  - 禁止传入nullptr
     *
     * @return 返回新生成的字符串对象，包含所有替换后的结果
     *
     * @throw std::runtime_error 当new_value.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 内部缓冲区机制优化内存使用效率
     * - 支持C++20编译期字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊参数处理：
     *    - 旧值为空字符串("")将导致未定义行为
     * 2. 性能建议：
     *    - 默认缓冲区适合多数长度小于64字符的场景
     *    - 处理超长字符串时建议增大buffer_size参数
     *
     * @example
     * basic_string<char> str("Hello World");
     * auto result = str.replace("World", "C++");
     * // 结果字符串为"Hello C++"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const basic_string& old_value, const char_type* const new_value) const {
        return replace_helper<buffer_size>(
            old_value.data(), old_value.size(), new_value, internal::traits_length<char_traits>(new_value)
        );
    }

    /**
     * @brief 替换字符串中所有匹配的旧值为新值
     *
     * 该模板函数将当前字符串中所有出现的旧值(old_value)替换为指定的新值(new_value)，
     * 并通过缓冲区优化策略提升替换操作的性能
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于内存分配优化：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理大规模替换时可增大此值以减少内存重分配次数
     *
     * @param old_value 需要被替换的旧值，必须满足：
     *                  - 有效的以空字符结尾的C风格字符串
     *                  - 禁止传入nullptr
     * @param new_value 替换使用的新值，必须满足：
     *                  - 有效的以空字符结尾的字符串
     *                  - 禁止传入nullptr
     *
     * @return 返回新生成的字符串对象，包含所有替换后的结果
     *
     * @throw std::runtime_error 当new_value.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 内部缓冲区机制优化内存使用效率
     * - 支持C++20编译期字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊参数处理：
     *    - 旧值为空字符串("")将导致未定义行为
     * 2. 性能建议：
     *    - 默认缓冲区适合多数长度小于64字符的场景
     *    - 处理超长字符串时建议增大buffer_size参数
     *
     * @example
     * basic_string<char> str("Hello World");
     * auto result = str.replace("World", "C++");
     * // 结果字符串为"Hello C++"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const basic_string& old_value, const basic_string& new_value) const {
        return replace_helper<buffer_size>(
            old_value.data(), old_value.size(), new_value.data(), new_value.size()
        );
    }

    /**
     * @brief 替换字符串中所有匹配的旧值为新值
     *
     * 该模板函数将当前字符串中所有出现的旧值(old_value)替换为指定的新值(new_value)，
     * 并通过缓冲区优化策略提升替换操作的性能
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于内存分配优化：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理大规模替换时可增大此值以减少内存重分配次数
     *
     * @param old_value 需要被替换的旧值，必须满足：
     *                  - 有效的以空字符结尾的C风格字符串
     *                  - 禁止传入nullptr
     * @param new_value 替换使用的新值，必须满足：
     *                  - 有效的以空字符结尾的字符串
     *                  - 禁止传入nullptr
     *
     * @return 返回新生成的字符串对象，包含所有替换后的结果
     *
     * @throw std::runtime_error 当new_value.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 内部缓冲区机制优化内存使用效率
     * - 支持C++20编译期字符串处理
     *
     * @warning 注意事项：
     * 1. 特殊参数处理：
     *    - 旧值为空字符串("")将导致未定义行为
     * 2. 性能建议：
     *    - 默认缓冲区适合多数长度小于64字符的场景
     *    - 处理超长字符串时建议增大buffer_size参数
     *
     * @example
     * basic_string<char> str("Hello World");
     * auto result = str.replace("World", "C++");
     * // 结果字符串为"Hello C++"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(const char_type* const old_value, const char_type* const new_value) const {
        return replace_helper<buffer_size>(
            old_value, internal::traits_length<char_traits>(old_value),
            new_value, internal::traits_length<char_traits>(new_value)
        );
    }

private:
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR
    basic_string replace(const std::initializer_list<const std::pair<string_view_t, string_view_t> *>& args) const {
        internal::MutString<char_type, char_traits> temp =
                internal::MutString<char_type, char_traits>::get_string(to_string_view());
        STRIGNITE_DEFER {
            delete[] temp.data;
        };
        using namespace ::bcs::internal;
        for (const auto arg: args) {
            replace_helper<buffer_size>(
                temp, arg->first.data(), arg->first.size(), arg->second.data(), arg->second.size()
            );
        }
        return { temp.data, temp.length };
    }


#pragma region multiple argument replace

public:
#define r_arg const std::pair<string_view_t, string_view_t> &

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3, &arg4 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3, &arg4, &arg5 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3, &arg4, &arg5, &arg6 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7,
                         r_arg arg8) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11) const {
        return replace<buffer_size>({ &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11 });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14,
                         r_arg arg15) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16, r_arg arg17) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16, &arg17
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16, r_arg arg17, r_arg arg18) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16, &arg17, &arg18
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16, r_arg arg17, r_arg arg18, r_arg arg19) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16, &arg17, &arg18, &arg19
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16, r_arg arg17, r_arg arg18, r_arg arg19, r_arg arg20) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16, &arg17, &arg18, &arg19, &arg20
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16, r_arg arg17, r_arg arg18, r_arg arg19, r_arg arg20, r_arg arg21) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16, &arg17, &arg18, &arg19, &arg20, &arg21
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16, r_arg arg17, r_arg arg18, r_arg arg19, r_arg arg20, r_arg arg21,
                         r_arg arg22) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16, &arg17, &arg18, &arg19, &arg20, &arg21, &arg22
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16, r_arg arg17, r_arg arg18, r_arg arg19, r_arg arg20, r_arg arg21, r_arg arg22,
                         r_arg arg23) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16, &arg17, &arg18, &arg19, &arg20, &arg21, &arg22, &arg23
        });
    }

    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string replace(r_arg arg1, r_arg arg2, r_arg arg3, r_arg arg4, r_arg arg5, r_arg arg6, r_arg arg7, r_arg arg8,
                         r_arg arg9, r_arg arg10, r_arg arg11, r_arg arg12, r_arg arg13, r_arg arg14, r_arg arg15,
                         r_arg arg16, r_arg arg17, r_arg arg18, r_arg arg19, r_arg arg20, r_arg arg21, r_arg arg22,
                         r_arg arg23, r_arg arg24) const {
        return replace<buffer_size>({
            &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12,
            &arg13, &arg14, &arg15, &arg16, &arg17, &arg18, &arg19, &arg20, &arg21, &arg22, &arg23, &arg24
        });
    }

#undef r_arg

#pragma endregion multiple argument replace

#pragma endregion replace

public:
    /**
     * @brief 移除字符串中所有匹配的指定子串
     *
     * 该模板函数通过将目标子串替换为空字符串的方式，实现从当前字符串中删除所有目标子串的功能。
     *
     * @tparam buffer_size 内部替换操作缓冲区大小（默认64），控制用于存储匹配位置的
     *                     栈数组容量，影响大字符串处理的性能表现
     *
     * @param[in] target 需要移除的目标子串，必须满足：
     *               - 指向有效的空终止字符串
     *
     * @return basic_string 包含移除结果的新字符串对象
     *
     * @throw std::invalid_argument 当str为nullptr时抛出
     *
     * @note 实现特性：
     * - 空目标子串的特殊处理：直接返回原字符串副本
     * - 支持编译期常量表达式操作（C++20）
     * - 强制内联优化执行路径
     *
     * @warning 注意事项：
     * - 目标子串长度大于原字符串时直接返回原字符串副本
     * - 大容量字符串操作建议增大buffer_size模板参数
     * - 返回新字符串的内存布局可能与原字符串不同
     *
     * @example
     * string s = "a-b-c-d";
     * string_view sv = "-";
     * auto removed = s.remove(sv); // 返回"abcd"
     *
     * @see replace() 实际执行替换操作的底层方法
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string remove(const string_view_t target) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        constexpr char_type empty[] = { static_cast<char_type>(0) };
        return replace<buffer_size>(target, empty);
    }

    /**
     * @brief 移除当前字符串中所有指定的目标子串
     *
     * 该模板函数通过将目标子串替换为空字符串的方式，实现从当前字符串中删除所有目标子串的功能。
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于控制替换操作时的内存分配策略：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 处理超长字符串时可增大此值以减少内存重分配次数
     *
     * @param target 要移除的目标子串对象，必须满足：
     *               - 有效的basic_string实例
     *               - 非空字符串（空目标将导致未定义行为）
     *
     * @return 返回新生成的字符串对象，包含删除所有目标子串后的结果
     *
     * @note 核心特性：
     * - 内部通过利用缓冲区优化内存使用
     * - 支持编译期字符串操作
     * - 小字符串场景下完全避免堆内存分配
     *
     * @warning 重要注意事项：
     * 1. 目标字符串限制：
     *    - 目标字符串包含空字符时可能产生非预期结果
     * 2. 缓冲区策略：
     *    - 默认缓冲区适用于多数长度小于64字节的字符串场景
     *    - 处理包含大量匹配项的字符串时建议增大buffer_size
     *
     * @example
     * basic_string<char> str("apple,orange,apple,banana");
     * basic_string<char> target("apple");
     * auto result = str.remove(target); // 返回",orange,,banana"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string remove(const basic_string& target) const {
        constexpr char_type empty[] = { static_cast<char_type>(0) };
        return replace<buffer_size>(target, empty);
    }

    /**
     * @brief 移除字符串中所有指定目标子串
     *
     * 该模板函数通过将目标子串替换为空字符串的方式，实现从当前字符串中删除所有目标子串的功能。
     *
     * @tparam buffer_size 内部缓冲区大小参数，默认值64，用于优化替换操作时的内存分配：
     *                     - 当结果字符串长度小于buffer_size时，使用栈内存避免堆分配
     *                     - 需要大规模字符串操作时可适当增大该值
     *
     * @param target 需要移除的目标子串指针，必须满足：
     *               - 有效的以空字符结尾的C风格字符串
     *               - 禁止传入nullptr（未显式检查，需调用者保证有效性）
     *
     * @return 返回新生成的字符串对象，包含删除所有目标子串后的结果
     *
     * @note 特性说明：
     * - 内部通过利用缓冲区优化内存使用
     * - 支持编译时字符串处理
     * - 强制内联提升高频调用性能
     *
     * @warning 注意事项：
     * 1. 目标字符串处理：
     *    - 空目标字符串("")将导致未定义行为
     *    - 连续重叠子串可能产生级联删除效果
     * 2. 缓冲区选择：
     *    - 默认缓冲区适用于多数短字符串场景
     *    - 处理超长字符串时建议增大buffer_size参数以避免多次分配
     *
     * @example
     * basic_string<char> str("HelloWorldHello");
     * auto result = str.remove("Hello");
     * // result内容为"World"
     */
    template<size_t buffer_size = 64>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string remove(const char_type* const target) const {
        constexpr char_type empty[] = { static_cast<char_type>(0) };
        return replace<buffer_size>(target, empty);
    }

#pragma region expression templates

private:
    class expression;

    template<typename Data, typename Func>
    class string_expression;

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    static const char_type* get_str(const string_view_t str) noexcept {
        STRIGNITE_IF_UNLIKELY(!str.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return str.data();
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    static const char_type* get_str(const char_type* str) noexcept {
        STRIGNITE_IF_UNLIKELY(!str)
            throw std::runtime_error("string pointer cannot be nullptr.");
        return str;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    static size_t get_len(const string_view_t str) noexcept {
        return str.length();
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    static size_t get_len(const char_type* str) {
        return char_traits::length(str);
    }

    template<typename OldValue, typename NewValue, size_t buffer_size>
    class replace_expression {
        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        explicit replace_expression(const OldValue old_value, const NewValue new_value) noexcept :
            M_old_value(old_value),
            M_new_value(new_value) {}

        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        void operator()(internal::MutString<char_type, char_traits>& str) const {
            internal::replace_helper<buffer_size>(
                str, get_str(M_old_value), get_len(M_old_value), get_str(M_new_value), get_len(M_new_value)
            );
        }

        const OldValue M_old_value;
        const NewValue M_new_value;

        friend class expression;

        template<typename, typename>
        friend class string_expression;
    };

    template<typename Target, size_t buffer_size>
    class remove_expression {
        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        explicit remove_expression(const Target target) noexcept :
            M_target(target) {}

        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        void operator()(internal::MutString<char_type, char_traits>& str) const {
            constexpr char_type empty[] = { static_cast<char_type>(0) };
            internal::replace_helper<buffer_size>(str, get_str(M_target), get_len(M_target), empty, 0);
        }

        const Target M_target;

        friend class expression;

        template<typename, typename>
        friend class string_expression;
    };

    template<typename Data, typename Func>
    class string_expression {
    private:
        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        explicit string_expression(const Data& data, const Func& func) :
            M_data(data),
            M_func(func) {}

        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        void get_string(internal::MutString<char_type, char_traits>& str) const {
            M_data.get_string(str);
            M_func(str);
        }

    public:
        STRIGNITE_CPP17_NODISCARD
        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        operator basic_string() const {
            internal::MutString<char_type, char_traits> str;
            get_string(str);
            STRIGNITE_DEFER { delete[] str.data; };
            return { str.data, str.length };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<string_expression, replace_expression<const char_type *, const char_type *, buffer_size>>
        replace(const char_type* old_value, const char_type* new_value) const noexcept {
            return string_expression<string_expression,
                replace_expression<const char_type *, const char_type *, buffer_size>>{
                *this, replace_expression<const char_type *, const char_type *, buffer_size>{ old_value, new_value }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<string_expression, replace_expression<string_view_t, const char_type *, buffer_size>>
        replace(const string_view_t old_value, const char_type* new_value) const noexcept {
            return string_expression<string_expression,
                replace_expression<string_view_t, const char_type *, buffer_size>>{
                *this, replace_expression<string_view_t, const char_type *, buffer_size>{ old_value, new_value }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<string_expression, replace_expression<const char_type *, string_view_t, buffer_size>>
        replace(const char_type* old_value, const string_view_t new_value) const noexcept {
            return string_expression<string_expression,
                replace_expression<const char_type *, string_view_t, buffer_size>>{
                *this, replace_expression<const char_type *, string_view_t, buffer_size>{ old_value, new_value }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<string_expression, replace_expression<string_view_t, string_view_t, buffer_size>>
        replace(const string_view_t old_value, const string_view_t new_value) const noexcept {
            return string_expression<string_expression, replace_expression<string_view_t, string_view_t, buffer_size>>{
                *this, replace_expression<string_view_t, string_view_t, buffer_size>{ old_value, new_value }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<string_expression, remove_expression<string_view_t, buffer_size>>
        remove(const string_view_t target) const noexcept {
            return string_expression<string_expression, remove_expression<string_view_t, buffer_size>>{
                *this, remove_expression<string_view_t, buffer_size>{ target }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<string_expression, remove_expression<const char_type *, buffer_size>>
        remove(const char_type* target) const noexcept {
            return string_expression<string_expression, remove_expression<const char_type *, buffer_size>>{
                *this, remove_expression<const char_type *, buffer_size>{ target }
            };
        }

    private:
        Data M_data;
        Func M_func;

        friend class expression;

        template<typename, typename>
        friend class string_expression;
    };

    class expression {
    private:
        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        explicit expression(const basic_string& str) :
            M_str_ref(str) {}

        STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        void get_string(internal::MutString<char_type, char_traits>& str) const {
            auto view = M_str_ref.to_string_view();
            str = internal::MutString<char_type, char_traits>::get_string(view.data(), view.size());
        }

    public:
        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<expression, replace_expression<const char_type *, const char_type *, buffer_size>>
        replace(const char_type* old_value, const char_type* new_value) const noexcept {
            return string_expression<expression, replace_expression<const char_type *, const char_type *, buffer_size>>{
                *this, replace_expression<const char_type *, const char_type *, buffer_size>{ old_value, new_value }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<expression, replace_expression<string_view_t, const char_type *, buffer_size>>
        replace(const string_view_t old_value, const char_type* new_value) const noexcept {
            return string_expression<expression, replace_expression<string_view_t, const char_type *, buffer_size>>{
                *this, replace_expression<string_view_t, const char_type *, buffer_size>{ old_value, new_value }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<expression, replace_expression<const char_type *, string_view_t, buffer_size>>
        replace(const char_type* old_value, const string_view_t new_value) const noexcept {
            return string_expression<expression, replace_expression<const char_type *, string_view_t, buffer_size>>{
                *this, replace_expression<const char_type *, string_view_t, buffer_size>{ old_value, new_value }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<expression, replace_expression<string_view_t, string_view_t, buffer_size>>
        replace(const string_view_t old_value, const string_view_t new_value) const noexcept {
            return string_expression<expression, replace_expression<string_view_t, string_view_t, buffer_size>>{
                *this, replace_expression<string_view_t, string_view_t, buffer_size>{ old_value, new_value }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<expression, remove_expression<string_view_t, buffer_size>>
        remove(const string_view_t target) const noexcept {
            return string_expression<expression, remove_expression<string_view_t, buffer_size>>{
                *this, remove_expression<string_view_t, buffer_size>{ target }
            };
        }

        template<size_t buffer_size = 64>
        STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
        string_expression<expression, remove_expression<const char_type *, buffer_size>>
        remove(const char_type* target) const noexcept {
            return string_expression<expression, remove_expression<const char_type *, buffer_size>>{
                *this, remove_expression<const char_type *, buffer_size>{ target }
            };
        }

    private:
        const basic_string& M_str_ref;

        friend class basic_string;

        template<typename, typename>
        friend class string_expression;
    };

public:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    expression operator()() const & noexcept {
        return expression{ *this };
    }

    expression operator()() const && noexcept = delete;

#pragma endregion expression templates

#pragma region trims

private:
    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR T trim_helper(const char_type* const charset, size_t charset_length) const {
        STRIGNITE_IF_UNLIKELY(!charset)
            throw std::invalid_argument("string pointer cannot be nullptr.");

        auto view = to_string_view();

        STRIGNITE_IF_UNLIKELY(charset_length == 0) return { view.data(), view.size() };
        if (charset[1] == static_cast<char_type>(0)) return trim<T>(charset[0]);

        auto lp = view.begin();
        auto rp = view.end() - 1;
        for (; char_traits::find(charset, charset_length, *lp); ++lp) {}
        for (; char_traits::find(charset, charset_length, *rp); --rp) {}
        return { lp, static_cast<size_t>(rp + 1 - lp) };
    }

    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR T trim_start_helper(const char_type* charset, size_t charset_length) const {
        STRIGNITE_IF_UNLIKELY(!charset)
            throw std::invalid_argument("string pointer cannot be nullptr. ");

        auto view = to_string_view();
        if (view.size() == 0) return {};

        STRIGNITE_IF_UNLIKELY(charset_length == 0) return { view.data(), view.size() };
        if (charset[1] == static_cast<char_type>(0)) return trim_start<T>(charset[0]);

        auto lp = view.begin();
        for (; char_traits::find(charset, charset_length, *lp); ++lp) {}
        return { lp, view.end() - lp };
    }

    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR T trim_end_helper(const char_type* charset, const size_t charset_length) const {
        STRIGNITE_IF_UNLIKELY(!charset)
            throw std::invalid_argument("string pointer cannot be nullptr. ");

        auto view = to_string_view();
        if (view.size() == 0) return {};

        STRIGNITE_IF_UNLIKELY(charset_length == 0) return { view.data(), view.size() };
        if (charset[1] == static_cast<char_type>(0)) return trim_end<T>(charset[0]);

        auto rp = view.end() - 1;
        for (; char_traits::find(charset, charset_length, *rp); --rp) {}
        return { view.data(), static_cast<size_t>(rp + 1 - view.begin()) };
    }

public:
    /**
     * @brief 去除字符串两端指定字符并返回新字符串
     *
     * 该函数模板用于从当前字符串或视图的头部和尾部去除所有连续的指定字符，并返回处理后的新字符串/视图。
     *
     * @tparam T 返回类型模板参数，支持的类型包括：
     *           - basic_string (默认)
     *           - string_view_t
     *           - std::basic_string<char_type>
     *           - C++17及以上版本支持 std::basic_string_view<char_type>
     *
     * @param target 需要去除的目标字符。当该参数为0时（空字符），直接返回空容器
     *
     * @return 返回修剪后的新字符串/视图，具体类型由模板参数T决定。
     *         返回值包含以下情况：
     *         - 当原始数据为空字符串时返回空容器
     *         - 当target为0时返回空容器
     *         - 其他情况返回[lp, rp+1)区间的新字符串/视图
     *
     * @note 函数特性说明：
     * - 支持编译期操作（C++20 constexpr）
     * - 返回视图类型时零拷贝，其他类型执行深拷贝
     *
     * @note 特殊情形处理：
     * 1. 当字符串全部由target字符组成时，返回空字符串
     * 2. 修剪边界时严格匹配字符值，不考虑字符编码等特性
     * 3. 对于string_view类型返回值，需要注意视图的生命周期管理
     */
    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR T trim(char_type target) const {
        auto view = to_string_view();
        if (view.size() == 0 || target == static_cast<char_type>(0)) return {};
        auto lp = view.begin();
        auto rp = view.end() - 1;
        for (; *lp == target; ++lp) {}
        for (; *rp == target; --rp) {}
        return { lp, static_cast<size_t>(rp + 1 - lp) };
    }

    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR T trim_start(char_type target) const {
        auto view = to_string_view();
        if (view.size() == 0 || target == static_cast<char_type>(0)) return {};
        auto lp = view.begin();
        for (; *lp == target; ++lp) {}
        return { lp, view.end() - lp };
    }

    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR T trim_end(char_type target) const {
        auto view = to_string_view();
        if (view.size() == 0 || target == static_cast<char_type>(0)) return {};
        auto rp = view.end() - 1;
        for (; *rp == target; --rp) {}
        return { view.begin(), static_cast<size_t>(rp + 1 - view.begin()) };
    }

    /**
     * @brief 去除字符串两端空白字符并返回新字符串
     *
     * 本函数通过扫描字符串首尾的非空白字符位置，生成去除两端空白后的子串，
     * 支持返回多种字符串类型以满足不同场景需求。
     *
     * @tparam T 返回的字符串类型，允许以下类型：
     *           - basic_string（默认，独立内存副本）
     *           - string_view_t（原字符串视图）
     *           - std::basic_string<char_type>（标准字符串对象）
     *           - C++17及以上版本支持 std::basic_string_view<char_type>
     *
     * @return T
     *  - 去除两端空白后的字符串对象，可能为空字符串
     *  - 返回视图类型时直接引用原字符串数据
     *  - 全空白字符串返回空对象
     *
     * @note 关键特性：
     * - 使用bcs::is_space检测空白字符（包含空格/制表符/换行等）
     * - 支持编译期操作（C++20 constexpr）
     * - 返回视图类型时零拷贝，其他类型执行深拷贝
     * - 自动处理全空白字符串和空字符串
     *
     * @warning 注意事项：
     * - 返回视图类型时需确保原字符串生命周期
     * - 不修改原字符串内容
     * - 多字节空白字符（如中文全角空格）可能不被识别
     *
     * @example
     * auto s1 = str.trim(); // 返回basic_string
     * auto s2 = str.trim<string_view_t>(); // 返回视图
     * auto s3 = str.trim<std::string>(); // 返回标准字符串
     *
     * @see trim_start() 仅去除首部空白的方法
     * @see trim_end()   仅去除尾部空白的方法
     */
    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR T trim() const {
        auto view = to_string_view();
        if (view.size() == 0) return {};
        auto lp = view.begin();
        auto rp = view.end() - 1;
        for (; bcs::is_space(*lp); ++lp) {}
        for (; bcs::is_space(*rp); --rp) {}
        return { lp, static_cast<size_t>(rp + 1 - lp) };
    }

    /**
     * @brief 修剪字符串两端出现在指定字符集中的任意字符
     *
     * 该函数模板用于从当前字符串或视图的头部和尾部去除所有连续出现在字符集中的字符，返回处理后的新字符串/视图
     *
     * @tparam T 返回类型模板参数，默认为 basic_string<char_type> 类型
     *           - 实际支持的返回类型与单字符版本 trim() 函数一致
     *
     * @param charset 需要修剪的字符集合指针，必须满足：
     *                - 不可为 nullptr (触发异常)
     *                - 字符列表以空字符结尾的C风格字符串
     *
     * @return 返回修剪后的新字符串/视图，类型由模板参数T决定
     *
     * @throw std::invalid_argument 当 charset 参数为 nullptr 时抛出
     *
     * @note 函数特性说明：
     * - 支持编译期操作（C++20 constexpr）
     * - 返回视图类型时零拷贝，其他类型执行深拷贝
     */
    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim(const char_type* charset) const {
        return trim_helper<T>(charset, internal::traits_length<char_traits>(charset));
    }

    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim(const basic_string& charset) const {
        return trim_helper<T>(charset.data(), charset.size());
    }

    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim(const string_view_t charset) const {
        STRIGNITE_IF_UNLIKELY(!charset.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return trim_helper<T>(charset.data(), charset.size());
    }

    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim_start() const {
        auto view = to_string_view();
        if (view.size() == 0) return {};
        auto lp = view.begin();
        for (; bcs::is_space(*lp); ++lp) {}
        return { lp, static_cast<size_t>(view.end() - lp) };
    }

    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim_start(const char_type* charset) const {
        return trim_start_helper<T>(charset, internal::traits_length<char_traits>(charset));
    }

    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim_start(const string_view_t charset) const {
        STRIGNITE_IF_UNLIKELY(!charset.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return trim_start_helper<T>(charset.data(), charset.size());
    }

    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim_start(const basic_string& charset) const {
        return trim_start_helper<T>(charset.data(), charset.size());
    }

    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim_end() const noexcept {
        auto view = to_string_view();
        if (view.size() == 0) return {};
        auto rp = view.end() - 1;
        for (; bcs::is_space(*rp); --rp) {}
        return { view.begin(), static_cast<size_t>(rp + 1 - view.begin()) };
    }

    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim_end(const char_type* const charset) const {
        return trim_end_helper<T>(charset, internal::traits_length<char_traits>(charset));
    }

    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim_end(const string_view_t charset) const {
        STRIGNITE_IF_UNLIKELY(!charset.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return trim_end_helper<T>(charset.data(), charset.size());
    }

    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T trim_end(const basic_string& charset) const {
        return trim_end_helper<T>(charset.data(), charset.size());
    }

#pragma endregion trim

#pragma region substring

public: //substring
    /**
     * @brief 截取指定位置和长度的子字符串
     *
     * 本函数提供安全高效的子串截取能力，支持返回多种字符串类型，
     * 根据模板参数生成对应类型的子串对象。
     *
     * @tparam T 返回的字符串类型，允许以下类型：
     *           - basic_string（默认）
     *           - string_view_t
     *           - std::basic_string<char_type>
     *           - C++17起支持std::basic_string_view<char_type>
     *
     * @param[in] pos    起始位置索引，需满足：
     *                  - 0 <= pos <= 原字符串长度
     *                  - 超过字符串长度将抛出std::out_of_range
     * @param[in] length 要截取的长度，实际可能被限制为：
     *                  - 可用长度 = min(参数length, 原字符串长度 - pos)
     *
     * @return T
     *  - 包含请求子串的新字符串对象
     *  - pos==0且length==原长度时返回原字符串副本
     *  - length==0时返回空字符串对象
     *
     * @note 关键特性：
     * - 基于字符串视图实现，无额外内存分配
     * - 支持编译期截取操作（C++20 constexpr）
     * - 强制内联优化执行路径
     * - 自动参数校验（通过底层string_view实现）
     *
     * @warning 注意事项：
     * - 返回视图类型时需确保原字符串生命周期
     * - 参数length超过实际可用长度时自动截断
     *
     * @example
     * auto sv = str.substring<string_view_t>(2, 5); // 获取视图
     * auto s = str.substring<std::string>(0, 3);    // 获取标准字符串
     */
    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T substring(size_t pos, size_t length) const {
        auto res = to_string_view().substring(pos, length);
        return { res.data(), res.length() };
    }

    /**
     * @brief 截取从指定位置到末尾的子字符串
     *
     * 本函数提供从指定位置到字符串末尾的安全截取功能，支持返回多种字符串类型，
     * 根据模板参数生成对应类型的子串对象或视图。
     *
     * @tparam T 返回的字符串类型，允许以下类型：
     *           - basic_string（默认，深拷贝）
     *           - string_view_t（轻量视图）
     *           - std::basic_string<char_type>（标准字符串）
     *           - C++17起支持std::basic_string_view<char_type>（标准视图）
     *
     * @param[in] pos 起始位置索引，需满足：
     *               - 0 <= pos <= 原字符串长度
     *               - 超过字符串长度将抛出std::out_of_range
     *
     * @return T
     *  - 包含从pos到末尾的子串对象
     *  - pos==0时返回完整字符串副本/视图
     *  - pos等于字符串长度时返回空对象
     *
     * @note 关键特性：
     * - 基于字符串视图实现，无额外内存分配（视图类型）
     * - 支持编译期截取操作（C++20 constexpr）
     * - 自动参数校验（通过底层string_view实现）
     * - 强制内联优化执行路径
     *
     * @warning 注意事项：
     * - 返回视图类型时需确保原字符串的生命周期
     * - 参数pos超过字符串长度将触发异常
     *
     * @example
     * auto full = str.substring(0);       // 获取完整副本
     * auto view = str.substring<string_view_t>(3); // 获取从索引3开始的视图
     * auto stdsv = str.substring<std::string_view>(2); // C++17标准视图
     *
     * @see substring(size_t, size_t) 指定长度版本的子串截取
     */
    template<typename T = basic_string,
        typename = internal::enable_if_t<
            STRIGNITE_IS_SAME_V(T, basic_string)
            || STRIGNITE_IS_SAME_V(T, string_view_t)
            || STRIGNITE_IS_SAME_V(T, std::basic_string<char_type>)
#if STRIGNITE_CPLUSPLUS_17
            || STRIGNITE_IS_SAME_V(T, std::basic_string_view<char_type>)
#endif
        >>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T substring(const size_t pos) const {
        auto res = to_string_view().substring(pos);
        return { res.data(), res.length() };
    }

    /**
     * @brief 获取字符串左侧指定长度的子串
     *
     * 本函数提供高效的前缀截取功能，返回从字符串起始位置开始的指定长度子串，
     * 支持多种返回类型以满足不同场景需求。
     *
     * @tparam T 返回的字符串类型，允许以下类型：
     *           - basic_string（默认，独立内存副本）
     *           - string_view_t（零拷贝视图）
     *           - std::basic_string<char_type>（标准字符串对象）
     *           - C++17起支持std::basic_string_view<char_type>（标准库视图）
     *
     * @param[in] length 要截取的长度，实际处理为：
     *                  - 当length超过字符串长度时，返回整个字符串的副本/视图
     *                  - length为0时返回空字符串对象
     *
     * @return T
     *  - 包含前length个字符的子串对象
     *  - 当length>=原字符串长度时返回完整副本/视图
     *  - length为0时返回空对象
     *
     * @note 关键特性：
     * - 基于substring(0, length)实现，行为一致
     * - 支持编译期常量表达式操作（C++20 constexpr）
     * - 强制内联优化执行路径
     * - 自动长度校正，避免越界访问
     *
     * @warning 注意事项：
     * - 返回视图类型时需确保原字符串生命周期足够长
     * - 大length值不会引发异常，自动截断为最大可用长度
     * - 参数为无符号类型，传递负值会导致意外行为
     *
     * @example
     * auto prefix = str.left(5); // 获取前5个字符的basic_string
     * auto view = str.left<string_view_t>(3); // 获取前3字符的视图
     * auto stdstr = str.left<std::string>(10); // 获取前10字符的标准字符串
     *
     * @see substring() 更通用的子串截取方法
     * @see right() 对应的右侧截取方法
     */
    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T left(const size_t length) const {
        return substring<T>(0, length);
    }

    /**
     * @brief 获取字符串右侧指定长度的子串
     *
     * 本函数提供高效的后缀截取功能，返回字符串末尾指定长度的子串，
     * 支持多种返回类型以适应不同使用场景。
     *
     * @tparam T 返回的字符串类型，允许以下类型：
     *           - basic_string（默认，独立内存副本）
     *           - string_view_t（零拷贝视图）
     *           - std::basic_string<char_type>（标准字符串对象）
     *           - C++17起支持std::basic_string_view<char_type>（标准库视图）
     *
     * @param[in] length 要截取的长度，实际处理为：
     *                  - 当length超过字符串长度时，返回整个字符串的副本/视图
     *                  - length为0时返回空字符串对象
     *
     * @return T
     *  - 包含后length个字符的子串对象
     *  - 当length>=原字符串长度时返回完整副本/视图
     *  - length为0时返回空对象
     *
     * @note 关键特性：
     * - 基于substring(size() - length)实现，自动计算起始位置
     * - 支持编译期常量表达式操作（C++20 constexpr）
     * - 强制内联优化执行路径
     * - 自动长度校正，避免负向索引计算
     *
     * @warning 注意事项：
     * - 返回视图类型时需确保原字符串生命周期足够长
     * - 参数length超过字符串长度不会引发异常，自动调整为最大可用长度
     * - 对空字符串调用始终返回空对象
     *
     * @example
     * auto suffix = str.right(3); // 获取最后3个字符的basic_string
     * auto view = str.right<string_view_t>(5); // 获取末尾5字符的视图
     * auto stdstr = str.right<std::string>(10); // 获取最后10字符的标准字符串
     *
     * @see substring() 更通用的子串截取方法
     * @see left() 对应的左侧截取方法
     */
    template<typename T = basic_string>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    T right(const size_t length) const {
        return substring<T>(size() - length);
    }

#pragma endregion substring

#pragma region insert

private:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_string insert(size_t pos, char_type value) const {
        assert(value != static_cast<char_type>(0));

        auto view = to_string_view();

        STRIGNITE_IF_UNLIKELY(pos > view.size())
            throw std::out_of_range("bcs::basic_string::insert");

        const auto new_length = view.size() + 1;

        auto result = M_reserve(new_length);
        char_type* ptr = result.M_pointer();

        char_traits::copy(ptr, view.data(), pos);
        ptr[pos] = value;
        char_traits::copy(ptr + pos + 1, view.data() + pos, view.size() - pos);

        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))

        return result;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_string insert_helper(size_t pos, const string_view_t value) const {
        STRIGNITE_IF_UNLIKELY(value.empty()) return *this;
        if (value.size() == 1) return insert(pos, value[0]);

        auto view = to_string_view();

        if (pos > view.size())
            throw std::out_of_range("bcs::basic_string::insert");

        size_t value_length = value.size();
        const auto new_length = view.size() + value_length;

        auto result = M_reserve(new_length);
        char_type* ptr = result.M_pointer();

        char_traits::copy(ptr, view.data(), pos);
        char_traits::copy(ptr + pos, value.data(), value_length);
        char_traits::copy(ptr + pos + value_length, view.data() + pos, view.size() - pos);

        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))

        return result;
    }

public:
    /**
     * @brief 在指定位置插入字符串内容并生成新字符串
     *
     * 本函数通过三阶段内存拷贝操作，在指定位置插入目标字符串视图内容，
     * 生成并返回包含插入结果的新字符串对象，原字符串保持不变。
     *
     * @param[in] pos   插入位置索引，需满足：
     *                 - 0 <= pos <= 原字符串长度
     *                 - 超过原长度将抛出异常
     * @param[in] value 要插入的字符串视图，允许空视图：
     *                 - 空视图直接返回原字符串副本
     *                 - 单字符视图调用优化版本
     *
     * @return basic_string 包含插入结果的新字符串对象
     *
     * @throw std::out_of_range 当pos超过原字符串长度时抛出
     *
     * @note 实现特性：
     * - 空插入内容快速返回优化
     * - 单字符插入调用专用重载函数
     * - 预计算目标长度并精确分配内存
     * - 支持编译期常量表达式操作（C++20）
     * - 分三次内存拷贝保证数据完整性
     *
     * @warning 注意事项：
     * - 插入位置以字符单位计算，多字节编码需自行处理
     * - 返回对象的哈希值在编译期可能预计算
     *
     * @example
     * string s = "hello";
     * auto s1 = s.insert(3, "123"); // 返回"hel123lo"
     *
     * @see insert(size_t, char_type) 单字符插入的优化版本
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string insert(const size_t pos, const string_view_t value) const {
        STRIGNITE_IF_UNLIKELY(!value.data())
            throw std::runtime_error("string pointer cannot be nullptr.");
        return insert_helper(pos, value);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string insert(const size_t pos, const char_type* const value) const {
        return insert_helper(pos, string_view_t{ value });
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string insert(const size_t pos, const basic_string& value) const {
        return insert_helper(pos, value.to_string_view());
    }

#pragma endregion insert

public:
    /**
     * @brief 生成当前字符串重复多次拼接后的新字符串
     *
     * 本函数通过高效的内存预分配和批量拷贝操作，生成指定重复次数的新字符串。
     * 采用循环复制策略保证最优性能，支持编译期常量表达式计算。
     *
     * @param[in] times 重复次数，允许为0的特殊情况：
     *                 - times=0 时返回空字符串
     *                 - times=1 时返回原字符串副本
     *
     * @return basic_string 包含重复结果的新字符串对象，特性包括：
     *                   - 容量精确匹配结果长度（无多余分配）
     *                   - 保持原字符串字符顺序
     *                   - 编译期计算时预生成哈希值
     *
     * @note 实现特性：
     * - 预计算目标长度并一次性分配内存
     * - 使用char_traits::copy进行高效内存拷贝
     * - 支持编译期字符串重复构建（C++20 constexpr）
     * - 严格noexcept保证（假设内存分配成功）
     *
     * @warning 注意事项：
     * - 大次数值（如1e6）可能导致内存耗尽
     * - 原字符串为空时直接返回空字符串（无视times参数）
     * - 时间复杂度为O(n)，n为重复次数
     *
     * @example
     * string s = "ab";
     * auto s3 = s.repeat(3); // 返回"ababab"
     * auto s0 = s.repeat(0); // 返回空字符串
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_string repeat(const size_t times) const noexcept {
        auto view = to_string_view();
        const auto new_length = view.size() * times;
        auto result = M_reserve(new_length);
        char_type* ptr = result.M_pointer();
        for (size_t i = 0; i < times; ++i) {
            char_traits::copy(ptr, view.data(), view.size());
            ptr += view.size();
        }
        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))
        return result;
    }

    /**
     * @brief 生成当前字符串的反转副本
     *
     * 本函数通过内部反转算法创建原字符串的逆序副本，保持原字符串内容不变。
     * 适用于需要逆序文本处理的场景，支持编译期常量字符串反转操作。
     *
     * @return basic_string
     *  - 包含原字符串字符逆序排列的新字符串对象
     *  - 空字符串输入返回空字符串对象
     *  - 单字符字符串返回自身副本
     *
     * @note 实现特性：
     * - 预分配精确长度的内存空间
     * - 支持编译期常量表达式反转（C++20）
     * - 强制内联优化执行路径
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 多字节编码字符（如UTF-8）可能被逐字节反转导致乱码
     * - 返回新字符串的内存布局独立于原字符串
     * - 大字符串（>1MB）可能影响性能
     *
     * @example
     * string s = "hello";
     * auto rev = s.reverse(); // 返回"olleh"
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string reverse() const noexcept {
        auto view = to_string_view();
        basic_string result = M_reserve(view.size());
        internal::reverse_helper(view, result.M_pointer());
        return result;
    }

#pragma region starts ends with

    /**
     * @brief 检查当前字符串是否以目标字符串视图结尾（精确匹配）
     *
     * 通过二进制级精确比较方式判断当前字符串是否以指定的字符串视图作为后缀，
     * 并对输入参数进行空指针安全检查。
     *
     * @param target 要匹配的字符串视图对象，必须满足：
     *               - data()返回值不可为nullptr（否则触发异常）
     *               - 视图范围[size()]内内存合法可访问
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标视图内容结尾（包括空视图情况）
     *              - false: 目标视图长度超过当前字符串或后缀不匹配
     *
     * @throw std::invalid_argument 当target.data()返回空指针时抛出
     *
     * @note 核心特性：
     * - 支持编译时后缀检查
     * - 强制内联提升关键路径性能
     *
     * @warning 重要注意事项：
     * 1. 生命周期管理：
     *    - 不持有目标视图数据所有权，调用者需确保视图有效性
     *    - 视图可能引用临时对象时需注意悬挂指针风险
     * 2. 匹配规则：
     *    - 严格区分大小写（如"END"与"end"视为不同）
     *    - 完全匹配字节序列（含多字节编码字符）
     *
     * @example
     * basic_string str("SecureContainer");
     * string_view_t sv("Container");
     * str.ends_with(sv);  // 返回true（精确匹配）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const string_view_t target) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        return to_string_view().ends_with(target);
    }

    /**
     * @brief 检查当前字符串是否以目标字符串结尾（精确匹配）
     *
     * 通过二进制级精确比较方式判断当前字符串是否以指定的字符串对象作为后缀，
     * 该比较严格区分字符大小写、字符编码及存储顺序。
     *
     * @param target 要匹配的后缀字符串对象，必须满足：
     *               - 有效的basic_string实例
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标内容结尾（包含目标为空字符串的特殊情况）
     *              - false: 目标长度超过当前字符串长度 或 字符序列不匹配
     *
     * @note 核心特性：
     * - 支持C++20编译时字符串后缀验证
     * - 关键路径强制内联提升高频调用性能
     *
     * @warning 重要注意事项：
     * 1. 匹配规则：
     *    - 区分全半角字符（如"A"与"Ａ"视为不同）
     *    - 大小写敏感（"END"与"end"视为不同）
     *
     * @example
     * basic_string s1("RuntimeOptimization");
     * basic_string s2("ization");
     * s1.ends_with(s2);  // 返回false（精确匹配需完整"Optimization"）
     * basic_string s3("Optimization");
     * s1.ends_with(s3);  // 返回true
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const basic_string& target) const {
        return to_string_view().ends_with(target.to_string_view());
    }

    /**
     * @brief 检查当前字符串是否以指定C字符串结尾（精确匹配）
     *
     * 通过二进制精确比较方式判断当前字符串是否以给定的C风格字符串作为后缀，
     * 该比较严格区分字符大小写和编码格式。
     *
     * @param target 要匹配的C风格字符串指针，必须满足：
     *               - 有效的以空字符('\0')结尾的字符串
     *               - 禁止传入nullptr（未显式检查，需调用者保证有效性）
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标内容结尾（包括目标为空字符串的情况）
     *              - false: 目标长度超过当前字符串或后缀不匹配
     *
     * @note 特性说明：
     * - 支持编译时后缀检查
     * - 强制内联优化高频调用
     *
     * @warning 注意事项：
     * 1. 比较过程严格遵循二进制匹配规则：
     *    - 区分字符大小写（如'x'与'X'视为不同）
     *    - 严格校验字符编码和存储顺序
     * 2. 参数必须为合法C字符串，传入nullptr将导致未定义行为
     *
     * @example
     * basic_string str("Documentation");
     * str.ends_with("tion");  // 返回true
     * str.ends_with("TION");  // 返回false（大小写敏感）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const char_type* const target) const {
        return to_string_view().ends_with(string_view_t{ target });
    }

    /**
     * @brief 检查当前字符串是否以目标字符串视图结尾（可选忽略大小写）
     *
     * 判断当前字符串是否以指定的字符串视图内容作为后缀，支持精确匹配或忽略大小写匹配，
     * 并对空指针参数进行显式安全检查
     *
     * @param target       要匹配的字符串视图对象，必须满足：
     *                     - data()返回值不可为nullptr（否则触发异常）
     *                     - 视图范围[size()]内内存合法有效
     * @param ignore_case  匹配模式选择标志：
     *                     - true:  不区分大小写匹配（双方转为小写比较）
     *                     - false: 精确二进制匹配
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标视图内容结尾（包括空视图情况）
     *              - false: 目标视图长度超过当前字符串或后缀不匹配
     *
     * @throw std::invalid_argument 当target.data()返回nullptr时抛出
     *
     * @note 特性说明：
     * - 支持编译时后缀检查
     * - 强制内联优化关键路径性能
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 生成两个临时字符串对象进行转换，内存开销为O(n+m)
     *    - 使用basic_string默认构造器，可能影响分配器状态
     *    - 连续调用可能导致内存碎片
     *
     * @example
     * basic_string str("DataStorage");
     * string_view_t sv("storage");
     * str.ends_with(sv, true);   // 返回true（忽略大小写）
     * str.ends_with(sv, false);  // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const string_view_t target, const bool ignore_case) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        if (ignore_case) {
            if (target.length() > size()) return false;
            return to_lower().ends_with(basic_string{ target }.to_lower());
        }
        return ends_with(target);
    }

    /**
     * @brief 检查当前字符串是否以目标字符串结尾（可选忽略大小写）
     *
     * 判断当前字符串是否以指定的字符串对象作为后缀，支持区分大小写/不区分大小写两种匹配模式，
     * 并自动处理目标长度超过当前字符串的边界情况
     *
     * @param target       要匹配的后缀字符串对象，必须满足：
     *                     - 有效的basic_string实例（允许空字符串）
     *                     - 生命周期在当前操作期间有效
     * @param ignore_case  匹配模式选择标志：
     *                     - true:  不区分大小写匹配（双方转为小写比较）
     *                     - false: 精确二进制匹配
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标内容结尾（包括目标为空字符串的情况）
     *              - false: 目标长度超过当前字符串或后缀不匹配
     *
     * @note 特性说明：
     * - 支持编译时后缀检查
     * - 强制内联优化高频调用路径
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 生成两个临时小写字符串对象，内存开销为O(n+m)
     *
     * @example
     * basic_string str("RuntimeLibrary");
     * str.ends_with(basic_string("library"), true);  // 返回true
     * str.ends_with(basic_string("LIB"), false);     // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const basic_string& target, const bool ignore_case) const {
        if (ignore_case) {
            if (target.length() > size()) return false;
            return to_lower().ends_with(target.to_lower());
        }
        return ends_with(target);
    }

    /**
     * @brief 检查当前字符串是否以指定C字符串结尾（可选忽略大小写）
     *
     * 判断当前字符串是否以给定的C风格字符串作为后缀，支持区分大小写/不区分大小写两种匹配模式
     *
     * @param target       要匹配的C风格字符串指针，必须满足：
     *                     - 有效的以空字符('\0')结尾的字符串
     *                     - 禁止传入nullptr（未显式检查，需调用者保证有效性）
     * @param ignore_case  匹配模式选择标志：
     *                     - true:  不区分大小写匹配（双方转为小写比较）
     *                     - false: 精确二进制匹配
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标字符串结尾（包括目标为空字符串的情况）
     *              - false: 目标长度超过当前字符串或后缀不匹配
     *
     * @note 特性说明：
     * - 支持编译时后缀检查
     * - 强制内联优化高频调用场景
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 生成临时basic_string对象并转换大小写，内存开销为O(n+m)
     *    - 使用默认构造的basic_string实例，可能影响分配器行为
     * 2. 传入nullptr将导致未定义行为
     *
     * @example
     * basic_string str("ImageProcessor");
     * str.ends_with("sor", true);    // 返回true（忽略大小写）
     * str.ends_with("Processor", false);  // 返回true（精确匹配）
     * str.ends_with("ER", false);    // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool ends_with(const char_type* const target, const bool ignore_case) const {
        if (ignore_case) {
            return to_lower().ends_with(basic_string{ target }.to_lower());
        }
        return ends_with(target);
    }

    /**
     * @brief 检查当前字符串是否以目标字符串视图开头（精确匹配）
     *
     * 通过二进制精确比较方式判断当前字符串是否以指定的字符串视图内容作为前缀，
     * 并对空指针参数进行显式安全检查
     *
     * @param target 要匹配的字符串视图对象，必须满足：
     *               - data()返回值不可为nullptr（否则触发异常）
     *               - 视图范围[size()]内内存合法可访问
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标视图内容开头（包括空视图情况）
     *              - false: 目标视图长度超过当前字符串或前缀不匹配
     *
     * @throw std::invalid_argument 当target.data()返回空指针时抛出
     *
     * @note 特性说明：
     * - 支持编译时前缀检查
     * - 强制内联优化关键路径性能
     * - 显式空指针校验确保操作安全性
     *
     * @warning 注意事项：
     * 比较过程严格遵循二进制匹配规则：
     *    - 区分字符大小写（如'A'与'a'视为不同）
     *    - 严格匹配字符编码和存储顺序
     *
     * @example
     * basic_string str("SecureChannel");
     * string_view_t sv("Secure");
     * str.starts_with(sv);  // 返回true
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const string_view_t target) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        return to_string_view().starts_with(target);
    }

    /**
     * @brief 检查当前字符串是否以指定C字符串开头（精确匹配）
     *
     * 通过二进制精确比较方式判断当前字符串是否以给定的C风格字符串作为前缀，
     * 该比较严格区分字符大小写和编码格式。
     *
     * @param target 要匹配的C风格字符串指针，必须满足：
     *               - 有效的以空字符('\0')结尾的字符串
     *               - 禁止传入nullptr（未显式检查，需调用者保证有效性）
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标内容开头（包括目标为空字符串的情况）
     *              - false: 目标长度超过当前字符串或前缀不匹配
     *
     * @note 特性说明：
     * - 支持编译时前缀检查
     * - 强制内联优化高频调用
     *
     * @warning 注意事项：
     * 1. 比较过程严格区分大小写（如'A'与'a'视为不同）
     * 2. 参数必须为合法C字符串，传入nullptr将导致未定义行为
     *
     * @example
     * basic_string str("NetworkProtocol");
     * str.starts_with("Network");  // 返回true
     * str.starts_with("net");      // 返回false（大小写敏感）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const char_type* const target) const {
        return to_string_view().starts_with(string_view_t{ target });
    }

    /**
     * @brief 检查当前字符串是否以目标字符串开头（精确匹配）
     *
     * 通过二进制精确比较方式判断当前字符串是否以指定的目标字符串作为前缀，
     * 该比较严格区分字符大小写和编码格式。
     *
     * @param target 要匹配的前缀字符串对象，必须满足：
     *               - 有效的basic_string实例（允许空字符串）
     *               - 生命周期在当前操作期间有效
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标内容开头（包括目标为空字符串的情况）
     *              - false: 目标长度超过当前字符串或前缀不匹配
     *
     * @note 特性说明：
     * - 支持编译时前缀检查
     * - 强制内联优化高频调用
     *
     * @warning 注意事项：
     * 1. 比较过程严格遵循二进制匹配，包括：
     *    - 区分字符大小写（'A' != 'a'）
     *    - 严格校验字符编码格式
     *    - 完全匹配字符序列顺序
     *
     * @example
     * basic_string str("Database");
     * str.starts_with(basic_string("Data"));  // 返回true
     * str.starts_with(basic_string("data"));  // 返回false（大小写敏感）
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const basic_string& target) const {
        return to_string_view().starts_with(target.to_string_view());
    }

    /**
     * @brief 检查当前字符串是否以目标字符串视图开头（可选忽略大小写）
     *
     * 判断当前字符串是否以指定的字符串视图内容作为前缀，支持精确匹配或忽略大小写匹配，
     * 并对空指针参数进行安全性检查
     *
     * @param target       要匹配的字符串视图对象，必须满足：
     *                     - data()返回值不可为nullptr（否则触发异常）
     *                     - 视图范围[size()]内内存合法有效
     * @param ignore_case  匹配模式选择标志：
     *                     - true:  不区分大小写匹配（双方转为小写比较）
     *                     - false: 精确二进制匹配
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标视图内容开头（包括空视图情况）
     *              - false: 目标视图长度超过当前字符串或前缀不匹配
     *
     * @throw std::invalid_argument 当target.data()返回nullptr时抛出
     *
     * @note 特性说明：
     * - 支持编译时前缀检查
     * - 强制内联优化高频调用路径
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 生成两个临时字符串对象进行转换，内存开销为O(n+m)
     *    - 使用basic_string的默认构造器，可能影响分配器状态
     *    - 连续调用可能导致内存碎片
     * 3. 视图生命周期需独立维护，操作不持有视图数据
     *
     * @example
     * basic_string str("DocumentView");
     * string_view_t sv("doc");
     * str.starts_with(sv, true);  // 返回true
     * str.starts_with(sv, false); // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const string_view_t target, const bool ignore_case) const {
        STRIGNITE_IF_UNLIKELY(!target.data())
            throw std::invalid_argument("string pointer cannot be nullptr.");
        if (ignore_case) {
            if (target.length() > size()) return false;
            return to_lower().starts_with(basic_string{ target }.to_lower());
        }
        return starts_with(target);
    }

    /**
     * @brief 检查当前字符串是否以指定C字符串开头（可选忽略大小写）
     *
     * 判断当前字符串是否以给定的C风格字符串作为前缀，支持区分大小写/不区分大小写两种匹配模式
     *
     * @param target       要匹配的C风格字符串指针，必须满足：
     *                     - 有效的以空字符('\0')结尾的字符串
     *                     - 禁止传入nullptr（未显式检查，需调用者保证有效性）
     * @param ignore_case  匹配模式选择标志：
     *                     - true:  不区分大小写匹配（双方转为小写比较）
     *                     - false: 精确二进制匹配
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标字符串开头（包括目标为空字符串的情况）
     *              - false: 目标长度超过当前字符串或前缀不匹配
     *
     * @note 特性说明：
     * - 支持编译时前缀检查
     * - 强制内联优化高频调用场景
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 生成临时basic_string对象并转换大小写，内存开销为O(n+m)
     *    - 使用默认构造的basic_string实例，可能影响分配器行为
     * 2. 传入nullptr将导致未定义行为
     *
     * @example
     * basic_string str("FileSystem");
     * str.starts_with("file", true);   // 返回true（忽略大小写）
     * str.starts_with("File", false); // 返回true（精确匹配）
     * str.starts_with("Sys", false);  // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const char_type* const target, const bool ignore_case) const {
        if (ignore_case) {
            return to_lower().starts_with(basic_string{ target }.to_lower());
        }
        return starts_with(target);
    }

    /**
     * @brief 检查当前字符串是否以目标字符串开头（可选忽略大小写）
     *
     * 判断当前字符串是否以指定的目标字符串作为前缀，支持区分大小写/不区分大小写两种匹配模式
     *
     * @param target       要匹配的前缀字符串对象，必须满足：
     *                     - 有效的basic_string实例
     *                     - 空字符串将始终返回true
     * @param ignore_case  匹配模式选择标志：
     *                     - true:  不区分大小写匹配
     *                     - false: 精确二进制匹配
     *
     * @return bool 返回匹配结果：
     *              - true:  当前字符串以目标字符串开头
     *              - false: 目标长度超过当前字符串长度或前缀不匹配
     *
     * @note 特性说明：
     * - 支持编译时前缀检查
     * - 强制内联优化高频调用场景
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 生成两个临时小写字符串对象，内存开销为O(n+m)
     *    - 连续调用可能影响性能
     *
     * @example
     * basic_string str("HelloWorld");
     * str.starts_with("hello", true);   // 返回true
     * str.starts_with("HELLO", false); // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool starts_with(const basic_string& target, const bool ignore_case) const {
        if (ignore_case) {
            if (target.length() > size()) return false;
            return to_lower().starts_with(target.to_lower());
        }
        return starts_with(target);
    }

#pragma endregion starts ends with

#pragma region equals

public:
    /**
     * @brief 比较当前字符串与字符串视图的精确相等性
     *
     * 通过精确二进制匹配方式比较当前字符串对象与目标字符串视图的内容是否完全一致，
     * 直接调用底层operator==操作符实现核心比较逻辑。
     *
     * @param other 要比较的字符串视图对象，必须满足：
     *              - 视图指向有效内存区域
     *              - 视图范围[size()]内内存可安全访问
     *
     * @return bool 返回比较结果：
     *              - true:  当前字符串内容与视图字符序列完全一致
     *              - false: 长度不同/内容不匹配/视图数据无效
     *
     * @note 特性说明：
     * - 支持编译时字符串比较
     * - 强制内联优化关键路径性能
     * - 保证操作不抛出异常
     *
     * @warning 注意事项：
     * 2. 即使视图size()为0，仍需保证data()有效性
     * 3. 不持有视图数据所有权，比较时视图必须有效
     *
     * @example
     * basic_string<char> str("Hello");
     * string_view_t sv("Hello");
     * str.equals(sv); // 返回true
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const string_view_t other) const noexcept {
        return *this == other;
    }

    /**
     * @brief 比较当前字符串与另一字符串对象的精确相等性
     *
     * 通过精确二进制匹配方式比较当前字符串对象与目标字符串对象的内容是否完全一致，
     * 直接调用底层operator==操作符实现核心比较逻辑。
     *
     * @param other 要比较的目标字符串对象，必须满足：
     *              - 有效的basic_string实例
     *              - 必须完成初始化且处于有效状态
     *
     * @return bool 返回比较结果：
     *              - true:  两个对象存储的字符序列完全一致
     *              - false: 内容长度不同/字符序列不同/参数对象无效
     *
     * @note 特性说明：
     * - 支持C++20编译时字符串比较
     * - 强制编译器进行内联优化
     * - noexcept 保证操作不抛出异常（依赖operator==的实现安全性）
     *
     * @warning 注意事项：
     * 1. 比较过程严格区分大小写和编码格式
     * 3. 比较不涉及容量(capacity)等元信息，仅对比实际内容
     *
     * @example
     * basic_string<char> s1("Hello"), s2("Hello"), s3("hello");
     * s1.equals(s2); // 返回true
     * s1.equals(s3); // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const basic_string& other) const noexcept {
        return *this == other;
    }

    /**
     * @brief 比较当前字符串与C风格字符串的精确相等性
     *
     * 使用精确字符匹配方式比较当前字符串与指定的以空字符结尾的C风格字符串内容是否完全一致
     *
     * @param other 要比较的C风格字符串指针，必须满足：
     *              - 有效的以空字符('\0')结尾的字符数组
     *              - 禁止传入nullptr(未显式检查，需调用者保证)
     *
     * @return bool 返回比较结果：
     *              - true:  当前字符串与参数内容二进制级别完全一致
     *              - false: 内容不匹配或参数指针无效
     *
     * @note 特性说明：
     * - 支持C++20编译时字符串比较
     * - 强制编译器进行内联优化
     * - 保证操作不抛出异常
     *
     * @warning 注意事项：
     * 1. 直接调用operator==实现，比较过程严格区分大小写
     * 2. 参数必须为合法C字符串，传入无效指针将导致未定义行为
     *
     * @example
     * basic_string<char> str("Hello");
     * str.equals("Hello");  // 返回true
     * str.equals("hello");  // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const char_type* const other) const noexcept {
        return *this == other;
    }

    /**
     * @brief 比较当前字符串与字符串视图的相等性（可选忽略大小写）
     *
     * 执行当前字符串对象与字符串视图的内容比较，支持大小写敏感/不敏感两种模式
     *
     * @param other        要比较的字符串视图对象，必须满足：
     *                     - 视图指向有效内存区域（data()非空）
     *                     - 长度由size()确定（允许包含空字符）
     * @param ignore_case  是否启用不区分大小写比较模式：
     *                     - true:  双方转换为小写形式后比较
     *                     - false: 直接进行二进制精确比较
     *
     * @return bool 返回比较结果：
     *              - true:  内容语义相等（根据模式可能忽略大小写）
     *              - false: 内容不匹配或视图无效
     *
     * @note 特性说明：
     * - 支持编译时比较操作
     * - 强制内联提升关键路径性能
     * - 保证不抛出异常（依赖底层转换的安全性）
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 将字符串视图转换为临时basic_string对象，产生O(n)内存开销
     *    - 转换使用默认构造的basic_string实例，可能影响分配器行为
     * 2. 即使other.size()为0，仍需保证other.data()有效性
     *
     * @example
     * basic_string str("Hello");
     * string_view_t sv("HELLO");
     * str.equals(sv, true);   // 返回true
     * str.equals(sv, false);  // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const string_view_t other, const bool ignore_case) const noexcept {
        return ignore_case ? to_lower() == basic_string{ other }.to_lower().to_string_view() : *this == other;
    }

    /**
     * @brief 比较当前字符串与另一字符串对象的相等性（可选忽略大小写）
     *
     * 执行字符串对象间的相等性比较，可选择是否忽略字符大小写差异进行内容匹配
     *
     * @param other        要比较的另一个字符串对象，必须满足：
     *                     - 必须为有效的字符串实例
     *                     - 允许接受临时对象/右值引用
     * @param ignore_case  是否启用不区分大小写比较模式：
     *                     - true:  比较前双方转为小写形式
     *                     - false: 直接调用operator==进行精确比较
     *
     * @return bool 返回比较结果：
     *              - true:  内容相等（根据模式可能忽略大小写）
     *              - false: 内容不相等或对象状态异常
     *
     * @note 特性说明：
     * - 支持编译时字符串比较
     * - 强制内联优化提升性能
     * - 保证操作不抛出异常（依赖to_lower()的实现安全性）
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 双方均生成临时小写字符串对象，内存开销为O(n)
     *    - 使用char_traits定义的大小写转换规则
     *    - 连续调用可能影响缓存效率
     * 2. 比较时严格遵循对象内容，不考虑字符串存储格式差异
     *
     * @example
     * basic_string str1("Hello"), str2("HELLO");
     * str1.equals(str2, true);   // 返回true
     * str1.equals(str2, false);  // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const basic_string& other, const bool ignore_case) const noexcept {
        return ignore_case ? to_lower() == other.to_lower() : *this == other;
    }

    /**
     * @brief 比较当前字符串与C风格字符串的相等性（可选忽略大小写）
     *
     * 执行字符串相等性比较操作，可选择是否忽略字符大小写差异进行内容匹配
     *
     * @param other        要比较的C风格字符串指针，必须满足：
     *                     - 有效的以空字符结尾的字符串
     *                     - 禁止传入nullptr（未显式检查，需调用者保证）
     * @param ignore_case  是否启用不区分大小写比较模式：
     *                     - true: 比较前双方转为小写形式
     *                     - false: 直接进行二进制精确比较
     *
     * @return bool 返回比较结果：
     *              - true:  内容相等（根据模式可能忽略大小写）
     *              - false: 内容不相等
     *
     * @note 特性说明：
     * - 支持C++20编译时比较
     * - 强制内联优化
     * - 保证不抛出异常
     *
     * @warning 注意事项：
     * 1. 当ignore_case为true时：
     *    - 生成临时字符串对象，可能影响性能
     *    - 使用默认本地化规则进行大小写转换
     *    - 不支持unicode特殊字符的语义化比较
     * 2. 调用者需确保other指针有效性，传入nullptr将导致未定义行为
     *
     * @example
     * basic_string str("Hello");
     * str.equals("HELLO", true);  // 返回true
     * str.equals("hello", false); // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool equals(const char_type* const other, const bool ignore_case) const noexcept {
        return ignore_case ? to_lower() == basic_string{ other }.to_lower() : *this == other;
    }

#pragma endregion equals

public:
    /**
     * @brief 获取当前字符串的字符长度（逻辑长度）
     *
     * 本函数提供与标准库字符串兼容的接口，返回字符串的实际字符数量。
     * 该值为逻辑长度而非内存占用大小，与容器类size()方法行为完全一致。
     *
     * @return size_t
     *  - 返回字符串包含的有效字符数量（不包含终止符）
     *  - 空字符串返回0
     *
     * @note 关键特性：
     * - 支持编译期常量表达式（C++20起可在编译期计算）
     * - 强制内联优化访问路径
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 多字节/宽字符编码下不代表实际显示长度（如UTF-8多字节字符）
     * - 与std::string::length()行为完全一致
     *
     * @example
     * size_t len = str.length();
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t length() const noexcept {
        return size();
    }

private:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t byte_length() const noexcept {
        return size() * sizeof(char_type);
    }

    /**
     * @return 字符串长度
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t size() const noexcept {
        return M_is_long() ? M_long_length() : M_short_length();
    }

public:
    /**
     * @brief 检查当前字符串是否为空容器
     *
     * 本函数提供高效的空状态检测，通过直接比较内部维护的字符串长度实现，
     * 用于快速判断字符串是否包含有效内容。
     *
     * @return bool
     *  - true  : 字符串长度为0（不含任何字符）
     *  - false : 字符串包含至少1个字符
     *
     * @note 实现特性：
     * - length() == 0的快捷方法
     * - 编译期常量表达式（C++20起可在编译期计算）
     * - 强制内联优化，无函数调用开销
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 与标准库容器的empty()方法行为完全一致
     *
     * @example
     * if (str.empty()) {  //处理空字符串情况  }
     *
     * @see length() 获取具体字符串长度的关联方法
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool empty() const noexcept {
        return size() == 0;
    }

    /**
     * @brief 获取字符串内部存储的只读数据指针
     *
     * 本函数提供对字符串底层字符数组的直接访问，返回指向连续内存空间的常量指针。
     * 适用于需要与C风格API交互或进行零拷贝数据读取的场景。
     *
     * @return const char_type*
     *  - 非空指针：指向字符串内容的字符数组起始地址
     *
     * @note 关键特性：
     * - 返回指针保证在字符串生命周期内有效
     * - 支持编译期获取数据指针（C++20 constexpr）
     * - 严格noexcept保证，永不抛出异常
     * - 强制内联优化访问路径
     * - 与std::string::data()行为兼容
     *
     * @warning 注意事项：
     * - 禁止通过指针修改字符串内容（返回类型为const）
     * - 保证空终止
     *
     * @example
     * - const char* ptr = str.data();
     * - size_t len = str.length();
     * - memcpy(buffer, ptr, len);
     *
     * @see length() 获取关联数据长度
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    const char_type* data() const noexcept {
        return M_pointer();
    }

    /**
     * @brief 创建当前字符串的轻量级或深度拷贝
     *
     * 本函数根据字符串存储类型智能选择拷贝策略，在保证语义正确性的前提下尽可能优化性能。
     * 对于短字符串优化(SSO)和字面量类型直接返回自身，其他类型创建具有独立内存的新对象。
     *
     * @return basic_string
     *  - SSO/字面量类型：返回当前对象的引用（无内存拷贝）
     *  - 动态分配类型：返回持有独立内存的完整副本
     *
     * @note 实现特性：
     * - SSO/字面量类型利用值语义特性避免内存分配
     * - 动态字符串执行深拷贝保证数据独立性
     * - 支持编译期常量表达式拷贝（C++20）
     * - 严格noexcept保证，永不抛出异常
     *
     * @example
     * - auto s1 = str.copy(); // 获取安全副本
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR
    basic_string copy() const & noexcept {
        switch (M_category()) {
            case M_StringCategory::sso:
            case M_StringCategory::literal:
                return *this;
                break;
            default: {
                basic_string result = to_string_view();
                result.M_set_hash(M_hash());
                return result;
            }
        }
    }

    /**
     * @brief 移动构造方式获取当前右值字符串的所有权
     *
     * 本函数为右值引用限定版本，通过移动语义将当前字符串资源所有权转移至新对象，
     * 避免不必要的深拷贝操作，适用于临时对象优化场景。
     *
     * @return basic_string
     *  - 接收当前字符串所有权的全新对象
     *  - 原对象进入有效但未定义状态（符合移动语义规范）
     *
     * @note 关键特性：
     * - 专为右值对象设计的优化拷贝方法
     * - 零拷贝资源转移（指针所有权转移）
     * - 支持编译期移动操作（C++20 constexpr）
     * - 强制内联优化执行路径
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 仅限右值对象调用（如临时对象、std::move转换后的对象）
     * - 调用后原对象不应再被使用（除非重新赋值）
     * - 与左值版本copy()的深拷贝行为有本质区别
     *
     * @example
     * auto s1 = std::move(str).copy(); // 移动构造新对象
     * auto s2 = get_temporary_string().copy(); // 优化临时对象
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string copy() && noexcept {
        return std::move(*this);
    }

    /**
     * @brief 计算并缓存字符串的哈希值（惰性初始化）
     *
     * 本函数提供高效的哈希值获取机制，采用首次访问时计算的惰性初始化策略。
     * 生成的32位哈希值将存储在对象内部，后续调用直接返回缓存值，避免重复计算。
     *
     * @return uint32_t
     *  - 已缓存时：直接返回缓存的哈希值
     *  - 未缓存时：计算并缓存后返回新哈希值
     *
     * @note 关键实现细节：
     * - 通过内部标志位记录缓存状态
     * - 缓存后设置内部标志位
     * - 支持编译期哈希计算（C++20 constexpr）
     *
     * @warning 注意事项：
     * - 非线程安全：多线程环境首次调用需外部同步
     * - 修改字符串内容后缓存不会自动失效，需确保哈希值使用场景
     *
     * @example
     * auto h1 = str.hash(); // 首次计算并缓存
     * auto h2 = str.hash(); // 直接返回缓存值
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t hash() const noexcept {
        if (!M_has_hash()) {
            M_set_hash(internal::str_hash(to_string_view()));
            STRIGNITE_IF_LIKELY(M_remain_capacity() != 0) {
                M_set_remain_capacity(M_remain_capacity() | M_C_has_hash_mask);
            }
        }
        return M_hash();
    }

#pragma region to number

    /**
     * @brief 将字符串内容转换为8位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int8_t
     *  - 成功转换时返回对应的8位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-128, 127]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int8_t to_int8() const noexcept {
        return internal::to_int_not_check_nullptr<int8_t>(data());
    }

    /**
     * @brief 将字符串内容转换为8位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint8_t
     *  - 成功转换时返回对应的8位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 255]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint8_t to_uint8() const noexcept {
        return internal::to_int_not_check_nullptr<uint8_t>(data());
    }

    /**
     * @brief 将字符串内容转换为16位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int16_t
     *  - 成功转换时返回对应的16位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-32768, 32767]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int16_t to_int16() const noexcept {
        return internal::to_int_not_check_nullptr<int16_t>(data());
    }

    /**
     * @brief 将字符串内容转换为16位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int16_t
     *  - 成功转换时返回对应的16位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 65535]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint16_t to_uint16() const noexcept {
        return internal::to_int_not_check_nullptr<uint16_t>(data());
    }

    /**
     * @brief 将字符串内容转换为32位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int32_t
     *  - 成功转换时返回对应的32位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-2147483648, 2147483647]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int32_t to_int32() const noexcept {
        return internal::to_int_not_check_nullptr<int32_t>(data());
    }

    /**
     * @brief 将字符串内容转换为32位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint32_t
     *  - 成功转换时返回对应的32位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 4294967295]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t to_uint32() const noexcept {
        return internal::to_int_not_check_nullptr<uint32_t>(data());
    }

    /**
     * @brief 将字符串内容转换为64位有符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return int64_t
     *  - 成功转换时返回对应的64位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[-9223372036854775808, 9223372036854775807]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    int64_t to_int64() const noexcept {
        return internal::to_int_not_check_nullptr<int64_t>(data());
    }

    /**
     * @brief 将字符串内容转换为64位无符号整数
     *
     * 本函数提供高性能的字符串到整型转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return uint64_t
     *  - 成功转换时返回对应的64位整数值
     *  - 转换失败时行为未定义（可能返回截断值或无效数据）
     *
     * @note 实现特性：
     * - 不进行格式有效性验证
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法整数字符序列
     *   - 数值在[0, 18446744073709551615]范围内
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint64_t to_uint64() const noexcept {
        return internal::to_int_not_check_nullptr<uint64_t>(data());
    }

    /**
     * @brief 将字符串内容转换为单精度浮点数
     *
     * 本函数提供高性能的字符串到浮点数转换，
     * 适用于已知安全格式的快速数值转换场景。
     *
     * @return float
     *  - 成功转换时返回对应的单精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0f或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 内容为合法浮点数字符序列（如"3.14"、"-0.5e3"）
     *   - 数值在float类型表示范围内
     * - 非法输入可能导致未定义行为（包括数值截断、INFINITY或NAN）
     * - 建议仅在绝对可控的数据源场景使用
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    float to_float() const noexcept {
        return std::strtof(data(), nullptr);
    }

    /**
     * @brief 将字符串内容转换为双精度浮点数
     *
     * 本函数提供高性能的字符串到双精度浮点数转换，
     * 适用于需要高精度数值转换且数据源可信的场景。
     *
     * @return double
     *  - 成功转换时返回对应的双精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 格式符合C标准浮点数表示规范
     *   - 数值在double类型表示范围内
     * - 非法格式可能导致静默错误或精度丢失
     * - 科学计数法支持依赖本地化设置
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    double to_double() const noexcept {
        return std::strtod(data(), nullptr);
    }

    /**
     * @brief 将字符串内容转换为扩展精度浮点数
     *
     * 本函数提供最高精度的字符串到浮点数转换，适用于科学计算等需要高精度
     * 数值解析的场景。
     *
     * @return long double
     *  - 成功转换时返回扩展精度浮点数值
     *  - 转换失败时行为未定义（可能返回0.0L或无效数据）
     *
     * @note 实现特性：
     * - 不处理转换错误状态
     * - 支持编译期常量表达式转换（C++20）
     * - 强制内联优化转换路径
     *
     * @warning 注意事项：
     * - 必须确保字符串：
     *   - 格式兼容long double类型的解析要求
     *   - 数值在long double表示范围内
     * - 不同平台精度表现可能不一致（80位/128位等）
     * - 十六进制浮点格式（如0x1.2p3）支持依赖编译器实现
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    long double to_long_double() const noexcept {
        return std::strtold(data(), nullptr);
    }

#pragma endregion to number

    // // 待重写
    // STRIGNITE_CPP17_NODISCARD
    // STRIGNITE_CPP20_CONSTEXPR bool to_bool() const noexcept {
    //     return false;
    // }

    /**
     * @brief 将字符串转换为全大写形式
     *
     * 本函数通过指定的字符特性类，将当前字符串的所有字符转换为大写形式，
     * 生成并返回新的字符串对象，原字符串内容保持不变。
     *
     * @tparam to_upper_traits 字符转换特性类（默认latin1），需提供：
     *                         - 静态方法 to_upper(char_type) 实现大写转换逻辑
     *
     * @return basic_string 包含转换后大写字符的新字符串对象
     *
     * @note 特性说明：
     * - 预分配与源字符串等长的内存空间
     * - 逐个字符应用特性类转换方法
     * - 编译期转换时保留哈希值优化
     * - 严格noexcept保证，永不抛出异常
     * - 支持编译期常量表达式转换（C++20）
     *
     * @warning 注意事项：
     * - 返回新字符串的哈希值可能在编译期预计算
     *
     * @example
     * string s = "Hello World";
     * auto upper = s.to_upper();  // 返回"HELLO WORLD"
     *
     * @see to_lower() 对应的小写转换方法
     */
    template<typename to_upper_traits = latin1>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR basic_string to_upper() const noexcept {
        auto view = to_string_view();
        auto result = M_reserve(view.size());
        internal::to_upper_helper<char_type, to_upper_traits>(view, result.M_pointer());
        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))
        return result;
    }

    /**
     * @brief 将字符串转换为全小写形式
     *
     * 本函数通过指定的字符特性类，将当前字符串的所有字符转换为小写形式，
     * 生成并返回新的字符串对象，保持原字符串内容不变。
     *
     * @tparam to_lower_traits 字符转换特性类（默认latin1），需提供：
     *                         - 静态方法 to_lower(char_type) 实现小写转换逻辑
     *
     * @return basic_string 包含转换后小写字符的新字符串对象
     *
     * @note 实现细节：
     * - 预分配与源字符串等长的内存空间
     * - 逐个字符应用特性类转换方法
     * - 编译期转换时保留哈希值优化
     * - 严格noexcept保证，永不抛出异常
     * - 支持编译期常量表达式转换（C++20）
     *
     * @warning 注意事项：
     * - 返回新字符串的哈希值可能在编译期预计算
     *
     * @example
     * string s = "HELLO WORLD";
     * auto lower = s.to_lower();  // 返回"hello world"
     *
     * @see to_upper() 对应的大写转换方法
     */
    template<typename to_lower_traits = latin1>
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    basic_string to_lower() const noexcept {
        auto view = to_string_view();
        auto result = M_reserve(view.size());
        internal::to_lower_helper<char_type, to_lower_traits>(view, result.M_pointer());
        STRIGNITE_IF_CONSTANT_EVALUATED(result.M_set_hash(result.hash()))
        return result;
    }

    /**
     * @brief 检测当前字符串是否符合数字格式规范
     *
     * 本函数通过底层数字解析引擎验证字符串内容，判断其是否为合法的数字表示形式。
     * 支持整数、浮点数及科学计数法格式的检测。
     *
     * @return bool
     *  - true  : 字符串内容可解析为有效数字（如"123", "-45.67", "8.9e+3"）
     *  - false : 包含非数字字符/格式错误（如"12a3", "--5", "3.14.15"）
     *
     * @note 实现特性：
     * - 空字符串直接返回false
     * - 支持编译期常量表达式检测（C++20）
     * - 强制内联优化检测路径
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 前导/尾随空格会导致验证失败
     * - 不识别千位分隔符（如"1,234"返回false）
     * - 十六进制格式（如"0x1A"）是否支持取决于底层实现
     *
     * @example
     * str = "123.45e-6";  // 返回true
     * str = "12AB34";     // 返回false
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool is_number() const noexcept {
        return bcs::is_number(data());
    }

    /**
     * @brief 将当前字符串转换为标准库字符串对象
     *
     * 本函数执行深拷贝操作，根据当前字符串的存储模式（SSO/长字符串）创建独立的
     * std::basic_string 对象，保证返回字符串的数据所有权独立于原对象。
     *
     * @return std::basic_string<char_type>
     *  - 包含原字符串所有字符的新标准字符串对象
     *  - 空字符串返回空的标准字符串对象
     *
     * @note 关键特性：
     * - 长字符串模式：从堆内存数据构造，触发内存拷贝
     * - 短字符串模式：从栈缓冲区构造，零拷贝优化
     * - 支持编译期转换（C++20 constexpr）
     * - 强制内联优化执行路径
     * - 严格强异常安全保证
     *
     * @warning 注意事项：
     * - 返回对象与原字符串内存完全独立，修改互不影响
     * - 大字符串转换可能引发内存分配（取决于标准库实现）
     * - 不保留原字符串的哈希值等元信息
     *
     * @example
     * auto std_str = bcs_str.to_std_string();
     * std::cout << std_str << std::endl;
     *
     * @see to_std_string_view() 零拷贝转换为标准字符串视图的方法
     * @see to_string_view() 转换为兼容字符串视图的方法
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    std::basic_string<char_type> to_std_string() const {
        return M_is_long()
                   ? std::basic_string<char_type>{ M_long_pointer(), M_long_length() }
                   : std::basic_string<char_type>{ M_short_pointer(), M_short_length() };
    }

#if STRIGNITE_CPLUSPLUS_17
    /**
     * @brief 将当前字符串转换为标准库字符串视图对象
     *
     * 本函数提供零开销的类型转换，生成指向原字符串数据的只读视图，
     * 适用于需要与标准库API交互且不需要数据所有权的场景。
     *
     * @return std::basic_string_view<char_type>
     *  - 长字符串模式：引用堆内存数据的视图
     *  - 短字符串模式：引用本地缓冲区的视图
     *  - 空字符串返回空视图（data()返回非空指针但size()为0）
     *
     * @note 关键特性：
     * - 零拷贝转换：不涉及内存分配或数据复制
     * - 生命周期绑定：返回视图的有效性与原字符串实例绑定
     * - 支持C++17起的constexpr上下文
     * - 强制内联优化转换路径
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 必须确保原字符串的生命周期覆盖视图的使用范围
     * - 修改原字符串可能导致视图失效（如内存重分配）
     * - 禁止对右值对象调用此方法（函数已禁用）
     *
     * @example
     * void process(std::string_view sv);
     * process(bcs_str.to_std_string_view());
     *
     * @see to_std_string()   需要数据所有权时使用的转换方法
     * @see to_string_view()  转换为兼容的字符串视图类型
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    std::basic_string_view<char_type> to_std_string_view() const & noexcept {
        return M_is_long()
                   ? std::basic_string_view<char_type>{ M_long_pointer(), M_long_length() }
                   : std::basic_string_view<char_type>{ M_short_pointer(), M_short_length() };
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP17_CONSTEXPR STRIGNITE_FORCE_INLINE
    std::basic_string_view<char_type> to_std_string_view() const && noexcept = delete;
#endif

    /**
     * @brief 将当前字符串转换为字符串视图对象
     *
     * 本函数提供零开销的类型转换，生成指向原始字符串数据的非持有型视图，
     * 适用于需要只读访问且避免内存拷贝的场景。
     *
     * @return string_view_t
     *  - 长字符串模式：引用堆内存数据的视图（包含指针和长度）
     *  - 短字符串模式：引用SSO缓冲区的视图（直接访问内部存储）
     *  - 空字符串返回有效视图（data()可能非空，size()为0）
     *
     * @note 关键特性：
     * - 零拷贝转换：不涉及内存分配或数据复制
     * - 生命周期绑定：视图有效性取决于原字符串实例的生命周期
     * - 支持编译期转换（C++20 constexpr）
     * - 强制内联优化访问路径
     * - 严格noexcept保证，永不抛出异常
     *
     * @warning 注意事项：
     * - 修改原字符串可能导致视图失效（如触发内存重分配）
     * - 视图不应在源字符串对象销毁后继续使用
     * - SSO模式下视图直接引用对象内部内存，需注意对齐和访问边界
     *
     * @example
     * auto sv = str.to_string_view();
     * size_t len = sv.length(); // 获取视图长度
     *
     * @see data() 获取原始指针的关联方法
     * @see to_std_string_view() 转换为标准库字符串视图的方法
     */
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    string_view_t to_string_view() const noexcept {
        return M_is_long()
                   ? string_view_t{ M_long_pointer(), M_long_length() }
                   : string_view_t{ M_short_pointer(), M_short_length() };
    }

#pragma region private member functions

private:
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool M_is_long() const noexcept {
        return M_remain_capacity() & M_C_category_mask;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool M_has_enable_ref_count() const noexcept {
        return M_category() == M_StringCategory::ref_count;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type* M_pointer() const noexcept {
        return M_is_long() ? M_long_pointer() : M_short_pointer();
    }

private:
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_long_pointer(const char_type* ptr) noexcept {
        M_basic_data.M_long.M_str = ptr;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type* M_long_pointer() const noexcept {
        assert(M_category() != M_StringCategory::sso);
        return const_cast<char_type *>(M_basic_data.M_long.M_str);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    char_type* M_short_pointer() const noexcept {
        return const_cast<char_type *>(M_basic_data.M_short.M_buffer);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t M_long_length() const noexcept {
        assert(M_category() != M_StringCategory::sso);
        return M_basic_data.M_long.M_length;
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_long_length(size_t length) noexcept {
        M_basic_data.M_long.M_length = length;
    }

    /**
     * 此函数会默认将字符串的类型设置成 sso
     * @param length 设置字符串的长度
     */
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_short_length(uint8_t length) const noexcept {
        STRIGNITE_ASSUME(length <= M_C_max_sso_capacity)
        M_set_remain_capacity(M_C_max_sso_capacity - length << M_C_short_length_shift);
        assert(M_category() == M_StringCategory::sso);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    size_t M_short_length() const noexcept {
        assert(M_category() == M_StringCategory::sso);
        return M_C_max_sso_capacity - (M_remain_capacity() >> M_C_short_length_shift);
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_length(const size_t length) noexcept {
        if (M_is_long()) {
            M_set_long_length(length);
        } else {
            M_set_short_length(length);
        }
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_category(const M_StringCategory category) const noexcept {
        assert(category != M_StringCategory::sso);
        M_set_remain_capacity(static_cast<uint8_t>(category));
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    M_StringCategory M_category() const noexcept {
        return static_cast<M_StringCategory>(M_remain_capacity() & M_C_category_mask);
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t M_hash() const noexcept {
        return M_extend_data.M_hash;
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_hash(const uint32_t value) const noexcept {
        const_cast<uint32_t &>(M_extend_data.M_hash) = value;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    bool M_has_hash() const noexcept {
        STRIGNITE_IF_LIKELY(M_remain_capacity() != 0) {
            return M_remain_capacity() & static_cast<uint8_t>(M_C_has_hash_mask);
        }
        return M_hash() != 0;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint8_t M_remain_capacity() const noexcept {
        return M_extend_data.M_sso_remain_capacity_and_flag;
    }

    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    void M_set_remain_capacity(const uint8_t value) const noexcept {
        const_cast<uint8_t &>(M_extend_data.M_sso_remain_capacity_and_flag) = value;
    }

    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR static basic_string M_reserve(size_t length) noexcept {
        basic_string result;
        if (length > M_C_max_sso_capacity) {
            char_type* ptr;
            if (length >= M_C_enable_ref_count_size) {
                ptr = M_RefCount::create(length)->data;
                result.M_set_category(M_StringCategory::ref_count);
            } else {
                ptr = new char_type[length + 1];
                result.M_set_category(M_StringCategory::eager_copy);
            }
            ptr[length] = static_cast<char_type>(0);
            result.M_set_long_length(length);
            result.M_set_long_pointer(ptr);
        } else {
            STRIGNITE_IF_CONSTANT_EVALUATED(
                result.M_basic_data.M_short = {};
                result.M_extend_data = {};
                result.M_set_short_length(length);
            ) STRIGNITE_ELSE_NOT_CONSTANT_EVALUATED(
                result.M_set_short_length(length);
                result.M_short_pointer()[length] = static_cast<char_type>(0);
            )
        }
        result.M_set_hash(0);
        return result;
    }

#pragma endregion private member functions

private: //data member
    struct M_RefCount {
        std::atomic<size_t> count;
        char_type data[1];

        STRIGNITE_CPP17_NODISCARD STRIGNITE_FORCE_INLINE
        constexpr static size_t data_offset() noexcept {
            constexpr size_t offset = offsetof(M_RefCount, data);
            return offset;
        }

        STRIGNITE_CPP17_NODISCARD STRIGNITE_FORCE_INLINE
        static size_t counts(const char_type* data) noexcept {
            return form_data(data)->count.load(std::memory_order_acquire);
        }

        STRIGNITE_CPP17_NODISCARD STRIGNITE_FORCE_INLINE
        static M_RefCount* form_data(const char_type* data) noexcept {
            return reinterpret_cast<M_RefCount *>(
                reinterpret_cast<uint8_t *>(const_cast<char_type *>(data)) - data_offset()
            );
        }

        STRIGNITE_CPP17_NODISCARD STRIGNITE_FORCE_INLINE
        static M_RefCount* create(const size_t size) {
            auto* result = static_cast<M_RefCount *>(
                malloc(sizeof(std::atomic<size_t>) + size * sizeof(char_type) + 1));
            result->count.store(1, std::memory_order_release);
            return result;
        }

        STRIGNITE_FORCE_INLINE
        static void increment_count(const char_type* data) noexcept {
            form_data(data)->count.fetch_add(1, std::memory_order_acq_rel);
        }

        STRIGNITE_FORCE_INLINE
        static void decrement_count(const char_type* data) noexcept {
            auto ref = form_data(data);
            auto old_count = ref->count.fetch_sub(1, std::memory_order_acq_rel);
            assert(old_count > 0);
            if (old_count == 1) {
                free(ref);
            }
        }
    };

    /**
     * @brief 长字符串存储结构
     *
     * 用于非SSO模式的字符串存储：
     */
    struct M_Long {
        const char_type* M_str;   ///< 字符串数据指针
        size_t M_length;         ///< 字符串长度
    };

    /// @name 内存布局相关常量
    /// @{
    enum M_Constants : uint8_t {
        M_C_size_t_int32_diff_size = sizeof(size_t) - sizeof(uint32_t), ///< size_t与uint32_t的尺寸差
        M_C_short_padding_bytes_size = sizeof(char_type) - 1,           ///< 短字符串填充字节数
        M_C_max_sso_capacity = (sizeof(M_Long) + (sizeof(size_t) - M_C_size_t_int32_diff_size) - 1) /
                               sizeof(char_type),                       ///< SSO最大容量（字符数）
        M_C_enable_ref_count_size = 128 / sizeof(char_type),            ///< 启用引用计数的阈值大小
        M_C_category_mask = 0x3,                                        ///< 存储类别位掩码
        M_C_short_length_shift = 0x3,                                   ///< 短字符串长度位偏移
        M_C_has_hash_mask = 0x4,                                        ///< 哈希值存在标志位掩码
    };

    /// @}

    /// @name 数据成员
    /// @{

    /**
     * @brief 短字符串存储数组
     *
     * 用于SSO模式的字符串存储，尺寸由M_C_max_sso_capacity确定
     */
    struct M_Short {
        char_type M_buffer[M_C_max_sso_capacity]; ///< 内联字符缓冲区
    };

#if defined(__GNUC__) || defined(__clang__)
    /**
     * @brief 基础数据联合体
     *
     * 根据存储模式不同使用不同布局：
     * - 长字符串模式：存储指针和长度
     * - 短字符串模式：直接存储字符数组
     */
    union M_BasicData {
        M_Long M_long;      ///< 长字符串存储结构
        M_Short M_short;    ///< 短字符串存储数组
    } __attribute__((packed));

    /**
     * @brief 扩展数据结构
     *
     * 包含短字符串的剩余容量标志位和哈希值：
     * - 短字符串模式：存储剩余容量和长度信息
     * - 所有模式：存储预计算的哈希值
     */
    struct M_ExtendData : internal::padding<M_C_short_padding_bytes_size> {
        uint8_t M_sso_remain_capacity_and_flag; ///< 短字符串剩余容量与标志位
        uint32_t M_hash;                        ///< 预计算的哈希值
    } __attribute__((packed));
#else
#  pragma pack(push, 1)
    /**
     * @brief 基础数据联合体
     *
     * 根据存储模式不同使用不同布局：
     * - 长字符串模式：存储指针和长度
     * - 短字符串模式：直接存储字符数组
     */
    union M_BasicData {
        M_Long M_long;      ///< 长字符串存储结构
        M_Short M_short;    ///< 短字符串存储数组
    };

    /**
     * @brief 扩展数据结构
     *
     * 包含短字符串的剩余容量标志位和哈希值：
     * - 短字符串模式：存储剩余容量和长度信息
     * - 所有模式：存储预计算的哈希值
     */
    struct M_ExtendData : internal::padding<M_C_short_padding_bytes_size> {
        uint8_t M_sso_remain_capacity_and_flag; ///< 短字符串剩余容量与标志位
        uint32_t M_hash;                        ///< 预计算的哈希值
    };
#  pragma pack(pop)
#endif

    M_BasicData M_basic_data; ///< 长字符串和短字符串缓存
    M_ExtendData M_extend_data; ///< 短字符串的长度与哈希码

    /// @}

    static_assert(CHAR_BIT == 8, " ");
    static_assert(sizeof(char) == 1, " ");
    static_assert(sizeof(char_type) <= 4, " ");
    static_assert((sizeof(M_BasicData) + sizeof(M_ExtendData)) % sizeof(size_t) == 0, " ");
};

#pragma endregion class string

#pragma region to number

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR static int8_t to_int8(const char_type* const str) noexcept {
    assert(str);
    return internal::to_int_not_check_nullptr<int8_t>(str);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR static uint8_t to_uint8(const char_type* const str) noexcept {
    assert(str);
    return internal::to_int_not_check_nullptr<uint8_t>(str);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR static int16_t to_int16(const char_type* const str) noexcept {
    assert(str);
    return internal::to_int_not_check_nullptr<int16_t>(str);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR static uint16_t to_uint16(const char_type* const str) noexcept {
    assert(str);
    return internal::to_int_not_check_nullptr<uint16_t>(str);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR static int32_t to_int32(const char_type* const str) noexcept {
    assert(str);
    return internal::to_int_not_check_nullptr<int32_t>(str);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR static uint32_t to_uint32(const char_type* const str) noexcept {
    assert(str);
    return internal::to_int_not_check_nullptr<uint32_t>(str);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR static int64_t to_int64(const char_type* const str) noexcept {
    assert(str);
    return internal::to_int_not_check_nullptr<int64_t>(str);
}

template<typename char_type>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP14_CONSTEXPR static uint64_t to_uint64(const char_type* const str) noexcept {
    assert(str);
    return internal::to_int_not_check_nullptr<uint64_t>(str);
}

#pragma endregion to number

#pragma region code convert

template<typename Char32, template<typename...> class BasicString, typename Char8, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char32> internal::_to_utf32(const Char8* str, size_t len, bool) {
    Char32 temp[len];
    size_t length = 0;
    auto ptr = temp;
    uint32_t ch = 0;
    while (*str) {
        ch = *str & 0xF0;
        if (ch == 0xE0) {
            *ptr++ = ((*(str + 1) & 0xF) << 6 | *(str + 2) & 0x3F) << 6 | *(str + 3) & 0x3F;
            str += 3;
        } else if (ch == 0xF0) {
            *ptr++ = (((*(str + 1) & 0x7) << 6 | *(str + 2) & 0x3F) << 6 | *(str + 3) & 0x3F) << 6 | *(str + 4) & 0x3F;
            str += 4;
        } else if ((ch &= 0xC0) == 0xC0) {
            *ptr++ = (*(str + 1) & 0x1F) << 6 | *(str + 2) & 0x3F;
            str + 2;
        } else if (ch == 0x80) {
            ++str;
            length--;
        } else {
            *ptr++ = *str++;
        }
        length++;
    }
    return { temp, length };
}

template<typename Char32, template<typename...> class BasicString, typename Char16, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char32> internal::_to_utf32(const Char16* str, size_t len, int) {
    auto _str = str;
    Char32 temp[len];
    auto ptr = temp;
    size_t length = 0;
    uint32_t ch = 0;
    while (*_str) {
        ch = *_str++;
        if (ch >= 0xD800 && ch <= 0xDBFF) {
            const uint32_t code_point = ((ch - 0xD800) << 10 | (*_str++ - 0xDC00)) + 0x10000;
            *ptr++ = code_point;
        } else {
            *ptr++ = ch;
        }
        length++;
    }
    return { temp, length };
}

template<typename Char32, template<typename...> class BasicString, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char32> internal::_to_utf32(const char32_t* str, size_t len) {
    return BasicString<Char32>{ str, len };
}

template<typename RChar8, template<typename...> class BasicString, typename Char8, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<RChar8> internal::_to_utf8(const Char8* str, size_t len, bool) {
#if STRIGNITE_CPLUSPLUS >= 202002L
    if (std::is_constant_evaluated()) {
        RChar8 temp[len];
        for (auto i = 0; i < len; i++) {
            temp[i] = static_cast<RChar8>(str[i]);
        }
        return { temp, len };
    }
#endif
    return { reinterpret_cast<const RChar8 *>(str), len };
}

template<typename Char8, template<typename...> class BasicString, typename Char16, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char8> internal::_to_utf8(const Char16* str, size_t len, int) {
    auto _str = str;
    Char8 temp[len * 4];
    auto ptr = temp;
    size_t length = 0;
    uint32_t ch = 0;
    while (*_str) {
        ch = *_str++;
        if (ch >= 0xD800 && ch <= 0xDBFF) {
            const uint32_t code_point = ((ch - 0xD800) << 10 | (*_str++ - 0xDC00)) + 0x10000;
            *ptr++ = static_cast<Char8>(code_point >> 18 | 0xF0);
            *ptr++ = static_cast<Char8>(code_point >> 12 & 0x3F | 0x80);
            *ptr++ = static_cast<Char8>(code_point >> 6 & 0x3F | 0x80);
            *ptr++ = static_cast<Char8>(code_point & 0x3F | 0x80);
            length += 4;
        } else if (ch > 0x7FF) {
            *ptr++ = static_cast<Char8>(ch >> 12 | 0xE0);
            *ptr++ = static_cast<Char8>(ch >> 6 & 0x3F | 0x80);
            *ptr++ = static_cast<Char8>(ch & 0x3F | 0x80);
            length += 3;
        } else if (ch > 0xFF) {
            *ptr++ = static_cast<Char8>(ch >> 6 | 0xC0);
            *ptr++ = static_cast<Char8>(ch & 0x3F | 0x80);
            length += 2;
        } else {
            *ptr++ = static_cast<Char8>(ch);
            length += 1;
        }
    }
    return { temp, length };
}

template<typename Char8, template<typename...> class BasicString, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char8> internal::_to_utf8(const char32_t* str, size_t len) {
    Char8 temp[len * 4];
    auto ptr = temp;
    size_t length = 0;
    uint32_t ch = 0;
    while (*str) {
        ch = *str++;
        if (ch > 0xFFFF) {
            *ptr++ = static_cast<Char8>(ch >> 18 | 0xF0);
            *ptr++ = static_cast<Char8>(ch >> 12 & 0x3F | 0x80);
            *ptr++ = static_cast<Char8>(ch >> 6 & 0x3F | 0x80);
            *ptr++ = static_cast<Char8>(ch & 0x3F | 0x80);
            length += 4;
        } else if (ch > 0x7FF) {
            *ptr++ = static_cast<Char8>(ch >> 12 | 0xE0);
            *ptr++ = static_cast<Char8>(ch >> 6 & 0x3F | 0x80);
            *ptr++ = static_cast<Char8>(ch & 0x3F | 0x80);
            length += 3;
        } else if (ch > 0xFF) {
            *ptr++ = static_cast<Char8>(ch >> 6 | 0xC0);
            *ptr++ = static_cast<Char8>(ch & 0x3F | 0x80);
            length += 2;
        } else {
            *ptr++ = static_cast<Char8>(ch);
            length += 1;
        }
    }
    return { temp, length };
}

template<typename Char16, template<typename...> class BasicString, typename Char8, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char16> internal::_to_utf16(const Char8* str, size_t len, bool) {
    auto _str = str;
    Char16 temp[len * 2];
    size_t length = 0;
    auto ptr = temp;
    uint32_t ch = 0;
    while (*_str) {
        ch = *_str & 0xF0;
        if (ch == 0xE0) {
            *ptr++ = ((*_str++ & 0xF) << 6 | *_str++ & 0x3F) << 6 | *_str++ & 0x3F;
            length++;
        } else if (ch == 0xF0) {
            const uint32_t code_point =
                    ((((*_str++ & 0x7) << 6 | *_str++ & 0x3F) << 6 | *_str++ & 0x3F) << 6 | *_str++ & 0x3F) - 0x10000;
            *ptr++ = code_point >> 10 | 0xD800;
            *ptr++ = code_point & 0x3FF | 0xDC00;
            length += 2;
        } else if ((ch &= 0xC0) == 0xC0) {
            *ptr++ = (*_str++ & 0x1F) << 6 | *_str++ & 0x3F;
            length++;
        } else if (ch == 0x80) {
            ++_str;
        } else {
            *ptr++ = *_str++;
            length++;
        }
    }
    return BasicString<Char16>{ temp, length };
}

template<typename RChar16, template<typename...> class BasicString, typename Char16, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<RChar16> internal::_to_utf16(const Char16* str, size_t len, int) {
#if STRIGNITE_CPLUSPLUS >= 202002L
    if (std::is_constant_evaluated()) {
        RChar16 temp[len];
        for (auto i = 0; i < len; i++) {
            temp[i] = static_cast<RChar16>(str[i]);
        }
        return { temp, len };
    }
#endif
    return { reinterpret_cast<const RChar16 *>(str), len };
}

template<typename Char16, template<typename...> class BasicString, typename>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR BasicString<Char16> internal::_to_utf16(const char32_t* str, size_t len) {
    Char16 temp[len * 2];
    auto ptr = temp;
    size_t length = 0;
    uint32_t ch = 0;
    while (*str) {
        ch = *str++;
        if (ch > 0xFFFF) {
            const uint32_t code_point = ch - 0x10000;
            *ptr++ = code_point >> 10 | 0xD800;
            *ptr++ = code_point & 0x3FF | 0xDC00;
            length += 2;
        } else {
            *ptr++ = ch;
            length += 1;
        }
    }
    return { temp, length };
}

template<template<typename...> class BasicString>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR
BasicString<wchar_t> internal::_convert<BasicString, wchar_t>::from_utf8(const char* str, const size_t len) {
#if defined(_WIN32)
    return _to_utf16<wchar_t, BasicString>(str, len);
#else
    return _to_utf32<wchar_t, BasicString>(str, len);
#endif
}

#if STRIGNITE_CPLUSPLUS >= 202002L
template<template<typename...> typename BasicString>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR
BasicString<char8_t> internal::_convert<BasicString, char8_t>::from_utf8(const char* str, const size_t len) {
    return _to_utf8<char8_t, BasicString>(str, len);
}
#endif

template<template<typename...> class BasicString>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR
BasicString<char16_t> internal::_convert<BasicString, char16_t>::from_utf8(const char* str, const size_t len) {
    return _to_utf16<char16_t, BasicString>(str, len);
}

template<template<typename...> class BasicString>
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR
BasicString<char32_t> internal::_convert<BasicString, char32_t>::from_utf8(const char* str, const size_t len) {
    return _to_utf32<char32_t, BasicString>(str, len);
}

#pragma endregion code convert

#pragma region literals

template<typename char_type>
STRIGNITE_CPP17_NODISCARD STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
basic_string<char_type> internal::make_literals_string(const char_type* str, size_t len) noexcept {
    basic_string<char_type> result;
    result.M_set_long_pointer(str);
    result.M_set_long_length(len);
    result.M_set_category(basic_string<char_type>::M_StringCategory::literal);
    STRIGNITE_IF_CONSTANT_EVALUATED(
        result.M_set_hash(result.hash())
    ) STRIGNITE_ELSE_NOT_CONSTANT_EVALUATED(
        result.M_set_hash(0)
    )
    return result;
}

namespace literals {
/**
 * @brief 从 char 类型的字符串字面量创建 basic_string 对象
 * @param str char 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string<char> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
string operator "" s(const char* str, const size_t len) noexcept {
    return internal::make_literals_string(str, len);
}

/**
 * @brief 从 wchar_t 类型的字符串字面量创建 basic_string 对象
 * @param str wchar_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string<wchar_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
wstring operator "" s(const wchar_t* str, const size_t len) noexcept {
    return internal::make_literals_string(str, len);
}

#if STRIGNITE_CPLUSPLUS_20
/**
 * @brief 从 char8_t 类型的字符串字面量创建 basic_string 对象
 * @param str char8_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string<char8_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
u8string operator "" s(const char8_t* str, const size_t len) noexcept {
    return internal::make_literals_string(str, len);
}
#endif

/**
 * @brief 从 char16_t 类型的字符串字面量创建 basic_string 对象
 * @param str char16_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string<char16_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
u16string operator "" s(const char16_t* str, const size_t len) noexcept {
    return internal::make_literals_string(str, len);
}

/**
 * @brief 从 char32_t 类型的字符串字面量创建 basic_string 对象
 * @param str char32_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string<char32_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
u32string operator "" s(const char32_t* str, const size_t len) noexcept {
    return internal::make_literals_string(str, len);
}

/**
 * @brief 从 char 类型的字符串字面量创建 basic_string_view 对象
 * @param str char 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string_view<char> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
string_view operator "" sv(const char* str, const size_t len) noexcept {
    return { str, len };
}

/**
 * @brief 从 wchar_t 类型的字符串字面量创建 basic_string_view 对象
 * @param str wchar_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string_view<wchar_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
wstring_view operator "" sv(const wchar_t* str, const size_t len) noexcept {
    return { str, len };
}

#if STRIGNITE_CPLUSPLUS_20
/**
 * @brief 从 char8_t 类型的字符串字面量创建 basic_string_view 对象
 * @param str char8_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string_view<char8_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
u8string_view operator "" sv(const char8_t* str, const size_t len) noexcept {
    return { str, len };
}
#endif

/**
 * @brief 从 char16_t 类型的字符串字面量创建 basic_string_view 对象
 * @param str char16_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string_view<char16_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
u16string_view operator "" sv(const char16_t* str, const size_t len) noexcept {
    return { str, len };
}

/**
 * @brief 从 char32_t 类型的字符串字面量创建 basic_string_view 对象
 * @param str char32_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_string_view<char32_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
u32string_view operator "" sv(const char32_t* str, const size_t len) noexcept {
    return { str, len };
}

/**
 * @brief 从 char 类型的字符串字面量创建 basic_mutable_string 对象
 * @param str char 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_mutable_string<char> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
mutable_string operator "" ms(const char* str, const size_t len) noexcept {
    return { str, len };
}

/**
 * @brief 从 wchar_t 类型的字符串字面量创建 basic_mutable_string 对象
 * @param str wchar_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_mutable_string<wchar_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
mutable_wstring operator "" ms(const wchar_t* str, const size_t len) noexcept {
    return { str, len };
}

#if STRIGNITE_CPLUSPLUS_20
/**
 * @brief 从 char8_t 类型的字符串字面量创建 basic_mutable_string 对象
 * @param str char8_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_mutable_string<char8_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
mutable_u8string operator "" ms(const char8_t* str, const size_t len) noexcept {
    return { str, len };
}
#endif

/**
 * @brief 从 char16_t 类型的字符串字面量创建 basic_mutable_string 对象
 * @param str char16_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_mutable_string<char16_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
mutable_u16string operator "" ms(const char16_t* str, const size_t len) noexcept {
    return { str, len };
}

/**
 * @brief 从 char32_t 类型的字符串字面量创建 basic_mutable_string 对象
 * @param str char32_t 类型的字符串字面量
 * @param len 字符串字面量的长度
 * @return 一个 basic_mutable_string<char32_t> 类型对象
 */
STRIGNITE_CPP17_NODISCARD
STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
mutable_u32string operator "" ms(const char32_t* str, const size_t len) noexcept {
    return { str, len };
}
}

#pragma endregion literals
}

#pragma region hash function

template<typename... T>
struct std::hash<bcs::basic_string<T...>> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t operator()(const bcs::basic_string<T...>& str) const noexcept {
        return str.hash();
    }
};

template<typename... T>
struct std::hash<bcs::basic_mutable_string<T...>> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t operator()(const bcs::basic_mutable_string<T...>& str) const noexcept {
        return bcs::internal::str_hash(str.to_string_view());
    }
};

template<size_t Capacity, typename... T>
struct std::hash<bcs::basic_static_string<Capacity, T...>> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t operator()(const bcs::basic_static_string<Capacity, T...>& str) const noexcept {
        return bcs::internal::str_hash(str.to_string_view());
    }
};

template<size_t Capacity, typename... T>
struct std::hash<bcs::basic_static_mutable_string<Capacity, T...>> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t operator()(const bcs::basic_static_mutable_string<Capacity, T...>& str) const noexcept {
        return bcs::internal::str_hash(str.to_string_view());
    }
};

template<typename... T>
struct std::hash<bcs::basic_string_view<T...>> {
    STRIGNITE_CPP17_NODISCARD
    STRIGNITE_CPP20_CONSTEXPR STRIGNITE_FORCE_INLINE
    uint32_t operator()(const bcs::basic_string_view<T...>& str) const noexcept {
        return bcs::internal::str_hash(str);
    }
};

#pragma endregion hash function

#endif
