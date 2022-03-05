# Sanums
_Sane numbers library for C++20_

## Disclaimer

This is a toy project, if you wanna use it in production great! But be prepared to maintain it yourself.

## Overview

This is mostly a toy project to see what could be done with C++20 features such as concepts.

It is designed to mimic rust built-in numeric types, with no implicit conversions and checked casts.
No longer will you have to debug things such as [this insanity](https://godbolt.org/z/8fhxaabjz)


## Usage

Simply include `sanums.hpp` and then access the types through the `sn` namespace (e.g. `sn::u8`).
All the types are `constexpr` usable and are trivially copyable
(cannot be fully trivial due to default-initialising on default construction).
Additionally, if possible, invalid conversions cause a compile error, if this is not possible it throws an exception.

There is also a `from` function that can be used for some type inference stuffs, e.g.:
```cpp
#include <sanums.hpp>

int main() {
    const sn::u8 x{25};
    const sn::u16 y{65};
    const sn::u16 z = sn::from(x) + y;
}

```
beware that `from` can cause issues with overload ambiguities so should only be used in expressions
where there is a strong type present (i.e. u8, u16, etc)


