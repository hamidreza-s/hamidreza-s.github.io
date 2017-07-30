---
layout: post
disqus: true
title: "Erlang Bit Syntax Details and Why it Matters"
date: YYYY-MM-DD
categories: erlang datatype bitsyntax bitstring binary protocol kafka
---

{ preface }

## What is Erlang Binary Data Type

{ explain binary, its history, goal, usage, protocol, efficiency }
{ difference among binary, bit string, and bit syntax }

- Binary: A BitString that consists of a number of bits that are evenly divisible by eight, is called Binary.
- BitString: BitStrings are expressed using the BitSyntax.
- BitSyntax: A BitString is used to store an area of untyped memory.

## Binary Data Type Internals

{ explain memory structure and internal representation and types }
{ explain heap binary, reference counted binary, and sub binary }

## Bit Syntax

{ explain details like element, segment, type, signedness, endianness, unit, value, and size }

## How to Implement a Binary Protocol

{ implement kafka binary protocol }

## Conclusion

{ say it is unique }
{ say something impressive }

## Resources

- https://github.com/erlang/otp/blob/master/erts/emulator/beam/binary.c#L68-L108
- http://user.it.uu.se/~pergu/papers/erlang05.pdf
- http://user.it.uu.se/~pergu/papers/erlang02.pdf
- http://www.erlang.se/workshop/2004/esmb.pdf
- http://erlang.org/doc/reference_manual/expressions.html#bit_syntax
- http://erlang.org/doc/programming_examples/bit_syntax.html