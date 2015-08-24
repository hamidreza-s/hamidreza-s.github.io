---
layout: post
title: "Erlang Garbage Collection Details and Why It Matters"
date: 2015-08-24 20:15:00
category: erlang garbage-collection soft-realtime
---

One of the main problems that Erlang tried to solve was implementig a [Soft Realtime](--#--) system with a high level of responsiveness. Such systems require a fast [Garbage Collection](--#--) mechanism that doesn't stop the system from responsing in a timely manner. In other hand Garbage Collection gets more importance when we consider Erlang as an [Immutable](--#--) language with [Non-destructive Update](--#--) property, because there is a high rate of producing garbage in such languages.

Before digging deeper into it, lets inspect the memory layout of an Erlang process which can be divided into three main part; PCB, Stack and Heap. It is so similar to Unix process memory layout.

## Memory Layout

```
           Shared Heap                        Erlang Process Memory Layout                  
                                                                                            
+----------------------------------+      +----------------------------------+              
|                                  |      |                                  |              
|                                  |      |  PID / Status / Registered Name  |       Process
|                                  |      |                                  |       Control
|                                  |      |   Initial Call / Current Call    +---->  Block  
|                                  |      |                                  |       (PCB)  
|                                  |      |         Mailbox Pointers         |              
|                                  |      |                                  |              
|                                  |      +----------------------------------+              
|                                  |      |                                  |              
|                                  |      |        Function Parameters       |              
|                                  |      |                                  |       Process
|                                  |      |         Return Addresses         +---->  Stack  
|                                  |      |                                  |              
|    +--------------+              |      |         Local Variables          |              
|    |              |              |      |                                  |              
|    | +------------+--+           |      +-------------------------------+--+              
|    | |               |           |      |                               |  |              
|    | | +-------------+--+        |      |  ^                            v  +---->  Free   
|    | | |                |        |      |  |                               |       Space  
|    | | | +--------------+-+      |      +--+-------------------------------+              
|    +-+ | |                |      |      |                                  |              
|      +-+ |  Refc Binary   |      |      |  Mailbox Messages (Linked List)  |              
|        +-+                |      |      |                                  |              
|          +------^---------+      |      |  Compound Terms (List, Tuples)   |       Process
|                 |                |      |                                  +---->  Private
|                 |                |      |     Terms Larger than a word     |       Heap   
|                 |                |      |                                  |              
|                 +--+ ProcBin +-------------+ Pointers to Large Binaries    |              
|                                  |      |                                  |              
+----------------------------------+      +----------------------------------+              
```

* **PCB**: Process Control Block holds some pointers and values about the process, such as its identifier (PID) in Process Table, current status (running, waiting), its registered name, the initial and current call and some pointers to physical incoming messages which is a *Linked List* and is stored in heap.

* **Stack**: It is a downward growing memory area which holds incoming and outgoing parameters, return addresses, local variables and temporary space for evaluating expressions.

* **Heap**: It is an upward growing memory area which holds physical messages of process mailbox, compound terms like [List](--#--), [Tuples](--#--) and [Binaries](--#--) and objects which are larger than a machine word such as floating point numbers. Binary terms which are larger than 64 machine words are not stored in process private heap. They are called *Refc Binary* (Reference Counted Binary) and are stored in a large *Shared Heap* which is accessible by all processes who have the pointer of the Refc Binaries. That pointer is called *ProcBin* and is stored in process private heap.

## GC Details

In order to explain current default Erlang's GC mechanism concisely we can say; it is a *Generational Mark-Sweep* garbage collection that runs inside each Erlang process private heap independently, and also a *Reference Counting* garbage collection occurs for global shared heap.

### Private Heap GC

The GC for private heap is generational. Based on the fact that if an object survives a GC cycle the chances of it becoming garbage in short term is low, generational GC divides the heap into two segments; young and old generations. The young generation is for newly allocated data, and old generation is for the data that have survived an implementation specific number of GC. Also in context of Erlang garbage collection there are two strategies; *Generational* (Minor) and *Fullsweep* (Major). The generational GC just collects the young heap, but fullsweep collect both young and old heap. Now lets review the GC steps in private heap of a newly started Erlang process:

* Process starts with fullsweep strategy and collects garbages this way.
* If the heap data grows above a certain size, GC switches to generational strategy.
* If generational strategy cannot reclaims enough memory, GC reverts to fullsweep again.
* If fullsweep strategy cannot reclaims enough memory, then the heap size is increased.

Each Time GC runs inside process, it stops the Erlang process being collected, but because each process has its own private heap, this action doesn't stop other processes, and this is what a soft realtime system needs.

### Shared Heap GC

The GC for shared heap is reference counting. Each object in shared heap (Refc) has a counter of references to it held by other objects (ProcBin) which are stored inside private heap of Erlang processes. If an object's reference count reaches zero, the object has become inaccessible and will be destroyed.

In shared heap GC there are some well-known bad situations that can cause memory leak.
First when a Refc is splited into a *Sub-Binary*. In order to be cheap; a sub-binary is not a new copy of spilited part of another binary, but just a reference into part of it. However this sub-binary counts as a reference, and you know, it can cause problem when the original binary must hang around for its sub-binary to be collected.
The other known problem is when there is sort of a router process for transfering Refc binary messages. As the router process touches each message, the counter of them increments. So collecting thoes messages depends on router process GC, and because ProcBins are just a pointer hence they are so cheap, it could take so long to happen a GC inside that router process. So the Refc messages remain on shared heap even if they have been collected from other processes.

Of course there are some solutions for Refc binary leak issues which [Fred Hebert](--#--) well explains them in his book; [Erlang in Anger](--#--).

## GC Tips

## Conclusion

## Resources

* [Reference 1](http://www.erlang.org/faq/academic.html)
* [Reference 2](http://lampwww.epfl.ch/resources/lamp/teaching/advancedCompiler/2004/slides/ImplementationOfConcurrency_slides.pdf)
* [Reference 3](http://user.it.uu.se/~jesperw/publications/Wilhelmsson_lic.pdf)
* [Reference 4](http://labouseur.com/courses/erlang/programming-parallel-with-erlang.pdf)
