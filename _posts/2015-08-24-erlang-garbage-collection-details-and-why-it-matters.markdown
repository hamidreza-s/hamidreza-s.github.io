---
layout: post
title: "Erlang Garbage Collection Details and Why It Matters"
date: 2015-08-24 20:15:00
category: erlang garbage collection memory layout soft realtime
---

One of the main problems that Erlang tried to solve was creating a platform for implementing [Soft Realtime](https://en.wikipedia.org/wiki/Real-time_computing) systems with a high level of responsiveness. Such systems require a fast [Garbage Collection](https://en.wikipedia.org/wiki/Garbage_collection_%28computer_science%29) mechanism that doesn't stop the system from responding in a timely manner. In other hand Garbage Collection gets more importance when we consider Erlang as an [Immutable](https://en.wikipedia.org/wiki/Immutable_object) language with *Non-destructive Update* property, because there is a high rate of producing garbage in such languages.



## Memory Layout

Before digging into GC, it is essential to inspect the memory layout of an Erlang process which can be divided into three main parts: Process Control Block, Stack and Heap. It is so similar to Unix process memory layout.

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

* **PCB**: Process Control Block holds some information about the process such as its identifier (PID) in Process Table, current status (running, waiting), its registered name, the initial and current call, and also PCB holds some pointers to incoming messages which are members of a *Linked List* that is stored in heap.

* **Stack**: It is a downward growing memory area which holds incoming and outgoing parameters, return addresses, local variables and temporary spaces for evaluating expressions.

* **Heap**: It is an upward growing memory area which holds physical messages of process mailbox, compound terms like [Lists](http://www.erlang.org/doc/man/lists.html), [Tuples](http://www.erlang.org/documentation/doc-5.8/doc/reference_manual/data_types.html) and [Binaries](http://www.erlang.org/doc/man/binary.html) and objects which are larger than a machine word such as floating point numbers. Binary terms which are larger than 64 machine words are not stored in process private heap. They are called *Refc Binary* (Reference Counted Binary) and are stored in a large *Shared Heap* which is accessible by all processes who have the pointer of that Refc Binaries. That pointer is called *ProcBin* and is stored in process private heap.

## GC Details

In order to explain current default Erlang's GC mechanism concisely we can say; it is a *Generational Copying* garbage collection that runs inside each Erlang process private heap independently, and also a *Reference Counting* garbage collection occurs for global shared heap.

### Private Heap GC

The GC for private heap is generational. Generational GC divides the heap into two segments: young and old generations. This separation is based on the fact that if an object survives a GC cycle the chances of it becoming garbage in short term is low. So the young generation is for newly allocated data, and old generation is for the data that have survived an implementation specific number of GC. This separation helps the GC to reduce its unnecessary cycles over the data which have not become garbage yet.
In context of Erlang garbage collection there are two strategies; *Generational* (Minor) and *Fullsweep* (Major). The generational GC just collects the young heap, but fullsweep collect both young and old heap. Now lets review the GC steps in private heap of a newly started Erlang process:


**Scenario 1**:

```
Spawn > No GC > Terminate
```
No GC occurs in a short-lived process which doesn't use heap more that *min_heap_size* and then terminates. This way the whole memory used by process is collected.

**Scenario 2**:

```
Spawn > Fullsweep > Generational > Terminate
```
A newly spawned process whose data grows more that *min_heap_size* uses fullsweep GC, obviously because no GC has occurred yet and so there is no separation between objects as young and old generations. After that first fullsweep GC, the heap is separated into young and old segments and afterward the GC strategy switches to generational and remains on it until the process terminates.

**Scenario 3**:

```
Spawn > Fullsweep > Generational > Fullsweep > Generational > ... > Terminate
```
There are cases in a process lifetime when GC strategy switches from generational back to fullsweep again. First case is after certain number of generational GC occurs. This certain number can be specified globally or per process with *fullsweep_after* flag. Also the counter of generational GC per process and its upper bound before fullsweep GC are *minor_gcs* and *fullsweep_after* properties respectively, and can be seen in return value of *process_info(PID, garbage_collection)*. Second case is when the generation GC cannot collect enough memory and the last case is when the *garbage_collect(PID)* function is called manually. After these cases the GC strategy reverts again from fullsweep to generational and remains on it until aforementioned cases occurs.

**Scenario 4**:

```
Spawn > Fullsweep > Generational > Fullsweep > Increase Heap > Fullsweep > ... > Terminate
```
In scenario 3 if the second fullsweep GC cannot collect enough memory, then the heap size is increased and the GC strategy switches to fullsweep again, like a newly spawned process, and all these four scenarios can be occurred again and again.

Now the question is why it matters in an automatic garbage collected language like Erlang.
Firstly this knowledge can help you to make your system go faster by tuning the GC occurrence and strategy globally or per process. Secondly this is where we can understand one of the main reasons that makes Erlang a soft realtime platform from its garbage collection point of view. This is because each process has its own private heap and its own GC, so each time GC occurs inside a process it just stops the Erlang process which is being collected, but doesn't stop other processes, and this is what a soft realtime system needs.

### Shared Heap GC

The GC for shared heap is reference counting. Each object in shared heap (Refc) has a counter of references to it held by other objects (ProcBin) which are stored inside private heap of Erlang processes. If an object's reference counter reaches zero, the object has become inaccessible and will be destroyed. Reference counting GC is so cheap and helps the system to avoid unexpected long time pauses and boosts the system responsiveness. But being unaware of some well-known anti-patterns in designing your actor model system could make troubles in case of memory leak.

* First when a Refc is splitted into a *Sub-Binary*. In order to be cheap; a sub-binary is not a new copy of splitted part of original binary, but just a reference into that part. However this sub-binary counts as a new reference in addition to the original binary, and you know, it can cause problem when the original binary must hang around for its sub-binary to be collected.

* The other known problem is when there is a sort of long-lived middleware process acting as a request controller or message router for controlling and transferring large Refc binary messages. As this process touches each Refc message, the counter of them increments. So collecting those Refc messages depends on collecting all ProcBin objects even ones that are inside the middleware process. Unfortunately because ProcBins are just a pointer hence they are so cheap and it could take so long to happen a GC inside the middleware process. As a result the Refc messages remain on shared heap even if they have been collected from all other processes, except the middleware.

Shared heap matters because it reduces the IO of passing large binary messages between processes. Also creating sub-binaries are so fast because they are just pointers to another binary. But as a rule of thumb using shortcuts for being faster has cost, and its cost is well architecting your system in a way that doesn't become trapped in bad conditions. Also there are some well-known architectural patterns for Refc binary leak issues which [Fred Hebert](http://ferd.ca/) explains them in his free ebook; [Erlang in Anger](http://www.erlang-in-anger.com/), and I think that I cannot explain it better than him. So I strongly recommend you to read it.

## Conclusion

Even if we are using a language that manages memory itself like Erlang, nothing prevents us from understanding how memory is allocated and deallocated. Unlike [Go Language Memory Model Documentation Page](https://golang.org/ref/mem) that advices "*If you must read the rest of this document to understand the behavior of your program, you are being too clever. Don't be clever.*", I believe that we must be clever enough to make our system faster and safer, and sometimes it doesn't happen unless we dig deeper into what is going on.

## Resources

* [Academic and Historical Questions about Erlang](http://www.erlang.org/faq/academic.html)
* [Implementation of FPL & Concurrency](http://lampwww.epfl.ch/resources/lamp/teaching/advancedCompiler/2004/slides/ImplementationOfConcurrency_slides.pdf)
* [Efficient Memory Management for Message-Passing Concurrency Paper](http://user.it.uu.se/~jesperw/publications/Wilhelmsson_lic.pdf)
* [Programming the Parallel World by Erlang Paper](http://labouseur.com/courses/erlang/programming-parallel-with-erlang.pdf)
