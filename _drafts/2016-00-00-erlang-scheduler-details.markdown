---
layout: port
title: "Erlang Scheduler Details and How It Can Benefit from CPU Affinity"
date: 2016-00-00
categories: erlang scheduling cpu affinity realtime preemptive
---

There are some internal features that let Erlang to be a Soft Realtime platform. One of them is its Garbage Collection mechanism which I talked about it in my previous article, [Erlang Garbage Collection Details and Why It Matters](--link--). The other one is its Scheduling mechanism that is well worth looking at. In this article I will explain its history, the current status, controlling API and how it can benefit from [CPU Affinity](--link--) to improve the performance.

## What is Scheduling

Generally speaking, Scheduling is a mechanism that assigns works to workers. The works could be a mathematical operation, string processing or data extraction and the workers are resources which could be virtual like [Green Threads](--like--) or physical like [Processor Cores](--link--). A scheduler is what carries out the scheduling activity in a way that maximizes throughput and fairness and minimizes response time and latency. Schdeduling is a main part of mutitasking systems like Operating Systems and Virtual Machines and is divided into two types:

* [Preemtive](--link--): A Preemptive Scheduler does context switching among running tasks and has the power to preempt (interrupt) tasks and resume them at a later time without the cooperation of the preempted tasks. This is done based on some factors like their priority, time slice or reductions.

* [Cooperative](--link--):  A Cooperative Scheduler needs tasks' cooperation for context switching. This way the scheduler simply lets running tasks to voluntarily release control periodically or when idle, then starts a new task and again waits for it to return the control back voluntarily.

Now the question is what scheduling mechanism is suitable for Realtime systems which must response within a specified time. Cooperative Scheduling system cannot satisfy a Realtime system because a running task in such system might never return control back or returns late after a deadline. So Realtime systems commonly use Preemptive Scheduling.

## Erlang Scheduling

Erlang as a Realtime platform for multitasking uses Preemtive Scheduling. The responsiblity of an Erlang Scheduler is selecting a [Process](--link--) and executing their code. It also do Garbage Collection and Memory Management. The factor of selecting a process for execution is based on their priority level which is configurable per process. In the other hand the factor of preemting a process from execution is based on a certain number of **Reductions** since the last time it was selected for execution, regardless of its priority level. The reduction is a counter per process that is normally incremented by one for each function call. It is used for preempting processes and context switching them when the counter of a process reaches the maximum number of reductions. For example in Erlang/OTP R12B this maximum number was 2000 reductions.

The scheduling of tasks in Erlang has a long history. It has been changing over the time. These changes were affected by the changes in SMP (Symmetric Multi-Processing) feature of Erlang.

### Scheduling Before R11B

Before R11B Erlang didn't have SMP support, so just one scheduler was run in the main OS process's thread and accordingly just one **Run Queue** existed. The scheduler picked runnable Erlang processes and IO tasks from the run queue and executed them.

```
                         Erlang VM

+--------------------------------------------------------+
|                                                        |
|  +-----------------+              +-----------------+  |
|  |                 |              |                 |  |
|  |    Scheduler    +-------------->     Task # 1    |  |
|  |                 |              |                 |  |
|  +-----------------+              |     Task # 2    |  |
|                                   |                 |  |
|                                   |     Task # 3    |  |
|                                   |                 |  |
|                                   |     Task # 4    |  |
|                                   |                 |  |
|                                   |     Task # N    |  |
|                                   |                 |  |
|                                   +-----------------+  |
|                                   |                 |  |
|                                   |    Run Queue    |  |
|                                   |                 |  |
|                                   +-----------------+  |
|                                                        |
+--------------------------------------------------------+
```

This way there was no need to lock data structures but the written application couldn't take advantage of parallelism.

### Scheduling In R11B and R12B

SMP support was added to Erlang VM so it could have 1 to 1024 schedulers each was run in one OS process's thread. However in this version schedulers could pick runnable tasks from just one common run queue.

```
                         Erlang VM

+--------------------------------------------------------+
|                                                        |
|  +-----------------+              +-----------------+  |
|  |                 |              |                 |  |
|  |  Scheduler # 1  +-------------->     Task # 1    |  |
|  |                 |    +--------->                 |  |
|  +-----------------+    |    +---->     Task # 2    |  |
|                         |    |    |                 |  |
|  +-----------------+    |    |    |     Task # 3    |  |
|  |                 |    |    |    |                 |  |
|  |  Scheduler # 2  +----+    |    |     Task # 4    |  |
|  |                 |         |    |                 |  |
|  +-----------------+         |    |     Task # N    |  |
|                              |    |                 |  |
|  +-----------------+         |    +-----------------+  |
|  |                 |         |    |                 |  |
|  |  Scheduler # N  +---------+    |    Run Queue    |  |
|  |                 |              |                 |  |
|  +-----------------+              +-----------------+  |
|                                                        |
+--------------------------------------------------------+
```

Because of the resulting parallelism of this method, all shared data structures are protected with locks. For example the run queue itself is a shared data structure which must be protected. Although the lock can provide performance penalty, the performance improvements which was achived in multi-core processors systems was interesting.

Some known bottlenecks in this version was as follows:
* The common run queue become a bottleneck when the number of schedulers increases.
* Increasing the involved lock of ETS tables which also affects Mnesia.
* Increasing the lock confilicts when many processes are sending messages to the same process.
* A process waiting to get a lock can block its scheduler.

However seperating run queues per scheduler was picked to solve these bottleneck issues in next versions.

#### Scheduling After R13B

In this version each scheduler has its own run queue. It decreases the number of lock confilicts in systems with many schedulers on many cores and also improves the overal performace.

```
                         Erlang VM

+--------------------------------------------------------+
|                                                        |
|  +-----------------+-----------------+                 |
|  |                 |                 |                 |
|  |  Scheduler # 1  |  Run Queue # 1  <--+              |
|  |                 |                 |  |              |
|  +-----------------+-----------------+  |              |
|                                         |              |
|  +-----------------+-----------------+  |              |
|  |                 |                 |  |              |
|  |  Scheduler # 2  |  Run Queue # 2  <----> Migration  |
|  |                 |                 |  |     Logic    |
|  +-----------------+-----------------+  |              |
|                                         |              |
|  +-----------------+-----------------+  |              |
|  |                 |                 |  |              |
|  |  Scheduler # N  |  Run Queue # N  <--+              |
|  |                 |                 |                 |
|  +-----------------+-----------------+                 |
|                                                        |
+--------------------------------------------------------+

```

This way the locking confilicts when accessing the run queue is solved but introduces some new concerns:

* How fair is the process of dividing tasks among run queues?
* What if one scheduler gets overloaded with tasks while others are idle? 
* Based on what order a scheduler can steal tasks from an overloaded scheduler?
* What if we started many schedulers but there all so few tasks to do?

These concerns lead the Erlang team to introduce a concept for making scheduling fair and efficient, the **Migration Login**. It tries to control and balance run queues based on the statistics that collects from the system.

{ controlling api }
{ what is cpu affinity }
{ how to benefit from cpu affinity }
{ cpu affinity cautions }
{ conclusion }

## Resources

* http://www.erlang.org/doc/man/erl.html
* https://vimeo.com/113483904
* http://jlouisramblings.blogspot.co.uk/2013/01/how-erlang-does-scheduling.html
* http://www.erlang.org/euc/08/euc_smp.pdf
* http://developer.vz.net/2009/07/30/about-erlangotp-and-multi-core-performance-in-particular-kenneth-lundin/
* http://www.erlang-factory.com/upload/presentations/708/HitchhikersTouroftheBEAM.pdf
* http://erlang.2086793.n4.nabble.com/Some-facts-about-Erlang-and-SMP-td2108770.html
* http://erlang.org/pipermail/erlang-questions/2001-April/003132.html
* http://www.erlang-factory.com/upload/presentations/389/EFSF11-ErlangVM.pdf
