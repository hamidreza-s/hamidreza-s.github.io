---
layout: post
title: "Erlang Scheduler Details and Why It Matters"
date: 2016-02-09
categories: erlang scheduling real-time preemptive migration
---

There are some underlying features that make Erlang a soft real-time platform. One of them is its Garbage Collection mechanism which I talked about it in my previous article, [Erlang Garbage Collection Details and Why It Matters](https://hamidreza-s.github.io/erlang%20garbage%20collection%20memory%20layout%20soft%20realtime/2015/08/24/erlang-garbage-collection-details-and-why-it-matters.html). The other one is its scheduling mechanism that is well worth looking at. In this article I will explain its history, the current status, controlling and monitoring API.

## What is Scheduling

Generally speaking, scheduling is a mechanism that assigns works to workers. The works could be a mathematical operation, string processing or data extraction and the workers are resources which could be virtual like [Green Threads](https://en.wikipedia.org/wiki/Green_threads) or physical like [Native Threads](https://en.wikipedia.org/wiki/Thread_%28computing%29). A scheduler is what carries out the scheduling activity in a way that maximizes throughput and fairness and minimizes response time and latency. Scheduling is a main part of multitasking systems like Operating Systems and Virtual Machines and is divided into two types:

* [Preemptive](https://en.wikipedia.org/wiki/Preemption_%28computing%29): A preemptive scheduler does context switching among running tasks and has the power to preempt (interrupt) tasks and resume them at a later time without the cooperation of the preempted tasks. This is done based on some factors like their priority, time slice or reductions.

* [Cooperative](https://en.wikipedia.org/wiki/Cooperative_multitasking):  A Cooperative scheduler needs tasks' cooperation for context switching. This way the scheduler simply lets running tasks to voluntarily release control periodically or when idle, then starts a new task and again waits for it to return the control back voluntarily.

Now the question is what scheduling mechanism is suitable for real-time systems which must response within a specified time. Cooperative Scheduling system cannot satisfy a real-time system because a running task in such system might never return control back or returns late after a deadline. So real-time systems commonly use Preemptive Scheduling.

## Erlang Scheduling

Erlang as a real-time platform for multitasking uses Preemptive Scheduling. The responsibility of an Erlang scheduler is selecting a [Process](http://erlang.org/doc/reference_manual/processes.html) and executing their code. It also does Garbage Collection and Memory Management. The factor of selecting a process for execution is based on their priority level which is configurable per process and in each priority level processes are scheduled in a round robin fashion. In the other hand the factor of preempting a process from execution is based on a certain number of **Reductions** since the last time it was selected for execution, regardless of its priority level. The reduction is a counter per process that is normally incremented by one for each function call. It is used for preempting processes and context switching them when the counter of a process reaches the maximum number of reductions. For example in Erlang/OTP R12B this maximum number was 2000 reductions.

The scheduling of tasks in Erlang has a long history. It has been changing over the time. These changes were affected by the changes in SMP (Symmetric Multi-Processing) feature of Erlang.

### Scheduling Before R11B

Before R11B Erlang did not have SMP support, so just one scheduler was run in the main OS process's thread and accordingly just one **Run Queue** existed. The scheduler picked runnable Erlang processes and IO tasks from the run queue and executed them.

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

SMP support was added to Erlang VM so it could have 1 to 1024 schedulers each was run in one OS process's thread. However, in this version schedulers could pick runnable tasks from just one common run queue.

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

Because of the resulting parallelism of this method, all shared data structures are protected with locks. For example the run queue itself is a shared data structure which must be protected. Although the lock can provide performance penalty, the performance improvements which was achieved in multi-core processors systems was interesting.

Some known bottlenecks in this version was as follows:

* The common run queue becomes a bottleneck when the number of schedulers increases.
* Increasing the involved lock of ETS tables which also affects Mnesia.
* Increasing the lock conflicts when many processes are sending messages to the same process.
* A process waiting to get a lock can block its scheduler.

However, separating run queues per scheduler was picked to solve these bottleneck issues in next versions.

### Scheduling After R13B

In this version each scheduler has its own run queue. It decreases the number of lock conflicts in systems with many schedulers on many cores and also improves the overall performance.

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

This way the locking conflicts when accessing the run queue is solved but introduces some new concerns:

* How fair is the process of dividing tasks among run queues?
* What if one scheduler gets overloaded with tasks while others are idle? 
* Based on what order a scheduler can steal tasks from an overloaded scheduler?
* What if we started many schedulers but there all so few tasks to do?

These concerns lead the Erlang team to introduce a concept for making scheduling fair and efficient, the **Migration Logic**. It tries to control and balance run queues based on the statistics that collects from the system. 

However we should not depend on the scheduling to remain exactly as it is today, because it is likely to be changed in future releases in order to get better.

## Controlling and Monitoring API

There are some Erlang emulator flags as well as internal controlling and monitoring functions for the scheduling behaviours.

### Scheduler Threads

The number of maximum available scheduler threads and online scheduler threads can be specified by passing two colon-seperated numbers to `+S` flag when booting Erlang emulator with `erl` start script. 

```shell
$ erl +S MaxAvailableSchedulers:OnlineSchedulers
```

The number of maximum available scheduler threads just can be specified at boot time and is fixed during the run time, but the number of online scheduler threads can be specified and changed in boot time as well as run time. For example we can start an emulator with 16 schedulers from which 8 schedulers are online.

```shell
$ erl +S 16:8
```
Then inside shell the online scheduler threads can be changed as follows.

```erlang
> erlang:system_info(schedulers). %% => returns 16
> erlang:system_info(schedulers_online). %% => returns 8
> erlang:system_flag(schedulers_online, 16). %% => returns 8
> erlang:system_info(schedulers_online). %% => returns 16
```

Also, using `+SP` flag they can be set by percentages.

### Process Priority

As I said before schedulers select a process for execution based on their priority level. The priority can be specified inside a process by calling `erlang:process_flag/2` function.

```erlang
PID = spawn(fun() ->
   process_flag(priority, high),
   %% ...
   end).
```

The priority can be any of `low | normal | high | max` atom. The default priority level is `normal` and the `max` is reserved for internal use in Erlang runtime and should not be used by others.

### Run Queue Statistics

As I explained before run queues hold the processes which are ready for execution before picking them by schedulers. It is possible to get the count of all processes which are ready for execution on all available run queues by `erlang:statistics(run_queue)` function. As a real example let's boot Erlang emulator with 4 online schedulers and assign them 10 heavy CPU-bound processes concurrently. This process could be [calculating prime numbers](https://gist.github.com/hamidreza-s/9e3ed289f65759048875) up to a big number.

```erlang
%% Everything is clean and ready
> erlang:statistics(online_schedulers). %% => 4
> erlang:statistics(run_queue). %% => 0

%% Spawn 10 heavy number crunching processes concurrently
> [spawn(fun() -> calc:prime_numbers(10000000) end) || _ <- lists:seq(1, 10)].

%% Run queues have remaining tasks to do
> erlang:statistics(run_queue). %% => 8

%% Erlang is still responsive, great!
> calc:prime_numbers(10). %% => [2, 3, 5, 7]

%% Wait a moment
> erlang:statistics(run_queue). %% => 4

%% Wait a moment
> erlang:statistics(run_queue). %% => 0
```

Because the number of concurrent processes are more than online schedulers, it takes time for schedulers to execute all the processes in run queues and make them empty. The interesting thing here is that after spawning those heavy processes, the Erlang emulator is still responsive because of its preemptive scheduling. It doesn't let those heavy and rogue processes to chew all the runtime out without letting other light but likely important processes to execute, and it is a great feature when it comes to implementing a real-time system.

## Conclusion

Although implementing a preemptive scheduling system could be complex but in case of Erlang it is not the responsibility of developer because the feature is inside the virtual machine. In the other hand the extra processing cost of tracking, balancing, selecting, executing, migrating and preempting processes is totally affordable when the system needs to scale on all processing resources with a high level of fairness and also responds timely in a real-time system. By the way it is worth mentioning that fully preemptive scheduling is a feature that nearly all operating systems support, but in case of high level platforms, languages or libraries it can be claimed that Erlang virtual machine is almost unique because JVM threads depend on operating system schedulers, [CAF](http://actor-framework.org/) which is a C++ actor library uses cooperative scheduling, Golang is not fully preemptive and it also applies to Python's Twisted, Ruby's Event Machine and Nodejs. It doesn't mean that it is the best option for every possible challenge, it means that if we need to implement a real-time system with low latency Erlang can be a good option.

## Resources

* [Official Documentation for erl script](http://www.erlang.org/doc/man/erl.html)
* [Official Documentation for erlang module](http://erlang.org/doc/man/erlang.html)
* [How Erlang Does Scheduling](http://jlouisramblings.blogspot.co.uk/2013/01/how-erlang-does-scheduling.html)
* [Inside the Erlang VM](http://www.erlang.org/euc/08/euc_smp.pdf)
* [Erlang Scheduler: What Does It Do](http://erlang.org/pipermail/erlang-questions/2001-April/003132.html)
