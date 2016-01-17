---
layout: port
title: "Erlang Scheduler Details and How It Can Benefit from CPU Affinity"
date: 2016-00-00
categories: erlang scheduling cpu affinity realtime preemptive
---

There are some internal features that let Erlang to be a Soft Realtime platform. One of them is its Garbage Collection mechanism which I talked about it in my [previous article](--link--). The other one is its Scheduling mechanism that is well worth looking at. In this article I will explain its history, the current status, controlling API and how it can benefit from [CPU Affinity](--link--) to improve the performance.

## What is Scheduling

Generally speaking, Scheduling is a mechanism that assigns works to workers. The works could be a mathematical operation, string processing or data extraction and the workers are resources which could be virtual like [Green Threads](--like--) or physical like [Processor Cores](--link--). A scheduler is what carries out the scheduling activity in a way that maximizes throughput and fairness and minimizes response time and latency. Schdeduling is a main part of mutitasking systems like Operating Systems and Virtual Machines and is divided into two types:

* [Preemtive](--link--): A Preemptive Scheduler does context switching among running tasks and has the power to preempt (interrupt) tasks and resume them at a later time without the cooperation of the preempted tasks. This is done based on some factors like their priority, time slice or reductions.

* [Cooperative](--link--):  A Cooperative Scheduler needs tasks' cooperation for context switching. This way the scheduler simply lets running tasks to voluntarily release control periodically or when idle, then starts a new task and again waits for it to return the control back voluntarily.

Now the question is what scheduling mechanism is suitable for Realtime systems which must response within a specified time. Cooperative Scheduling system cannot satisfy a Realtime system because a running task in such system might never return control back or returns late after a deadline. So Realtime systems commonly use Preemptive Scheduling.

## Erlang Scheduling

Erlang as a Realtime platform for multitasking uses Preemtive Scheduling. The responsiblity of an Erlang Scheduler is selecting a [Process](--link--) and executing their code. It also do Garbage Collection and Memory Management. The factor of selecting a process for execution is based on their priority level which is configurable per process. In the other hand the factor of preemting a process from execution is based on a certain number of *reductions* since the last time it was selected for execution, regardless of its priority level. The reduction is a counter per process that is normally incremented by one for each function call. It is used for preempting processes and context switching them when the counter of a process reaches the maximum number of reductions. For example in Erlang/OTP R12B this maximum number was 2000 reductions.

### History

The scheduling of tasks in Erlang has a long history. It has been changing over the time.

{ ... }

### Current Status

{ ... }

### Migration Mechanism

{ ... }

### Controlling API

{ ... }

{ what is cpu affinity }
{ how to benefit from cpu affinity }
{ cpu affinity cautions }
{ conclusion }

## Resources

* erlang schedulers
 * http://www.erlang.org/doc/man/erl.html
 * https://vimeo.com/113483904
 * http://jlouisramblings.blogspot.co.uk/2013/01/how-erlang-does-scheduling.html
 * http://www.erlang.org/euc/08/euc_smp.pdf
 * http://developer.vz.net/2009/07/30/about-erlangotp-and-multi-core-performance-in-particular-kenneth-lundin/
 * http://www.erlang-factory.com/upload/presentations/708/HitchhikersTouroftheBEAM.pdf
 * http://erlang.2086793.n4.nabble.com/Some-facts-about-Erlang-and-SMP-td2108770.html
 * http://erlang.org/pipermail/erlang-questions/2001-April/003132.html
