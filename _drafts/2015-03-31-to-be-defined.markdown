---
layout: post
title: "Erlang meets Java - A Performance-centric Review"
date: 2014-03-31 17:30:00
categories: IDL erlang distributed protocol java jinterface performance benchmark database
---

When it comes to architect a system that must store and retrieve an ever-growing set of data, say billions of key-values, time-series or graph data records, what technology can rescue us from drowning under this bunch of data? A gunior web developer just suggest MongoDB or CouchDB. An Erlang fan may offer a combination of Riak and Mnesia. Your old fellow may suggest to use RDBMSes and a young one urge you to use Cassandra, Hbase, ArangoDB and such. All of their ideas can be fit to your problem, and what I know is to consider all, because each of them is an answer to a specific problem.

Now imagine that we select Riak for key-values, Cassandra for time-series, Mnesia for distributed in-memory caching, ArangoDB for graphs and Postgresql for transactions. They must work seamless in their jobs, but suddenly we face to a big problem; their **client drivers**. Riak speaks in [Protocol-Buffers](--link--) (and HTTP), Cassandra spoke in [Thrift](--link--) but now it suggets to use its own binary protocol, and also ArangoDB and Postgresql have their own protocol. And this is **the mess we are in; several divergent and different protocols**.

However as a Java developer you don't have much problem, because almost all of those databases have official Java driver. But if you code in a language with a small community and low adoption rate like Erlang, you must select between these options:

- Using not reliable third-party drivers (if exists):
This option redeuces the cost and time of developing but may not provide all the API and features of the database. And also you are tightly dependent to its developer, say someone that may die in a car-crash next week.

- Forking a third-party driver or writing it from scratch:
This is a good option if you have enough time and can afford the cost of continuously developing, updating and maintaing your driver along with its database updates.

- Using an official driver and then making connection to it:
I mean a driver that is being officially supported by the database vendor or a stable community of developers. This option provides all the API and features of the database and also is reliable and has good enough performance. Also redeuces the time of developing, but note that you need to be a polyglot and multi platform developer because you must code in different languages.

Not as the best choice, but I choose the last option because I need a reliable way, which doesn't take so much time and energy and has good enough performance. 

Now imagine a scenario in which we use Erlang as a base platform/language and want to store time-series data on Cassandra and retrieve them frequently. Let's pick [Datastax Java Client Driver](--link--) of Cassandra and then what we need is making a connection between our application which is writtern in Erlang and the Java client.

## Erlang meets Java

Connection between languages is not a new thing that I've found in my scenario; this is a common problem that most languages try to solve it. One of the Erlang/OTP answers to this problem is [Jinterface](--link--). Jinterface is a set of Java classes which is used to make communication between Java and Erlang. The transport protocol is [Erlang Distributed Protocol](--link--) and [EPMD](--link--) is resposible for introducing Erlang nodes and Java nodes together, like a name server. The interesting thing here is that the Erlang node doesn't have any idea about Java node internals, so it sends and receives messages from and to it like an Erlang node. So you can replace a Java node with an Erlang node without any side effects.

## Jinterface Performance

What makes me worried always is performance and scalibility. Its scalibility mostly depends on our design, but the internal performance of Jinterface is not. As I saw in Jinterface code, in the time of writting it doesn't use [Java NIO](--link--) and this can be a bottleneck. Without digging deeper into Jinterface code, let's make a performance benchmark for a our specific scenario.

The scenario is sending 1,000,000 messages from Erlang node and echo back that message from Java node as acknowlegement response. The size of each message is 1 KB and is send synchronosly by one thread on both sides.

Machine Info:

- OS: Debian 8
- RAM: 8 GB
- CPU: i5-4570 @ 3.20GHz
- Erlang: R17
- Java: 1.7

Benchmark Result:

- Message Count: 1,000,000
- Each Message Size: 1 KB
- Total Message Size: 1 GB
- Message Rate: 47,344 msg/sec
- Transfer Rate: 47 MB/sec
- Total Time: 21 sec

Note that the result excludes ack messages, which can double __Total Message Size__, __Message Rate__ and __Transfer Rate__.
You can simply make the benchmark yourself with your parameters and machine config by cloning [EdpBench repository](--link--) and running __GNU make__ command.

## Conclusion

The ability to make connection between languages simply, effectivly and without performance penalty is a big advantage. The lack of global specifications and standards in this feild can hurts you when it comes to architect not trivial systems. This condition can be even worse when we have multiple standards that diverge from each other.
However Google's Protocol Buffer, Apache Thrift, Erlang Distributed Protocol or other [Interface Description Languages](--link--) can make this happen to a limited degree.
