---
layout: post
title: "to be defined"
date: 2014-03-31 17:30:00
categories: erlang distributed protocol java jinterface performance benchmark database
---

When it comes to architect a system that must store and retrieve an ever-growing set of data, say billions of key-value, time-series or graph data records, what technology can rescue us from drowning under this bunch of data? A gunior web developer just suggest MongoDB or CouchDB. An Erlang fan may offer a combination of Riak and Mnesia. Your old fellow may suggest to use RDBMSes and a young one urge you to use Cassandra, Hbase, ArangoDB and such. All of their ideas can be fit to your problem, and what I know is to consider all, because each of them is an answer to a specific problem.

Now imagine that we select Riak for key-values, Cassandra for time-series, Mnesia for distributed in-memory caching, ArangoDB for graphs and Postgresql for transactions. They must work seamless in their jobs, but suddenly we face to a big problem; their **client drivers**. Riak speaks in Protocol-Buffers (and HTTP), Cassandra spoke in Thrift but suggets to use its own binary protocol, and also ArangoDB and Postgresql have their own protocol. And this is **the mess we are in; several divergent and different protocols**.

However as a Java developer you don't have much problem, because almost all of those databases have native Java driver. But if you code in a language with a small community and low adoption rate like Erlang, you must select between these options:

- Using not reliable third-party drivers (if exists):
This option redeuce the cost and time of developing but may not provide all the API and features of the database. And also you are tightly dependent to its developer, say someone that may die in a car-crash next week.

- Forking a third-party drivers or writing it from scratch:
This is a good option if you have enough time and can afford the cost of continuously developing, updating and maintaing your driver along with its database updates.

- Using a native driver and then making connection to it:
I mean a driver that is being formally supported by the database vendor or a stable community of developers. This option provides all the API and features of the database and also is reliable and has good enough performance. Also redeuces the time of developing, but note that you need to be a polyglot and multi platform developer because you must code in different languages.

Not as the best choice, but I choose the last option because I need a reliable way, which doesn't take so much time and energy and has good enough performance. Now we need to make a connection between our application which is writtern in Erlang and a database like Cassandra which has a reliable Java driver.

## Erlang meets Java

## Jinterface

### Features

### Performance

## Conclusion
