---
layout: post
title: "Mnesia Table Types and Time Complexity"
date: 2014-06-10 17:16:00
categories: erlang mnesia time-complixity
---

In this post, I want to introduce [Mnesia](--link--) [Data Base Management System (DBMS)](--link--), its **Lock Mechanism**, **Time Complexity** and **Table Types**. In the last part of this post, I will explain some simple scenarios for using Mnesia in production environments.

## What is Mnesia
Mnesia is an **Embedded**, **Key/Value**, **Distributed** and **Transactional** database which is bundled with [Erlang/OTP](--link--) platform for use in [Soft Real-time](--link--) applications.

It is Embedded in Erlang and you don't need to install it separately. With an Erlang/OTP installed in your machine, you have already had an installed instance of Mnesia.

It is Key/Value, so for storing data in Mnesia all you need is a key and its value. This approach, like other [NoSql](--link--) databases, provides simplicity of design, horizontal scaling and finer control over availability.

It is Distributed and you can run multiple instances of Mnesia in multiple network's nodes and these instances can communicate and coordinate their actions by passing messages. In this way, your distributed system can benefit from concurrency of components, lack of global clock and no single point of failure.

It is Transactional and implements the [ACID](--link--) properties. So Mnesia guarantees that your database transactions run Atomic, Consistent, Isolated and Durable.

One of Mnesia features that has not been talked about very much, is its ability to store Erlang's terms without any transforming or serializing. If you come from an Object-Oriented world and its [Object-Relational Mapping](--link--) systems, you might hear about [Object-Relational Impedance Mismatch](--link--) difficulties. One of the main difficulties caused by it is the data structure differences between your language objects and your database types. However in Mnesia you can store Erlang's terms intact, solving that Impedance Mismatch between language and database.

### Amdahl's Law
Since Mnesia is embedded in Erlang, and in Erlang we can simply spawn thousands of concurrent processes to achieve speedup of program, it is possible to make a mistake to run concurrent transactions for speedup and reducing the time of our program. But in these situations we don't have to forget the [Amdahl's Law](--link--). It says "The speedup of a concurrent program using multiple processors is limited by the time needed for the sequential fraction of the program".
 
With this law in hand, we know that running multiple transactions concurrently but in a same row, can't reduce the running time of the program. The **Atomicity**, **consistency** and **Isolation** properties of Mnesia's transactions would order its _Transaction Manager_ to lock the given row when each transaction wants to access it. So not only the concurrent running of these transactions can't reduce the running time of the program, but also it undertakes extra actions like row-locking, table-locking, process context-switching and synchronization which may eventually run sequentially.

### Lock Mechanism
As mentioned before, Mnesia needs some _Locking Mechanisms_ to achieve Atomicity, Consistency and Isolation properties of ACID. Note that this mechanism is mainly based on **Pessimistic Locking** concepts. It means that whenever _Mnesia Transaction Manager_ wants to access a record, it tries to lock the record or entire table depending upon the _Transaction Context_. These Locking types are as follows:

- **Read row-lock**: Whenever a transaction context includes a read operation on a row, transaction manager locks just that specified row before it can be read.
- **Write row-lock**: Whenever a transaction context includes a write operation on a row, transaction manager locks that row and all of its replicas in other nodes.
- **Read table-lock**: Whenever a transaction traverses an entire table for a record row by row, it is more efficient to lock the entire table instead of multiple locks and releases row by row.
- **Write table-lock**: When a transaction writes a large number of records on a table, it is more efficient to set a write-lock on entire table.
- **Sticky lock**: This is an efficient locking mechanism when we have multiple instances of Mnesia in different nodes. Sticky locks are same as write locks, but they stay on the node that initiated it, instead of spreading across all the nodes.

## Mnesia Table Types
In this part of post, I will explain all _types_ of Mnesia tables, Their _data structure_ and _time complexity_ as well as some sample codes.

### Set and Bag
**Set** type requires that all the keys in the table are unique. So you can't have two records with the same key, but in **Bag** type we can have several records with a same key.

The data structure of these types are based on [Hash Table](--link--) algorithm. According to Time Complexity of Hash Tables in [Big O Notation](--link--) symbolism, we can infer following table of information from it:

| Operation | Average | Worst Case |
|:---------:|:-------:|:----------:|
| Space     | O(n)    | O(n)       |
| Search    | O(1)    | O(n)       |
| Insert    | O(1)    | O(n)       |
| Delete    | O(1)    | O(n)       |

As you see, in average we have a slight _space penalty_ in sets, but for _search_, _insert_ and _delete_ operations it doesn't matter how large table rows could be, and they take place in _constant time_.

### Ordered Set
The **Ordered Set** type is equal to **Set** just it sorts records by their keys. The data structure of it are based on [Balanced Binary Tree](--link--) and Time Complexity of it is as follows:

| Operation | Average  | Worst Case |
|:---------:|:--------:|:----------:|
| Space     | O(n)     | O(n)       |
| Search    | O(log n) | O(n)       |
| Insert    | O(log n) | O(n)       |
| Delete    | O(log n) | O(n)       |

The logarithmic time of _search_, _insert_ and _delete_ means, if we have `n` items in a table, these operations in average takes place in `log n` runnig time. So it has time penalty as opposed to **Set** table type.

### Code Samples
The following sample codes are as simple as possible just for getting the sense of it.

```erlang
-module(set_table). %% defines module
-export([init/1, write/1, read/1]). %% sets public functions
-record(user, {id, username, password}). %% defines user record

%% ------------------------
%% --- initialize table ---
%% ------------------------
init(TableType) ->
	%% create database in current node
	mnesia:create_schema([node()]),
	
	%% starts mneisa database
	mnesia:start(),
	
	%% creates user table based on given type
	{atomic, ok} = mnesia:create_table(user, [
		{attributes, record_info(fields, user)},
		{type, TableType}
	]).
	
%% ------------------------
%% ---- write a record ----
%% ------------------------
write(Record) ->

	%% wraps write operation in a fun
	Write = fun() ->
		mnesia:write(Record)
	end,
	
	%% runs given fun transactionally
	mnesia:transaction(Write).
	
%% ------------------------
%% ---- read a record -----
%% ------------------------
read(Id) ->

	%% wraps read operation in a fun
	Read = fun() ->
		mnesia:read(user, Id)
	end,
	
	%% runs given fun transactionally
	mnesia:transaction(Read).
```

I tried to write self-explanatory codes in order to bypass that boaring line by line explaining.

## Simple Scenarios
{according to joe's book}
