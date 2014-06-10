---
layout: post
title: "Mnesia Table Types and Time Complixity"
date: 2014-06-10 17:16:00
categories: erlang mnesia time-complixity
---

{intro}

## What is Mnesia
{define mnesia}
{history}
{purposes}
{origins (ets)}
{ACID}

### Amdahl's Law
{can't run transaction in parallel}

### Lock Mechanism
{pessimistic lock}
{read, write lock}
{table lock}
{sticky lock}

### Memory Model
{separate area}
{off-heap for large binary records}

## Mnesia Table Types
{intro to its type}

### Set
{set data structure (hash table}}
{time complixity for read and write in best, average and worst case}
{sample code}
{benchmark}

### Ordered Set
{ordered set data structure (balanced binary tree)}
{time complixity for read and write in best, average and worst case}
{sample code}
{benchmark}

### Bag
{bag data structure (hash table)}
{time complixity for read and write in best, average and worst case}
{sample code}
{benchmark}

## Simple Scenarios
{according to joe's book}
