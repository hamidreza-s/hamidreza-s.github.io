---
layout: post
title: "Writing RESTful Web Services with Webmachine"
date: 2014-09-11 12:30:00
categories: erlang restful webservice webmachine
---

Today I want to talk about RESTful Web Services and how to create one with Webmachie, which is a REST toolkit, written in Erlang programming language. The reason which attracted me to write this post was the unique approaches that I've found in the Webmachine both syntatically and semantically. To be succinct, let's get our hands dirty with it. 

## What is REST
What is all the fuss around the REST? It is an acronym of REpresentional State Transfer and can be defined as an architectural style that can be applied to all the entities on the web including __components__, __connectors__ and __data elements__. Simply put, a component can be a web client with an graphical interface for providing data elements from a remote component which can be a web application. These components communicate through a connector. Therefore the connector can monitor, facilate, balance the load or make other controling actions.

In order to call a system as a RESTful system, it must be compatible with some architectural properties and constrants which I explain in next section.

### Architectural Properties
Some of its key properties are as follows:

- __Performance__: Trying to use some techniques to increase the performance, like caching unchanged server responses or making each request processing as single-responsible as possible.

- __Scaliblity__: Designing the API in a stateless manner, in a way that the session data is contained in each request. So the connector can be scalled out without any trouble

- __Simplicity__: Making everything as simple and seperate as possible. In this way the connectors and components can talk to each other simply while there is no sharing of internal details.

- __Visibility__: Making the API visible to such an extent that the connectors and middlewares can involve and undrestand the content of interaction for monitoring and regulating it.s

- __Portability__, Designing you system in a way that you can move your components from one deployed location to another without any trouble in building and running it.


### Architectural Constraints
The architectural properties of REST can be acheived by applying some constraints when designin a RESTful system. I mention some of key constraints of it:

- __Client-Server__: The components must be seperated to clients and servers, to achieve the seperation of concerns, thereby supporting the independent development of client-side logic and server-side logic.

- __Stateless__: The clients and servers must talk to each other through a stateless communication, meaning no client context being stored in the server. So the session state is held in the client and is sent with every request.

- __Cacheable__: The clients must be able to cache the server responses in order to prevent reusing stale or inappropirate data. In this way we can improve the overal system performance by eliminating not nessecary intractions.

- __Layerd System__: In a layerd system, no one layer can see past the next. They can be added, removed or modified transparently as a middleware, and each layer cannot ordinarily tell wheter it is connected directly to an end server or a middleware.

- __Uniform Interface__: Each component must share a uniform interface, free of business context, in order to be reusable by a wide range of other components. So it must be standard, generic, high-level and abstract enough like HTTP methods, URL syntax and MIME types.

## What is Webmachine
webmachine is ...

### Webmachine feature one
feature one ...

### Webmachine feature two
feature two ...

### Webmachine tutorial
tutorial ...

