---
layout: post
title: "Writing RESTful Web Services with Webmachine"
date: 2014-09-11 12:30:00
categories: erlang restful webservice webmachine
---

Today I want to talk about [RESTful](https://en.wikipedia.org/wiki/Representational_state_transfer) Web Services and how to create one with [Webmachie](https://github.com/basho/webmachine), which is a REST toolkit, written in [Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language)) programming language. The reason which attracted me to write this post was the unique approaches that I've found in the Webmachine both syntactically and semantically. To be succinct, let's get our hands dirty with it. 

## What is REST
What is all the fuss about the REST? It is an acronym of REpresentational State Transfer and can be defined as an architectural style that can be applied to all the entities on the web including __components__, __connectors__ and __data elements__. Simply put, a component can be a web client with a graphical interface for providing data elements from a remote component which can be a web application. These components communicate through their connectors. Therefore the connector can monitor, facilitate, balance the load or make other controlling actions.

In order to call a system RESTful, it must be compatible with some architectural properties and constraint which I'll explain in the next section.

### Architectural Properties
Some of its key properties are as follows:

- __Performance__: Trying to use some techniques to increase the performance, like caching unchanged server responses or making each request processing as single-responsible as possible.

- __Scaliblity__: Designing the API in a stateless manner, in a way that the session data is contained in each request. So the connector can be scaled out without any troubles.

- __Simplicity__: Making everything as simple and separate as possible. In this way the components can talk to each other simply while there is no sharing of internal details.

- __Visibility__: Making the API visible to such an extent that the connectors and middlewares can involve and understand the content of interaction for monitoring and regulating it.

- __Portability__, Designing you system in a way that you can move your components from one deployed location to another without any trouble in building and running it.

### Architectural Constraints
The architectural properties of REST can be achieved by applying some constraints when designing a RESTful system. I'll mention some of key constraints of it:

- __Client-Server__: The components must be separated to clients and servers, to achieve the separation of concerns, thereby supporting the independent development of client-side logic and server-side logic.

- __Stateless__: The clients and servers must talk to each other through a stateless communication, meaning no client context being stored in the server. So the session state is held in the client and is sent with every request.

- __Cacheable__: The clients must be able to cache the server responses in order to prevent reusing stale or inappropriate data. In this way we can improve the overal system performance by eliminating not nessecary intractions.

- __Layerd System__: In a layered system, no one layer can see past the next. They can be added, removed or modified transparently as a middleware, and each layer cannot ordinarily tell wheter it is connected directly to an end server or a middleware.

- __Uniform Interface__: Each component must share a uniform interface, free of business context, in order to be reusable by a wide range of other components. So it must be standard, generic, high-level and abstract enough like HTTP methods, URL syntax and MIME types.

## What is Webmachine
Webmachine is neither a web framework, nor a web server. The smart folks on [Basho Technologies](https://en.wikipedia.org/wiki/Basho_Technologies) who have created webmachine, called it a __REST Toolkit__. It uses [Mochiweb](https://github.com/mochi/mochiweb) as a web server and provides an extra layer over it to serve web resources as data elements in the RESTful dialect.

The web applications that we use every day are mostly a limited set of graphical interfaces to get their unlimited data resources from their servers. So web resources are first-class objects on the web. You know what they are; the videos, pictures, news, tweets or every data that you get from the web are all web resources.

### How to implement Webmachine
To be compliant with REST architectural constraints, especially the Uniform Interface item, Webmachine uses [HTTP](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol) semantic for __Method Calls__, [URL](https://en.wikipedia.org/wiki/Uniform_resource_locator) for __Resource Identifier Syntax__, and [MIME Types](https://en.wikipedia.org/wiki/Internet_media_type) for __Media Types__. All of them are standard and generic. As an example, for getting the current weather of world's countries with JSON MIME Type, we use the following request.

```bash
$ telnet example.com 80
> 
> GET weather/country HTTP/1.1
> Accept: application/json
>
<
< HTTP/1.1 200 OK
< Vary: Accept
< Server: MochiWeb/1.1 WebMachine/1.10
< Content-type: application/json
< 
```

Now let's create a web service to serve such a response. Use [this](https://github.com/basho/webmachine/wiki/Quickstart) quickstart for creating the base Webmachine skeleton.
Then add your route to priv/dispatch.conf file:

```erlang
{["weather", "country"], weather_country, []}
```

We've just add a dispatch route and a dispatcher module with the name of weather_country. Now we can implement it:

```erlang
-module(weather_country).
-export([init/1,
	allowed_methods/2,
	content_types_provided/2,
	generate_etag/2,
	to_json/2,
	to_yaml/2,
	to_html/2]).
	
%%%%%%%%%%%%%%%%%%%%%%%%
%%% public functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->

	%% initialize state
	State = [],
	
	{ok, State}.


allowed_methods(Request, State) ->

	%% set allowed request methods
	AllowedMethods = ['GET', 'POST'],

	{AllowedMethods, Request, State}.


generate_etag(Request, State) ->
	
	{ok, Etag} = get_weather_by_country_etag(),
	
	{Etag, Request, State}.


content_types_provided(Request, State) ->

	%% map requested content types with its functions
	ContentTypes = [
		{"application/json", to_json},
		{"text/yaml", to_yaml}
	],

	{ContetTypes, Request, State}.

	
to_json(Request, State) ->

	%% get list and format it to JSON
	{ok, List} = get_weather_by_country(),
	Response = my_formatter:list_to_json(List),
	
	{Response, Request, State}.


to_yaml(RequestData, State) ->

	%% get list and format it to YAML
	{ok, List} = get_weather_by_country(),
	Response = my_formatter:list_to_yaml(List),
	
	{Response, Request, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

get_weather_by_country() ->
	
	%% fetch raw data from database
	Result = my_data:weather_by_country(),
	
	{ok, Result}.
	
get_etag() ->

	%% get entity tag of data
	Etag = my_data:weather_by_country_etag(),
	
	{ok, Etag}.
```

Notice how it uses [Etag](https://en.wikipedia.org/wiki/HTTP_ETag) for response caching or content types for resource mapping.
I used this simple module to show you how it works in the simplest form. However you can create enterprise web services, which is compliant with REST constraints, with it in a same way. Also you can find a detailed tutorial in [Webmachine's GitHub repo](https://github.com/basho/webmachine/wiki/Demo).

### Webmachine is functional
As you've just seen, Webmachine is written in Erlang, a functional programming language. Now the question; what benefit does it have for us?
One of the key principles of functionl programming languages is [Referential Transparency](https://en.wikipedia.org/wiki/Referential_transparency_(computer_science\)) for its functions. It says that every referential transparent function returns the same result for a same input at any point in time. So there is no [Global Variables](https://en.wikipedia.org/wiki/Global_variable) or [Mutator Methods](https://en.wikipedia.org/wiki/Mutator_method) that can change the output of a same input in subsequent function calls, so testing and debugging it becomes simpler. 

Let's list some main benefits of it:

- Simple Testing: There is no need to mock and fake objects for database connections and such things.
- Simple Debugging: Each function has its own responsibility and therfore is simpler to reason, understand and debug.
- Low Cost: The lack of extra layers of Object Oriented structures helps it to be cheaper.
- Robust Platform: It's worth to say that Erlang itself has many benfits for you like concurrency and fault-tolerancy that need extra articles.

## Conclusion
Although REST is not totally perfect and has its own drawbacks, its advantages bring us performance, scalibility, simplicity, and some other good thins. Implementing a RESTful web service with most of the common web frameworks completely depends on the developer, but in Webmachine it is a must. No matter what data elements you have, you must be compliant with REST standards when you are implementing it.
