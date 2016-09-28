---
layout: post
disqus: true
title: "Erlang for Web Developers with Leptus"
date:  2014-04-22 10:27:00
categories: erlang web micro-framework leptus tutorial
---

In this article, I will try to introduce [Erlang Programming Language](https://en.wikipedia.org/wiki/Erlang_(programming_language)) not only as a robust networking platform, but also as a handy tool for web developers in their everyday usage. As a practical example for it, I am going to use [Leptus](https://github.com/s1n4/leptus), which is a RESTful Micro-Framework runs on top of [Cowboy](https://github.com/extend/cowboy).

## What is Erlang

Simply put, Erlang is an open-source, 28-years-old networking platform that was born in [Ericsson](http://www.ericsson.com/) with the initial target of making a __reliable__, __distributed__, __concurrent__, and __fault-tolerant__ system for using in Ericsson [ATM](https://en.wikipedia.org/wiki/Asynchronous_Transfer_Mode) switches to achieve wonderful [Nine Nines](https://en.wikipedia.org/wiki/High_availability) (99.9999999%) reliability . It means 1 second downtime during 20 years!

One of the important Erlang advantages is bundling it with [OTP](https://en.wikipedia.org/wiki/Open_Telecom_Platform) framework, which is a set of modules, behaviors and best practices for Erlang developers to get the most possible benefits of this platform for their applications. OTP provides strong serving, supervising and packaging features for your Erlang applications. Cowboy, which is an Erlang Web Server and also the under layer of Leptus, itself is built on OTP.

## What is Leptus

Leptus is a Greek word that means Thin or Slim. Now it is obvious why it is a micro-framework. Leptus is inspired by [Sinatra](http://www.sinatrarb.com/) [Domain Specific Language](https://en.wikipedia.org/wiki/Domain-specific_language), but is powered by Erlang performance. It aims at simply creating RESTful APIs. It has some dependencies but with the help of [Rebar](https://github.com/basho/rebar) which is an Erlang build tool, we can simply get its dependencies and compile them altogether.

## Building a Todo List with Leptus

I am going to create a todo list application to show the simplicity that Leptus offers us for creating web applications. It is a CRUD (Create, Retrieve, Update and Delete) one using `POST`, `GET`, `PUT` and `DELETE` HTTP requests.

### Step 1: Requirements

__Erlang VM__

You won't have any trouble for installing Erlang VM:

_Fedora:_

```bash
$ yum install erlang
```

_Ubuntu:_

```bash
$ apt-get install erlang
```

_Mac:_

```bash
$ brew install erlang
```

__Rebar__

It is possible to download a prebuild version of rebar from [this link](https://github.com/rebar/rebar/wiki/rebar). 

### Step 2: Instruction

Common structure of OTP applications are as follows:

```bash
|-- todo
    |-- deps  # dependencies directory
    |-- ebin  # erlang binary files directory
    |-- priv  # static files directory
    `-- src   # application source files directory
```

So make a new directory somewhere in your machine and call it `todo`, then download Rebar prebuild file on it. Open `todo` directory and after creating necessary directories there, create a new text file and call it `rebar.config`. It is where you put your application's configuration and dependencies. So fill it as follows:

```erlang
{deps, [
	%% {application name, version, {repository, url, branch}}
	{leptus, ".*", {git, "git://github.com/s1n4/leptus.git", "master"}}
]}.
```
Save it and run following command for getting Leptus master branch from github. As Leptus itself has dependency to `cowboy`, `cowlib`, `ranch` and `crypto` libraries, __Rebar__ downloads them altogether for your application.

```bash
$ ./rebar get-deps
```

Now everything is ready for writing the Back-End stuff of todo application.

#### Step 3: Back-End Stuff
In `src` directory create 5 files with the following names:

```bash
`-- src
    |-- todo.app.src      # application's general information
    |-- todo_app.erl      # application's entry point
    |-- todo_handler.erl  # application's route handler
    |-- todo_helper.erl   # application's helper tools
    `-- todo_record.hrl   # application's record definition
```

Let's define the application in `todo.app.src` by following information:

```erlang
%% file: src/todo.app.src
%% ----------------------
{application, todo,
 [
  {description, "A simple todo application"},
  {vsn, "1"}, %% version
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { todo_app, []}},
  {env, []}
 ]}.
```

In the simplest form, our todo record needs `id`, `content`, `priority` and `status`. So let's create it in `todo_record.hrl` as follows:

```erlang
%% file: src/todo_record.hrl
%% -------------------------
-record(todo, {
   id,
   content,
   priority,
   status
}).
```

Now open `todo_app.erl` and define the entry point of your application:

```erlang
%% file: src/todo_app.erl
%% ----------------------
-module(todo_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Include todo record
-include("todo_record.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

   %% Start mnesia database in current node
   %% which is nonode@nohost
   mnesia:create_schema([node()]),
   mnesia:start(),
   
   %% Create mnesia table based on todo record
   %% which is defined in src/todo_records.hrl
   mnesia:create_table(todo, [
      {attributes, record_info(fields, todo)},
      {disc_copies, [node()]} %% disc_copies means persistent
   ]),

   %% Define static directory for application
   Opts = [{static_dir, {'_', {priv_dir, ?MODULE, "templates"}}}],
   
   %% Start Leptus listener and set it to route every requests
   %% to src/todo_handler.erl
   leptus:start_listener(http, [{'_', [{todo_handler, undef}]}], Opts).

stop(_State) ->
   ok.
```

As you can see, `todo_app` module is an OTP's application behavior which must provide two callbacks; `start/2` and `stop/1`. The `start/2` callback is obviously responsible for starting the application. So we initiate necessary stuff in it. For instance we start Mnesia database for storing todo records. Don't worry about it, Mnesia is a Key/Value database which, like OTP, is bundled with Erlang VM by default, So fortunately it doesn't need to install or configure it separately.
After starting Mnesia database we must create todo table, so we use `todo` record which we've just defined in `todo_record.hrl` file.

Now we can start Leptus framework to handle `HTTP` requests from all matched hosts, which specified with `'_'`, via `todo_handler` module. This module can be implemented as follows:

```erlang
%% file: src/todo_handler.erl
%% --------------------------
-module(todo_handler).
-compile({parse_transform, leptus_pt}).

%% Leptus callbacks
-export([init/3]).
-export([cross_domains/3]).
-export([terminate/4]).

%% Leptus routes
-export([get/3]).
-export([post/3]).
-export([put/3]).
-export([delete/3]).

%% Includes
-include("todo_record.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Cross Domain Origin
%% It accepts any host for cross-domain requests
cross_domains(_Route, _Req, State) ->
   {['_'], State}.

%% Start
init(_Route, _Req, State) ->
   {ok, State}.

%% List
get("/todos", _Req, State) ->
   Query = fun() ->
      qlc:e(
         qlc:q([X || X <- mnesia:table(todo)])
      )
   end,
   {atomic, Records} = mnesia:transaction(Query),
   Json = todo_helper:format(Records),
   {200, {json, Json}, State};

%% Retrieve
get("/todo/:id", Req, State) ->
   %% Get ID from query string
   Id = leptus_req:param(Req, id),
   
   %% Fetch record from database
   Query = fun() ->
      mnesia:read(todo, Id)
   end,
   {atomic, Record} = mnesia:transaction(Query),
   
   %% Format record to JSON
   Json = todo_helper:format(Record),
   
   %% Return JSON formated data
   %% with success (200) HTTP status code
   {200, {json, Json}, State}.

%% Create
post("/todo", Req, State) ->
   %% Get POST body query string
   Post = leptus_req:body_qs(Req),
   
   %% Create record ID based on timestamp
   {MegaS, S, MicroS} = erlang:now(),
   Id = list_to_binary(
      integer_to_list(MegaS) ++
      integer_to_list(S) ++
      integer_to_list(MicroS)
   ),
   
   %% Get desired fields from POST
   {<<"content">>, Content} = lists:keyfind(<<"content">>, 1, Post),
   {<<"priority">>, Priority} = lists:keyfind(<<"priority">>, 1, Post),
   {<<"status">>, Status} = lists:keyfind(<<"status">>, 1, Post),
   
   %% Write new record in database
   Write = fun() ->
      Todo = #todo{
         id = Id,
         content = Content,
         priority = Priority,
         status = Status
      },
      mnesia:write(Todo)
   end,
   mnesia:transaction(Write),
   
   %% Return success
   {200, {json, Post}, State}.

%% Update
put("/todo/:id", Req, State) ->
   Id = leptus_req:param(Req, id),
   Post = leptus_req:body_qs(Req),
   {<<"content">>, Content} = lists:keyfind(<<"content">>, 1, Post),
   {<<"priority">>, Priority} = lists:keyfind(<<"priority">>, 1, Post),
   {<<"status">>, Status} = lists:keyfind(<<"status">>, 1, Post),
   Write = fun() ->
      Todo = #todo{
         id = Id,
         content = Content,
         priority = Priority,
         status = Status
      },
      mnesia:write(Todo)
   end,
   mnesia:transaction(Write),
   {200, {json, Post}, State}.

%% Delete
delete("/todo/:id", Req, State) ->
   Id = leptus_req:param(Req, id),
   Delete = fun() ->
      mnesia:delete({todo, Id})
   end,
   mnesia:transaction(Delete),
   {200, {json, [{<<"status">>, <<"deleted">>}]}, State}.

%% End
terminate(_Reason, _Route, _Req, _State) ->
   ok.
```

Let me explain each function for clarity:

* __cross_domains/3__: Enables [CORS](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing) feature for any host domain.
* __ini/3__: It will be called for each request at first place.
* __get/3__: It will be called for `GET` requests after `init/3`.
* __post/3__: It will be called for `POST` requests after `init/3`.
* __put/3__: It will be called for `PUT` requests after `init/3`.
* __delete/3__: It will be called for `DELETE` requests after `init/3`.
* __terminate/4__: It will be called in the last part.

The last file that we have to write is `todo_helper.erl`. It is a helper module that transform Mnesia query result to JSON.

```erlang
%% file src/todo_helper.erl
%% ------------------------
-module(todo_helper).
-export([format/1]).

format(List) -> format(List, []).
format([], Results) -> Results;
format([H|T], Results) -> format(T, [json(H)|Results]).

json({_, Key, Content, Priority, Status}) ->
   {Key, [Content, Priority, Status]}.
```

Everything is ready for moving on to the next level; Front-End.

#### Step 4: Front-End Stuff

The main focus of this tutorial is to introduce Erlang and Leptus framework. So I just write a simple presentation layer with [jQuery](---link---) and [underscore](---link---) which help us to make Ajax request and have a Single Page application. In the other hand, I do need to use JavaScript for making Ajax `PUT` and `DELETE` requests from browser, because browsers don't support `PUT` and `DELETE` HTTP request methods in the `HTML Form Tag` yet.

So just create an `index.html` file in `priv/templates/` directory and fill it with the following codes:

```html
<!-- file: priv/templates/index.html -->
<html>
<head>
	<title>Todo List</title>
   <style>
      .center { text-align: center; }
   </style>
   <script src="http://code.jquery.com/jquery-1.11.0.min.js"></script>
   <script src="http://underscorejs.org/underscore-min.js"></script>
   <script>
      $(function(){
         
         // Initialize variables
         var todoList = {};
         var $todoList = $("#todo-list");
         var $todoTpl = $("#todos-tpl");
         var $todoForm = $("#todo-form");
         var $todoId = $("#todo-id");
         var $todoContent = $("#todo-content");
         var $todoPriority = $("#todo-priority");
         var $todoStatus = $("#todo-status");
         var todoRender = _.template($todoTpl.html());
         
         // List todos
         $todoList.on('fetch reset', function(){
            $.get("/todos")
               .done(function(data){
                  todoList = data;
                  $todoList.html(todoRender({todos: data}));
               });
         });

         // Clear form
         $todoForm.on('clear', function(){
            $todoId.val('');
            $todoContent.val('');
            todoList = {};
         });

         // Create or Update
         $todoForm.on('submit', function(event){
            event.preventDefault();
            var data = $todoForm.serializeArray();

            if($todoId.val() == ''){ 
               $.post("/todo", data);
            }else{
               console.log(data);
               $.ajax({
                  url: "/todo/" + data[0].value,
                  type: 'put',
                  data: data
               });
            }
            
            $todoList.trigger('reset');
            $todoForm.trigger('clear');
         });

         // Delete
         $todoList.on('click', '.delete-todo', function(){
            var id = $(this).parents("li").attr("todo-id");
            $.ajax({
               url: '/todo/'+id,
               type: 'delete'
            }).done(function(data){
               console.log(data);
               $todoList.trigger('reset');
               $todoForm.trigger('clear');
            });
         });

         // Populate form with an existing todo for edit
         $todoList.on('click', '.edit-todo', function(){
            var id = $(this).parents("li").attr("todo-id");
            $todoId.val(id);
            $todoContent.val(todoList[id][0]);
            $todoPriority.val(todoList[id][1]);
            $todoStatus.val(todoList[id][2]);
         });

         // Start!
         $todoList.trigger('fetch');

      });
   </script>
   <script type="javascript/ejs" id="todos-tpl">
      <ul>
         <% _.each(todos, function(value, key){ %>
            <li todo-id="<%= key %>">
               <div><%= value[0] %></div>
               Priority: <%= value[1] %>
               Status: <%= value[2] %>
               <div>
                  <button class="delete-todo">Delete</button> 
                  <button class="edit-todo">Edit</button>
               </div>
            </li>
         <% }); %>
      </ul>
   </script>
</head>
<body>
   <div class="">
      <p>Welcome to Leptus Todo List</p>
      <form id="todo-form">
         <input type="hidden" name="id" id="todo-id"/>
         <label>Content</label>
         <div>
            <textarea id="todo-content" name="content"></textarea>
         </div>        
         <label>Priority</label>
         <div>
            <select id="todo-priority" name="priority">
               <option value="Low">Low</option>
               <option value="Medium">Medium</option>
               <option value="High">High</option>
            </select>
         </div>        
         <label>Status</label>
         <div>
            <select id="todo-status" name="status">
               <option value="Undone">Undone</option>
               <option value="Done">Done</option>
            </select>
         </div>
         <br/>
         <div><button type="submit">Save</button></div>
      </form>
   </div>
   <div id="todo-list">

   </div>
</body>
</html>
```

Now we can start the application.

#### Step 5: Start the Application

For starting your application, first you need to compile it by Rebar as follows:

```bash
$ ./rebar compile
```

Then open the Erlang shell with required paths and start the application with `application:start(todo)` function call as follows:

```bash
$ erl -pa ebin deps/*/ebin

Erlang R16B03 (erts-5.10.4) [...]
Eshell V5.10.4  (abort with ^G)
1> application:start(todo).
Leptus 0.3.4 started on http://127.0.0.1:8080
ok
2> 
```
If everything went well, you can open your browser and point to the `http://127.0.0.1:8080` url to see the result.
Also the source code of this tutorial is available in [it's GitHub page](https://github.com/hamidreza-s/LeptusTodo).

## Conclusion

You may complain that writing a todo application with Ruby's [Sinatra](http://www.sinatrarb.com/) or Python's [Flask](http://flask.pocoo.org/) or PHP's [Slim](http://www.slimframework.com/) is much simpler. They are simpler just because their syntax are more familiar for you, or because they are all OOP, the programming paradigm that you know from high school up to now. However these simple frameworks raise their ugly head when it comes to performance demand for your web application.
In the other hand, Erlang, as a web platform, can handle masive load of requests in a reliable way and it's concepts, semantics and syntax would become simpler and more beautiful for you just after you become brave enough to give it a try.
