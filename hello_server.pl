:- use_module(library(http/thread_httpd)).    % http_server/2
:- use_module(library(http/http_dispatch)).   % http_handler/3
:- use_module(library(http/http_json)).       % reply_json_dict/1

% 1. Declare that requests to /hello invoke say_hello/1
:- http_handler(root(hello), say_hello, []).

% 2. The handler itself
say_hello(_Request) :-
    reply_json_dict(_{ hello : "world" }).

% 3. On load, start the HTTP server on port 8000
:- initialization(
     http_server(http_dispatch, [port(8000)])
   ).
