%% ------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ------------------------------------------------------------------

-module(example_app).
-behaviour(application).

%% Application callbacks
-export([
	start/2,
	stop/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, _} =
		cowboy:start_https(http_listner, 100,
			[	{port, example:port()},
				{cacertfile, application:get_env(cas, cacertfile, "priv/ssl/pal-ca.crt")},
				{certfile, application:get_env(cas, certfile, "priv/ssl/pal.crt")},
				{keyfile, application:get_env(cas, keyfile, "priv/ssl/pal.key")} ],
			[	{env,
				[	{dispatch, dispatch()},
					{pt_cowboy_session, session()} ]},
				{middlewares, [pt_cowboy_session, cowboy_router, cowboy_handler]} ]),

	example_sup:start_link().

stop(_State) ->
	ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

session() ->
	GenF = fun() -> uuid:uuid_to_string(uuid:get_v4(), binary_nodash) end,
	pt_cowboy_session:init(
		pt_session_memory_store:init(GenF)).

dispatch() ->
	BasicW = example_basic:auth(),
	OAuth2Ws = example_oauth2:auth(),
	OAuth2Constraint =
		{	provider,
			function,
			fun(P) ->
				lists:member(P, pt_plist:keys(OAuth2Ws))
			end },

	cowboy_router:compile(
		[	{'_',
				[	{"/examples/basic", example_basic_handler, [{auth, BasicW}]},
					{"/examples/oauth2/:provider/[callback]", [OAuth2Constraint], example_oauth2_handler, [{auth, OAuth2Ws}]} ]} ]).

