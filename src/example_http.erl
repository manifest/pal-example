%% ----------------------------------------------------------------------------
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
%% ----------------------------------------------------------------------------

-module(example_http).

%% API
-export([
	start/0,
	port/0,
	uri/1,
	host_url/0,
	options/2
]).

%% =============================================================================
%% API
%% =============================================================================

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
	cowboy:start_https(
		http_listner,
		100,
		[	{port, port()},
			{cacertfile, application:get_env(cas, cacertfile, "priv/ssl/pal-ca.crt")},
			{certfile, application:get_env(cas, certfile, "priv/ssl/pal.crt")},
			{keyfile, application:get_env(cas, keyfile, "priv/ssl/pal.key")} ],
		[	{env, [{dispatch, dispatch()}]} ]).

-spec port() -> integer().
port() ->
	case os:getenv("PORT") of
		false ->
			application:get_env(application:get_application(), http_port, 8081);
		Port ->
			list_to_integer(Port)
	end.

-spec uri(string() | binary()) -> binary().
uri(Path) when is_list(Path) ->
	uri(list_to_binary(Path));
uri(Path) ->
	<<(host_url())/binary, Path/binary>>.

-spec host_url() -> binary().
host_url() ->
	<<"https://localhost:", (integer_to_binary(port()))/binary>>.

-spec options(Req, State) -> {ok, Req, State} when Req :: cowboy_req:req(), State :: any().
options(Req, State) ->
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"x-requested-with, content-type">>, Req),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req2),
	Req4 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"1728000">>, Req3),
	{ok, Req4, State}.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec dispatch() -> cowboy_router:dispatch_rules().
dispatch() ->
	OAuth2Ws = example_oauth2:auth(),
	OAuth2Constraint =
		{	provider,
			fun(P) ->
				lists:member(P, pt_kvlist:keys(OAuth2Ws))
			end },

	cowboy_router:compile([{'_', [
		{"/examples/oauth2/:provider/[callback]", [OAuth2Constraint], example_oauth2_handler, [{auth, OAuth2Ws}]}
	]}]).

