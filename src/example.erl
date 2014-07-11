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

-module(example).

%% API
-export([
	uri/1,
	host_url/0,
	port/0,
	to_json/2,
	qs_pretty/1,
	options/2
]).

%% Definitions
-define(PRETTY, <<"pretty">>).

%% ===================================================================
%% API
%% ===================================================================

-spec uri(string() | binary()) -> binary().
uri(Path) when is_list(Path) ->
	uri(list_to_binary(Path));
uri(Path) ->
	<<(host_url())/binary, Path/binary>>.

-spec host_url() -> binary().
host_url() ->
	<<"https://localhost:", (integer_to_binary(port()))/binary>>.

-spec port() -> integer().
port() ->
	case os:getenv("PORT") of
		false ->
			application:get_env(application:get_application(), http_port, 8081);
		Port ->
			list_to_integer(Port)
	end.

-spec to_json(jsx:json_term(), boolean()) -> jsx:json_text().
to_json(Data, false) ->
	jsxn:encode(Data);
to_json(Data, true) ->
	jsx:prettify(jsxn:encode(Data)).

-spec qs_pretty(Req) -> {boolean(), Req} when Req :: cowboy_req:req().
qs_pretty(Req) ->
	{Pretty, Req2} = cowboy_req:qs_val(?PRETTY, Req),
	Pretty2 =	
		case Pretty of
			true        -> true;
			<<"true">>  -> true;
			<<"false">> -> false;
			_           -> false
		end,
	{Pretty2, Req2}.

-spec options(Req, State) -> {ok, Req, State} when Req :: cowboy_req:req(), State :: any().
options(Req, State) ->
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"x-requested-with, content-type">>, Req),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req2),
	Req4 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"1728000">>, Req3),
	{ok, Req4, State}.

