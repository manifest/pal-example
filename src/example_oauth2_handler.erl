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

-module(example_oauth2_handler).

%% API
-export([
	to_json/2
]).

%% REST handler callbacks
-export([
	init/2,
	allowed_methods/2, 
	is_authorized/2,
	content_types_provided/2,
	options/2
]).

%% Definitions
-define(CONTENT_TYPE, <<"content-type">>).

%% Types
-record(state, {
	authg :: pal:workflow(),
	authm :: map()
}).

%% =============================================================================
%% API
%% =============================================================================

to_json(Req, #state{authm = M} = State) ->
	{jsx:prettify(jsxn:encode(M)), Req, State}.

%% ============================================================================
%% REST handler callbacks
%% ============================================================================

init(Req, Opts) ->
	Provider = cowboy_req:binding(provider, Req),
	State =
		#state{
			authg = pt_kvlist:get_in([auth, Provider], Opts)},

	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, #state{authg = Group} = State) ->
	%% Retrieving "code", "state" and "error" fields
	%% from the query string of request if any of them appears.
	Data =
		lists:foldl(
			fun({Key, Val}, M) ->
				maps:put(binary_to_existing_atom(Key, utf8), Val, M)
			end,
			#{},
			pt_kvlist:with(
				[<<"code">>, <<"state">>, <<"error">>],
				cowboy_req:parse_qs(Req))),

	%% Executing an authentication workflow group.
	case pal:authenticate(Data, Group) of
		{ok, M} -> {true, Req, State#state{authm = M}};
		R       -> {stop, example_http_pal:handle_result(R, Req), State}
	end.

content_types_provided(Req, State) ->
	{[{{<<"application">>,  <<"json">>, '*'}, to_json}], Req, State}.

options(Req, State) ->
	example_http:options(Req, State).

