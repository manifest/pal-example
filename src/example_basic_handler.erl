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

-module(example_basic_handler).

%% API
-export([
	to_json/2,
	from_any/2
]).

%% REST handler callbacks
-export([
	init/2,
	allowed_methods/2, 
	is_authorized/2,
	content_types_accepted/2,
	content_types_provided/2,
	options/2
]).

%% Definitions
-define(BASIC, <<"Basic">>).
-define(CONTENT_TYPE, <<"content-type">>).

%% Types
-record(state, {
	auth   :: pal:workflow(),
	user   :: map(),
	pretty :: boolean()
}).

%% ===================================================================
%% API
%% ===================================================================

to_json(Req, #state{user = User, pretty = Pretty} = State) ->
	User2 =
		case maps:get(rules, User) of
			#{role := roommate} ->
				User#{roommate_agreement => example_basic:roommate_agreement()};
			_ ->
				User
		end,
	Body = example:to_json(User2, Pretty),
	{Body, Req, State}.

from_any(Req, State) ->
	{Body, Req2, State2} = to_json(Req, State),
	Req3 = cowboy_req:set_resp_body(Body, Req2),
	{true, Req3, State2}.

%% ==================================================================
%% REST handler callbacks
%% ==================================================================

init(Req, Opts) ->
	#{pretty := Pretty} =
		cowboy_req:match_qs(
			[	{pretty, fun example_http:pretty_constraint/1, false} ],
			Req),

	State =
		#state{
			auth = pt_kvterm:get(auth, Opts),
			pretty = Pretty},

	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, #state{auth = W} = State) ->
	case pal:authenticate(Req, W) of
		{#{uid := _} = M, Req2} ->
			{true, Req2, State#state{user = M}};
		{#{}, Req2} ->
			{{false, ?BASIC}, Req2, State};
		{stop, Req2} ->
			{stop, Req2, State}
	end.

content_types_provided(Req, State) ->
	{[{{<<"application">>,  <<"json">>, '*'}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, from_any},
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, from_any}
	], Req, State}.

options(Req, State) ->
	example_http:options(Req, State).

