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

-module(example_oauth2_handler).

%% Base handler callbacks
-export([
	init/3
]).

%% REST handler callbacks
-export([
	options/2,
	rest_init/2,
	allowed_methods/2, 
	is_authorized/2,
	content_types_provided/2
]).

%% API
-export([
	to_json/2
]).

%% Definitions
-define(CONTENT_TYPE, <<"content-type">>).

%% Types
-record(state, {
	auth :: pal:workflow(),
	user :: map()
}).

%% ==================================================================
%% Base handler callbacks
%% ==================================================================

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

%% ==================================================================
%% REST handler callbacks
%% ==================================================================

rest_init(Req, Opts) ->
	{Provider, Req2} = cowboy_req:binding(provider, Req),
	State =
		#state{
			auth = pt_plist:get_in([auth, Provider], Opts)},

	{ok, Req2, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, #state{auth = W} = State) ->
	case pal:authenticate(Req, W) of
		{M, Req2} when is_map(M) ->
			{true, Req2, State#state{user = M}};
		{halt, Req2} ->
			{halt, Req2, State}
	end.

options(Req, State) ->
	example:options(Req, State).

content_types_provided(Req, State) ->
	{[{{<<"application">>,  <<"json">>, '*'}, to_json}], Req, State}.

%% ===================================================================
%% API
%% ===================================================================

to_json(Req, #state{user = User} = State) ->
	Body = example:to_json(User, true),
	{Body, Req, State}.

