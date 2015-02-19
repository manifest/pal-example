%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014-2015 Andrei Nesterov <ae.nesterov@gmail.com>
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

-module(pal_example_http_misc).

%% API
-export([
	handle_result/2
]).

%% Definitions
-define(CONTENT_TYPE_JSON, {<<"content-type">>, <<"application/json">>}).

%% ============================================================================
%% API
%% ============================================================================

-spec handle_result(pal_workflow:result(), cowboy_req:req()) -> cowboy_req:req().
handle_result({stop, Resp}, Req) ->
	pal_http:reply(Resp, fun(Status, Headers, Body) ->
		cowboy_req:reply(Status, Headers, Body, Req)
	end);
handle_result({error, {_Type, Data}}, Req) ->
	cowboy_req:reply(
		422,
		[?CONTENT_TYPE_JSON],
		error_to_json(Data),
		Req);
handle_result({ok, Data}, Req) ->
	cowboy_req:reply(
		200,
		[?CONTENT_TYPE_JSON],
		jsxn:encode(Data),
		Req).

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec error_to_json(any()) -> binary().
error_to_json(M) when is_map(M)  -> jsxn:encode(M);
error_to_json(L) when is_list(L) -> jsx:encode(L);
error_to_json(T)                 -> jsx:encode([{error, T}]).

