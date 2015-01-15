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

-module(example_oauth2).

%% API
-export([
	auth/0,
	callback_uri/1
]).

%% ===================================================================
%% API
%% ===================================================================

-spec auth() -> [{binary(), pal:group()}].
auth() ->
	{ok, Conf} = application:get_env(example, oauth2),
	lists:map(
		fun({Provider, Ws, Opts}) ->
			Group =
				pal:group(
					Ws,
					Opts#{
						redirect_uri => callback_uri(Provider),
						includes => [uid, credentials, info]}),

			{Provider, Group}
		end, Conf).

-spec callback_uri(binary()) -> binary().
callback_uri(Provider) ->
	example_http:uri(<<"/examples/oauth2/", Provider/binary, "/callback">>).

