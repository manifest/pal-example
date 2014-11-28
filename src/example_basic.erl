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

-module(example_basic).

%% API
-export([
	auth/0,
	users/0,
	roommate_agreement/0
]).

%% Definitions
-define(USERID, <<"userid">>).
-define(PASSWORD, <<"password">>).

%% Types
-type user() :: #{UserID :: binary() => #{name => binary(), role => atom(), secret => binary() | list()}}.

%% ===================================================================
%% API
%% ===================================================================

-spec auth() -> pal:workflow().
auth() ->
	pal:new([[fun basic_qs/2, pal_basic], fun authorize/2]).

-spec users() -> user().
users() ->
	#{<<"john">> =>
			#{name   => <<"John Doe">>,
				role   => roommate,
				secret => pbkdf2(<<"123">>, salt())},
		<<"jeff">> =>
			#{name   => <<"Jeff Loe">>,
				role   => roommate,
				secret => pbkdf2(<<"123">>, salt())},
		<<"jane">> =>
			#{name   => <<"Jane Roe">>,
				role   => neighbour,
				secret => pbkdf2(<<"123">>, salt())}}.

-spec roommate_agreement() -> binary().
roommate_agreement() ->
	<<"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras tristique sodales odio nec tincidunt. Pellentesque ut accumsan nibh. Sed auctor non eros quis scelerisque. Mauris sollicitudin, mauris in rutrum vestibulum, orci mauris mollis sapien, id fermentum odio nibh eget eros. Etiam placerat sem odio, pellentesque tincidunt turpis vehicula et. Pellentesque convallis accumsan suscipit. Proin vel mauris est. Sed quam elit, fermentum non sem ac, luctus laoreet metus. Pellentesque sollicitudin eros sed ligula hendrerit ultrices. Suspendisse potenti. Aliquam blandit nibh id ligula ultricies, sit amet mattis lorem iaculis. Mauris ac ipsum cursus, rutrum mi sit amet, sollicitudin erat. Integer nec lacus congue ipsum sollicitudin placerat et et erat.">>.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec salt() -> binary().
salt() ->
	<<219,133,27,130,70,38,169,214,94,216,21,106,10,15,202,106,165,147,118,140,191,
	156,63,74,99,96,59,65,91,63,242,209,250,64,179,206,208,104,230,14,144,223,13,
	247,89,150,29,94,105,198,187,116,255,19,42,4,136,186,187,147,106,53,162,13>>.

-spec pbkdf2(binary(), binary()) -> binary().
pbkdf2(Password, Salt) ->
	{ok, Secret} = pbkdf2:pbkdf2(sha256, Password, Salt, 4096),
	Secret.

-spec basic_qs(map(), Req) -> {Resp, Req} when Resp :: pal:response(), Req :: cowboy_req:req().
basic_qs(M, Req) ->
	{ok, Qs, Req2} = cowboy_req:body_qs(Req),
	case {pt_kvlist:find(?USERID, Qs), pt_kvlist:find(?PASSWORD, Qs)} of
		{undefined, _} ->
			{undefined, Req2};
		{_, undefined} ->
			{undefined, Req2};
		{UserID, Password} ->
			{M
				#{credentials =>
					#{userid   => UserID,
						password => Password}}, Req2}
	end.

-spec authorize(map(), Req) -> {Resp, Req} when Resp :: pal:response(), Req :: cowboy_req:req().
authorize(#{credentials := #{userid := UserID, password := Password}} = M, Req) ->
	case pt_map:find(UserID, users()) of
		undefined ->
			{undefined, Req};
		User ->
			Secret = maps:get(secret, User),
			case Secret =:= pbkdf2(Password, salt()) of
				true ->
					{M
						#{uid   => UserID,
							info  =>
								#{name     => maps:get(name, User),
									nickname => UserID},
							rules =>
								#{role => maps:get(role, User)}}, Req};
				false ->
					{undefined, Req}
			end
	end.

