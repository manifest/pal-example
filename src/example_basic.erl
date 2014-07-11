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
	Salt = <<"$2a$04$sH.Cltb6i.x5/dujsHEFUe">>,
	#{<<"john">> =>
			#{name   => <<"John Doe">>,
				role   => roommate,
				secret => begin {ok, Secret} = bcrypt:hashpw(<<"123">>, Salt), Secret end},
		<<"jeff">> =>
			#{name   => <<"Jeff Loe">>,
				role   => roommate,
				secret => begin {ok, Secret} = bcrypt:hashpw(<<"123">>, Salt), Secret end},
		<<"jane">> =>
			#{name   => <<"Jane Roe">>,
				role   => neighbour,
				secret => begin {ok, Secret} = bcrypt:hashpw(<<"123">>, Salt), Secret end}}.

-spec roommate_agreement() -> binary().
roommate_agreement() ->
	<<"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras tristique sodales odio nec tincidunt. Pellentesque ut accumsan nibh. Sed auctor non eros quis scelerisque. Mauris sollicitudin, mauris in rutrum vestibulum, orci mauris mollis sapien, id fermentum odio nibh eget eros. Etiam placerat sem odio, pellentesque tincidunt turpis vehicula et. Pellentesque convallis accumsan suscipit. Proin vel mauris est. Sed quam elit, fermentum non sem ac, luctus laoreet metus. Pellentesque sollicitudin eros sed ligula hendrerit ultrices. Suspendisse potenti. Aliquam blandit nibh id ligula ultricies, sit amet mattis lorem iaculis. Mauris ac ipsum cursus, rutrum mi sit amet, sollicitudin erat. Integer nec lacus congue ipsum sollicitudin placerat et et erat.">>.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec basic_qs(map(), Req) -> {Resp, Req} when Resp :: pal:response(), Req :: cowboy_req:req().
basic_qs(M, Req) ->
	{ok, Qs, Req2} = cowboy_req:body_qs(Req),
	case {pt_plist:find(?USERID, Qs), pt_plist:find(?PASSWORD, Qs)} of
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
			case {ok, Secret} =:= bcrypt:hashpw(Password, Secret) of
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

