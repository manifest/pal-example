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
	auth/0
]).

%% Definitions
-define(GOOGLE, <<"google">>).
-define(FACEBOOK, <<"facebook">>).

%% ===================================================================
%% API
%% ===================================================================

-spec auth() -> [{binary(), pal:workflow()}].
auth() ->
	{ok, ProviderConf} = application:get_env(example, providers),

	GlobalOpts =
		#{includes        => [credentials, uid, info],
			session         => pt_cowboy_session},

	GoogleOpts =
		GlobalOpts
			#{client_id     => pt_map:get_in([google, client_id], ProviderConf),
				client_secret => pt_map:get_in([google, client_secret], ProviderConf),
				redirect_uri  => example_http:uri("/examples/oauth2/google/callback")},

	FacebookOpts =
		GlobalOpts
			#{client_id     => pt_map:get_in([facebook, client_id], ProviderConf),
				client_secret => pt_map:get_in([facebook, client_secret], ProviderConf),
				redirect_uri  => example_http:uri("/examples/oauth2/facebook/callback")},

	GoogleW = pal:new([pal_google_oauth2_authcode, pal_google_oauth2_people], GoogleOpts),
	FacebookW = pal:new([pal_facebook_oauth2_authcode, pal_facebook_oauth2_user], FacebookOpts),
	
	[{?GOOGLE, GoogleW}, {?FACEBOOK, FacebookW}].

