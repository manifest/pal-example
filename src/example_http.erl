-module(example_http).

%% API
-export([
	start/0,
	port/0,
	uri/1,
	host_url/0,
	pretty_constraint/1,
	options/2
]).

%% ===================================================================
%% API
%% ===================================================================

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
	cowboy:start_https(http_listner, 100,
		[	{port, port()},
			{cacertfile, application:get_env(cas, cacertfile, "priv/ssl/pal-ca.crt")},
			{certfile, application:get_env(cas, certfile, "priv/ssl/pal.crt")},
			{keyfile, application:get_env(cas, keyfile, "priv/ssl/pal.key")} ],
		[	{env,
			[	{dispatch, dispatch()},
				{pt_cowboy_session, session()} ]},
			{middlewares, [pt_cowboy_session, cowboy_router, cowboy_handler]} ]).

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

-spec pretty_constraint(term()) -> {true, boolean()} | false.
pretty_constraint(true)        -> {true, true};
pretty_constraint(<<"true">>)  -> {true, true};
pretty_constraint(<<"false">>) -> {true, false};
pretty_constraint(false)       -> {true, false};
pretty_constraint(_)           -> false.

-spec options(Req, State) -> {ok, Req, State} when Req :: cowboy_req:req(), State :: any().
options(Req, State) ->
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"x-requested-with, content-type">>, Req),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req2),
	Req4 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"1728000">>, Req3),
	{ok, Req4, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec dispatch() -> cowboy_router:dispatch_rules().
dispatch() ->
	BasicW = example_basic:auth(),
	OAuth2Ws = example_oauth2:auth(),
	OAuth2Constraint =
		{	provider,
			fun(P) ->
				lists:member(P, pt_kvlist:keys(OAuth2Ws))
			end },

	cowboy_router:compile(
		[	{'_',
				[	{"/examples/basic", example_basic_handler, [{auth, BasicW}]},
					{"/examples/oauth2/:provider/[callback]", [OAuth2Constraint], example_oauth2_handler, [{auth, OAuth2Ws}]} ]} ]).

-spec session() -> pt_cowboy_session:initializer().
session() ->
	GenF = fun() -> uuid:uuid_to_string(uuid:get_v4(), binary_nodash) end,
	pt_cowboy_session:init(
		pt_session_memory_store:init(GenF)).

