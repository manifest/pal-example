-module(example_constraint).

%% API
-export([
	boolean/1
]).

%% ===================================================================
%% API
%% ===================================================================

-spec boolean(binary() | boolean()) -> boolean() | {true, boolean()}.
boolean(true)        -> true;
boolean(false)       -> true;
boolean(<<"true">>)  -> {true, true};
boolean(<<"false">>) -> {true, false};
boolean(_)           -> false.

