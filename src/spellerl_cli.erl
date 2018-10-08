-module(spellerl_cli).

%% API exports
-export([main/1]).

%%====================================================================
%% Types
%%====================================================================

-type error(Reason) :: {error, Reason}.
-type cast_error() :: error(cast_error_reason()).
-type too_few_arguments_error_reason() :: too_few_arguments.
-type spell_error_reason() :: cast_error_reason()
                            | spellerl:error_reasons()
                            | too_few_arguments_error_reason().
-type cast_error_reason() :: bad_type.
-type spell_result() :: spellerl:ok() | spell_error_reason().

%%====================================================================
%% API functions
%%====================================================================

-spec main([string()]) -> no_return().
%% escript Entry point
main([Term]) ->
    Result = spell_it(Term),
    terminate(Result);
main(_) ->
    terminate(too_few_arguments).

%%====================================================================
%% Internal functions
%%====================================================================

-spec spell_it(string()) -> spell_result().
spell_it(Term) ->
    case to_integer(Term) of
        {ok, Integer} ->
            case spellerl:spell(Integer) of
                {ok, _Spelled} = OK ->
                    OK;
                {error, Reason} ->
                    Reason
            end;
        {error, bad_type = Reason} ->
            Reason
    end.


-spec terminate(spellerl:ok() | spell_error_reason()) -> no_return().
terminate({Reason, Output}) ->
    io:format("~s~n", [Output]),
    erlang:halt(exit_code(Reason));
terminate(Reason) ->
    io:format("~s~n", [error_desc(Reason)]),
    erlang:halt(exit_code(Reason)).


-spec exit_code(ok | spell_error_reason()) -> pos_integer().
exit_code(ok)                -> 0;
exit_code(too_few_arguments) -> 1;
exit_code(bad_type)          -> 2;
exit_code(negative)          -> 3;
exit_code(unknown_scale)     -> 4.


-spec error_desc(spell_error_reason()) -> string().
error_desc(too_few_arguments) ->
    "usage: spellerl TERM";
error_desc(bad_type) ->
    "type of a specified argument is not supported";
error_desc(negative) ->
    "negative integers are not supported";
error_desc(unknown_scale) ->
    "scale of provided number is unknown".


-spec to_integer(string()) -> {ok, integer()} | cast_error().
to_integer(String) ->
    try
        {ok, list_to_integer(String)}
    catch
        _:_ ->
            {error, bad_type}
    end.
