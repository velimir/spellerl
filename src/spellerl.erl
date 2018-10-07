-module(spellerl).

%% TODO: remove from here
-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([spell/1]).

%% Public types
-export_type([object/0, result/0]).

%%====================================================================
%% Types
%%====================================================================

-type object() :: int().
-type int() :: 1..1000.

-type result() :: ok() | error().

-type ok() :: {ok, string()}.

-type error() :: bad_type_error() | int_error().
-type bad_type_error() :: {error, bad_type}.

-type int_error() :: out_of_range_error() | negative_error().
-type negative_error() :: {error, negative}.
-type out_of_range_error() :: {error, unknown_scale}.

%%====================================================================
%% API functions
%%====================================================================

-spec spell(object()) -> result().

spell(Int) when is_integer(Int) ->
    spell_int(Int);
spell(_Else) ->
    {error, bad_type}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec spell_int(int()) -> ok() | int_error().
%% TODO: docs
spell_int(Int) when Int < 0 ->
    {error, negative};
spell_int(Int)  ->
    case scale_floor(Int) of
        ScaleFloor when ScaleFloor > 1000 ->
            Scale = trunc(math:log10(ScaleFloor)),
            case scale_name(Scale) of
                {ok, _Name} ->
                    spell_int(Int, ScaleFloor);
                {error, not_found} ->
                    {error, unknown_scale}
            end;
        ScaleFloor ->
            spell_int(Int, ScaleFloor)
    end.

scale_floor(Int) when Int < 1000 ->
    1;
scale_floor(Int) ->
    Floor = floor(math:log10(Int)),
    ScaleSize = 3,                          % trunc(math:log10(1000)),
    pow(10, Floor - (Floor rem ScaleSize)).


%% Not using math:pow/2 here as it's backed by c library, which has
%% float limitations
pow(_Base, 0)  -> 1;
pow(Base, Exp) -> Base * pow(Base, Exp - 1).

%% Spelling according to https://en.wikipedia.org/w/index.php?oldid=861363276
spell_int(Int, ScaleFloor) ->
    spell_int(Int, ScaleFloor, []).

spell_int(0 = Int, _ScaleFloor, [] = _Acc) ->
    num_to_cardinal(Int);
spell_int(0 = _Int, _ScaleFloor, Acc) ->
    iolist_to_binary(lists:join(<<", ">>, lists:reverse(Acc)));
spell_int(Int, ScaleFloor, Acc) when (Int div ScaleFloor) == 0 ->
    spell_int(Int rem ScaleFloor, ScaleFloor div 1000, Acc);
spell_int(Int, 1 = ScaleFloor, [_|_] = Acc) when Int < 100 ->
    Words = scale_to_cardinal(Int div ScaleFloor, ScaleFloor),
    HeadPhrase = iolist_to_binary([lists:join(<<", ">>, lists:reverse(Acc))]),
    iolist_to_binary([HeadPhrase, <<" and ">>, Words]);
spell_int(Int, ScaleFloor, Acc) ->
    Words = scale_to_cardinal(Int div ScaleFloor, ScaleFloor),
    spell_int(Int rem ScaleFloor, ScaleFloor div 1000, [Words | Acc]).

scale_to_cardinal(Num, 1 = _ScaleFloor) ->
    num_to_cardinal(Num);
scale_to_cardinal(Num, ScaleFloor) ->
    Scale = trunc(math:log10(ScaleFloor)),
    {ok, ScaleName} = scale_name(Scale),
    iolist_to_binary([num_to_cardinal(Num), " ", ScaleName]).

num_to_cardinal(Num) ->
    Hundreds = Num div 100,
    Tens = (Num rem 100) div 10,
    Ones = Num rem 10,
    num_to_cardinal(Hundreds, Tens, Ones).

num_to_cardinal(0 = _Hundreds, Tens, Ones) ->
    num_to_cardinal(Tens, Ones);
num_to_cardinal(Hundreds, 0 = _Tens, 0 = _Ones) ->
    hundreds_to_cardinal(Hundreds);
num_to_cardinal(Hundreds, Tens, Ones) ->
    HWord = hundreds_to_cardinal(Hundreds),
    NumWord = num_to_cardinal(Tens, Ones),
    iolist_to_binary([HWord, " and ", NumWord]).

hundreds_to_cardinal(Hundreds) ->
    OnesWord = ones_to_cardinal(Hundreds),
    iolist_to_binary([OnesWord, " hundred"]).

num_to_cardinal(0 = _Tens, Ones) ->
    ones_to_cardinal(Ones);
num_to_cardinal(Tens, Ones) ->
    tens_to_cardinal(Tens, Ones).

tens_to_cardinal(1 = _Tens, 0 = _Ones) -> <<"ten">>;
tens_to_cardinal(1 = _Tens, 1 = _Ones) -> <<"eleven">>;
tens_to_cardinal(1 = _Tens, 2 = _Ones) -> <<"twelve">>;
tens_to_cardinal(1 = _Tens, 3 = _Ones) -> <<"thirteen">>;
tens_to_cardinal(1 = _Tens, 4 = _Ones) -> <<"fourteen">>;
tens_to_cardinal(1 = _Tens, 5 = _Ones) -> <<"fifteen">>;
tens_to_cardinal(1 = _Tens, 6 = _Ones) -> <<"sixteen">>;
tens_to_cardinal(1 = _Tens, 7 = _Ones) -> <<"seventeen">>;
tens_to_cardinal(1 = _Tens, 8 = _Ones) -> <<"eighteen">>;
tens_to_cardinal(1 = _Tens, 9 = _Ones) -> <<"nineteen">>;
tens_to_cardinal(Tens, 0 = _Ones)      -> tens_to_cardinal(Tens);
tens_to_cardinal(Tens, Ones) ->
    Prefix = tens_to_cardinal(Tens),
    Suffix = ones_to_cardinal(Ones),
    iolist_to_binary([Prefix, "-", Suffix]).

tens_to_cardinal(2) -> <<"twenty">>;
tens_to_cardinal(3) -> <<"thirty">>;
tens_to_cardinal(4) -> <<"forty">>;
tens_to_cardinal(5) -> <<"fifty">>;
tens_to_cardinal(6) -> <<"sixty">>;
tens_to_cardinal(7) -> <<"seventy">>;
tens_to_cardinal(8) -> <<"eighty">>;
tens_to_cardinal(9) -> <<"ninety">>.

ones_to_cardinal(0) -> <<"zero">>;
ones_to_cardinal(1) -> <<"one">>;
ones_to_cardinal(2) -> <<"two">>;
ones_to_cardinal(3) -> <<"three">>;
ones_to_cardinal(4) -> <<"four">>;
ones_to_cardinal(5) -> <<"five">>;
ones_to_cardinal(6) -> <<"six">>;
ones_to_cardinal(7) -> <<"seven">>;
ones_to_cardinal(8) -> <<"eight">>;
ones_to_cardinal(9) -> <<"nine">>.

scale_name(3)  -> {ok, <<"thousand">>};
scale_name(6)  -> {ok, <<"million">>};
scale_name(9)  -> {ok, <<"billion">>};
scale_name(12) -> {ok, <<"trillion">>};
scale_name(15) -> {ok, <<"quadrillion">>};
scale_name(18) -> {ok, <<"quintillion">>};
scale_name(21) -> {ok, <<"sextillion">>};
scale_name(24) -> {ok, <<"septillion">>};
scale_name(27) -> {ok, <<"octillion">>};
scale_name(30) -> {ok, <<"nonillion">>};
scale_name(33) -> {ok, <<"decillion">>};
scale_name(36) -> {ok, <<"undecillion">>};
scale_name(39) -> {ok, <<"duodecillion">>};
scale_name(42) -> {ok, <<"tredecillion">>};
scale_name(45) -> {ok, <<"quattuordecillion">>};
scale_name(48) -> {ok, <<"quindecillion">>};
scale_name(51) -> {ok, <<"sedecillion">>};
scale_name(54) -> {ok, <<"septendecillion">>};
scale_name(57) -> {ok, <<"octodecillion">>};
scale_name(60) -> {ok, <<"novemdecillion">>};
scale_name(63) -> {ok, <<"vigintillion">>};
scale_name(_Scale) -> {error, not_found}.


%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(EUNIT).

pow_test_() ->
    [
     ?_assertEqual(1, pow(1, 0)),
     ?_assertEqual(2, pow(2, 1)),
     ?_assertEqual(4, pow(2, 2)),
     ?_assertEqual(1000, pow(10, 3))
    ].

num_to_cardinal_test_() ->
    [
     ?_assertEqual(<<"zero">>, num_to_cardinal(0)),
     ?_assertEqual(<<"one">>, num_to_cardinal(1)),
     ?_assertEqual(<<"nine">>, num_to_cardinal(9)),
     ?_assertEqual(<<"ten">>, num_to_cardinal(10)),
     ?_assertEqual(<<"eleven">>, num_to_cardinal(11)),
     ?_assertEqual(<<"nineteen">>, num_to_cardinal(19)),
     ?_assertEqual(<<"twenty">>, num_to_cardinal(20)),
     ?_assertEqual(<<"twenty-one">>, num_to_cardinal(21)),
     ?_assertEqual(<<"ninety-nine">>, num_to_cardinal(99)),
     ?_assertEqual(<<"one hundred">>, num_to_cardinal(100)),
     ?_assertEqual(<<"nine hundred">>, num_to_cardinal(900)),
     ?_assertEqual(<<"one hundred and one">>, num_to_cardinal(101)),
     ?_assertEqual(<<"one hundred and ten">>, num_to_cardinal(110)),
     ?_assertEqual(<<"one hundred and nineteen">>, num_to_cardinal(119)),
     ?_assertEqual(<<"one hundred and twenty">>, num_to_cardinal(120)),
     ?_assertEqual(<<"one hundred and twenty-one">>, num_to_cardinal(121)),
     ?_assertEqual(<<"nine hundred and ninety-nine">>, num_to_cardinal(999))
    ].

scale_to_cardinal_test_() ->
    [
     ?_assertEqual(<<"one thousand">>,
                   scale_to_cardinal(1, pow(10, 3))),
     ?_assertEqual(<<"one million">>,
                   scale_to_cardinal(1, pow(10, 6))),
     ?_assertEqual(<<"nine hundred and ninety-nine thousand">>,
                   scale_to_cardinal(999, pow(10, 3)))
    ].


scale_floor_test_() ->
    [
     ?_assertEqual(1, scale_floor(0)),
     ?_assertEqual(1, scale_floor(1)),
     ?_assertEqual(1, scale_floor(999)),
     ?_assertEqual(pow(10, 3), scale_floor(pow(10, 3))),
     ?_assertEqual(pow(10, 3), scale_floor(pow(10, 4))),
     ?_assertEqual(pow(10, 3), scale_floor(pow(10, 5))),
     ?_assertEqual(pow(10, 6), scale_floor(pow(10, 6)))
    ].

spell_int_test_() ->
    [
     ?_assertEqual(<<"zero">>, spell_int(0)),
     ?_assertEqual(<<"one thousand">>, spell_int(1000)),
     ?_assertEqual(<<"twenty-eight thousand, one hundred and twenty-three">>,
                   spell_int(28123)),
     ?_assertEqual(
        <<"seven million, three hundred and forty thousand, two hundred">>,
        spell_int(7340200)),
     ?_assertEqual(<<"ninety-two million and thirty-one">>,
                   spell_int(92000031)),
     ?_assertEqual(<<"one trillion, ten million and twenty-three">>,
                   spell_int(1000010000023)),
     ?_assertEqual(<<"ten vigintillion, one hundred novemdecillion and eighty-nine">>,
                   spell_int(pow(10, 64) + pow(10, 62) + 89))
    ].


spell_test_() ->
    [
     ?_assertEqual({error, bad_type}, spell(atom)),
     ?_assertEqual({error, bad_type}, spell("string")),
     ?_assertEqual({error, bad_type}, spell(<<"binary">>)),
     ?_assertEqual({error, bad_type}, spell(1.0)),
     ?_assertEqual({error, negative}, spell(-1)),
     ?_assertEqual({error, unknown_scale}, spell(pow(10, 68)))
    ].

-endif.
