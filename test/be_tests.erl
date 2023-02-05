-module(be_tests).
-include_lib("eunit/include/eunit.hrl").

%% @doc Tests whether AVG, MED, MIN and MAX of raw data (no anonimization, duplicates allowed)
%% is the same as the reference values. Additionally tests whether a correct number of tuples
%% and triples is extracted from the dataset.
raw_data_test_() ->
  Data = be:get_user_tuples("data//", yes, 1),
  Aggregated = be:aggregate(Data),
  [ ?_assert(length(lists:flatten(Data)) =:= 11063),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Aggregated) =:= 11063),
    ?_assert(be:avg(Aggregated) =:= 2524.600379643858),
    ?_assert(be:med(Aggregated) =:= 2521),
    ?_assert(be:min(Aggregated) =:= 1),
    ?_assert(be:max(Aggregated) =:= 100000)
  ].

%% @doc Tests whether AVG, MED, MIN and MAX of the data (anonimized, no duplicates per user allowed)
%% is the same as the reference values calculated manually in Excel. Additionally tests whether a
%% correct number of tuples and triples is extracted from the dataset.
processed_data_test_() ->
  Data = be:get_user_tuples("data//", no, 9),
  Aggregated = be:aggregate(Data),
  Anonimized = be:anonimize(Aggregated),
  [ ?_assert(length(lists:flatten(Data)) =:= 11056),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Aggregated) =:= 11056),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Anonimized) =:= 11053),
    ?_assert(be:avg(Anonimized) =:= 2503.4917217045145),
    ?_assert(be:med(Anonimized) =:= 2520),
    ?_assert(be:min(Anonimized) =:= 9),
    ?_assert(be:max(Anonimized) =:= 4995)
  ].

%% @doc Tests whether AVG, MED, MIN and MAX of the data (anonimized, no duplicates per user allowed)
%% is the same as the reference values calculated manually in Excel. Additionally tests whether a
%% correct number of tuples and triples is extracted from the dataset.
initial_data_test_() ->
  Data = be:get_user_tuples("data//", no, 1),
  Aggregated = be:aggregate(Data),
  Anonimized = be:anonimize(Aggregated),
  [ ?_assert(length(lists:flatten(Data)) =:= 11061),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Aggregated) =:= 11061),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Anonimized) =:= 813),
    ?_assert(be:avg(Anonimized) =:= 2468.093480934809),
    ?_assert(be:med(Anonimized) =:= 2549),
    ?_assert(be:min(Anonimized) =:= 56),
    ?_assert(be:max(Anonimized) =:= 4995)
  ].
