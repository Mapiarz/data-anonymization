-module(data_anonymization_tests).
-include_lib("eunit/include/eunit.hrl").

%% @doc Tests whether AVG, MED, MIN and MAX of raw data (no anonimization, duplicates allowed)
%% is the same as the reference values. Additionally tests whether a correct number of tuples
%% and triples is extracted from the dataset.
raw_data_test_() ->
  Data = data_anonymization:get_user_tuples("data//", yes, 1),
  Aggregated = data_anonymization:aggregate(Data),
  [ ?_assert(length(lists:flatten(Data)) =:= 11063),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Aggregated) =:= 11063),
    ?_assert(data_anonymization:avg(Aggregated) =:= 2524.600379643858),
    ?_assert(data_anonymization:med(Aggregated) =:= 2521),
    ?_assert(data_anonymization:min(Aggregated) =:= 1),
    ?_assert(data_anonymization:max(Aggregated) =:= 100000)
  ].

%% @doc Tests whether AVG, MED, MIN and MAX of the data (anonimized, no duplicates per user allowed)
%% is the same as the reference values calculated manually in Excel. Additionally tests whether a
%% correct number of tuples and triples is extracted from the dataset.
processed_data_test_() ->
  Data = data_anonymization:get_user_tuples("data//", no, 9),
  Aggregated = data_anonymization:aggregate(Data),
  Anonimized = data_anonymization:anonimize(Aggregated),
  [ ?_assert(length(lists:flatten(Data)) =:= 11056),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Aggregated) =:= 11056),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Anonimized) =:= 11053),
    ?_assert(data_anonymization:avg(Anonimized) =:= 2503.4917217045145),
    ?_assert(data_anonymization:med(Anonimized) =:= 2520),
    ?_assert(data_anonymization:min(Anonimized) =:= 9),
    ?_assert(data_anonymization:max(Anonimized) =:= 4995)
  ].

%% @doc Tests whether AVG, MED, MIN and MAX of the data (anonimized, no duplicates per user allowed)
%% is the same as the reference values calculated manually in Excel. Additionally tests whether a
%% correct number of tuples and triples is extracted from the dataset.
initial_data_test_() ->
  Data = data_anonymization:get_user_tuples("data//", no, 1),
  Aggregated = data_anonymization:aggregate(Data),
  Anonimized = data_anonymization:anonimize(Aggregated),
  [ ?_assert(length(lists:flatten(Data)) =:= 11061),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Aggregated) =:= 11061),
    ?_assert(lists:foldl(fun(X, Sum) -> {_, _, Count} = X, Sum + Count end, 0, Anonimized) =:= 813),
    ?_assert(data_anonymization:avg(Anonimized) =:= 2468.093480934809),
    ?_assert(data_anonymization:med(Anonimized) =:= 2549),
    ?_assert(data_anonymization:min(Anonimized) =:= 56),
    ?_assert(data_anonymization:max(Anonimized) =:= 4995)
  ].
