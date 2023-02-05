-module(be).
-export([get_user_tuples/3, data_to_file/2, aggregate/1, anonimize/1,
  avg/1, min/1, max/1, med/1, compute_stats/3, groups_stats/2]).

-spec compute_stats(Directory, Include_duplicates, Group_size) -> list() when
  Directory::string(),
  Include_duplicates::yes | no,
  Group_size ::number().
%% @doc Parses JSON dataset, extracts airline ticket prices, generates tuples, aggregates them,
%% anonimizes and then computes stats: AVG, MED, MIN, MAX. Reads the dataset from
%% <code>Directory</code> path string, <code>Include_duplicates</code> specifies whether to
%% return duplicate tuples (yes) per user or remove them (no). <code>Group_size</code> specifies
%% size of histogram buckets.
compute_stats(Directory, Include_duplicates, Group_size) ->
  User_tuples = get_user_tuples(Directory, Include_duplicates, Group_size),
  Aggregated = aggregate(User_tuples),
  Anonimized = anonimize(Aggregated),
  [ {avg, avg(Anonimized)},
    {med, med(Anonimized)},
    {min, min(Anonimized)},
    {max, max(Anonimized)} ].

%% @doc Wrapper function that combines several preprocessing steps such as dataset deserialization
%% up to generation of user price tuples.
get_user_tuples(Directory, Include_duplicates, Group_size) ->
  Files = user_files(Directory),
  lists:map(fun(Filename) ->
    Binary_data = file_to_binary(Filename), % Reads a single JSON file
    Json_data = decode_json(Binary_data), % Decodes the JSON
    Prices_list = extract_user_prices(Json_data), % Extracts a list of prices for a user
    compute_user_tuples(Prices_list, Include_duplicates, Group_size) % Creates a list of tuples
            end, Files).

%% @doc Extracts airplane ticket purchase prices per user from decoded JSON structure.
extract_user_prices(Json_data) ->
  %Get inner purchases proplist
  Purchases = proplists:get_value(<<"purchases">>, Json_data),
  %Get inner tuple lists
  A = lists:map(fun(L) -> element(1, L) end, Purchases),
  %Filter out tuple lists that are not of type "airline"
  Airlines = lists:filter(fun(L) -> proplists:get_value(<<"type">>, L) == <<"airline">> end, A),
  lists:map(fun(X) ->
    proplists:get_value(<<"amount">>, X) end, Airlines).

%% @doc Processes PER USER airplane ticket purchase prices. Individual user data is processed
%% in isolation, no information about other users is passed around.
%% During this step data grouping and optional removal of duplicates occurs.
compute_user_tuples(Prices_list, Include_duplicates, Group_size) ->
  % This fun defines how individual users tuples will look like
  Mapfun = fun(X) ->
    % Alternatively, I could generate {User_id, round(X / Group_size)}
    % but in this case the user id is never needed at any point while
    % group size comes handy later on, especially if the method ever
    % used dynamic sizes.
    {Group_size, round(X / Group_size)} end,
  Result = lists:map(Mapfun, Prices_list),
  case Include_duplicates of
    yes ->
      lists:sort(Result);
    no ->
      lists:usort(Result)
  end.

% ---- Aggregation & Anonimization ----

%% @doc Aggregates user price tuples and returns triples with occurrence count.
aggregate(User_tuples) ->
  Flat_list = lists:flatten(User_tuples),
  Uniq_list = lists:usort(Flat_list),
  lists:map(fun(X) ->
    {A, B} = X,
    {A, B, length([ok || I <- Flat_list, I == X])} end, Uniq_list).

%% @doc Anonimizes user price triples based on Count > 5 constraint.
anonimize(Aggregated_user_triples) ->
  lists:filter(fun(X) ->
    {_, _, Count} = X,
    Count > 5 end, Aggregated_user_triples).

% ---- Statistics ----

%% @doc Computes average ticket purchase price.
avg(Aggregated_user_triples) ->
  avg(Aggregated_user_triples, 0, 0).

avg([H | T], Length, Sum) ->
  {Group_size, Value, Count} = H,
  avg(T, Length + Count, Sum + (Group_size * Value * Count));

avg([], Length, Sum) ->
  Sum / Length.

%% @doc Computes median ticket purchase price.
med(Aggregated_user_triples) ->
  % Sort on {_, Purchase_price, _}
  Sorted = lists:keysort(2, Aggregated_user_triples),
  Length = lists:foldl(fun(X, Sum) ->
    {_, _, Count} = X,
    Count + Sum end, 0, Sorted),

  case even_or_odd(Length) of
    even ->
      A = get_element_at(Length / 2, Sorted),
      B = get_element_at(Length / 2 + 1, Sorted),
      (A + B) / 2;
    odd -> get_element_at((Length + 1) / 2, Sorted)
  end.

%% @doc Computes minimum ticket purchase price.
min(Aggregated_user_triples) ->
  Values = get_values(Aggregated_user_triples),
  lists:min(Values).

%% @doc Computes maximum ticket purchase price.
max(Aggregated_user_triples) ->
  Values = get_values(Aggregated_user_triples),
  lists:max(Values).

% ---- Utilities ----

%% @doc Utility function that returns <code>Nth</code> element of a triple list,
%% taking into account that the data is aggregated.
get_element_at(Nth, Aggregated_user_triples) when Nth >= 1 ->
  get_element_at(Nth, Aggregated_user_triples, 1).

get_element_at(Nth, [H|T], Sum) ->
  {Group_size, Value, Count} = H,
  if
    Sum + Count > Nth -> Value * Group_size;
    true -> get_element_at(Nth, T, Sum + Count)
  end.

%% @doc Returns whether <code>Number</code> is even or odd.
even_or_odd(Number) ->
  if
    (Number band 1) == 0 -> even;
    true -> odd
  end.

%% @doc Utility function that returns a list of actual ticket prices.
get_values(Aggregated_user_triples) ->
  lists:map(fun(X) ->
    {Group_size, Value, _} = X,
    Group_size * Value end, Aggregated_user_triples).

%% @doc Utility function that computes useful data given a list of group sizes.
groups_stats(Directory, Groups) ->
  lists:map(fun(X) ->
    User_tuples = get_user_tuples(Directory, no, X),
    Aggregated = aggregate(User_tuples),
    Anonimized = anonimize(Aggregated),
    Len = lists:foldl(fun(Y, Sum) ->
      {_, _, Count} = Y,
      Sum + Count end, 0, Anonimized),
    {X, Len, avg(Anonimized), med(Anonimized), min(Anonimized), max(Anonimized)} end, Groups).

% ---- File utility functions ----

%% @doc Reads a file to binary and splits it on new line character.
file_to_binary(File) ->
  {ok, Data} = file:read_file(File),
  Data.

%% @doc Decodes binary JSON data and returns proplist of the serialized values.
decode_json(Binary_data) ->
  element(1, jiffy:decode(Binary_data)).

%% @doc Returns a list of JSON file paths in a given directory.
user_files(Dir) ->
  Data_wildcard = Dir ++ "*.json",
  filelib:wildcard(Data_wildcard).

%% @doc Utility function that writes any <code>Data</code> to a file under given <code>Path</code>.
data_to_file(Path, Data) ->
  file:write_file(Path, io_lib:fwrite("~p.\n", [Data])).