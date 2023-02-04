-module(data_extraction).
-export([airline_tuples/1, data_to_file/2, aggregate/1, anonimize/1, avg/1, min/1, max/1, med/1]).
%% Remember to use -A concurrency when reading files.

file_to_binary(File) ->
	{ok, Data} = file:read_file(File),
	%split the lines if the JSON is not in a single line
	binary:split(Data, [<<"\n">>], [global]).

decode_json(Binary_data) ->
	{struct, Json_data} = mochijson2:decode(Binary_data),
	Json_data.

extract_user_data(Json_data) ->
	%deepprops usable here?
	%Get inner purchases proplist
	Purchases = proplists:get_value(<<"purchases">>, Json_data),
	%Get inner tuple lists
	A = proplists:get_all_values(struct, Purchases),
	%Filter out tuple lists that are not of type "airline"
	Airlines = lists:filter(fun(L) -> proplists:get_value(<<"type">>, L) == <<"airline">> end, A),
	%Mapping function
	Mapfun = fun(X) -> {"airline", proplists:get_value(<<"amount">>, X)} end,
	Result = lists:map(Mapfun, Airlines),
	%Remove duplicates
	Set = sets:from_list(Result), %basic alternative to implementing custom uniqueness logic.
	sets:to_list(Set).

user_files(Dir) ->
	Data_wildcard = Dir ++ "*.json",
	filelib:wildcard(Data_wildcard).

airline_tuples(Directory) ->
	Files = user_files(Directory),
	Result = lists:map(fun(Filename) ->
		Binary_Data = file_to_binary(Filename),
		Json_data = decode_json(Binary_Data),
		extract_user_data(Json_data) end, Files),
	Result.

data_to_file(File, Data) ->
	file:write_file(File, io_lib:fwrite("~p.\n", [Data])).

%NEW FILE
aggregate(Airline_data) ->
	Flat_list = lists:flatten(Airline_data),
	Uniq = sets:from_list(Flat_list),
	Uniq_list = sets:to_list(Uniq),
	lists:map(fun (X) ->
		{A, B} = X,
		{A, B, length([ok || I <- Flat_list, I == X])} end, Uniq_list
	).
	%Aggregated = lists:foldl(fun (Airline_tuple, CountAcc) ->
	%	{Airline_tuple, CountAcc}

anonimize(Airline_data) ->
	lists:filter(fun (X) ->
		{_, _, Count} = X,
		Count > 5 end, Airline_data).

avg(Airline_data) ->
	avg(Airline_data, 0, 0).

avg([H|T], Length, Sum) ->
	{_, Value, Count} = H,
	avg(T, Length + Count, Sum + (Value * Count));

avg([], Length, Sum) ->
	Sum / Length.

med(Airline_data) ->
	Sorted = lists:keysort(2, Airline_data),
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

get_element_at(Nth, Airline_data) when Nth >= 1 ->
	get_element_at(Nth, Airline_data, 1).

get_element_at(Nth, [H|T], Sum) ->
	{_, Value, Count} = H,
	if
		Sum + Count > Nth -> Value;
		true -> get_element_at(Nth, T, Sum + Count)
	end.

even_or_odd(Number) ->
	if
		(Number band 1) == 0 -> even;
		true -> odd
	end.

min(Airline_data) ->
	Values = get_values(Airline_data),
	lists:min(Values).

max(Airline_data) ->
	Values = get_values(Airline_data),
	lists:max(Values).

get_values(Airline_data) ->
	lists:map(fun(X) ->
		{_, Value, _} = X,
		Value end, Airline_data).
