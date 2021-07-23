%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 7月 2021 下午1:53
%%%-------------------------------------------------------------------
-module(influxdb_line).
-author("root").
-export([encode/1]).



encode(Point) when is_map(Point) -> encode([Point]);
encode(Points) when is_list(Points) ->
  try encode_([atom_key_map(Point) || Point <- Points]) of
    Encoded -> Encoded
  catch
    error:Reason -> {error, Reason}
  end.

encode_(Points) when is_list(Points), length(Points) > 0 ->
  lists:foldr(fun (Point, Acc) when is_map(Point) -> [encode_(Point) | Acc] end, [], Points);
encode_(Point = #{measurement := Measurement, fields := Fields}) ->
  [encode_measurement(Measurement),
    encode_tags(maps:get(tags, Point, #{})),
    " ",
    encode_fields(Fields),
    case maps:get(timestamp, Point, undefined) of
      undefined -> [];
      Timestamp -> [" ", encode_timestamp(Timestamp)]
    end,
    "\n"];
encode_(_Point) -> error(invalid_point).

encode_measurement(Measurement) ->
  escape_special_chars(measurement,to_binary(Measurement)).

encode_fields(Fields) when is_map(Fields) -> encode_fields(maps:to_list(Fields));
encode_fields([]) -> error(missing_field);
encode_fields([{Key, Value}]) -> encode_field(Key, Value);
encode_fields([{Key, Value} | Rest]) -> [encode_field(Key, Value), ",", encode_fields(Rest)].

encode_field(Key, Value) ->
  [escape_special_chars(field_key, to_binary(Key)), "=", encode_field_value(Value)].

encode_field_value(Value) when is_integer(Value) ->
  Int = erlang:integer_to_binary(Value),
  <<Int/binary, "i">>;
encode_field_value(Value) when is_float(Value) ->
  erlang:float_to_binary(Value, [compact, {decimals, 12}]);
encode_field_value(Value) when is_atom(Value) ->
  if Value =:= t;Value =:= 'T';Value =:= true;Value =:= 'True';Value =:= 'TRUE' ->
    <<"t">>;
    Value =:= f;
    Value =:= 'F';
    Value =:= false;
    Value =:= 'False';
    Value =:= 'FALSE' ->
      <<"f">>;
    true -> encode_field_value(to_binary(Value))
  end;
encode_field_value(Value) ->
  Bin = escape_special_chars(field_value, to_binary(Value)),
  <<<<"\"">>/binary, Bin/binary, <<"\"">>/binary>>.

encode_tags(Tags) when is_map(Tags) -> encode_tags(maps:to_list(Tags));
encode_tags([]) -> [];
encode_tags([{Key, Value}]) -> encode_tag(Key, Value);
encode_tags([{Key, Value} | Rest]) -> [encode_tag(Key, Value), encode_tags(Rest)].

encode_tag(Key, Value) ->
  [",", escape_special_chars(tag_key, to_binary(Key)), "=", escape_special_chars(tag_value, to_binary2(Value))].

encode_timestamp(Timestamp) when is_integer(Timestamp) ->
  erlang:integer_to_binary(Timestamp).

escape_special_chars(field_value, String) when is_binary(String) ->
  escape_special_chars([<<"\"">>], String);
escape_special_chars(measurement, String) when is_binary(String) ->
  escape_special_chars([<<",">>, <<" ">>], String);
escape_special_chars(Element, String) when is_binary(String), Element =:= tag_key;Element =:= tag_value;Element =:= field_key ->
  escape_special_chars([<<",">>, <<"=">>, <<" ">>], String);
escape_special_chars(Pattern, String) when is_list(Pattern) ->
  binary:replace(String, Pattern, <<"\\">>, [global, {insert_replaced, 1}]).

to_binary(Data) when is_binary(Data) -> Data;
to_binary(Data) when is_list(Data) -> erlang:list_to_binary(Data);
to_binary(Data) when is_atom(Data) -> erlang:atom_to_binary(Data, utf8);
to_binary(_) -> error(invalid_type).

to_binary2(Data) when is_integer(Data) -> erlang:integer_to_binary(Data);
to_binary2(Data) when is_float(Data) -> erlang:float_to_binary(Data, [{decimals, 8}, compact]);
to_binary2(Data) -> to_binary(Data).

atom_key_map(BinKeyMap) when is_map(BinKeyMap) ->
  maps:fold(fun (K, V, Acc) when is_binary(K) ->
    Acc#{binary_to_atom(K, utf8) => V};
    (K, V, Acc) -> Acc#{K => V}
            end,
    #{},
    BinKeyMap).
