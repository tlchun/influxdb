%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 7ζ 2021 δΈε1:53
%%%-------------------------------------------------------------------
-module(influxdb_app).
-author("root").

-behaviour(application).
-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
  influxdb_sup:start_link().

stop(_State) -> ok.