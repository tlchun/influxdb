%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 7月 2021 下午1:54
%%%-------------------------------------------------------------------
-module(influxdb_sup).
-author("root").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local, influxdb_sup},influxdb_sup,[]).

init([]) -> {ok, {{one_for_one, 10, 100}, []}}.
