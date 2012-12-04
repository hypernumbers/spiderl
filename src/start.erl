%%% @author Gordon Guthrie <gordon@hypernumbers.dev>
%%% @copyright (C) 2012, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created :  4 Dec 2012 by Gordon Guthrie <gordon@hypernumbers.dev>

-module(start).

-export([
         start/0
        ]).

start() -> application:start(spiderl).
