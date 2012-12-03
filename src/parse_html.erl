%%% @author Gordon Guthrie <gordon@hypernumbers.dev>
%%% @copyright (C) 2012, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created :  3 Dec 2012 by Gordon Guthrie <gordon@hypernumbers.dev>

-module(parse_html).

-export([
         parse_file/1
         ]).

-export([
         test/0
         ]).

test() ->
    File = "/home/gordon/websites/plugins/wordpress.org/extend/plugins/zpecards/index.html",
    {ok, Binary} = file:read_file(File),
    Tree = mochiweb_html:parse(Binary),
    Links = strip(mochiweb_xpath:execute("//a/@href", Tree)),
    io:format("Links is ~p~n", [Links]),
    ok.

parse_file(_File) ->
    ok.

strip(List) -> s2(List, []).

s2([], Acc)       -> Acc;
s2([[] | T], Acc) -> s2(T, Acc);
s2([H | T], Acc)  -> L = binary_to_list(H),
                     Tks = string:tokens(L, "="),
                     NewAcc = case length(Tks) of
                                  3 -> [_ | Rest] = Tks,
                                       string:join(Rest, "=");
                                  2 -> lists:nth(2, Tks);
                                  1 -> [Tk] = Tks,
                                       Tk
                     end,
                     s2(T, [NewAcc | Acc]).
