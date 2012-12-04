%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie <gordon@hypernumbers.dev>
%%% @copyright (C) 2012, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created :  3 Dec 2012 by Gordon Guthrie <gordon@hypernumbers.dev>
%%%-------------------------------------------------------------------
-module(spiderl_srv).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

-export([
         dump/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
          output_file,
          no_of_spiders = 0,
          url_matches = [],
          processed_links = [],
          unprocessed_links = [],
          words = dict:new()
         }).

-record(tag,
        {
          tag,
          no_of_plugins,
          plugins = []
          }).

-record(word,
        {
          word,
          tag,
          occurences,
          no_of_plugins_in = 0,
          plugins = []
         }).

%%%===================================================================
%%% API
%%%===================================================================
dump() ->
    List = gen_server:call(?MODULE, dump),
    [io:format("~s~n", [lists:flatten(X)]) || X <- List],
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, File1} = application:get_env(spiderl, url_matches),
    Matches = file:consult(File1),
    {ok, File2} = application:get_env(spiderl, start_pages),
    Terms = parse_csv:parse_file(File2),
    {Words, Unproc} = process_tags(Terms, dict:new()),
    {ok, NoSpiders}  = application:get_env(spiderl, no_of_spiders),
    {ok, OutputFile} = application:get_env(spiderl, output_file),
    {ok, #state{words = Words,
                unprocessed_links = Unproc,
                no_of_spiders = NoSpiders,
                output_file = OutputFile,
                url_matches = Matches}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(dump, _From, State) ->
    Dump1 = [
            io_lib:format("Output File:   ~p", [State#state.output_file]),
            io_lib:format("No Of Spiders: ~p", [State#state.no_of_spiders]),
            io_lib:format("URL Matches:   ~p", [State#state.url_matches]),
            io_lib:format("Output File:   ~p", [State#state.output_file]),
            "Processed links:"
            "----------------"
            ],
    Dump2 = [
             "Unprocessed links:",
            "------------------"
            ],
    Dump3 = [
             "Words:",
             "------"
            ],

    Reply = lists:append([
                          Dump1, State#state.processed_links,
                          Dump2, State#state.unprocessed_links,
                          Dump3, dump_words(State#state.words)
                         ]),

    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_tags(Terms, Words) -> p2(Terms, Words, []).

p2([], Words, Acc) -> {Words, Acc};
p2([H | T], Words, Acc) ->
    {Link, NoTags, Word} = H,
    Tag = #tag{tag = Word, no_of_plugins = list_to_integer(NoTags)},
    NewWords = add_tag(Words, Tag),
    p2(T, NewWords, [Link | Acc]).

add_tag(Words, Tag) ->
    #tag{tag = Word} = Tag,
    NewW = case dict:is_key(Word, Words) of
        true  -> W = dict:fetch(Word, Words),
                 W#word{tag = Tag};
        false -> #word{word = Word, tag = Tag}
    end,
    dict:store(Word, NewW, Words).

dump_words(Words) -> Keys = dict:fetch_keys(Words),
                     d2(Keys, Words, []).

d2([], _Words, Acc)     -> lists:sort(Acc);
d2([H | T], Words, Acc) -> W = dict:fetch(H, Words),
                           NewAcc = io_lib:format("~p", [W]),
                           d2(T, Words, [NewAcc | Acc]).
