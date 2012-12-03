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
         parse_tags/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
          inited = false,
          output_file,
          no_of_spiders = 0,
          url_matches = [],
          processed_links = dict:new(),
          unprocessed_links = dict:new(),
          words = []
         }).

%%%===================================================================
%%% API
%%%===================================================================
parse_tags() ->
    io:format("about to call gen server..."),
    gen_server:cast(?MODULE, parse_tags).

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
    {ok, File} = application:get_env(spiderl, url_matches),
    Matches = file:consult(File),
    {ok, NoSpiders}  = application:get_env(spiderl, no_of_spiders),
    {ok, OutputFile} = application:get_env(spiderl, output_file),
    {ok, #state{no_of_spiders = NoSpiders,
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
handle_call(Request, _From, State) ->
    io:format("Request is ~p~n", [Request]),
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
handle_cast(parse_tags, #state{inited = false} = State) ->
    io:format("in parse tags (1)..."),
    {ok, File} = application:get_env(spiderl, start_pages),
    Terms = parse_csv:parse_file(File),
    {Words, Unproc} = process_tags(Terms),
    {noreply, State#state{unprocessed_links = Unproc, words = Words,
                          inited = true}};
handle_cast(parse_tags, #state{inited = true} = State) ->
    io:format("Cant reinit...~n"),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("in parse tags (3) for ~p~n", [Msg]),
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
process_tags(Terms) ->
    io:format("Terms is ~p~n", [Terms]),
    {bish, bash}.
