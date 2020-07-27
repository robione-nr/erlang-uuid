%% @author nolan
%% @doc @todo Add description to uuid.


-module(uuid).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

-include("../include/uuid.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([nil/0, new/0, to_string/1, to_binary/1]).
-export([v1/0, v2/0, v2/1, v2/2, v3/1, v3/2, v4/0, v5/1, v5/2]).

nil() -> <<0:128>>.

new() -> v4().

to_string(<<_:128>> = UUID) ->
	<<Int:128/integer-unsigned>> = UUID,
	Str = lists:flatten(string:pad(integer_to_list(Int,16),32, leading, $0)),
	
	lists:flatten(lists:join("-", [
		string:slice(Str,0,8),
		string:slice(Str,8,4),
		string:slice(Str,12,4),
		string:slice(Str,16,2),
		string:slice(Str,18,2),
		string:slice(Str,20,12)	]));	
to_string(_) ->
	{error, badarg}.

to_binary(String) ->
	try
		case re:run(String, <<"^([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{2}-[0-9a-f]{2}-[0-9a-f]{12})$">>, [caseless]) of
		nomatch ->
			{error, badarg};
		_ ->
			<<(list_to_integer(lists:flatten(string:replace(String, "-", "", all)),16)):128/integer-unsigned>>
		end
	catch
		_:_ -> {error, badarg}
	end.

v1() -> gen_server:call(?MODULE, new_v1).

v2() -> v2([], ?UUID_DOMAIN_ALL).

v2(User) when is_list(User) ->
	gen_server:call(?MODULE, {new_v2, User, ?UUID_DOMAIN_ALL});	
v2(User) when is_binary(User) ->
	v2(unicode:characters_to_list(User));
v2(User) when is_atom(User) ->
	v2(atom_to_binary(User, utf8));
v2(_) ->
	{error, badarg}.

v2(User, Domain) when is_list(User) andalso Domain >= 0 andalso Domain =<2 ->
	gen_server:call(?MODULE, {new_v2, User, Domain});	
v2(User, Domain) when is_binary(User) ->
	v2(unicode:characters_to_list(User), Domain);
v2(User, Domain) when is_atom(User) ->
	v2(atom_to_binary(User, utf8), Domain);
v2(_, _) ->
	{error, badarg}.

v3(Namespace,Name) ->
	v3({Namespace, Name}).

v3({Namespace, Name}) -> 
	BinNS = term_to_binary(Namespace),
	BinNm = term_to_binary(Name),
	
	<<P1:48, _:4, P2:12, _:2, P3:62>> = crypto:hash(md5, <<BinNS/binary, BinNm/binary>>),
	<<P1:48, 2#0011:4, P2:12, 2#10:2, P3:62>>.

v4() -> 
	<<P1:48, _:4, P2:12, _:2, P3:62>> = crypto:strong_rand_bytes(16),
	<<P1:48, 2#0100:4, P2:12, 2#10:2, P3:62>>.

v5(Namespace,Name) ->
	v5({Namespace, Name}).

v5({Namespace, Name}) ->
	BinNS = term_to_binary(Namespace),
	BinNm = term_to_binary(Name),
	
	<<P1:48, _:4, P2:12, _:2, P3:62>> = <<(crypto:hash(sha, <<BinNS/binary, BinNm/binary>>)):128/bits>>,
	<<P1:48, 2#0101:4, P2:12, 2#10:2, P3:62>>.

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link() ->
	start_link([]).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% ====================================================================
%% Framework Functions
%% ====================================================================

%% init/1
%% ====================================================================
init([]) ->
	application:start(crypto),
	
	<<MAC:6/binary, _/binary>> =
		try
			{ok, List} = inet:getifaddrs(),
			list_to_binary(hd([Addr || {_, Opts} <- List, {hwaddr, Addr} <- Opts,
							{addr, _} <- Opts, {flags, Flags} <- Opts,
							lists:member(loopback,Flags) =/= true]
			))
		catch
			_:_ -> 
				<<P1:7, _:1, P2:40>> = crypto:strong_rand_bytes(6),
				<<P1:7, 1:1, P2:40>>
		end,
	
	<<ClkSeq:16/integer-unsigned>> = crypto:strong_rand_bytes(2),
	
    {ok, {MAC, ClkSeq band 16#3fff}}.


%% handle_call/3
%% ====================================================================
handle_call(new_v1, _, {Node, ClkSeq}) ->
	<<_:4, TH:12, TM:16, TL:32>> = <<(timestamp()):64/integer-unsigned>>,
	Reply = <<TL:32, TM:16, 2#0001:4, TH:12, 2#10:2, ClkSeq:14/integer-unsigned, Node:6/binary>>,
	{reply, Reply, {Node, (ClkSeq +1) band 16#3fff}};
handle_call({new_v2, User, Domain}, _, {Node, ClkSeq}) ->
	<<_:4, TH:12, TM:16, _:32>> = <<(timestamp()):64/integer-unsigned>>,

	Reply = try
		UID = list_to_integer(string:chomp(os:cmd("id -ur " ++ User))),
		GID = list_to_integer(string:chomp(os:cmd("id -gr " ++ User))),
	
		TL = case Domain of
		?UUID_DOMAIN_ALL -> <<(GID):16/integer-unsigned, (UID):16/integer-unsigned>>;
		?UUID_DOMAIN_GROUP -> <<(GID):32/integer-unsigned>>;
		?UUID_DOMAIN_PERSON -> <<(UID):32/integer-unsigned>>
		end,
	
		<<TL:4/binary, TM:16, 2#0010:4, TH:12, 2#10:2, 
		  ClkSeq:6/integer, Domain:8/integer, Node:6/binary>>
	
	catch
		_:_ -> {error,badarg}
	end,
	
	{reply, Reply, {Node, (ClkSeq +1) band 16#3fff}};
handle_call(_, _, State) ->
    {reply, {error, badarg}, State}.


%% handle_cast/2
%% ====================================================================
handle_cast(_, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
handle_info(_, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(_, _) ->
	application:stop(crypto),
    ok.


%% code_change/3
%% ====================================================================
code_change(_, State, _) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

timestamp() ->
	(erlang:monotonic_time(nanosecond) + 2167963015000000000) div 100.