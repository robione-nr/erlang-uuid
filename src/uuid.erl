%% @author nolan
%% @doc @todo Add description to uuid.

-module(uuid).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

-compile(inline).

%% Build internal "unused" functions
-export([v1/3, v4/0]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([nil/0, new/0, new/1, new/2, to_string/1, to_binary/1, sync/0, setopt/1]).

nil() -> <<0:128>>.

new() ->
	gen_server:call(?MODULE, {new_uuid, 1}).

new(N) when is_integer(N) andalso N > 0 ->
	gen_server:call(?MODULE, {new_uuid, N});
new(_) ->
	{error, badarg}.

new(NS, N) ->
	gen_server:call(?MODULE, {new_uuid,NS,N}).

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

sync() -> gen_server:cast(?MODULE, calc_offset).

setopt(Param = {version, V}) when is_integer(V) andalso V >= 1 andalso V =< 5 ->
	gen_server:call(?MODULE, Param);
setopt(Param = {privacy, V}) when is_boolean(V) ->
	gen_server:cast(?MODULE, Param);
setopt(_) ->
	{error, badarg}.
	
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
init(OptList) when is_list(OptList) ->
	application:start(crypto),
	
	Version = proplists:get_value(version,OptList,v1),
	
	PrivMAC = case proplists:is_defined(privacy, OptList) of
				  true -> proplists:get_bool(privacy,OptList);
				  false -> true
			  end,
	
	NodeID = get_node(PrivMAC),
	
	<<ClkSeq:16/integer-unsigned>> = crypto:strong_rand_bytes(2),
	
    {ok, {Version, NodeID, ClkSeq band 16#3fff, time_offset()}}.


%% handle_call/3
%% ====================================================================
handle_call({new_uuid, N}, _, {v4,_,_,_} = State) ->
	{reply, iterate({v4,[]},[],N), State};

handle_call({new_uuid, N}, _, State = {v1, Node, ClkSeq, Offset}) ->
	{reply, iterate({v1,[Node, ClkSeq, Offset]}, [],N), State};

handle_call({new_uuid, NS, N}, _, {v3,_,_,_} = State) ->
	{reply, vNS(NS,N,3), State};

handle_call({new_uuid, NS, N}, _, {v5,_,_,_} = State) ->
	{reply, vNS(NS,N,5), State};

handle_call({version, N}, _, {Version, Node, ClkSeq, Offset}) ->
	{Reply, NewVer} =
			try
				{ok, list_to_existing_atom([$v|integer_to_list(N)])}
			catch
				_:_ -> {{error, undef}, Version}
			end,
				
	{reply, Reply, {NewVer, Node, ClkSeq, Offset}};

handle_call(_, _, State) ->
    {reply, {error, badarg}, State}.


%% handle_cast/2
%% ====================================================================
handle_cast(calc_offset, {Version, Node, ClkSeq, _}) ->
	{noreply, {Version, Node, ClkSeq, time_offset()}};
handle_cast({privacy, V}, {Version, _, ClkSeq, Offset}) ->
	{noreply, {Version, get_node(V), ClkSeq, Offset}};
handle_cast(_, State) ->
    {noreply, State}.


%% Placeholders
%% ====================================================================
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% Loop
iterate(_, Acc, 0) -> 
	Acc;
iterate(MFA = {F, A}, Acc, N) ->
	iterate(MFA, [erlang:apply(?MODULE, F,A) | Acc], N-1).

%% Node Stuff
get_node(Private) ->
	case Private of
		true -> random_nodeid();
		false -> 
			try
				{ok, List} = inet:getifaddrs(),
				
				<<NodeID:6/binary, _/binary>> =
					
				list_to_binary(hd([Addr || {_, Opts} <- List, {hwaddr, Addr} <- Opts,
								{addr, _} <- Opts, {flags, Flags} <- Opts,
								lists:member(loopback,Flags) =/= true]
				)),
				
				NodeID
			catch
				_:_ -> random_nodeid()
			end
	end.

random_nodeid() ->
	<<P1:7, _:1, P2:40>> = crypto:strong_rand_bytes(6),
	<<P1:7, 1:1, P2:40>>.

%% Time Stuff
time_offset() ->
	%% 141427 		*	86400	*	1000000000
	%% 1582 - Epoch		Sec/Day		NanoS/Sec
	12219292800000000000 + erlang:time_offset(nanosecond).

timestamp(Offset) ->
	(erlang:monotonic_time(nanosecond) + Offset) div 100.

%% UUID Versions
v1(Node, ClkSeq, Offset) ->
	<<_:4, TH:12, TM:16, TL:32>> = <<(timestamp(Offset)):64/integer-unsigned>>,
	<<TL:32, TM:16, 2#0001:4, TH:12, 2#10:2, ClkSeq:14/integer-unsigned, Node:6/binary>>.

v4() ->
	<<I:128/unsigned-integer>> = crypto:strong_rand_bytes(16),
	<<(I band 16#ffffffffffff0fff3fffffffffffffff bor 16#40008000000000000000):128/unsigned-integer>>.

vNS(Namespace, Name, Version) ->
	BinNS = term_to_binary(Namespace),
	BinNm = term_to_binary(Name),

	Method = case Version of
	3 ->	md5;
	5 ->	sha
	end,
	
	<<I:128/unsigned-integer>> = <<(crypto:hash(Method, <<BinNS/binary, BinNm/binary>>)):128/bits>>,
	<<(I band 16#ffffffffffff0fff3fffffffffffffff bor (Version bsl 76 bor 16#8000000000000000)):128/unsigned-integer>>.
