-module(subscribe_msg).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		                terminate/2,code_change/3]).
-export([insert_subscribe_msg/6,insert_subscribe_msg_v2/7]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {host,pid}).
-record(subscribe_users,{room,users}).
-record(session, {sid, usr, us, priority, info, show}).
-record(rbts_map,{cn_name,en_name}).

start_link(Host,Opts) ->
    gen_server:start_link(?MODULE, [Host,Opts], []).

init([Host,_Opts]) ->
	mod_muc_subscribe:add_pid(Host,self()),
    {ok, #state{host = Host,pid = self()}}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({insert_subscribe_msg,Host,Room,User,Nick,Xml,Body},State) ->
	do_insert_subscribe_msg(Host,Room,User,Nick,Xml,Body),
	{noreply,State};
handle_cast({insert_subscribe_msg_v2,Host,Room,Conserver,User,Nick,Body,Msg_Type},State) ->
	do_insert_subscribe_msg_v2(Host,Room,Conserver,User,Nick,Body,Msg_Type),
	{noreply,State};
handle_cast(stop, State) ->
	mod_muc_subscribe:remove_pid(State#state.host,self()),
    {stop, normal, State}.

handle_info(_From,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
	mod_muc_subscribe:remove_pid(State#state.host,self()),
	{ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

insert_subscribe_msg(Host,Room,User,Nick,Xml,Body) ->
	Spid = mod_muc_subscribe:get_random_pid(Host),
	gen_server:cast(Spid, {insert_subscribe_msg,Host,Room,User,Nick,Xml,Body}).

do_insert_subscribe_msg(Host,Room,User,Nick,Xml,Body) ->
	List_Sub_Users = 
		case catch ets:lookup(muc_subscribe_users,Room) of
		[] ->
			[];
		[SU] when is_record(SU,subscribe_users) ->
			At_users = 
				case str:str(Body,<<"@">>) of
				0 ->
					[];
				_ ->
					do_check_at_user(Body,Room,SU#subscribe_users.users,User)
				end,
			get_muc_subscribe_user(User,Host,SU,At_users);
		_ ->
			[]
		end,
	Sub_Users = list_to_binary(List_Sub_Users),
	case size(Sub_Users) of
	0 ->
		ok;
	_ ->
		case catch odbc_queries:insert_subscribe_msg(Host,Sub_Users,Room,Nick,Xml) of
		{selected,[<<"spilt_users_to_insert_xml">>],[[<<"1">>]]} ->
			{atomic, ok};
		Reason ->
			?INFO_MSG("insert_subscribe_msg  ~p ~n",[Reason])
		end
	end.

do_check_at_user(Body,Muc,UL,FUser) ->
	Split_str = str:tokens(Body,<<"@">>),
	?INFO_MSG("insert_subscribe_msg  ~p ~n",[Split_str]),
	Muc_UL =
		case ets:lookup(muc_users,Muc) of
		[{_,MUL}] when is_list(MUL) ->
			MUL;
		_ ->
			[]
		end,
    case Split_str of
    [] ->
        [];
    _ ->
        Str = lists:nth(1,Split_str),
        Users = str:tokens(Str,<<" ">>),
        case Users of [] ->
            [];
        L when is_list(L) ->
            User = lists:nth(1,Users),
            handle_rbt_user(Muc,User,Body,Muc_UL,FUser);
        _ ->
            []
        end
    end.
        
%%   	UL1 = lists:filter(fun({U,S}) -> 
%			 lists:member(U,UL) == false end,Muc_UL),
%     ?INFO_MSG("@ at Body ~p ~n",[UL]),
%     ?INFO_MSG("@ at Body1 ~p ~n",[Muc_UL]),
%	lists:flatmap(fun({U,S}) ->
%		case catch ets:lookup(roster_name_nick,U) of
%		[{_,N,_}] ->
%			L = size(N),
%			case lists:filter(fun(Str) ->
%					str:substr(Str,1,L) =:= N end,Split_str) of
%			[] ->
%				[];
%			_ ->
%				[U]
%			end;
%		_ ->
 %           handle_rbt_user(Muc,U,S,Body),
%			[]
%		end end,UL1).


get_muc_subscribe_user1(User,Server,SU,At_users) ->
	lists:foldl(fun(U,Acc) ->
		case User =/= U of
		true ->
			case ejabberd_sm:get_user_away_rescources(U,Server) of
			[] ->
				Acc;
			L  when is_list(L) ->
				case lists:filter(fun(Rs) -> 
						str:str(Rs,<<"PC_Client">>) =/= 0 orelse str:str(Rs,<<"mac">>) =/= 0 
						orelse str:str(Rs,<<"PC32">>) =/= 0 orelse str:str(Rs,<<"PC64">>) =/= 0 
						orelse str:str(Rs,<<"IOS">>) =/= 0 
						orelse str:str(Rs,<<"none">>) =/= 0 end,L) of
				[] ->	
					Acc;
				_ ->
					case Acc of
					[] ->
						[U];
					_ ->
						lists:append([U,<<",">>],Acc)
					end
				end;
			_ ->
				Acc
			end;
		 _ ->
	  		Acc
		end end,[],lists:concat([SU#subscribe_users.users,At_users])).

get_muc_subscribe_user(User,Server,SU,At_users) ->
	lists:foldl(fun(U,Acc) ->
		case User =/= U of
		true ->
			Rs = get_user_res_and_status(U,Server),
			case ejabberd_sm:judge_away_flag(Rs,true) of
			true ->
				case Acc of
				[] ->
					[U];
				_ ->
					lists:append([U,<<",">>],Acc)
				end;
			_ ->
				Acc
			end;
		_ ->
			Acc
		end end,[],lists:concat([SU#subscribe_users.users,At_users])).

get_muc_subscribe_user_v2(User,Server,SU,At_users) ->
	lists:foldl(fun(U,Acc) ->
		case User =/= U of
		true ->
			Rs = get_user_res_and_status(U,Server),
			case ejabberd_sm:judge_away_flag(Rs,true) of
			true ->
				case Acc of
				[] ->
					[U];
				_ ->
					lists:append([U],Acc)
				end;
			_ ->
				Acc
			end;
		_ ->
			Acc
		end end,[],lists:concat([SU#subscribe_users.users,At_users])).


get_user_res_and_status(User,Server) ->
	case ejabberd_sm:get_user_session(User,Server) of
	[] ->
		[];
	Ss ->
		lists:map(fun(S) ->
			{S#session.show,element(3, S#session.usr)} end,Ss)
	end.

insert_subscribe_msg_v2(Host,Room,ConServer,User,Nick,Body,Msg_Type) ->
    Spid = mod_muc_subscribe:get_random_pid(Host),
    gen_server:cast(Spid, {insert_subscribe_msg_v2,Host,Room,ConServer,User,Nick,Body,Msg_Type}).

do_insert_subscribe_msg_v2(Host,Room,ConServer,User,Nick,Body,Msg_Type) ->
	List_Sub_Users = 
		case catch ets:lookup(muc_subscribe_users,Room) of
		[] ->
			[];
		[SU] when is_record(SU,subscribe_users) ->
            ?INFO_MSG("do_insert_subscribe_msg_v2 ~p ~n",[Body]),
			At_users = 
				case str:str(Body,<<"@">>) of
				0 ->
					[];
				_ ->
					do_check_at_user(Body,Room,SU#subscribe_users.users,User)
				end,
			get_muc_subscribe_user_v2(User,Host,SU,At_users);
		_ ->
			[]
		end,
	case length(List_Sub_Users) of
	0 ->
		ok;
	_ ->
       Http_Body = 
	   		[{obj,[{<<"username">>,List_Sub_Users},	{<<"message">>,Body},{<<"fromname">>,Room},{<<"fromhost">>,ConServer},
			       {<<"nick">>,Nick},{<<"count">>,<<"1">>},{<<"msg_type">>,Msg_Type},{"type",<<"group">>}]}],
		ejabberd_public:send_http_offline_msg(Host,rfc4627:encode(Http_Body))
	end.

handle_rbt_user(Muc,Cn_name,[],UL,FUser) ->
    ok;
handle_rbt_user(Muc,Cn_name,Body,UL,FUser) ->
    ServerHost = str:concat(<<"@conference.">>,lists:nth(1,ejabberd_config:get_myhosts())),
    Host = lists:nth(1,ejabberd_config:get_myhosts()),
    case catch ets:lookup(rbts_map,Cn_name) of
    [RM] when is_record(RM,rbts_map)  ->
        User = RM#rbts_map.en_name,
%        case lists:member({User,Host},UL) of
        case true of
        true ->
            Body1 = str:substr(Body,size(Cn_name)+2,size(Body) - 1 - size(Cn_name)),
            Body2 = 
                case catch str:left(Body1,1) of
                <<" ">> ->
                    str:substr(Body1,2,size(Body1));
               <<",">> ->
                    str:substr(Body1,2,size(Body1));
                _ ->
                    Body1
                end,
            Packet =  ejabberd_public:make_message_packet(<<"subscription">>,Body2,<<"">>,<<"1">>),
            catch ejabberd_router:route(jlib:make_jid(Muc,ServerHost,FUser), 
                              jlib:make_jid(User,Host,<<"">>),Packet);
        _ ->
            []
        end;
    Err ->
        ?INFO_MSG("Not found rbts map ~p ~n",[Err]),
        ok
    end. 
	
