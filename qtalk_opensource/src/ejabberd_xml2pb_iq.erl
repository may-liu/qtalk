-module(ejabberd_xml2pb_iq).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([encode_pb_error_iq/3,encode_muc_user_pb/3,encode_pb_iq_bind_result/4]).
-export([encode_pb_iq_create_muc/3,encode_user_muc_pb/3,encode_muc_invite_user_v2_pb/3]). 
-export([encode_pb_muc_user_register/3,encode_pb_muc_user_del_register/3,encode_pb_muc_amdin/3]).
-export([encode_pb_set_friend_opt/3,encode_pb_get_user_friends/3,encode_pb_del_user_friend/3]).
-export([encode_pb_time_http_key/3,encode_pb_get_friend_opt/3,encode_pb_destroy_muc/3]).
-export([encode_pb_ping/3,encode_pb_get_mask_user/3,encode_pb_set_mask_user/3,encode_pb_cancel_mask_user/3]).
-export([encode_pb_handle_user_subscribe/3]).
-export([struct_pb_iq_msg/10]).


%%----------------------------------------------
%% @date 2016-9
%% 对iqmessage进行encode
%% qunar.com
%%----------------------------------------------
encode_pb_iq_msg(Key,Val,Msg_ID,Header,Body,Headers,Bodys) ->
	PB_IQ = 
		#iqmessage{
    			value = Val,
    			messageid = Msg_ID,
    			header = Header,
    			body = Body,
    			receivedtime = mod_time:get_timestamp(),
    			headers = Headers,
    			bodys = Bodys
			},
    FPB_IQ = handle_pb_iq_key(Key,PB_IQ), 
	message_pb:encode_iqmessage(FPB_IQ).


handle_pb_iq_key(Key,IQ) ->
    case ejabberd_xml2pb_public:set_iqKey_type(Key) of
    'none' ->
        IQ#iqmessage{key = Key};
    V ->
        IQ#iqmessage{definedkey = V}
    end.
       


%%----------------------------------------------
%% @date 2016-9
%% 对iqmessage进行封包
%% qunar.com
%%----------------------------------------------
struct_pb_iq_msg(From,To,Type,Key,Val,Msg_ID,Header,Body,Haeders,Bodys) ->
        IQ = list_to_binary(encode_pb_iq_msg(Key,Val,Msg_ID,Header,Body,Haeders,Bodys)),
        ?DEBUG("IQ ~p,Type ~p  ~n",[IQ,Type]),
        Pb_Msg = list_to_binary(ejabberd_xml2pb_public:encode_pb_protomessage(From,To,Type,0,IQ)),
        ?DEBUG("Pb_MSg ~p , size ~p~n",[Pb_Msg,size(Pb_Msg)]),
        Opt = ejabberd_xml2pb_public:get_proto_header_opt(Pb_Msg),
        Res = list_to_binary(ejabberd_xml2pb_public:encode_pb_protoheader(Opt,Pb_Msg)),
        ?DEBUG("Pb_MSg  , sizeen   ~p~n",[size(Res)]),
        Res.
        
	
%%----------------------------------------------
%% @date 2016-9
%% 构建PB获取群成员信息Bodys
%% qunar.com
%%----------------------------------------------
get_pb_iq_muc_user_bodys(Packet) ->
%%	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    case xml:get_subtag(Packet,<<"query">>) of
    false ->
        []; 
    Query ->
        M_users = xml:get_subtags(Query,<<"m_user">>),
        Bodys =	lists:flatmap(fun(Xml) ->
              Headers = 
                 case is_record(Xml,xmlel) of
                 true ->
        		    JID = proplists:get_value(<<"jid">>,Xml#xmlel.attrs,<<"">>),
                    case proplists:get_value(<<"affiliation">>,Xml#xmlel.attrs) of
                    undefined ->
                        [{<<"jid">>,JID}];
                    Aff ->
                        [{<<"jid">>,JID},
                         {<<"affiliation">>,Aff}]
                    end;
                _ ->
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"m_user">>)] end,M_users),
        Bodys
    end.
	
%%----------------------------------------------
%% @date 2016-9
%% PB获取群成员信息
%% qunar.com
%%----------------------------------------------
encode_muc_user_pb(From,To,Packet) ->
	Bodys = get_pb_iq_muc_user_bodys(Packet),
    ID = case  xml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(mod_time:timestamp());
    {Value,I} ->
            I
    end,    
	struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_users">>,ID,'undefined','undefined',[],Bodys).
		

%%----------------------------------------------
%% @date 2016-9
%% encode PB error IQ
%% qunar.com
%%----------------------------------------------
encode_pb_error_iq(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"error">>) of
        false ->
            'undefined';
        Error ->
            case xml:get_attr(<<"code">>,Error#xmlel.attrs) of
            false ->
                 'undefined';
            {value,Code} ->
               CData = xml:get_subtag_cdata(Error,<<"text">>),
               Headers = [{<<"code">>,Code},
                          {<<"data">>,CData}],
               ejabberd_xml2pb_public:encode_messagebody(Headers,<<"error_info">>)
            end
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"error">>,'undefined',ID,'undefined',Body,[],[]).
   
%%----------------------------------------------
%% @date 2016-9
%% encode PB IQ BIND Resut
%% qunar.com
%%----------------------------------------------
encode_pb_iq_bind_result(From,To,Packet,Key) ->
    Body = 
        case xml:get_subtag(Packet,<<"bind">>) of
        false ->
            'undefined';
        Bind ->
            CData = xml:get_subtag_cdata(Bind,<<"jid">>),
            Headers = [{<<"time_value">>,integer_to_binary(mod_time:get_timestamp())},
                       {<<"key_value">>,Key}],
            ejabberd_xml2pb_public:encode_messagebody(Headers,CData)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"bind">>,ID,'undefined',Body,[],[]).
           
%%----------------------------------------------
%% @date 2016-9
%% encode PB IQ Create MUC
%% qunar.com
%%----------------------------------------------
encode_pb_iq_create_muc(From,To,Packet) ->
    Body = case xml:get_subtag(Packet,<<"query">>) of
           false ->
                'undefined';
           Query ->
                case xml:get_subtag(Query,<<"create_muc">>) of
                false ->
                    'undefined';
                Muc_Res ->
                    case xml:get_attr(<<"result">>,Muc_Res#xmlel.attrs) of
                    false ->
                            'undefined';
                    {value,Res} ->
                         ejabberd_xml2pb_public:encode_messagebody([],Res)
                    end
                end
            end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"create_muc">>,ID,'undefined',Body,[],[]).
                    
    
%%----------------------------------------------
%% @date 2016-9
%% 获取PB类型IQ：用户所有注册群的bodys
%% qunar.com
%%----------------------------------------------
get_pb_iq_user_mucs_bodys(Packet) ->
%%	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    case xml:get_subtag(Packet,<<"query">>) of
    false ->
        ?INFO_MSG("get_pb_iq_user_mucs_bodys1 ~p ~n",[Packet]),
        []; 
    Query ->
        Mucs = xml:get_subtags(Query,<<"muc_rooms">>),
        Bodys =	lists:flatmap(fun(Xml) ->
              Headers = 
                 case is_record(Xml,xmlel) of
                 true ->
        		    Host = proplists:get_value(<<"host">>,Xml#xmlel.attrs,<<"conference.ejabhost1">>),
                    case proplists:get_value(<<"name">>,Xml#xmlel.attrs) of
                    undefined ->
                        ?INFO_MSG("get_pb_iq no found name ~n",[]),
                        [];
                    Name ->
                        [{<<"name">>,Name},
                         {<<"host">>,Host}]
                    end;
                _ ->
                    ?INFO_MSG("get_pb_iq_user_mucs_bodys ~p ~n",[Packet]),
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"muc_room">>)] end,Mucs),
        Bodys
    end.
	
%%----------------------------------------------
%% @date 2016-9
%% 对PB类型IQ进行组装
%% qunar.com
%%----------------------------------------------
encode_user_muc_pb(From,To,Packet) ->
	Bodys = get_pb_iq_user_mucs_bodys(Packet),
    ID = case  xml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(mod_time:timestamp());
    {Value,I} ->
            I
    end,    
	struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"user_mucs">>,ID,'undefined','undefined',[],Bodys).

%%----------------------------------------------
%% @date 2016-9
%% 获取邀请muc用户的bodys
%% qunar.com
%%----------------------------------------------
get_pb_iq_invite_muc_user_bodys(Packet) ->
    case xml:get_subtag(Packet,<<"query">>) of
    false ->
        []; 
    Query ->
        M_invites = xml:get_subtags(Query,<<"muc_invites">>),
        Bodys =	lists:flatmap(fun(Xml) ->
              Headers = 
                 case is_record(Xml,xmlel) of
                 true ->
        		    JID = proplists:get_value(<<"jid">>,Xml#xmlel.attrs,<<"">>),
                    case proplists:get_value(<<"status">>,Xml#xmlel.attrs) of
                    undefined ->
                        [{<<"jid">>,JID}];
                    Status ->
                        [{<<"jid">>,JID},
                         {<<"status">>,Status}]
                    end;
                _ ->
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"muc_invites">>)] end,M_invites),
        Bodys
    end.
	
%%----------------------------------------------
%% @date 2016-9
%% 对聊天室邀请用户进行v2处理
%% qunar.com
%%----------------------------------------------
encode_muc_invite_user_v2_pb(From,To,Packet) ->
	Bodys = get_pb_iq_invite_muc_user_bodys(Packet),
    ID = case  xml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(mod_time:timestamp());
    {Value,I} ->
            I
    end,    
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_invite_user_v2">>,ID,'undefined','undefined',[],Bodys).

%%----------------------------------------------
%% @date 2016-9
%% 用户注册信息
%% qunar.com
%%----------------------------------------------
encode_pb_muc_user_register(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"query">>) of
        false ->
            'undefined';
        Query ->
            [Xml] = Query#xmlel.children,
            case Xml#xmlel.name of
            <<"set_register">> ->
                ejabberd_xml2pb_public:encode_messagebody([],<<"set_register">>);
             _ ->
                ok
            end
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_set_register">>,ID,'undefined',Body,[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 取消用户注册信息
%% qunar.com
%%----------------------------------------------
encode_pb_muc_user_del_register(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"query">>) of
        false ->
            'undefined';
        Query ->
            ejabberd_xml2pb_public:encode_messagebody([],<<"del_register">>)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_del_register">>,ID,'undefined',Body,[],[]).
     
 
%%----------------------------------------------
%% @date 2016-9
%% 提升管理员/取消管理员/T人，回复是一样的
%% qunar.com
%%----------------------------------------------
encode_pb_muc_amdin(From,To,Packet) ->
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_admin">>,ID,'undefined','undefined',[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 设置添加好友配置项
%% qunar.com
%%----------------------------------------------

encode_pb_set_friend_opt(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"set_verify_friend_mode">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  xml:get_attr(<<"result">>,Query#xmlel.attrs) of
                false ->
                        [{<<"result">>,<<"false">>}];
                {value,Res} ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"set_verify_friend_mode">>)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"set_verify_friend_mode">>,ID,'undefined',Body,[],[]).

%%----------------------------------------------
%% @date 2016-9
%% 获取好友配置项
%% qunar.com
%%----------------------------------------------

encode_pb_get_friend_opt(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"get_verify_friend_mode">>) of
        false ->
            'undefined';
        Query ->
            Headers = make_friend_opt_headers(Query#xmlel.attrs), 
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"result">>)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_verify_friend_mode">>,ID,'undefined',Body,[],[]).

%%----------------------------------------------
%% @date 2016-9
%% 获取用户好友列表
%% qunar.com
%%----------------------------------------------

encode_pb_get_user_friends(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"get_user_friends">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  xml:get_attr(<<"friends">>,Query#xmlel.attrs) of
                false ->
                        [];
                {value,Res} ->
                        [{<<"friends">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"get_friends">>)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"user_get_friends">>,ID,'undefined',Body,[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 删除好友
%% qunar.com
%%----------------------------------------------

encode_pb_del_user_friend(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"delete_friend">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  xml:get_attr(<<"jid">>,Query#xmlel.attrs) of
                false ->
                        [];
                {value,Res} ->
                        case  xml:get_attr(<<"result">>,Query#xmlel.attrs) of
                        false ->
                                [];
                        {value,Result} ->
                                [{<<"jid">>,Res},
                                 {<<"result">>,Result}]
                        end
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"del_user_friend">>)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"user_del_friend">>,ID,'undefined',Body,[],[]).



%%----------------------------------------------
%% @date 2016-9
%% 获取time_key和http auth key
%% qunar.com
%%----------------------------------------------

encode_pb_time_http_key(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"key">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  xml:get_attr(<<"value">>,Query#xmlel.attrs) of
                false ->
                        [];
                {value,Res} ->
                    [{<<"time_key">>,integer_to_binary(mod_time:get_timestamp())},
                     {<<"key">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"get_key">>)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_key">>,ID,'undefined',Body,[],[]).



%%----------------------------------------------
%% @date 2016-9
%% 销毁群结果
%% qunar.com
%%----------------------------------------------

encode_pb_destroy_muc(From,To,Packet) ->
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    Body =  ejabberd_xml2pb_public:encode_messagebody([{<<"result">>,<<"success">>}],<<"desctroy_muc">>),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"desctroy_muc">>,ID,'undefined',Body,[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 心跳结果
%% qunar.com
%%----------------------------------------------

encode_pb_ping(From,To,Packet) ->
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"ping">>,ID,'undefined','undefined',[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 获取用户屏蔽列表
%% qunar.com
%%----------------------------------------------

encode_pb_get_mask_user(From,To,Packet) ->
    Mask_Users = xml:get_subtags(Packet,<<"get_mask_user">>), 
    Bodys =	lists:flatmap(fun(Xml) ->
               Headers = 
                case is_record(Xml,xmlel) of
                true ->
                    case proplists:get_value(<<"masked_user">>,Xml#xmlel.attrs) of
                    undefined ->
                        [];
                    JID ->
                        [{<<"masked_user">>,JID}]
                    end;
                _ ->
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"get_mask_user">>)] end,Mask_Users),

    ID = case  xml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(mod_time:timestamp());
    {Value,I} ->
            I
    end,    
	struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_mask_users">>,ID,'undefined','undefined',[],Bodys).
		


%%----------------------------------------------
%% @date 2016-9
%% 屏蔽好友
%% qunar.com
%%----------------------------------------------

encode_pb_set_mask_user(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"mask_user">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  xml:get_attr(<<"result">>,Query#xmlel.attrs) of
                false ->
                        [{<<"result">>,<<"failed">>}];
                {value,Res} ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"mask_user">>)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"set_mask_user">>,ID,'undefined',Body,[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 取消屏蔽好友
%% qunar.com
%%----------------------------------------------

encode_pb_cancel_mask_user(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"cancel_mask_user">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  xml:get_attr(<<"result">>,Query#xmlel.attrs) of
                false ->
                        [{<<"result">>,<<"failed">>}];
                {value,Res} ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"mask_user">>)
        end,
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"cancel_mask_user">>,ID,'undefined',Body,[],[]).


encode_pb_handle_user_subscribe(From,To,Packet) ->
    Body = 
        case xml:get_subtag(Packet,<<"query">>) of
        false ->
            'undefined';
        Query ->
            case handle_subscribe_xml(Query,<<"subscribe">>) of
            [] ->
                case handle_subscribe_xml(Query,<<"add_subscribe">>) of
                [] ->
                    case handle_subscribe_xml(Query,<<"delete_subscribe">>) of
                    [] ->
                        'undefined';
                    Del ->
                        ejabberd_xml2pb_public:encode_messagebody(Del,<<"delete_subscribe">>)
                    end;
                Add ->
                    ejabberd_xml2pb_public:encode_messagebody(Add,<<"add_subscribe">>)
                end;
            Sub ->
                ejabberd_xml2pb_public:encode_messagebody(Sub,<<"subscribe">>)
            end
        end,
            
    ID = ejabberd_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"subscribe">>,ID,'undefined',Body,[],[]).
            
        
handle_subscribe_xml(Packet,Key) ->
    case xml:get_subtag(Packet,Key) of
    false ->
        [];
    Sub ->
        case xml:get_attr(<<"status">>,Sub#xmlel.attrs) of
        {value,V} ->
            [{<<"status">>,V}];
        _ ->
            [{<<"status">>,<<"false">>}]
        end
    end.
        


make_friend_opt_headers(Attrs) ->
    JID = proplists:get_value(<<"jid">>,Attrs,<<"">>),
    Mode = proplists:get_value(<<"mode">>,Attrs,<<"">>),
    Question = proplists:get_value(<<"question">>,Attrs,<<"">>),
    Answer = proplists:get_value(<<"answer">>,Attrs,<<"">>),
    [{<<"jid">>,JID},
     {<<"mode">>,Mode},
     {<<"question">>,Question},
     {<<"answer">>,Answer}].
