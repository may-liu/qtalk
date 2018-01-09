-module(ejabberd_xml2pb_message).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([xml2pb_msg/3]).

encode_pb_xmpp_msg(Msg_Type,Client_Type,Read_Type,Client_version,Msg_ID,Channel_ID,Ex_INFO,Backup_info,Carbon,Message,ID,Time) ->
	Msg_Body = 
		#messagebody{headers = 
			  	ejabberd_xml2pb_public:encode_pb_stringheaders(
                    [{<<"chatid">>,Msg_ID},
                     {<<"channelid">>,Channel_ID},
                     {<<"extendInfo">>,Ex_INFO},
                     {<<"backupinfo">>,Backup_info},
                     {<<"read_type">>,Read_Type},
                     {<<"carbon_message">>,Carbon}]),
			     value  = Message},
	
	Xmpp = #xmppmessage{
		messagetype = Msg_Type,
  		clienttype = Client_Type,
  		clientversion = Client_version,
  		messageid = ID,
   		body = Msg_Body,
   		receivedtime = Time},
	message_pb:encode_xmppmessage(Xmpp).

struct_pb_xmpp_msg(From,To,Type,Msg_Type,Client_Type,Read_Type,Client_version,Msg_ID,Channel_ID,Ex_INFO,Backup_info,Carbon,Message,ID,Time) ->
        ?DEBUG("struct_pb_xmpp_msg 1111 ~p,Message ~p ~n",[Type,Message]),
        Msg = list_to_binary(
                encode_pb_xmpp_msg(message_pb:enum_to_int(messagetype,Msg_Type), message_pb:enum_to_int(clienttype,Client_Type),
                    Read_Type,Client_version,Msg_ID,Channel_ID,Ex_INFO,Backup_info,Carbon,Message,ID,Time)),
        ?DEBUG("struct_pb_xmpp_msg Msg ~p ~n",[Msg]),
        Pb_Msg = list_to_binary(ejabberd_xml2pb_public:encode_pb_protomessage(From,To,Type,0,Msg)),
        Opt = ejabberd_xml2pb_public:get_proto_header_opt(Pb_Msg),
        list_to_binary(ejabberd_xml2pb_public:encode_pb_protoheader(Opt,Pb_Msg)).
	
	
xml2pb_msg(From,To,Packet) ->
    case xml:get_attr(<<"type">>,Packet#xmlel.attrs) of
    false ->
        <<"">>;
    {Value,Type} ->
        Msg_ID = proplists:get_value(<<"id">>,Packet#xmlel.attrs,<<"1">>),
        Client_Type = proplists:get_value(<<"client_type">>,Packet#xmlel.attrs,<<"">>),
        Client_Ver = proplists:get_value(<<"client_ver">>,Packet#xmlel.attrs,<<"0">>),
        Carbon_Flag = proplists:get_value(<<"carbon_message">>,Packet#xmlel.attrs,<<"">>),
        Read_type = proplists:get_value(<<"read_type">>,Packet#xmlel.attrs,<<"">>),
        
        case xml:get_subtag(Packet,<<"body">>) of
        false ->
            case Type of 
            <<"stat">> ->
                struct_pb_xmpp_msg(From,To,ejabberd_xml2pb_public:set_type(Type),ejabberd_xml2pb_public:set_msg_type(<<"1">>),
                           ejabberd_xml2pb_public:set_client_type(<<"">>),<<"">>,
                               binary_to_integer(Client_Ver),Msg_ID,<<"">>,<<"">>,<<"">>, <<"">>,<<"">>,<<"">>,
                                mod_time:get_exact_timestamp());
            _ ->
                <<"">>
            end;
        Body -> 
            Msg_Type = proplists:get_value(<<"msgType">>,Body#xmlel.attrs,<<"1">>),
            Channel_ID = proplists:get_value(<<"channelid">>,Body#xmlel.attrs,<<"">>),
            Ex_INFO = proplists:get_value(<<"extendInfo">>,Body#xmlel.attrs,<<"">>),
            Backup_info = proplists:get_value(<<"backupinfo">>,Body#xmlel.attrs,<<"">>),
            Time = proplists:get_value(<<"msec_times">>,Body#xmlel.attrs,mod_time:get_exact_timestamp()),
            

            ID = case proplists:get_value(<<"id">>,Body#xmlel.attrs) of
                I when is_binary(I) ->
                    I;
                _ ->
                    list_to_binary("http_" ++ integer_to_list(random:uniform(65536)) ++ integer_to_list(mod_time:get_exact_timestamp())) 
                end,
            ?DEBUG("from ~p,To ~p ,Body ~p  ~n",[From,To,Packet]),
            Message = xml:get_subtag_cdata(Packet,<<"body">>),
            ?DEBUG("message ~p,Type ~p ~n",[Message,Msg_Type]),
            struct_pb_xmpp_msg(From,To,ejabberd_xml2pb_public:set_type(Type),ejabberd_xml2pb_public:set_msg_type(Msg_Type),
                                ejabberd_xml2pb_public:set_client_type(Client_Type),Read_type,
                                binary_to_integer(Client_Ver),Msg_ID,Channel_ID,Ex_INFO,Backup_info,Carbon_Flag, Message,ID,Time)
        end
    end.

