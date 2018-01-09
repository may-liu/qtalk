-module(ejabberd_xml2pb_public).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-export([encode_messagebody/2,encode_pb_protomessage/5,encode_pb_protoheader/2,encode_pb_stringheaders/1]).
-export([set_type/1,set_client_type/1,set_msg_type/1,get_proto_header_opt/1]).
-export([set_presenceKey_type/1,set_iqKey_type/1]).


encode_pb_protoheader(Opt,Pro_msg) ->
        Msg =
            case Opt of
            1 ->
                zlib:gzip(Pro_msg);
	        5 ->
			    tea_crypto:encrypt(Pro_msg,<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>);
            _ ->
                Pro_msg
            end,
        ?DEBUG("encode_pb_protoheader size ~p ~n",[size(Pro_msg)]),
        ?DEBUG("encode_pb_protoheader size2 ~p ~n",[size(Msg)]),
        message_pb:encode_protoheader(#protoheader{options = Opt,message = Msg}).

encode_pb_protomessage(From,To,Type,Opts,Msg) ->
        Message = #protomessage{options = Opts,signaltype = message_pb:enum_to_int(signaltype,Type),from = From,to = To,message = Msg},
        message_pb:encode_protomessage(Message).

encode_pb_stringheader([]) ->
        [];
encode_pb_stringheader({_,<<"">>}) ->
        [];
encode_pb_stringheader({<<"">>,_}) ->
        [];
encode_pb_stringheader({K,V}) ->
        case set_header_definedkey(K) of
        'none' -> 
		    [#stringheader{key = K,value = V}];
        Dv ->
            [#stringheader{definedkey = Dv,value = V}]
        end.

encode_pb_stringheaders(Headers) when is_list(Headers) ->
        lists:flatmap(fun(Header) ->
            encode_pb_stringheader(Header) end,Headers);
encode_pb_stringheaders(Headers) ->
        [].

encode_messagebody(Headers,Value) ->
    Pb_headers = encode_pb_stringheaders(Headers), 
	#messagebody{headers = Pb_headers,value = Value}.
	
set_msg_type(Type) ->
	case Type of
	<<"-1">> ->
	    'MessageTypeRevoke';
	<<"1">> ->
	    'MessageTypeText';
	<<"2">> ->
    	'MessageTypeVoice';
	<<"3">> ->
    	'MessageTypePhoto';
    <<"4">> ->
    	'MessageTypeSogouIcon';
    <<"5">> ->
    	'MessageTypeFile';
	<<"6">>->
    	'MessageTypeTopic';
	<<"7">> ->
    	'MessageTypeRichText';
	<<"8">> ->
    	'MessageTypeActionRichText';
	<<"9">> ->
       	'MessageTypeReply';
    <<"10">> ->
    	'MessageTypeShock';
    <<"13">> ->
        'MessageTypeMarkdown';
	<<"15">> ->
	    'MessageTypeGroupNotify';
	<<"16">> ->
    	'MessageTypeLocalShare';
	<<"32">> ->
    	'MessageTypeSmallVideo';
	<<"64">> ->
    	'MessageTypeSourceCode';
	<<"101">> ->
    	'MessageTypeTime';
	<<"128">> ->
    	'MessageTypeBurnAfterRead';
	<<"256">> ->
    	'MessageTypeCardShare';
	<<"511">> ->
    	'MessageTypeActivity';
	<<"512">> ->
    	'MessageTypeRedPack';
    <<"513">> ->
    	'MessageTypeAA';
    <<"666">> ->
    	'MessageTypeCommonTrdInfo';
	<<"888">> ->
    	'MessageTypeCommonProductInfo';
	<<"1024">> ->
    	'MessageTypeRedPackInfo';
    <<"5001">> ->
        'WebRTC_MsgType_VideoMeeting';
	<<"4294967295">> ->
	    'MessageTypeRevoke';
    <<"65536">> ->
        'WebRTC_MsgType_Video';
    <<"131072">> ->
        'WebRTC_MsgType_Audio';
    _ ->
        'MessageTypeText'
    end.

set_client_type(Type) ->
	case Type of
	<<"ClientTypeMac">> ->
	    'ClientTypeMac';
	<<"ClientTypeiOS">> ->
    	'ClientTypeiOS';
    <<"ClientTypePC">> ->
    	'ClientTypePC';
	<<"ClientTypeAndroid">> ->
    	'ClientTypeAndroid';
	<<"ClientTypeLinux">> ->
    	'ClientTypeLinux';
    <<"ClientTypeWeb">> ->
    	'ClientTypeWeb';
	_ ->
    	'ClientTypePC'
	end.

set_type(Type) ->
    	case Type of 
	    <<"chat">> ->
            'SignalTypeChat';
	<<"groupchat">> ->
    	'SignalTypeGroupChat';
    <<"readmark">> ->
        'SignalTypeReadmark';
    <<"mstat">> ->
        'SignalTypeMState';
    <<"carbon">> ->
        'SignalTypeCarbon';
    <<"subscription">> ->
        'SignalTypeSubscription';
    <<"headline">> ->
        'SignalTypeHeadline';
    <<"revoke">> ->
        'SignalTypeRevoke';
    <<"webrtc">> ->
        'SignalTypeWebRtc';
	_ ->
        'SignalTypeChat'
	end.

set_header_definedkey(Key) ->
    case Key of
    <<"chatid">> ->
        'StringHeaderTypeChatId';
    <<"channelid">> ->
        'StringHeaderTypeChannelId';
    <<"extendInfo">> ->
        'StringHeaderTypeExtendInfo';
    <<"backupinfo">> ->
        'StringHeaderTypeBackupInfo';
    <<"read_type">> ->
        'StringHeaderTypeReadType';
    <<"jid">> ->
        'StringHeaderTypeJid';
    <<"real_jid">> ->
        'StringHeaderTypeRealJid';
    <<"invite_jid">> ->
        'StringHeaderTypeInviteJid';
    <<"del_jid">> ->
        'StringHeaderTypeDeleleJid';
    <<"nick">> ->
        'StringHeaderTypeNick';
    <<"title">> ->
        'StringHeaderTypeTitle';
    <<"pic">> ->
        'StringHeaderTypePic';
    <<"version">> ->
        'StringHeaderTypeVersion';
    <<"method">> ->
        'StringHeaderTypeMethod';
    <<"body">> ->
        'StringHeaderTypeBody';
    <<"affiliation">> ->
        'StringHeaderTypeAffiliation';
    <<"type">> ->
        'StringHeaderTypeType';
    <<"result">> ->
        'StringHeaderTypeResult';
    <<"reason">> ->
        'StringHeaderTypeReason';
    <<"role">> ->
        'StringHeaderTypeRole';
    <<"domain">> ->
        'StringHeaderTypeDomain';
    <<"status">> ->
        'StringHeaderTypeStatus';
    <<"code">> ->
        'StringHeaderTypeCode';
    <<"cdata">> ->
        'StringHeaderTypeCdata';
    <<"time_value">> ->
        'StringHeaderTypeTimeValue';
    <<"key_value">> ->
        'StringHeaderTypeKeyValue';
    <<"name">> ->
        'StringHeaderTypeName';
    <<"host">> ->
        'StringHeaderTypeHost';
    <<"question">> ->
        'StringHeaderTypeQuestion';
    <<"answer">> ->
        'StringHeaderTypeAnswer';
    <<"friends">> ->
        'StringHeaderTypeFriends';
    <<"value">> ->
        'StringHeaderTypeValue';
    <<"masked_user">> ->
        'StringHeaderTypeMaskedUuser';
    <<"key">> ->
        'StringHeaderTypeKey';
    <<"mode">> ->
        'StringHeaderTypeMode';
    <<"carbon_message">> ->
        'StringHeaderTypeCarbon';
    _ ->
        'none'
    end.
    
get_proto_header_opt(Pro_msg) ->
    if size(Pro_msg) > 200 ->
        1;
    true ->
        5
    end.

        
set_iqKey_type(Key) ->
    case Key of
    <<"result">> ->
        'IQKeyResult';
    <<"error">> ->
        'IQKeyError';
    _ ->
        'none'
    end.

set_presenceKey_type(Key) ->
    case Key of
    <<"result">> ->
        'PresenceKeyResult';
    _ ->
        'none'
    end.
