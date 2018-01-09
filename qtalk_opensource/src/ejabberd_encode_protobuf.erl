-module(ejabberd_encode_protobuf).

-include("message_pb.hrl").
-include("jlib.hrl").


-export([start/1,encode_pb_message/6,struct_pb_message/9,uint32_pack/2]).
-export([struct_pb_welcome/6,struct_pb_streamend/2,struct_pb_auth_msg/3]).
-export([struct_pb_response_suc/6,struct_pb_response_err/6,struct_pb_iq_msg/6]).
-export([make_iq_xmpp/1,encode_pb_protomessage/5,encode_pb_protoheader/2]).
-export([encode_pb_proceedtls/0,struct_pb_startTLS/2]).


start(A) ->
	ok.

encode_pb_message(Msg_Type,Client_Type,Client_version,Msg_ID,Message,ID) ->
	Header = #stringheader{params = [],key = "Id",value = ID},
	Body = #messagebody{headers = [],value = Message},
	#xmppmessage{messagetype = Msg_Type, clienttype = Client_Type,  clientversion = Client_version,messageid = Msg_ID,
	   					header = Header,   body = Body,   
						receivedtime = mod_time:get_timestamp(),   transfertime = 0}.

encode_pb_protomessage(From,To,Type,Opts,Msg) ->
	Message = #protomessage{signaltype = Type,from = From,to = To,message = Msg},
	list_to_binary(message_pb:encode_protomessage(Message)).

encode_pb_welcome(Host,Version,User,SockMod) ->
	Welcome = #welcomemessage{domain = Host,version = Version,user = User,sockmod = SockMod},
	list_to_binary(message_pb:encode_welcomemessage(Welcome)).

encode_pb_streamend() ->
	list_to_binary(message_pb:encode_streamend(#streamend{})).

encode_pb_proceedtls() ->
    list_to_binary(message_pb:encode_proceedtls(#proceedtls{})).

encode_pb_authmsg(Res) ->
	Auth = #authmessage{authkey = Res},
	list_to_binary(message_pb:encode_authmessage(Auth)).

encode_pb_iqmsg(Key,Value,Msg_ID,Body) ->
	IQ = #iqmessage{key = Key,value = Value,messageid = Msg_ID,body = #messagebody{value = Body}},
	list_to_binary(message_pb:encode_iqmessage(IQ)).

encode_pb_response_suc(Code,Msg_ID,Info,Body) ->
	Res = #responsesucceeded{code = Code,msgid = Msg_ID,info = Info,body = #messagebody{value = Body}},
	list_to_binary(message_pb:encode_responsesucceeded(Res)).

encode_pb_response_err(Code,Msg_ID,Info,Body) ->
	Res = #responsefailure{code = Code,error = Info,body = #messagebody{value = Body}},
	list_to_binary(message_pb:encode_responsefailure(Res)).

encode_pb_protoheader(_Opt,Pro_msg) ->
    Opt = if size(Pro_msg) > 200 ->
                1;
          true ->
                5
          end,
	Msg = 
		case Opt of 
		1 ->
			zlib:gzip(Pro_msg);
		_ ->
            tea_crypto:encrypt(Pro_msg,<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>)
		end,
    
	message_pb:encode_protoheader(#protoheader{options = Opt,message = Msg}).

struct_pb_message(From,To,Msg_Type,Client_Type,Client_version,Msg_ID,Message,Type,ID) ->
	Msg = encode_pb_message(Msg_Type,Client_Type,Client_version,Msg_ID,Message,ID),
	Pb_Msg = encode_pb_protomessage(From,To,Type,0,Msg),
	list_to_binary(encode_pb_protoheader(1,Pb_Msg)).

struct_pb_welcome(From,To,Host,Version,User,SockMod) ->
	Welcome_Msg = encode_pb_welcome(Host,Version,User,SockMod),
	Pb_Msg =  encode_pb_protomessage(From,To,message_pb:enum_to_int(signaltype,'SignalTypeWelcome'),0,Welcome_Msg),
	list_to_binary(encode_pb_protoheader(0,Pb_Msg)).

struct_pb_startTLS(From,To) ->
	StartTLS_Msg = encode_pb_proceedtls(),
	Pb_Msg =  encode_pb_protomessage(From,To,message_pb:enum_to_int(signaltype,'SignalProceedTLS'),0,StartTLS_Msg),
	list_to_binary(encode_pb_protoheader(0,Pb_Msg)).

struct_pb_streamend(From,To) ->
	Streamend = encode_pb_streamend(),
	End_Msg = encode_pb_protomessage(From,To,message_pb:enum_to_int(signaltype,'SignalTypeStreamEnd'),0,Streamend),
	list_to_binary(encode_pb_protoheader(0,End_Msg)).

struct_pb_auth_msg(From,To,Res) ->
	Auth_Msg = encode_pb_authmsg(Res),
	Pro_Msg = encode_pb_protomessage(From,To,message_pb:enum_to_int(signaltype,'SignalTypeAuth'),0,Auth_Msg),
	list_to_binary(encode_pb_protoheader(0,Pro_Msg)).

struct_pb_iq_msg(From,To,Key,Value,Msg_ID,Body) ->
	IQ = encode_pb_iqmsg(Key,Value,Msg_ID,Body),
	Pro_Msg = encode_pb_protomessage(From,To,message_pb:enum_to_int(signaltype,'SignalTypeIQ'),0,IQ),
	list_to_binary(encode_pb_protoheader(0,Pro_Msg)).

struct_pb_response_suc(From,To,Code,Msg_ID,Info,Body) ->
	Res = encode_pb_response_suc(Code,Msg_ID,Info,Body),
	Pro_Msg = encode_pb_protomessage(From,To,message_pb:enum_to_int(signaltype,'SignalTypeSucceededResponse'),0,Res),
	list_to_binary(encode_pb_protoheader(0,Pro_Msg)).

struct_pb_response_err(From,To,Code,Msg_ID,Info,Body) ->
	Res = encode_pb_response_err(Code,Msg_ID,Info,Body),
	Pro_Msg = encode_pb_protomessage(From,To,message_pb:enum_to_int(signaltype,'SignalTypeFailureResponse'),0,Res),
	list_to_binary(encode_pb_protoheader(0,Pro_Msg)).
	
%uint32_pack(Size) when Size < 127 ->
%	Size bor 16#80;
%uint32_pack(Size) when Size > 128 andalso Size < 128->
uint32_pack(Size,Data) ->
	<<D/bitstring>> = Data,
	if Size > 127 ->
		T1 = Size bor  16#80,
		S1 = Size bsr 7,
		if S1 > 127 ->
			T2 = S1 bor  16#80,
			S2  = S1 bsr 7,
			if S2 > 127 ->
				T3 = S2 bor 16#80,
				L3 = <<T1,T2,T3>>,
				S3  = S2 bsr 7,
				if  S3 > 127 ->
					T4 = S3 bor 16#80,
					S4  = S3 bsr 7,
					<<T1,T2,T3,T4,S4,D/bitstring>>;
				true ->
					<<T1,T2,T3,S3,D/bitstring>>
				end;
			true ->
				<<T1,T2,S2,D/bitstring>>
			end;
		true ->
			<<T1,S1,D/bitstring>>
		end;
	true ->
		<<Size,D/bitstring>>
	end.
		
				
make_iq_xmpp(IQ) when is_record(IQ,iqmessage) ->
	Key = IQ#iqmessage.key,
	do_make_iq_xmpp(Key,IQ);
make_iq_xmpp(_IQ)->
	error.

do_make_iq_xmpp("BIND",IQ) ->
	#xmlel{name = <<"iq">>, 
			attrs = [{<<"id">>,list_to_binary(IQ#iqmessage.messageid)},{<<"type">>,<<"set">>}], 
			children = [#xmlel{name = <<"bind">>,
						   attrs = [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-bind">>}],
							   children = [#xmlel{name = <<"resource">>, 
						   				  attrs = [],children = []}]}]};
do_make_iq_xmpp(_,IQ) ->
	error.

make_presence_xmpp(From,To,Presence) when is_record(Presence,presencemessage) ->
	Key = Presence#presencemessage.key,
	do_make_presence_xmpp(Key,From,To,Presence);
make_presence_xmpp(From,To,Presence) ->
	error.

do_make_presence_xmpp("MUC",From,To,Presence) ->
	#xmlel{name = <<"presence">>,
		   attrs = [{<<"to">>,To},{<<"priority">>,<<"5">>},{<<"version">>,<<"2">>}],
		   children = [
				#xmlel{name = <<"x">>,
					   attrs =[{<<"xmlns">>,?NS_MUC}],
					   	children = []}]};
do_make_presence_xmpp(Key,From,To,Presence) ->
	ok.
