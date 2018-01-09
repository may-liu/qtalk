-module(make_muc_pic).

-export([make_muc_pic/2]).

-include("logger.hrl").
-include("qunar_ejabberd_extend.hrl").
-export([send_update_muc_pic/3]).

make_muc_pic(Server,Muc) ->
	case catch ets:lookup(muc_users,Muc) of
	[] ->
		ok;
	[{Muc,L}] ->
		case http_make_new_pic(Server,Muc,L) of
		null ->
			ok;
		Muc_Pic  ->
			send_update_muc_pic(Server,Muc,Muc_Pic)
		end;	
	_ ->
		ok
	end.

http_make_new_pic(Server,Muc,Users) ->
	Num = length(Users),
	Pics = 
		lists:flatmap(fun({User,Host}) ->
			case Host   =:=  Server of
			true ->
				case catch ets:lookup(vcard_version,User) of
				[] ->
					[];
				[Vv] when is_record(Vv,vcard_version)  ->
					[complete_url(Vv#vcard_version.url)];
				_ ->
					[]
				end;
			_ ->
				[]
			end	end,Users),
	%Key = str:concat(Muc,str:concat(<<"_">>,integer_to_binary(Num))),
	Key = Muc,
	Url = "http://xxxxxxx.com/file",
	Header = [],
	Type = "application/json",
    HTTPOptions = [],
    Options = [],
    Body =  rfc4627:encode({obj,[{"urls",Pics},{"key",Key}]}),
    case catch http_client:http_post(Server,Url,Header,Type,Body,HTTPOptions,Options) of
    {ok, {_Status,_Headers, Res}} ->
    	case rfc4627:decode(Res) of
        {ok,{obj,Args},_} ->
            case  proplists:get_value("data",Args) of
            Data when is_binary(Data) ->
                %% proplists:get_value(binary_to_list(Http_Url),Urls);
			    case str:str(Data,<<"qunar.com/">>)  of
			    0 ->
			    	<<"file/xxxx.png">>;
			    P ->
			    	str:substr(Data,P + 10,size(Data))
			    end;
            _ ->
	    	<<"file/xxxx.png">>
            end;
         _ ->
	    	<<"file/xxxx.png">>
         end;
    _ ->
    	<<"file/xxxx.png">>
    end.

complete_url(Url) ->
    case catch str:left(Url,4) of
    <<"http">> ->
        Url;
	Url1 when is_binary(Url) ->
        str:concat(<<"http://xxxxx.com/">>,Url);
	_ ->
		[]
	end.

send_update_muc_pic(Server,Muc,Pic) ->
	Full_Muc = str:concat(Muc,str:concat(<<"@conference.">>,Server)),
	Body = rfc4627:encode([{obj,[{<<"muc_name">>,Full_Muc},{<<"pic">>,Pic}]}]),
	Url = "http://xxxxxxxx.com/setmucvcard",
	Header = [],
	Type = "application/json",
    HTTPOptions = [],
    Options = [],
    case catch http_client:http_post(Server,Url,Header,Type,Body,HTTPOptions,Options) of
    {ok, {Status,Headers, Res}} ->
    	case rfc4627:decode(Res) of
        {ok,{obj,Args},_} ->
			?DEBUG("Args ~p ~n",[Args]),
			true;
         E ->
		 	false
         end;
    Error ->
		false
    end.

