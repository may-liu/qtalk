%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_odbc).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, set_password/3, check_password/3,check_wlan_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0,
	 plain_password_required/0]).

-export([get_login_url_by_keyword/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) -> ok.

plain_password_required() -> false.

store_type() -> plain.

%% @spec (User, Server, Password) -> true | false | {error, Error}
check_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
        error -> false;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            do_check_password(LServer, Username, Password, check_frozen_flag(Username))
%%			true
    end.

check_frozen_flag(Username) ->
    case ets:lookup(blacklist, Username) of
        [] ->
            true;
        _ ->
            false
    end.

check_white_list(Username) ->
%% white list  allow user to login in use pgsql
	case ets:lookup(whitelist,Username)  of
        [] ->
            false;
        % 只允许单点登录
        [{Username, <<"1">>}|_] ->
            {true, single};
        % 允许多个设备同时登录
        _ ->
            {true, no_limit}
	end.

do_check_password(_, _, _, false) ->
    {false, <<"frozen-in">>};
do_check_password(LServer, Username, Password, _) ->
	check_user_password(LServer, Username, Password).

check_user_password(LServer, Username, Password) ->
    try check_white_list(Username) of
        {true, SingleFlag} ->
			case catch do_check_password1(LServer,Username,Password) of
			true ->
                check_user_resources(LServer, Username, SingleFlag),
				true;
			_ ->
				case catch check_password_use_http(password,LServer,get_login_url_by_keyword('password'),Username,Password)  of
				true ->
					true;
				_ ->
					{false,<<"cancel-rsa">>}
				end
			end;
		false ->
			case catch check_password_use_http(password,LServer,get_login_url_by_keyword('password'),Username,Password)  of
			true ->
				true;
			_ ->
				false
			end
    catch
        _:_ ->
            false %% Typical error is database not accessible
    end.

check_user_resources(_, _, no_limit) ->
    ok;
check_user_resources(LServer, Username, single) ->
    case ejabberd_sm:get_user_resources(Username, LServer) of
        [] ->
            ok;
        Resources ->
            lists:foreach(
              fun(Resource) ->
                      PID = ejabberd_sm:get_session_pid(Username, LServer, Resource),
                      PID ! kick
              end, Resources)
    end.
do_check_password1(LServer, Username, Password) ->
	?DEBUG("User ~p ,Password ~p ~n",[Username, Password]),
    try odbc_queries:get_password(LServer, Username) of
        {selected, [<<"password">>], [[Password]]} ->
            Password /= <<"">>;
        {selected, [<<"password">>], [[_Password2]]} ->
            %%	false; %% Password is not correct
			false;
        {selected, [<<"password">>], []} ->
            false; %% Account does not exist
        {error, _Error} ->
            false %% Typical error is that table doesn't exist
    catch
        _:_ ->
            false %% Typical error is database not accessible
    end.
%%--------------------------------------------------------
%% Port 5223 password format: Key@Passowrd
%% Key: machine key,use to mark single machine
%% Passoword: identifying code 
%% -------------------------------------------------------
check_wlan_password(User, Server, Pass) ->
    case str:str(Pass,<<"@">>) of
    0 ->
		case catch check_white_list(User) of
		 {true, SingleFlag} ->
		 	case catch do_check_password1(Server,User,Pass) of
			true ->
				 check_user_resources(Server, User, SingleFlag),
				true;
			_ ->
				false
			end;
		_ ->
			false
		end;
	L ->
        Key = str:substr(Pass,1,L-1),
        Password = str:substr(Pass,L+1,size(Pass)-1),
        case jlib:nodeprep(User) of
        error -> false;
        LUser ->
        	Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            NewKey = lists:concat([binary_to_list(Username),"_",binary_to_list(Key)]),
			case catch ets:lookup(sn_user,str:to_upper(Username)) of
			[] ->
            	do_check_wlan_password(LServer, Username, NewKey, Password,Key,check_frozen_flag(Username));
			 _ ->
				{false,<<"regular_staffs not alow sn check_in">>}
		    end
        end
   end.

%%---------------------------------------------------------
%%检查用户token，黑名单中禁止登陆，检查顺序：1.redis 2.http
%%---------------------------------------------------------
do_check_wlan_password(_, _, _, _, _,false) ->
    {false, <<"frozen-in">>};
do_check_wlan_password(LServer, Username, NewKey, Password,_, _) ->
    case redis_link:str_get(LServer,0,NewKey) of
    {ok,undefined}  ->
		check_http_redis_token(LServer,Username,Password,NewKey,false);
    {ok,V} ->
		case V =:= Password of 
        true ->
            true;
       	false ->
			check_http_redis_token(LServer,Username,Password,NewKey,true)	
        end;
     _ ->
        false
     end.

%%----------------------------------------------
%%http验证token成功，使用改token作为密码,实效7天
%%----------------------------------------------
check_http_redis_token(LServer,Username,Password,NewKey,Redis_Flag)  ->
	case check_password_use_http(token,LServer,get_login_url_by_keyword('token'),Username,Password) of
    true ->
       	case redis_link:str_setex(LServer,0,NewKey,86400*7,Password) of
		{ok,<<"OK">>} ->
			true;
         _ ->
		 	?INFO_MSG("Redis set Username ~p  key ~p failed !",[Username,NewKey]),
            true
         end;
     false ->
	 		case Redis_Flag of 
			true ->
				false;
			_ ->
				{false,<<"out_of_date">>}
			end
     end.
%% @spec (User, Server, Password, Digest, DigestGen) -> true | false | {error, Error}
check_password(User, Server, Password, Digest,
	       DigestGen) -> 
    case jlib:nodeprep(User) of
     error -> false;
    LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
	  try odbc_queries:get_password(LServer, Username) of
	    %% Account exists, check if password is valid
	    {selected, [<<"password">>], [[Passwd]]} ->
		DigRes = if Digest /= <<"">> ->
				Digest == DigestGen(Passwd);
			    true -> false
			 end,
		if DigRes -> true;
		   true -> (Passwd == Password) and (Password /= <<"">>)
		end;
	    {selected, [<<"password">>], []} ->
		false; %% Account does not exist
	    {error, _Error} ->
		false %% Typical error is that table doesn't exist
	  catch
	    _:_ ->
		false %% Typical error is database not accessible
	  end
   end.


%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
      error -> {error, invalid_jid};
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  Pass = ejabberd_odbc:escape(Password),
	  LServer = jlib:nameprep(Server),
	  case catch odbc_queries:set_password_t(LServer,
						 Username, Pass)
	      of
	    {atomic, ok} -> ok;
	    Other -> {error, Other}
	  end
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid}
try_register(User, Server, Password) ->
    case jlib:nodeprep(User) of
      error -> {error, invalid_jid};
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  Pass = ejabberd_odbc:escape(Password),
	  LServer = jlib:nameprep(Server),
	  case catch odbc_queries:add_user(LServer, Username,
					   Pass)
	      of
	    {updated, 1} -> {atomic, ok};
	    _ -> {atomic, exists}
	  end
    end.

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(odbc),
    lists:flatmap(fun (Server) ->
			  get_vh_registered_users(Server)
		  end,
		  Servers).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:list_users(LServer) of
      {selected, [<<"username">>], Res} ->
	  [{U, LServer} || [U] <- Res];
      _ -> []
    end.

get_vh_registered_users(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:list_users(LServer, Opts) of
      {selected, [<<"username">>], Res} ->
	  [{U, LServer} || [U] <- Res];
      _ -> []
    end.

get_vh_registered_users_number(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer) of
      {selected, [_], [[Res]]} ->
	  jlib:binary_to_integer(Res);
      _ -> 0
    end.

get_vh_registered_users_number(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer, Opts) of
      {selected, [_], [[Res]]} ->
	  jlib:binary_to_integer(Res);
      _Other -> 0
    end.

get_password(User, Server) ->
    case jlib:nodeprep(User) of
      error -> false;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
	  case catch odbc_queries:get_password(LServer, Username)
	      of
	    {selected, [<<"password">>], [[Password]]} -> Password;
	    _ -> false
	  end
    end.

get_password_s(User, Server) ->
    case jlib:nodeprep(User) of
      error -> <<"">>;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
	  case catch odbc_queries:get_password(LServer, Username)
	      of
	    {selected, [<<"password">>], [[Password]]} -> Password;
	    _ -> <<"">>
	  end
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    case jlib:nodeprep(User) of
    error -> false;
    LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
	  case catch ets:lookup(user_list,Username) of
	  [] ->
	  	try odbc_queries:get_password(LServer, Username) of
	    	{selected, [<<"password">>], [[_Password]]} ->
				true; %% Account exists
		    {selected, [<<"password">>], []} ->
				false; %% Account does not exist
			{error, Error} -> {error, Error}
	  	catch
	  	  _:B -> {error, B}
	  	end;
	   _ ->
	   	false
	   end
    end.

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(User, Server) ->
    case jlib:nodeprep(User) of
      error -> error;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
	  catch odbc_queries:del_user(LServer, Username),
	  ok
    end.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    case jlib:nodeprep(User) of
      error -> error;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  Pass = ejabberd_odbc:escape(Password),
	  LServer = jlib:nameprep(Server),
	  F = fun () ->
		      Result = odbc_queries:del_user_return_password(LServer,
								     Username,
								     Pass),
		      case Result of
			{selected, [<<"password">>], [[Password]]} -> ok;
			{selected, [<<"password">>], []} -> not_exists;
			_ -> not_allowed
		      end
	      end,
	  {atomic, Result} = odbc_queries:sql_transaction(LServer,
							  F),
	  Result
    end.

%%-------------------------------------------
%%预设的3种验密
%%-------------------------------------------
check_password_use_http(password,Server,URL,User,Password) ->
	?INFO_MSG("URL ~p ~n",[URL]),
    Data = binary_to_list(Password),
	Random_Num1 = random:uniform(99999),
	Random_Num2 = random:uniform(99999),
    Boundary = lists:concat(["------------!",integer_to_list(Random_Num1),"cb",integer_to_list(Random_Num2),"!"]),
    Type = lists:concat(["multipart/form-data; boundary=", Boundary]),
    HTTPOptions = [{timeout,3000}],
    Options = [],
    UserName  = binary_to_list(User),
    Bodys = format_multipart_formdata(Boundary, [{a,"testapp"},{uid, UserName}], [{p, Data,[]}]),
    Headers = [{"Content-Length", integer_to_list(length(Bodys))}], 
	http_validate_password(Server,URL,Headers,Type,Bodys,HTTPOptions,Options,User,Password);
check_password_use_http(token,Server,URL,User,Password) ->
    Header = [],
    Type = "application/x-www-form-urlencoded",
    Body = lists:concat(["token=",binary_to_list(Password)]),
    HTTPOptions = [],
    Options = [],	
	http_validate_password(Server,URL,Header,Type,Body,HTTPOptions,Options,User,Password);
check_password_use_http(token1,Server,URL,User,Password) ->
    Header = [],
    Type = "application/x-www-form-urlencoded",
    Body = lists:concat(["token=",binary_to_list(Password)]),
    HTTPOptions = [],
    Options = [],	
	http_validate_password(Server,URL,Header,Type,Body,HTTPOptions,Options,User,Password);
check_password_use_http(_,_,_,_,_) ->
	false.

%%----------------------------------------------------------------------------------
%%通过http接口验证登陆密码,密码经过rsa加密，关键字中含有时间，不存在服务端泄密的问题
%%----------------------------------------------------------------------------------
http_validate_password(Server,Url,Headers,Type,Bodys,HTTPOptions,Options,UserName,Password) ->
	case catch http_client:http_post(Server,Url,Headers,Type,Bodys,HTTPOptions,Options) of
	{ok, {_Status,_Headers, Data}} -> 
		case rfc4627:decode(Data) of
    	{ok,{obj,Val},_} -> 
   			case proplists:get_value("status_id", Val) of
    		0 ->
        		true;
			101 ->
				?INFO_MSG("Check auth User : [~p] Passowrd: [~p] error.~n",[UserName,Password]),
				false;
    		_ ->
				?INFO_MSG("Check auth User : [~p] Passowrd: [~p] unknown error.~n",[UserName,Password]),
				false
	    	end;
		{error,Reason} ->
			?INFO_MSG("Check auth  User : [~p] , rfc4627 decode [~p] error,Reason [~p]. ~n",[UserName,Data,Reason]),
			false;
		_ ->
			?INFO_MSG("Check auth  User : [~p] , rfc4627 decode [~p] unknown error. ~n",[UserName,Data]),
			false
		end;
	{error,Reason1} ->
		?INFO_MSG("Check auth User : [~p] , http_validate_password post error,Reason [~p]. ~n",[UserName,Reason1]),
		false;
	_ ->
		?INFO_MSG("Check auth User : [~p] , http_validate_password post unknown error. ~n",[UserName]),
		false
	end.

format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = lists:map(fun({FieldName, FieldContent}) ->
                               [lists:concat(["--", Boundary]),
                                 lists:concat(["Content-Disposition: form-data; name=\"",atom_to_list(FieldName),"\""]),
                                  "",     FieldContent]
                                   end, Fields),
    FieldParts2 = lists:append(FieldParts),

    FileParts = lists:map(fun({FieldName, FileName, _FileContent}) ->
       [lists:concat(["--", Boundary]),
        lists:concat(["Content-Disposition: format-data; name=\"",atom_to_list(FieldName),"\""]),"",FileName]
        end, Files),
    FileParts2 = lists:append(FileParts),

    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
    string:join(Parts, "\r\n").

%%-------------------------------------------
%%通过关键字获取登陆url
%%-------------------------------------------
get_login_url_by_keyword(Keyword) ->
	case catch ejabberd_config:get_option(http_login_url, fun(V) -> V end) of
	Url when is_list(Url) ->
		binary_to_list(proplists:get_value(Keyword,Url));
	_ ->
		[]
	end.
