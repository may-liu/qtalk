-module(mod_extend_iq).

-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(SUPERVISOR, ejabberd_sup).

-define(DEFAULT_SEND_PINGS, false).

-define(DEFAULT_PING_INTERVAL, 60).

-define(DICT, dict).

%% gen_mod callbacks
-export([start/2, stop/1]).


%% Hook callbacks
-export([get_key/3]).


%%====================================================================
stop(Host) ->
 %   Proc = gen_mod:get_module_proc(Host, ?MODULE),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_KEY),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_KEY).
  %  supervisor:terminate_child(ejabberd_sup, Proc),
   % supervisor:delete_child(ejabberd_sup, Proc).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,one_queue),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_KEY, ?MODULE, get_key, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_KEY, ?MODULE, get_key, IQDisc).


get_key(From, _To,
	#iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
      {get, #xmlel{name = <<"key">>}} ->
	  IQ#iq{type = result, sub_el = [make_iq_key_reply(From)]};
      _ ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

make_iq_key_reply(From) ->
	Resource = From#jid.resource,
	User = From#jid.user,
	LServer = jlib:nameprep(From#jid.server),
	V =
		case redis_link:hash_get(LServer,1,binary_to_list(User),binary_to_list(Resource)) of
		{ok,undefined} ->
			<<"">>;
		{ok,Key} ->
			Key;
		_ ->
			<<"">>
		end,
	#xmlel{name = <<"key">>,
			attrs = [{<<"xmlns">>,?NS_KEY},{<<"value">>,V}],children = []}.

