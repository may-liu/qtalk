%%%----------------------------------------------------------------------
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

-define(NS_DISCO_ITEMS,
	<<"http://jabber.org/protocol/disco#items">>).
-define(NS_DISCO_INFO,
	<<"http://jabber.org/protocol/disco#info">>).
-define(NS_VCARD, <<"vcard-temp">>).
-define(NS_VCARD_UPDATE, <<"vcard-temp:x:update">>).
-define(NS_AUTH, <<"jabber:iq:auth">>).
-define(NS_AUTH_ERROR, <<"jabber:iq:auth:error">>).
-define(NS_REGISTER, <<"jabber:iq:register">>).
-define(NS_SEARCH, <<"jabber:iq:search">>).
-define(NS_ROSTER, <<"jabber:iq:roster">>).
-define(NS_VER_FRI_MODE, <<"jabber:iq:verify_friend_mode">>).
-define(NS_VER_FRI, <<"jabber:x:verify_friend">>).
-define(NS_DEL_FRI, <<"jabber:x:delete_friend">>).
-define(NS_GET_FRI, <<"jabber:x:get_friend">>).
-define(NS_DEL_INVITE, <<"jabber:x:delete_invite">>).
-define(NS_RECV_LIMIT, <<"jabber:x:recv_msg_limit">>).
-define(NS_MASK_USER, <<"jabber:x:mask_user">>).
-define(NS_VIRTUAL_USER, <<"jabber:x:virtual_user">>).
-define(NS_UPDATE_PRES_A, <<"jabber:x:update_pres_a">>).
-define(NS_VER_MAN_AUTH, <<"jabber:x:manual_authentication">>).
-define(NS_ROSTER_VER,
	<<"urn:xmpp:features:rosterver">>).
-define(NS_PRIVACY, <<"jabber:iq:privacy">>).
-define(NS_BLOCKING, <<"urn:xmpp:blocking">>).
-define(NS_PRIVATE, <<"jabber:iq:private">>).
-define(NS_VERSION, <<"jabber:iq:version">>).
-define(NS_TIME90, <<"jabber:iq:time">>).
-define(NS_TIME, <<"urn:xmpp:time">>).
-define(NS_LAST, <<"jabber:iq:last">>).
-define(NS_XDATA, <<"jabber:x:data">>).
-define(NS_IQDATA, <<"jabber:iq:data">>).
-define(NS_DELAY91, <<"jabber:x:delay">>).
-define(NS_TIME91, <<"jabber:stime:delay">>).
-define(NS_DELAY, <<"urn:xmpp:delay">>).
-define(NS_HINTS, <<"urn:xmpp:hints">>).
-define(NS_EXPIRE, <<"jabber:x:expire">>).
-define(NS_EVENT, <<"jabber:x:event">>).
-define(NS_CHATSTATES,
	<<"http://jabber.org/protocol/chatstates">>).
-define(NS_XCONFERENCE, <<"jabber:x:conference">>).
-define(NS_STATS,
	<<"http://jabber.org/protocol/stats">>).
-define(NS_MUC, <<"http://jabber.org/protocol/muc">>).
-define(NS_CREATE_MUC, <<"http://jabber.org/protocol/create_muc">>).
-define(NS_MUC_USER,
	<<"http://jabber.org/protocol/muc#user">>).
-define(NS_MUC_ADMIN,
	<<"http://jabber.org/protocol/muc#admin">>).
-define(NS_MUC_OWNER,
	<<"http://jabber.org/protocol/muc#owner">>).
-define(NS_MUC_UNIQUE,
	<<"http://jabber.org/protocol/muc#unique">>).
-define(NS_MUC_ONLINE_REGISTER,
	<<"http://jabber.org/protocol/muc#online_register">>).
-define(NS_MUC_IQ_MEMBER,
	<<"http://jabber.org/protocol/muc#member">>).
-define(NS_MUC_INVITE,
	<<"http://jabber.org/protocol/muc#invite">>).
-define(NS_MUC_INVITE_V2,
	<<"http://jabber.org/protocol/muc#invite_v2">>).
-define(NS_MUC_AUTHORITY,
	<<"http://jabber.org/protocol/muc#authority">>).
-define(NS_MUC_REGISTER,
	<<"http://jabber.org/protocol/muc#register">>).
-define(NS_MUC_DEL_USER,
	<<"http://jabber.org/protocol/muc#del_user">>).
-define(NS_MUC_ADD_USER,
	<<"http://jabber.org/protocol/muc#add_user">>).
-define(NS_MUC_SET_REGISTER,
	<<"http://jabber.org/protocol/muc#set_register">>).
-define(NS_MUC_VCARD_UPDATE,
	<<"http://jabber.org/protocol/muc#vcard_update">>).
-define(NS_MUC_DEL_REGISTER,
	<<"http://jabber.org/protocol/muc#del_register">>).
-define(NS_MUC_USER_SUBSCRIBE,
	<<"http://jabber.org/protocol/muc#muc_user_subscribe">>).
-define(NS_USER_MUCS,
	<<"http://jabber.org/protocol/muc#user_mucs">>).
-define(NS_PRESENCE_ALL,
	<<"http://jabber.org/protocol/muc#presence_all">>).
-define(NS_PRESENCE_ALL_V2,
	<<"http://jabber.org/protocol/muc#presence_all_v2">>).
-define(NS_SUBPUBSUB,
	<<"http://jabber.org/protocol/subscription">>).
-define(NS_PUBSUB,
	<<"http://jabber.org/protocol/pubsub">>).
-define(NS_PUBSUB_EVENT,
	<<"http://jabber.org/protocol/pubsub#event">>).
-define(NS_PUBSUB_META_DATA,
	<<"http://jabber.org/protocol/pubsub#meta-data">>).
-define(NS_PUBSUB_OWNER,
	<<"http://jabber.org/protocol/pubsub#owner">>).
-define(NS_PUBSUB_NMI,
	<<"http://jabber.org/protocol/pubsub#node-meta-info">>).
-define(NS_PUBSUB_ERRORS,
	<<"http://jabber.org/protocol/pubsub#errors">>).
-define(NS_PUBSUB_NODE_CONFIG,
	<<"http://jabber.org/protocol/pubsub#node_config">>).
-define(NS_PUBSUB_SUB_OPTIONS,
	<<"http://jabber.org/protocol/pubsub#subscribe_options">>).
-define(NS_PUBSUB_SUBSCRIBE_OPTIONS,
	<<"http://jabber.org/protocol/pubsub#subscribe_options">>).
-define(NS_PUBSUB_PUBLISH_OPTIONS,
	<<"http://jabber.org/protocol/pubsub#publish_options">>).
-define(NS_PUBSUB_SUB_AUTH,
	<<"http://jabber.org/protocol/pubsub#subscribe_authorization">>).
-define(NS_PUBSUB_GET_PENDING,
	<<"http://jabber.org/protocol/pubsub#get-pending">>).
-define(NS_COMMANDS,
	<<"http://jabber.org/protocol/commands">>).
-define(NS_BYTESTREAMS,
	<<"http://jabber.org/protocol/bytestreams">>).
-define(NS_MAC_PUSH_NOTICE,
	<<"http://jabber.org/protocol/mac_push_notice">>).
-define(NS_ADMIN,
	<<"http://jabber.org/protocol/admin">>).
-define(NS_ADMIN_ANNOUNCE,
	<<"http://jabber.org/protocol/admin#announce">>).
-define(NS_ADMIN_ANNOUNCE_ALL,
	<<"http://jabber.org/protocol/admin#announce-all">>).
-define(NS_ADMIN_SET_MOTD,
	<<"http://jabber.org/protocol/admin#set-motd">>).
-define(NS_ADMIN_EDIT_MOTD,
	<<"http://jabber.org/protocol/admin#edit-motd">>).
-define(NS_ADMIN_DELETE_MOTD,
	<<"http://jabber.org/protocol/admin#delete-motd">>).
-define(NS_ADMIN_ANNOUNCE_ALLHOSTS,
	<<"http://jabber.org/protocol/admin#announce-allhosts">>).
-define(NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS,
	<<"http://jabber.org/protocol/admin#announce-all-allhosts">>).
-define(NS_ADMIN_SET_MOTD_ALLHOSTS,
	<<"http://jabber.org/protocol/admin#set-motd-allhosts">>).
-define(NS_ADMIN_EDIT_MOTD_ALLHOSTS,
	<<"http://jabber.org/protocol/admin#edit-motd-allhosts">>).
-define(NS_ADMIN_DELETE_MOTD_ALLHOSTS,
	<<"http://jabber.org/protocol/admin#delete-motd-allhosts">>).
-define(NS_SERVERINFO,
	<<"http://jabber.org/network/serverinfo">>).
-define(NS_RSM, <<"http://jabber.org/protocol/rsm">>).
-define(NS_EJABBERD_CONFIG, <<"ejabberd:config">>).
-define(NS_STREAM,
	<<"http://etherx.jabber.org/streams">>).
-define(NS_STANZAS,
	<<"urn:ietf:params:xml:ns:xmpp-stanzas">>).
-define(NS_STREAMS,
	<<"urn:ietf:params:xml:ns:xmpp-streams">>).
-define(NS_TLS, <<"urn:ietf:params:xml:ns:xmpp-tls">>).
-define(NS_SASL,
	<<"urn:ietf:params:xml:ns:xmpp-sasl">>).
-define(NS_SESSION,
	<<"urn:ietf:params:xml:ns:xmpp-session">>).
-define(NS_BIND,
	<<"urn:ietf:params:xml:ns:xmpp-bind">>).
-define(NS_FEATURE_IQAUTH,
	<<"http://jabber.org/features/iq-auth">>).
-define(NS_FEATURE_IQREGISTER,
	<<"http://jabber.org/features/iq-register">>).
-define(NS_FEATURE_COMPRESS,
	<<"http://jabber.org/features/compress">>).
-define(NS_FEATURE_MSGOFFLINE, <<"msgoffline">>).
-define(NS_COMPRESS,
	<<"http://jabber.org/protocol/compress">>).
-define(NS_CAPS, <<"http://jabber.org/protocol/caps">>).
-define(NS_SHIM, <<"http://jabber.org/protocol/shim">>).
-define(NS_ADDRESS,
	<<"http://jabber.org/protocol/address">>).
-define(NS_OOB, <<"jabber:x:oob">>).
-define(NS_CAPTCHA, <<"urn:xmpp:captcha">>).
-define(NS_MEDIA, <<"urn:xmpp:media-element">>).
-define(NS_BOB, <<"urn:xmpp:bob">>).
-define(NS_PING, <<"urn:xmpp:ping">>).
-define(NS_KEY, <<"urn:xmpp:key">>).
-define(NS_SET_BLOCKED, <<"urn:xmpp:set-blocked">>).
-define(NS_CANCEL_BLOCKED, <<"urn:xmpp:cancel-blocked">>).
-define(NS_RECENT_CONTACT, <<"urn:xmpp:recent-contact">>).
-define(NS_MUC_CONTACT, <<"urn:xmpp:muc-contact">>).
-define(NS_CARBONS_2, <<"urn:xmpp:carbons:2">>).
-define(NS_CARBONS_1, <<"urn:xmpp:carbons:1">>).
-define(NS_FORWARD, <<"urn:xmpp:forward:0">>).
-define(NS_STREAM_MGMT_2,  <<"urn:xmpp:sm:2">>).
-define(NS_STREAM_MGMT_3,  <<"urn:xmpp:sm:3">>).
-define(NS_CHECK_VERSION,  <<"jabber:iq:check-version">>).
-define(NS_BLOCK, <<"urn:xmpp:block">>).
-define(CONFIG_XMPP_TIME_KEY, <<"config:xmpp:time_key">>).
