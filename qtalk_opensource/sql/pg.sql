--
-- ejabberd, Copyright (C) 2002-2014   ProcessOne
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--                         
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--



CREATE TABLE users
(
 username text NOT NULL,
 password text NOT NULL,
 created_at timestamp without time zone NOT NULL DEFAULT now(),
 name text,
 department text DEFAULT '未知部门'::text,
 dep1 text DEFAULT '未知部门'::text,
 dep2 text DEFAULT ''::text,
 dep3 text DEFAULT ''::text,
 dep4 text DEFAULT ''::text,
 dep5 text DEFAULT ''::text,
 fpinyin text,
 spinyin text,
 frozen_flag text DEFAULT '0'::text,
 sn text NOT NULL,
 hire_type text,
 version integer DEFAULT 0,
 type character(1),
 hire_flag smallint,
 CONSTRAINT users_pkey PRIMARY KEY (username)
);
-- Index: i_user_version

-- DROP INDEX i_user_version;

CREATE INDEX i_user_version
ON users
USING btree
(version);



CREATE TABLE last (
    username text PRIMARY KEY,
    seconds text NOT NULL,
    state text NOT NULL
);


CREATE TABLE rosterusers (
    username text NOT NULL,
    jid text NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text,
    "type" text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers USING btree (username, jid);
CREATE INDEX i_rosteru_username ON rosterusers USING btree (username);
CREATE INDEX i_rosteru_jid ON rosterusers USING btree (jid);


CREATE TABLE rostergroups (
    username text NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL
);

CREATE INDEX pk_rosterg_user_jid ON rostergroups USING btree (username, jid);

CREATE TABLE sr_group (
    name text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE sr_user (
    jid text NOT NULL,
    grp text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_sr_user_jid_grp ON sr_user USING btree (jid, grp);
CREATE INDEX i_sr_user_jid ON sr_user USING btree (jid);
CREATE INDEX i_sr_user_grp ON sr_user USING btree (grp);

CREATE TABLE spool (
	username text NOT NULL,
	xml text NOT NULL,
	seq integer NOT NULL,
	created_at timestamp without time zone DEFAULT now() NOT NULL,
	send_flag text DEFAULT '1'::text,
	from_username text DEFAULT '未知'::text NOT NULL,
	notice_flag text DEFAULT '0'::text,
	away_flag text DEFAULT '0'::text
	);

CREATE SEQUENCE spool_seq_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE spool_seq_seq OWNED BY spool.seq;

ALTER TABLE ONLY spool
    ADD CONSTRAINT spool_pkey PRIMARY KEY (seq);


CREATE INDEX i_despool ON spool USING btree (username);
CREATE INDEX spool_crated_at_idx ON spool USING btree (created_at);
CREATE INDEX spool_send_flag_idx ON spool USING btree (send_flag);


CREATE TABLE vcard (
    username text PRIMARY KEY,
    vcard text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_vcard_un ON vcard USING btree (username);


CREATE TABLE vcard_xupdate (
    username text PRIMARY KEY,
    hash text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);


CREATE TABLE vcard_search (
    username text NOT NULL,
    lusername text PRIMARY KEY,
    fn text NOT NULL,
    lfn text NOT NULL,
    family text NOT NULL,
    lfamily text NOT NULL,
    given text NOT NULL,
    lgiven text NOT NULL,
    middle text NOT NULL,
    lmiddle text NOT NULL,
    nickname text NOT NULL,
    lnickname text NOT NULL,
    bday text NOT NULL,
    lbday text NOT NULL,
    ctry text NOT NULL,
    lctry text NOT NULL,
    locality text NOT NULL,
    llocality text NOT NULL,
    email text NOT NULL,
    lemail text NOT NULL,
    orgname text NOT NULL,
    lorgname text NOT NULL,
    orgunit text NOT NULL,
    lorgunit text NOT NULL
);

CREATE INDEX i_vcard_search_lfn       ON vcard_search(lfn);
CREATE INDEX i_vcard_search_lfamily   ON vcard_search(lfamily);
CREATE INDEX i_vcard_search_lgiven    ON vcard_search(lgiven);
CREATE INDEX i_vcard_search_lmiddle   ON vcard_search(lmiddle);
CREATE INDEX i_vcard_search_lnickname ON vcard_search(lnickname);
CREATE INDEX i_vcard_search_lbday     ON vcard_search(lbday);
CREATE INDEX i_vcard_search_lctry     ON vcard_search(lctry);
CREATE INDEX i_vcard_search_llocality ON vcard_search(llocality);
CREATE INDEX i_vcard_search_lemail    ON vcard_search(lemail);
CREATE INDEX i_vcard_search_lorgname  ON vcard_search(lorgname);
CREATE INDEX i_vcard_search_lorgunit  ON vcard_search(lorgunit);

CREATE TABLE privacy_default_list (
    username text PRIMARY KEY,
    name text NOT NULL
);

CREATE TABLE privacy_list (
    username text NOT NULL,
    name text NOT NULL,
    id SERIAL UNIQUE,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_privacy_list_username ON privacy_list USING btree (username);
CREATE UNIQUE INDEX i_privacy_list_username_name ON privacy_list USING btree (username, name);

CREATE TABLE privacy_list_data (
    id bigint REFERENCES privacy_list(id) ON DELETE CASCADE,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
);

CREATE TABLE private_storage (
    username text NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_private_storage_username ON private_storage USING btree (username);
CREATE UNIQUE INDEX i_private_storage_username_namespace ON private_storage USING btree (username, namespace);


CREATE TABLE roster_version (
    username text PRIMARY KEY,
    version text NOT NULL
);

-- To update from 0.9.8:
-- CREATE SEQUENCE spool_seq_seq;
-- ALTER TABLE spool ADD COLUMN seq integer;
-- ALTER TABLE spool ALTER COLUMN seq SET DEFAULT nextval('spool_seq_seq');
-- UPDATE spool SET seq = DEFAULT;
-- ALTER TABLE spool ALTER COLUMN seq SET NOT NULL;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;

CREATE TABLE pubsub_node (
  host text,
  node text,
  parent text,
  "type" text,
  nodeid SERIAL UNIQUE
);
CREATE INDEX i_pubsub_node_parent ON pubsub_node USING btree (parent);
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node USING btree (host, node);

CREATE TABLE pubsub_node_option (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  name text,
  val text
);
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option USING btree (nodeid);

CREATE TABLE pubsub_node_owner (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  owner text
);
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner USING btree (nodeid);

CREATE TABLE pubsub_state (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  jid text,
  affiliation character(1),
  subscriptions text,
  stateid SERIAL UNIQUE
);
CREATE INDEX i_pubsub_state_jid ON pubsub_state USING btree (jid);
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state USING btree (nodeid, jid);

CREATE TABLE pubsub_item (
  nodeid bigint REFERENCES pubsub_node(nodeid) ON DELETE CASCADE,
  itemid text, 
  publisher text,
  creation text,
  modification text,
  payload text
);
CREATE INDEX i_pubsub_item_itemid ON pubsub_item USING btree (itemid);
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item USING btree (nodeid, itemid);

CREATE TABLE pubsub_subscription_opt (
  subid text,
  opt_name varchar(32),
  opt_value text
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt USING btree (subid, opt_name);

CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    opts text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room USING btree (name, host);

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    nick text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_muc_registered_nick ON muc_registered USING btree (nick);
CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered USING btree (jid, host);

CREATE TABLE irc_custom (
    jid text NOT NULL,
    host text NOT NULL,
    data text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_irc_custom_jid_host ON irc_custom USING btree (jid, host);

CREATE TABLE motd (
    username text PRIMARY KEY,
    xml text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE caps_features (
    node text NOT NULL,
    subnode text NOT NULL,
    feature text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_caps_features_node_subnode ON caps_features USING btree (node, subnode);


------New Add


CREATE TABLE day_online_time
(
  username character varying(50) NOT NULL,
  date_time character varying(20) NOT NULL,
  online_time integer,
  CONSTRAINT day_online_time_pkey PRIMARY KEY (username, date_time)
);

CREATE TABLE iplimit
(
 ip text NOT NULL,
 created_at timestamp(6) without time zone NOT NULL DEFAULT now(),
 descriptions text NOT NULL DEFAULT now(),
 priority text NOT NULL DEFAULT '1'::text,
 name text NOT NULL DEFAULT 'all'::text,
 CONSTRAINT pk_iplimit PRIMARY KEY (ip, name, priority)
 );


-- Table: msg_history

-- DROP TABLE msg_history;

CREATE TABLE msg_history
(
 m_from character varying(255),
 m_to character varying(255),
 m_body text,
 m_timestamp integer DEFAULT date_part('epoch'::text, now()),
 msg_id text,
 read_flag integer DEFAULT 0,
 to_host text DEFAULT 'ejabberd_host'::text,
 from_host text DEFAULT 'ejabberd_host'::text
 );

-- Index: i_from_host_idx

-- DROP INDEX i_from_host_idx;

CREATE INDEX i_from_host_idx ON msg_history USING btree (from_host);

-- Index: i_msg_from_fhost_to_thost_timestamp

-- DROP INDEX i_msg_from_fhost_to_thost_timestamp;

CREATE INDEX i_msg_from_fhost_to_thost_timestamp_idx ON msg_history USING btree (m_from, m_to, m_timestamp, from_host, to_host);

CREATE INDEX i_msg_history_m_timestamp_m_from_m_to_idx ON msg_history USING btree (m_timestamp, m_from, m_to);

CREATE INDEX i_msg_history_from_host_to_host_idx ON msg_history USING btree (from_host, to_host);

CREATE INDEX i_msg_history_msg_id_idx1 ON msg_history USING btree (msg_id);

CREATE TABLE muc_room_history (
	muc_room_name character varying(255),
	nick character varying(255),
	packet text,
	have_subject boolean,
	size character varying(255),
	m_timestamp integer DEFAULT date_part('epoch'::text, now()),
	create_time timestamp with time zone DEFAULT now(),
	id bigint NOT NULL,
	host text DEFAULT 'ejabberd_host'::text
);

CREATE SEQUENCE muc_room_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE muc_room_history_id_seq OWNED BY muc_room_history.id;

ALTER TABLE ONLY muc_room_history
    ADD CONSTRAINT muc_room_history_pkey PRIMARY KEY (id);


CREATE INDEX muc_room_history_muc_room_name_idx ON muc_room_history USING btree (muc_room_name);
CREATE INDEX muc_room_history_nick_idx ON muc_room_history USING btree (nick);
CREATE INDEX muc_room_history_room_name_m_t_idx ON muc_room_history USING btree (muc_room_name, m_timestamp);

CREATE TABLE muc_room_users (
	muc_name character varying(200) NOT NULL,
	username character varying(200) NOT NULL,
	host character varying(200) NOT NULL,
	subscribe_flag text DEFAULT '0'::text NOT NULL,
	id bigint NOT NULL,
	date bigint,
	login_date integer
);

CREATE SEQUENCE muc_room_users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE muc_room_users_id_seq OWNED BY muc_room_users.id;


ALTER TABLE ONLY muc_room_users
    ADD CONSTRAINT muc_room_users_pkey PRIMARY KEY (id);

CREATE UNIQUE INDEX muc_room_users_muc_name_username_idx ON muc_room_users USING btree (muc_name, username);

CREATE TABLE muc_spool (
	username text NOT NULL,
	xml text NOT NULL,
	seq integer NOT NULL,
	created_at timestamp without time zone DEFAULT now() NOT NULL,
	send_flag text DEFAULT '1'::text,
	from_username text DEFAULT '未知'::text NOT NULL,
	nick text
);

CREATE SEQUENCE muc_spool_seq_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE muc_spool_seq_seq OWNED BY muc_spool.seq;

ALTER TABLE ONLY muc_spool
    ADD CONSTRAINT muc_spool_pkey PRIMARY KEY (seq);

CREATE INDEX muc_spool_creaetd_at_idx ON muc_spool USING btree (created_at);

CREATE INDEX muc_spool_username_idx ON muc_spool USING btree (username);

CREATE TABLE muc_vcard_info (
	muc_name text NOT NULL,
	show_name text NOT NULL,
	muc_desc text,
	muc_title text,
	muc_pic text,
	version text
);

ALTER TABLE ONLY muc_vcard_info
    ADD CONSTRAINT muc_vcard_info_pkey PRIMARY KEY (muc_name);

CREATE TABLE robot_info (
	en_name text NOT NULL,
	cn_name text NOT NULL,
	request_url text NOT NULL,
	rbt_desc text,
	rbt_body text NOT NULL,
	rbt_version bigint,
	password text,
	recommend smallint
);

ALTER TABLE ONLY robot_info
    ADD CONSTRAINT robot_info_pkey PRIMARY KEY (en_name);

CREATE TABLE robot_pubsub (
    rbt_name text NOT NULL,
    user_name text NOT NULL
);


ALTER TABLE ONLY robot_pubsub
    ADD CONSTRAINT robot_pubsub_pkey PRIMARY KEY (rbt_name, user_name);


CREATE TABLE user_mac_key (
	user_name character varying(255) NOT NULL,
	host character varying(255) NOT NULL,
	mac_key character varying(255),
	os text DEFAULT 'ios'::text NOT NULL,
	version text,
	push_flag integer DEFAULT 0 NOT NULL
);

ALTER TABLE ONLY user_mac_key
    ADD CONSTRAINT user_mac_key_pkey PRIMARY KEY (user_name, host, os);

CREATE INDEX uer_mac_key_user_name_idx ON user_mac_key USING btree (user_name);

CREATE TABLE vcard_version (
	username text NOT NULL,
	version integer,
	url text,
	uin character varying(30),
	id bigint NOT NULL,
	profile_version smallint DEFAULT (1)::smallint,
	mood text
);

CREATE SEQUENCE vcard_version_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE vcard_version_id_seq OWNED BY vcard_version.id;

ALTER TABLE ONLY vcard_version
    ADD CONSTRAINT vcard_version_pkey PRIMARY KEY (id);

CREATE UNIQUE INDEX vcard_version_username_idx ON vcard_version USING btree (username);




CREATE TABLE white_list (
    username text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    single_flag text DEFAULT '1'::text
);

ALTER TABLE ONLY white_list
    ADD CONSTRAINT white_list_pkey PRIMARY KEY (username);


ALTER TABLE ONLY muc_room_history ALTER COLUMN id SET DEFAULT nextval('muc_room_history_id_seq'::regclass);
ALTER TABLE ONLY muc_spool ALTER COLUMN seq SET DEFAULT nextval('muc_spool_seq_seq'::regclass);
ALTER TABLE ONLY muc_room_users ALTER COLUMN id SET DEFAULT nextval('muc_room_users_id_seq'::regclass);
ALTER TABLE ONLY vcard_version ALTER COLUMN id SET DEFAULT nextval('vcard_version_id_seq'::regclass);

CREATE FUNCTION spilt_users_to_insert_xml(users text, muc_name text, nick text, msg text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  declare cnt int;
  declare i int;
  declare v_result text;
  declare f_delimiter varchar(10);
  declare ret_sql text;
  begin
      i := 1;
      f_delimiter := ',';
      ret_sql := '';
      cnt := length(users) - length(replace(users,f_delimiter,''))+1;
      while i <= cnt
     loop
        v_result := split_part(users,f_delimiter,i);
        i := i + 1;
        ret_sql := 'insert into muc_spool(username,xml,from_username,nick) values(''' || v_result || ''',' || quote_literal(msg) || ',''' || muc_name || ''',''' || nick|| ''')';
	execute ret_sql;
     end loop;
  return 1;
  EXCEPTION  
	when others then  
	return 0 ; 
  end;
