-ifndef(MESSAGEKEYVALUE_PB_H).
-define(MESSAGEKEYVALUE_PB_H, true).
-record(messagekeyvalue, {
    key,
    value
}).
-endif.

-ifndef(STRINGHEADER_PB_H).
-define(STRINGHEADER_PB_H, true).
-record(stringheader, {
    params = [],
    key,
    value,
    definedkey
}).
-endif.

-ifndef(PACKAGELENGTH_PB_H).
-define(PACKAGELENGTH_PB_H, true).
-record(packagelength, {
    length
}).
-endif.

-ifndef(PROTOHEADER_PB_H).
-define(PROTOHEADER_PB_H, true).
-record(protoheader, {
    version,
    options,
    optionlist = [],
    length,
    content,
    message
}).
-endif.

-ifndef(AUTHMESSAGE_PB_H).
-define(AUTHMESSAGE_PB_H, true).
-record(authmessage, {
    mechanism,
    method,
    msgid,
    authkey,
    otherbody
}).
-endif.

-ifndef(WELCOMEMESSAGE_PB_H).
-define(WELCOMEMESSAGE_PB_H, true).
-record(welcomemessage, {
    domain,
    version,
    user,
    sockmod
}).
-endif.

-ifndef(STREAMBEGIN_PB_H).
-define(STREAMBEGIN_PB_H, true).
-record(streambegin, {
    domain,
    version,
    bodys = []
}).
-endif.

-ifndef(STARTTLS_PB_H).
-define(STARTTLS_PB_H, true).
-record(starttls, {
    
}).
-endif.

-ifndef(PROCEEDTLS_PB_H).
-define(PROCEEDTLS_PB_H, true).
-record(proceedtls, {
    
}).
-endif.

-ifndef(STREAMEND_PB_H).
-define(STREAMEND_PB_H, true).
-record(streamend, {
    
}).
-endif.

-ifndef(USERCONNECT_PB_H).
-define(USERCONNECT_PB_H, true).
-record(userconnect, {
    domain,
    version
}).
-endif.

-ifndef(CAPABILITY_PB_H).
-define(CAPABILITY_PB_H, true).
-record(capability, {
    version,
    bodys
}).
-endif.

-ifndef(RESPONSESUCCEEDED_PB_H).
-define(RESPONSESUCCEEDED_PB_H, true).
-record(responsesucceeded, {
    code,
    msgid,
    info,
    body
}).
-endif.

-ifndef(RESPONSEFAILURE_PB_H).
-define(RESPONSEFAILURE_PB_H, true).
-record(responsefailure, {
    code,
    msgid,
    error,
    body
}).
-endif.

-ifndef(PROTOMESSAGE_PB_H).
-define(PROTOMESSAGE_PB_H, true).
-record(protomessage, {
    options,
    signaltype = erlang:error({required, signaltype}),
    from,
    to,
    message
}).
-endif.

-ifndef(MESSAGEBODY_PB_H).
-define(MESSAGEBODY_PB_H, true).
-record(messagebody, {
    headers = [],
    value,
    bodys = []
}).
-endif.

-ifndef(IQMESSAGE_PB_H).
-define(IQMESSAGE_PB_H, true).
-record(iqmessage, {
    namespace,
    key,
    value,
    messageid,
    header,
    body,
    receivedtime,
    transfertime,
    headers = [],
    bodys = [],
    definedkey
}).
-endif.

-ifndef(PRESENCEMESSAGE_PB_H).
-define(PRESENCEMESSAGE_PB_H, true).
-record(presencemessage, {
    namespace,
    key,
    value,
    messageid,
    header,
    body,
    receivedtime,
    transfertime,
    headers = [],
    bodys = [],
    definedkey
}).
-endif.

-ifndef(XMPPMESSAGE_PB_H).
-define(XMPPMESSAGE_PB_H, true).
-record(xmppmessage, {
    messagetype = erlang:error({required, messagetype}),
    clienttype = erlang:error({required, clienttype}),
    clientversion = erlang:error({required, clientversion}),
    namespace,
    key,
    value,
    messageid,
    header,
    body,
    receivedtime,
    transfertime,
    headers = [],
    bodys = []
}).
-endif.

