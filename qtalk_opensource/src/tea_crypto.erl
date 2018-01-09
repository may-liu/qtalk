-module(tea_crypto).
-export([encrypt/2,decrypt/2]).
-define(DELTA,16#9e3779b9).
-define(MAX,16#FFFFFFFF).

encrypt(V, K) when erlang:is_binary(K) and erlang:is_binary(V) and
					(erlang:size(K) == 16) ->
	encrypt(V, K, <<>>);
encrypt(_, _) ->
	erlang:error(badarg).

encrypt(<<V:8/binary, Rest/binary>>, K, Acc) ->
	R = do_encrypt(V, K),
	encrypt(Rest, K, <<Acc/binary, R/binary>>);
encrypt(V, _K, Acc) ->
	<<Acc/binary, V/binary>>.

do_encrypt(V,K) when erlang:is_binary(K) and erlang:is_binary(V) and
                       (erlang:size(V) == 8) and (erlang:size(K) == 16)->
    <<V0:32,V1:32>> = V,
    <<K0:32,K1:32,K2:32,K3:32>> = K,
    encrypt_compute(0,V0,V1,K0,K1,K2,K3,0);
do_encrypt(_,_) ->
    erlang:error(badarg).

encrypt_compute(32,V0,V1,_K0,_K1,_K2,_K3,_Sum)->
    <<V0:32,V1:32>>;
encrypt_compute(I,V0,V1,K0,K1,K2,K3,Sum)->    
    Sum1 = Sum + ?DELTA,
    V1K0 = V1 bsl 4 + K0,
    V1Sum = V1 + Sum1,
    V1K1 = (V1 bsr 5) + K1,
    NewV0 = (V0 + ((V1K0 bxor V1Sum) bxor V1K1) ) band ?MAX,
    V0K2 = NewV0 bsl 4 + K2,
    V0Sum = NewV0 + Sum1,
    V0K3 = (NewV0 bsr 5) + K3 ,
    NewV1 = (V1 +((V0K2 bxor V0Sum) bxor V0K3)) band ?MAX,
    encrypt_compute(I + 1,NewV0,NewV1,K0,K1,K2,K3,Sum1).

decrypt(V, K) when erlang:is_binary(K) and erlang:is_binary(V) and
					(erlang:size(K) == 16) ->
	decrypt(V, K, <<>>);
decrypt(_, _) ->
	erlang:error(badarg).

decrypt(<<V:8/binary, Rest/binary>>, K, Acc) ->
	R = do_decrypt(V, K),
	decrypt(Rest, K, <<Acc/binary, R/binary>>);
decrypt(V, _K, Acc) ->
	<<Acc/binary, V/binary>>.

do_decrypt(V,K) when erlang:is_binary(K) and erlang:is_binary(V) and
                       (erlang:size(V) == 8) and (erlang:size(K) == 16)->
    <<V0:32,V1:32>> = V,
    <<K0:32,K1:32,K2:32,K3:32>> = K,
    decrypt_compute(0,V0,V1,K0,K1,K2,K3,16#c6ef3720);
do_decrypt(_,_) ->
    erlang:error(badarg).

decrypt_compute(32,V0,V1,_K0,_K1,_K2,_K3,_Sum)->
    <<V0:32,V1:32>>;
decrypt_compute(I,V0,V1,K0,K1,K2,K3,Sum)->
    V0K2 = V0 bsl 4 + K2,
    V0Sum = V0 + Sum,
    V0K3 = (V0 bsr 5) + K3 ,
    NewV1 = (V1 - (((V0K2 bxor V0Sum) bxor V0K3) band ?MAX)) band ?MAX,

    V1K0 = NewV1 bsl 4 + K0,
    V1Sum = NewV1 + Sum,
    V1K1 = (NewV1 bsr 5) + K1,
    NewV0 = (V0 - (((V1K0 bxor V1Sum) bxor V1K1) band ?MAX)) band ?MAX,
    Sum1 = Sum - ?DELTA,
    decrypt_compute(I + 1,NewV0,NewV1,K0,K1,K2,K3,Sum1).
