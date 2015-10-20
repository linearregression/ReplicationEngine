%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-module(replication_helper).
-author("jonromero").

-ifndef('USE_ERLANG_NOW').
-compile({nowarn_deprecated_function, {erlang,now,0}}).
-define(timestamp(), erlang:now()).
-else.
-define(timestamp(), erlang:timestamp()).
-endif.

-export([otp_release/0, now/0]).

-spec otp_release() -> integer().
otp_release() ->
    try
        erlang:list_to_integer(erlang:system_info(otp_release))
    catch
        error:badarg ->
            16
    end.

-spec now() -> erlang:timestamp().
% erlang:now is deprecated post 1
% should use erlang:unique_integer,
% timestamp is not strictly monontonic
now() -> ?timestamp().
%    case otp_release() >= 18 of
%        true -> 
%             erlang:timestamp(); 
%        false ->
%             erlang:now()
%    end.


