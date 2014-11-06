%% Convert datime which in list or in binary to same datetime but with
%% erlang native datetime format or unixtime
%% The following pattern characters are recognized:
%%
%% Type          | Description
%% ----------------------------------------------------
%% datetime      | erlang native datetime
%% unixtime      | seconds
%% UTC           | 0-9 Contry zone
%%
%% For example, to convert list or binary a datetime or unixtime, use the following:
%% datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00:00">>, datetime, 0).
%% datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00:00">>, unixtime, 1).

-module(datetime_conversion).
-export([datetime_conv_dt_ux/3]).

-spec datetime_conv_dt_ux(Datetime::binary() | list(), Type::atom(), UTC::integer())->
  tuple() | integer().
datetime_conv_dt_ux(Datetime, Type, UTC)
  when ((is_binary(Datetime) or is_list(Datetime)) and is_atom(Type) and is_integer(UTC))->
  try
    check_conv(Datetime, Type, UTC)
  catch
    _ : _Reason -> {error, wrong_format}
  end;
datetime_conv_dt_ux(_Datetime, _Type, _UTC) -> {error, wrong_format}.

-spec check_conv(Datetime::binary(), Type::atom(), UTC::integer()) ->
  tuple() | integer().
check_conv(Datetime, datetime, _UTC) ->
  {Y, M, D, H, I, S} = get_data(Datetime),
  {{Y,M,D},{H,I,S}};
check_conv(Datetime, unixtime, UTC) ->
  {Y, M, D, H, I, S} = get_data(Datetime),
  calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H + UTC, I, S}}).

-spec to_binary(Param::binary() | list()) -> binary() | list().
to_binary(Param) when is_binary(Param) -> Param;
to_binary(Param) when is_list(Param) -> list_to_binary(Param).

-spec get_separator(boolean(), Dispatcher::binary(), _ListEl::list()) ->
  binary().
get_separator(false, Dispatcher, _ListEl) ->Dispatcher;
get_separator(true, _Dispatcher, ListEl) ->lists:nth(3, ListEl).

-spec get_data(Datetime::binary())->list().
get_data(Datetime) ->
    is_separator(is_split(binary:split(to_binary(Datetime), <<" ">>))).

-spec is_split(list() | any()) -> list() | tuple().
is_split([Date, Time]) ->
  Sheet         = [<<"0">>,<<"1">>,<<"2">>,<<"3">>,<<"4">>,<<"5">>,<<"6">>,<<"7">>,<<"8">>,<<"9">>],
  ListEl        = [ <<El>> || El <- binary_to_list(Date) ],
  Dispatcher    = lists:nth(5, ListEl),
  DispatcherEL  = get_separator(lists:member(Dispatcher, Sheet),Dispatcher, ListEl),
  [DispatcherEL, Date, Time];
is_split([_])   -> {error, wrong_format};
is_split(_)     -> {error, wrong_format}.

-spec is_separator(IsPlit::list() | false) -> tuple().
is_separator(IsPlit) ->
  [DispatcherEL, Date, Time]  = IsPlit,
  [H,I,S]       = binary:split(Time, <<":">>, [global]),
  [Y,M,D]       = binary:split(Date, DispatcherEL, [global]),
  {binary_to_integer(Y),binary_to_integer(M),binary_to_integer(D),binary_to_integer(H),binary_to_integer(I),binary_to_integer(S)}.