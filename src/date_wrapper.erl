%% Wrapper datime which call datetime functions
%%
%% Function             | Description
%% ----------------------------------------------------
%% check_date           | Verifies that the value is a date pattern
%% date2format          | Formating the value into a date pattern
%% datetime_conv_dt_ux  | Convert datime which in list or in binary to same datetime (erlang native datetime | unixtime)
%%

-module(date_wrapper).
-export([check_date/2]).
-export([date2format/2]).
-export([datetime_conv_dt_ux/3]).

%% ====================================================================================
%% API
%% ====================================================================================
-spec check_date(Date::binary(), Format::binary()) -> boolean() .
check_date(Date, Format) ->
  date_validate:check_date(Date, Format).

-spec date2format(Date::binary(), Format::binary()) -> binary() | tuple().
date2format(Date, Format)->
  date_format:date2format(Date, Format).

-spec datetime_conv_dt_ux(Datetime::binary() | list(), Type::atom(), UTC::integer())->
  binary() | integer().
datetime_conv_dt_ux(Datetime, Type, UTC)->
  datetime_conversion:datetime_conv_dt_ux(Datetime, Type, UTC).