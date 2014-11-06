%% Formating the value into a date pattern
%% The following pattern characters are recognized:
%%
%% Pattern |      Description
%% ----------------------------------------------------
%% dd      | Day of month 01 to 31, zero leading
%% DD      | Day of month 01 to 31, zero leading
%% mm      | Month digit 01 to 12, zero leading
%% MM      | Month digit 01 to 12, zero leading
%% yyyy    | 4 year digit
%% YYYY    | 4 year digit
%%
%% For example, to format a date binary <<"21-10-2008">>, use the following:
%% date_format:date2format(<<"21-10-2008">>, <<"DD/MM/YYYY">>).

-module(date_format).
-export([date2format/2]).

-spec date2format(Date::binary() | list(), Format::binary()) -> tuple() | binary().
date2format(Date, Format) when ((is_binary(Date) or is_list(Date)) and (is_binary(Format) or is_list(Format))) ->
  date_vs_remark(format_match(to_binary(Format)), to_binary(Format), to_binary(Date));
date2format(_Date,_Format) -> {error , wrong_format}.

-spec to_binary(Param::binary() | list()) -> binary() | list().
to_binary(Param) when is_binary(Param) -> Param;
to_binary(Param) when is_list(Param) -> list_to_binary(Param).

-spec is_format(List::list()) -> binary().
is_format(List) ->
  [D1,F2,D3,F4,D5,_D2,_D4] = List,
  list_to_binary(binary_to_list(D1) ++ binary_to_list(F2) ++ binary_to_list(D3) ++ binary_to_list(F4) ++ binary_to_list(D5)).

-spec format_match(Format::binary()) -> binary().
format_match(Format) ->
  binary:matches(Format, [<<"YYYY">>,<<"YY">>,<<"yyyy">>,<<"yy">>,<<"MM">>,<<"mm">>, <<"DD">>,<<"dd">>]).

-spec date_vs_remark(list(), Format::binary(), Date::binary()) -> tuple() | binary() .
date_vs_remark([{0,2},{3,2},{6,2}], <<_F1:16,F2:8,_F3:16,F4:8,_F5:16>>, <<D1:16,D2:8,D3:16,D4:8,D5:16>>) ->
  is_format([<<D1:16>>,<<F2:8>>,<<D3:16>>,<<F4:8>>,<<D5:16>>,D2,D4]);
date_vs_remark([{0,2},{3,2},{6,4}],<<_F1:16,F2:8,_F3:16,F4:8,_F5:32>>, <<D1:16,D2:8,D3:16,D4:8,D5:32>>) ->
  is_format([<<D1:16>>,<<F2:8>>,<<D3:16>>,<<F4:8>>,<<D5:32>>,D2,D4]);
date_vs_remark([{0,2},{3,4},{8,2}],<<_F1:16,F2:8,_F3:32,F4:8,_F5:16>>, <<D1:16,D2:8,D3:32,D4:8,D5:16>>) ->
  is_format([<<D1:16>>,<<F2:8>>,<<D3:32>>,<<F4:8>>,<<D5:16>>,D2,D4]);
date_vs_remark([{0,4},{5,2},{8,2}], <<_F1:32,F2:8,_F3:16,F4:8,_F5:16>>, <<D1:32,D2:8,D3:16,D4:8,D5:16>>) ->
  is_format([<<D1:32>>,<<F2:8>>,<<D3:16>>,<<F4:8>>,<<D5:16>>,D2,D4]);
date_vs_remark(_,_,_) -> {error, wrong_source_expression}.