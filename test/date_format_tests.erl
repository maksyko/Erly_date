-module(date_format_tests).
-include_lib("eunit/include/eunit.hrl").

date2format_test() ->
  <<"21/10/2008">> = date_format:date2format(<<"21-10-2008">>, <<"DD/MM/YYYY">>),
  <<"error, failed match format, source expression">> = date_format:date2format(<<"21-10-20">>, <<"DD/MM/YYYY">>),
  <<"error, expression not fit format, failed source expression">> = date_format:date2format("21-10-2012", <<"DD/MM/YYYY">>).