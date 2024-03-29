-module(date_format_tests).
-include_lib("eunit/include/eunit.hrl").

date2format_test() ->
  <<"21/10/2008">> = date_format:date2format(<<"21-10-2008">>, <<"DD/MM/YYYY">>),
  <<"21/10/2008">> = date_format:date2format("21-10-2008", <<"DD/MM/YYYY">>),
  <<"21/10/2008">> = date_format:date2format("21-10-2008", "DD/MM/YYYY"),
  <<"21/10/2008">> = date_format:date2format("21-10-2008", <<"DD/MM/YYYY">>),
  {error,wrong_source_expression} = date_format:date2format("21-10-20", <<"DD/MM/YYYY">>),
  <<"21/10/2012">> = date_format:date2format("21-10-2012", <<"DD/MM/YYYY">>),
  <<"21$10$2012">> = date_format:date2format("21-10-2012", <<"DD$MM$YYYY">>).

