-module(date_validate_tests).
-include_lib("eunit/include/eunit.hrl").

date_validate_test() ->
  true  = date_validate:check_date(<<"21-10-2008">>, <<"DD-MM-YYYY">>),
  true  = date_validate:check_date(<<"2112-10-20">>, <<"YYYY-MM-DD">>),
  true  = date_validate:check_date(<<"21-2000-10">>, <<"DD-YYYY-DD">>),
  false = date_validate:check_date(<<"21-2000-10">>, <<"DD/YYYY-DD">>),
  true  = date_validate:check_date(<<"21/2000/10">>, <<"DD/YYYY/DD">>),
  false = date_validate:check_date(<<"2/200/1">>, <<"DD/YYYY/DD">>),
  false = date_validate:check_date(<<"22/2002/12">>, <<"DD/YYYY/M">>).