-module(date_validate_tests).
-include_lib("eunit/include/eunit.hrl").

date_validate_test() ->
  true = date_validate:check_date(<<"21-10-2008">>, <<"DD-MM-YYYY">>),
  false = date_validate:check_date(<<"21-102008">>, <<"DD-MM-YYYY">>).