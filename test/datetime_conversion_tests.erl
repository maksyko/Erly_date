-module(datetime_conversion_tests).
-include_lib("eunit/include/eunit.hrl").

datetime_conversion_test() ->
  {{2008,12,12},{12,0,0}} = datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00:00">>, datetime, 0),
  63396306000 = datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00:00">>, unixtime, 1),
  <<"error,  expression not fit format,  failed source expression">> = datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00">>, datetime, 0).