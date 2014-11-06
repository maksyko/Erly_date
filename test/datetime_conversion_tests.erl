-module(datetime_conversion_tests).
-include_lib("eunit/include/eunit.hrl").

datetime_conversion_test() ->
  {{2008,12,12},{12,0,0}} = datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00:00">>, datetime, 0),
  {{2008,12,12},{12,0,0}} = datetime_conversion:datetime_conv_dt_ux("2008-12-12 12:00:00", datetime, 0),
  {{12,2000,12},{12,0,0}} = datetime_conversion:datetime_conv_dt_ux(<<"12-2000-12 12:00:00">>, datetime, 0),
  {{12,12,2000},{12,0,0}} = datetime_conversion:datetime_conv_dt_ux(<<"12-12-2000 12:00:00">>, datetime, 0),
  {error,wrong_format} = datetime_conversion:datetime_conv_dt_ux(<<"12-12-2000 12:00:">>, datetime, 0),
  {error,wrong_format} = datetime_conversion:datetime_conv_dt_ux(<<"12-12-2000 12:00:00">>, unixtime, 0),
  {error,wrong_format} = datetime_conversion:datetime_conv_dt_ux(<<"12-12-2000 12:00:00">>, unixti, 0),
  {error,wrong_format} = datetime_conversion:datetime_conv_dt_ux(<<"12-12-2000 12:00:00">>, unixtime, 0),
  63143845200 = datetime_conversion:datetime_conv_dt_ux(<<"2000-12-12 12:00:00">>, unixtime, 1),
  63143841600 = datetime_conversion:datetime_conv_dt_ux(<<"2000-12-12 12:00:00">>, unixtime, 0).

