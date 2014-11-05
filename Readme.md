#Erly_date

This library contains useful functions to use datetime.

**License**: PrivatBank

**Author**: Evgenij Maksimenko (evgenij.maksimenko.01@privatbank.ua)

Examples
--------

date_format.erl:

.. code_block:: erlang

    10> date_format:date2format(<<"21-10-2008">>, <<"DD/MM/YYYY">>).
    <<"21/10/2008">>
    12> date_format:date2format(<<"21-2008-10">>, <<"DD/YYYY/MM">>).  
    <<"21/2008/10">>
    13> date_format:date2format(<<"2002-02-20">>, <<"YYYY$MM$DD">>).
    <<"2002$02$20">>
    
date_validate.erl:

.. code_block:: erlang
    
    14> date_validate:check_date(<<"21-10-2008">>, <<"DD-MM-YYYY">>).
    true
    15> date_validate:check_date(<<"21-10-2008">>, <<"DD$MM$YYYY">>).
    false
    16> date_validate:check_date(<<"2008-12-21">>, <<"DD-MM-YYYY">>).
    false

datetime_conversion.erl:

.. code_block:: erlang

    19> datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00:00">>, datetime, 0).
    {{2008,12,12},{12,0,0}}
    20> datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00:00">>, unixtime, 1).
    63396306000
    21> datetime_conversion:datetime_conv_dt_ux(<<"2008-12-12 12:00:00">>, unixtime, 0).
    63396302400
    28> datetime_conversion:datetime_conv_dt_ux("2008-12-12 12:00:00", datetime, 0).
    {{2008,12,12},{12,0,0}}
