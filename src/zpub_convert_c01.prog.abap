*&---------------------------------------------------------------------*
*& Include          ZPUB_CONVERT_C01
*&---------------------------------------------------------------------*
CLASS lcl_demo DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      main.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD main.

    DATA lv_text TYPE char20.
    DATA lv_raw TYPE xstring.
    DATA lo_cx_convert TYPE REF TO zcx_pub_convert.
    DATA lv_error TYPE string.

*************************************************************************************
    "1 Convert CHAR to text table
*************************************************************************************
    DATA: lt_text TYPE TABLE OF char2.
    lv_text = 'Hello World'(t01).
    zcl_pub_convert=>text_to_tab(
      EXPORTING
        iv_text = lv_text
      IMPORTING
        et_text = lt_text ).

*************************************************************************************
    "2 Convert CHAR to RAW and vice versa
*************************************************************************************
    " Code page
    TRY .
        DATA(lv_codepage) = zcl_pub_convert=>codepage_ext_to_int('UTF-16LE').
        lv_raw = zcl_pub_convert=>text_to_raw( iv_text = lv_text
                                           " iv_encoding = CONV #( lv_codepage )
                                           ).
        CLEAR lv_text.
        lv_text = zcl_pub_convert=>raw_to_text( iv_raw = lv_raw
                                            "iv_encoding = CONV #( lv_codepage )
                                           ).
      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    TYPES:
      BEGIN OF t_mrres,
        month   TYPE char8,
        zwstand TYPE e_zwstand,
        date    TYPE d,
      END OF t_mrres,

      tt_mrres TYPE STANDARD TABLE OF t_mrres.

    DATA lt_mrres TYPE tt_mrres.

    lt_mrres = VALUE #( ( month = 'January'(t02)   zwstand = 0   date = '20220131' )
                        ( month = 'March'(t03)     zwstand = 100 date = '20220228' )
                        ( month = 'June'(t04)      zwstand = 200 date = '20220331' )
                        ( month = 'September'(t05) zwstand = 300 date = '20220430' )
                        ( month = 'December'(t06)  zwstand = 500 date = '20220531' ) ).

*************************************************************************************
    "3 Convert to CSV and vice versa
*************************************************************************************
    DATA(lt_csv) = zcl_pub_convert=>tab_to_csv(
      EXPORTING
        it_data       = lt_mrres ).

    CLEAR lt_mrres.

    zcl_pub_convert=>csv_to_tab(
      EXPORTING
        it_csv       = lt_csv
      IMPORTING
        et_data  = lt_mrres ).


*************************************************************************************
    "4 Convert to JSON and vice versa
*************************************************************************************
    DATA(lv_json) = zcl_pub_convert=>data_to_json(
      EXPORTING
        iv_data  = lt_mrres ).

    CLEAR lt_mrres.

    zcl_pub_convert=>json_to_data(
      EXPORTING
        iv_json  = lv_json
      IMPORTING
        ev_data  = lt_mrres ).

*************************************************************************************
    "5 Convert to XML and vice versa
*************************************************************************************
    TRY .
        DATA(lv_xml) = zcl_pub_convert=>data_to_xml(
          EXPORTING
            iv_data = lt_mrres ).

        CLEAR lt_mrres.

        zcl_pub_convert=>xml_to_data(
          EXPORTING
            iv_xml  = lv_xml
          IMPORTING
            ev_data = lt_mrres ).
      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

*************************************************************************************
    "6 Convert to BASE64 and vice versa
*************************************************************************************
    TRY.
        DATA(lv_base64) = zcl_pub_convert=>text_to_base64(
          EXPORTING
            iv_text = CONV string( lv_text ) ).

        CLEAR lv_text.

        lv_text = zcl_pub_convert=>base64_to_text(
          EXPORTING
            iv_base64 = lv_base64 ).
      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

*************************************************************************************
    "7 Convert DEC to HEX and vice versa
*************************************************************************************
    DATA lv_dec TYPE i VALUE '31'.

    DATA(lv_hex) = zcl_pub_convert=>dec_to_hex( lv_dec ).

    CLEAR lv_dec.

    lv_dec = zcl_pub_convert=>hex_to_dec( lv_hex ).

*************************************************************************************
    "8 Convert sum to string and vice versa
*************************************************************************************
    DATA lv_amount TYPE wrbtr VALUE '123.4'.
    DATA(lv_amount_str) = zcl_pub_convert=>amount_to_string(
      EXPORTING
        iv_amount = lv_amount ).

    CLEAR lv_amount.

    zcl_pub_convert=>string_to_amount(
      EXPORTING
        iv_string = lv_amount_str
      IMPORTING
        ev_amount = lv_amount ).

*************************************************************************************
    "9 Convert UoM
*************************************************************************************
    DATA lv_menge TYPE p DECIMALS 2.

    TRY .
        zcl_pub_convert=>units(
          EXPORTING
            iv_menge_in = lv_amount
            iv_meins_in = 'KM'
          IMPORTING
            ev_menge_out = lv_menge ).
      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

*************************************************************************************
    "10 Convert GUID
*************************************************************************************
    TRY .
        DATA(lv_abap_guid) = zcl_pub_convert=>guid_xml_to_abap( 'x021087b-9908-43b2-a3a3-93baf8a83d17' ) ##NO_TEXT.
        DATA(lv_xml_guid) = zcl_pub_convert=>guid_abap_to_xml( lv_abap_guid ).
      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

*************************************************************************************
    "11 Convert DATETIME
*************************************************************************************
    TRY .
        DATA(lv_timestamp) = zcl_pub_convert=>datetime_xml_to_abap( '2002-05-30T09:00:00+03:00' ).
        DATA(lv_date) = zcl_pub_convert=>date_xml_to_abap( '2002-04-30T09:00:00+03:00' ).
        DATA(lv_datetime) = zcl_pub_convert=>datetime_abap_to_xml( iv_timestamp = lv_timestamp iv_zone = 'RUS03' ).
      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

*************************************************************************************
    "12 Convert date from external to internal format
*************************************************************************************
    TRY .
        DATA(lv_date_ext) = zcl_pub_convert=>date_int_to_ext( '20150430' ).
        DATA(lv_date_in) = zcl_pub_convert=>date_ext_to_int( lv_date_ext ).
      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

*************************************************************************************
    "13 Convert FLTP
*************************************************************************************
    TRY .
        lv_text = zcl_pub_convert=>fltp_to_char( '2.0071012000000000E+07' ).
        DATA(lv_fltp) = zcl_pub_convert=>char_to_fltp( lv_text ).
      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    lv_date_in = zcl_pub_convert=>fltp_to_date( '2.0071012000000000E+07' ).

*************************************************************************************
    "14 Convert with CONVERSION_EXIT
*************************************************************************************
    DATA:
      lv_atinn TYPE atinn,
      lv_atnam TYPE atnam VALUE 'MM_EKKO_GNETW_INR'.

    TRY.
        zcl_pub_convert=>conv_exit_input(
          EXPORTING
            iv_source = lv_atnam
          IMPORTING
            ev_target = lv_atinn ).

        CLEAR lv_atnam.

        zcl_pub_convert=>conv_exit_output(
          EXPORTING
            iv_source = lv_atinn
          IMPORTING
            ev_target = lv_atnam ).

      CATCH zcx_pub_convert INTO lo_cx_convert.
        lv_error = lo_cx_convert->get_text( ).
        MESSAGE lv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
