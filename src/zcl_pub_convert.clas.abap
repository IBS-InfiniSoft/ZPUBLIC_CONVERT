class ZCL_PUB_CONVERT definition
  public
  final
  create public .

public section.

  class-methods CONV_EXIT_INPUT
    importing
      !IV_SOURCE type ANY
      !IV_CONV_EXIT type CONVEXIT optional
      !IV_CONV_SAP type ABAP_BOOL default ABAP_TRUE
    exporting
      !EV_TARGET type ANY
    raising
      ZCX_PUB_CONVERT .
  class-methods CONV_EXIT_OUTPUT
    importing
      !IV_SOURCE type ANY
      !IV_CONV_EXIT type CONVEXIT optional
    exporting
      !EV_TARGET type ANY
    raising
      ZCX_PUB_CONVERT .
  class-methods DATE_EXT_TO_INT
    importing
      !IV_DATE type SIMPLE
    returning
      value(RV_DATE) type D
    raising
      ZCX_PUB_CONVERT .
  class-methods DATE_INT_TO_EXT
    importing
      !IV_DATE type D
    returning
      value(RV_DATE) type STRING
    raising
      ZCX_PUB_CONVERT .
  class-methods HEX_TO_DEC
    importing
      !IV_HEX type XSTRING
    returning
      value(RV_DEC) type TREX_RFC-REQUEST_FLAG_L .
  class-methods DEC_TO_HEX
    importing
      !IV_DEC type NUMERIC
    returning
      value(RV_HEX) type XSTRING .
  class-methods FLTP_TO_DATE
    importing
      !IV_FLTP type ATFLV
    returning
      value(RV_DATE) type D .
  class-methods FLTP_TO_CHAR
    importing
      !IV_FLTP type F
    returning
      value(RV_CHAR) type STRING .
  class-methods CHAR_TO_FLTP
    importing
      !IV_CHAR type SIMPLE
    returning
      value(RV_FLTP) type F
    raising
      ZCX_PUB_CONVERT .
  class-methods CODEPAGE_EXT_TO_INT
    importing
      !IV_CODEPAGE_EXT type SIMPLE
    returning
      value(RV_CODEPAGE_INT) type CPCODEPAGE
    raising
      ZCX_PUB_CONVERT .
  class-methods TEXT_TO_RAW
    importing
      !IV_TEXT type SIMPLE
      !IV_ENCODING type ABAP_ENCODING default 'DEFAULT'
    returning
      value(RV_RAW) type XSTRING
    raising
      ZCX_PUB_CONVERT .
  class-methods RAW_TO_TEXT
    importing
      !IV_RAW type SIMPLE
      !IV_ENCODING type ABAP_ENCODING default 'DEFAULT'
    returning
      value(RV_TEXT) type STRING
    raising
      ZCX_PUB_CONVERT .
  class-methods TEXT_TO_SOLI
    importing
      !IV_TEXT type SIMPLE
    returning
      value(RT_SOLI) type SOLI_TAB .
  class-methods SOLI_TO_TEXT
    importing
      !IT_SOLI type SOLI_TAB
    returning
      value(RV_TEXT) type STRING .
  class-methods TAB_TO_SOLI
    importing
      !IT_TEXT type STRING_TABLE
    returning
      value(RT_SOLI) type SOLI_TAB .
  class-methods TEXT_TO_TAB
    importing
      !IV_TEXT type SIMPLE
    exporting
      !ET_TEXT type STANDARD TABLE .
  class-methods ITF_TO_TEXT
    importing
      !IT_ITF type TLINETAB
    returning
      value(RV_TEXT) type STRING .
  class-methods TEXT_TO_ITF
    importing
      !IV_TEXT type SIMPLE
    returning
      value(RT_ITF) type TLINETAB .
  class-methods CSV_TO_TAB
    importing
      !IT_CSV type STRINGTAB
      !IV_SEPARATOR type TBA_SEPARATOR default ';'
    exporting
      !ET_DATA type STANDARD TABLE .
  class-methods TAB_TO_CSV
    importing
      !IT_DATA type ANY TABLE
      !IV_SEPARATOR type TBA_SEPARATOR default ';'
    returning
      value(RT_CSV) type STRINGTAB .
  class-methods DATA_TO_JSON
    importing
      !IV_DATA type DATA
    returning
      value(RV_JSON) type STRING .
  class-methods JSON_TO_DATA
    importing
      !IV_JSON type SIMPLE
    exporting
      !EV_DATA type DATA .
  class-methods DATA_TO_XML
    importing
      !IV_DATA type DATA
    returning
      value(RV_XML) type STRING
    raising
      ZCX_PUB_CONVERT .
  class-methods XML_TO_DATA
    importing
      !IV_XML type SIMPLE
    exporting
      !EV_DATA type DATA
    raising
      ZCX_PUB_CONVERT .
  class-methods RAW_TO_VARCHAR
    importing
      !IV_RAW type XSTRING
    returning
      value(RV_VARCHAR) type STRING .
  class-methods RAW_TO_BASE64
    importing
      !IV_RAW type XSTRING
    returning
      value(RV_BASE64) type STRING .
  class-methods BASE64_TO_RAW
    importing
      value(IV_BASE64) type STRING
    returning
      value(RV_RAW) type XSTRING .
  class-methods TEXT_TO_BASE64
    importing
      !IV_TEXT type STRING
    returning
      value(RV_BASE64) type STRING
    raising
      ZCX_PUB_CONVERT .
  class-methods BASE64_TO_TEXT
    importing
      !IV_BASE64 type STRING
    returning
      value(RV_TEXT) type STRING
    raising
      ZCX_PUB_CONVERT .
  class-methods RAW_TO_XTAB
    importing
      !IV_RAW type XSTRING
    exporting
      !ET_DATA type STANDARD TABLE
      !EV_LENGTH type I .
  class-methods XTAB_TO_RAW
    importing
      !IT_DATA type STANDARD TABLE
      !IV_LENGTH type SIMPLE
    returning
      value(RV_RAW) type XSTRING
    raising
      ZCX_PUB_CONVERT .
  class-methods AMOUNT_TO_STRING
    importing
      !IV_AMOUNT type NUMERIC
      !IV_USER type SY-UNAME default SY-UNAME
      !IV_WAERS type WAERS optional
      !IV_DCPFM type USR01-DCPFM optional
    returning
      value(RV_STRING) type STRING .
  class-methods STRING_TO_AMOUNT
    importing
      !IV_STRING type STRING
    exporting
      !EV_AMOUNT type NUMERIC .
  class-methods UNITS
    importing
      !IV_MENGE_IN type ANY
      !IV_MEINS_IN type MEINS
      !IV_MATNR type MATNR optional
    exporting
      !EV_MENGE_OUT type ANY
    changing
      !CV_MEINS_OUT type MEINS optional
    raising
      ZCX_PUB_CONVERT .
  class-methods OTF_TO_PDF
    importing
      !IT_OTF type TSFOTF
    returning
      value(RV_PDF) type FPCONTENT
    raising
      ZCX_PUB_CONVERT .
  class-methods BOOLEAN_XML_TO_ABAP
    importing
      !IV_VALUE type SIMPLE
    returning
      value(RV_BOOL) type ABAP_BOOL
    raising
      ZCX_PUB_CONVERT .
  class-methods BOOLEAN_ABAP_TO_XML
    importing
      !IV_VALUE type SIMPLE
    returning
      value(RV_BOOL) type STRING .
  class-methods GUID_XML_TO_ABAP
    importing
      !IV_GUID type SIMPLE
    returning
      value(RV_GUID) type GUID
    raising
      ZCX_PUB_CONVERT .
  class-methods GUID_ABAP_TO_XML
    importing
      !IV_GUID type GUID
    returning
      value(RV_GUID) type STRING
    raising
      ZCX_PUB_CONVERT .
  class-methods DATETIME_XML_TO_ABAP
    importing
      !IV_DATETIME type SIMPLE
    returning
      value(RV_TIMESTAMP) type TIMESTAMP
    raising
      ZCX_PUB_CONVERT .
  class-methods DATETIME_ABAP_TO_XML
    importing
      !IV_TIMESTAMP type TIMESTAMP
      !IV_ZONE type SIMPLE optional
    returning
      value(RV_DATETIME) type STRING
    raising
      ZCX_PUB_CONVERT .
  class-methods DATE_XML_TO_ABAP
    importing
      !IV_DATE type SIMPLE
    returning
      value(RV_DATE) type D
    raising
      ZCX_PUB_CONVERT .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_PUB_CONVERT IMPLEMENTATION.


  METHOD amount_to_string.
    DATA:
      lv_dec_notation TYPE usr01-dcpfm,
      lv_char         TYPE char255.
    DATA: lv_text     TYPE dd07t-ddtext ##NEEDED.
    DATA: lt_return TYPE bapiret2_t ##NEEDED.
    DATA:
      lv_thousands TYPE c,
      lv_decimal   TYPE c.

* Читаем настройки пользователя
    IF iv_user IS NOT INITIAL.
      CALL FUNCTION '/OSP/GET_DECIMAL_NOTATION'
        EXPORTING
          i_uname             = iv_user
        IMPORTING
          ev_decimal_notation = lv_dec_notation
          ev_text             = lv_text
          et_return           = lt_return.
    ELSE.
      lv_dec_notation = iv_dcpfm.
    ENDIF.

    CASE lv_dec_notation.
      WHEN space. " 1.234.567,89
        lv_thousands = '.'.
        lv_decimal   = ','.
      WHEN 'X'. " 1,234,567.89
        lv_thousands = ','.
        lv_decimal   = '.'.
      WHEN 'Y'. " 1 234 567,89
        lv_thousands = space.
        lv_decimal   = ','.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CALL FUNCTION 'FMCA_AMOUNT_TO_STRING_CONVERT'
      EXPORTING
        betrg                   = iv_amount
        waers                   = iv_waers
        new_decimal_separator   = lv_decimal
        new_thousands_separator = lv_thousands
      IMPORTING
        string                  = lv_char.

    CONDENSE lv_char NO-GAPS.
    rv_string = lv_char.

  ENDMETHOD.


  METHOD BASE64_TO_RAW.

    IF iv_base64 IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = iv_base64
      IMPORTING
        output = rv_raw.

  ENDMETHOD.


  METHOD BASE64_TO_TEXT.

    DATA lv_raw TYPE xstring.

    IF iv_base64 IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = iv_base64
      IMPORTING
        output = lv_raw.


    rv_text = raw_to_text(
      EXPORTING
        iv_raw = lv_raw ).

  ENDMETHOD.


  METHOD BOOLEAN_ABAP_TO_XML.
    cl_gdt_conversion=>indicator_outbound(
      EXPORTING
        im_value = CONV abap_bool( iv_value )
      IMPORTING
        ex_value = rv_bool ).
  ENDMETHOD.


  METHOD BOOLEAN_XML_TO_ABAP.
    TRY .
        cl_gdt_conversion=>indicator_inbound(
          EXPORTING
            im_value = iv_value
          IMPORTING
            ex_value = rv_bool ).
      CATCH cx_gdt_conversion INTO DATA(lo_gdt_conversion).
        MESSAGE i000 INTO DATA(lv_dummy) ##NEEDED.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_gdt_conversion.
    ENDTRY.

  ENDMETHOD.


  METHOD CHAR_TO_FLTP.

    CALL FUNCTION 'CHAR_FLTP_CONVERSION'
      EXPORTING
        string                   = iv_char
      IMPORTING
        flstr                    = rv_fltp
      EXCEPTIONS
        exponent_too_big         = 1
        exponent_too_small       = 2
        string_not_fltp          = 3
        too_many_decim           = 4
        OTHERS                   = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE.
    ENDIF.

  ENDMETHOD.


  METHOD CODEPAGE_EXT_TO_INT.

    CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
      EXPORTING
        external_name      = iv_codepage_ext
     IMPORTING
       sap_codepage        = rv_codepage_int
     EXCEPTIONS
       not_found           = 1
       OTHERS              = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE.
    ENDIF.

  ENDMETHOD.


  METHOD CONV_EXIT_INPUT.

    CLEAR ev_target.

    DATA(lv_found) = abap_true.

    DESCRIBE FIELD ev_target TYPE DATA(lv_type)
                             EDIT MASK DATA(lv_conv_exit).

    IF iv_conv_exit IS NOT INITIAL.
      lv_conv_exit = iv_conv_exit.
    ENDIF.

    DO 1 TIMES.
      IF lv_conv_exit IS INITIAL.
        lv_found = abap_false.
        EXIT.
      ENDIF.

      REPLACE '==' IN lv_conv_exit WITH ''.
      IF lv_conv_exit = 'ALPHA' AND
         lv_type <> cl_abap_datadescr=>typekind_string.

        DATA(lv_source_len) = strlen( iv_source ).
        DESCRIBE FIELD ev_target LENGTH DATA(lv_target_len) IN CHARACTER MODE.

        IF lv_source_len > lv_target_len.
          MESSAGE e001 WITH lv_target_len lv_source_len INTO DATA(lv_dummy) ##NEEDED.
          RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE.
        ENDIF.
      ENDIF.

      DATA(lv_func_name) = |CONVERSION_EXIT_{ lv_conv_exit }_INPUT|.


      CALL FUNCTION 'RH_FUNCTION_EXIST'
        EXPORTING
          name = CONV funcname( lv_func_name )
        EXCEPTIONS
          function_not_found = 1
          OTHERS             = 2.

      IF sy-subrc <> 0.
        lv_found = abap_false.
        EXIT.
      ENDIF.

      TRY .
          CALL FUNCTION lv_func_name
            EXPORTING
              input         = iv_source
            IMPORTING
              output        = ev_target
            EXCEPTIONS
              error_message = 4
              OTHERS        = 8.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE.
          ENDIF.
        CATCH cx_root INTO DATA(lr_error) ##CATCH_ALL.
          MESSAGE i000 INTO lv_dummy.
          RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lr_error.
      ENDTRY.
    ENDDO.


    IF lv_found = abap_false.
      IF iv_conv_sap = abap_false AND
         ( lv_type = cl_abap_datadescr=>typekind_char OR
         lv_type = cl_abap_datadescr=>typekind_date OR
         lv_type = cl_abap_datadescr=>typekind_time OR
         lv_type = cl_abap_datadescr=>typekind_num ).

        lv_source_len = strlen( iv_source ).
        DESCRIBE FIELD ev_target LENGTH lv_target_len IN CHARACTER MODE.

        IF lv_source_len > lv_target_len.
          MESSAGE e001 WITH lv_target_len lv_source_len INTO lv_dummy.
          RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE.
        ENDIF.
      ENDIF.

      TRY.

          IF iv_conv_sap = abap_true.
            DATA lv_subrc TYPE sy-subrc.
            PERFORM input_data2sap_data IN PROGRAM sapltrux IF FOUND
              USING iv_source
              CHANGING ev_target lv_subrc.
            IF lv_subrc <> 0.
              RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE.
            ENDIF.
          ELSE.
            ev_target = iv_source.
          ENDIF.
        CATCH cx_root INTO lr_error ##CATCH_ALL.
          MESSAGE i000 INTO lv_dummy.
          RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lr_error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD CONV_EXIT_OUTPUT.
    CLEAR ev_target.

    DATA(lv_found) = abap_true.

    DESCRIBE FIELD iv_source EDIT MASK DATA(lv_conv_exit).

    IF iv_conv_exit IS NOT INITIAL.
      lv_conv_exit = iv_conv_exit.
    ENDIF.

    DO 1 TIMES.
      IF lv_conv_exit IS INITIAL.
        lv_found = abap_false.
        EXIT.
      ENDIF.

      REPLACE '==' IN lv_conv_exit WITH ''.

      DATA(lv_func_name) = |CONVERSION_EXIT_{ lv_conv_exit }_OUTPUT|.

      CALL FUNCTION 'RH_FUNCTION_EXIST'
        EXPORTING
          name = CONV funcname( lv_func_name )
        EXCEPTIONS
          function_not_found = 1
          OTHERS             = 2.

      IF sy-subrc <> 0.
        lv_found = abap_false.
        EXIT.
      ENDIF.

      TRY .
          CALL FUNCTION lv_func_name
            EXPORTING
              input         = iv_source
            IMPORTING
              output        = ev_target
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE.
          ENDIF.
        CATCH cx_root INTO DATA(lr_error) ##CATCH_ALL.
          MESSAGE i000 INTO DATA(lv_dummy) ##NEEDED.
          RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lr_error.
      ENDTRY.
    ENDDO.

    IF lv_found = abap_false.
      DESCRIBE FIELD ev_target TYPE DATA(lv_type).
      IF lv_type = cl_abap_datadescr=>typekind_char OR
         lv_type = cl_abap_datadescr=>typekind_date OR
         lv_type = cl_abap_datadescr=>typekind_time OR
         lv_type = cl_abap_datadescr=>typekind_num.

        DATA(lv_source_len) = strlen( iv_source ).
        DESCRIBE FIELD ev_target LENGTH DATA(lv_target_len) IN CHARACTER MODE.

        IF lv_source_len > lv_target_len.
          MESSAGE e001 WITH lv_target_len lv_source_len INTO lv_dummy.
          RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE.
        ENDIF.
      ENDIF.

      TRY.
          ev_target = iv_source.
        CATCH cx_root INTO lr_error ##CATCH_ALL.
          MESSAGE i000 INTO lv_dummy.
          RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lr_error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD CSV_TO_TAB.

    DATA lv_csv_str TYPE c LENGTH 65535.

    CLEAR et_data.

    DATA(lo_converter) = cl_rsda_csv_converter=>create( i_separator = iv_separator ).

    DATA:
      lo_tabledescr TYPE REF TO cl_abap_tabledescr,
      lo_structdescr TYPE REF TO cl_abap_structdescr.

    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data( et_data ).
    lo_structdescr ?= lo_tabledescr->get_table_line_type( ).

    LOOP AT it_csv ASSIGNING FIELD-SYMBOL(<ls_csv>).
      lv_csv_str = <ls_csv>.
      APPEND INITIAL LINE TO et_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      lo_converter->csv_to_structure(
        EXPORTING
          i_data   = lv_csv_str
        IMPORTING
          e_s_data = <ls_data> ).

      LOOP AT lo_structdescr->components INTO DATA(ls_component) "#EC CI_NESTED
        WHERE type_kind = cl_abap_typedescr=>typekind_date. "#EC CI_STDSEQ

        ASSIGN COMPONENT ls_component-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_value>).
        CHECK sy-subrc = 0.

        IF <lv_value> = '0'.
          CLEAR <lv_value>. "Заполняем нулевые даты правильным abap значением 00000000
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD DATA_TO_JSON.

    rv_json = /ui2/cl_json=>serialize( data = iv_data  ).

  ENDMETHOD.


  METHOD DATA_TO_XML.

    TRY.
        CALL TRANSFORMATION id
          SOURCE data = iv_data
          RESULT XML rv_xml.
      CATCH cx_dynamic_check INTO DATA(lo_dynamic_check).
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT EXPORTING previous = lo_dynamic_check.
    ENDTRY.

  ENDMETHOD.


  METHOD DATETIME_ABAP_TO_XML.

    IF iv_timestamp IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_gdt_conversion=>date_time_outbound(
          EXPORTING
            im_value_short = iv_timestamp
            im_timezone    = CONV #( iv_zone )
          IMPORTING
            ex_value       = rv_datetime ).

      CATCH cx_gdt_conversion INTO DATA(lo_gdt_conversion).
        MESSAGE i000 INTO DATA(lv_dummy) ##NEEDED.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_gdt_conversion.
    ENDTRY.
  ENDMETHOD.


  METHOD DATETIME_XML_TO_ABAP.

    IF iv_datetime IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
      cl_gdt_conversion=>date_time_inbound(
        EXPORTING
          im_value       = iv_datetime
        IMPORTING
          ex_value_short = rv_timestamp ).

      CATCH cx_gdt_conversion INTO DATA(lo_gdt_conversion).
        MESSAGE i000 INTO DATA(lv_dummy) ##NEEDED.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_gdt_conversion.
    ENDTRY.

  ENDMETHOD.


  METHOD date_ext_to_int.

    DATA:
      lv_year_c  TYPE c LENGTH 4,
      lv_month_c TYPE c LENGTH 2,
      lv_day_c   TYPE c LENGTH 2,

      lv_year_n  TYPE n LENGTH 4,
      lv_month_n TYPE n LENGTH 2,
      lv_day_n   TYPE n LENGTH 2.

    IF iv_date IS INITIAL.
      RETURN.
    ENDIF.

    FIND '/' IN iv_date.
    IF sy-subrc = 0.
      SPLIT iv_date AT '/' INTO lv_month_c lv_day_c lv_year_c.
      lv_day_n = lv_day_c.
      lv_month_n = lv_month_c.
      lv_year_n = lv_year_c.
      rv_date = |{ lv_year_n }{ lv_month_n }{ lv_day_n }|.
    ELSE.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = iv_date
          accept_initial_date      = abap_true
        IMPORTING
          date_internal            = rv_date
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_pub_convert USING MESSAGE .
      ENDIF.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = rv_date
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_pub_convert USING MESSAGE .
    ENDIF.

  ENDMETHOD.


  METHOD DATE_INT_TO_EXT.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal                  = iv_date
      IMPORTING
        date_external                  = rv_date
      EXCEPTIONS
        date_internal_is_invalid       = 1
        OTHERS                         = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE .
    ENDIF.

  ENDMETHOD.


  METHOD DATE_XML_TO_ABAP.
    IF iv_date IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_date(10).
    lv_date = iv_date.

    REPLACE ALL OCCURRENCES OF '-' IN lv_date WITH ``.

    rv_date = lv_date.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                           = rv_date
     EXCEPTIONS
       plausibility_check_failed       = 1
       OTHERS                          = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE .
    ENDIF.

  ENDMETHOD.


  METHOD DEC_TO_HEX.
    DATA lv_n TYPE trex_rfc-request_flag_l.

    lv_n = iv_dec.
    rv_hex = cl_trex_utility=>conv_dec_to_hex( lv_n ).
  ENDMETHOD.


  METHOD FLTP_TO_CHAR.

    DATA lv_char TYPE char22.

    CALL FUNCTION 'FLTP_CHAR_CONVERSION'
      EXPORTING
        input = iv_fltp
      IMPORTING
        flstr = lv_char.

    CONDENSE lv_char NO-GAPS.

    rv_char = lv_char.

  ENDMETHOD.


  METHOD FLTP_TO_DATE.
    DATA lv_pack(16) TYPE p.
    FIELD-SYMBOLS <lv_p> TYPE any.

    ASSIGN lv_pack TO <lv_p>.

    <lv_p> = iv_fltp.
    UNPACK <lv_p> TO rv_date.
  ENDMETHOD.


  METHOD GUID_ABAP_TO_XML.

    TRY .
        cl_gdt_conversion=>guid_outbound(
          EXPORTING
            im_guid_x = iv_guid
          IMPORTING
            ex_value  = rv_guid ).
      CATCH cx_gdt_conversion INTO DATA(lo_gdt_conversion).
        MESSAGE i000 INTO DATA(lv_dummy) ##NEEDED.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_gdt_conversion.
    ENDTRY.

  ENDMETHOD.


  METHOD GUID_XML_TO_ABAP.
    IF iv_guid IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_gdt_conversion=>guid_inbound(
          EXPORTING
            im_value  = iv_guid
          IMPORTING
            ex_guid_x = rv_guid ).
      CATCH cx_gdt_conversion INTO DATA(lo_gdt_conversion).
        MESSAGE i000 INTO DATA(lv_dummy) ##NEEDED.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_gdt_conversion.
    ENDTRY.

  ENDMETHOD.


  METHOD HEX_TO_DEC.
    rv_dec = cl_trex_utility=>conv_hex_to_dec( iv_hex ).
  ENDMETHOD.


  METHOD ITF_TO_TEXT.

    DATA lt_stream TYPE string_table.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        lf           = 'X'
      IMPORTING
        stream_lines = lt_stream
      TABLES
        itf_text     = it_itf.

    LOOP AT lt_stream ASSIGNING FIELD-SYMBOL(<lv_stream>).
      IF sy-tabix = 1.
        rv_text = <lv_stream>.
      ELSE.
        CONCATENATE rv_text <lv_stream> INTO rv_text
          SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD JSON_TO_DATA.

    CLEAR ev_data.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = iv_json
      CHANGING
        data = ev_data ).

  ENDMETHOD.


  METHOD otf_to_pdf.
    DATA: lt_lines        TYPE TABLE OF tline.
    DATA: lv_bin_filesize TYPE int4 ##NEEDED.

    CHECK it_otf IS NOT INITIAL.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = lv_bin_filesize
        bin_file              = rv_pdf
      TABLES
        otf                   = it_otf
        lines                 = lt_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_pub_convert MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD RAW_TO_BASE64.

    IF iv_raw IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = iv_raw
      IMPORTING
        output = rv_base64.

  ENDMETHOD.


  METHOD RAW_TO_TEXT.

    IF iv_raw IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_raw) = CONV xstring( iv_raw ).

    TRY.
        DATA(lo_conv) = cl_abap_conv_in_ce=>create(
          EXPORTING
            input       = lv_raw
            encoding    = iv_encoding
            ignore_cerr = abap_true ).
      CATCH cx_dynamic_check INTO DATA(lo_dynamic_check).
        MESSAGE i000 INTO DATA(lv_dummy) ##NEEDED.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_dynamic_check.
    ENDTRY.

    TRY.
        lo_conv->read(
          IMPORTING
            data = rv_text ).
      CATCH cx_dynamic_check INTO lo_dynamic_check.
        MESSAGE i000 INTO lv_dummy.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_dynamic_check.
    ENDTRY.


  ENDMETHOD.


  METHOD RAW_TO_VARCHAR.

    DATA lt_data TYPE tsfixml.

    raw_to_xtab(
      EXPORTING
        iv_raw  = iv_raw
      IMPORTING
        et_data = lt_data ).

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lv_data>).
      DATA(lv_string) = CONV string( <lv_data> ).
      CONCATENATE rv_varchar lv_string INTO rv_varchar.
    ENDLOOP.

    DATA(lv_length) = xstrlen( iv_raw ) * 2.

    rv_varchar = '0x' && rv_varchar(lv_length).

  ENDMETHOD.


  METHOD RAW_TO_XTAB.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = iv_raw
      IMPORTING
        output_length = ev_length
      TABLES
        binary_tab = et_data.

  ENDMETHOD.


  METHOD SOLI_TO_TEXT.

    IF it_soli IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SO_TAB_TO_STRING'
     IMPORTING
       content_str       = rv_text
      TABLES
        content_tab       = it_soli.

  ENDMETHOD.


  METHOD STRING_TO_AMOUNT.
    STATICS ls_usr01 TYPE usr01.

    DATA:
      lv_thousands TYPE char1,
      lv_decimal   TYPE char1.

*   Get separator from user record
    IF ls_usr01 IS INITIAL.
      SELECT SINGLE *
        INTO ls_usr01
        FROM usr01
        WHERE bname EQ sy-uname.
    ENDIF.

    CASE ls_usr01-dcpfm.
      WHEN space.  " 1.234.567,89
        lv_thousands = '.'.
        lv_decimal   = ','.
      WHEN 'X'.    " 1,234,567.89
        lv_thousands = ','.
        lv_decimal   = '.'.
      WHEN 'Y'.    " 1 234 567,89
        lv_thousands = space.
        lv_decimal   = ','.
    ENDCASE.

    CALL FUNCTION 'HRCM_STRING_TO_AMOUNT_CONVERT'
      EXPORTING
        string                    = iv_string
        decimal_separator         = lv_decimal
        thousands_separator       = lv_thousands
      IMPORTING
        betrg                     =  ev_amount
      EXCEPTIONS
        convert_error             = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD TAB_TO_CSV.

    DATA lv_data_str TYPE c LENGTH 65535.

    DATA(lo_converter) = cl_rsda_csv_converter=>create( i_separator = iv_separator ).

    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      lo_converter->structure_to_csv(
        EXPORTING
          i_s_data = <ls_data>
        IMPORTING
          e_data   = lv_data_str ).

      DATA(lv_data) = CONV string( lv_data_str ).
      INSERT lv_data INTO TABLE rt_csv.

    ENDLOOP.

  ENDMETHOD.


  METHOD TAB_TO_SOLI.
    DATA lt_soli TYPE soli_tab.
    LOOP AT it_text ASSIGNING FIELD-SYMBOL(<lv_text>).
      CLEAR lt_soli.
      IF <lv_text> IS NOT INITIAL.
        CALL FUNCTION 'SO_STRING_TO_TAB'
          EXPORTING
            content_str = <lv_text>
          TABLES
            content_tab = lt_soli.
        INSERT LINES OF lt_soli INTO TABLE rt_soli.
      ELSE.
        INSERT INITIAL LINE INTO TABLE rt_soli.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD TEXT_TO_BASE64.

    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
      DATA(lv_raw) = text_to_raw(
        EXPORTING
          iv_text     = iv_text ).

      CATCH ZCX_PUB_CONVERT ##NO_HANDLER.
    ENDTRY.

    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = lv_raw
      IMPORTING
        output = rv_base64.

  ENDMETHOD.


  METHOD TEXT_TO_ITF.

    DATA lt_stream TYPE string_table.

    DATA(lv_text) = CONV string( iv_text ).

    INSERT lv_text INTO TABLE lt_stream.

    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        stream_lines = lt_stream
        lf           = 'X'
      TABLES
        itf_text     = rt_itf.

  ENDMETHOD.


  METHOD TEXT_TO_RAW.

    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_text) = CONV string( iv_text ).

    DATA(lv_length) = strlen( lv_text ).

    TRY .
        DATA(lo_conv) =
          cl_abap_conv_out_ce=>create(
            encoding    = iv_encoding
            ignore_cerr = abap_true ).
      CATCH cx_dynamic_check INTO DATA(lo_dynamic_check).
        MESSAGE i000 INTO DATA(lv_dummy) ##NEEDED.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_dynamic_check.
    ENDTRY.

    TRY .
      lo_conv->write(
        data = lv_text
        n    = lv_length ).
      CATCH cx_dynamic_check INTO lo_dynamic_check.
        MESSAGE i000 INTO lv_dummy.
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT USING MESSAGE EXPORTING previous = lo_dynamic_check.
    ENDTRY.

    rv_raw = lo_conv->get_buffer( ).
  ENDMETHOD.


  METHOD TEXT_TO_SOLI.

    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_text) = CONV string( iv_text ).

    CALL FUNCTION 'SO_STRING_TO_TAB'
      EXPORTING
        content_str       = lv_text
      TABLES
        content_tab       = rt_soli.

  ENDMETHOD.


  METHOD text_to_tab.
    CLEAR et_text[].

    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_text) = CONV string( iv_text ).

    DO.
      INSERT INITIAL LINE INTO TABLE et_text ASSIGNING FIELD-SYMBOL(<lv_text>).
      <lv_text> = lv_text.
      DATA(lv_length) = strlen( <lv_text> ).
      IF strlen( lv_text ) LE lv_length.
        EXIT.
      ENDIF.
      lv_text = lv_text+lv_length.
    ENDDO.

  ENDMETHOD.


  METHOD units.

    DATA:
      lv_menge_in  TYPE ekpo-menge,
      lv_menge_out TYPE ekpo-menge,
      lv_meins_out TYPE meins.

    IF cv_meins_out IS INITIAL.
      CALL FUNCTION 'SI_UNIT_GET'
        EXPORTING
          unit                = iv_meins_in
        IMPORTING
          si_unit             = cv_meins_out
        EXCEPTIONS
          dimension_not_found = 1
          unit_not_found      = 2
          OTHERS              = 3.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_pub_convert MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    IF iv_matnr IS INITIAL.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = iv_menge_in
          unit_in              = iv_meins_in
          unit_out             = cv_meins_out
        IMPORTING
          output               = ev_menge_out
        EXCEPTIONS
          conversion_not_found = 1
          division_by_zero     = 2
          input_invalid        = 3
          output_invalid       = 4
          overflow             = 5
          type_invalid         = 6
          units_missing        = 7
          unit_in_not_found    = 8
          unit_out_not_found   = 9
          OTHERS               = 10.
      IF sy-subrc <> 0 ##NEEDED.
*   Implement suitable error handling here
      ENDIF.
    ELSE.
      lv_menge_in = iv_menge_in.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr  = iv_matnr
          i_in_me  = iv_meins_in
          i_out_me = lv_meins_out
          i_menge  = lv_menge_in
        IMPORTING
          e_menge  = lv_menge_out
        EXCEPTIONS
*         error_in_application = 1
*         error    = 2
          OTHERS   = 0.

      ev_menge_out = lv_menge_out.
    ENDIF.

  ENDMETHOD.


  METHOD XML_TO_DATA.

    CLEAR ev_data.

    IF iv_xml IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML iv_xml
          RESULT data = ev_data.
      CATCH cx_root INTO DATA(lo_root).                  "#EC CATCH_ALL
        RAISE EXCEPTION TYPE ZCX_PUB_CONVERT EXPORTING previous = lo_root.
    ENDTRY.

  ENDMETHOD.


  METHOD XTAB_TO_RAW.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = CONV i( iv_length )
      IMPORTING
        buffer       = rv_raw
      TABLES
        binary_tab   = it_data
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_PUB_CONVERT MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
