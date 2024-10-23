# PO
Purchase Order Integration

METHOD if_rest_resource~get.
*************************************************************************
*                                                                       *
*    Class for Extraction of Purchase Order  details From SAP.          *                                                                  *
*    Class name : ZCL_PURCHASE_ORDER_PROVIDER                           *                                    *
*    DATE CREATED         : 30.05.2023                                  *
*                                                                       *
*************************************************************************
*-----------------------------------Structure for CDHDR tables values----------------------------
  TYPES : BEGIN OF ty_cdhdr,
            objectclas(15) TYPE c,
            objectid(90)   TYPE c,
            changenr       TYPE cdchangenr,
            udate          TYPE cddatum,
            change_ind(8)  TYPE c,
          END OF ty_cdhdr.
*-----------------------------------Structure for EKKO tables values----------------------------
  TYPES : BEGIN OF ty_ekko,
            ebeln              TYPE ekko-ebeln,
            aedat              TYPE ekko-aedat,
            lifnr              TYPE ekko-lifnr,
            waers              TYPE ekko-waers,
            unsez              TYPE ekko-unsez,
            loekz              TYPE ekko-loekz,
            bstyp              TYPE ekko-bstyp,
            lastchangedatetime TYPE ekko-lastchangedatetime,
          END OF ty_ekko.
*-----------------------------------Structure for EKPO tables values----------------------------
  TYPES : BEGIN OF ty_ekpo,
            ebeln TYPE ebeln,
            werks TYPE werks,
            loekz TYPE loekz,
            aedat TYPE ekpo-aedat,
            matnr TYPE matnr,
            meins TYPE bstme,
            menge TYPE ekpo-menge,
            netwr TYPE ekpo-netwr,
            ebelp TYPE ebelp,
            bstyp TYPE bstyp,

          END OF ty_ekpo.

  TYPES : BEGIN OF ty_ekpo1,
            ebeln TYPE c LENGTH 90,
            werks TYPE werks,
            loekz TYPE loekz,
            aedat TYPE ekpo-aedat,
            matnr TYPE matnr,
            meins TYPE bstme,
            menge TYPE ekpo-menge,
            netwr TYPE ekpo-netwr,
            ebelp TYPE ebelp,
            bstyp TYPE bstyp,
          END OF ty_ekpo1.
**-----------------------------------Structure for EKET tables values----------------------------
  TYPES:BEGIN OF ty_eket,
          ebeln TYPE eket-ebeln,
          ebelp TYPE eket-ebelp,
          etenr TYPE eket-etenr,
          eindt TYPE eket-eindt,
          menge TYPE eket-menge,
          wemng TYPE eket-menge,
        END OF ty_eket.
*-----------------------------------Structure for LFA1 tables values----------------------------
  TYPES : BEGIN OF ty_lfa1,
            lifnr TYPE lifnr,
            name1 TYPE name1,
          END OF ty_lfa1.
*-----------------------------------Structure for MAKT tables values----------------------------
  TYPES : BEGIN OF ty_makt,
            matnr       TYPE makt-matnr,
            description TYPE makt-maktx,
          END OF ty_makt.
*-----------------------------------Structure for EKBE tables values----------------------------
  TYPES: BEGIN OF ty_ekbe,
           ebeln TYPE ekbe-ebeln,
           ebelp TYPE ekbe-ebelp,
           bwart TYPE ekbe-bwart,
           bpwes TYPE ekbe-bpwes,
           menge TYPE ekbe-menge,
         END OF ty_ekbe.
*-----------------------------------Structure for Final table----------------------------
  TYPES : BEGIN OF ty_final,
            recordtype(8)   TYPE c,
            pono            TYPE ekko-ebeln,
            podate          TYPE ekko-aedat,
            vendorcode      TYPE ekko-lifnr,
            currency        TYPE ekko-waers,
            modifiedon_h    TYPE sy-datum,
            vendorname      TYPE lfa1-name1,
            refno           TYPE ekko-unsez,
            branchid        TYPE ekpo-werks,
            is_deleted_h(3) TYPE c,
            modifiedon_i    TYPE ekpo-aedat,
            itemcode        TYPE ekpo-matnr,
            uom             TYPE bstme,
            qty             TYPE ekpo-menge,
            rate            TYPE ekpo-netwr,
            remarks         TYPE tdline,
            poitemlineno    TYPE ekpo-ebelp,
            description     TYPE makt-maktx,
            duedate         TYPE eket-eindt,
            is_deleted_i(3) TYPE c,
            schedule(3)     TYPE c,
            gateentry(20)   TYPE c,
            grn(20)         TYPE c,
          END OF ty_final.
**------------------------------Types and Data declaration for extract header text---------------------------------------------
  TYPES: BEGIN OF ty_stxl,
           relid  TYPE stxl-relid,
           tdname TYPE stxl-tdname,
           clustr TYPE stxl-clustr,
           clustd TYPE stxl-clustd,
         END OF ty_stxl.
  DATA:  t_stxl TYPE STANDARD TABLE OF ty_stxl.
  FIELD-SYMBOLS: <stxl> TYPE ty_stxl.
* compressed text data without text name
  TYPES: BEGIN OF ty_stxl_raw,
           clustr TYPE stxl-clustr,
           clustd TYPE stxl-clustd,
         END OF ty_stxl_raw.
  DATA:  t_stxl_raw TYPE STANDARD TABLE OF ty_stxl_raw.
  DATA:  w_stxl_raw TYPE ty_stxl_raw.

  TYPES : BEGIN OF ty,
            tdname TYPE stxl-tdname,
          END OF ty.

  TYPES : BEGIN OF ty_header,
            ebeln   TYPE stxl-tdname,
            remarks TYPE tline-tdline,
          END OF ty_header.
  DATA : gt_header TYPE TABLE OF ty_header,
         gs_header TYPE ty_header.
* decompressed text
  DATA:  t_tline TYPE STANDARD TABLE OF tline.
  FIELD-SYMBOLS: <tline> TYPE tline.

  TYPES : BEGIN OF ts_stxh,
            tdobject TYPE stxh-tdobject,
            tdname   TYPE stxh-tdname,
            tdid     TYPE tdid,
            tdspras  TYPE stxh-tdspras,
          END OF ts_stxh.

  DATA: t_stxh TYPE STANDARD TABLE OF ts_stxh,
        w_stxh TYPE ts_stxh.
*-----------------------------------Data Declaration for Internal tables  and Work area----------------------------

      TYPES : BEGIN OF ty_main,
                ebeln   TYPE ebeln,
                ebelp   TYPE ebelp,
                etenr   TYPE etenr,
                chngnr  TYPE cdchangenr,
                qty     TYPE ktmng,
                tabname TYPE tabname,
                chngind TYPE cdchngind,
              END OF ty_main.
*
      DATA : ls_main TYPE ty_main,
             lt_main TYPE TABLE OF ty_main.
  DATA :gt_cdhdr TYPE TABLE OF ty_cdhdr,
        gs_cdhdr TYPE ty_cdhdr.

  DATA : gt_ekko TYPE TABLE OF ty_ekko,
         gs_ekko TYPE ty_ekko.

  DATA : gt_ekpo TYPE TABLE OF ty_ekpo,
         gs_ekpo TYPE ty_ekpo.
  DATA : gt_ekpo1 TYPE TABLE OF ty_ekpo1,
         gs_ekpo1 TYPE ty_ekpo1.

  DATA : gt_lfa1 TYPE TABLE OF ty_lfa1,
         gs_lfa1 TYPE ty_lfa1.

  DATA : gt_eket TYPE TABLE OF ty_eket,
         gs_eket TYPE ty_eket.
  DATA : gt_eket_n TYPE TABLE OF ty_eket,
         gs_eket_n TYPE ty_eket.

  DATA : gt_makt TYPE TABLE OF ty_makt,
         gs_makt TYPE ty_makt.

  DATA : gt_final TYPE TABLE OF ty_final,
         gs_final TYPE ty_final,
         ls_json  TYPE string.

  DATA : lv_date TYPE sy-datum.
  DATA : lv_dt     TYPE string.
  DATA:gt_ekbe TYPE TABLE OF ty_ekbe,
       gs_ekbe TYPE ty_ekbe.

  DATA : lv_104ge  TYPE c LENGTH 20,
         lv_cal1ge TYPE c LENGTH 20,
         lv_cal2ge TYPE c LENGTH 20,
         lv_cal3ge TYPE c LENGTH 20,
         lv_103ge  TYPE c LENGTH 20.
  DATA : lv_105grn TYPE c LENGTH 20,
         lv_p1grn  TYPE c LENGTH 20,
         lv_p2grn  TYPE c LENGTH 20,
         lv_p3grn  TYPE c LENGTH 20,
         lv_106grn TYPE c LENGTH 20.
*----------variable for get data from user ------------------
  DATA(l_value) = mo_request->get_uri_query_parameter( 'AEDAT' ).
  DATA(l_value1) = mo_request->get_uri_query_parameter( 'DATE' ).
  DATA : lv TYPE ekko-lastchangedatetime.
  DATA : lv1 TYPE ekko-lastchangedatetime.

*----------------------Delta Load--------------------------------
  IF l_value IS NOT INITIAL.
    SELECT ebeln
           werks
           loekz
           aedat
           matnr
           meins
           menge
           netwr
           ebelp
           bstyp
           FROM ekpo
           INTO TABLE gt_ekpo WHERE aedat = l_value AND ( bstyp = 'F' OR bstyp = 'L' ) AND pstyp NE 3.


    SELECT objectclas,
     objectid,
     changenr,
     udate,
     change_ind,
     tcode FROM cdhdr INTO TABLE @DATA(lt_cdhdr) WHERE  objectclas = 'EINKBELEG' AND udate = @l_value.
    SORT lt_cdhdr BY changenr DESCENDING.

    IF lt_cdhdr[] IS NOT INITIAL.

      SELECT objectclas,
             objectid,
             changenr,
             chngind,
             tabkey,
             tabname,
             fname,
             value_new,
             value_old  FROM cdpos INTO TABLE @DATA(lt_cdpos) FOR ALL ENTRIES IN @lt_cdhdr WHERE changenr = @lt_cdhdr-changenr.

      LOOP AT   lt_cdpos INTO DATA(ls_cdpos).
        CLEAR ls_main.
        DATA(lv_string)  = ls_cdpos-tabkey.
        ls_main-ebeln = lv_string+3(11).   "55000000000
        ls_main-ebelp = lv_string+14(4).      "0020
        if ls_main-ebelp is INITIAL.
          ls_main-tabname = ls_cdpos-tabname.
        ENDIF.
        ls_main-chngind = ls_cdpos-chngind.
            APPEND ls_main TO lt_main.

        CLEAR gs_ekpo.

      ENDLOOP.
    ENDIF.
  ENDIF.
  IF l_value1 IS NOT INITIAL.
**-----------------------------------Full Load -------------------------------------
    SELECT ebeln
           werks
           loekz
           aedat
           matnr
           meins
           menge
           netwr
           ebelp
           bstyp
           FROM ekpo
           INTO TABLE gt_ekpo WHERE aedat GE l_value1 AND ( bstyp = 'F' OR bstyp = 'L') AND pstyp NE 3 .
  ENDIF.

  SORT gt_ekpo BY ebeln ebelp.
  CHECK gt_ekpo[] IS NOT INITIAL.
  SELECT ebeln
         aedat
         lifnr
         waers
         unsez
         loekz
         bstyp
         lastchangedatetime
         FROM ekko
         INTO TABLE gt_ekko
       FOR ALL ENTRIES IN gt_ekpo
       WHERE ebeln = gt_ekpo-ebeln.
  SORT gt_ekko BY ebeln.
* ------------------------Fetch data from EKET table for schedule date ----------------------
  IF gt_ekpo IS NOT INITIAL.
    SELECT ebeln
           ebelp
           etenr
           eindt
           menge
           wemng
           FROM eket INTO TABLE gt_eket FOR ALL ENTRIES IN gt_ekpo
      WHERE ebeln = gt_ekpo-ebeln AND ebelp = gt_ekpo-ebelp.

    IF gt_eket IS NOT INITIAL.
      SORT gt_eket BY ebeln ebelp etenr.
      LOOP AT gt_eket INTO gs_eket.
        IF gs_eket-menge NE gs_eket-wemng.
          gs_eket_n-ebeln = gs_eket-ebeln.
          gs_eket_n-ebelp = gs_eket-ebelp.
          gs_eket_n-etenr = gs_eket-etenr.
          gs_eket_n-eindt = gs_eket-eindt.
          gs_eket_n-menge = gs_eket-menge.
          gs_eket_n-wemng = gs_eket-wemng.

          APPEND gs_eket_n TO gt_eket_n.
        ENDIF.
        CLEAR gs_eket.
        CLEAR gs_eket_n.

      ENDLOOP.
    ENDIF.
    SORT gt_eket_n BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM gt_eket_n COMPARING ebeln ebelp.

    REFRESH gt_ekpo.
* ------------------------Fetch data from EKPO table based on the EKET ebeln values ----------------------
    IF gt_eket_n IS NOT INITIAL.
      SELECT ebeln
             werks
             loekz
             aedat
             matnr
             meins
             menge
             netwr
             ebelp
             bstyp
             FROM ekpo
             INTO TABLE gt_ekpo
             FOR ALL ENTRIES IN gt_eket_n
             WHERE ebeln = gt_eket_n-ebeln
             AND   ebelp = gt_eket_n-ebelp.

      SORT gt_ekpo BY ebeln ebelp.
    ENDIF.

    IF l_value IS NOT INITIAL.
      MOVE-CORRESPONDING gt_ekpo TO gt_ekpo1.
      SORT gt_ekpo1 BY ebeln.
      CHECK gt_ekpo1[] IS NOT INITIAL.
* -------------------------To change the data type ----------------------------------------
      SELECT objectclas
        objectid
        changenr
        udate
        change_ind
        FROM cdhdr
        INTO TABLE gt_cdhdr
        FOR ALL ENTRIES IN gt_ekpo1
        WHERE  objectclas = 'EINKBELEG'
        AND objectid = gt_ekpo1-ebeln
        AND udate = l_value.

    ENDIF.
*------------------------Fetch data from LFA1 table for vendorname ------------------------
    IF gt_ekko IS NOT INITIAL.
      SELECT lifnr
             name1
             FROM lfa1 INTO TABLE gt_lfa1 FOR ALL ENTRIES IN gt_ekko
        WHERE lifnr = gt_ekko-lifnr.

      SORT gt_lfa1 BY lifnr.
    ENDIF.
*    *------------------fetch data from MAKT table for Material Description -----------------
    IF gt_ekpo IS NOT INITIAL.
      SELECT matnr
             maktx
             FROM makt INTO TABLE gt_makt FOR ALL ENTRIES IN gt_ekpo
        WHERE matnr = gt_ekpo-matnr AND spras = 'E'.

      SORT gt_makt BY matnr.
      SELECT ebeln
             ebelp
             bwart
             bpwes
             menge
             FROM ekbe INTO TABLE gt_ekbe FOR ALL ENTRIES IN gt_ekpo
             WHERE ebeln = gt_ekpo-ebeln
             AND ebelp   = gt_ekpo-ebelp
              AND ( bwart   = '103' OR bwart   = '104' OR bwart   = '105' OR bwart   = '106' ).

      SORT  gt_ekbe  BY ebeln ebelp bwart.

    ENDIF.
  ENDIF.
*------------------------------------Get the header text-----------------------------
  SELECT  tdobject tdname tdid tdspras
            FROM stxh
            INTO TABLE t_stxh.
  SORT t_stxh BY tdobject tdname tdid tdspras.
  CHECK t_stxh IS NOT INITIAL.
  SELECT relid tdname clustr clustd
            INTO TABLE t_stxl
            FROM stxl
            FOR ALL ENTRIES IN t_stxh "WITH APPLICATION DATA AND TDNAME
          WHERE relid    = 'TX'          "standard text
        AND tdobject = 'EKKO'
        AND tdname   = t_stxh-tdname
        AND ( tdid     = 'F01' OR tdid = 'L01' )
        AND tdspras  = 'E'.

  LOOP AT t_stxl ASSIGNING <stxl>.
    CLEAR: t_stxl_raw[], t_tline[].
    gs_header-ebeln = <stxl>-tdname.
    w_stxl_raw-clustr = <stxl>-clustr.
    w_stxl_raw-clustd = <stxl>-clustd.
    APPEND w_stxl_raw TO t_stxl_raw.
    IMPORT tline = t_tline FROM INTERNAL TABLE t_stxl_raw.
    LOOP AT t_tline ASSIGNING <tline>.
      gs_header-remarks = <tline>-tdline.
      APPEND gs_header TO gt_header.
      CLEAR gs_header.
    ENDLOOP.
  ENDLOOP.
  FREE t_stxl.

  SORT gt_ekpo BY ebeln ebelp.
  SORT gt_header BY ebeln.
  SORT gt_eket BY ebeln ebelp.
  SORT gt_ekko BY ebeln.
  SORT gt_lfa1 BY lifnr.
  SORT gt_makt BY matnr.
  SORT gt_ekpo1 BY ebeln.
  SORT gt_cdhdr BY objectclas objectid udate.
  SORT gt_ekbe  BY ebeln ebelp bwart.
*------------------------Uploading the data into final table  ----------------------------
  REFRESH gt_final.
  LOOP AT gt_ekpo INTO gs_ekpo.

    CLEAR: gs_final.
    gs_final-pono         = gs_ekpo-ebeln.
    gs_final-branchid     = gs_ekpo-werks.
    gs_final-modifiedon_i = gs_ekpo-aedat.
    gs_final-itemcode     = gs_ekpo-matnr.
    gs_final-rate         = gs_ekpo-netwr.
    gs_final-uom          = gs_ekpo-meins.
    gs_final-qty          = gs_ekpo-menge.
    gs_final-poitemlineno = gs_ekpo-ebelp.

    IF gs_ekpo-loekz = 'L'.
      gs_final-is_deleted_i = TEXT-008."'Yes'.
    ELSE.
      gs_final-is_deleted_i = TEXT-009."'No'.
    ENDIF.

    READ TABLE gt_ekko INTO gs_ekko WITH KEY ebeln = gs_ekpo-ebeln BINARY SEARCH."Reading the header field values

    IF sy-subrc IS INITIAL.
      gs_final-podate     = gs_ekko-aedat.
      gs_final-vendorcode = gs_ekko-lifnr.
      gs_final-currency   = gs_ekko-waers.
      gs_final-refno      = gs_ekko-unsez.

      IF gs_ekko-loekz = 'L'.
        gs_final-is_deleted_h = TEXT-008."'Yes'.
      ELSE.
        gs_final-is_deleted_h = TEXT-009."'No'.
      ENDIF.
      IF gs_ekko-bstyp = 'L'.
        gs_final-schedule = TEXT-008."'Yes'.
      ELSE.
        gs_final-schedule = TEXT-009."'No'.
      ENDIF.
      lv_dt = gs_ekko-lastchangedatetime.
      lv_date = lv_dt(8).
      gs_final-modifiedon_h = lv_date.
    ENDIF.
    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_ekko-lifnr BINARY SEARCH. "Read the vendor field values

    IF sy-subrc IS INITIAL.
      gs_final-vendorname = gs_lfa1-name1.
    ENDIF.

    READ TABLE gt_makt INTO gs_makt WITH KEY matnr  = gs_ekpo-matnr BINARY SEARCH."Read the material description values

    IF sy-subrc IS INITIAL.
      gs_final-description = gs_makt-description.
    ENDIF.

    IF l_value IS NOT INITIAL.

      READ TABLE lt_main INTO ls_main WITH KEY ebeln = gs_ekpo-ebeln
                                               ebelp = gs_ekpo-ebelp
                                               chngind = 'I'.
      IF sy-subrc IS INITIAL.
        ls_main-chngind = 'I'.
        gs_final-recordtype  = TEXT-003."'New'.
      ELSE.
        READ TABLE lt_main INTO ls_main with key ebeln = gs_ekpo-ebeln
                                                  chngind = 'I'
                                                  tabname = 'EKKO'.
        if sy-subrc is INITIAL.
               gs_final-recordtype  = TEXT-003."'New'.
        ELSE.
        gs_final-recordtype  = TEXT-002.".'Exist'
        endif.
      ENDIF.
    ENDIF.

    READ TABLE gt_ekbe INTO gs_ekbe WITH KEY ebeln = gs_ekpo-ebeln                 "Read the purchase document values
                                             ebelp = gs_ekpo-ebelp
                                             BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF gs_ekbe-bwart EQ 103.
        lv_103ge = gs_ekbe-bpwes.
      ENDIF.
      CLEAR gs_ekbe.
    ENDIF.

    READ TABLE gt_ekbe INTO gs_ekbe WITH KEY ebeln = gs_ekpo-ebeln                 "Read the purchase document values
                                     ebelp = gs_ekpo-ebelp
                                     BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF gs_ekbe-bwart EQ 104.
        lv_104ge = gs_ekbe-bpwes.
      ENDIF.
      CLEAR gs_ekbe.
    ENDIF.
    lv_cal1ge = lv_103ge - lv_104ge.
    IF lv_cal1ge = 0.
      gs_final-gateentry = TEXT-004."'Not started'.
    ELSE.
      lv_cal3ge = gs_final-qty - lv_cal1ge.
      IF lv_cal3ge = 0.
        gs_final-gateentry = TEXT-005."'Fully Complete'.
      ELSE.
        gs_final-gateentry = TEXT-006."'Partially Complete'.
      ENDIF.
    ENDIF.
    IF gt_ekbe IS NOT INITIAL.
      READ TABLE gt_ekbe INTO gs_ekbe WITH KEY ebeln = gs_ekpo-ebeln                 "Read the purchase document values
                                               ebelp = gs_ekpo-ebelp
                                               BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        IF gs_ekbe-bwart EQ 105.
          lv_105grn = gs_ekbe-menge.
        ENDIF.
        CLEAR gs_ekbe.
      ENDIF.

      READ TABLE gt_ekbe INTO gs_ekbe WITH KEY ebeln = gs_ekpo-ebeln                 "Read the purchase document values
                                             ebelp = gs_ekpo-ebelp
                                             BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        IF gs_ekbe-bwart EQ 106.
          lv_106grn = gs_ekbe-menge.
        ENDIF.
        CLEAR gs_ekbe.
      ENDIF.
      lv_p1grn  = lv_105grn - lv_106grn.
      IF lv_p1grn = 0.
        gs_final-grn = TEXT-004."'Not started'.
      ENDIF.
      lv_p3grn = gs_final-qty - lv_p1grn.
      IF  lv_p3grn = 0.
        gs_final-grn = TEXT-005."'Fully Complete'.
      ELSE.
        gs_final-grn = TEXT-006."'Partially Complete'.
      ENDIF.
    ENDIF.

    READ TABLE gt_header INTO gs_header WITH KEY ebeln = gs_ekpo-ebeln BINARY SEARCH.  "Read the purchase document header text values
    IF sy-subrc IS INITIAL.
      gs_final-remarks = gs_header-remarks.
    ENDIF.
    READ TABLE gt_eket_n INTO gs_eket_n WITH KEY ebeln = gs_ekpo-ebeln
                                                 ebelp = gs_ekpo-ebelp BINARY SEARCH.  "Read the Schedule Agreement date values
    IF sy-subrc IS INITIAL.
      gs_final-duedate = gs_eket_n-eindt.
    ENDIF.

    APPEND gs_final TO gt_final.
    CLEAR :gs_ekpo,gs_ekbe,gs_cdhdr,gs_ekko,gs_lfa1,gs_eket,gs_makt,gs_ekpo1,gs_header,gs_eket_n.

  ENDLOOP.
  SORT gt_final BY pono.
*-----------Convert the internal table to Json format
  IF gt_final IS NOT INITIAL.
    CALL METHOD /ui2/cl_json=>serialize(
      EXPORTING
        data   = gt_final
      RECEIVING
        r_json = ls_json
    ).
  ELSE.
    CALL METHOD /ui2/cl_json=>serialize(
      EXPORTING
        data   = TEXT-001
      RECEIVING
        r_json = ls_json
    ).
  ENDIF.
  mo_response->create_entity( )->set_string_data( iv_data = ls_json ).

ENDMETHOD.
