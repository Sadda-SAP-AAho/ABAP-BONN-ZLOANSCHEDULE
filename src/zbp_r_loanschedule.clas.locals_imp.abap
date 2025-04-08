CLASS LHC_ZR_LOANSCHEDULE DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.
    METHODS:
      GET_GLOBAL_AUTHORIZATIONS FOR GLOBAL AUTHORIZATION
        IMPORTING
           REQUEST requested_authorizations FOR ZrLoanschedule
        RESULT result.
     METHODS generateData FOR MODIFY
      IMPORTING keys FOR ACTION ZrLoanschedule~generateData RESULT result.

     METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ZrLoanschedule RESULT result.

     METHODS getCID RETURNING VALUE(cid) TYPE abp_behv_cid.

      METHODS approved FOR DETERMINE ON MODIFY
      IMPORTING keys FOR ZrLoanschedule~approved.

ENDCLASS.

CLASS LHC_ZR_LOANSCHEDULE IMPLEMENTATION.
  METHOD GET_GLOBAL_AUTHORIZATIONS.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    DATA: update_requested TYPE abap_boolean,
          delete_requested TYPE abap_boolean.

    READ ENTITIES OF zr_loanschedule IN LOCAL MODE
    ENTITY ZrLoanschedule
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(GateEntryHeaders)
    FAILED failed.

    CHECK GateEntryHeaders IS NOT INITIAL.

    update_requested = COND #( WHEN requested_authorizations-%update = if_abap_behv=>mk-on
                                  THEN abap_true ELSE abap_false ).
    delete_requested = COND #( WHEN requested_authorizations-%delete = if_abap_behv=>mk-on
                                  THEN abap_true ELSE abap_false ).

    LOOP AT GateEntryHeaders ASSIGNING FIELD-SYMBOL(<lfs_gateheader>).
      IF update_requested = abap_true OR delete_requested = abap_true.
        IF <lfs_gateheader>-Approved = abap_true.

          APPEND VALUE #( %tky = <lfs_gateheader>-%tky ) TO failed-zrloanschedule.

          APPEND VALUE #( %tky = <lfs_gateheader>-%tky
                          %msg = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text = 'Cannot Change or delete Document.'
                          ) ) TO reported-zrloanschedule.

        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

   METHOD generateData.
    DATA : lt_irn TYPE TABLE OF zr_loanschedule.
    DATA : wa_irn TYPE zr_loanschedule.

    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    DATA(Company) = ls_key-%param-comp_code.
    DATA(monthYear) = ls_key-%param-month_year.

    SPLIT monthYear AT '/' INTO DATA(lv_month) DATA(lv_year).
    DATA(sysDate) = cl_abap_context_info=>get_system_date( ).
    DATA(lv_date) = sysDate+6(2).

    CONCATENATE lv_year lv_month lv_date INTO DATA(lv_filterdate).

    SELECT FROM zr_loanmaster
    FIELDS BalanceAmount, EmployeeId, EmployeeName, CompCode, LoanNo, LoanAmount, LoanType, TotalAmount, EMICount
    WHERE DeductedFrom LE @lv_filterdate and BalanceAmount GT 0 and EMICount > 0
    INTO TABLE @DATA(lv_loan).
    SORT lv_loan BY LoanNo.
    DELETE ADJACENT DUPLICATES FROM lv_loan COMPARING LoanNo CompCode.
    GET TIME STAMP FIELD DATA(lv_timestamp).
    LOOP AT lv_loan INTO DATA(wa_loan).

        SELECT SINGLE FROM zr_loanschedule
        FIELDS LoanNo
        WHERE LoanNo = @wa_loan-LoanNo and CompCode = @wa_loan-CompCode and MonthYear = @monthyear
        INTO @DATA(curSchedule).

        IF curSchedule IS NOT INITIAL.
            CONTINUE.
        ENDIF.

        DATA installAmt TYPE P LENGTH 10 DECIMALS 3.

        IF wa_loan-LoanType = 'LOAN'.
            installAmt = wa_loan-TotalAmount / wa_loan-EMICount.
        ELSE.
            installAmt = wa_loan-TotalAmount.
        ENDIF.

        DATA(cid) = getCID( ).


        MODIFY ENTITIES OF zr_loanschedule IN LOCAL MODE
            ENTITY ZrLoanschedule
            CREATE
            FIELDS ( CompCode LoanNo LoanType EmployeeId EmployeeName MonthYear InstallmentAmount )
            WITH VALUE #(
              ( %cid = cid
                CompCode = wa_loan-CompCode
                LoanNo = wa_loan-LoanNo
                LoanType = wa_loan-LoanType
                EmployeeId = wa_loan-EmployeeId
                EmployeeName = wa_loan-EmployeeName
                InstallmentAmount = installAmt
                MonthYear = monthyear
                lastchangedat = lv_timestamp
                )
            )
            MAPPED mapped
            FAILED   failed
            REPORTED reported.
    ENDLOOP.


    APPEND VALUE #( %cid = ls_key-%cid
                    %msg = new_message_with_text(
                      severity = if_abap_behv_message=>severity-success
                      text = 'Data Generated.' )
                      ) TO reported-zrloanschedule.
    RETURN.
  ENDMETHOD.

  METHOD approved.

     READ ENTITIES OF zr_loanschedule IN LOCAL MODE
        ENTITY ZrLoanschedule
        FIELDS ( ApprovedAmount LoanNo CompCode )
        WITH CORRESPONDING #( keys )
        RESULT DATA(lv_loanschedule).



*
    LOOP AT lv_loanschedule INTo DATA(wa_loanschedule).
        SELECT SINGLE FROM zr_loanmaster
        FIELDS BalanceAmount
        WHERE LoanNo = @wa_loanschedule-LoanNo and CompCode = @wa_loanschedule-CompCode
        INTO @DATA(BalAmt).

        DATA newBalAmt TYPE P LENGTH 10 DECImALS 3.
        newBalAmt = BalAmt - wa_loanschedule-ApprovedAmount.


       MODIFY ENTITIES OF zr_loanmaster
        ENTITY ZrLoanmaster
        UPDATE FIELDS ( BalanceAmount )
        WITH VALUE #( ( LoanNo = wa_loanschedule-LoanNo CompCode = wa_loanschedule-CompCode BalanceAmount = newbalamt ) )
        FAILED DATA(lt_failed2)
        REPORTED DATA(lt_reported2).

       MODIFY ENTITIES OF zr_loanschedule IN LOCAL MODE
        ENTITY ZrLoanschedule
        UPDATE FIELDS ( ApprovedBy )
        WITH VALUE #( (
            %tky       = wa_loanschedule-%tky
            ApprovedBy = sy-uname
          ) )
        FAILED DATA(lt_failed)
        REPORTED DATA(lt_reported).


    ENDLOOP.

  ENDMETHOD.

  METHOD getCID.
            TRY.
                cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
            CATCH cx_uuid_error.
                ASSERT 1 = 0.
            ENDTRY.
  ENDMETHOD.

ENDCLASS.
