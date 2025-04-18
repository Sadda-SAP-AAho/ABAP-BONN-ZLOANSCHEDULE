CLASS zc_dellndata DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zc_dellndata IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    delete from zloanmaster.
    delete from zloanschedule.
  ENDMETHOD.
ENDCLASS.
