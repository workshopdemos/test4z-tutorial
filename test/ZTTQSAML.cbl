       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTQSAML' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This unit test example has been replaced by ZTTQSAMD.          *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ZTESTWS.
       LINKAGE SECTION.

       PROCEDURE DIVISION.

           MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
           MOVE 'ZTTQSAML this example replaced by ZTTQSAMD'
                TO MESSAGETEXT IN ZWS_MESSAGE
           CALL ZTESTUT USING ZWS_MESSAGE

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'emptyUnitTest'
           MOVE MESSAGETEXT IN ZWS_MESSAGE TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST
           
           GOBACK.
           
           ENTRY 'emptyUnitTest'.

           GOBACK.
           