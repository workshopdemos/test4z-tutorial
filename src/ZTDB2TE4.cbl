       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTDB2TE4.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      ******************************************************************
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  SQLRECORD1.
           02  CUST-NAME PIC X(20).
           02  CUST-ADDR PIC X(20).
           02  ACCOUNT-NUMBER PIC X(12).
           02  PRODUCT-TYPE PIC X(1).
           02  TOTAL-CHECKS PIC S9(9) COMP-4.
           02  ACTUAL-CHECKS PIC S9(9) COMP-4.
           02  NOTIF-DATE PIC X(8).
           02  REORDER-DATE PIC X(8).
           02  REORDERED PIC X(1).
           EXEC SQL INCLUDE SQLCA    END-EXEC.
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
               10  WS-CURRENT-SECOND       PIC 9(02).
               10  WS-CURRENT-MILLISECONDS PIC 9(02).
       01  WS-CURREN-DATE PIC X(10).
      * SQLCODE IN PRINTABLE FORMAT (COBOL CORRUPTS BINARY NUMBERS < 0)
       01  SQCD PIC -9999.
      * VARIABLES FOR ERROR-MESSAGE FORMATTING             *
       01  ERROR-MESSAGE.
               02  ERROR-LEN   PIC S9(4)  COMP VALUE +960.
               02  ERROR-TEXT  PIC X(120) OCCURS 8 TIMES
                                          INDEXED BY ERROR-INDEX.
       77  ERROR-TEXT-LEN      PIC S9(8)  COMP VALUE +120.
       LINKAGE SECTION.
       01  MYNAME PIC X(20).
       PROCEDURE DIVISION USING MYNAME.
           DISPLAY 'PARM=' MYNAME
      * SQL RETURN CODE HANDLING
           EXEC SQL WHENEVER SQLERROR   GOTO DBERROR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GOTO DBERROR END-EXEC.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           DISPLAY 'CURRENT DATE IS ' WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR TO WS-CURREN-DATE(1:4).
           MOVE '-' TO WS-CURREN-DATE(5:1).
           MOVE WS-CURRENT-MONTH TO WS-CURREN-DATE(6:2).
           MOVE '-' TO WS-CURREN-DATE(8:1).
           MOVE WS-CURRENT-DAY TO WS-CURREN-DATE(9:2).
           DISPLAY 'NOTIFICATION DATE 2 IS ' WS-CURREN-DATE.
           EXEC SQL
               UPDATE MYUSERID.TBZELDA SET NOTIFICATION_DATE
               = :WS-CURREN-DATE
               WHERE (((TOTAL_CHECKS = 30 AND ACTUAL_CHECKS <=3) OR
               (TOTAL_CHECKS = 50 AND ACTUAL_CHECKS <=5) OR
               (TOTAL_CHECKS = 80 AND ACTUAL_CHECKS <=8)) AND
               PRODUCT_TYPE IN ('S','C'))
           END-EXEC
           IF SQLCODE = 0 THEN
               DISPLAY 'UPDATE SUCCESSFUL'
           END-IF
           DISPLAY 'THE NOTIFICATION DATE IS: ' WS-CURREN-DATE

           EXEC SQL DECLARE STAT_CSR CURSOR FOR
               SELECT CUST_NAME, CUST_ADDR
               FROM MYUSERID.TBZELDA
               WHERE CUST_NAME = :MYNAME
           END-EXEC.
           EXEC SQL OPEN STAT_CSR END-EXEC.

       NEXT-STATIC-ROW.
           EXEC SQL FETCH FROM STAT_CSR
               INTO :CUST-NAME,:CUST-ADDR
           END-EXEC.
           EVALUATE SQLCODE
               WHEN 0
                 DISPLAY 'CUST-NAME = ' CUST-NAME
                 DISPLAY 'CUST-ADDR = ' CUST-ADDR
                 GO TO NEXT-STATIC-ROW
               WHEN 100
                 DISPLAY '--- END OF DATA ---'
               WHEN OTHER
                 MOVE SQLCODE TO SQCD
                 DISPLAY '--- ERROR: SQLCODE = ' SQCD
           END-EVALUATE.
           GOBACK.

       DBERROR.
           CALL 'DSNTIAR' USING SQLCA ERROR-MESSAGE ERROR-TEXT-LEN.
           DISPLAY ERROR-MESSAGE
           GOBACK.
