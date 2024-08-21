       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTDB2TE1.

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
       01  MY-CUST-NAME PIC X(20).
       PROCEDURE DIVISION.
           MOVE 'RONALD REAGONE' TO MY-CUST-NAME
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           DISPLAY 'CURRENT DATE IS ' WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR TO WS-CURREN-DATE(1:4).
           MOVE '-' TO WS-CURREN-DATE(5:1).
           MOVE WS-CURRENT-MONTH TO WS-CURREN-DATE(6:2).
           MOVE '-' TO WS-CURREN-DATE(8:1).
           MOVE WS-CURRENT-DAY TO WS-CURREN-DATE(9:2).
           DISPLAY 'NOTIFICATION DATE 2 IS ' WS-CURREN-DATE.
           PERFORM 5 TIMES
           EXEC SQL
               UPDATE MYUSERID.TBZELDA SET NOTIFICATION_DATE
               = :WS-CURREN-DATE
               WHERE (((TOTAL_CHECKS = 30 AND ACTUAL_CHECKS <=3) OR
               (TOTAL_CHECKS = 50 AND ACTUAL_CHECKS <=5) OR
               (TOTAL_CHECKS = 80 AND ACTUAL_CHECKS <=8)) AND
               PRODUCT_TYPE IN ("S","C"))
           END-EXEC
           IF SQLCODE = 0 THEN
               DISPLAY "SUCCESS"
           ELSE
               DISPLAY SQLSTATE
           END-IF
           DISPLAY "THE NOTIFICATION DATE IS : " WS-CURREN-DATE
           EXEC SQL
               SELECT CUST_NAME, CUST_ADDR
               INTO :CUST-NAME,:CUST-ADDR
               FROM MYUSERID.TBZELDA
               WHERE CUST_NAME = :MY-CUST-NAME
           END-EXEC
           IF SQLCODE = 0 THEN
               DISPLAY "SUCCESS"
           ELSE
               DISPLAY SQLSTATE
           END-IF
           END-PERFORM
           EXEC SQL
               UPDATE MYUSERID.TBZELDA SET NOTIFICATION_DATE
               = :WS-CURREN-DATE
               WHERE (((TOTAL_CHECKS = 30 AND ACTUAL_CHECKS <=3) OR
               (TOTAL_CHECKS = 50 AND ACTUAL_CHECKS <=5) OR
               (TOTAL_CHECKS = 80 AND ACTUAL_CHECKS <=8)) AND
               PRODUCT_TYPE IN ("S","C"))
           END-EXEC.
           IF SQLCODE = 0 THEN
               DISPLAY "SUCCESS"
           ELSE
               DISPLAY SQLSTATE
           END-IF.
           DISPLAY "THE NOTIFICATION DATE IS : " WS-CURREN-DATE.
           EXEC SQL
               SELECT CUST_NAME, CUST_ADDR
               INTO :CUST-NAME,:CUST-ADDR
               FROM MYUSERID.TBZELDA
               WHERE CUST_NAME = :MY-CUST-NAME
           END-EXEC.
           IF SQLCODE = 0 THEN
               DISPLAY "SUCCESS"
           ELSE
               DISPLAY SQLSTATE
           END-IF.
           EXEC SQL
               UPDATE MYUSERID.TBZELDA SET NOTIFICATION_DATE
               = :WS-CURREN-DATE
               WHERE (((TOTAL_CHECKS = 30 AND ACTUAL_CHECKS <=3) OR
               (TOTAL_CHECKS = 50 AND ACTUAL_CHECKS <=5) OR
               (TOTAL_CHECKS = 80 AND ACTUAL_CHECKS <=8)) AND
               PRODUCT_TYPE IN ("S","C"))
           END-EXEC.
           IF SQLCODE = 0 THEN
               DISPLAY "SUCCESS"
           ELSE
               DISPLAY SQLSTATE
           END-IF.
           DISPLAY "THE NOTIFICATION DATE IS : " WS-CURREN-DATE.
           EXEC SQL
               SELECT CUST_NAME, CUST_ADDR
               INTO :CUST-NAME,:CUST-ADDR
               FROM MYUSERID.TBZELDA
               WHERE CUST_NAME = :MY-CUST-NAME
           END-EXEC.
           IF SQLCODE = 0 THEN
               DISPLAY "SUCCESS"
           ELSE
               DISPLAY SQLSTATE
           END-IF.
           GOBACK.
           STOP RUN.
