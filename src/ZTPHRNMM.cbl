       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPHRNMM.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This is one of three programs that collectively support the    *
      * promotion decision-making process of a fictional "widget"      *
      * marketing team:                                                *
      *                                                                *
      * 1. ZTPHRNMM - main program that reads a list of "watched"      *
      *               widgets for possible sales promotion             *
      * 2. ZTPHRNDD - called program to fetch the associated recent    *
      *               sales data of the watched widgets (e.g.,         *
      *               last 30 days, updated regularly)                 *
      * 3. ZTPHRNAA - called program to analyze recent sales data      *
      *               and "score" popular widgets that are promotion   *
      *               candidates for the marketing team.               *
      *                                                                *
      * The unit test suite, ZTTHRNES, is responsible for validating   *
      * the correct operation of ZTPHRNMM/ZTPHRNDD/ZTPHRNAA.           *
      *                                                                *
      * This example demonstrates how to mock other resources like     *
      * files and called programs in order to test a given called      *
      * program in isolation using the Test4z harness.                 *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-WATCH-FILE ASSIGN SALESWCH
           FILE STATUS IS SALES-WATCH-FILE-STATUS.

           SELECT SALES-PROMO-FILE ASSIGN SALESPRM
           FILE STATUS IS SALES-PROMO-FILE-STATUS.

       DATA DIVISION.
       
       FILE SECTION.
       FD  SALES-WATCH-FILE RECORD CONTAINS 7 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS SALES-WATCH-RECORD.
       01  SALES-WATCH-RECORD PIC X(7).
       
       FD  SALES-PROMO-FILE RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS SALES-PROMO-RECORD.
       01  SALES-PROMO-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       
       01  SALES-WATCH-FILE-STATUS PIC X(2).
       01  SALES-PROMO-FILE-STATUS PIC X(2).

           COPY ZTPHRNRR
               REPLACING ==:HRN:== BY ==WID==.

       01  RPT-WIDGET-SALES-PROMO.
           05 FILLER PIC X(8) VALUE      'Widget: '.
           05 RPT-WIDGET-ID              PIC X(7).
           05 FILLER PIC X(8) VALUE      ' Score: '.
           05 RPT-WIDGET-SALES-SCORE     PIC Z9(1).
           05 FILLER PIC X(6) VALUE      ' Avg: '.
           05 RPT-WIDGET-SALES-AVG       PIC Z(3)9.
           05 FILLER PIC X(6) VALUE      ' Max: '.
           05 RPT-WIDGET-SALES-MAX       PIC Z(3)9.
           05 FILLER PIC X(6) VALUE      ' Hot: '.
           05 RPT-WIDGET-SALES-HOT-COUNT PIC 9.

       01  LOCAL-WIDGET-ID PIC X(7).

       LINKAGE SECTION.

       PROCEDURE DIVISION.

           DISPLAY 'ZTPHRNMM start'

      ******************************************************************
      * Read the list of "watched" widgets, score them, and write      *
      * out a report for the marketing department.                     *
      ******************************************************************
           PERFORM OPEN-FILES
           PERFORM PROCESS-FILES
           PERFORM CLOSE-FILES

           DISPLAY 'ZTPHRNMM end'

           GOBACK.

      ******************************************************************
      * Read the list of "watched" widgets; these are candidates       *
      * for possible sales promotion. In this ficticious scenario,     *
      * the list is frequently updated, i.e., similar to a top         *
      * selling items list for an e-commerce business.                 *
      ******************************************************************
       PROCESS-FILES.

           READ SALES-WATCH-FILE
           PERFORM UNTIL SALES-WATCH-FILE-STATUS > '04'
               MOVE SALES-WATCH-RECORD TO LOCAL-WIDGET-ID
               PERFORM GET-SALES-DATA
               
               PERFORM CALCULATE-PROMO-DATA
               PERFORM WRITE-PROMO-RECORD

               READ SALES-WATCH-FILE
           END-PERFORM

           EXIT.

      ******************************************************************
      * For each watched widget, retrieve its associated sales data    *
      * provided by ZTPHRNDD. Like the watch list, the sales data is   *
      * updated frequently.                                            *
      *                                                                *
      * NB: The Test4z harness helps isolate a given development team  *
      *     from such volatile data sources by capturing a             *
      *     recording -- essentially a snapshot in time for the        *
      *     resources that are not being changed and tested at this    *
      *     time.                                                      *
      *                                                                *
      *     For example, the developers of ZTPHRNAA (analytics)        *
      *     can work from a recording with the data from ZTPHRNMM      *
      *     (main) and ZTPHRNAA (data retrieval), knowing the results  *
      *     of the called programs will not change.                    *
      ******************************************************************
       GET-SALES-DATA.

           DISPLAY 'ZTPHRNMM requesting recent sales data for ' 
               LOCAL-WIDGET-ID ' from ZTPHRNDD'

           MOVE LOCAL-WIDGET-ID
               TO WID-WIDGET-ID IN WID-WIDGET-SALES-RECENT-REC
           CALL 'ZTPHRNDD' USING WID-WIDGET-SALES-RECENT-REC

           EXIT.

      ******************************************************************
      * Using the recent sales data provided by ZTPHRNDD, request      *
      * that ZTPHRNAA analyze and "score" the promotion value of       *
      * the watched widget. In our ficticious story, this algorithm is *
      * tweaked regularly to improve the accuracy of its findings.     *
      ******************************************************************
       CALCULATE-PROMO-DATA.

           DISPLAY 'ZTPHRNMM requesting promo sales data for ' 
               WID-WIDGET-ID IN WID-WIDGET-SALES-RECENT-REC
               ' from ZTPHRNAA'

           CALL 'ZTPHRNAA' 
               USING WID-WIDGET-SALES-RECENT-REC, 
               WID-WIDGET-SALES-PROMO-REC

           EXIT.

      ******************************************************************
      * Write out the summary watch list and (fictional) analysis.     *
      ******************************************************************
       WRITE-PROMO-RECORD.

           MOVE LOCAL-WIDGET-ID TO RPT-WIDGET-ID
           MOVE WID-WIDGET-SALES-SCORE TO RPT-WIDGET-SALES-SCORE
           MOVE WID-WIDGET-SALES-AVG TO RPT-WIDGET-SALES-AVG
           MOVE WID-WIDGET-SALES-MAX TO RPT-WIDGET-SALES-MAX
           MOVE WID-WIDGET-SALES-HOT-COUNT
               TO RPT-WIDGET-SALES-HOT-COUNT

           DISPLAY 'ZTPHRNMM writing promo sales data: ' 
               RPT-WIDGET-SALES-PROMO

           MOVE SPACES TO SALES-PROMO-RECORD
           MOVE RPT-WIDGET-SALES-PROMO TO SALES-PROMO-RECORD
           WRITE SALES-PROMO-RECORD

           EXIT.

      ******************************************************************
      * Open/close the files as needed. This program under test        *
      * should really verify all I/O operations, but we've             *
      * intentionally omitted these checks for instructive purposes.   *
      *                                                                *
      * For example, the unit test suite should verify these are       *
      * done by forcing I/O errors (see ZTTFILES for an example        *
      * of how to force such "unhappy paths").                         *
      ******************************************************************
       OPEN-FILES.

           OPEN OUTPUT SALES-PROMO-FILE
           OPEN INPUT SALES-WATCH-FILE

           EXIT.

       CLOSE-FILES.

           CLOSE SALES-WATCH-FILE
           CLOSE SALES-PROMO-FILE

           EXIT.
           