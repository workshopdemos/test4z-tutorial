       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPDOGOS.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This short example program accepts dog adoption records and    *
      * produces a report. See the Test4z example unit test suite      *
      * ZTTDOGOS that validates the correct operation of ZTPDOGOS.     *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADOPTS-INPUT ASSIGN ADOPTS
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ADOPTS-FS.

           SELECT ADOPTS-REPORT ASSIGN OUTREP
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ADOPTS-REPORT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD ADOPTS-INPUT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS ADOPTED-DOGS-REC.
           
           COPY ZTPDGADR.

       FD ADOPTS-REPORT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS ADOPTED-REPORT-REC.

           COPY ZTPDGARR.

       WORKING-STORAGE SECTION.

      *-----------------------------------------------------------------
      * Input and output file status.
      *-----------------------------------------------------------------
       01  ADOPTS-FS         PIC 9(2).
       01  ADOPTS-REPORT-FS  PIC 9(2).
       01  ADOPTS-INPUT-FLAG PIC 9(1) VALUE 0.
           88 ADOPTS-EOF     VALUE 1.

      *-----------------------------------------------------------------
      * Internal index for mapping breed name and adopted total.
      *-----------------------------------------------------------------
       01 BREED-INDEXES.
           05 SHIBA-INDEX.
               10 SHIBA-INDEX-VALUE      PIC 9(1) VALUE 1.
               10 SHIBA-BREED-NAME       PIC X(30) VALUE 'SHIBA'.
           05 SCHNAUZER-INDEX.
               10 SCHNAUZER-INDEX-VALUE  PIC 9(1) VALUE 2.
               10 SCHNAUZER-BREED-NAME   PIC X(30) VALUE 'SCHNAUZER'.
           05 CORGI-INDEX.
               10 CORGI-INDEX-VALUE      PIC 9(1) VALUE 3.
               10 CORGI-BREED-NAME       PIC X(30) VALUE 'CORGI'.
           05 CHI-INDEX.
               10 CHI-INDEX-VALUE        PIC 9(1) VALUE 4.
               10 CHI-BREED-NAME         PIC X(30) VALUE 'CHI'.
           05 POODLE-INDEX.
               10 POODLE-INDEX-VALUE     PIC 9(1) VALUE 5.
               10 POODLE-BREED-NAME      PIC X(30) VALUE 'POODLE'.
           05 POMERANIAN-INDEX.
               10 POMERANIAN-INDEX-VALUE PIC 9(1) VALUE 6.
               10 POMERANIAN-BREED-NAME  PIC X(30) VALUE 'POMERANIAN'.
           05 BULLDOG-INDEX.
               10 BULLDOG-INDEX-VALUE    PIC 9(1) VALUE 7.
               10 BULLDOG-BREED-NAME     PIC X(30) VALUE 'BULLDOG'.
           05 JINGO-INDEX.
               10 JINGO-INDEX-VALUE      PIC 9(1) VALUE 8.
               10 JINGO-BREED-NAME       PIC X(30) VALUE 'JINGO'.
           05 OTHER-INDEX.
               10 OTHER-INDEX-VALUE      PIC 9(1) VALUE 9.
               10 OTHER-BREED-NAME       PIC X(30) VALUE 'OTHER'.
       01  BREED-INDEXES-REDEF REDEFINES BREED-INDEXES.
           05 BREED-INDEX OCCURS 9 TIMES.
               10 BREED-INDEX-VALUE      PIC 9(1).
               10 BREED-NAME             PIC X(30).
       01  I                             PIC 9(2).
       01  J                             PIC 9(2).
       01  WS-CURRENT-DATE.
           05  CURR-YEAR                 PIC 9(04).
           05  CURR-MONTH                PIC 9(02).
           05  CURR-DAY                  PIC 9(02).
           05  FILLER                    PIC X(13).

      *-----------------------------------------------------------------
      * Internal data structure to total adoptions per breed.
      *-----------------------------------------------------------------
       01  ACCUMULATOR.
           05 BREED-ADOPTIONS PIC 9(3) OCCURS 9 TIMES VALUE 0.

      *-----------------------------------------------------------------
      * Record format for report totals per breed.
      *-----------------------------------------------------------------
       01  ADOPTED-RESULT.
           05 FILLER                 PIC X(6)  VALUE 'BREED '.
           05 DOG-BREED              PIC X(30).
           05 FILLER                 PIC X(13) VALUE ' WAS ADOPTED '.
           05 ADOPTED-AMOUNT         PIC 9(3).
           05 FILLER                 PIC X(6)  VALUE ' TIMES'.
           05 FILLER                 PIC X(22).

       PROCEDURE DIVISION.

           PERFORM PRINT-WELCOME
           PERFORM OPEN-INPUT
           PERFORM OPEN-OUTPUT

      *-----------------------------------------------------------------
      * Read adoption records and keep running total per breed.
      *-----------------------------------------------------------------
           PERFORM READ-ADOPTION
           PERFORM UNTIL ADOPTS-EOF
                PERFORM UPDATE-ACCUMULATOR
                PERFORM READ-ADOPTION
           END-PERFORM

      *-----------------------------------------------------------------
      * Write the breed totals into the report.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
               MOVE BREED-NAME(I) 
                   TO DOG-BREED IN ADOPTED-RESULT
               MOVE BREED-ADOPTIONS(I) 
                   TO ADOPTED-AMOUNT IN ADOPTED-RESULT
               MOVE ADOPTED-RESULT TO ADOPTED-REPORT-REC
               PERFORM WRITE-ADOPTION-BREED-REPORT
           END-PERFORM

           PERFORM CLOSE-INPUT
           PERFORM CLOSE-OUTPUT

           GOBACK.

      ******************************************************************
      * Display "hello" message to SYSOUT.                             *
      ******************************************************************
       PRINT-WELCOME.
           DISPLAY 'ZTPDOGOS - THIS PROGRAM WILL TOTAL ADOPTED BY BREED'
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           DISPLAY 'ZTPDOGOS - TODAY IS ' 
               CURR-YEAR '-' CURR-MONTH '-' CURR-DAY

           EXIT.

      ******************************************************************
      * Open ADOPTS file for input.                                    *
      ******************************************************************
       OPEN-INPUT.
           OPEN INPUT ADOPTS-INPUT.
           IF ADOPTS-FS NOT = 0
               DISPLAY 'ZTPDOGOS - CANNOT OPEN INPUT FILE: ' ADOPTS-FS
               COMPUTE RETURN-CODE = 8
               GOBACK
           END-IF

           EXIT.

      ******************************************************************
      * Open OUTREP report for output.                                 *
      ******************************************************************
       OPEN-OUTPUT.
           OPEN OUTPUT ADOPTS-REPORT.
           IF ADOPTS-REPORT-FS NOT = 0
               DISPLAY 'ZTPDOGOS - CANNOT OPEN OUTPUT FILE: '
                   ADOPTS-REPORT-FS
               COMPUTE RETURN-CODE = 12
               GOBACK
           END-IF

           EXIT.

      ******************************************************************
      * Read adoption record for processing.                           *
      ******************************************************************
       READ-ADOPTION.
           READ ADOPTS-INPUT.
           IF ADOPTS-FS IS EQUAL TO 10
               SET ADOPTS-EOF TO TRUE
           END-IF
           IF ADOPTS-FS NOT = 0 AND NOT = 10
               DISPLAY 'ZTPDOGOS - CANNOT READ RECORD: ' ADOPTS-FS
               COMPUTE RETURN-CODE = 4
               GOBACK
           END-IF

           EXIT.

      ******************************************************************
      * Update internal accumulator of adoptions per breed.            *
      ******************************************************************
       UPDATE-ACCUMULATOR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
               IF INP-DOG-BREED = BREED-NAME(I)
                   MOVE BREED-INDEX-VALUE(I) TO J
                   ADD INP-ADOPTED-AMOUNT TO BREED-ADOPTIONS(J)
                   EXIT PERFORM
               END-IF
           END-PERFORM

           EXIT.

      ******************************************************************
      * Write a report record for the current breed.                   *
      ******************************************************************
       WRITE-ADOPTION-BREED-REPORT.
           WRITE ADOPTED-REPORT-REC
           IF ADOPTS-REPORT-FS NOT = 0
               DISPLAY 'ZTPDOGOS - CANNOT WRITE RECORD: '
                   ADOPTS-REPORT-FS
               COMPUTE RETURN-CODE = 16
               GOBACK
           ELSE
               DISPLAY 'ZTPDOGOS wrote - ' ADOPTED-REPORT-REC
           END-IF

           EXIT.

      ******************************************************************
      * Close input and output files.                                  *
      ******************************************************************
       CLOSE-INPUT.
           CLOSE ADOPTS-INPUT
           
           EXIT.

       CLOSE-OUTPUT.
           CLOSE ADOPTS-REPORT

           EXIT.
