       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPKSDST.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This short program writes to a KSDS file and then outputs its  *
      * records to SYSOUT. It is used to demonstrate the unit test     *
      * program ZTTKSDSD.                                              *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KSDS-FILE ASSIGN TO TESTKSDS
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS KSDS-KEY
           FILE STATUS IS KSDS-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  KSDS-FILE
           RECORD IS VARYING IN SIZE
               FROM 1 TO 80 CHARACTERS
               DEPENDING ON KSDS-RECORD-SIZE
           DATA RECORD IS KSDS-RECORD.
       01  KSDS-RECORD.
           02  KSDS-KEY        PIC X(8).
           02  KSDS-DATA       PIC X(72).
       
       WORKING-STORAGE SECTION.
       01  KSDS-STATUS         PIC X(2).
       01  KSDS-RECORD-SIZE    PIC 9(2) COMP-5.

       01  WS-OUTPUT-LETTER    PIC X(10) VALUE 'ABCDE'.
       01  I                   PIC 9(8).
       01  J                   PIC 9(2).
       01  K                   PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.

           PERFORM WRITE-RECORDS
           PERFORM READ-RECORDS
           DISPLAY 'ZTPKSDST complete'

           GOBACK.

      ******************************************************************
      * Echo the records in the output file. It should include the     *
      * records written by this program as well as the pre-loaded      *
      * records from the unit tests in ZTTKSDSD.                       *
      ******************************************************************
       READ-RECORDS.

           OPEN INPUT KSDS-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 15
               MOVE I TO KSDS-KEY
               READ KSDS-FILE

               IF KSDS-STATUS <= '04'
                   DISPLAY 'ZTPKSDST read size=' KSDS-RECORD-SIZE
                           ' key=' KSDS-KEY
                           ' data=' KSDS-DATA
                               (1:KSDS-RECORD-SIZE - LENGTH OF KSDS-KEY)
               ELSE
                   DISPLAY 'ZTPKSDST key ' KSDS-KEY ' not read, '
                           ' status ' KSDS-STATUS
               END-IF
           END-PERFORM
           CLOSE KSDS-FILE

           EXIT.

      ******************************************************************
      * Write some records that will not rewrite those pre-loaded      *
      * by the unit test program ZTTKSDSD.                             *
      ******************************************************************
       WRITE-RECORDS.

           OPEN I-O KSDS-FILE
           PERFORM VARYING I FROM 11 BY 1 UNTIL I > 15

      *-----------------------------------------------------------------
      * For ease of verifying the output, add column number indicators
      * as well as an "eyecatcher" prefix (AAA... BBBB... CCCCC...).
      *
      * For example:
      *
      * AAAAAAAAAAA---+----+----+----+----+----+--...
      * BBBBBBBBBBBB--+----+----+----+----+----+----...
      *-----------------------------------------------------------------
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 72
                   IF ((J / 5) * 5) = J
                       MOVE '+' TO KSDS-DATA(J:1)
                   ELSE
                       MOVE '-' TO KSDS-DATA(J:1)
                   END-IF
               END-PERFORM

               ADD 1 TO K
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > I
                   MOVE WS-OUTPUT-LETTER(K:1) TO KSDS-DATA(J:1)
               END-PERFORM

               MOVE I TO KSDS-KEY
               MOVE 80 TO KSDS-RECORD-SIZE
               WRITE KSDS-RECORD
               IF KSDS-STATUS NOT = '00'
                   DISPLAY 'ZTPKSDST key ' KSDS-KEY ' not written, '
                           ' status ' KSDS-STATUS
               END-IF
           END-PERFORM
           CLOSE KSDS-FILE

           EXIT.
