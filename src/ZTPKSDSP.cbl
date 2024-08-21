       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPKSDSP.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This example does basic KSDS file operations that is validated *
      * by the unit test suite ZTTFILES.                               *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BETA-FILE ASSIGN TESTKSDS
           INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS BETA-KEY
           FILE STATUS IS BETA-FILE-FS.
       DATA DIVISION.

       FILE SECTION.
       FD  BETA-FILE RECORD CONTAINS 128 CHARACTERS
           DATA RECORD IS BETA-RECORD.
       01  BETA-RECORD.
           02  BETA-KEY  PIC 9(8).
           02  FILLER    PIC X.
           02  BETA-DATA PIC X(119).

       WORKING-STORAGE SECTION.
       01  BETA-FILE-FS PIC 9(2).
       77  I PIC 9(2).
       77  J PIC 9(2).

       01  LETTERS PIC X(10) VALUE 'ABCDEFGHIJ'.
       01  LETTER-REDEF REDEFINES LETTERS.
           05 BETA PIC X OCCURS 10 TIMES.

       PROCEDURE DIVISION.

      *-----------------------------------------------------------------
      * It's only a sample, so no checking of file I/O errors.
      * A unit test could force I/O errors and would find that
      * nothing "wrong" was reported. Not good!
      *-----------------------------------------------------------------
           OPEN OUTPUT BETA-FILE

      *-----------------------------------------------------------------
      * Add records from A, BB, CCC, DDDD, EEEEE, etc. through J.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE SPACES TO BETA-RECORD
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > I
                   MOVE BETA(I) TO BETA-DATA(J:1)
               END-PERFORM

               MOVE I TO BETA-KEY
               WRITE BETA-RECORD

               DISPLAY 'ZTPKSDSP record=' BETA-RECORD
           END-PERFORM

           CLOSE BETA-FILE

           DISPLAY 'ZTPKSDSP wrote 10 records to BETA-FILE'

      *-----------------------------------------------------------------
      * Confirm they were written as expected.
      *-----------------------------------------------------------------
           OPEN I-O BETA-FILE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE I TO BETA-KEY
               READ BETA-FILE
               DISPLAY 'ZTPKSDSP record=' 
                   BETA-RECORD ' status=' BETA-FILE-FS
           END-PERFORM

      *-----------------------------------------------------------------
      * Delete every other record.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 2 UNTIL I > 10
               MOVE I TO BETA-KEY
               DELETE BETA-FILE
               DISPLAY 'ZTPKSDSP deleted record key=' BETA-KEY 
                       ' status=' BETA-FILE-FS
           END-PERFORM

           DISPLAY 'ZTPKSDSP deleted 5 records from BETA-FILE'

      *-----------------------------------------------------------------
      * Confirm the file was updated as expected.
      *-----------------------------------------------------------------
           DISPLAY 'ZTPKSDSP start read of BETA-FILE'

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE I TO BETA-KEY
               READ BETA-FILE

               IF BETA-FILE-FS = 0
                   DISPLAY 'ZTPKSDSP record key=' BETA-KEY
                           '=' BETA-RECORD
               ELSE
                   DISPLAY 'ZTPKSDSP record key=' BETA-KEY
                           ' status=' BETA-FILE-FS
               END-IF
           END-PERFORM

           CLOSE BETA-FILE

           DISPLAY 'ZTPKSDSP end read of BETA-FILE'

           GOBACK.
