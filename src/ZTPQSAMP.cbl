       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPQSAMP.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This example does basic QSAM file operations that is validated *
      * by the unit test suite ZTTFILES.                               *
      ******************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALPHA-FILE ASSIGN TESTQSAM
           FILE STATUS IS ALPHA-FS.
       DATA DIVISION.

       FILE SECTION.
       FD  ALPHA-FILE RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS ALPHA-RECORD.
       01  ALPHA-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01  ALPHA-FS PIC 9(2).
           88 ALPHA-FS-OK VALUE 0.
           88 ALPHA-FS-EOF VALUE 10.
           88 ALPHA-FS-DNE VALUE 35.
       77  INPUT-FLAG PIC 9.
           88 INPUT-NOT-DONE VALUE 0.
           88 INPUT-DONE VALUE 1.

       01  LETTERS PIC X(16) VALUE 'KLMNOPQRSTUVWXYZ'.
       01  LETTER-REDEF REDEFINES LETTERS.
           05 ALPHA PIC X OCCURS 16 TIMES.

       77  I PIC 9(2).
       77  J PIC 9(2).

       PROCEDURE DIVISION.

      *-----------------------------------------------------------------
      * This example includes file I/O error checking every time;
      * the unit test could "force" an error to verify it handles
      * them correctly.
      *
      * Open the QSAM file for append; the unit test
      * has an initialized mocked QSAM file, so this should not
      * take the exit path.
      *-----------------------------------------------------------------
           OPEN EXTEND ALPHA-FILE
           IF ALPHA-FS-OK
               DISPLAY 'ZTPQSAMP appending to existing ALPHA-FILE'
           ELSE
               IF ALPHA-FS-DNE
                   DISPLAY 'ZTPQSAMP creating new ALPHA-FILE'
                   OPEN OUTPUT ALPHA-FILE
               ELSE
                   DISPLAY 'ZTPQSAMP I/O error=' ALPHA-FS
                   MOVE 12 TO RETURN-CODE
                   STOP RUN
               END-IF
           END-IF

      *-----------------------------------------------------------------
      * Add records from KKKKKKKKKK, LLLLLLLLLLL, MMMMMMMMMMMM, etc.
      * The unit test creates records A, BB, CCC, DDDD, EEEEE, etc.,
      * so the final result should be 26 records A-Z.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 16
               MOVE SPACES TO ALPHA-RECORD
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > I + 10
                   MOVE ALPHA(I) TO ALPHA-RECORD(J:1)
               END-PERFORM

               WRITE ALPHA-RECORD
               IF NOT ALPHA-FS-OK
                   DISPLAY 'ZTPQSAMP I/O error=' ALPHA-FS
                   MOVE 8 TO RETURN-CODE
                   STOP RUN
               END-IF
               DISPLAY 'ZTPQSAMP wrote record ' ALPHA-RECORD
           END-PERFORM

           CLOSE ALPHA-FILE
           IF NOT ALPHA-FS-OK
               DISPLAY 'ZTPQSAMP I/O error=' ALPHA-FS
               MOVE 4 TO RETURN-CODE
           END-IF

           DISPLAY 'ZTPQSAMP appended 16 records to ALPHA-FILE'

      *-----------------------------------------------------------------
      * Now that the records are written, read them back. This should
      * trigger QSAM Spy callbacks in the unit test for each file
      * operation (OPEN, CLOSE, WRITE, READ).
      *-----------------------------------------------------------------
           OPEN INPUT ALPHA-FILE
           IF NOT ALPHA-FS-OK
               DISPLAY 'ZTPQSAMP I/O error=' ALPHA-FS
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF

           DISPLAY 'ZTPQSAMP start read of ALPHA-FILE'

           SET INPUT-NOT-DONE TO TRUE
           MOVE 1 TO I

           PERFORM UNTIL INPUT-DONE OR NOT ALPHA-FS-OK
               READ ALPHA-FILE

               IF ALPHA-FS-EOF
                   SET INPUT-DONE TO TRUE
               ELSE
                   IF ALPHA-FS-OK
                       DISPLAY 'ZTPQSAMP read record '
                               I '=' ALPHA-RECORD
                   ELSE
                       DISPLAY 'ZTPQSAMP I/O error=' ALPHA-FS
                       MOVE 8 TO RETURN-CODE
                       STOP RUN
                   END-IF
               END-IF

               ADD 1 TO I
           END-PERFORM

      *-----------------------------------------------------------------
      * This SUT sets the RETURN-CODE if something goes wrong where
      * RC 12 = nothing worked, 8 = partially complete, 4 = trouble
      * closing files, 0 = no errors. The unit test could force
      * I/O errors and verify these return codes are correctly set.
      *-----------------------------------------------------------------
           CLOSE ALPHA-FILE
           IF NOT ALPHA-FS-OK
               DISPLAY 'ZTPQSAMP I/O error=' ALPHA-FS
               MOVE 4 TO RETURN-CODE
           END-IF

           DISPLAY 'ZTPQSAMP end read of ALPHA-FILE'

           GOBACK.
           