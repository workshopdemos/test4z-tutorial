       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPQHELO.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This "hello world" program reads names from an input file and  *
      * writes a greeting to an output file. See ZTTQSAMP for its      *
      * associated unit test that uses a Test4z QSAM spy to validate   *
      * this program's output.                                         *
      *                                                                *
      * NB: This example intentionally omits I/O error handling to     *
      *     demonstrate what happens if "unhappy paths" aren't tested. *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-NAMES-FILE ASSIGN SYSIN1
           FILE STATUS IS INPUT-NAMES-FILE-STATUS.
           SELECT OUTPUT-GREETINGS-FILE ASSIGN SYSOUT1.

       DATA DIVISION.
       
       FILE SECTION.
       FD  INPUT-NAMES-FILE RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS INPUT-NAME-RECORD.
       01  INPUT-NAME-RECORD PIC X(80).
       
       FD  OUTPUT-GREETINGS-FILE RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-GREETING-RECORD.
       01  OUTPUT-GREETING-RECORD PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  INPUT-NAMES-FILE-STATUS PIC X(2).
      
       PROCEDURE DIVISION.
           OPEN INPUT INPUT-NAMES-FILE
           OPEN OUTPUT OUTPUT-GREETINGS-FILE

           READ INPUT-NAMES-FILE
           PERFORM UNTIL INPUT-NAMES-FILE-STATUS > '04'
               MOVE SPACES TO OUTPUT-GREETING-RECORD
               STRING 'Hello, ' FUNCTION TRIM(INPUT-NAME-RECORD) '!'
                   DELIMITED BY SIZE INTO OUTPUT-GREETING-RECORD
               WRITE OUTPUT-GREETING-RECORD
               READ INPUT-NAMES-FILE
           END-PERFORM

           CLOSE INPUT-NAMES-FILE
           CLOSE OUTPUT-GREETINGS-FILE

           GOBACK.
