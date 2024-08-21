       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPQSAMT.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This short program reads the content of a QSAM file and        *
      * outputs its records to SYSOUT. It is used to demonstrate the   *
      * unit test program ZTTQSAMD.                                    *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TESTQSAM
           FILE STATUS IS QSAM-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-RECORD.
       01  OUTPUT-RECORD PIC X(80).
       WORKING-STORAGE SECTION.
       01  QSAM-STATUS PIC X(2).
       
       PROCEDURE DIVISION.
       
           OPEN INPUT OUTPUT-FILE
           PERFORM UNTIL QSAM-STATUS > '04'
              READ OUTPUT-FILE
                  NOT AT END
                      DISPLAY 'ZTPQSAMT record=' OUTPUT-RECORD
              END-READ
           END-PERFORM
           CLOSE OUTPUT-FILE

           DISPLAY 'ZTPQSAMT complete'

           GOBACK.
           