       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPQSAMV.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This short program writes variable length records to a QSAM    *
      * file, both for OUTPUT and EXTEND. It also echos out the        *
      * resulting file content. It is used to demonstrate the unit     *
      * test program ZTTQSAMD.                                         *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TESTQSAM
           FILE STATUS IS QSAM-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE RECORD IS VARYING IN SIZE
           FROM 1 TO 80 CHARACTERS DEPENDING ON OUTPUT-RECORD-SIZE
           RECORDING MODE IS V
           DATA RECORD IS OUTPUT-RECORD.
       01  OUTPUT-RECORD       PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  OUTPUT-RECORD-SIZE  PIC 9(2) COMP-5 VALUE 10.
       01  QSAM-STATUS         PIC X(2).
       
       01  WS-OUTPUT-RECORD    PIC X(80).
       01  WS-OUTPUT-LETTER    PIC X(10) VALUE 'ABCDEFGHIJ'.
       01  I                   PIC 9(2).
       01  J                   PIC 9(2).
       01  K                   PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.

      ******************************************************************    
      * Write 10 records to OUTPUT-FILE of varying length, first       *
      * as OUTPUT and then as EXTEND.                                  *
      ******************************************************************
           OPEN OUTPUT OUTPUT-FILE
           DISPLAY 'ZTPQSAMV writing 5 records (OUTPUT)'
           PERFORM WRITE-RECORDS.
           CLOSE OUTPUT-FILE

           OPEN EXTEND OUTPUT-FILE
           DISPLAY 'ZTPQSAMV writing 5 records (EXTEND)'
           PERFORM WRITE-RECORDS.
           CLOSE OUTPUT-FILE

      *-----------------------------------------------------------------  
      * Echo back the newly written content; there should be 20
      * records.
      *-----------------------------------------------------------------  
           OPEN INPUT OUTPUT-FILE
           PERFORM UNTIL QSAM-STATUS > '04'
               READ OUTPUT-FILE
                   NOT AT END
                       MOVE SPACES TO WS-OUTPUT-RECORD
                       MOVE OUTPUT-RECORD(1:OUTPUT-RECORD-SIZE)
                           TO WS-OUTPUT-RECORD
                       DISPLAY 'ZTPQSAMV read size=' OUTPUT-RECORD-SIZE 
                           ' record=' WS-OUTPUT-RECORD
               END-READ
           END-PERFORM
           CLOSE OUTPUT-FILE

           DISPLAY 'ZTPQSAMV complete'

           GOBACK.

      ******************************************************************    
      * Write 5 records to OUTPUT-FILE of varying length.              *
      ******************************************************************
       WRITE-RECORDS.

      *-----------------------------------------------------------------  
      * Some example record input:
      *
      * 05AA-AAAA-AAAA-AAAA-AAAA-...
      * 10BB-BBBB-BBBB-BBBB-BBBB-BBBB-...
      * 15CC-CCCC-CCCC-CCCC-CCCC-CCCC-CCCC-...
      *
      * The leading number is the record length (05, 10, 15, ...).
      * Although the record is variable length, fill out all 80
      * characters to confirm the trailing characters are not included.
      * The actual written record content should be:
      *
      * 05AA-
      * 10BB-BBBB-
      * 15CC-CCCC-CCCC-
      * ...etc.
      *-----------------------------------------------------------------  
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 80
                   ADD 1 TO K
                   IF K = 5
                       MOVE '-' TO WS-OUTPUT-RECORD(J:1)
                       MOVE 0 TO K
                   ELSE 
                       MOVE WS-OUTPUT-LETTER(I:1)
                           TO WS-OUTPUT-RECORD(J:1)
                   END-IF
               END-PERFORM 
               MOVE OUTPUT-RECORD-SIZE TO WS-OUTPUT-RECORD(1:2)

               DISPLAY 'ZTPQSAMV write size=' OUTPUT-RECORD-SIZE 
                   ' record=' WS-OUTPUT-RECORD(1:OUTPUT-RECORD-SIZE)

               WRITE OUTPUT-RECORD FROM WS-OUTPUT-RECORD 
               ADD 5 TO OUTPUT-RECORD-SIZE
           END-PERFORM

           EXIT.
