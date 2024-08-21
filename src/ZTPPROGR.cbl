       PROCESS NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPPROGR.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This program demonstrates a program, ZTPPROGR, that calls      *
      * another program, ZTPCALLD. The latter simply maps the input    *
      * (single letter A-Z) into an output (corresponding animal       *
      * name). The Test4z example unit test program ZTTSTUBP uses the  *
      * _StubProgram API to intercept these calls, log parameters,     *
      * and substitute its own results as part of unit testing.        *
      *                                                                *
      * The unit test provides its own implementation of the called    *
      * program and optionally can introduce an "unhappy path" test    *
      * that returns an error to confirm the system-under-test handles *
      * it correctly.                                                  *
      ******************************************************************

       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       01  ANIMAL-LETTER-LIST.
           02 PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       01  WS-OUTPUT-ANIMAL-NAME PIC X(10) VALUE SPACES.
       01  WS-INPUT-LETTER PIC X(1).
       
       LINKAGE SECTION.

       PROCEDURE DIVISION.

      *-----------------------------------------------------------------
      * Call ZTPCALLD with every letter, A-Z. 
      *----------------------------------------------------------------- 
           PERFORM VARYING TALLY FROM 1 BY 1 UNTIL TALLY > 26
              MOVE ANIMAL-LETTER-LIST(TALLY: 1) TO WS-INPUT-LETTER
              CALL 'ZTPCALLD'
                 USING WS-INPUT-LETTER, WS-OUTPUT-ANIMAL-NAME
              DISPLAY 'ZTPPROGR input=' WS-INPUT-LETTER 
                      ' to ZTPCALLD, output=' WS-OUTPUT-ANIMAL-NAME
                  
           END-PERFORM

      *-----------------------------------------------------------------
      * Intentionally provide invalid input to verify the called
      * program ZTPCALLD returns a reasonable result.
      *-----------------------------------------------------------------
           MOVE '9' TO WS-INPUT-LETTER
           CALL 'ZTPCALLD'
              USING WS-INPUT-LETTER, WS-OUTPUT-ANIMAL-NAME
              DISPLAY 'ZTPPROGR input=' WS-INPUT-LETTER 
                      ' for ZTPCALLD, output=' WS-OUTPUT-ANIMAL-NAME
                      ' [intentionally invalid]'

      *-----------------------------------------------------------------
      * This quick check verifies that invalid input is detected
      * by the called program. The unit test ZTTSTUBP is checking, too,
      * but it never hurts to have a proactive SUT. So check for
      * the invalid result and set a return code accordingly, where
      * RC=12 means it failed and RC=4 means the invalid input was
      * detected (and not processed by this program).
      *-----------------------------------------------------------------
           IF WS-OUTPUT-ANIMAL-NAME(1:1) NOT = '?'
              COMPUTE RETURN-CODE = 12
           ELSE
              COMPUTE RETURN-CODE = 4
           END-IF

           GOBACK.
           