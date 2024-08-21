       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTSTUBP' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to run a unit test on a program    *
      * (ZTPPROGR) that calls another program (ZTPCALLD) that is to be *
      * stubbed out and replaced by the logic from this unit test.     *
      *                                                                *
      * NB: "PROCESS PGMN(LM),NODYNAM" above is required for unit      *
      *     tests to enable long entry point names and locating entry  *
      *     points.                                                    *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      ******************************************************************
      * Copy in the required control blocks from Test4z. The           *
      * WORKING-STORAGE control blocks start with ZWS_ and include     *
      * an "I" interface sub-block, e.g.:                              *
      *                                                                *
      * ZWS_STUBPROGRAM -> I_STUBPROGRAM IN ZWS_STUBPROGRAM            *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
       COPY ZTESTWS.

      *-----------------------------------------------------------------
      * Define any working-storage items that we need.
      *-----------------------------------------------------------------
       01  RUN-PROGRAM USAGE FUNCTION-POINTER.

      *-----------------------------------------------------------------
      * Unit test metrics to be validated.
      *-----------------------------------------------------------------
       01  WS-INPUT-LETTERS-CONSONANT-CNT PIC 9(2)  VALUE 0.
       01  WS-INPUT-LETTERS-VOWEL-CNT     PIC 9(2)  VALUE 0.
       01  WS-INPUT-LETTERS-INVALID-CNT   PIC 9(2)  VALUE 0.
       01  WS-ANIMAL-NAME-COPY            PIC X(10).

      *-----------------------------------------------------------------
      * Unit test coverage - which letters were not attempted?
      *-----------------------------------------------------------------
       01  WS-INPUT-LETTERS-UNTESTED      PIC X(26) VALUE 
           'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

       LINKAGE SECTION.

      *-----------------------------------------------------------------
      * These linkage variables are the input/output variables for
      * the stub program.
      *-----------------------------------------------------------------
       1 LS-INPUT-LETTER       PIC X(1).
       1 LS-OUTPUT-ANIMAL-NAME PIC X(10).

       PROCEDURE DIVISION.

      ******************************************************************
      * Register the test to be run.                                   *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'stubbedAnimalProgramTest'
           MOVE 'Stub ZTPCALLD program test (mix of live and stubbed)' 
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

      *-----------------------------------------------------------------
      * Once all of the tests have been registered, return back to
      * Test4z to start processing.
      *-----------------------------------------------------------------
           GOBACK.

      ******************************************************************
      * UNIT TEST: Provide the callback routine entry point for the    *
      *            unit test. This test will execute a user program    *
      *            that calls another program. The SUT's invocations   *
      *            of ZTPCALLD will be replaced by our program logic   *
      *            in the callback 'ZTPCALLD_stubProgramCallback'      *
      *            below.                                              *
      ******************************************************************
           ENTRY 'stubbedAnimalProgramTest'.

      *-----------------------------------------------------------------
      * Define the program ZTPCALLD that is to be prevented from being 
      * called in the original load module, but instead to be
      * redirected to our stub routine located in this test.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_STUBPROGRAM
           MOVE 'ZTPPROGR' TO MODULENAME IN ZWS_STUBPROGRAM
           MOVE 'ZTPCALLD' TO FUNCTIONNAME IN ZWS_STUBPROGRAM
           SET STUBROUTINE IN ZWS_STUBPROGRAM
                TO ENTRY 'ZTPCALLD_stubProgramCallback'
           CALL ZTESTUT USING ZWS_STUBPROGRAM

      *-----------------------------------------------------------------
      * Load and prepare the user application program ZTPPROGR for use.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPPROGR' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

      *-----------------------------------------------------------------
      * Get the entry point of the ZTPPROGR routine in load module
      * ZTPPROGR.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTPPROGR' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTPPROGR' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, RUN-PROGRAM

      *-----------------------------------------------------------------
      * Start the ZTPPROGR function. When it calls ZTPCALLD, those
      * calls will be diverted to the ZTPCALLD_stubProgramCallback
      * entry point located below. 
      *-----------------------------------------------------------------
           CALL RUN-PROGRAM

      *-----------------------------------------------------------------
      * The SUT has ended.
      *
      * Assert the values of the various metrics are as expected.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTSTUBP tested ' WS-INPUT-LETTERS-VOWEL-CNT
                ' vowels and ' WS-INPUT-LETTERS-CONSONANT-CNT
                ' consonants with ' WS-INPUT-LETTERS-INVALID-CNT
                ' invalid'

           IF WS-INPUT-LETTERS-INVALID-CNT NOT = 1
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTSTUBP invalid input'
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                CALL ZTESTUT USING ZWS_FAIL
           END-IF

      *-----------------------------------------------------------------
      * The callback changed each called letter in the variable below
      * into a space; if the coverage was 100%, the variable should
      * be all SPACES.
      *-----------------------------------------------------------------
           IF WS-INPUT-LETTERS-UNTESTED NOT = SPACES
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE SPACES TO FAILMESSAGE IN ZWS_FAIL
                STRING 'ZTTSTUBP missing test for some input: '
                     WS-INPUT-LETTERS-UNTESTED DELIMITED BY SIZE
                     INTO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                CALL ZTESTUT USING ZWS_FAIL
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: This is the callback routine entry point for the     *
      *           stubbed subroutine originally called ZTPCALLD in the *
      *           user program, but diverted to our entry point below. *
      *                                                                *
      *           The ZTPCALLD program receives two arguments,         *
      *           therefore so will our stub. The stub has the option  *
      *           of calling the original program and modifying the    *
      *           result. This example demonstrates both replacing     *
      *           the called program result and modifying it.          *
      ******************************************************************
           ENTRY 'ZTPCALLD_stubProgramCallback' 
                USING LS-INPUT-LETTER, LS-OUTPUT-ANIMAL-NAME.

      *-----------------------------------------------------------------
      * Change the corresponding letter to space in the variable
      * used to confirm 100% coverage of all letters, A-Z.
      *-----------------------------------------------------------------
           IF LS-INPUT-LETTER >= 'A' AND LS-INPUT-LETTER <= 'Z'
                PERFORM VARYING TALLY FROM 1 BY 1 UNTIL TALLY > 26
                     IF LS-INPUT-LETTER = 
                               WS-INPUT-LETTERS-UNTESTED(TALLY:1)
                          MOVE SPACE
                               TO WS-INPUT-LETTERS-UNTESTED(TALLY:1)
                          EXIT PERFORM
                     END-IF
                END-PERFORM
           END-IF

      *-----------------------------------------------------------------
      * For the vowels, call the original ZTPCALLD and modify its
      * result. For the consonants, simply uppercase the input
      * letter. For invalid input, return '?'.
      *-----------------------------------------------------------------
           IF LS-INPUT-LETTER = 'A'
                     OR LS-INPUT-LETTER = 'E'
                     OR LS-INPUT-LETTER = 'I'
                     OR LS-INPUT-LETTER = 'O'
                     OR LS-INPUT-LETTER = 'U'

                ADD 1 TO WS-INPUT-LETTERS-VOWEL-CNT

      *-----------------------------------------------------------------
      * Test4z requires NODYNAM because the entry point names of the
      * unit test (i.e., this program) must be statically defined
      * so they are addressable by the runtime. However, the called
      * called program's entry point (ZTPCALLD) can be invoked 
      * dynamically by adding the compiler directive below or
      * using a non-literal (e.g., CALL WS-ZTPCALLD-FUNCTION).
      *-----------------------------------------------------------------
                >>CALLINTERFACE DYNAMIC
                CALL 'ZTPCALLD' USING 
                     LS-INPUT-LETTER, LS-OUTPUT-ANIMAL-NAME
                >>CALLINTERFACE STATIC

                MOVE LS-OUTPUT-ANIMAL-NAME TO WS-ANIMAL-NAME-COPY
                MOVE FUNCTION LOWER-CASE(LS-OUTPUT-ANIMAL-NAME(2:9))
                     TO LS-OUTPUT-ANIMAL-NAME(2:9)

                DISPLAY 'ZTTSTUBP changed ZTPCALLD output for '
                     LS-INPUT-LETTER ' from '
                     WS-ANIMAL-NAME-COPY ' to ' LS-OUTPUT-ANIMAL-NAME
           ELSE
                MOVE SPACES TO LS-OUTPUT-ANIMAL-NAME

                IF LS-INPUT-LETTER >= 'A' AND LS-INPUT-LETTER <= 'Z'
                     ADD 1 TO WS-INPUT-LETTERS-CONSONANT-CNT
                     MOVE FUNCTION LOWER-CASE(LS-INPUT-LETTER) 
                          TO LS-OUTPUT-ANIMAL-NAME(1:1)
                ELSE
                     ADD 1 TO WS-INPUT-LETTERS-INVALID-CNT
                     MOVE ALL '?' TO LS-OUTPUT-ANIMAL-NAME
                END-IF

                DISPLAY 'ZTTSTUBP replaced ZTPCALLD output for '
                     LS-INPUT-LETTER ' with ' LS-OUTPUT-ANIMAL-NAME
           END-IF

           GOBACK.
           