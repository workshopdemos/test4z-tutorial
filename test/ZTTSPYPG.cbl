       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTSPYPG' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This unit test example shows how to "spy" on programs that     *
      * invoke other programs as subroutines using the Test4z          *
      * _SpyProgram API.                                               *
      *                                                                *
      * This program spy's callback will be invoked after the called   *
      * program (ZTPCALLD) has returned its result. The spy callback   *
      * can simply observe and test the input/output, or can modify    *
      * the returned result that the caller would otherwise get.       *
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
      * ZWS_SPYPROGRAM -> I_SPYPROGRAM IN ZWS_SPYPROGRAM               *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
       COPY ZTESTWS.

      *-----------------------------------------------------------------
      * Define any working-storage items that we need.
      *-----------------------------------------------------------------
       01  WS-RUN-PROGRAM         USAGE FUNCTION-POINTER.
       01  WS-RETURN-CODE-SUT     PIC S9(4) USAGE BINARY.
       01  WS-EVERY-OTHER-FLAG    PIC 9(1) VALUE 0.
           88 WS-IS-EVEN VALUE 0.
           88 WS-IS-ODD  VALUE 1.
       01  I                      PIC 9(2).
       01  J                      PIC 9(2).

      ******************************************************************
      * For those APIs with return values, the copybook block contains *
      * an output field (e.g., ZSPPGM -> PROGRAMSPYOBJECT). These      *
      * copybooks only define one control block and they start at the  *
      * 03 level, so you can define your own 01 level variable.        *
      ******************************************************************
       01  WS-ZSPPGM-CALLD-ANIMAL-SPY.
           COPY ZSPPGM.

       01  WS-CALLD-ANIMAL-CALL-HISTORY.
           05 WS-INPUT-LETTER       PIC X(1).
           05 WS-OUTPUT-ANIMAL-NAME PIC X(10).

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

      *-----------------------------------------------------------------
      * These are the input/output parameters for ZTPCALLD that are
      * queried during the program spy callback.
      *-----------------------------------------------------------------
       01  LS-CALLD-INPUT-LETTER          PIC X(1).
       01  LS-CALLD-OUTPUT-ANIMAL-NAME    PIC X(10).

      *-----------------------------------------------------------------
      * The spy history is a concatenated list of the input/results
      * intercepted during execution. The record below defines the
      * format of a INPUT-LETTER/ANIMAL-NAME call parameter invocation;
      * it's used to more easily display the spy history without
      * pointer manipulations.
      *-----------------------------------------------------------------
       01  LS-CALLD-PROGRAM-CALL-HISTORY  PIC X(11).

      *-----------------------------------------------------------------
      * This is the program spy callback parameter provided to the
      * callback to provide context for the invocation. This is helpful
      * if the entry point callback is shared by multiple spies.
      *
      * See ZTPCALLD_spyCallback for usage.
      *-----------------------------------------------------------------
       01  LS-ZSPPGM-CALLD-PROGRAM-SPY.
           COPY ZSPPGM.

       PROCEDURE DIVISION.

      ******************************************************************
      * Register the unit test. It will be executed after the SUT      *
      * is prepared to run.                                            *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'animalSpyProgramTest'
           MOVE 'Spy on ZTPCALLD subroutine test'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           GOBACK.

      ******************************************************************
      * UNIT TEST: This test will execute a user program that calls    *
      *            another program. Let's spy on it.                   *
      ******************************************************************
           ENTRY 'animalSpyProgramTest'.

      *-----------------------------------------------------------------
      * Define a program spy that will intercept calls to
      * the ZTPCALLD program that maps letters to animal names.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYPROGRAM
           MOVE 'ZTPPROGR' TO MODULENAME IN ZWS_SPYPROGRAM
           MOVE 'ZTPCALLD' TO FUNCTIONNAME IN ZWS_SPYPROGRAM
           SET CALLBACK IN ZWS_SPYPROGRAM
                TO ENTRY 'ZTPCALLD_spyCallback'
           CALL ZTESTUT USING ZWS_SPYPROGRAM,
                PROGRAMSPYOBJECT IN WS-ZSPPGM-CALLD-ANIMAL-SPY

      *-----------------------------------------------------------------
      * Load and prepare the user application program ZTPPROGR,
      * then get the module entry point of the SUT.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPPROGR' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTPPROGR' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTPPROGR' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, WS-RUN-PROGRAM

      *-----------------------------------------------------------------
      * Start the ZTPPROGR program. When it calls ZTPCALLD, those
      * calls will be diverted to the ZTPCALLD_spyCallback entry point
      * located below.
      *-----------------------------------------------------------------
           CALL WS-RUN-PROGRAM
           MOVE RETURN-CODE TO WS-RETURN-CODE-SUT

      *-----------------------------------------------------------------
      * The SUT has ended.
      * 
      * Display the program spy's captured history; these were copied
      * by the spy into local UT memory and can be used to validate
      * program execution.
      *
      * The spy history includes both the before and after; for this
      * example, we'll only show the after.
      *
      * NB: The Test4z API _DisplayProgram produces similar output
      *     for debugging purposes.
      *-----------------------------------------------------------------
           MOVE SIZE_ IN CALLS IN WS-ZSPPGM-CALLD-ANIMAL-SPY TO J
           SET ADDRESS OF ZLS_PROGRAM_HISTORIES
                TO PTR IN CALLS IN WS-ZSPPGM-CALLD-ANIMAL-SPY

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
                SET ADDRESS OF LS-CALLD-PROGRAM-CALL-HISTORY
                     TO VALUE_ IN RESULTS IN ZLS_PROGRAM_HISTORY(I)
                MOVE LS-CALLD-PROGRAM-CALL-HISTORY
                     TO WS-CALLD-ANIMAL-CALL-HISTORY

                IF I = J
                     DISPLAY 'ZTTSPYPG [LASTCALL] '
                          I ' of ' J '=' 
                          WS-INPUT-LETTER ':' WS-OUTPUT-ANIMAL-NAME
                ELSE
                     DISPLAY 'ZTTSPYPG [HISTORY]  '
                          I ' of ' J '='
                          WS-INPUT-LETTER ':' WS-OUTPUT-ANIMAL-NAME
                END-IF
           END-PERFORM

      *-----------------------------------------------------------------
      * The program under test intentionally provides an invalid
      * value to its called program, ZTPCALLD, and returns an 
      * error. Verify this was detected and reported.
      *-----------------------------------------------------------------
           IF WS-RETURN-CODE-SUT NOT = 4
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTSPYPG unexpected return code from ZTPPROGR'
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=4'
                DISPLAY '     Got=' WS-RETURN-CODE-SUT

                CALL ZTESTUT USING ZWS_FAIL
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: This entry point is invoked for the spy on the       *
      *           called program. Spies typically only observe/capture *
      *           values for later validation, but they can also       *
      *           modify the result returned to the SUT.               *
      ******************************************************************
           ENTRY 'ZTPCALLD_spyCallback' 
                USING LS-ZSPPGM-CALLD-PROGRAM-SPY.

      *-----------------------------------------------------------------
      * The spy is provided with the same parameters available to the
      * callee. There can be 0-n number of parameters, and
      * they're retrieveable with the _GETPROGRAMARGUMENT API.
      *
      * The called program ZTPCALLD accepts one input and one
      * output parameter; get them and map their addresses.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETPROGRAMARGUMENT 
           MOVE 1 TO ARGUMENTINDEX IN ZWS_GETPROGRAMARGUMENT 

           CALL ZTESTUT USING ZWS_GETPROGRAMARGUMENT,
                ADDRESS OF LS-CALLD-INPUT-LETTER

           DISPLAY 'ZTTSPYPG input=' LS-CALLD-INPUT-LETTER

           MOVE LOW-VALUES TO I_GETPROGRAMARGUMENT 
           MOVE 2 TO ARGUMENTINDEX IN ZWS_GETPROGRAMARGUMENT 
           CALL ZTESTUT USING ZWS_GETPROGRAMARGUMENT,
                ADDRESS OF LS-CALLD-OUTPUT-ANIMAL-NAME

      *-----------------------------------------------------------------
      * It's a trivial example, but demonstrate how the program spy
      * can change the returned result, in this case, for every other
      * call or 'Z' => 'ZEBU'. The changed value will also be
      * captured in the spy's history.
      *
      * This feature can be used in a unit test to force "unhappy paths"
      * for a program's called subroutine, thereby testing the SUT more
      * thoroughly.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTSPYPG original output=' 
                LS-CALLD-OUTPUT-ANIMAL-NAME

           IF LS-CALLD-INPUT-LETTER = 'Z'
                MOVE SPACES TO LS-CALLD-OUTPUT-ANIMAL-NAME
                MOVE 'ZEBU' TO LS-CALLD-OUTPUT-ANIMAL-NAME 
           ELSE 
                IF WS-IS-EVEN
                     MOVE FUNCTION 
                          LOWER-CASE(LS-CALLD-OUTPUT-ANIMAL-NAME)
                          TO LS-CALLD-OUTPUT-ANIMAL-NAME
                     SET WS-IS-ODD TO TRUE
                ELSE
                     SET WS-IS-EVEN TO TRUE
                END-IF
           END-IF

           DISPLAY 'ZTTSPYPG output returned=' 
                LS-CALLD-OUTPUT-ANIMAL-NAME 

           GOBACK.
           