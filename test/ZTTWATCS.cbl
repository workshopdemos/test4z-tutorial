       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTWATCS' RECURSIVE.
       
      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This unit test example shows how to "spy" on variable values   *
      * the system under test.                                         *
      *                                                                *
      * Spying on a variable (_SpyVariable) and watching a             *
      * variable (_WatchVariable) are similar APIs, but they have      *
      * different uses:                                                *
      *                                                                *
      * == Variable spies automatically capture a copy of the variable *
      *    changes, allowing for the list of changed variable          *
      *    values to be referenced after the SUT's execution.          *
      *                                                                *
      * == Variable spies are intended for accessing a variable after  *
      *    the container program ends, to ensure that the values are   *
      *    correct.                                                    *
      *                                                                *
      * The variable spy framework is built on the watch variable      *
      * APIs:                                                          *
      *                                                                *
      * == Like variable spies, watch spies have real-time access      *
      *    when a variable changes.                                    *
      *                                                                *
      * == Watch spies are most appropriate for observing a variable   *
      *    while the program under test is still active. It allows     *
      *    the changing of the variable while the program is           *
      *    executing.                                                  *
      *                                                                *
      * == If you simply want to change a variable during a callback   *
      *    of a section breakpoint or similar, use the _GetVariable    *
      *    API to retrieve the variable's address and modify it        *
      *    directly.                                                   *
      *                                                                *
      * NB: The real-time access may not be at the exact instruction   *
      *     that alters the variable, but the first time that the      *
      *     change is detected.                                        *
      *                                                                *
      * For the complete list of Test4z APIs and helpful reminders     *
      * when writing a unit test suite, see the Test4z COBOL API       *
      * document (PDF). The APIs are also documente in the copybook    *
      * ZTESTWSn.cpy where "n" is the document revision number.        *   
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
      * ZWS_SPYVARIABLE -> I_SPYVARIABLE IN ZWS_SPYVARIABLE            *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
       COPY ZTESTWS.

      ******************************************************************
      * For those APIs with return values, the copybook block contains *
      * an output field (e.g., ZSPVAR -> VARIABLESPYOBJECT). These     *
      * copybooks only define one control block and they start at the  *
      * 03 level, so you can define your own 01 level variable.        *
      ******************************************************************
       01  WS-ZSPVAR-COUNTER.
           COPY ZSPVAR.

       01  WS-ZSPVAR-RECORD.
           COPY ZSPVAR.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       01  RUN-PROGRAM            USAGE FUNCTION-POINTER.

       01  WS-RECORD-CALLBACK-CNT PIC 9(4) VALUE 0.
       01  WS-RECORD-FINAL        PIC X(80)
                                  VALUE '06 Counter incremented'.
       01  WS-FAILED-TESTS        PIC 9(2).                                  
       01  I                      PIC 9(4).
       01  J                      PIC 9(4).

       LINKAGE SECTION.
      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

       01  LS-RECORD             PIC X(80).
       01  LS-COUNTER            PIC 9(4).

      *-----------------------------------------------------------------
      * This is the Spy Variable callback parameter.
      * See mySpyRecordCallback for usage.
      *-----------------------------------------------------------------
       01  LS-ZSPVAR-RECORD-CALLBACK.
           COPY ZSPVAR.

       PROCEDURE DIVISION.

      ******************************************************************
      * Register the test to be run.                                   *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'mySpyVariableTest'
           MOVE 'Spy variables for record and counter'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           GOBACK.

      ******************************************************************
      * UNIT TEST: Provide the callback routine entry point for the    *
      *            unit test. While the tested program executes,       *
      *            the spied variable will be tracked and can be       *
      *            validated once the SUT ends.                        *
      ******************************************************************
           ENTRY 'mySpyVariableTest'.

      *-----------------------------------------------------------------
      * Load and prepare the user application program ZTPWATCH for use.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

      *-----------------------------------------------------------------
      * Now that the module has been prepared, create a spy for
      * the WS-COUNTER and WS-RECORD variables.
      *
      * For WS-COUNTER, validation is against the final value of the
      * variable, comparing its expected and actual value.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYVARIABLE
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_SPYVARIABLE
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_SPYVARIABLE
           MOVE 'WS-COUNTER' TO VARIABLENAME IN ZWS_SPYVARIABLE
           CALL ZTESTUT USING ZWS_SPYVARIABLE,
                VARIABLESPYOBJECT IN WS-ZSPVAR-COUNTER

      *-----------------------------------------------------------------
      * The callback is optional; if specified, the unit test will be
      * notified whenever the named variable changes. Otherwise, the
      * values will be captured by the spy and can be validated when
      * the SUT ends.

      * For WS-RECORD, validation is against the expected and final
      * value. Since there's a callback (mySpyRecordCallback), the
      * validation includes the number of modifications by the
      * program under test (e.g., an unusually high number of changes
      * could indicate a coding problem or even security-related
      * problem for sensitive data).
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYVARIABLE
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_SPYVARIABLE
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_SPYVARIABLE
           MOVE 'WS-RECORD' TO VARIABLENAME IN ZWS_SPYVARIABLE
           SET CALLBACK IN ZWS_SPYVARIABLE
                TO ENTRY 'mySpyRecordCallback'
           CALL ZTESTUT USING ZWS_SPYVARIABLE,
                VARIABLESPYOBJECT IN WS-ZSPVAR-RECORD

      *-----------------------------------------------------------------
      * Get the entry point of the ZTPWATCH routine in load module
      * ZTPWATCH.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, RUN-PROGRAM

      *-----------------------------------------------------------------
      * Start the ZTPWATCH program. When a change in the spied
      * variable WS-RECORD is detected, the mySpyRecordCallback entry
      * point will be called.
      *-----------------------------------------------------------------
           MOVE 0 TO WS-FAILED-TESTS      
           CALL RUN-PROGRAM

      *-----------------------------------------------------------------
      * The SUT has ended.
      *
      * Assert the values of the WS-RECORD and WS-COUNTER variables;
      * these were copied by the variable spies into local UT memory.
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_VARIABLE_CALL
                TO LASTCALL IN WS-ZSPVAR-RECORD
           SET ADDRESS OF LS-RECORD
                TO PTR IN ZLS_VARIABLE_CALL
           DISPLAY 'ZTTWATCS final WS-RECORD value=' LS-RECORD

           SET ADDRESS OF ZLS_VARIABLE_CALL
                TO LASTCALL IN WS-ZSPVAR-COUNTER
           SET ADDRESS OF LS-COUNTER
                TO PTR IN ZLS_VARIABLE_CALL
           DISPLAY 'ZTTWATCS final WS-COUNTER value=' LS-COUNTER

      *-----------------------------------------------------------------
      * WS-RECORD: Verify the number of updates.
      *-----------------------------------------------------------------
           IF SIZE_ IN WS-ZSPVAR-RECORD NOT = 5
                ADD 1 TO WS-FAILED-TESTS

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE SPACES TO MESSAGETEXT IN ZWS_MESSAGE
                MOVE 'ZTTWATCS for update count mismatch WS-RECORD'
                     TO MESSAGETEXT IN ZWS_MESSAGE

                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY 'Expected=5'
                DISPLAY '     Got=' SIZE_ IN WS-ZSPVAR-RECORD

                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

      *-----------------------------------------------------------------
      * Verify the final value of WS-RECORD.
      *-----------------------------------------------------------------
           IF LS-RECORD NOT = WS-RECORD-FINAL
                ADD 1 TO WS-FAILED-TESTS

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE SPACES TO MESSAGETEXT IN ZWS_MESSAGE
                MOVE 'ZTTWATCS value mismatch for WS-RECORD '
                     TO MESSAGETEXT IN ZWS_MESSAGE

                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY 'Expected=' WS-RECORD-FINAL
                DISPLAY '     Got=' LS-RECORD

                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

      *-----------------------------------------------------------------
      * Display the spy history of WS-RECORD.
      *-----------------------------------------------------------------
           MOVE SIZE_ IN CALLS IN WS-ZSPVAR-RECORD TO J
           SET ADDRESS OF ZLS_VARIABLE_HISTORIES
                TO PTR IN CALLS IN WS-ZSPVAR-RECORD

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
                SET ADDRESS OF LS-RECORD
                     TO PTR IN DATA_ IN ZLS_VARIABLE_HISTORY(I)

                IF I = J
                     DISPLAY 'ZTTWATCS WS-RECORD [LASTCALL] '
                          I ' of ' J '=' LS-RECORD
                ELSE
                     DISPLAY 'ZTTWATCS WS-RECORD [HISTORY] '
                          I ' of ' J '=' LS-RECORD
                END-IF
           END-PERFORM

      *-----------------------------------------------------------------
      * WS-COUNTER: Verify the number of updates.
      *-----------------------------------------------------------------
            IF SIZE_ IN CALLS IN WS-ZSPVAR-COUNTER NOT = 6
                ADD 1 TO WS-FAILED-TESTS

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE SPACES TO MESSAGETEXT IN ZWS_MESSAGE
                MOVE 'ZTTWATCS update mismatch for WS-COUNTER'
                     TO MESSAGETEXT IN ZWS_MESSAGE

                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY 'Expected=6'
                DISPLAY '     Got=' SIZE_ IN CALLS IN WS-ZSPVAR-COUNTER

                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

      *-----------------------------------------------------------------
      * Display the spy history of WS-COUNTER.
      *-----------------------------------------------------------------
           MOVE SIZE_ IN CALLS IN WS-ZSPVAR-COUNTER TO J
           SET ADDRESS OF ZLS_VARIABLE_HISTORIES
                TO PTR IN CALLS IN WS-ZSPVAR-COUNTER

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
                SET ADDRESS OF LS-COUNTER TO
                    PTR IN ZLS_VARIABLE_HISTORY(I)

                IF I = J
                     DISPLAY 'ZTTWATCS WS-COUNTER [LASTCALL] '
                          I ' of ' J '=' LS-COUNTER
                ELSE
                     DISPLAY 'ZTTWATCS WS-COUNTER [HISTORY] '
                          I ' of ' J '=' LS-COUNTER
                END-IF
           END-PERFORM

      *-----------------------------------------------------------------
      * Verify the final value of the program under test's WS-COUNTER
      * (it is referenced by the unit test via the LS-COUNTER linkage
      * section variable).
      *
      * NB: This spy had no callback registered, but the spy history
      *     records all changes. As a convenience, the LASTCALL field
      *     points to the last one in the spy history since, in most
      *     cases, that's what we're most interested in -- the final
      *     value.
      *-----------------------------------------------------------------
           IF LS-COUNTER NOT = 6
                ADD 1 TO WS-FAILED-TESTS

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE SPACES TO MESSAGETEXT IN ZWS_MESSAGE
                MOVE 'ZTTWATCS value mismatch for WS-COUNTER'
                     TO MESSAGETEXT IN ZWS_MESSAGE

                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY 'Expected=6'
                DISPLAY '     Got=' LS-COUNTER

                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

      *-----------------------------------------------------------------
      * A quick sanity check for SpyVariable callbacks. This should
      * be the same value as SIZE_ IN CALLS of the SpyVariable
      * control block representing the spy history.
      *-----------------------------------------------------------------
           IF WS-RECORD-CALLBACK-CNT NOT = 5
                ADD 1 TO WS-FAILED-TESTS

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE SPACES TO MESSAGETEXT IN ZWS_MESSAGE
                MOVE 'ZTTWATCS callback count mismatch for WS-RECORD'
                     TO MESSAGETEXT IN ZWS_MESSAGE

                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY 'Expected=5'
                DISPLAY '     Got=' WS-RECORD-CALLBACK-CNT

                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

      *-----------------------------------------------------------------
      * Calling _FAIL ends the unit test immediately; since the
      * validations above were potentially cumulative, only messages
      * were logged unti the end of the unit test below.
      *-----------------------------------------------------------------
           IF WS-FAILED-TESTS > 0
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTWATCS test(s) failed, refer to ZLMSG'
                     TO FAILMESSAGE IN ZWS_FAIL
                CALL ZTESTUT USING ZWS_FAIL
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: This entry point is called whenever a change to      *
      *           the spied variable WS-RECORD has been detected.      *
      *                                                                *
      *           For variable spies, the callback routines are always *
      *           called on the "after" side of the related change,    *
      *           e.g., after watch spies are triggered.               *
      ******************************************************************
           ENTRY 'mySpyRecordCallback'
                USING LS-ZSPVAR-RECORD-CALLBACK.

           ADD 1 TO WS-RECORD-CALLBACK-CNT

      *-----------------------------------------------------------------
      * This callback is spying on variable WS_RECORD. Since that's
      * the only registered variable, no other is expected and
      * should be treated as a UT coding error (i.e.,
      * 'mySpyRecordCallback' was inadvertently specified for multiple
      * _SpyVariable invocations).
      *-----------------------------------------------------------------
           IF VARIABLENAME IN LS-ZSPVAR-RECORD-CALLBACK = 'WS-RECORD'
                SET ADDRESS OF ZLS_VARIABLE_CALL TO
                     LASTCALL IN LS-ZSPVAR-RECORD-CALLBACK
                SET ADDRESS OF LS-RECORD TO
                     PTR IN ZLS_VARIABLE_CALL
                DISPLAY 'ZTTWATCS mySpyRecordCallback #'
                     WS-RECORD-CALLBACK-CNT
                     ' WS-RECORD=' LS-RECORD
           ELSE
                ADD 1 TO WS-FAILED-TESTS

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                STRING 'ZTTWATCS mySpyRecordCallback unexpected call: '
                     VARIABLENAME IN LS-ZSPVAR-RECORD-CALLBACK
                     DELIMITED BY SIZE INTO MESSAGETEXT IN ZWS_MESSAGE

                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

           GOBACK.
