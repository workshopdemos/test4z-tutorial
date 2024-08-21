       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTWATCH' RECURSIVE.
       
      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to "watch" variable values of the  *
      * system under test. It also shows how the unit test can modify  *
      * the SUT's variables in real-time.                              *
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
      *    the program under test ends, to ensure that the values are  *
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
      * For the complete list of Test4z APIs and helpful reminders     *
      * when writing a unit test suite, see the Test4z COBOL API       *
      * document (PDF). The APIs are also documente in the copybook    *
      * ZTESTWSn.cpy where "n" is the document revision number.        *   
      *                                                                *
      * NB: The real-time access may not be at the exact instruction   *
      *     that alters the variable, but the first time that the      *
      *     change is detected.                                        *
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
      * ZWS_WATCHVARIABLE -> I_WATCHVARIABLE IN ZWS_WATCHVARIABLE      *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
       COPY ZTESTWS.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       01  RUN-PROGRAM         USAGE FUNCTION-POINTER.
       
       01  WS-RECORD-COPY      PIC X(80).
       01  WS-RECORD-COPY-FORMATTED REDEFINES WS-RECORD-COPY.
           05 RECORD-NUM       PIC 9(2).
           05 FILLER           PIC X.
           05 RECORD-MESSAGE   PIC X(77).
       01  WS-RETURN-CODE-SUT  PIC S9(4) COMP-4.
       01  WS-RETURN-CODE-DISP PIC 9(2).
       01  WS-REMAINDER        PIC 9(2).
       01  WS-ORIGINAL-DATE.
           05 OD-YEAR          PIC 9(4).
           05 OD-MONTH         PIC 9(2).
           05 OD-DAY           PIC 9(2).
           05 OD-HOUR          PIC 9(2).
           05 OD-MINUTE        PIC 9(2).
       01  WS-FAILED-TESTS     PIC 9(2).

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

      ******************************************************************
      * Define any linkage section items that we need.                 *
      ******************************************************************
       01  LS-RECORD-FORMATTED.
           05 RECORD-NUM      PIC 9(2).
           05 FILLER          PIC X.
           05 RECORD-MESSAGE  PIC X(77).
       01  LS-RETURN-CODE-SUT PIC S9(4) COMP-4.

       01  LS-CURRENT-DATE.
           05 CD-YEAR         PIC 9(4).
           05 CD-MONTH        PIC 9(2).
           05 CD-DAY          PIC 9(2).
           05 CD-HOUR         PIC 9(2).
           05 CD-MINUTE       PIC 9(2).

       PROCEDURE DIVISION.

      ******************************************************************
      * Register the test to be run.                                   *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myWatchVariableTest'
           MOVE 'Watch variables for records, dates, etc.'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           GOBACK.

      ******************************************************************
      * UNIT TEST: Provide the callback routine entry point for the    *
      *            unit test. While the tested program executes,       *
      *            the watched variable will be checked, and           *
      *            if a change is detected, a callback occurs.         *
      ******************************************************************
           ENTRY 'myWatchVariableTest'.

      *-----------------------------------------------------------------
      * Load and prepare the user application program ZTPWATCH for use.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

      *-----------------------------------------------------------------
      * Now that the module has been prepared, create a watch for 
      * some variables. The same entry point 'myWatchVariableCallback'
      * is specified for several variables, so the callback will
      * need to check which variable has changed.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_WATCHVARIABLE
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_WATCHVARIABLE
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_WATCHVARIABLE
           SET CALLBACK IN ZWS_WATCHVARIABLE
                TO ENTRY 'myWatchVariableCallback'

      *-----------------------------------------------------------------
      * The ZWS_WATCHVARIABLE block was initialized - except for the
      * variable name. Set it and call ZTESTUT for the variables
      * that will be monitored or changed.
      *
      * Watched variables: WS-RECORD, RETURN-CODE, WS-CURRENT-DATE
      * Modified variable: WS-CURRENT-DATE (forced to fixed date)
      *-----------------------------------------------------------------
           MOVE 'WS-RECORD' TO VARIABLENAME IN ZWS_WATCHVARIABLE
           CALL ZTESTUT USING ZWS_WATCHVARIABLE

           MOVE 'RETURN-CODE' TO VARIABLENAME IN ZWS_WATCHVARIABLE
           CALL ZTESTUT USING ZWS_WATCHVARIABLE

           MOVE 'WS-CURRENT-DATE' TO VARIABLENAME IN ZWS_WATCHVARIABLE
           CALL ZTESTUT USING ZWS_WATCHVARIABLE

      *-----------------------------------------------------------------
      * Get the entry point of the ZTPWATCH routine in load module
      * ZTPWATCH.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, RUN-PROGRAM

      *-----------------------------------------------------------------
      * Start the ZTPWATCH program. When a watched variable change is
      * detected, the myWatchVariableCallback entry point is called.
      *-----------------------------------------------------------------
           MOVE 0 TO WS-FAILED-TESTS
           CALL RUN-PROGRAM

      *-----------------------------------------------------------------
      * The SUT has ended. Assert the value of the variable WS-RECORD
      * and RETURN-CODE at the end of the ZTPWATCH program.
      *
      * Note the RECORD-NUM for this final record was modified by
      * the unit test's callback (it was 6 and was changed to 16). See
      * myWatchVariableCallback / WS-RECORD for details.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTWATCH WS-RECORD = ' WS-RECORD-COPY

           IF RECORD-NUM IN WS-RECORD-COPY-FORMATTED NOT = 16
                ADD 1 TO WS-FAILED-TESTS
                
                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                STRING 'ZTTWATCH WS-RECORD-NUM expected 16, got '
                     RECORD-NUM IN WS-RECORD-COPY-FORMATTED
                     DELIMITED BY SIZE INTO MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

           MOVE WS-RETURN-CODE-SUT TO WS-RETURN-CODE-DISP
           DISPLAY 'ZTTWATCH WS-RETURN-CODE-SUT = ' WS-RETURN-CODE-DISP

           IF WS-RETURN-CODE-SUT NOT = 4 THEN
                ADD 1 TO WS-FAILED-TESTS

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                STRING 'ZTTWATCH RETURN-CODE expected 04, got '
                     WS-RETURN-CODE-DISP
                     DELIMITED BY SIZE INTO MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                CALL ZTESTUT USING ZWS_MESSAGE
           ELSE
                DISPLAY 'ZTTWATCH PASS on ZTPWATCH''s RETURN-CODE'
           END-IF

      *-----------------------------------------------------------------
      * Calling _FAIL ends the unit test immediately; since the
      * validations above were potentially cumulative, only messages
      * were logged unti the end of the unit test below.
      *-----------------------------------------------------------------
           IF WS-FAILED-TESTS > 0
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTWATCH test(s) failed, refer to ZLMSG'
                     TO FAILMESSAGE IN ZWS_FAIL
                CALL ZTESTUT USING ZWS_FAIL
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: This entry point is called whenever a change to      *
      *           the watched variable has been detected.              *
      *                                                                *
      * Input:                                                         *
      *   ZLS_GOBLOCK   = global block                                 *
      *   ZLS_W_ITBLOCK = item block                                   *
      *   ZLS_W_IFBLOCK = interface block                              *
      ******************************************************************
           ENTRY 'myWatchVariableCallback' 
                USING ZLS_GOBLOCK, ZLS_W_ITBLOCK, ZLS_W_IFBLOCK.

      *-----------------------------------------------------------------
      * This callback is watching variables WS_RECORD, RETURN-CODE,
      * and WS-CURRENT-DATE.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTWATCH watched variable=' 
                VARIABLENAME IN ZLS_W_ITBLOCK
           DISPLAY 'ZTTWATCH change occurred at offset='
                CHANGEDATOFFSET IN ZLS_W_IFBLOCK
                ' in program SECTION/PARAGRAPH='
                CURRENTSECTION IN ZLS_W_IFBLOCK
     
      *-----------------------------------------------------------------
      * For this scenario, let's assume the ZTPWATCH program should
      * only modify the record within a specific section, otherwise
      * it's a programming error.
      *-----------------------------------------------------------------
           IF VARIABLENAME IN ZLS_W_ITBLOCK = 'WS-RECORD'
                IF CURRENTSECTION IN 
                          ZLS_W_IFBLOCK NOT = '210-INCREMENT-COUNTER'
                     MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                     STRING 
                          'ZTTWATCH Unexpected change to WS-RECORD in '
                          CURRENTSECTION IN ZLS_W_IFBLOCK
                          DELIMITED BY SIZE INTO FAILMESSAGE IN ZWS_FAIL
                     DISPLAY FAILMESSAGE IN ZWS_FAIL
                     CALL ZTESTUT USING ZWS_FAIL
                END-IF

      *-----------------------------------------------------------------
      * Display the previous record...
      *
      * Note: For the first invocation of this callback, this is the
      * variable value from working storage. If the variable wasn't
      * initialized with a VALUE, it will be uninitialized storage.
      *-----------------------------------------------------------------
                SET ADDRESS OF LS-RECORD-FORMATTED
                     TO PREVIOUSADDRESS IN ZLS_W_IFBLOCK
                DISPLAY 'ZTTWATCH previous WS-RECORD='
                     LS-RECORD-FORMATTED

      *-----------------------------------------------------------------
      * And now display the current record.
      *-----------------------------------------------------------------
                SET ADDRESS OF LS-RECORD-FORMATTED
                     TO CURRENTADDRESS IN ZLS_W_IFBLOCK

                DISPLAY 'ZTTWATCH current WS-RECORD='
                     LS-RECORD-FORMATTED

      *-----------------------------------------------------------------
      * Modify every other record within the watch spy, demonstrating
      * that it can be done, similar to the _GetVariable API.
      *-----------------------------------------------------------------
                COMPUTE WS-REMAINDER = FUNCTION MOD
                     (RECORD-NUM IN LS-RECORD-FORMATTED, 2)
                IF WS-REMAINDER = 0
                     ADD 10 TO RECORD-NUM IN LS-RECORD-FORMATTED
                     MOVE 'Modified by ZTTWATCH'
                          TO RECORD-MESSAGE in LS-RECORD-FORMATTED
                     DISPLAY 'ZTTWATCH modified WS-RECORD=' 
                          LS-RECORD-FORMATTED
                END-IF

      *-----------------------------------------------------------------
      * Remember that unlike a spy variable, the watch variable can't
      * be referenced outside of the callback's context. So copy the
      * callback variable reference to the unit test's working storage.
      *-----------------------------------------------------------------
                MOVE LS-RECORD-FORMATTED TO WS-RECORD-COPY-FORMATTED
           END-IF

      *-----------------------------------------------------------------
      * Capture the SUT's return code, if it's set. This enables
      * the unit test to detect a RETURN-CODE change immediately
      * versus waiting for the SUT to end. Optionally, the unit test
      * could force an "unhappy path", e.g., changing the SUT's return
      * code from 0 to 12 to validate cleanup is handled correctly.
      *-----------------------------------------------------------------
           IF VARIABLENAME IN ZLS_W_ITBLOCK = 'RETURN-CODE'
                SET ADDRESS OF LS-RETURN-CODE-SUT TO 
                     CURRENTADDRESS IN ZLS_W_IFBLOCK
                MOVE LS-RETURN-CODE-SUT TO WS-RETURN-CODE-SUT
           END-IF

      *-----------------------------------------------------------------
      * Unit tests often have recorded data with dates within the
      * recording. If the SUT expects FUNCTION CURRENT-DATE to
      * match, the unit test can watch and modify the variable its
      * moved to via a watch spy.    
      *-----------------------------------------------------------------
           IF VARIABLENAME IN ZLS_W_ITBLOCK = 'WS-CURRENT-DATE'
                SET ADDRESS OF LS-CURRENT-DATE TO 
                     CURRENTADDRESS IN ZLS_W_IFBLOCK
                MOVE LS-CURRENT-DATE TO WS-ORIGINAL-DATE

      *-----------------------------------------------------------------
      * Force the CURRENT-DATE to a fixed date chosen by the unit test.
      *-----------------------------------------------------------------
                INITIALIZE LS-CURRENT-DATE
                MOVE 2024 TO CD-YEAR 
                MOVE 04 TO CD-MONTH
                MOVE 01 TO CD-DAY
                MOVE 12 TO CD-HOUR
                MOVE 42 TO CD-MINUTE

                DISPLAY 'ZTTWATCH changed WS-CURRENT-DATE: '
                DISPLAY 'ZTTWATCH was='
                     OD-YEAR '-' OD-MONTH '-' OD-DAY ' at '
                     OD-HOUR ':' OD-MINUTE
                DISPLAY 'ZTTWATCH now='
                     CD-YEAR '-' CD-MONTH '-' CD-DAY ' at '
                     CD-HOUR ':' CD-MINUTE
           END-IF

           GOBACK.
           