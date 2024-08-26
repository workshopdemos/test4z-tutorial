       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTDOGWS' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite example.                       *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This example unit test suite validates the correct operation   *
      * of the SUT / program-under-test, ZTPDOGOS. For more details,   *
      * refer to the exercise instructions.                            *
      *                                                                *
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

      *-----------------------------------------------------------------
      * Return value control block for _LoadData API. It loads data
      * from a previous "real" recording of ZTPDOGOS created from JCL.
      *-----------------------------------------------------------------
       01  WS-ZDATA-RECORDING.
           COPY ZDATA.

      *-----------------------------------------------------------------
      * Return value control block for _MockQSAM API. Mocks ADOPTS file
      * based on previous "real" recording of ZTPDOGOS, stored as JSON.
      *-----------------------------------------------------------------
       01  WS-ZQSAM-ADOPTS-MOCK.
           COPY ZQSAM.

      *-----------------------------------------------------------------
      * Return value control block for _MockQSAM API. Mocks OUTREP file
      * DD whose operations can later be validated by a unit test.
      *-----------------------------------------------------------------
       01  WS-ZQSAM-OUTREP-MOCK.
           COPY ZQSAM.

      *-----------------------------------------------------------------
      * Return value control block for _SpyQSAM API. Spies on
      * operations on the OUTREP file (which is mocked above). Used
      * for black box validation of the report output.
      *-----------------------------------------------------------------
       01  WS-ZSPQSAM-OUTREP-SPY.
           COPY ZSPQSAM.

      *-----------------------------------------------------------------
      * Return value control block for _SpyVariable API. Spies on
      * modifications to ZTPDOGOS' ACCUMULATOR variable and is used for
      * gray box validation.
      *-----------------------------------------------------------------
       01  WS-ZSPVAR-ACCUMULATOR-SPY.
           COPY ZSPVAR.

      *-----------------------------------------------------------------
      * Entry point address for the ZTPDOGOS "dog adoption" program.
      * (see 300-RUN-PROGRAM-UNDER-TEST).
      *-----------------------------------------------------------------
       01  WS-RUN-PROGRAM USAGE FUNCTION-POINTER.

      *-----------------------------------------------------------------
      * The return code from ZTPDOGOS; it's non-zero if there's a
      * file I/O error or processing error.
      *-----------------------------------------------------------------
       01  WS-RETURN-CODE-SUT PIC S9(4) USAGE BINARY.

      *-----------------------------------------------------------------
      * Count of WRITE commands from _SpyQSAM callback for OUTREP;
      * it will be compared against expected value as an initial
      * black box validation.
      *-----------------------------------------------------------------
       01  WS-ACTUAL-OUTREP-WRITES   PIC 9(3) VALUE 0.
       01  WS-EXPECTED-OUTREP-WRITES PIC 9(3) VALUE 9.

      *-----------------------------------------------------------------
      * Expected records written to OUTREP captured by the _SpyQSAM
      * history; as part of black box validation, these are
      * compared against the program's actual written records.
      *-----------------------------------------------------------------
       01  WS-EXPECTED-OUTREP-RECS.
           05 OUTREP-1 PIC X(80) VALUE 
           'BREED SHIBA                          WAS ADOPTED 008 TIMES'.
           05 OUTREP-2 PIC X(80) VALUE 
           'BREED SCHNAUZER                      WAS ADOPTED 000 TIMES'.
           05 OUTREP-3 PIC X(80) VALUE 
           'BREED CORGI                          WAS ADOPTED 007 TIMES'.
           05 OUTREP-4 PIC X(80) VALUE 
           'BREED CHI                            WAS ADOPTED 001 TIMES'.
           05 OUTREP-5 PIC X(80) VALUE 
           'BREED POODLE                         WAS ADOPTED 000 TIMES'.
           05 OUTREP-6 PIC X(80) VALUE 
           'BREED POMERANIAN                     WAS ADOPTED 000 TIMES'.
           05 OUTREP-7 PIC X(80) VALUE 
           'BREED BULLDOG                        WAS ADOPTED 000 TIMES'.
           05 OUTREP-8 PIC X(80) VALUE 
           'BREED JINGO                          WAS ADOPTED 006 TIMES'.
           05 OUTREP-9 PIC X(80) VALUE 
           'BREED OTHER                          WAS ADOPTED 000 TIMES'.
       01  WS-EXPECTED-OUTREP-RECS-REDEF
                REDEFINES WS-EXPECTED-OUTREP-RECS.
           05 WS-EXPECTED-OUTREP-RECORD   PIC X(80) OCCURS 9 TIMES.

      *-----------------------------------------------------------------
      * If a mismatch is found against the expected written records,
      * the unit test will fail; SYSOUT will have the details.
      *-----------------------------------------------------------------
       01  WS-MISMATCHED-OUTREP-RECORDS   PIC 9(2).

      *-----------------------------------------------------------------
      * Keep track of how many validations failed -- before exiting the
      * unit test, call Test4z _Fail API if it's non-zero. Details on
      * what failed are logged in SYSOUT.
      *-----------------------------------------------------------------       
       01  WS-FAILED-VALIDATIONS           PIC 9(2) VALUE 0.       

      *-----------------------------------------------------------------
      * Expected values for ZTPDOGOS' ACCUMULATOR variable that will 
      * be compared with those captured by its variable spy. This is 
      * a gray box validation because it's taking advantage of 
      * knowledge of the tested program's implementation details.
      *-----------------------------------------------------------------
       01  WS-EXPECTED-ACCUMULATOR.
           05 BREED-ADOPTIONS-1-SHIBA      PIC 9(3) VALUE 8.
           05 BREED-ADOPTIONS-2-SCHNAUZER  PIC 9(3) VALUE 0.
           05 BREED-ADOPTIONS-3-CORGI      PIC 9(3) VALUE 7.
           05 BREED-ADOPTIONS-4-CHI        PIC 9(3) VALUE 1.
           05 BREED-ADOPTIONS-5-POODLE     PIC 9(3) VALUE 0.       
           05 BREED-ADOPTIONS-6-POMERANIAN PIC 9(3) VALUE 0.       
           05 BREED-ADOPTIONS-7-BULLDOG    PIC 9(3) VALUE 0.       
           05 BREED-ADOPTIONS-8-JINGO      PIC 9(3) VALUE 6.
           05 BREED-ADOPTIONS-9-OTHER      PIC 9(3) VALUE 0.
       01  WS-FINAL-ACCUMULATOR.
           05 BREED-ADOPTIONS PIC 9(3) OCCURS 9 TIMES VALUE 0.

      *-----------------------------------------------------------------
      * Miscellaneous indices.
      *-----------------------------------------------------------------      
       77  I PIC 9(3).
       77  J PIC 9(3).

       LINKAGE SECTION.

      ******************************************************************
      * Copy in required control blocks from Test4z.                   *
      ******************************************************************
           COPY ZTESTLS.

      *-----------------------------------------------------------------
      * Incoming parameter from _SpyQSAM callback. See entry
      * 'spyCallbackOUTREP' for more details.
      *-----------------------------------------------------------------
       01  LS-ZSPQSAM-OUTREP-SPY.
           COPY ZSPQSAM.

      *-----------------------------------------------------------------
      * Reference to ZTPDOGOS accumulator from variable spy. See
      * 430-VALIDATION-GRAYBOX for more details.
      *-----------------------------------------------------------------
       01  LS-ZTPDOGOS-ACCUMULATOR.
           05 BREED-ADOPTIONS PIC 9(3) OCCURS 9 TIMES.

      *-----------------------------------------------------------------
      * Reference to ZTPDOGOS record for _SpyQSAM callback and
      * spy history traversal. See entry 'spyCallbackOUTREP' and
      * 420-VALIDATION-BLACKBOX-T2 for more details.
      *-----------------------------------------------------------------
       01  LS-OUTREP-RECORD   PIC X(80).

       PROCEDURE DIVISION.

      ******************************************************************
      * Register test to be run (only one in this simple example).     *
      *                                                                *
      * NB: Test4z will call 'outrepTotalsUnitTest' after the SUT is   *
      *     prepared (see 200-PREPARE-PROGRAM-UNDER-TEST for more      *
      *     details).                                                  *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'outrepTotalsUnitTest'
           MOVE 'ZTTDOGWS simple totals test' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           GOBACK.

      ******************************************************************
      * UNIT TEST: Called by Test4z unit test suite runner, ZESTRUN.   *
      ******************************************************************
           ENTRY 'outrepTotalsUnitTest'

           PERFORM 100-MOCK-ADOPTS-FILE
           PERFORM 110-MOCK-OUTREP-FILE

      *-----------------------------------------------------------------
      * NB: Test4z APIs that map references in the program-under-test
      *     (e.g., _SpyVariable) must be preceded by _PrepareModule; the
      *     latter maps these references/CSECTs to runtime addresses.
      *-----------------------------------------------------------------
           PERFORM 200-PREPARE-PROGRAM-UNDER-TEST
           PERFORM 210-REGISTER-OUTREP-SPY
           PERFORM 220-REGISTER-ACCUMULATOR-SPY

      *-----------------------------------------------------------------
      * The mocks and spies are ready, so run the program-under-test
      * and then validate the results.
      *-----------------------------------------------------------------
           PERFORM 300-RUN-PROGRAM-UNDER-TEST

           PERFORM 400-VALIDATION-NO-ERRORS
           PERFORM 410-VALIDATION-BLACKBOX-T1
           PERFORM 420-VALIDATION-BLACKBOX-T2
           PERFORM 430-VALIDATION-GRAYBOX

      *-----------------------------------------------------------------
      * The validations log mismatches to SYSOUT, but they don't
      * call Test4z _Fail IF the mismatch won't disqualify all others.
      * So, if we've reached this point and a validation did not pass,
      * fail the unit test; Test4z will immediately end this unit test
      * and continue to the next one.
      *-----------------------------------------------------------------
           IF WS-FAILED-VALIDATIONS > 0
                PERFORM 530-FAIL-UNIT-TEST
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: QSAM spy callback for OUTREP file. This is invoked   *
      *           when an operation is performed against the target    *
      *           file. The invocation occurs before the operation     *
      *           returns to the program-under-test.                   *
      ******************************************************************
           ENTRY 'spyCallbackOUTREP'
                USING LS-ZSPQSAM-OUTREP-SPY.

      *-----------------------------------------------------------------
      * Map the linkage section to the address of the last file
      * operation record.
      *
      * NB: The LS-ZSPQSAM-OUTREP-SPY field 'CALLS' has all middleware
      *     calls recorded so far, but we're only considering the last
      *     one; that's recorded in the field LASTCALL.
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_QSAM_RECORD
                TO LASTCALL IN LS-ZSPQSAM-OUTREP-SPY

      *-----------------------------------------------------------------
      * For initial validation, we're only interested in valid WRITEs.
      *-----------------------------------------------------------------
           IF COMMAND IN ZLS_QSAM_RECORD = 'WRITE' AND
                     STATUSCODE IN ZLS_QSAM_RECORD = '00'

      *-----------------------------------------------------------------
      * TUTORIAL(5) - Record count of actual WRITEs to OUTREP.
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      * Write the output record to SYSOUT for unit test debugging.
      *-----------------------------------------------------------------
                SET ADDRESS OF LS-OUTREP-RECORD
                     TO PTR IN RECORD_ IN ZLS_QSAM_RECORD
                DISPLAY 'ZTTDOGWS spied - ' LS-OUTREP-RECORD
           END-IF

           GOBACK.

      ******************************************************************
      * Load data for ADOPTS file from previous recording and mock it. *
      ******************************************************************
       100-MOCK-ADOPTS-FILE.

           DISPLAY 'ZTTDOGWS 100-MOCK-ADOPTS-FILE'

      *-----------------------------------------------------------------
      * Load data from a previous "real" recording of ZTPDOGOS. 
      * 
      * NB: The MEMBERNAME field is the file/dataset name of the
      *     recording of all ZTPDOGOS operations, not a specific
      *     mocked file like ADOPTS. In a VS Code workspace, it's
      *     located in the test/data folder.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      * TUTORIAL(1) - Use "t4z loaddata" snippet.
      *-----------------------------------------------------------------      


      *-----------------------------------------------------------------
      * Initialize QSAM file access mock object for the ADOPTS DD
      * with the load object (data) created above.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'ADOPTS' TO FILENAME IN ZWS_MOCKQSAM

           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-ADOPTS-MOCK
      *-----------------------------------------------------------------
      * TUTORIAL(2) - Complete SET LOADOBJECT above.
      *-----------------------------------------------------------------   

           EXIT.

      ******************************************************************
      * Mock OUTREP QSAM output file (no need to load data for it).    *
      ******************************************************************
       110-MOCK-OUTREP-FILE.

           DISPLAY 'ZTTDOGWS 110-MOCK-OUTREP-FILE'

      *-----------------------------------------------------------------
      * TUTORIAL(3) - Use "t4z mockqsam" snippet.
      *-----------------------------------------------------------------


           EXIT.

      ******************************************************************
      * Prepare the ZTPDOGOS program; this loads it into memory so     *
      * symbols (CSECTs) can be resolved, e.g., the ACCUMULATOR        *
      * variable.                                                      *
      ******************************************************************
       200-PREPARE-PROGRAM-UNDER-TEST.

           DISPLAY 'ZTTDOGWS 200-PREPARE-PROGRAM-UNDER-TEST'

           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPDOGOS' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

           EXIT.

      ******************************************************************
      * Register a QSAM spy for monitoring changes in the OUTREP file. *
      ******************************************************************
       210-REGISTER-OUTREP-SPY.

           DISPLAY 'ZTTDOGWS 210-REGISTER-OUTREP-SPY'

      *-----------------------------------------------------------------
      * TUTORIAL(4) - Use "t4z spyqsam with callback" snippet.
      *-----------------------------------------------------------------


           EXIT.

      ******************************************************************
      * Register a variable spy for monitoring changes in ACCUMULATOR. *
      ******************************************************************
       220-REGISTER-ACCUMULATOR-SPY.

           DISPLAY 'ZTTDOGWS 220-REGISTER-ACCUMULATOR-SPY'

      *-----------------------------------------------------------------
      * Create a spy that will capture the program-under-test's
      * ACCUMULATOR variable value. Test4z will copy its history
      * into storage that can be referenced after ZTPDOGOS program
      * termination.
      *
      * NB: We're only interested in the final value, so we won't
      *     specify a CALLBACK. Variable spy callbacks, if specified,
      *     are invoked when a variable change is detected.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYVARIABLE
           MOVE 'ZTPDOGOS' TO MODULENAME IN ZWS_SPYVARIABLE
           MOVE 'ZTPDOGOS' TO FUNCTIONNAME IN ZWS_SPYVARIABLE
           MOVE 'ACCUMULATOR' TO VARIABLENAME IN ZWS_SPYVARIABLE
           CALL ZTESTUT USING ZWS_SPYVARIABLE,
                VARIABLESPYOBJECT IN WS-ZSPVAR-ACCUMULATOR-SPY

           EXIT.

      ******************************************************************
      * Run the ZTPDOGOS program; it's already been prepared, so       *
      * resolve its entry point and call it directly.                  *
      ******************************************************************
       300-RUN-PROGRAM-UNDER-TEST.

           DISPLAY 'ZTTDOGWS 300-RUN-PROGRAM-UNDER-TEST'

           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTPDOGOS' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTPDOGOS' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, WS-RUN-PROGRAM

           CALL WS-RUN-PROGRAM
           MOVE RETURN-CODE TO WS-RETURN-CODE-SUT

           EXIT.

      ******************************************************************
      * If ZTPDOGOS reported an error, it's unlikely that further      *
      * validations are meaningful, so fail (and end) the unit test.   *
      ******************************************************************
       400-VALIDATION-NO-ERRORS.

           DISPLAY 'ZTTDOGWS 400-VALIDATION-NO-ERRORS'

           IF WS-RETURN-CODE-SUT NOT = 0
                ADD 1 TO WS-FAILED-VALIDATIONS

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE 'ZTTDOGWS non-zero return code, ending unit test' 
                     TO MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                DISPLAY 'Expected: 0000'
                DISPLAY '     Got: ' WS-RETURN-CODE-SUT

                CALL ZTESTUT USING ZWS_MESSAGE

                PERFORM 530-FAIL-UNIT-TEST   
           END-IF

           EXIT.

      ******************************************************************
      * Validate OUTREP write count. This is a black box validation    *
      * because it only considers resources accessible from outside of *
      * the program-under-test.                                        *
      ******************************************************************
       410-VALIDATION-BLACKBOX-T1.

           DISPLAY 'ZTTDOGWS 410-VALIDATION-BLACKBOX-T1'

      *-----------------------------------------------------------------
      * TUTORIAL(6) - If mismatch of actual/expected, 
      *               call 500-REPORT-COUNT-MISMATCH.
      *-----------------------------------------------------------------


           EXIT.

      ******************************************************************
      * Validate OUTREP records. This is a black box validation        *
      * because it only considers resources accessible from outside of *
      * the program-under-test.                                        *
      ******************************************************************
       420-VALIDATION-BLACKBOX-T2.

           DISPLAY 'ZTTDOGWS 420-VALIDATION-BLACKBOX-T2'

           MOVE 0 TO WS-MISMATCHED-OUTREP-RECORDS
           MOVE 1 TO J

      *-----------------------------------------------------------------
      * Get addressability to the QSAM spy's history of operations
      * for the OUTREP file, then compare actual and expected writes.
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_QSAM_HISTORIES
                TO PTR IN CALLS IN WS-ZSPQSAM-OUTREP-SPY
        
           PERFORM VARYING I FROM 1 BY 1 
                     UNTIL I > SIZE_ IN CALLS IN WS-ZSPQSAM-OUTREP-SPY

      *-----------------------------------------------------------------
      * TUTORIAL(7) - Add DISPLAY for the command (operation)
      *               in the QSAM spy's call history.
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      * The QSAM spy operation history includes OPEN, READ, WRITE,
      * CLOSE, and so on. We're only interested in the WRITE history.
      *-----------------------------------------------------------------
                IF COMMAND IN ZLS_QSAM_HISTORY(I) = 'WRITE'
                     SET ADDRESS OF LS-OUTREP-RECORD
                          TO PTR IN RECORD_ IN ZLS_QSAM_HISTORY(I)

                     IF J > WS-EXPECTED-OUTREP-WRITES
                          ADD 1 TO WS-MISMATCHED-OUTREP-RECORDS

                          DISPLAY 'ZTTDOGWS Mismatched OUTREP'
                          DISPLAY 'Expected: <none>'
                          DISPLAY '     Got: ' LS-OUTREP-RECORD
                     ELSE 
                          IF LS-OUTREP-RECORD NOT = 
                                    WS-EXPECTED-OUTREP-RECORD(J)
                               ADD 1 TO WS-MISMATCHED-OUTREP-RECORDS

                               DISPLAY 'ZTTDOGWS Mismatched OUTREP'
                               DISPLAY 'Expected: '
                                    WS-EXPECTED-OUTREP-RECORD(J)
                               DISPLAY '     Got: ' LS-OUTREP-RECORD
                          END-IF
                     END-IF

      *-----------------------------------------------------------------
      * NB: The index "I" is all operations and index "J" is just the 
      *     WRITEs. This was a WRITE operation, so continue to the next
      *     expected output record.
      *-----------------------------------------------------------------
                     ADD 1 TO J
                END-IF
           END-PERFORM

      *-----------------------------------------------------------------
      * Done checking history. If there were any mismatches, report it.
      *-----------------------------------------------------------------
           IF WS-MISMATCHED-OUTREP-RECORDS > 0
                PERFORM 510-REPORT-RECORD-MISMATCH
           END-IF

           EXIT.

      ******************************************************************
      * Validate ZTPDOGOS internal acculator values. This is a gray    *
      * box  validation because it accesses the internal state of the  *
      * program-under-test.                                            *
      ******************************************************************
       430-VALIDATION-GRAYBOX.

           DISPLAY 'ZTTDOGWS 430-VALIDATION-GRAYBOX'

      *-----------------------------------------------------------------
      * Get access to ZTPDOGOS' internal accumulator, specifically the
      * variable's last captured value.
      *
      * NB: While arguably an error, the variable history will be null
      *     if the ADOPTS file is empty, so check the number of entries.
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_VARIABLE_CALL
                TO LASTCALL IN WS-ZSPVAR-ACCUMULATOR-SPY

           IF SIZ IN ZLS_VARIABLE_CALL > 0
                SET ADDRESS OF LS-ZTPDOGOS-ACCUMULATOR
                     TO PTR IN ZLS_VARIABLE_CALL
                MOVE LS-ZTPDOGOS-ACCUMULATOR TO WS-FINAL-ACCUMULATOR
           END-IF

      *-----------------------------------------------------------------
      * Check the values in the ACCUMULATOR record and report if
      * one of the totals doesn't match an expected total.
      *-----------------------------------------------------------------
           IF WS-FINAL-ACCUMULATOR NOT = WS-EXPECTED-ACCUMULATOR
                PERFORM 520-REPORT-TOTALS-MISMATCH
           END-IF

           EXIT.

      ******************************************************************
      * Signal failure of expected count of OUTREP records.            *
      ******************************************************************
       500-REPORT-COUNT-MISMATCH.

           ADD 1 TO WS-FAILED-VALIDATIONS

           DISPLAY 'ZTTDOGWS Invalid OUTREP count'
           DISPLAY 'Expected: ' WS-EXPECTED-OUTREP-WRITES
           DISPLAY '     Got: ' WS-ACTUAL-OUTREP-WRITES

           EXIT.

      ******************************************************************
      * Signal failure of expected OUTREP record content.              *
      ******************************************************************
       510-REPORT-RECORD-MISMATCH.

           ADD 1 TO WS-FAILED-VALIDATIONS

           DISPLAY 'ZTTDOGWS Mismatched OUTREP: ' 
                WS-MISMATCHED-OUTREP-RECORDS ' record(s)'

           EXIT.

      ******************************************************************
      * Signal failure checking the internal ZTPDOGOS accumulator.     *
      ******************************************************************
       520-REPORT-TOTALS-MISMATCH.

           ADD 1 TO WS-FAILED-VALIDATIONS

           DISPLAY 'ZTTDOGWS Invalid accumulator value(s)'
           DISPLAY 'Expected: ' WS-EXPECTED-ACCUMULATOR
           DISPLAY '     Got: ' WS-FINAL-ACCUMULATOR

           EXIT.

      ******************************************************************
      * Signal FAIL of the unit test; this stops the current one and
      * continues to the next registered unit test, if available.
      ******************************************************************
       530-FAIL-UNIT-TEST.

           MOVE LOW-VALUES TO I_FAIL
           STRING 'Failed ' DELIMITED BY SIZE
                WS-FAILED-VALIDATIONS DELIMITED BY SIZE
                ' validations' DELIMITED BY SIZE
                INTO FAILMESSAGE IN ZWS_FAIL
           CALL ZTESTUT USING ZWS_FAIL

           EXIT.
