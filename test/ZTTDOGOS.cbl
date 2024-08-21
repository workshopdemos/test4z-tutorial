       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTDOGOS' RECURSIVE.
              
      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This Test4z example unit test suite validates the correct      *
      * operation of the SUT (program under test) ZTPDOGOS.            *
      *                                                                *
      * NB: "PROCESS PGMN(LM),NODYNAM" above is required for unit      *
      *     tests to enable long entry point names and locating entry  *
      *     points.                                                    *
      *                                                                *
      *     For the complete list of Test4z APIs and helpful           *
      *     reminders when writing a unit test suite, see the Test4z   *
      *     COBOL API document (PDF). The APIs are also documented     *
      *     in the copybook ZTESTWSn.cpy where "n" is the document     *
      *     revision number (1, 2, 3...).                              *      
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * Copy in the required control blocks from Test4z. The           *
      * WORKING-STORAGE control blocks start with ZWS_ and include     *
      * an "I" interface sub-block, e.g.:                              *
      *                                                                *
      * ZWS_GETVARIABLE -> I_GETVARIABLE IN ZWS_GETVARIABLE            *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
           COPY ZTESTWS.

      *-----------------------------------------------------------------
      * Copy return value control block for _LoadData API.
      * It loads data from previous "live" recording.
      *-----------------------------------------------------------------
       1 LOAD_DATA.
           COPY ZDATA.

      *-----------------------------------------------------------------
      * Copy return value control block for _MockQSAM API.
      * Mocks ADOPTS file based on previous "live" recording.
      *-----------------------------------------------------------------
       1 MOCK_ADOPTS.
           COPY ZQSAM.

      *-----------------------------------------------------------------
      * Copy return value control block for _MockQSAM API.
      * Mocks OUTREP file writes to DD.
      *-----------------------------------------------------------------
       1 MOCK_OUTREP.
           COPY ZQSAM.

      *-----------------------------------------------------------------
      * Copy return value control block for _SpyQSAM API.
      * Spies on OUTREP file modifications (which is also mocked).
      *-----------------------------------------------------------------
       1 OUTREP_SPY.
           COPY ZSPQSAM.

      *-----------------------------------------------------------------
      * Count of WRITE commands from _SpyQSAM callback for OUTREP.
      *-----------------------------------------------------------------
       1 OUTREP-SPY-WRITE-COUNT PIC 9(3) VALUE ZERO.

       LINKAGE SECTION.

      ******************************************************************
      * Copy in required control blocks from Test4z.                   *
      ******************************************************************
           COPY ZTESTLS.

      *-----------------------------------------------------------------
      * Incoming parameter from _SpyQSAM callback.
      *-----------------------------------------------------------------
       1 SPY_CALLBACK_OUTREP.
           COPY ZSPQSAM.

      *-----------------------------------------------------------------
      * Reference to ZTPDOGOS accumulator retrived via _GetVariable.
      *-----------------------------------------------------------------
       1 ZTPDOGOS-ACCUMULATOR.
           5 BREED-ADOPTIONS PIC 9(3) OCCURS 9 TIMES.

      *-----------------------------------------------------------------
      * Reference to ZTPDOGOS record from _SpyQSAM callback.
      * (ADOPTED-REPORT-REC)
      *-----------------------------------------------------------------
           COPY ZTPDGARR.

       PROCEDURE DIVISION.

      ******************************************************************
      * Register test to be run (only one in this simple example).     *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'outrepTotalsUnitTest'
           MOVE 'ZTTDOGOS simple totals test' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

      *-----------------------------------------------------------------
      * Note: Test4z will call 'outrepTotalsUnitTest' after the SUT
      * is prepared (see RUN-ZTPDOGOS for more details).
      *-----------------------------------------------------------------

           GOBACK.

      ******************************************************************
      * Unit test called by Test4z test suite runner.                  *
      ******************************************************************
           ENTRY 'outrepTotalsUnitTest'

           PERFORM MOCK-ADOPTS-FILE
           PERFORM MOCK-OUTREP-FILE
           PERFORM REGISTER-OUTREP-FILE-SPY
           PERFORM RUN-ZTPDOGOS

           GOBACK.

      ******************************************************************
      * QSAM spy callback for OUTREP file.                             *
      ******************************************************************
           ENTRY 'spyCallbackOUTREP' USING SPY_CALLBACK_OUTREP.

           DISPLAY 'ZTTDOGOS - See workshop step 3.1 (#SPYCALLBACK)'

      *-----------------------------------------------------------------
      * Map the linkage section to the address of the last record.
      *
      * (NB: The SPY_CALLBACK_OUTREP field CALLS has all middleware
      * calls recorded so far, but we're only considering the
      * last one; that's recorded in the field LASTCALL).
      *-----------------------------------------------------------------

           SET ADDRESS OF ZLS_QSAM_RECORD TO
                LASTCALL IN SPY_CALLBACK_OUTREP

      *-----------------------------------------------------------------
      * For a simple validation, we're only interested in valid WRITEs.
      *-----------------------------------------------------------------

           IF COMMAND IN ZLS_QSAM_RECORD = 'WRITE' AND
                     STATUSCODE IN ZLS_QSAM_RECORD = '00'

                DISPLAY 'ZTTDOGOS - See workshop step 3.2 (#SPYWRITE)'
                ADD 1 TO OUTREP-SPY-WRITE-COUNT

      *-----------------------------------------------------------------
      * Write the output record to SYSOUT for unit test debugging.
      *-----------------------------------------------------------------
      
                SET ADDRESS OF ADOPTED-REPORT-REC
                   TO PTR IN RECORD_ IN ZLS_QSAM_RECORD
                DISPLAY 'ZTTDOGOS spied - ' ADOPTED-REPORT-REC

           END-IF

      *-----------------------------------------------------------------
      * When the file is closed, verify ZTPDOGOS' internal acculator.
      *
      * NB: Alternatively, this could have been done post-execution of
      *     the SUT using spies. See ZTTWATCH and ZTTWATCS for examples
      *     of this approach.
      *-----------------------------------------------------------------

           IF COMMAND IN ZLS_QSAM_RECORD = 'CLOSE'
                DISPLAY 'ZTTDOGOS - See workshop step 3.3 (#SPYCLOSE)'

                PERFORM VALIDATE-RESULTS
           END-IF

           GOBACK.

      ******************************************************************
      * Mock OUTREP QSAM output file (no need to load data for it).    *
      ******************************************************************
       MOCK-OUTREP-FILE.

           DISPLAY 'ZTTDOGOS - See workshop step 1.1 '
                   'and step 1.2 (#MOCKOUTREP)'

           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'OUTREP' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, QSAMOBJECT IN MOCK_OUTREP

           EXIT.

      ******************************************************************
      * Load data for ADOPTS file from previous recording and mock it. *
      ******************************************************************
       MOCK-ADOPTS-FILE.

      *-----------------------------------------------------------------
      * Load data from a previous "live" recording.
      *-----------------------------------------------------------------

           DISPLAY 'ZTTDOGOS - See workshop step 1.3 (#LOADRECORDED)'

           MOVE LOW-VALUES TO I_LOADDATA
           MOVE 'ZTPDOGOS' TO MEMBERNAME IN ZWS_LOADDATA
           CALL ZTESTUT USING ZWS_LOADDATA, LOADOBJECT IN LOAD_DATA

      *-----------------------------------------------------------------
      * Initialize QSAM file access mock object for the ADOPTS DD
      * with the load object (data) created above.
      *-----------------------------------------------------------------

           DISPLAY 'ZTTDOGOS - See workshop step 1.4 (#MOCKADOPTS)'

           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'ADOPTS' TO FILENAME IN ZWS_MOCKQSAM
           SET LOADOBJECT IN ZWS_MOCKQSAM TO LOADOBJECT IN LOAD_DATA
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, QSAMOBJECT IN MOCK_ADOPTS

           EXIT.

      ******************************************************************
      * Register a QSAM spy callback for changes in the OUTREP file.   *
      ******************************************************************
       REGISTER-OUTREP-FILE-SPY.

           DISPLAY 'ZTTDOGOS - See workshop step 2.1 '
                   'and step 2.2 (#REGISTERSPY)'

           MOVE LOW-VALUES TO I_SPYQSAM
           SET CALLBACK IN ZWS_SPYQSAM TO ENTRY 'spyCallbackOUTREP'
           MOVE 'OUTREP' TO FILENAME IN ZWS_SPYQSAM
           CALL ZTESTUT USING ZWS_SPYQSAM, QSAMSPYOBJECT IN OUTREP_SPY

           EXIT.

      ******************************************************************
      * Run the ZTPDOGOS program.                                      *
      ******************************************************************
       RUN-ZTPDOGOS.

           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPDOGOS' TO MODULENAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

           EXIT.

      ******************************************************************
      * Validate OUTREP writes and ZTPDOGOS internal acculator values. *
      ******************************************************************
       VALIDATE-RESULTS.

      *-----------------------------------------------------------------
      * Black box test - confirm the correct number of OUTREP records.
      *-----------------------------------------------------------------

           DISPLAY 'ZTTDOGOS - See workshop step 3.3 (#VALIDATEOUTREP)'

           IF OUTREP-SPY-WRITE-COUNT NOT = 9 THEN
             PERFORM FAIL-OUTREP-WRITE-COUNT
           END-IF

      *-----------------------------------------------------------------
      * Gray box test - get access to ZTPDOGOS' internal accumulator.
      *-----------------------------------------------------------------

           DISPLAY 'ZTTDOGOS - See workshop step 3.4 '
                   '(#VALIDATEACCUMULATOR)'

           MOVE LOW-VALUES TO I_GETVARIABLE
           MOVE 'ACCUMULATOR' TO VARIABLENAME IN ZWS_GETVARIABLE
           CALL ZTESTUT USING ZWS_GETVARIABLE,
                ADDRESS OF ZTPDOGOS-ACCUMULATOR

      *-----------------------------------------------------------------
      * Check accumulator values, assert fail if an incorrect total.
      *-----------------------------------------------------------------

           IF BREED-ADOPTIONS(1) NOT = 8 OR
                     BREED-ADOPTIONS(2) NOT = 0 OR
                     BREED-ADOPTIONS(3) NOT = 7 OR
                     BREED-ADOPTIONS(4) NOT = 1 OR
                     BREED-ADOPTIONS(5) NOT = 0 OR
                     BREED-ADOPTIONS(6) NOT = 0 OR
                     BREED-ADOPTIONS(7) NOT = 0 OR
                     BREED-ADOPTIONS(8) NOT = 6 OR
                     BREED-ADOPTIONS(9) NOT = 0 THEN
                PERFORM FAIL-INTERNAL-ACCUMULATOR
           END-IF

           EXIT.

      ******************************************************************
      * Signal failure checking the internal ZTPDOGOS accumulator.     *
      ******************************************************************
       FAIL-INTERNAL-ACCUMULATOR.

           DISPLAY 'ZTTDOGOS - Invalid accumulator value(s) ' 
                ZTPDOGOS-ACCUMULATOR
           MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
           MOVE 'Invalid accumulator value(s) from ZTPDOGOS'
                TO FAILMESSAGE IN ZWS_FAIL
           CALL ZTESTUT USING ZWS_FAIL

           EXIT.

      ******************************************************************
      * Signal failure of expected count of OUTREP records.            *
      ******************************************************************
       FAIL-OUTREP-WRITE-COUNT.

           DISPLAY 'ZTTDOGOS - OUTREP record count is '
                OUTREP-SPY-WRITE-COUNT
           MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
           MOVE 'Invalid OUTREP count from ZTTDOGOS'
                TO FAILMESSAGE IN ZWS_FAIL
           CALL ZTESTUT USING ZWS_FAIL

           EXIT.
           