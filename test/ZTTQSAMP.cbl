       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTQSAMP' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This unit test example shows how to run a program under test   *
      * using previously recorded QSAM data from a "live" execution    *
      * using ZTESTEXE (see below for details). This unit test         *
      * includes a simple intercept spy to validate output.            *
      *                                                                *
      * The Test4z spy has a history of operations. To validate, there *
      * are two options:                                               *
      *                                                                *
      * 1. Wait until the program under test finishes and verify       *
      *    the spy's complete history of operations.                   *
      * 2. Or, as in this example, validate as each file operation     *
      *    occurs using the spy's callback.                            *
      *                                                                *
      * Both approaches are valid.                                     *
      *                                                                *   
      * The former is preferred if the unit test wants to validate ALL *
      * operations and not stop if one operation doesn't pass          *
      * validation.                                                    *
      *                                                                *   
      * The latter is preferred if one failed validation would         *
      * invalidate all subsequent validations, or the number of        *
      * validation failures would be so great, there's little point    *
      * in continuing the unit test.                                   *    
      *                                                                *
      * RELATED TOPIC:                                                 *
      *                                                                *
      * The recording was captured by having ZESTRUN load and run      *
      * the ZTPQHELO program using JCL similar to this:                *
      *                                                                *
      *   //ZTPQHELO EXEC PGM=ZTESTEXE                                 *
      *   //         DD DISP=SHR,DSN=YOUR.HLQ.PROGRAM.LOAD             *
      *   //         DD DISP=SHR,DSN=YOUR.HLQ.TEST4Z.CT4ZLOAD          *
      *   //SYSIN1   DD DISP=SHR,DSN=YOUR.HLQ.SYSIN1                   *
      *   //SYSOUT1  DD SYSOUT=*                                       *
      *   //ZLMSG    DD SYSOUT=*                                       *
      *   //ZLDATA   DD DISP=SHR,DSN=YOUR.HLQ.ZLDATA                   *
      *   //ZLOPTS   DD *                                              *
      *     RUN(ZTPQHELO),RECORD                                       *
      *   /*                                                           *
      *   //CEEOPTS  DD *                                              *
      *     TRAP(ON,NOSPIE)                                            *
      *   /*                                                           *
      *                                                                *
      * The ZLDATA DD is a partitioned dataset and the member ZTPQHELO *
      * will be created as a recording when ZTESTEXE executes with the *
      * RECORD option in ZLOPTS. Once created, those using Visual      *
      * Studio and the Test4z CLI should copy it to the test/data      *
      * folder so the unit test can load it with the _LoadData API.    *
      *                                                                *
      * For more details, see "How to Obtain a Recording of a Program" *
      * in the Test4z documentation.                                   *
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
      * ZWS_LOADDATA -> I_LOADDATA IN ZWS_LOADDATA                     *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
       COPY ZTESTWS.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       01  WS-ZDATA-HELLO-RECORDING.
           COPY ZDATA.

       01  WS-ZQSAM-NAMES-INPUT.
           COPY ZQSAM.

       01  WS-ZQSAM-HELLO-OUTPUT.
           COPY ZQSAM.

       01  WS-ZSPQSAM-HELLO-OUTPUT-SPY.
           COPY ZSPQSAM.

       01  WS-RUN-PROGRAM USAGE FUNCTION-POINTER.

      *-----------------------------------------------------------------
      * See the QSAM "qsamHelloWithPatternValidation" spy
      * callback for details -- it validates the output greetings match
      * the expected pattern "Hello, [name]!"
      *-----------------------------------------------------------------
       01  WS-HELLO-RECORD            PIC X(80).
       01  WS-OLLEH-RECORD            PIC X(80).
       01  WS-PHRASE-LTH              PIC 9(2).

      *-----------------------------------------------------------------
      * See the QSAM "qsamHelloWithExactValidation" spy
      * callback for details -- it validates the greetings match
      * the expected output below exactly.
      *-----------------------------------------------------------------
       01  WS-EXPECTED-GREETINGS.
           05 FILLER PIC X(80) VALUE 'Hello, Chaithanya!'.
           05 FILLER PIC X(80) VALUE 'Hello, Dan!'.
           05 FILLER PIC X(80) VALUE 'Hello, Eleni!'.
           05 FILLER PIC X(80) VALUE 'Hello, Jeff!'.
           05 FILLER PIC X(80) VALUE 'Hello, Karthik!'.
           05 FILLER PIC X(80) VALUE 'Hello, Mike!'.
           05 FILLER PIC X(80) VALUE 'Hello, Olga!'.
           05 FILLER PIC X(80) VALUE 'Hello, PetrP!'.
           05 FILLER PIC X(80) VALUE 'Hello, PetrV!'.
           05 FILLER PIC X(80) VALUE 'Hello, Ritika!'.
           05 FILLER PIC X(80) VALUE 'Hello, Vibhore!'.
           05 FILLER PIC X(80) VALUE 'Hello, Steve!'.
           05 FILLER PIC X(80) VALUE 'Hello, Swathi!'.
           05 FILLER PIC X(80) VALUE 'Hello, Tim!'.
       01  WS-EXPECTED-GREETINGS-REDEF REDEFINES WS-EXPECTED-GREETINGS.
           05 WS-EXPECTED-GREETING     PIC X(80) OCCURS 14 TIMES.
       01  WS-EXPECTED-GREETINGS-COUNT PIC 9(2) VALUE 14.
       01  WS-WRITTEN-GREETINGS-COUNT  PIC 9(2) VALUE 0.
       01  I                           PIC 9(2).

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

       01  LS-ZSPQSAM-HELLO-OUTPUT-SPY.
           COPY ZSPQSAM.

       01  LS-HELLO-RECORD PIC X(80).

       PROCEDURE DIVISION.

      ******************************************************************
      * Register the QSAM tests to execute; Test4z will call these     *
      * entries once control returns to it with the GOBACK below.      *
      ******************************************************************

           PERFORM REGISTER-UNIT-TESTS

           GOBACK.

      ******************************************************************
      * UNIT TEST 1: Provide the unit test entry point for the         *
      *              QSAM loaded data with data provided by a          *
      *              previous recording. Validate the actual output    *
      *              matches the expected output exactly.              *
      ******************************************************************   
           ENTRY 'qsamHelloWithExactValidation'.

           PERFORM CREATE-FILE-MOCKS-AND-LOADDATA

      *-----------------------------------------------------------------
      * Create a spy for the QSAM file. This spy will confirm exact
      * matches of the actual output given known input.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'SYSOUT1' TO FILENAME IN ZWS_SPYQSAM
           SET CALLBACK IN ZWS_SPYQSAM
                TO ENTRY 'qsamSpyAssertHelloExact'
           CALL ZTESTUT USING ZWS_SPYQSAM,
                QSAMSPYOBJECT IN WS-ZSPQSAM-HELLO-OUTPUT-SPY
                
      *-----------------------------------------------------------------
      * The mocks and spies are in place, so run the program.
      *-----------------------------------------------------------------
           PERFORM RUN-PROGRAM-UNDER-TEST

      *-----------------------------------------------------------------
      * If we've made it this far, the QSAM spy hasn't found
      * any non-matches and ended the test. But there could be no
      * input records or extras, so double-check the final count.
      *-----------------------------------------------------------------
           IF WS-EXPECTED-GREETINGS-COUNT 
                     NOT = WS-WRITTEN-GREETINGS-COUNT
                DISPLAY 'ZTTQSAMP Expected=' 
                     WS-EXPECTED-GREETINGS-COUNT ' records'
                DISPLAY 'ZTTQSAMP      Got='
                     WS-WRITTEN-GREETINGS-COUNT ' records'

                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTQSAMP record count mismatch' 
                     TO FAILMESSAGE IN ZWS_FAIL
                CALL ZTESTUT USING ZWS_FAIL
           END-IF

           DISPLAY 'ZTTQSAMP validation for exact matches complete'
           
           GOBACK.

      ******************************************************************
      * UNIT TEST 2: Provide the unit test entry point for the         *
      *              QSAM loaded data with data provided by a          *
      *              previous recording. Validate the output matches   *
      *              the expected pattern for a greeting.              *
      ******************************************************************   
           ENTRY 'qsamHelloWithPatternValidation'.

           PERFORM CREATE-FILE-MOCKS-AND-LOADDATA

      *-----------------------------------------------------------------
      * Create a spy for the QSAM file. Whether it's an operation
      * against the mock created above or the "real" file, the spy
      * will be notified. The optional CALLBACK is a unit test entry 
      * point that will be invoked for each operation, giving it an
      * opportunity to validate, log, or both.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'SYSOUT1' TO FILENAME IN ZWS_SPYQSAM
           SET CALLBACK IN ZWS_SPYQSAM
                TO ENTRY 'qsamSpyAssertHelloPattern'
           CALL ZTESTUT USING ZWS_SPYQSAM,
                QSAMSPYOBJECT IN WS-ZSPQSAM-HELLO-OUTPUT-SPY

      *-----------------------------------------------------------------
      * The mocks and spies are in place, so run the program.
      *-----------------------------------------------------------------
           PERFORM RUN-PROGRAM-UNDER-TEST

      *-----------------------------------------------------------------
      * At this point, the SUT "ZTPQHELO" has finished and control has
      * returned to this unit test. The validation of written output
      * was done as each operation was performed by the spy
      * callback 'qsamSpyAssertHelloPattern', so we're done.
      * 
      * See other Test4z examples for more validation techniques using
      * spies, watches, SUT variable access, and so on.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTQSAMP validation for pattern matches complete'

           GOBACK.

      ******************************************************************
      * CALLBACK 1: This QSAM spy is called when a SYSOUT1 file        *
      *             operation is performed. That allows the spy to     *
      *             validate and optionally log the output. In this    *
      *             example, it will confirm the actual output matches *
      *             the expected output exactly.                       *
      ******************************************************************
           ENTRY 'qsamSpyAssertHelloExact'
                USING LS-ZSPQSAM-HELLO-OUTPUT-SPY.

      *-----------------------------------------------------------------
      * The spy is called for all file operations against SYSOUT1;
      * we're only interested in WRITE operations that succeeded.
      *
      * NB: The incoming LASTCALL is from the QSAM spy history of
      *     file operations.
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_QSAM_RECORD 
                TO LASTCALL IN LS-ZSPQSAM-HELLO-OUTPUT-SPY

           IF COMMAND IN ZLS_QSAM_RECORD = 'WRITE' AND
                     STATUSCODE IN ZLS_QSAM_RECORD = '00'

                ADD 1 TO WS-WRITTEN-GREETINGS-COUNT
                MOVE WS-WRITTEN-GREETINGS-COUNT TO I

      *-----------------------------------------------------------------
      * Only check the newly written record if the total number of
      * written records hasn't exceeded the expected number of records.
      *
      * Once the program under test has finished, the unit test will
      * double-check the expected exactly equals the actual count, so
      * there's no need to check it here. Waiting until the SUT ends
      * will also cover the case where there's zero records (i.e., if 
      * there's no input records, this spy would never have been called
      * for a WRITE operation).
      *-----------------------------------------------------------------
                IF I <= WS-EXPECTED-GREETINGS-COUNT

                     SET ADDRESS OF LS-HELLO-RECORD TO
                          PTR IN RECORD_ IN ZLS_QSAM_RECORD

                     IF LS-HELLO-RECORD NOT = WS-EXPECTED-GREETING(I)

                          DISPLAY 'ZTTQSAMP Expected=' 
                               WS-EXPECTED-GREETING(I)
                          DISPLAY 'ZTTQSAMP      Got='
                               LS-HELLO-RECORD
      
                          MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                          MOVE 'Greeting does not match expected output'
                               TO FAILMESSAGE IN ZWS_FAIL
                          CALL ZTESTUT USING ZWS_FAIL
                     ELSE
                          DISPLAY 'ZTTQSAMP validated greeting (exact) ' 
                               LS-HELLO-RECORD
                     END-IF 
                END-IF
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK 2: This QSAM spy is called when a SYSOUT1 file        *
      *             operation is performed. That allows the spy to     *
      *             validate and optionally log the output. In this    *
      *             example, it will verify the output record          *
      *             conforms to the pattern "Hello, [name]!"           *
      ******************************************************************
           ENTRY 'qsamSpyAssertHelloPattern'
                USING LS-ZSPQSAM-HELLO-OUTPUT-SPY.

      *-----------------------------------------------------------------
      * The spy is called for all file operations against SYSOUT1;
      * we're only interested in WRITE operations that succeeded.
      *
      * NB: The incoming LASTCALL is from the QSAM spy history of 
      *     file operations.
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_QSAM_RECORD 
                TO LASTCALL IN LS-ZSPQSAM-HELLO-OUTPUT-SPY

           IF COMMAND IN ZLS_QSAM_RECORD = 'WRITE' AND
                     STATUSCODE IN ZLS_QSAM_RECORD = '00'
                SET ADDRESS OF LS-HELLO-RECORD TO
                     PTR IN RECORD_ IN ZLS_QSAM_RECORD
                MOVE LS-HELLO-RECORD TO WS-HELLO-RECORD

      *-----------------------------------------------------------------
      * See ZTPQHELO for details; it's a short program that reads
      * a list of names and outputs a greeting "Hello, [name]!".
      *
      * For a quick validation, confirm the greeting and the final "!".
      *-----------------------------------------------------------------
                MOVE 0 TO WS-PHRASE-LTH
                MOVE FUNCTION REVERSE(WS-HELLO-RECORD)
                     TO WS-OLLEH-RECORD
                INSPECT WS-OLLEH-RECORD 
                     TALLYING WS-PHRASE-LTH FOR LEADING SPACES
                COMPUTE WS-PHRASE-LTH = 
                     LENGTH OF WS-HELLO-RECORD - WS-PHRASE-LTH

      *-----------------------------------------------------------------
      * If the input isn't valid, display the expected result in
      * SYSOUT and fail the test. The unit test will end immediately
      * after the call to _FAIL.
      *-----------------------------------------------------------------
                IF WS-PHRASE-LTH < 9 OR 
                          WS-HELLO-RECORD(1:7) NOT = 'Hello, ' OR
                          WS-HELLO-RECORD(WS-PHRASE-LTH:1) NOT = '!'

                     DISPLAY 'ZTTQSAMP Expected=Hello, [name]!'
                     DISPLAY 'ZTTQSAMP      Got=' WS-HELLO-RECORD

                     MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                     MOVE 'Greeting does not match expected pattern'
                          TO FAILMESSAGE IN ZWS_FAIL
                     CALL ZTESTUT USING ZWS_FAIL
                ELSE
                     DISPLAY 'ZTTQSAMP validated greeting (pattern) ' 
                          WS-HELLO-RECORD
                END-IF 

           END-IF

           GOBACK.

      ******************************************************************
      * One unit test will verify the given input generates the        *
      * expected output exactly. The second unit test will only        *
      * verify the output "greeting" matches the expected pattern.     *
      ******************************************************************
       REGISTER-UNIT-TESTS.

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'qsamHelloWithExactValidation'
           MOVE '1: Recorded QSAM data for ZTPQHELO - exact match'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'qsamHelloWithPatternValidation'
           MOVE '2: Recorded QSAM data for ZTPQHELO - pattern match'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           EXIT.

      ******************************************************************
      * Create QSAM file mocks for the SYSIN1 (names) and SYSOUT1      *
      * (greetings).                                                   *
      *                                                                *
      * The input data comes from a previous recording and is stored   *
      * in a partitioned dataset member named 'ZTPQHELO'. For those    *
      * using Visual Studio and the Test4z CLI, this file is in the    *
      * test/data folder and uploaded by the "t4z test" command.       *
      *                                                                *
      * This recorded data is in JSON format.                          *
      ******************************************************************
       CREATE-FILE-MOCKS-AND-LOADDATA.

      *-----------------------------------------------------------------
      * Create a loaded data object for a QSAM test program
      * from a previous recording of a "live" environment. 
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_LOADDATA
           MOVE 'ZTPQHELO' TO MEMBERNAME IN ZWS_LOADDATA
           CALL ZTESTUT USING ZWS_LOADDATA, 
                LOADOBJECT IN WS-ZDATA-HELLO-RECORDING

      *-----------------------------------------------------------------
      * Using the loaded data, create a QSAM file mock to 
      * be intercepted when SYSIN1 is accessed by program ZTPQHELO.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'SYSIN1' TO FILENAME IN ZWS_MOCKQSAM
           SET LOADOBJECT IN ZWS_MOCKQSAM 
                TO LOADOBJECT IN WS-ZDATA-HELLO-RECORDING
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, 
                QSAMOBJECT IN WS-ZQSAM-NAMES-INPUT

      *-----------------------------------------------------------------
      * Create a QSAM file mock to be intercepted when SYSOUT1 is
      * accessed by program ZTPQHELO. Since this file is output-only, 
      * there is no need to use the recorded data.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'SYSOUT1' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-HELLO-OUTPUT

           EXIT.

      ******************************************************************
      * Load and prepare the user application program ZTPQHELO for use *
      * and run it. The program will behave as if running in a live    *
      * environment, but these unit tests are providing the input      *
      * data and capturing the output data in QSAM mock files.         *
      ******************************************************************
       RUN-PROGRAM-UNDER-TEST.

           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPQHELO' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTPQHELO' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTPQHELO' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, WS-RUN-PROGRAM

           CALL WS-RUN-PROGRAM

           EXIT.
