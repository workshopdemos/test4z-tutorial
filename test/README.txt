      ******************************************************************
      * Copyright (c) 2023, 2024 Broadcom. All Rights Reserved.        *
      * The term "Broadcom" refers to Broadcom Inc. and/or its         *
      * subsidiaries.                                                  *
      *----------------------------------------------------------------*
      * Unit test cases that use the COBOL unit testing API (ZTESTUT)  *
      * must use these COBOL compiler options. Failure to use these    *
      * will cause attempts to dynamically load modules that are the   *
      * same name as the entry points the user defined, resulting in   *
      * failure.                                                       *
      *                                                                *
      * PROCESS PGMN(LM),NODYNAM                                       *
      *                                                                *
      * Unit test suite programs must define its PROGRAM-ID with the   *
      * RECURSIVE option, otherwise COBOL will throw a runtime error   *
      * stating that the program is recursive and cannot be processed. *
      *                                                                *
      * PROGRAM-ID. 'suitenam' RECURSIVE.                              *
      *                                                                *
      *----------------------------------------------------------------*
      * Programs that use a dynamic recording proxy for their          *
      * application (ZTESTREC) can use any COBOL compiler options.     *
      *                                                                *
      * This dynamic recording capability allows a user to create a    *
      * recording when using unusual or difficult software to          *
      * intercept from when using the standard ZTESTEXE recording      *
      * capabilities.                                                  *
      *                                                                *
      * See ZTESTREC for more information on how to set up a dynamic   *
      * recording proxy program.                                       *
      *                                                                *
      ******************************************************************

      ******************************************************************
      * Below is the dynamic recording proxy example program provided  *
      * in the example library.                                        *
      *----------------------------------------------------------------*
      *                                                                *
      * ZTTRECRD                                                       *
      * This dynamic recording proxy example shows how to perform a    *
      * recording in an IMS Transaction Isolation environment.         *
      *                                                                *
      ******************************************************************

      ******************************************************************
      * For those new to the Test4z APIs, below is a partial list of   *
      * unit test programs provided in the example library, in order   *
      * of recommended review:                                         *
      *                                                                *
      * File mocking from UT-defined data, spying: ZTTQSAMD            *
      * File mocking from recorded data, spying:   ZTTQSAMP            *
      * File mocking from UT-defined data, spying: ZTTKSDSD            *
      * File mocking from UT-defined data, spying: ZTTFILES            *
      * Validation via variable watching:          ZTTWATCH            *
      * Validation via variable spying:            ZTTWATCS            *
      * Simple end-to-end UT with DB2 mocking:     ZTTDB2PH            *
      * Simple end-to-end UT:                      ZTTDOGOS            *
      * Middleware mocking with test harness:      ZTTHRNES            *
      * Called programs replaced by UT callback:   ZTTSTUBP            *
      * Called programs replaced by recording:     ZTTMOCKP            *
      * Moderately complex end-to-end unit test:   ZTTRNDY             *
      * Called program spy:                        ZTTSPYPG            *
      * Section execution, spying, and assertion:  ZTTSECTN            *
      * Unit test control with parameters:         ZTTPARMP            *
      *                                                                *
      *----------------------------------------------------------------*
      * Below is an alphabetical list of the unit test programs        *
      * provided in the example library.                               *
      *----------------------------------------------------------------*
      * ZTTCICSM                                                       *
      * This test example shows how to run a unit test using no        *
      * previously recorded data but instead mocking CICS statements   *
      * for a program. It uses API calls to define container data      *
      * programmatically in the unit test itself.                      *
      *                                                                *
      * ZTTCICSR                                                       *
      * This test example shows how to run a unit test using           *
      * previously recorded data for a program using CICS.             *
      *                                                                *
      * ZTTDB2PH                                                       *
      * This test example shows how to run a unit test using           *
      * previously recorded data for a program using Db2 and           *
      * validate the actual versus expected results.                   *
      *                                                                *
      * ZTTDB2SR                                                       *
      * This test example shows how to run a unit test using           *
      * previously recorded data for a program using Db2 with no       *
      * intercept spy.                                                 *
      *                                                                *
      * ZTTDOGOS                                                       *
      * This test suite contains a set of tests meant to show how the  *
      * Test4z API can be used. It is not meant to be exhaustive       *
      * but instead show patterns of usage and code organization.      *
      *                                                                *
      * ZTTFILES                                                       *
      * This test example shows how to set up 2 tests. The first test  *
      * will intercept QSAM file calls and then display the records.   *
      * The second test will intercept VSAM/KSDS file calls and then   *
      * display the records. In both of these tests, the initial data  *
      * comes from the original dataset records. There is no data      *
      * by the test case in this example. For data-provided examples,  *
      * see ZTTKSDSD and ZTTQSAMD. The third test shows how to force   *
      * an "unhappy path" by intercepting one of the QSAM READ         *
      * middleware calls and returning an error status code.           *
      *                                                                *
      * ZTTHRNES                                                       *
      * This unit test suite validates the operation of three SUT      *
      * programs that collectively support the promotion               *
      * decision-making process of a fictional "widget" marketing      *
      * team: ZTPHRNMM (main program), ZTPHRNDD (data retrieval), and  *
      * ZTPHRNAA (data analysis).                                      *
      *                                                                *
      * ZTTKSDSD                                                       *
      * This test example shows how to set up 2 tests. The first test  *
      * will mock a fixed-length KSDS file with the test case          *
      * providing the data. The second test will mock a                *
      * variable-length KSDS file with the test case providing the     *
      * data. In both of these tests, the data comes from the test     *
      * case itself.                                                   *
      *                                                                *
      * ZTTMOCKD                                                       *
      * This test example shows how to set up a Db2 test with the test *
      * case providing the data.                                       *
      *                                                                *
      * ZTTMOCKP                                                       *
      * This test example shows how to run a unit test on a program    *
      * that calls another program that is to be stubbed out. Any      *
      * argument data is to be replaced using the previously recorded  *
      * data.                                                          *
      *                                                                *
      * ZTTPARMP                                                       *
      * This test example shows how to access parameters defined in    *
      * the ZLOPTS DD from within a test case. This technique can be   *
      * used to help direct the flow of tests or provide information   *
      * to them.                                                       *
      *                                                                *
      * ZTTQSAMD                                                       *
      * This test example shows how to set up 2 tests. The first test  *
      * will mock a fixed-length QSAM file with the test case          *
      * providing the data. The second test will mock a                *
      * variable-length QSAM file with the test case providing the     *
      * data. In both of these tests, the initial data comes from the  *
      * test case itself.                                              *
      *                                                                *
      * ZTTQSAMP                                                       *
      * These test examples shows how to run a unit test using         *
      * previously recorded data for a batch program using QSAM with   *
      * an intercept spy.                                              *
      *                                                                *
      * ZTTRNDY                                                        *
      * TRENDY was inspired by a performance utility used to measure   *
      * program performance. It monitors the health of systems         *
      * in various regions over extended periods, up to 30             *
      * days, as well as moment-to-moment. This test example shows     *
      * how to use many Test4z APIs for validating a non-trival        *
      * application program.                                           *
      *                                                                *
      * ZTTSECTN                                                       *
      * This test example shows how to run a unit test on a section in *
      * the COBOL application program. This will isolate the section   *
      * from the rest of the user program and only execute it. It also *
      * shows how to get the address of user variables in user program *
      * and display them.                                              *
      *                                                                *
      * ZTTSPYPG                                                       *
      * This unit test example shows how to "spy" on programs that     *
      * invoke other programs as subroutines.                          *
      *                                                                *
      * ZTTSTUBP                                                       *
      * This test example shows how to run a unit test on a program    *
      * that calls another program that is to be stubbed out and       *
      * replaced by the logic from this unit test.                     *
      *                                                                *
      * ZTTWATCH                                                       *
      * This test example shows how to run a unit test on a program    *
      * that watches for changes to user program variables.            *
      *                                                                *
      * ZTTWATCS                                                       *
      * This test example shows how to run a unit test on a program    *
      * that spies on user program variable and watches them as they   *
      * change. This is similar to ZTTWATCH, but a spy allows the      *
      * test case to parse the list of variable changes after the      *
      * program has ended.                                             *
      ******************************************************************

      ******************************************************************
      * All API calls take the same form when calling them. First you  *
      * move LOW-VALUES to the I_ variable (input variable) for the    *
      * API. The I_ variable is always I_ followed by the name of the  *
      * API. Then you call ZTESTUT passing the name of the entire      *
      * working-storage area for the API data followed by any          *
      * additional required output variables. The working-storage API  *
      * name is a concatenation of ZWS_ followed by the name of the    *
      * API. For example, to use the Test API call you would code:     *
      *                                                                *
      *      MOVE LOW-VALUES TO I_TEST                                 *
      *      CALL ZTESTUT USING ZWS_TEST                               *
      *                                                                *
      *----------------------------------------------------------------*
      * This is the list of available APIs to perform unit testing.    *
      * The details for each specific API call can be found in the     *
      * most recent ZTESTWS.cpy file. To locate the most recent        *
      * version, refer to the name of the member being copied in by    *
      * the ZTESTWS.cpy itself. Usually the ZTESTWS.cpy member name    *
      * will include the highest numerical suffix (1, 2, 3, etc.).     *
      *----------------------------------------------------------------*
      *                                                                *
      *   ZWS_AddContainerRecord (Technical Preview)                   *
      *        This API allows a unit test to programmatically create  *
      *        a CICS container record in the CICS mocking object.     *
      *        This record might be required as input by the user      *
      *        application program. The container record can also      *
      *        be defined using ZLINPUT on the _DispatchCICS API,      *
      *        however it might be easier for a unit test to define    *
      *        this data programmatically.                             *
      *                                                                *
      *   ZWS_AddRecord                                                *
      *        This API will add a record to a file object. This file  *
      *        object could be manually created or more likely,        *
      *        obtained from the test harness by using the             *
      *        _GetFileObject API.                                     *
      *                                                                *
      *   ZWS_AddRowset                                                *
      *        This API will add a new rowset to an existing Db2       *
      *        object. This Db2 object could be manually created or    *
      *        more likely, obtained from the test harness by using    *
      *        the _GetDb2Object API.                                  *
      *                                                                *
      *   ZWS_AddTDQRecord (Technical Preview)                         *
      *        This API allows a unit test to programmatically create  *
      *        a CICS TD queue record in the CICS mocking object. This *
      *        record might be required as input by the user           *
      *        application program. The TD queue record can also       *
      *        be defined using ZLINPUT on the _DispatchCICS API,      *
      *        however it might be easier for a unit test to define    *
      *        this data programmatically.                             *
      *                                                                *
      *   ZWS_AddTSQRecord (Technical Preview)                         *
      *        This API allows a unit test to programmatically create  *
      *        a CICS TS queue record in the CICS mocking object. This *
      *        record might be required as input by the user           *
      *        application program. The TS queue record can also       *
      *        be defined using ZLINPUT on the _DispatchCICS API,      *
      *        however it might be easier for a unit test to define    *
      *        this data programmatically.                             *
      *                                                                *
      *   ZWS_Assert                                                   *
      *        This API will cause an assert to happen based on the    *
      *        value of the conditionCode variable. When using COBOL   *
      *        there is no automatic way to check the condition code   *
      *        value without coding your own IF statement. For this    *
      *        reason it is easier to use the _Fail API instead.       *
      *                                                                *
      *   ZWS_AssertCalled                                             *
      *        This API will check that the spied upon program was     *
      *        invoked during the execution of the test case. If it    *
      *        was not then an assert will be issued.                  *
      *                                                                *
      *   ZWS_AssertCalledOnce                                         *
      *        This API will check that the spied upon program was     *
      *        invoked exactly once during the execution of the test   *
      *        case. If it was not then an assert will be issued.      *
      *                                                                *
      *   ZWS_AssertCalledOnceSection                                  *
      *        This API will check that the spied upon section was     *
      *        invoked exactly once during the execution of the test   *
      *        case. If it was not then an assert will be issued.      *
      *                                                                *
      *   ZWS_AssertCalledOnceWith                                     *
      *        This API will check that the spied upon program was     *
      *        invoked with an exact match on the concatenated         *
      *        argument area exactly once during the execution of the  *
      *        test case. If it was not then an assert will be issued. *
      *                                                                *
      *   ZWS_AssertCalledSection                                      *
      *        This API will check that the spied upon section was     *
      *        invoked during the execution of the test case. If it    *
      *        was not then an assert will be issued.                  *
      *                                                                *
      *   ZWS_AssertCalledWith                                         *
      *        This API will check that the spied upon program was     *
      *        invoked with an exact match on the concatenated         *
      *        argument area during the execution of the test case. If *
      *        it was not then an assert will be issued.               *
      *                                                                *
      *   ZWS_AssertNoErrorsAIB (Future Planned)                       *
      *        This API will check all of the I/O that occurred during *
      *        an AIB spy. If any errors occurred then an assert will  *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_AssertNoErrorsCICS (Technical Preview)                   *
      *        This API will check all of the I/O that occurred during *
      *        a CICS spy. If any errors occurred then an assert will  *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_AssertNoErrorsDb2                                        *
      *        This API will check all of the I/O that occurred during *
      *        a Db2 spy. If any errors occurred then an assert will   *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_AssertNoErrorsESDS                                       *
      *        This API will check all of the I/O that occurred during *
      *        a ESDS spy. If any errors occurred then an assert will  *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_AssertNoErrorsIMS (Future Planned)                       *
      *        This API will check all of the I/O that occurred during *
      *        an IMS spy. If any errors occurred then an assert will  *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_AssertNoErrorsKSDS                                       *
      *        This API will check all of the I/O that occurred during *
      *        a KSDS spy. If any errors occurred then an assert will  *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_AssertNoErrorsMQ (Future Planned)                        *
      *        This API will check all of the I/O that occurred during *
      *        an MQ spy. If any errors occurred then an assert will   *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_AssertNoErrorsQSAM                                       *
      *        This API will check all of the I/O that occurred during *
      *        a QSAM spy. If any errors occurred then an assert will  *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_AssertNoErrorsRRDS                                       *
      *        This API will check all of the I/O that occurred during *
      *        a RRDS spy. If any errors occurred then an assert will  *
      *        be issued.                                              *
      *                                                                *
      *   ZWS_CreateHarness                                            *
      *        This API will load recorded data created by previous    *
      *        record/replay processing to be used by mocking during a *
      *        unit test. The test harness has additional capability   *
      *        to instruct the test to use certain files, Db2, CICS or *
      *        programs as real instead of using recorded data. A      *
      *        mixture of mocked and real data can be used in a test   *
      *        harness.                                                *
      *        This API is the preferred method of performing unit     *
      *        test processing instead of the _LoadData API.           *
      *                                                                *
      *   ZWS_DispatchCICS (Technical Preview)                         *
      *        This API will combine the prepare module, get function  *
      *        and optionally the load data API calls and execute a    *
      *        set of CICS transactions/programs directly.             *
      *        One or more CICS transactions can be part of the set of *
      *        transactions that are invoked, with subsequent          *
      *        transactions dispatched through the use of EXEC CICS    *
      *        RETURN TRANSID calls.                                   *
      *                                                                *
      *   ZWS_DispatchIMS (Future Planned)                             *
      *        This API will combine the prepare module, get function  *
      *        and optionally the load data API calls and execute a    *
      *        set of IMS transactions/programs directly.              *
      *        One or more IMS transactions can be part of the set of  *
      *        transactions that are invoked, with subsequent          *
      *        transactions dispatched through the use of DL/1 ISRT    *
      *        calls to the alternate or express PCBs.                 *
      *                                                                *
      *   ZWS_Fail                                                     *
      *        This API will cause a test case failure to happen,      *
      *        unconditionally.                                        *
      *                                                                *
      *   ZWS_File                                                     *
      *        This API will create a file object block to be used by  *
      *        the unit test. The QSAM and VSAM control blocks may     *
      *        refer to this file object block and it must be created  *
      *        prior to the QSAM or VSAM mocking calls if the test     *
      *        case is to contain data provided data.                  *
      *        Initial data can be provided to this API to load        *
      *        records that are to be used for the unit test.          *
      *        Additionally or alternatively, the _LoadData API can be *
      *        used to load records for the unit test.                 *
      *        If both methods are used, then both sets of data are    *
      *        available in the test.                                  *
      *                                                                *
      *   ZWS_GetAIBArgument (Future Planned)                          *
      *        This API will allow a user to obtain the address of     *
      *        any AIB argument in the AIB call. This API is for use   *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetArguments                                             *
      *        This API will populate test case variables with         *
      *        recorded program arguments. If the program under test   *
      *        requires variable to be passed to it, these variables   *
      *        can be specified in the test case using manual setup,   *
      *        or by using this API to populate the test case          *
      *        variables with the recorded data. In either case, these *
      *        variables are to be passed along with the call to the   *
      *        program under test.                                     *
      *                                                                *
      *   ZWS_GetCICSArgument (Technical Preview)                      *
      *        This API will allow a user to obtain the address of     *
      *        any CICS argument in the CICS call. This API is for use *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetDb2Argument                                           *
      *        This API will allow a user to obtain the address of     *
      *        any Db2 argument in the Db2 call. This API is for use   *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetDb2Object                                             *
      *        This API will obtain a Db2 object from the test         *
      *        harness. The obtained object can be passed to other     *
      *        APIs for use.                                           *
      *                                                                *
      *   ZWS_GetESDSArgument                                          *
      *        This API will allow a user to obtain the address of     *
      *        any ESDS argument in the ESDS call. This API is for use *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetFileObject                                            *
      *        This API will obtain a file object from the test        *
      *        harness. The obtained object can be passed to other     *
      *        APIs for use. Any file type (KSDS, ESDS, RRDS or QSAM)  *
      *        can be obtained and mapped to the appropriate file type *
      *        object.                                                 *
      *                                                                *
      *   ZWS_GetFunction                                              *
      *        This API will request the address of a program within   *
      *        the prepared module.                                    *
      *        The _RunFunction combines the _PrepareModule and        *
      *        _GetFunction APIs and additionally calls the function   *
      *        to begin execution. The _RunFunction API reduces the    *
      *        coding for the test case and is the preferred method of *
      *        starting the test case.                                 *
      *                                                                *
      *   ZWS_GetIMSArgument (Future Planned)                          *
      *        This API will allow a user to obtain the address of     *
      *        any IMS argument in the IMS call. This API is for use   *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetKSDSArgument                                          *
      *        This API will allow a user to obtain the address of     *
      *        any KSDS argument in the KSDS call. This API is for use *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetMQArgument (Future Planned)                           *
      *        This API will allow a user to obtain the address of     *
      *        any MQ argument in the MQ call. This API is for use     *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetParm                                                  *
      *        This API will return the address and length of any      *
      *        Test4z parameter data defined in the ZLOPTS option      *
      *        PARM(xxx) for the execution of the test. The PARM(xxx)  *
      *        option allows a user to pass data from the ZLOPTS file  *
      *        to the running test.                                    *
      *                                                                *
      *   ZWS_GetProgramArgument                                       *
      *        This API will allow a user to obtain the address of     *
      *        any argument in the program call. This API is for use   *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetProgramObject                                         *
      *        This API will obtain a program object from the test     *
      *        harness. The obtained object can be passed to other     *
      *        APIs for use.                                           *
      *                                                                *
      *   ZWS_GetQSAMArgument                                          *
      *        This API will allow a user to obtain the address of     *
      *        any QSAM argument in the QSAM call. This API is for use *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetRRDSArgument                                          *
      *        This API will allow a user to obtain the address of     *
      *        any RRDS argument in the RRDS call. This API is for use *
      *        within a spy or mock callback.                          *
      *                                                                *
      *   ZWS_GetRowsetObject                                          *
      *        This API will obtain a rowset object from a Db2 object. *
      *        The obtained object can be passed to other APIs for     *
      *        use.                                                    *
      *                                                                *
      *   ZWS_GetVariable                                              *
      *        This API will request the address of a variable located *
      *        within the prepared module and selected function.       *
      *                                                                *
      *   ZWS_LoadData                                                 *
      *        This API will load recorded data created by previous    *
      *        record & replay processing to be used by any mocking by *
      *        a unit test.                                            *
      *                                                                *
      *   ZWS_Message                                                  *
      *        This API will write a message to the results log        *
      *        without altering the condition code of the test.        *
      *                                                                *
      *   ZWS_MockAIB (Future Planned)                                 *
      *        This API will create an IMS/AIB mocking object that may *
      *        refer to a previously created load object to allow the  *
      *        Test4z runtime to be used to respond to calls           *
      *        logically.                                              *
      *                                                                *
      *   ZWS_MockCICS (Technical Preview)                             *
      *        This API will create a CICS mocking object that allows  *
      *        the Test4z runtime to be used to respond to calls       *
      *        logically when there is no test harness and recorded    *
      *        data.                                                   *
      *                                                                *
      *   ZWS_MockCICSStatement (Technical Preview)                    *
      *        This API will create a CICS statement mocking object    *
      *        that provides the input and output arguments to be      *
      *        used by the user application program if the unit test   *
      *        is to provide the statement data for the test.          *
      *                                                                *
      *   ZWS_MockDb2                                                  *
      *        This API will create a Db2 mocking object that may      *
      *        refer to a previously created load object to allow the  *
      *        Test4z runtime to be used to respond to calls           *
      *        logically.                                              *
      *        Additionally, CAF calls are handled by this API.        *
      *                                                                *
      *   ZWS_MockESDS                                                 *
      *        This API will create a VSAM/ESDS (entry sequence        *
      *        access) mocking object that may refer to a previously   *
      *        created file object that provides user-supplied data or *
      *        a previously created load object to allow the Test4z    *
      *        runtime to be used to respond to calls logically.       *
      *                                                                *
      *   ZWS_MockIMS (Future Planned)                                 *
      *        This API will create a IMS mocking object that may      *
      *        refer to a previously created load object to allow the  *
      *        Test4z runtime to be used to respond to calls           *
      *        logically.                                              *
      *                                                                *
      *   ZWS_MockKSDS                                                 *
      *        This API will create a VSAM/KSDS (keyed access) mocking *
      *        object that may refer to a previously created file      *
      *        object that provides user-supplied data or a previously *
      *        created load object to allow the Test4z runtime to be   *
      *        used to respond to calls logically.                     *
      *                                                                *
      *   ZWS_MockMQ (Future Planned)                                  *
      *        This API will create an MQ mocking object that may      *
      *        refer to a previously created load object to allow the  *
      *        Test4z runtime to be used to respond to calls           *
      *        logically.                                              *
      *                                                                *
      *   ZWS_MockProgram                                              *
      *        This API will create a program mocking object to be     *
      *        used with previously recorded data. This API will       *
      *        prevent the called program from being executed. Instead *
      *        it will match arguments being passed to the called      *
      *        program and use recorded data to replace arguments on   *
      *        return. This API may refer to a previously created load *
      *        object to allow the Test4z runtime to be used to        *
      *        respond to calls logically.                             *
      *                                                                *
      *   ZWS_MockQSAM                                                 *
      *        This API will create a QSAM (sequential file) mocking   *
      *        object that may refer to a previously created file      *
      *        object that provides user-supplied data or a previously *
      *        created load object to allow the Test4z runtime to be   *
      *        used to respond to calls logically.                     *
      *                                                                *
      *   ZWS_MockRRDS                                                 *
      *        This API will create a VSAM/RRDS (relative record       *
      *        access) mocking object that may refer to a previously   *
      *        created file object that provides user-supplied data or *
      *        a previously created load object to allow the Test4z    *
      *        runtime to be used to respond to calls logically.       *
      *                                                                *
      *   ZWS_PrepareModule                                            *
      *        This API will load and prepare a user module to be run  *
      *        as a unit test.                                         *
      *        The _RunFunction combines the _PrepareModule and        *
      *        _GetFunction APIs and additionally calls the function   *
      *        to begin execution. The _RunFunction API reduces the    *
      *        coding for the test case and is the preferred method of *
      *        starting the test case.                                 *
      *                                                                *
      *   ZWS_PrepareSection                                           *
      *        This API will prepare a section/paragraph within a      *
      *        program within the prepared module and return the       *
      *        starting address.                                       *
      *                                                                *
      *   ZWS_PrintFile                                                *
      *        This API will display some or all of the records        *
      *        owned by a file object.                                 *
      *                                                                *
      *   ZWS_PrintRowset                                              *
      *        This API will display all of the records owned by a Db2 *
      *        rowset object.                                          *
      *                                                                *
      *   ZWS_RegisterCoverage                                         *
      *        This API will register the existence of a code coverage *
      *        handler to be called at the end of the unit test.       *
      *                                                                *
      *   ZWS_RegisterPattern                                          *
      *        This API will register the existence of a pattern match *
      *        table to be used for matching data in the effort to     *
      *        identify mismatches.                                    *
      *                                                                *
      *   ZWS_Rowset                                                   *
      *        This API will create a Db2 rowset required for a Db2    *
      *        mocking object.                                         *
      *                                                                *
      *   ZWS_RunFunction                                              *
      *        This API will combine both the _PrepareModule and the   *
      *        _GetFunction API calls and execute the program          *
      *        directly. This function reduces the coding for the test *
      *        case and is the preferred method of starting the test   *
      *        case.                                                   *
      *                                                                *
      *   ZWS_SetBreakpoint                                            *
      *        This API will set a break point at both the start of    *
      *        the execution of a section (or paragraph) of a COBOL    *
      *        user application program and at the end of the          *
      *        execution of the section.                               *
      *                                                                *
      *   ZWS_SetTestpoint                                             *
      *        This API will set a callback address to your test case  *
      *        logic that is to be executed when a test point call has *
      *        been added to a user program.                           *
      *                                                                *
      *   ZWS_Stop                                                     *
      *        This API will cause a test case to end processing,      *
      *        unconditionally. This will not affect the current       *
      *        return code for the test case. The current return code  *
      *        for the test case will remain unchanged.                *
      *                                                                *
      *   ZWS_StubProgram                                              *
      *        This API will define a subroutine within a load module  *
      *        to be stubbed out and replaced by the routine defined   *
      *        in the test. No recorded program data nor argument      *
      *        matching will be performed.                             *
      *                                                                *
      *   ZWS_Test                                                     *
      *        This API will register a test to be included in the     *
      *        suite of tests about to be started.                     *
      *                                                                *
      *   ZWS_WatchVariable                                            *
      *        This API will set a callback address to your test case  *
      *        logic that is to be executed when a watched COBOL       *
      *        variable in a user program is changed.                  *
      *                                                                *
      ******************************************************************
      * For each of these spy API calls there are additional routines  *
      * that can be used to determine what the spy detected during     *
      * its processing. These additional routines are as follows and   *
      * their description are found further in this copy file:         *
      *                                                                *
      *   _AssertNoErrors                                              *
      *   _AssertCalled                                                *
      *   _AssertCalledOnce                                            *
      *   _AssertCalledWith                                            *
      *   _AssertCalledOnceWith                                        *
      *   _Display                                                     *
      *                                                                *
      * You must use the correct type of the above API calls depending *
      * upon what type of spy routine is being performed. For example, *
      * for QSAM you would used _AssertNoErrorsQSAM.                   *
      ******************************************************************
      *                                                                *
      *   ZWS_DeregisterSpy                                            *
      *        This API will de-register any previously registered     *
      *        low-level spy handler with the same function address.   *
      *                                                                *
      *   ZWS_SpyAIB (Future Planned)                                  *
      *        This API will create a built-in spy routine on IMS/AIB  *
      *        calls. Once created, other spy API routines can be used *
      *        with the spy object to test situations.                 *
      *                                                                *
      *   ZWS_SpyCICS (Technical Preview)                              *
      *        This API will create a built-in spy routine on CICS     *
      *        calls. Once created, other spy API routines can be used *
      *        with the spy object to test situations.                 *
      *                                                                *
      *   ZWS_SpyDb2                                                   *
      *        This API will create a built-in spy routine on Db2      *
      *        calls. Once created, other spy API routines can be used *
      *        with the spy object to test situations.                 *
      *                                                                *
      *   ZWS_SpyESDS                                                  *
      *        This API will create a built-in spy routine on a        *
      *        VSAM/ESDS (entry sequence) file. Once created, other    *
      *        spy API routines can be used with the spy object to     *
      *        test situations.                                        *
      *                                                                *
      *   ZWS_SpyIMS (Future Planned)                                  *
      *        This API will create a built-in spy routine on IMS      *
      *        calls. Once created, other spy API routines can be used *
      *        with the spy object to test situations.                 *
      *                                                                *
      *   ZWS_SpyKSDS                                                  *
      *        This API will create a built-in spy routine on a        *
      *        VSAM/KSDS (keyed file) file. Once created, other spy    *
      *        API routines can be used with the spy object to test    *
      *        situations.                                             *
      *                                                                *
      *   ZWS_SpyMQ (Future Planned)                                   *
      *        This API will create a built-in spy routine on MQ       *
      *        calls. Once created, other spy API routines can be used *
      *        with the spy object to test situations.                 *
      *                                                                *
      *   ZWS_SpyProgram                                               *
      *        This API will create a built-in spy routine on program  *
      *        calls. Once created, other spy API routines can be used *
      *        with the spy object to test situations.                 *
      *                                                                *
      *   ZWS_SpyQSAM                                                  *
      *        This API will create a built-in spy routine on a QSAM   *
      *        (sequential file) file. Once created, other spy API     *
      *        routines can be used with the spy object to test        *
      *        situations.                                             *
      *                                                                *
      *   ZWS_SpyRRDS                                                  *
      *        This API will create a built-in spy routine on a        *
      *        VSAM/RRDS (relative record) file. Once created, other   *
      *        spy API routines can be used with the spy object to     *
      *        test situations.                                        *
      *                                                                *
      *   ZWS_SpySection                                               *
      *        This API will create a built-in spy routine on section  *
      *        calls. Once created, other spy API routines can be used *
      *        with the spy object to test situations.                 *
      *                                                                *
      *   ZWS_SpyVariable                                              *
      *        This API will create a built-in spy routine on COBOL    *
      *        WORKING-STORAGE or LOCAL-STORAGE variables. Once        *
      *        created, other spy API routines can be used with the    *
      *        spy object to test situations.                          *
      *                                                                *
      ******************************************************************
      *   ZWS_DisplayAIB (Future Planned)                              *
      *        This API will format and display any individual I/O     *
      *        call made to AIB. You may reference the lastCall from   *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      *   ZWS_DisplayCICS (Technical Preview)                          *
      *        This API will format and display any individual I/O     *
      *        call made to CICS. You may reference the lastCall from  *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      *   ZWS_DisplayDb2                                               *
      *        This API will format and display any individual I/O     *
      *        call made to Db2. You may reference the lastCall from   *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      *   ZWS_DisplayESDS                                              *
      *        This API will format and display any individual I/O     *
      *        call made to ESDS. You may reference the lastCall from  *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      *   ZWS_DisplayIMS (Future Planned)                              *
      *        This API will format and display any individual I/O     *
      *        call made to IMS. You may reference the lastCall from   *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      *   ZWS_DisplayKSDS                                              *
      *        This API will format and display any individual I/O     *
      *        call made to KSDS. You may reference the lastCall from  *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      *   ZWS_DisplayMQ (Future Planned)                               *
      *        This API will format and display any individual I/O     *
      *        call made to MQ. You may reference the lastCall from    *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      *   ZWS_DisplayProgram                                           *
      *        This API will format and display any individual program *
      *        call made. You may reference the lastCall from the      *
      *        callback's spy object or parse through the vector table *
      *        of calls.                                               *
      *                                                                *
      *   ZWS_DisplayQSAM                                              *
      *        This API will format and display any individual I/O     *
      *        call made to QSAM. You may reference the lastCall from  *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      *   ZWS_DisplayRRDS                                              *
      *        This API will format and display any individual I/O     *
      *        call made to RRDS. You may reference the lastCall from  *
      *        the callback's spy object or parse through the vector   *
      *        table of calls.                                         *
      *                                                                *
      ******************************************************************
      *                      L O W - L E V E L   A P I                 *
      *                                                                *
      * These are the low-level APIs that allow a more granular        *
      * control over the processing. Usually these API calls are only  *
      * if a callback is preferred over the built-in spying logic. All *
      * low-level API calls start with ZWS_LL.                         *
      *                                                                *
      ******************************************************************
      *   ZWS_LLDeregisterSpy                                          *
      *        This API will de-register any previously registered     *
      *        low-level spy handler with the same function address.   *
      *                                                                *
      *   ZWS_LLFile                                                   *
      *        This API will create a low-level file object block to   *
      *        be used by the unit test. The QSAM and VSAM control     *
      *        blocks may refer to this low-level file object block    *
      *        and it must be created prior to the low-level QSAM or   *
      *        VSAM mocking calls if the test case is to contain data  *
      *        provided data. Initial data can be provided to this API *
      *        to load records that are to be used for the unit test.  *
      *        Additionally or alternatively, the _LoadData API can be *
      *        used to load records for the unit test. If both methods *
      *        are used, then both sets of data are available in the   *
      *        test.                                                   *
      *                                                                *
      *   ZWS_LLMockAIB (Future Planned)                               *
      *        This API will create an IMS/AIB low-level mocking       *
      *        object.                                                 *
      *                                                                *
      *   ZWS_LLMockCICS (Technical Preview)                           *
      *        This API will create a CICS low-level mocking object.   *
      *                                                                *
      *   ZWS_LLMockDb2                                                *
      *        This API will create a Db2 low-level mocking object.    *
      *        CAF calls are handled as well.                          *
      *                                                                *
      *   ZWS_LLMockESDS                                               *
      *        This API will create a VSAM/ESDS (entry sequence        *
      *        access) low-level mocking object that may refer to a    *
      *        previously created low-level file object.               *
      *                                                                *
      *   ZWS_LLMockIMS (Future Planned)                               *
      *        This API will create an IMS low-level mocking object.   *
      *                                                                *
      *   ZWS_LLMockKSDS                                               *
      *        This API will create a VSAM/KSDS (keyed access)         *
      *        low-level mocking object that may refer to a previously *
      *        created low-level file object.                          *
      *                                                                *
      *   ZWS_LLMockMQ (Future Planned)                                *
      *        This API will create an MQ low-level mocking object.    *
      *                                                                *
      *   ZWS_LLMockQSAM                                               *
      *        This API will create a QSAM (sequential file) low-level *
      *        mocking object that may refer to a previously created   *
      *        low-level file object.                                  *
      *                                                                *
      *   ZWS_LLMockRRDS                                               *
      *        This API will create a VSAM/RRDS (relative record       *
      *        access) low-level mocking object that may refer to a    *
      *        previously created low-level file object.               *
      *                                                                *
      *   ZWS_LLRegisterSpy                                            *
      *        This API will register the existence of a low-level spy *
      *        handler to be called during the interactions between    *
      *        the user program and any middleware or other user       *
      *        programs during a unit test. The recommended API calls  *
      *        for spy processing are the APIs that start with         *
      *        ZWS_Spy_ and have the type as the suffix. These         *
      *        recommended spy routines provide built-in capabilities  *
      *        whereas the low-level spy routines leave all processing *
      *        up to the user.                                         *
      *                                                                *
      ******************************************************************
