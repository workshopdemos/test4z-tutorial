       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTRECRD' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to use the dynamic recording proxy *
      * to cause a recording of an aliased user program in an IMS      *
      * environment.                                                   *
      * Note that this dynamic recording proxy technique might be used *
      * in other environments to record data. CICS however, is not an  *
      * environment that will allow this.                              *
      *                                                                *
      * Notes:                                                         *
      *                                                                *
      * == The name of this proxy program must be the same as the      *
      *    IMS program. That way, IMS will load this program instead   *
      *    of the user program.                                        *
      *                                                                *
      * == The user program load module is to be renamed to something  *
      *    unique and that name is to be placed into the moduleAlias   *
      *    variable in the API below. An easy way to cause this        *
      *    renaming is to link edit the user program with an alias.    *
      *                                                                *
      * == The IMS STEPLIB must include the Test4z load library in its *
      *    list.                                                       *
      *                                                                *
      * == If the IMS JCL does not have a DD for ZLDATA, then it can   *
      *    dynamically added by the API if you place the DSN in the    *
      *    zldataDataset variable and optionally a member in           *
      *    zldataMember. The member name is only required if a name    *
      *    other than the name of the moduleName is wanted.            *
      *                                                                *
      * == If the IMS JCL does not have a DD for ZLDETAIL, then it can *
      *    dynamically added by the API if you place the DSN in the    *
      *    zldetailDataset variable. If the data set specified is a    *
      *    partitioned data set, then it will be processed when lookup *
      *    of argument lengths is needed. If the data set specified is *
      *    a sequential data set, then the first 44 bytes of each      *
      *    record is to be the name of a partitioned data set. Any     *
      *    number of records may be provided in the sequential data    *
      *    set and all of the partitioned data sets that these records *
      *    reference will be contatenated together to allow multiple   *
      *    data sets for argument length lookup.                       *
      *                                                                *
      * For IMS, you must add the MODE(IMS) into the API reference     *
      * area for ZLOPTS. This is how the API will know to obtain the   *
      * PSB list and pass it to the module alias.                      *
      *                                                                *
      * NB: Note that since the _Record API is separate from the unit  *
      *     testing framework and calls ZTESTREC instead of ZTESTUT,   *
      *     the "PROCESS PGMN(LM),NODYNAM" compiler options are NOT    *
      *     required.                                                  *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTREC.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       01  MY_ZLOPTS.
           03 PIC X(80) VALUE 'MODE(IMS)'.

       PROCEDURE DIVISION.

      ******************************************************************
      * Set up the API call to perform a dynamic recording.            *
      *                                                                *
      * 1. Set moduleName to the name of the original user program;    *
      *    it must be the same name as this program.                   *
      * 2. Set moduleAlias to the name that you renamed the original.  *
      * 3. Set zldataDataset to the data set name of the target ZLDATA.*
      * 4. Set zldetailDataset to the data set name of any ZLDETAIL    *
      *    data set if needed.                                         *
      * 5. Set zloptsData to the pointer and length of ZLOPTS data.    *
      ******************************************************************
           MOVE LOW-VALUES TO I_RECORD IN ZWS_RECORD
           MOVE 'ZTTRECRD' TO MODULENAME IN ZWS_RECORD
           MOVE 'MYALIAS' TO MODULEALIAS IN ZWS_RECORD
           MOVE 'MY.TEST4Z.ZLDATA' TO ZLDATADATASET IN ZWS_RECORD
           MOVE 'MY.TEST4Z.ZLDETAIL' TO ZLDETAILDATASET IN ZWS_RECORD
           SET PTR IN ZLOPTSDATA IN ZWS_RECORD TO ADDRESS OF MY_ZLOPTS
           MOVE LENGTH OF MY_ZLOPTS TO SIZ IN ZLOPTSDATA IN ZWS_RECORD

      ******************************************************************
      * Call the API to process the recording of the program.          *
      ******************************************************************
           CALL ZTESTREC USING ZWS_RECORD
           GOBACK.
