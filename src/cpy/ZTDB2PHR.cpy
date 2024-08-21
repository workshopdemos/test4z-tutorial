      ******************************************************************
      * Broadcom Test4z example. See ZTDB2PHN  for details.            *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      ******************************************************************

      ******************************************************************
      * Employee phone record update request.                          *
      *                                                                *
      * The request types are (D)elete, (U)pdate, (C)reate, (K)eep,    *
      * and (S)earch found in the PHUPDATE data set. These are similar *
      * to the standard database CRUD operations, but with some        *
      * variation to better demonstrate unit test validation.          *
      *                                                                *
      * For example, a unit test for KEEP should confirm the record    *
      * is valid (S = SEARCH), attempt to delete it (D = DELETE),      *
      * and then confirm it wasn't changed (S again). The unit test    *
      * can interrogate the log records in PHLOG to confirm.           *
      ******************************************************************
       01  UPD-PHONE-UPDATE-FILE-RECORD.
           05 UPD-REQUIRED-FIELDS.
               10 UPD-REQUEST-TYPE  PIC X.
                  88 UPD-DELETE     VALUE 'D'.
                  88 UPD-UPDATE     VALUE 'U'.
                  88 UPD-CREATE     VALUE 'C'.
                  88 UPD-KEEP       VALUE 'K'.
                  88 UPD-SEARCH     VALUE 'S'.
               10 UPD-KEY           PIC X(6).
               10 UPD-MESSAGE       PIC X(40).
           05 UPD-REQUEST-FIELDS.
               10 UPD-LASTNAME      PIC X(40).
               10 UPD-FIRSTNAME     PIC X(40).
               10 UPD-PHONE         PIC X(10).
           05 FILLER                PIC X(63).

      ******************************************************************
      * Employee phone record update log.                              *
      *                                                                *
      * This includes the information from the request record above    *
      * and its final disposition.                                     *
      ******************************************************************
       01  PHL-PHONE-LOG-RECORD.
           05 PHL-REQUIRED-FIELDS.
               10 FILLER            PIC X(4) VALUE 'REQ='.
               10 PHL-REQUEST-TYPE  PIC X.
               10 FILLER            PIC X(5) VALUE ' KEY='.
               10 PHL-KEY           PIC X(6).
           05 PHL-PROCESS-FIELDS.
               10 FILLER            PIC X(4) VALUE ' OK='.
               10 PHL-REQUEST-VALIDITY-FLAG  PIC X.
                  88 PHL-VALID-REQUEST   VALUE 'Y'.
                  88 PHL-INVALID-REQUEST VALUE 'N'.
               10 FILLER            PIC X(4) VALUE ' ST='.
               10 PHL-STATUS        PIC X(20).
           05 PHL-REQUEST-FIELDS-1.
               10 FILLER            PIC X(5) VALUE ' MSG='.               
               10 PHL-MESSAGE       PIC X(20).           
           05 PHL-REQUEST-FIELDS-2.
               10 FILLER            PIC X(4) VALUE ' LN='.                 
               10 PHL-LASTNAME      PIC X(20).
               10 FILLER            PIC X(4) VALUE ' FN='.                     
               10 PHL-FIRSTNAME     PIC X(20).
               10 FILLER            PIC X(4) VALUE ' PH='.                     
               10 PHL-PHONE         PIC X(10).