      ******************************************************************
      * Broadcom Test4z example. See ZTPHRNMM for details.             *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      ******************************************************************

       01  :HRN:-WIDGET-SALES-REC.
           05 :HRN:-WIDGET-ID                   PIC X(7).
           05 :HRN:-WIDGET-ID-REF REDEFINES :HRN:-WIDGET-ID.
               10 :HRN:-WIDGET-CATEGORY         PIC X(1).
               10 :HRN:-WIDGET-NUMBER           PIC 9(6).
    
       01  :HRN:-WIDGET-SALES-RECENT-REC.
           05 :HRN:-WIDGET-ID                   PIC X(7).
           05 :HRN:-WIDGET-ID-REF REDEFINES :HRN:-WIDGET-ID.
               10 :HRN:-WIDGET-CATEGORY         PIC X(1).
               10 :HRN:-WIDGET-NUMBER           PIC 9(6).
           05 :HRN:-WIDGET-SALES-RECENT-DATA    OCCURS 30 TIMES.
               10 :HRN:-WIDGET-SALES-RECENT     PIC 9(4).

       01  :HRN:-WIDGET-SALES-PROMO-REC.
           05 :HRN:-WIDGET-ID                   PIC X(7).
           05 :HRN:-WIDGET-ID-REF REDEFINES :HRN:-WIDGET-ID.
               10 :HRN:-WIDGET-CATEGORY         PIC X(1).
               10 :HRN:-WIDGET-NUMBER           PIC 9(6).
           05 :HRN:-WIDGET-SALES-PROMO-DATA.
               10 :HRN:-WIDGET-SALES-SCORE          PIC 9(2).
               10 :HRN:-WIDGET-SALES-AVG            PIC 9(4).
               10 :HRN:-WIDGET-SALES-MAX            PIC 9(4).
               10 :HRN:-WIDGET-SALES-HOT-COUNT      PIC 9(2).
               10 :HRN:-WIDGET-SALES-TOP3-DAYS      OCCURS 3 TIMES.
                   15 :HRN:-WIDGET-SALES-TOP-DAY    PIC 9(2).
