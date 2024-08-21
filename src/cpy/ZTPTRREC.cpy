      ******************************************************************
      * Copybook for ZTPTRNDY and related unit tests.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * Record for EXEC-LOG file is a simplified version of a          *
      * program execution log with some embellisments.                 *
      *                                                                *
      * YYYY-MM-DD HH:MM:SS CCC RRRR UUU III WWWWW SSSS TTTTTT         *
      *                                                                *
      * CC     = Return code 000, 004, 008, 012, or 016.               *
      * RRRR   = CPU time in seconds.                                  *
      * UUU    = System workload metric, self-reported on 0-100 scale. *
      * III    = System identifier, typically by region.               *
      * WWWW   = Relative workload score of HIGH, OK, MED, LOW         *
      * SSSS   = Relative response score of FAST, OK, SLOW, or BAD     *
      * TTTTTT = Request type. Typically "OK", but if a program wasn't *
      *          run, indicates disposition like ERROR or AUTH.        *
      *                                                                *
      * NB: Spaces included between fields for easier file browsing.   *
      ******************************************************************
       01  :ELR:-EXEC-LOG-RECORD.
           05 :ELR:-DATE-TIME           PIC X(16).
           05 :ELR:-KEY REDEFINES :ELR:-DATE-TIME.
               10 :ELR:-DATE.
                  15 :ELR:-YEAR         PIC 9(4).
                  15 FILLER             PIC X.
                  15 :ELR:-MONTH        PIC 9(2).
                  15 FILLER             PIC X.
                  15 :ELR:-DAY          PIC 9(2).
               10 FILLER                PIC X.
               10 :ELR:-TIME.
                  15 :ELR:-HOUR         PIC 9(2).
                  15 FILLER             PIC X.
                  15 :ELR:-MINUTE       PIC 9(2).
           05 FILLER                    PIC X.
           05 :ELR:-SECOND              PIC 9(2).
           05 FILLER                    PIC X.
           05 :ELR:-DATA.
              10 :ELR:-RETURN-CODE      PIC 9(2).
              10 FILLER                 PIC X.
              10 :ELR:-CPU-TIME         PIC 9(5).
              10 FILLER                 PIC X.
              10 :ELR:-WORKLOAD         PIC 9(3).
              10 FILLER                 PIC X.
              10 :ELR:-SYSTEM-ID        PIC X(3).
              10 FILLER                 PIC X.
              10 :ELR:-RESPONSE-SCORE   PIC X(4).
                   88 :ELR:-RESPONSE-FAST VALUE 'FAST'.
                   88 :ELR:-RESPONSE-OK   VALUE 'OK  '.
                   88 :ELR:-RESPONSE-SLOW VALUE 'SLOW'.
                   88 :ELR:-RESPONSE-BAD  VALUE 'BAD '.
              10 FILLER                 PIC X.
              10 :ELR:-WORKLOAD-SCORE   PIC X(4).
                   88 :ELR:-WORKLOAD-HIGH VALUE 'HIGH'.
                   88 :ELR:-WORKLOAD-OK   VALUE 'OK  '.
                   88 :ELR:-WORKLOAD-MED  VALUE 'MED '.
                   88 :ELR:-WORKLOAD-LOW  VALUE 'LOW '.
              10 FILLER                 PIC X.
              10 :ELR:-RESPONSE-TYPE    PIC X(6).
                   88 :ELR:-TYPE-AUTH     VALUE 'AUTH  '.
                   88 :ELR:-TYPE-ERROR    VALUE 'ERROR '.
                   88 :ELR:-TYPE-FORBID   VALUE 'FORBID'.
                   88 :ELR:-TYPE-OK       VALUE 'OK    '.
              10 FILLER                 PIC X(27).

      ******************************************************************
      * Record for HEALTH-LOG file is a summary of the server logs     *
      * within a given time interval (default 5 minutes).              *
      *                                                                *
      * NB: Spaces included between fields for easier file browsing.   *
      ******************************************************************
       01  :HLR:-HEALTH-LOG-RECORD.
           05 :HLR:-INTERVAL                PIC X(16).
           05 :HLR:-KEY REDEFINES :HLR:-INTERVAL.
               10 :HLR:-DATE.
                   15 :HLR:-YEAR            PIC 9(4).
                   15 FILLER                PIC X.
                   15 :HLR:-MONTH           PIC 9(2).
                   15 FILLER                PIC X.
                   15 :HLR:-DAY             PIC 9(2).
               10 FILLER                    PIC X.
               10 :HLR:-TIME.
                   15 :HLR:-HOUR            PIC 9(2).
                   15 FILLER                PIC X.
                   15 :HLR:-MINUTE          PIC 9(2).
           05 FILLER                        PIC X.
           05 :HLR:-DATA.
               10 :HLR:-REQUESTS            PIC 9(6).
               10 FILLER                    PIC X.
               10 :HLR:-CPU-TIME-TOTAL      PIC 9(6).
               10 FILLER                    PIC X.
               10 :HLR:-MAX-CPU-TIME        PIC 9(6).
               10 FILLER                    PIC X.
               10 :HLR:-WORKLOAD-TOTAL      PIC 9(6).
               10 FILLER                    PIC X.
               10 :HLR:-FAST                PIC 9(6).
               10 FILLER                    PIC X.
               10 :HLR:-OK                  PIC 9(6).
               10 FILLER                    PIC X.
               10 :HLR:-SLOW                PIC 9(6).
               10 FILLER                    PIC X.
               10 :HLR:-BAD                 PIC 9(6).
               10 FILLER                    PIC X.
               10 :HLR:-ERROR               PIC 9(6).
           05  FILLER                       PIC X.

      ******************************************************************
      * Record for HEALTH-TREND file is a summary of the health logs   *
      * within a longer time interval (default 60 minutes). It         *
      * includes basic heuristic "ratings" indicating the performance  *
      * during the reporting period.                                   *
      *                                                                *
      * NB: Spaces included between fields for easier file browsing.   *
      ******************************************************************
       01  :HTR:-HEALTH-TREND-RECORD.
           05 :HTR:-INTERVAL            PIC X(16).
           05 :HTR:-KEY REDEFINES :HTR:-INTERVAL.
               10 :HTR:-DATE.
                   15 :HTR:-YEAR        PIC 9(4).
                   15 FILLER            PIC X.
                   15 :HTR:-MONTH       PIC 9(2).
                   15 FILLER            PIC X.
                   15 :HTR:-DAY         PIC 9(2).
               10 FILLER                PIC X.
               10 :HTR:-TIME.
                   15 :HTR:-HOUR        PIC 9(2).
                   15 FILLER            PIC X.
                   15 :HTR:-MINUTE      PIC 9(2).
           05 FILLER                    PIC X.
           05 :HTR:-DATA.
               10 :HTR:-RESPONSE-RATING PIC 9(2).
               10 FILLER                PIC X.
               10 :HTR:-WORKLOAD-RATING PIC 9(2).
               10 FILLER                PIC X.
               10 :HTR:-HAPPY-RATING    PIC 9(2).
               10 FILLER                PIC X.
               10 :HTR:-REQUESTS        PIC 9(6).
               10 FILLER                PIC X.
               10 :HTR:-REPORT-DURATION PIC 9(3).
           05  FILLER                   PIC X(44).

      ******************************************************************
      * Grand totals of the health trend records within                *
      * a given health interval used for calculating the final         *
      * response, workload, and "happy" ratings on scale of 1-5        *
      * as part of the HEALTH-TREND output.                            *
      *                                                                *
      * See the HTR-HEALTH-TREND-RECORD for more details.              *
      ******************************************************************
       01 :HTT:-HEALTH-TREND-TOTALS.
           05 :HTT:-REQUESTS-TOTAL      PIC 9(6).
           05 :HTT:-CPU-TIME-TOTAL      PIC 9(6).
           05 :HTT:-MAX-CPU-TIME        PIC 9(6).
           05 :HTT:-WORKLOAD-TOTAL      PIC 9(6).
           05 :HTT:-FAST-TOTAL          PIC 9(6).
           05 :HTT:-OK-TOTAL            PIC 9(6).
           05 :HTT:-SLOW-TOTAL          PIC 9(6).
           05 :HTT:-BAD-TOTAL           PIC 9(6).
           05 :HTT:-ERROR-TOTAL         PIC 9(6).

      ******************************************************************
      * Overall performance statistics as an input data sanity check.  *
      ******************************************************************
       01  :RPT:-STATS-REPORT-TOTALS.
           05 :RPT:-START-INTERVAL           PIC X(16).
           05 :RPT:-END-INTERVAL             PIC X(16).
           05 :RPT:-DATA.
               10 :RPT:-VALID-EXEC-LOGS      PIC 9(6).
               10 :RPT:-HEALTH-LOG-UPDATES   PIC 9(6).
               10 :RPT:-HEALTH-LOG-ADDITIONS PIC 9(6).
               10 :RPT:-HEALTH-TREND-COUNT   PIC 9(6).
               10 :RPT:-ERROR-EXEC-LOGS      PIC 9(6).
               10 :RPT:-INVALID-EXEC-LOGS    PIC 9(6).