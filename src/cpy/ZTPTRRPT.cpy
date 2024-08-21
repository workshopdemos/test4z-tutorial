      ******************************************************************
      * Copybook for ZTPTRNDY report formatting records.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      ******************************************************************

      ******************************************************************
      * Formatting records for the STATS-REPORT.                       *
      ******************************************************************
       01  RPL-HDR1-STATS-REPORT-OUTPUT.
           05 FILLER                   PIC X(23)
              VALUE '==> TRENDY SUMMARY FOR '.
           05 RPL-START-INTERVAL       PIC X(16).
           05 FILLER                   PIC X(4)
              VALUE ' TO '.
           05 RPL-END-INTERVAL         PIC X(16).
           05 FILLER                   PIC X(21)
              VALUE SPACES.

       01  RPL-HDR2-STATS-REPORT-OUTPUT.
           05 FILLER                   PIC X(13) VALUE '  EXEC-LOGS  '.
           05 FILLER                   PIC X(13) VALUE ' HEALTH-ADDS '.
           05 FILLER                   PIC X(13) VALUE ' HEALTH-CHGS '.
           05 FILLER                   PIC X(13) VALUE '  TREND-ADDS '.
           05 FILLER                   PIC X(13) VALUE '  ERROR-LOGS '.
           05 FILLER                   PIC X(13) VALUE 'INVALID-LOGS '.
           05 FILLER                   PIC X(3)  VALUE SPACES.

       01  RPL-STATS-REPORT-OUTPUT.
           05 RPL-VALID-EXEC-LOGS      PIC Z(10)9.
           05 FILLER                   PIC XX VALUE SPACE.
           05 RPL-HEALTH-LOG-ADDITIONS PIC Z(11)9.
           05 FILLER                   PIC X VALUE SPACE.
           05 RPL-HEALTH-LOG-UPDATES   PIC Z(11)9.
           05 FILLER                   PIC X VALUE SPACE.
           05 RPL-HEALTH-TREND-COUNT   PIC Z(11)9.
           05 FILLER                   PIC X VALUE SPACE.
           05 RPL-ERROR-EXEC-LOGS      PIC Z(11)9.
           05 FILLER                   PIC X VALUE SPACE.
           05 RPL-INVALID-EXEC-LOGS    PIC Z(11)9.
           05 FILLER                   PIC X(3) VALUE SPACES.

      ******************************************************************
      * Development-only report for spot checking performance data.    *
      ******************************************************************
       01  RPL-HDR-HEALTH-TREND-OUTPUT.
           05 RPL-INTERVAL            PIC X(14) VALUE '==> TRENDY INT'.
           05 FILLER                  PIC X(3) VALUE SPACE.
           05 RPL-DATA.
               10 FILLER              PIC X(2) VALUE 'RS'.
               10 FILLER              PIC X VALUE SPACES.
               10 FILLER              PIC X(2) VALUE 'WL'.
               10 FILLER              PIC X VALUE SPACES.
               10 FILLER              PIC X(2) VALUE 'HP'.
               10 FILLER              PIC X VALUE SPACES.
               10 FILLER              PIC X(6) VALUE '  REQS'.
               10 FILLER              PIC X VALUE SPACES.
               10 FILLER              PIC X(3) VALUE 'MIN'.
           05  FILLER                 PIC X(48) VALUE SPACES.