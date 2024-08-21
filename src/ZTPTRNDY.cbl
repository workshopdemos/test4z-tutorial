       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPTRNDY.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * TRENDY was inspired by a simplified performance utility used   *
      * to monitors the health of a group of systems over extended     *
      * periods, up to 30 days, as well as moment-to-moment.           *
      *                                                                *
      * The long-term nature of the former monitoring is why it's      *
      * called "TRENDY". Based on trend analysis, the operator can     *
      * anticipate brewing problems long before they are noted by      *
      * users and comfortably "ignore" performance issues that         *
      * previous trend data has proven to be transitory.               *
      *                                                                *
      * The TRENDY SUT is composed of two main activities:             *
      *                                                                *
      * 1. Aggregating EXEC-LOG entries into a keyed HEALTH-LOG file.  *
      *                                                                *
      *    The entries are grouped by time intervals (by default,      *
      *    those occurring within the same 5 minute period). Each      *
      *    aggregation includes a tally of system response times       *
      *    bucketed into fast/ok/medium/bad ratings, based on          *
      *    typical expectations.                                       *
      *                                                                *
      * 2. Summarizing the aggregated HEALTH-LOG into a HEALTH-TREND.  *
      *    This report summarizes the HEALTH-LOG and applies a rating  *
      *    for key metrics (response, workload, and "happy").          *
      *                                                                *
      *    The report tells the operator at a glance the expected      *
      *    performance over a longer period and flags potential        *
      *    problems (hence the happy rating).                          *
      *                                                                *
      * The EXEC-LOG records are written outside of TRENDY's           *
      * execution by the systems themselves, i.e., they are            *
      * self-reported and aggregated across systems for analysis.      *
      *                                                                *
      * NB: See ZTTRNDY for the associated unit test suite.            *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      ******************************************************************
      * ==> The EXEC-LOG is a sequential file and input only.          *
      *                                                                *
      * It contains system logging records similar with response       *
      * times, workload, and some system self-reported performance     *
      * ratings.                                                       *
      *                                                                *
      * NB: Response scores are reported by the systems                *
      *     themselves, not calculated by TRENDY. These performace     *
      *     assessments are system-specific because one system's "bad" *
      *     is another system's "fast". The CPU time is                *
      *     is included in the EXEC-LOG entry should it be needed      *
      *     for post-trend performance analysis.                       *
      *                                                                *
      * In a production environment, the EXEC-LOG would be deleted     *
      * or archived by the job upon successful completion of TRENDY.   *
      * For test purposes, a short list of records can be defined      *
      * directly using a JCL DD * statement.                           *
      ******************************************************************

           SELECT EXEC-LOG ASSIGN TO EXLOG
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS EXEC-LOG-FS.

      ******************************************************************
      * ==> The HEALTH-LOG is a keyed sequential file (KSDS) that      *
      *     will be open for input and output.                         *
      *                                                                *
      * It contains the summary data for the system logs grouped       *
      * into a short time interval (5 minutes by default).             *
      ******************************************************************

           SELECT HEALTH-LOG ASSIGN TO HLLOG
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS HEALTH-LOG-KEY
           FILE STATUS IS HEALTH-LOG-FS.

      ******************************************************************
      * ==> The HEALTH-TREND is a sequential file and output only.     *
      *                                                                *
      * It contains the summary data for the health logs grouped       *
      * in a longer time interval (60 minutes by default). It also     *
      * contains high-level performance heuristics or "ratings" used   *
      * for monitoring longer-term system health and capacity planning.*
      *                                                                *
      * For test purposes, it can be defined directly by JCL using a   *
      * DD SYSOUT=* statement.                                         *
      ******************************************************************

           SELECT HEALTH-TREND ASSIGN TO HLTREND
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS HEALTH-TREND-FS.

      ******************************************************************
      * ==> The STATS-REPORT is a a sequential file and output only.   *
      *                                                                *
      * It contains a short summary of how system log records          *
      * were processed into the trend log and trend health log.        *
      *                                                                *
      * For test purposes, it can be defined directly by JCL using a   *
      * DD SYSOUT=* statement.                                         *
      ******************************************************************

           SELECT STATS-REPORT ASSIGN TO STREPORT
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS STATS-REPORT-FS.

       DATA DIVISION.
       FILE SECTION.

       FD  EXEC-LOG RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS EXEC-LOG-RECORD.
       01  EXEC-LOG-RECORD PIC X(80).

       FD  HEALTH-LOG RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS HEALTH-LOG-RECORD.
       01  HEALTH-LOG-RECORD.
           02  HEALTH-LOG-KEY  PIC X(16).
           02  HEALTH-LOG-DATA PIC X(64).

       FD  HEALTH-TREND RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS HEALTH-TREND-RECORD.
       01  HEALTH-TREND-RECORD PIC X(80).

       FD  STATS-REPORT RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS STATS-REPORT-RECORD.
       01  STATS-REPORT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.

      ******************************************************************
      * Variables related to the definitions in FILE-CONTROL.          *
      ******************************************************************
       77  EXEC-LOG-FS                 PIC 9(2).
           88 EXEC-LOG-IO-SUCCESS      VALUE 00.
           88 EXEC-LOG-EOF             VALUE 10.
       77  HEALTH-LOG-FS               PIC 9(2).
           88 HEALTH-LOG-IO-SUCCESS    VALUE 00.
           88 HEALTH-LOG-EOF           VALUE 10.
           88 HEALTH-LOG-KEY-NOT-FOUND VALUE 23.
       77  HEALTH-TREND-FS             PIC 9(2).
           88 HEALTH-TREND-IO-SUCCESS  VALUE 00.
           88 HEALTH-TREND-EOF         VALUE 10.
       77  STATS-REPORT-FS             PIC 9(2).
           88 STATS-REPORT-IO-SUCCESS  VALUE 00.

      ******************************************************************
      * Definition of the TRENDY file-related records:                 *
      * 1. ELR-EXEC-LOG-RECORD                                         *
      * 2. HLR-HEALTH-LOG-RECORD                                       *
      * 3. HTR-HEALTH-TREND-RECORD                                     *
      *                                                                *
      * Record for tallying totals for HEALTH-TREND file:              *
      * 4. HTT-HEALTH-TREND-TOTALS                                     *
      *                                                                *
      * Record for tallying totals for STATS-REPORT:                   *
      * 5. RPT-STATS-REPORT-TOTALS                                     *
      ******************************************************************
           COPY ZTPTRREC REPLACING
                   ==:ELR:== BY ==ELR==
                   ==:HLR:== BY ==HLR==
                   ==:HTT:== BY ==HTT==
                   ==:HTR:== BY ==HTR==
                   ==:RPT:== BY ==RPT==.

      ******************************************************************
      * Formatting records for the STATS-REPORT:                       *
      * 6. RPL-HDR1-STATS-REPORT-OUTPUT                                *
      * 7. RPL-HDR2-STATS-REPORT-OUTPUT                                *
      * 8. RPL-STATS-REPORT-OUTPUT                                     *
      *                                                                *
      * Formatting record for the development-only summary report:     *
      * 9.  RPL-HDR-HEALTH-TREND-OUTPUT                                *
      * 10. HTR-HEALTH-TREND-RECORD (same as HEALTH-TREND file)        *
      ******************************************************************
           COPY ZTPTRRPT.

      ******************************************************************
      * Variables for 235-CALCULATE-INTERVALS.                         *
      * Input: DT-DATE-TIME Output: Calculated                         *
      *                                                                *
      * NB: Spaces included between fields for easier DISPLAY debug.   *
      ******************************************************************
       01  DT-DATE-TIME             PIC X(16)
           VALUE '0000-00-00 00:00'.
       01  DATE-TIME-REDEF REDEFINES DT-DATE-TIME.
           05 DT-DATE.
               10 DT-YEAR           PIC 9(4).
               10 FILLER            PIC X.
               10 DT-MONTH          PIC 9(2).
               10 FILLER            PIC X.
               10 DT-DAY            PIC 9(2).
           05 FILLER                PIC X.
           05 DT-TIME.
               10 DT-HOUR           PIC 9(2).
               10 FILLER            PIC X.
               10 DT-MINUTE         PIC 9(2).

       01  HI-HEALTH-INTERVAL.
           05  HS-HEALTH-INTERVAL-START.
               10 HS-DATE.
                   15 HS-YEAR        PIC 9(4).
                   15 FILLER         PIC X VALUE '-'.
                   15 HS-MONTH       PIC 9(2).
                   15 FILLER         PIC X VALUE '-'.
                   15 HS-DAY         PIC 9(2).
               10 FILLER             PIC X VALUE ' '.
               10 HS-TIME.
                   15 HS-HOUR        PIC 9(2).
                   15 FILLER         PIC X VALUE ':'.
                   15 HS-MINUTE      PIC 9(2).
           05  HE-HEALTH-INTERVAL-END.
               10 HE-DATE.
                   15 HE-YEAR        PIC 9(4).
                   15 FILLER         PIC X VALUE '-'.
                   15 HE-MONTH       PIC 9(2).
                   15 FILLER         PIC X VALUE '-'.
                   15 HE-DAY         PIC 9(2).
               10 FILLER             PIC X VALUE ' '.
               10 HE-TIME.
                   15 HE-HOUR        PIC 9(2).
                   15 FILLER         PIC X VALUE ':'.
                   15 HE-MINUTE      PIC 9(2).

       01  TI-TREND-INTERVAL.
           05 TI-TREND-INTERVAL-START.
               10 TS-DATE.
                   15 TS-YEAR        PIC 9(4).
                   15 FILLER         PIC X VALUE '-'.
                   15 TS-MONTH       PIC 9(2).
                   15 FILLER         PIC X VALUE '-'.
                   15 TS-DAY         PIC 9(2).
               10 FILLER             PIC X VALUE ' '.
               10 TS-TIME.
                   15 TS-HOUR        PIC 9(2).
                   15 FILLER         PIC X VALUE ':'.
                   15 TS-MINUTE      PIC 9(2).
           05  TI-TREND-INTERVAL-END.
               10 TE-DATE.
                   15 TE-YEAR        PIC 9(4).
                   15 FILLER         PIC X VALUE '-'.
                   15 TE-MONTH       PIC 9(2).
                   15 FILLER         PIC X VALUE '-'.
                   15 TE-DAY         PIC 9(2).
               10 FILLER             PIC X VALUE ' '.
               10 TE-TIME.
                   15 TE-HOUR        PIC 9(2).
                   15 FILLER         PIC X VALUE ':'.
                   15 TE-MINUTE      PIC 9(2).

      ******************************************************************
      * Configuration parameters.                                      *
      *   - Intervals are in minutes                                   *
      *   - Thresholds are weighted percentage of FAST responses       *
      *                                                                *
      * NB: These are hardcoded defaults in this example, but for a    *
      *     real implementation, they would be loaded externally or    *
      *     dynamically determined based on historical data.           *
      ******************************************************************
       01  CONFIGURATION-PARAMETERS-TRNDY.
           05 CFG-RECORDING-INTERVALS.
               10 CFG-HEALTH-LOG-INTERVAL    PIC 9(3) VALUE 5.
               10 CFG-HEALTH-TREND-INTERVAL  PIC 9(3) VALUE 60.
           05 CFG-VALID-RANGES.
               10 CFG-VALID-CPU-TIME-MIN     PIC 9(4) VALUE 4.
               10 CFG-VALID-CPU-TIME-MAX     PIC 9(4) VALUE 3600.

      ******************************************************************
      * Misc. flags                                                    *
      ******************************************************************
       77  EXEC-LOG-IO-FLAG               PIC 9.
           88 EXEC-LOG-DONE               VALUE 1.
           88 EXEC-LOG-HAS-MORE           VALUE 0.
       77  VALID-EXEC-LOG-IO-FLAG         PIC 9.
           88 VALID-EXEC-LOG-FOUND        VALUE 1.
           88 VALID-EXEC-LOG-NONE-YET     VALUE 0.
       77  HEALTH-LOG-IO-FLAG             PIC 9.
           88 HEALTH-LOG-DONE             VALUE 1.
           88 HEALTH-LOG-HAS-MORE         VALUE 0.
       77  HEALTH-LOG-RECORD-FLAG         PIC 9.
           88 HEALTH-LOG-RECORD-FOUND     VALUE 1.
           88 HEALTH-LOG-RECORD-NOT-FOUND VALUE 0.
       77  HEALTH-TREND-IO-FLAG           PIC 9.
           88 HEALTH-TREND-DONE           VALUE 1.
           88 HEALTH-TREND-HAS-MORE       VALUE 0.
       77  CTRL-HEALTH-INTERVAL-FLAG      PIC 9.
           88 START-NEW-HEALTH-INTERVAL   VALUE 1.
           88 CONTINUE-HEALTH-INTERVAL    VALUE 0.

      ******************************************************************
      * Temporary variables and indices.                               *
      ******************************************************************
       77  WS-INDEX                     PIC 9(3).
       77  WS-HEALTH-LOG-COUNT          PIC 9(6).
       77  WS-INTEGER-TEMP              PIC 9(6).
       77  WS-INTEGER-NUM               PIC 9(6).
       77  WS-INTEGER-REM               PIC 9(6).
       77  WS-NEXT-TREND-INTERVAL-START PIC X(16)
               VALUE '0000-00-00 00:00'.
           88 WS-NO-TREND-INTERVAL
               VALUE '0000-00-00 00:00'.
       77  WS-NEXT-TREND-INTERVAL-END   PIC X(16)
               VALUE '0000-00-00 00:00'.

      ******************************************************************
      * Temporary variables for DATE-TO-INTEGER and INTEGER-TO-DATE    *
      * calculations. See 235-CALCULATE-INTERVALS for details.         *
      ******************************************************************
       01  DAI-DATE-AS-INTEGER    PIC 9(8).
       01  DAI-DATE-AS-INTEGER-REDEF REDEFINES DAI-DATE-AS-INTEGER.
           05 DAI-YEAR            PIC 9(4).
           05 DAI-MONTH           PIC 9(2).
           05 DAI-DAY             PIC 9(2).
       77  WS-INTEGER-OF-DATE     PIC 9(7).
       77  WS-DATE-MINUTES        PIC 9(9).
       77  WS-DATE-MINUTES-START  PIC 9(9).
       77  WS-DATE-MINUTES-END    PIC 9(9).

       PROCEDURE DIVISION.

      ******************************************************************
      * These records may be in the LINKAGE SECTION and have FILLER    *
      * fields, so they can't have VALUE statements. Initialize them   *
      * so their pretty prints are better.                             *
      ******************************************************************

           MOVE SPACES TO HLR-HEALTH-LOG-RECORD
           INITIALIZE HLR-HEALTH-LOG-RECORD REPLACING NUMERIC BY 0
           MOVE '0000-00-00 00:00' TO HLR-INTERVAL

           MOVE SPACES TO HTR-HEALTH-TREND-RECORD
           INITIALIZE HTR-HEALTH-TREND-RECORD REPLACING NUMERIC BY 0
           MOVE '0000-00-00 00:00' TO HTR-INTERVAL

           MOVE SPACES TO RPT-STATS-REPORT-TOTALS
           INITIALIZE RPT-STATS-REPORT-TOTALS REPLACING NUMERIC BY 0
           MOVE '0000-00-00 00:00' TO RPT-START-INTERVAL
           MOVE '0000-00-00 00:00' TO RPT-END-INTERVAL

      ******************************************************************
      * Process the EXEC-LOG records, summarizing them in the          *
      * HEALTH-LOG for short intervals (default 5 minutes). These      *
      * summarizes are "rated" for an overall performance trend        *
      * and written to HEALTH-TREND based on an a longer interval      *
      * (default 60 minutes). The health trend include response,       *
      * workload, and "happy" ratings on a scale of 1-10.              *
      *                                                                *
      * NB: EXEC-LOG has many entries/minute.                          *
      *     HEALTH-LOG has one entry per CFG-HEALTH-LOG-INTERVAL.      *
      *     HEALTH-TREND has one entry per CFG-HEALTH-TREND-INTERVAL.  *
      ******************************************************************

           PERFORM 100-START-PROGRAM
           PERFORM 110-OPEN-TRENDY-FILES
           PERFORM 200-PROCESS-EXEC-LOGS
           PERFORM 300-WRITE-STATS-REPORT
           PERFORM 400-CLOSE-TRENDY-FILES
           PERFORM 500-END-PROGRAM

           STOP RUN.

      ******************************************************************
      * Consolidate the EXEC-LOG records into HEALTH-LOG, grouping     *
      * the records into "health check" intervals (default 5 minutes). *
      *                                                                *
      * Input:    EXEC-LOG file (ELR-EXEC-LOG-RECORD)                  *
      * Output 1: HEALTH-LOG processing by                             *
      *           230-CREATE-HEALTH-LOG-REC                            *
      * Output 2: TREND-LOG processing by                              *
      *           220-PROCESS-HEALTH-TREND-REC                         *
      ******************************************************************
       200-PROCESS-EXEC-LOGS.

           SET EXEC-LOG-HAS-MORE TO TRUE
           PERFORM UNTIL EXEC-LOG-DONE
               READ EXEC-LOG
                   AT END
                       SET EXEC-LOG-DONE TO TRUE
                   NOT AT END
                       MOVE EXEC-LOG-RECORD TO ELR-EXEC-LOG-RECORD
                       PERFORM 210-PROCESS-EXEC-LOG-REC
               END-READ

               IF NOT EXEC-LOG-IO-SUCCESS AND NOT EXEC-LOG-EOF
                   DISPLAY '1: Error reading system log: '
                       EXEC-LOG-FS
                   PERFORM 510-PROGRAM-ERROR-CONTINUE
               END-IF
           END-PERFORM

           EXIT.

      ******************************************************************
      * Update the health log based on ELR-EXEC-LOG-RECORD.            *
      *                                                                *
      * Input:  ELR-EXEC-LOG-RECORD                                    *
      * Output: EXEC-LOG file                                          *
      *                                                                *
      * NB: The system log scores are self-scored, so apply some       *
      *     minimal checking to accept the record as valid (e.g., CPU  *
      *     time greater than CFG-VALID-CPU-TIME-MAX is likely         *
      *     an error condition and should not be included).            *
      ******************************************************************
       210-PROCESS-EXEC-LOG-REC.

           IF (ELR-RETURN-CODE = 0 OR ELR-RETURN-CODE = 4 OR
                   ELR-RETURN-CODE = 8 OR ELR-RETURN-CODE = 12) AND
                   ELR-CPU-TIME >= CFG-VALID-CPU-TIME-MIN AND
                   ELR-CPU-TIME <= CFG-VALID-CPU-TIME-MAX
               ADD 1 TO RPT-VALID-EXEC-LOGS

               IF ELR-TYPE-ERROR
                   ADD 1 TO RPT-ERROR-EXEC-LOGS
               END-IF

               PERFORM 230-CREATE-HEALTH-LOG-REC
           ELSE
               PERFORM 530-REPORT-INVALID-EXEC-LOG
           END-IF

           EXIT.

      ******************************************************************
      * Update the total health trend record using the current         *
      * trend record as part of the health trend rollup.               *
      *                                                                *
      * Input:  HLR-HEALTH-LOG-RECORD                                  *
      * Output: HTT-HEALTH-TREND-TOTALS                                *
      ******************************************************************
       220-PROCESS-HEALTH-TREND-REC.

           ADD HLR-REQUESTS TO HTT-REQUESTS-TOTAL
           ADD HLR-FAST TO HTT-FAST-TOTAL
           ADD HLR-OK TO HTT-OK-TOTAL
           ADD HLR-SLOW TO HTT-SLOW-TOTAL
           ADD HLR-BAD TO HTT-BAD-TOTAL
           ADD HLR-ERROR TO HTT-ERROR-TOTAL

           IF HLR-MAX-CPU-TIME > HTT-MAX-CPU-TIME
               MOVE HLR-MAX-CPU-TIME TO HTT-MAX-CPU-TIME
           END-IF

           ADD HLR-CPU-TIME-TOTAL TO HTT-CPU-TIME-TOTAL
           ADD HLR-WORKLOAD-TOTAL TO HTT-WORKLOAD-TOTAL

           EXIT.

      ******************************************************************
      * Add the current system log ELR-EXEC-LOG-RECORD to the          *
      * HEALTH-LOG at its corresponding interval (i.e., the log        *
      * record timestamp rounded down to CFG-HEALTH-LOG-INTERVAL).     *
      *                                                                *
      * If there's an existing health record for that interval,        *
      * update it with the measures of the incoming log entry.         *
      *                                                                *
      * Input:  ELR-EXEC-LOG-RECORD                                    *
      * Output: HEALTH-LOG file via HLR-HEALTH-LOG-RECORD              *
      ******************************************************************
       230-CREATE-HEALTH-LOG-REC.

      *-----------------------------------------------------------------
      * Initialize health log working record including dashes/colons.
      *-----------------------------------------------------------------
           MOVE SPACES TO HLR-HEALTH-LOG-RECORD
           INITIALIZE HLR-HEALTH-LOG-RECORD REPLACING NUMERIC BY 0
           MOVE '0000-00-00 00:00' TO HLR-INTERVAL

      *-----------------------------------------------------------------
      * Round the log time to the nearest health interval.
      *-----------------------------------------------------------------
           MOVE ELR-DATE-TIME TO DT-DATE-TIME
           PERFORM 235-CALCULATE-INTERVALS

      *-----------------------------------------------------------------
      * If no health trend records have been written yet,
      * start the health trend interval based on the date/time
      * of the first log record.
      *-----------------------------------------------------------------
           IF WS-NO-TREND-INTERVAL
               MOVE TI-TREND-INTERVAL-START TO
                   WS-NEXT-TREND-INTERVAL-START
               MOVE TI-TREND-INTERVAL-END TO
                   WS-NEXT-TREND-INTERVAL-END
           END-IF

      *-----------------------------------------------------------------
      * Retrieve existing record and then update OR start a new one.
      *-----------------------------------------------------------------
           MOVE HS-HEALTH-INTERVAL-START TO HEALTH-LOG-KEY
           MOVE HS-HEALTH-INTERVAL-START TO HLR-KEY
           READ HEALTH-LOG

           IF HEALTH-LOG-IO-SUCCESS
      *-----------------------------------------------------------------
      *        Record was found, so it's an update.
      *-----------------------------------------------------------------

               SET HEALTH-LOG-RECORD-FOUND TO TRUE
               SET CONTINUE-HEALTH-INTERVAL TO TRUE
               MOVE HEALTH-LOG-RECORD TO HLR-HEALTH-LOG-RECORD
           ELSE
               IF HEALTH-LOG-KEY-NOT-FOUND
      *-----------------------------------------------------------------
      *            Record was not found, so use (new) initialized one.
      *-----------------------------------------------------------------

                   SET HEALTH-LOG-RECORD-NOT-FOUND TO TRUE
                   SET START-NEW-HEALTH-INTERVAL TO TRUE
               ELSE
                   DISPLAY '2: Error reading health log: ' HEALTH-LOG-FS
                   PERFORM 510-PROGRAM-ERROR-CONTINUE
               END-IF
           END-IF

      *-----------------------------------------------------------------
      * Update the health log summary of log entries.
      *-----------------------------------------------------------------
           ADD 1 TO HLR-REQUESTS
           ADD ELR-CPU-TIME TO HLR-CPU-TIME-TOTAL

      *-----------------------------------------------------------------
      * Keep track of the slowest time, within the bounds of reasonable.
      *-----------------------------------------------------------------
           IF ELR-CPU-TIME >= CFG-VALID-CPU-TIME-MIN AND
                   ELR-CPU-TIME <= CFG-VALID-CPU-TIME-MAX AND
                   ELR-CPU-TIME > HLR-MAX-CPU-TIME
               MOVE ELR-CPU-TIME TO HLR-MAX-CPU-TIME
           END-IF

           ADD ELR-WORKLOAD TO HLR-WORKLOAD-TOTAL

      *-----------------------------------------------------------------
      * NB: Logs include the actual CPU time. If the CPU time
      *     is well outside the norm, the record times will be flagged
      *     as invalid and ignored per the range defined by
      *     CFG-VALID-CPU-TIME-MIN / MAX to avoid wildly off CPU
      *     times skewing the other metrics.
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN ELR-RESPONSE-FAST
                   ADD 1 TO HLR-FAST
               WHEN ELR-RESPONSE-OK
                   ADD 1 TO HLR-OK
               WHEN ELR-RESPONSE-SLOW
                   ADD 1 TO HLR-SLOW
               WHEN ELR-RESPONSE-BAD
                   ADD 1 TO HLR-BAD
           END-EVALUATE

           IF ELR-TYPE-ERROR
               ADD 1 TO HLR-ERROR
           END-IF

      *-----------------------------------------------------------------
      * Write a new record, or rewrite it if it's already there.
      *-----------------------------------------------------------------
           MOVE HLR-HEALTH-LOG-RECORD TO HEALTH-LOG-RECORD

           IF HEALTH-LOG-RECORD-FOUND
               ADD 1 TO RPT-HEALTH-LOG-UPDATES
               REWRITE HEALTH-LOG-RECORD
           ELSE
               WRITE HEALTH-LOG-RECORD
               ADD 1 TO RPT-HEALTH-LOG-ADDITIONS
           END-IF

           IF NOT HEALTH-LOG-IO-SUCCESS
               DISPLAY '3: Error writing to health log: ' HEALTH-LOG-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

      *-----------------------------------------------------------------
      * Keep track of the first and last interval recorded for the
      * log entries. They're part of the end of processing report.
      *-----------------------------------------------------------------
           MOVE HE-HEALTH-INTERVAL-END TO RPT-END-INTERVAL
           IF VALID-EXEC-LOG-NONE-YET
               SET VALID-EXEC-LOG-FOUND TO TRUE
               MOVE HS-HEALTH-INTERVAL-START TO RPT-START-INTERVAL
           END-IF

      *-----------------------------------------------------------------
      * The logs are stored in timestamp order, so if we
      * didn't find a matching interval in the HEALTH-LOG to update,
      * we're starting a new interval, so create a new trend record
      * before exiting.
      *-----------------------------------------------------------------
           IF START-NEW-HEALTH-INTERVAL AND
                   (WS-NO-TREND-INTERVAL OR 
                   HS-HEALTH-INTERVAL-START >= 
                   WS-NEXT-TREND-INTERVAL-END)
               PERFORM 240-CREATE-HEALTH-TREND-REC
           END-IF

           EXIT.

      ******************************************************************
      * Create a summary trend record for the given interval.          *
      *                                                                *
      * Input:  WS-NEXT-TREND-INTERVAL-START / END                     *
      * Output: HTR-HEALTH-TREND-RECORD and HEALTH-TREND file          *
      ******************************************************************
       240-CREATE-HEALTH-TREND-REC.

           MOVE SPACES TO HTR-HEALTH-TREND-RECORD
           INITIALIZE HTR-HEALTH-TREND-RECORD REPLACING NUMERIC BY 0
           MOVE '0000-00-00 00:00' TO HTR-INTERVAL
           INITIALIZE HTT-HEALTH-TREND-TOTALS

           MOVE WS-NEXT-TREND-INTERVAL-START TO HTR-KEY
           MOVE CFG-HEALTH-TREND-INTERVAL TO HTR-REPORT-DURATION

      *-----------------------------------------------------------------
      * If the health trend file was empty, there's no matches,
      * so we'll quietly head for the exit.
      *-----------------------------------------------------------------
           MOVE HTR-KEY TO HEALTH-LOG-KEY
           START HEALTH-LOG KEY IS GREATER OR EQUAL TO HEALTH-LOG-KEY
               INVALID KEY
                   DISPLAY 'No matching keys for ' HEALTH-LOG-KEY
                       ' status code: ' HEALTH-LOG-FS
                   EXIT PARAGRAPH
           END-START

           IF NOT HEALTH-LOG-IO-SUCCESS
               DISPLAY '4: Error START KEY of health log: '
                   HEALTH-LOG-FS
               PERFORM 510-PROGRAM-ERROR-CONTINUE
           END-IF

      *-----------------------------------------------------------------
      * Total up the health log entries into a health trend record...
      *-----------------------------------------------------------------
           SET HEALTH-LOG-HAS-MORE TO TRUE
           MOVE 0 TO WS-HEALTH-LOG-COUNT

           PERFORM UNTIL HEALTH-LOG-DONE
               READ HEALTH-LOG NEXT RECORD
                   AT END
                       SET HEALTH-LOG-DONE TO TRUE
                   NOT AT END
                       IF HEALTH-LOG-KEY < WS-NEXT-TREND-INTERVAL-END
                           ADD 1 TO WS-HEALTH-LOG-COUNT
                           MOVE HEALTH-LOG-RECORD
                               TO HLR-HEALTH-LOG-RECORD

                           PERFORM 220-PROCESS-HEALTH-TREND-REC
                       ELSE
                           SET HEALTH-LOG-DONE TO TRUE
                       END-IF
               END-READ

               IF NOT HEALTH-LOG-IO-SUCCESS AND NOT HEALTH-LOG-EOF
                   DISPLAY '5: Error reading health log: '
                       HEALTH-LOG-FS
                   PERFORM 510-PROGRAM-ERROR-CONTINUE
               END-IF
           END-PERFORM

      *-----------------------------------------------------------------
      * For the calculated health trend reporting period that
      * ended at the start of the new health log period, sum up
      * the health log metrics and rate them on a scale of 1-10
      * for the overall period (default of 60 minutes).
      *
      * NB: If there's a long gap between log entries,
      *     there will be no previous health trend records. This is
      *     normal during development but would be unusual during
      *     production where the multiple log entries are
      *     created per minute. Quietly ignore the "miss".
      *-----------------------------------------------------------------
           IF WS-HEALTH-LOG-COUNT > 0
               PERFORM 250-CALCULATE-HEALTH-RATINGS
               PERFORM 260-WRITE-HEALTH-TREND-REC
           END-IF

      *-----------------------------------------------------------------
      * Ready to move onto the next health trend interval; use the
      * end of the last interval as the start of the next one. Convert
      * the date/time into minutes to avoid rollover problems
      * when calculating the next interval.
      *-----------------------------------------------------------------
           MOVE WS-NEXT-TREND-INTERVAL-END
               TO WS-NEXT-TREND-INTERVAL-START
           MOVE WS-NEXT-TREND-INTERVAL-END TO DT-DATE-TIME
           PERFORM CONVERT-DATE-TO-MINUTES
           ADD CFG-HEALTH-TREND-INTERVAL TO WS-DATE-MINUTES
           PERFORM CONVERT-MINUTES-TO-DATE
           MOVE DT-DATE-TIME(1:16) TO WS-NEXT-TREND-INTERVAL-END

           EXIT.


      ******************************************************************
      * Health ratings are based on the health trend records within    *
      * a given interval. See ZTPTRNDC for more details.               *
      *                                                                *
      * Input:  HTT-HEALTH-TREND-TOTALS                                *
      * Output: HTR-HEALTH-TREND-RECORD                                *
      ******************************************************************
       250-CALCULATE-HEALTH-RATINGS.

           CALL 'ZTPTRNDC'
               USING HTT-HEALTH-TREND-TOTALS, HTR-HEALTH-TREND-RECORD

           EXIT.

      ******************************************************************
      * All the health logs are processed for this interval, so        *
      * record HTR-HEALTH-TREND-RECORD in the health trend file.       *
      *                                                                *
      * Input:  HTR-HEALTH-TREND-RECORD                                *
      * Output: HEALTH-TREND file                                      *
      ******************************************************************
       260-WRITE-HEALTH-TREND-REC.

           MOVE HTR-HEALTH-TREND-RECORD TO HEALTH-TREND-RECORD
           WRITE HEALTH-TREND-RECORD
           IF NOT HEALTH-TREND-IO-SUCCESS
               DISPLAY '6: Error writing health trend: '
                   HEALTH-TREND-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

           ADD 1 TO RPT-HEALTH-TREND-COUNT

      *-----------------------------------------------------------------
      * For a quick sanity check, write it to SYSOUT, too.
      *-----------------------------------------------------------------
           IF RPT-HEALTH-TREND-COUNT = 1
               DISPLAY RPL-HDR-HEALTH-TREND-OUTPUT
           END-IF

           DISPLAY HTR-HEALTH-TREND-RECORD

           EXIT.

      ******************************************************************
      * Short summary of how many records were added/updated.          *
      *                                                                *
      * Input:  RPT-STATS-REPORT-TOTALS                                *
      * Output: STATS-REPORT file                                      *
      ******************************************************************
       300-WRITE-STATS-REPORT.

      *-----------------------------------------------------------------
      * If no records were processed, there's something wrong...
      *-----------------------------------------------------------------
           IF RPT-VALID-EXEC-LOGS + RPT-INVALID-EXEC-LOGS = 0
               MOVE 'No exec records processed'
                   TO STATS-REPORT-RECORD
               PERFORM 510-PROGRAM-ERROR-CONTINUE

               DISPLAY STATS-REPORT-RECORD
               WRITE STATS-REPORT-RECORD

               IF NOT STATS-REPORT-IO-SUCCESS
                   DISPLAY '7: Error writing stats report: '
                       STATS-REPORT-FS
                   PERFORM 520-PROGRAM-ERROR
                   PERFORM 500-END-PROGRAM
                   STOP RUN
               END-IF

               EXIT PARAGRAPH
           END-IF

      *-----------------------------------------------------------------
      * Initialize the first and last interval.
      *-----------------------------------------------------------------
           MOVE RPT-START-INTERVAL TO RPL-START-INTERVAL
           MOVE RPT-END-INTERVAL TO RPL-END-INTERVAL

      *-----------------------------------------------------------------
      * ...and the quick summary.
      *-----------------------------------------------------------------
           MOVE RPT-VALID-EXEC-LOGS TO RPL-VALID-EXEC-LOGS
           MOVE RPT-ERROR-EXEC-LOGS TO RPL-ERROR-EXEC-LOGS
           MOVE RPT-INVALID-EXEC-LOGS TO RPL-INVALID-EXEC-LOGS
           MOVE RPT-HEALTH-LOG-ADDITIONS TO RPL-HEALTH-LOG-ADDITIONS
           MOVE RPT-HEALTH-LOG-UPDATES TO RPL-HEALTH-LOG-UPDATES
           MOVE RPT-HEALTH-TREND-COUNT TO RPL-HEALTH-TREND-COUNT

      *-----------------------------------------------------------------
      * For convenient confirmation of results.
      *-----------------------------------------------------------------
           DISPLAY RPL-HDR1-STATS-REPORT-OUTPUT
           DISPLAY RPL-HDR2-STATS-REPORT-OUTPUT
           DISPLAY RPL-STATS-REPORT-OUTPUT

      *-----------------------------------------------------------------
      * Write out the header and results.
      *-----------------------------------------------------------------
           WRITE STATS-REPORT-RECORD FROM RPL-HDR1-STATS-REPORT-OUTPUT
           WRITE STATS-REPORT-RECORD FROM RPL-HDR2-STATS-REPORT-OUTPUT
           WRITE STATS-REPORT-RECORD FROM RPL-STATS-REPORT-OUTPUT

           IF NOT STATS-REPORT-IO-SUCCESS
               DISPLAY '8: Error writing stats report: ' STATS-REPORT-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

           EXIT.

      ******************************************************************
      * Converts a date YYYY-MM-DD HH:MM to minutes for easier         *
      * math operations without risk of date/time overflow.            *
      *                                                                *
      * Input:  DT-DATE                                                *
      * Output: WS-DATE-MINUTES                                        *
      ******************************************************************
       CONVERT-DATE-TO-MINUTES.

      *-----------------------------------------------------------------
      * Sanity check the incoming date; replace with an obviously
      * out-of-range one so it's not ignored.
      *-----------------------------------------------------------------
           IF DT-YEAR < 1601 OR DT-MONTH < 01 OR DT-MONTH > 12 OR
                   DT-DAY < 01 OR DT-DAY > 31 OR 
                   DT-HOUR > 23 OR DT-MINUTE > 59 OR
                   ((DT-MONTH = 4 OR DT-MONTH = 6 OR DT-MONTH = 9 OR
                   DT-MONTH = 11) AND DT-DAY > 30) OR
                   (DT-MONTH = 2 AND DT-DAY > 29)
               DISPLAY '9: Error validating date/time ' DT-DATE-TIME
               MOVE '1601-01-01 01:01:01' TO DT-DATE-TIME
           END-IF

      *-----------------------------------------------------------------
      * Convert timestamp to minutes for easier interval calculation.
      * INTEGER-OF-DATE returns days since December 31, 1600.
      * Convert DT-DATE to minutes since then (1440 minutes/day).
      *-----------------------------------------------------------------
           MOVE DT-YEAR TO DAI-YEAR
           MOVE DT-MONTH TO DAI-MONTH
           MOVE DT-DAY TO DAI-DAY
           COMPUTE WS-DATE-MINUTES =
               FUNCTION INTEGER-OF-DATE(DAI-DATE-AS-INTEGER)
           COMPUTE WS-DATE-MINUTES =
               (WS-DATE-MINUTES * 1440) +
               (DT-HOUR * 60) + DT-MINUTE

           EXIT.

      ******************************************************************
      * Converts minutes to a date YYYY-MM-DD HH:MM as part of easier  *
      * math operations without risk of date/time overflow.            *
      *                                                                *
      * Input:  WS-DATE-MINUTES                                        *
      * Output: DT-DATE                                                *
      ******************************************************************
       CONVERT-MINUTES-TO-DATE.

      *-----------------------------------------------------------------
      * Convert the minutes back to YYYY-MM-DD HH:MM.
      *-----------------------------------------------------------------
           DIVIDE WS-DATE-MINUTES BY 1440
               GIVING WS-INTEGER-OF-DATE REMAINDER WS-INTEGER-REM
           DIVIDE WS-INTEGER-REM BY 60
               GIVING DT-HOUR REMAINDER DT-MINUTE

           COMPUTE DAI-DATE-AS-INTEGER =
               FUNCTION DATE-OF-INTEGER(WS-INTEGER-OF-DATE)
           MOVE DAI-YEAR TO DT-YEAR
           MOVE DAI-MONTH TO DT-MONTH
           MOVE DAI-DAY TO DT-DAY

           EXIT.

      ******************************************************************
      * 235-CALCULATE-INTERVALS accepts DT-DATE and sets four          *
      * variables:                                                     *
      *                                                                *
      *   HS-HEALTH-INTERVAL-START                                     *
      *   HE-HEALTH-INTERVAL-END                                       *
      *   TS-TREND-INTERVAL-START                                      *
      *   TS-TREND-INTERVAL-END                                        *
      *                                                                *
      * The intervals for the above are determined by two variables:   *
      *                                                                *
      *   CFG-HEALTH-LOG-INTERVAL                                      *
      *   CFG-HEALTH-TREND-INTERVAL                                    *
      *                                                                *
      * The start is defined as the "rounded down" time of DT-DATE;    *
      * the end is defined as that time plus the interval. For         *
      * example, for a 5 minute interval, 12:53 -> 12:50 to 12:55.     *
      *                                                                *
      * NB: This code avoids date and time overflows like              *
      *     December 31st and times near midnight by converting        *
      *     DT-DATE to a minutes-since timestamp, adding the interval  *
      *     duration, and then converting back to YYYY-MM-DD HH:MM     *
      *     with the functions INTEGER-OF-DATE and DATE-OF-INTEGER.    *
      ******************************************************************
       235-CALCULATE-INTERVALS.

      *-----------------------------------------------------------------
      * Convert input DT-DATE into WS-DATE-MINUTES.
      *-----------------------------------------------------------------
           PERFORM CONVERT-DATE-TO-MINUTES

      *-----------------------------------------------------------------
      * Round down to the start of the health check interval in minutes.
      *-----------------------------------------------------------------
           DIVIDE WS-DATE-MINUTES BY CFG-HEALTH-LOG-INTERVAL
               GIVING WS-INTEGER-TEMP REMAINDER WS-INTEGER-REM
           COMPUTE WS-DATE-MINUTES-START =
               WS-DATE-MINUTES - WS-INTEGER-REM

      *-----------------------------------------------------------------
      * Calculate the end of the health check interval.
      *-----------------------------------------------------------------
           ADD CFG-HEALTH-LOG-INTERVAL TO WS-DATE-MINUTES-START
               GIVING WS-DATE-MINUTES-END

      *-----------------------------------------------------------------
      * Convert the interval start minutes back to YYYY-MM-DD HH:MM.
      *-----------------------------------------------------------------
           MOVE WS-DATE-MINUTES-START TO WS-DATE-MINUTES
           PERFORM CONVERT-MINUTES-TO-DATE
           MOVE DT-DATE TO HS-DATE
           MOVE DT-TIME TO HS-TIME

      *-----------------------------------------------------------------
      * Convert the interval end minutes back to YYYY-MM-DD HH:MM.
      *-----------------------------------------------------------------
           MOVE WS-DATE-MINUTES-END TO WS-DATE-MINUTES
           PERFORM CONVERT-MINUTES-TO-DATE
           MOVE DT-DATE TO HE-DATE
           MOVE DT-TIME TO HE-TIME

      *-----------------------------------------------------------------
      * The trend end time is the start of the health check hour
      * (e.g., if the record was logged at 04:22, its health LOG
      * interval is 04:20 to 04:25 and its health TREND interval is
      * 04:00 to 05:00, assuming intervals of 5 and 60 minutes,
      * respectively).
      *-----------------------------------------------------------------
           COMPUTE WS-DATE-MINUTES-START = 
               (WS-DATE-MINUTES-START / 60) * 60
           COMPUTE WS-DATE-MINUTES-END = WS-DATE-MINUTES-START +
               CFG-HEALTH-TREND-INTERVAL

      *-----------------------------------------------------------------
      * Convert the trend start minutes back to YYYY-MM-DD HH:MM.
      *-----------------------------------------------------------------
           MOVE WS-DATE-MINUTES-START TO WS-DATE-MINUTES
           PERFORM CONVERT-MINUTES-TO-DATE
           MOVE DT-DATE TO TS-DATE
           MOVE DT-TIME TO TS-TIME               

      *-----------------------------------------------------------------
      * Convert the trend end minutes back to YYYY-MM-DD HH:MM.
      *-----------------------------------------------------------------
           MOVE WS-DATE-MINUTES-END TO WS-DATE-MINUTES
           PERFORM CONVERT-MINUTES-TO-DATE
           MOVE DT-DATE TO TE-DATE
           MOVE DT-TIME TO TE-TIME             

           EXIT.

      ******************************************************************
      * Let's start out thinking happy thoughts.                       *
      ******************************************************************
       100-START-PROGRAM.

           DISPLAY '==> TRENDY start'

           MOVE 0 TO RETURN-CODE
        
           EXIT.           

      ******************************************************************
      * Open EXEC-LOG, HEALTH-LOG, HEALTH-TREND, and STATS-REPORT.     *
      ******************************************************************
       110-OPEN-TRENDY-FILES.
           OPEN INPUT EXEC-LOG
           IF NOT EXEC-LOG-IO-SUCCESS
               DISPLAY '10: Error opening exec log: ' EXEC-LOG-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

      *-----------------------------------------------------------------
      * Open the HEALTH-LOG for output and close it; this avoids
      * an open error for I/O if the file is empty.
      *-----------------------------------------------------------------
           OPEN OUTPUT HEALTH-LOG
           IF NOT HEALTH-LOG-IO-SUCCESS
               DISPLAY '11: Error opening health log: ' HEALTH-LOG-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

           CLOSE HEALTH-LOG
           IF NOT HEALTH-LOG-IO-SUCCESS
               DISPLAY '12: Error closing health log: ' HEALTH-LOG-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

           OPEN I-O HEALTH-LOG
           IF NOT HEALTH-LOG-IO-SUCCESS
               DISPLAY '13: Error opening health log: ' HEALTH-LOG-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

      *-----------------------------------------------------------------
      * Now open the other files.
      *-----------------------------------------------------------------
           OPEN OUTPUT HEALTH-TREND
           IF NOT HEALTH-TREND-IO-SUCCESS
               DISPLAY '14: Error opening health trend: '
                   HEALTH-TREND-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

           OPEN OUTPUT STATS-REPORT
           IF NOT STATS-REPORT-IO-SUCCESS
               DISPLAY '15: Error opening stats report: '
                   STATS-REPORT-FS
               PERFORM 520-PROGRAM-ERROR
               PERFORM 500-END-PROGRAM
               STOP RUN
           END-IF

           EXIT.

      ******************************************************************
      * Close EXEC-LOG, HEALTH-LOG, HEALTH-TREND, and STATS-REPORT.    *
      ******************************************************************
       400-CLOSE-TRENDY-FILES.

           CLOSE EXEC-LOG
           IF NOT EXEC-LOG-IO-SUCCESS
               DISPLAY '16: Error closing exec log: ' EXEC-LOG-FS
               PERFORM 510-PROGRAM-ERROR-CONTINUE
           END-IF

           CLOSE HEALTH-LOG
           IF NOT HEALTH-LOG-IO-SUCCESS
               DISPLAY '17: Error closing health log: ' HEALTH-LOG-FS
               PERFORM 510-PROGRAM-ERROR-CONTINUE
           END-IF

           CLOSE HEALTH-TREND
           IF NOT HEALTH-TREND-IO-SUCCESS
               DISPLAY '18: Error closing health trend: '
                   HEALTH-TREND-FS
               PERFORM 510-PROGRAM-ERROR-CONTINUE
           END-IF

           CLOSE STATS-REPORT
           IF NOT STATS-REPORT-IO-SUCCESS
               DISPLAY '19: Error closing status report: '
                   STATS-REPORT-FS
               PERFORM 510-PROGRAM-ERROR-CONTINUE
           END-IF

           EXIT.

      ******************************************************************
      * Various return codes based on severity.                        *
      ******************************************************************
       500-END-PROGRAM.

           DISPLAY '==> TRENDY end with RETURN-CODE=' RETURN-CODE
          
           EXIT.

       510-PROGRAM-ERROR-CONTINUE.

           MOVE 4 TO RETURN-CODE
           DISPLAY '==> TRENDY continuing with RETURN-CODE='
               RETURN-CODE

           EXIT.

       520-PROGRAM-ERROR.

           MOVE 8 TO RETURN-CODE
           DISPLAY '==> TRENDY terminating with RETURN-CODE='
               RETURN-CODE
        
           EXIT.

      ******************************************************************
      * Since this is only an example, simply tally the invalid        *
      * records and add a message to SYSOUT. In a production version,  *
      * an invalid log record merits investigation; it                 *
      * should (a) be logged in a separate file and (b) flagged for    *
      * post-processing analysis to determine the cause.               *
      ******************************************************************
       530-REPORT-INVALID-EXEC-LOG.

           ADD 1 TO RPT-INVALID-EXEC-LOGS

           DISPLAY '==> TRENDY invalid exec log record '
               RPT-INVALID-EXEC-LOGS

           EXIT.
       