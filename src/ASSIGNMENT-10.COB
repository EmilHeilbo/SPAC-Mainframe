       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-10.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANK-FILE ASSIGN TO "data/BANKS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO "data/TRANSACTIONS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STATEMENTS-FILE ASSIGN TO "docs/BANK-STATEMENTS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  BANK-FILE.
       01  BANK-RECORD              PIC X(130).
       FD  TRANSACTION-FILE.
       01  TRANS-RECORD             PIC X(200).
       FD  STATEMENTS-FILE.
       01  STMNT-RECORD             PIC X(500)  VALUE SPACES.

       WORKING-STORAGE SECTION.
       01  BANKS.
           05  BANK-COUNT           PIC 999    VALUE ZEROES.
           05  BANK-INFO OCCURS 1 TO 999 TIMES
                   DEPENDING ON BANK-COUNT
                   INDEXED BY BANK-INDEX.
               COPY BANK.

       01  TRANSACTIONS.
           05  CPR-COUNT            PIC 9(5)    VALUE ZEROES.
           05  TRANS-GROUP OCCURS 1 TO 10000 TIMES
                   DEPENDING ON CPR-COUNT
                   INDEXED BY TRANS-INDEX.
               06  TRANS-KEY        PIC X(15).
               06  TRANS-COUNT      PIC 9(6)    VALUE ZEROES.
               06  TRANS-DETAILS OCCURS 10 TIMES.
                   COPY TRANSACT.

       77  USD-CONVERSION-RATE      PIC 9V99    VALUE 6.80.
       77  EUR-CONVERSION-RATE      PIC 9V99    VALUE 7.50.
       77  RECIPIENT-INDENT         PIC X(20)   VALUE SPACES.

       01  CURRENT-INFO.
           05  CURRENT-BANK         PIC 9999.
           05  CURRENT-CPR          PIC X(15).
           05  CURRENT-INDEX        PIC 9(5).
           05  CURRENT-TIMESTAMP    PIC X(20).
           05  CURRENT-INBOUND      PIC S9(13)V99.
           05  PRETTY-INBOUND       PIC Z(12)9.99+.
           05  CURRENT-OUTBOUND     PIC S9(13)V99.
           05  PRETTY-OUTBOUND      PIC Z(12)9.99+.
           05  ORIGINAL-PRETTY      PIC Z(12)9.99+.
           05  CURRENT-AMOUNT       PIC S9(13)V99    VALUE ZEROES.
           05  CURRENT-AMOUNT-DKK   PIC Z(12)9.99+   VALUE ZEROES.
           05  DKK-STRING           PIC X(16)  VALUE SPACES  JUST RIGHT.
           05  AMOUNT-STRING        PIC X(17)  VALUE SPACES  JUST RIGHT.
           05  CURRENT-TYPE         PIC X(15)        VALUE SPACES.
           05  BALANCE              PIC S9(13)V99 VALUE 50000.00.
           05  PRETTY-BALANCE       PIC Z(12)9.99+.

       01  CPR-FOUND                PIC X       VALUE "N".
           88  FOUND                            VALUE "Y".
       01  EOF-SWITCH               PIC X       VALUE "N".
           88  EOF                              VALUE "Y".


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-BANKS
           PERFORM READ-TRANSACTIONS
           OPEN OUTPUT STATEMENTS-FILE
           PERFORM PRINT-STATEMENT VARYING TRANS-INDEX FROM 1 BY 1
                   UNTIL TRANS-INDEX > 100
           CLOSE STATEMENTS-FILE
           STOP RUN.

       READ-BANKS.
           DISPLAY "Banks going in!"
           MOVE "N" TO EOF-SWITCH
           OPEN INPUT BANK-FILE
           PERFORM UNTIL EOF
           READ BANK-FILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END
                   ADD 1 TO BANK-COUNT
                   MOVE BANK-RECORD TO BANK-INFO(BANK-COUNT)
           END-READ
           END-PERFORM
           DISPLAY "Read " BANK-COUNT " banks."
           CLOSE BANK-FILE
           EXIT.

       READ-TRANSACTIONS.
           DISPLAY "Transactions incoming!"
           OPEN INPUT TRANSACTION-FILE
           MOVE "N" TO EOF-SWITCH
           PERFORM UNTIL EOF
           READ TRANSACTION-FILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END
      *    If the transaction doesn't belong to the selected customer
                   IF NOT TRANS-RECORD(1:15) = CURRENT-CPR
                       MOVE "N" TO CPR-FOUND
                       IF CPR-COUNT > 0
                       SEARCH TRANS-GROUP VARYING TRANS-INDEX
                       WHEN TRANS-KEY(TRANS-INDEX) = TRANS-RECORD(1:15)
                       PERFORM
                           DISPLAY "Match " TRANS-KEY(TRANS-INDEX)
                           " at " TRANS-INDEX
                           MOVE TRANS-RECORD(1:15) TO CURRENT-CPR
                           MOVE TRANS-INDEX TO CURRENT-INDEX
                           MOVE "Y" TO CPR-FOUND
                       END-PERFORM
                       END-IF
                       IF NOT FOUND
                           ADD 1 TO CPR-COUNT
                           MOVE CPR-COUNT TO TRANS-INDEX
                       ELSE
                           SET TRANS-INDEX TO CURRENT-INDEX
                       END-IF
                       MOVE TRANS-RECORD(1:15) TO CURRENT-CPR
                       MOVE CURRENT-CPR TO TRANS-KEY
                               IN TRANS-GROUP(TRANS-INDEX)
                   END-IF
                   ADD 1 TO TRANS-COUNT(TRANS-INDEX)
                   MOVE TRANS-RECORD TO TRANS-DETAILS(TRANS-INDEX
                                               TRANS-COUNT(TRANS-INDEX))
      *            PERFORM WRITE-LINE-OUT
           END-READ
           END-PERFORM
           CLOSE TRANSACTION-FILE
           DISPLAY "Read " CPR-COUNT " customers."
           MOVE SPACES TO CURRENT-CPR
           EXIT.

       WRITE-LINE-OUT.
      *    DISPLAY FUNCTION TRIM(STMNT-RECORD)
           WRITE STMNT-RECORD
           INITIALIZE STMNT-RECORD
           EXIT.

       WRITE-TRANSACTION.
           *>DISPLAY "Writing transaction " CURRENT-INDEX
           *>    " for customer " TRANS-INDEX
           INITIALIZE CURRENT-AMOUNT CURRENT-TYPE CURRENT-AMOUNT-DKK
                   DKK-STRING AMOUNT-STRING

           MOVE FUNCTION TRIM(TRANS-AMOUNT(TRANS-INDEX CURRENT-INDEX))
                   TO CURRENT-AMOUNT
           MOVE CURRENT-AMOUNT TO ORIGINAL-PRETTY
           MOVE TRANS-TIMESTAMP
                   IN TRANS-DETAILS(TRANS-INDEX CURRENT-INDEX)
                   TO CURRENT-TIMESTAMP
           MOVE TRANS-TYPE
                   IN TRANS-DETAILS(TRANS-INDEX CURRENT-INDEX)
                   TO CURRENT-TYPE
           IF TRANS-CURRENCY(TRANS-INDEX CURRENT-INDEX) = "USD"
               MULTIPLY USD-CONVERSION-RATE BY CURRENT-AMOUNT
           ELSE
               IF TRANS-CURRENCY(TRANS-INDEX CURRENT-INDEX) = "EUR"
                   MULTIPLY EUR-CONVERSION-RATE BY CURRENT-AMOUNT
               END-IF
           END-IF
      *    Convert amount if not
      *    DISPLAY CURRENT-AMOUNT
      *    DISPLAY BALANCE
           IF FUNCTION TRIM(CURRENT-TYPE) = "Indbetaling"
               MOVE "Deposit" TO CURRENT-TYPE
           ELSE
               IF FUNCTION TRIM(CURRENT-TYPE) = "Udbetaling"
                   MOVE "Withdrawal" TO CURRENT-TYPE
               ELSE
                   MOVE "Direct Transfer" TO CURRENT-TYPE
                END-IF
           END-IF
           IF CURRENT-AMOUNT > 0
               ADD CURRENT-AMOUNT TO CURRENT-INBOUND
           ELSE
               ADD CURRENT-AMOUNT TO CURRENT-OUTBOUND
           END-IF
           MOVE CURRENT-AMOUNT TO CURRENT-AMOUNT-DKK
           MOVE FUNCTION TRIM(CURRENT-AMOUNT-DKK) TO DKK-STRING
           STRING FUNCTION TRIM(ORIGINAL-PRETTY) " "
               TRANS-CURRENCY(TRANS-INDEX CURRENT-INDEX)
               DELIMITED BY SIZE INTO AMOUNT-STRING

           STRING "| " CURRENT-TIMESTAMP(1:10)
               " |  " CURRENT-TIMESTAMP(12:8)
               "  |  " CURRENT-TYPE
               " | " DKK-STRING
               " | " AMOUNT-STRING
               " | " TRANS-STORE(TRANS-INDEX CURRENT-INDEX)
               "|" DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           EXIT.

       PRINT-STATEMENT.
      *    DISPLAY "Initializing statement for Customer: " TRANS-INDEX
      *    DISPLAY TRANS-DETAILS(TRANS-INDEX 1)
           MOVE ZEROES TO CURRENT-BANK
           MOVE TRANS-BANK-CODE(TRANS-INDEX 1) TO CURRENT-BANK
           INITIALIZE BALANCE
           MOVE 50000.00 TO BALANCE
           MOVE ZEROES TO CURRENT-INBOUND
           MOVE ZEROES TO CURRENT-OUTBOUND
           MOVE "====================================" TO STMNT-RECORD
           PERFORM WRITE-LINE-OUT.
           STRING "Customer: " FUNCTION TRIM(
                   TRANS-NAME IN TRANS-DETAILS(TRANS-INDEX 1))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING "Address: " FUNCTION TRIM(
                   TRANS-ADDRESS IN TRANS-DETAILS(TRANS-INDEX 1))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           PERFORM WRITE-LINE-OUT
           STRING RECIPIENT-INDENT "Bank: "
               BANK-NAME IN BANK-INFO(CURRENT-BANK)
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING RECIPIENT-INDENT "Bank Address: " FUNCTION TRIM(
                   BANK-ADDRESS IN BANK-INFO(CURRENT-BANK))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING RECIPIENT-INDENT "Phone Number: " FUNCTION TRIM(
                   BANK-PHONE IN BANK-INFO(CURRENT-BANK))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING RECIPIENT-INDENT "Email Address: "
               FUNCTION TRIM(BANK-EMAIL IN BANK-INFO(CURRENT-BANK))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           PERFORM WRITE-LINE-OUT
           PERFORM WRITE-LINE-OUT.
           STRING "Account statement for account number: "
               FUNCTION TRIM(TRANS-ACCT-ID
                   IN TRANS-DETAILS(TRANS-INDEX 1))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT.
           STRING
               "|    Date    |    Time    | Transaction Type |"
               "   Amount (DKK)   | Amount (Currency) |"
               "     Store name      |"
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING
               "|------------|------------|------------------|"
               "------------------|-------------------|"
               "---------------------|"
               DELIMITED BY SIZE INTO STMNT-RECORD.
           PERFORM WRITE-LINE-OUT.
           PERFORM WRITE-TRANSACTION VARYING CURRENT-INDEX FROM 1 BY 1
                   UNTIL CURRENT-INDEX > TRANS-COUNT(TRANS-INDEX)
           STRING
               "|------------|------------|------------------|"
               "------------------|-------------------|"
               "---------------------|"
               DELIMITED BY SIZE INTO STMNT-RECORD.
           PERFORM WRITE-LINE-OUT.
           ADD CURRENT-INBOUND TO BALANCE
           ADD CURRENT-OUTBOUND TO BALANCE
           MOVE CURRENT-INBOUND TO PRETTY-INBOUND
           MOVE CURRENT-OUTBOUND TO PRETTY-OUTBOUND
           STRING "Total inbound (DKK): " FUNCTION TRIM(PRETTY-INBOUND)
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING "Total outbound (DKK): "FUNCTION TRIM(PRETTY-OUTBOUND)
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           INITIALIZE PRETTY-BALANCE
           MOVE BALANCE TO PRETTY-BALANCE
           STRING "Balance (DKK): " FUNCTION TRIM(PRETTY-BALANCE)
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           PERFORM WRITE-LINE-OUT
           MOVE "Kind regards," TO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           MOVE FUNCTION TRIM(BANK-NAME IN BANK-INFO(CURRENT-BANK))
               TO STMNT-RECORD
           PERFORM WRITE-LINE-OUT.
           EXIT.
