       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-8.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "data/CUSTOMERS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNTS-FILE ASSIGN TO "data/ACCOUNTS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DATABASE ASSIGN TO "build/A8.dat"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           COPY CUSTOMER.

       FD  ACCOUNTS-FILE.
       01  ACCOUNT-RECORD.
           COPY ACCOUNT.

       FD  DATABASE.
       01  DB-RECORD            PIC X(500).

       WORKING-STORAGE SECTION.
       01  READ-LOOP-INFO.
           COPY A6-LOOP.

       01  CUSTOMER-TABLE.
           02 CUSTOMERS OCCURS 1 TO 1000 TIMES
                       DEPENDING ON CUSTOMER-COUNT
                       INDEXED BY CUSTOMER-INDEX.
               03 CUSTOMER-DATA.
                   COPY CUSTOMER.
               03 ACCOUNT-COUNT    PIC 99  VALUE ZEROES.
               03 CUSTOMER-ACCOUNTS OCCURS 10 TIMES.
                   COPY ACCOUNT.

       77  PRETTY-BALANCE       PIC Z(7).99.
       01  EOF-SWITCH           PIC X        VALUE "N".
           88  EOF                           VALUE "Y".


       PROCEDURE DIVISION.
       MAIN-RUNTIME.
           OPEN INPUT CUSTOMER-FILE
           PERFORM READ-CUSTOMER UNTIL EOF
           CLOSE CUSTOMER-FILE

           OPEN INPUT ACCOUNTS-FILE
           MOVE "N" TO EOF-SWITCH
           PERFORM READ-ACCOUNTS UNTIL EOF
           CLOSE ACCOUNTS-FILE

           OPEN OUTPUT DATABASE
           PERFORM VARYING LOOP-INDEX FROM 1 BY 1
                   UNTIL LOOP-INDEX > CUSTOMER-COUNT
           MOVE CUSTOMERS(LOOP-INDEX) TO DB-RECORD
           WRITE DB-RECORD
           END-WRITE
           END-PERFORM
           CLOSE DATABASE

           PERFORM PRINT-CUSTOMER VARYING LOOP-INDEX FROM 1 BY 1
                   UNTIL LOOP-INDEX > CUSTOMER-COUNT
           DISPLAY "========================================"
           MOVE CUSTOMER-COUNT TO FINAL-COUNT
           DISPLAY "Customers added: " FUNCTION TRIM(FINAL-COUNT)

           STOP RUN.

       READ-CUSTOMER.
           READ CUSTOMER-FILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END
                   IF CUSTOMER-COUNT < MAX-CUSTOMERS
                       ADD 1 TO CUSTOMER-COUNT
                       MOVE CUSTOMER-RECORD
                           TO CUSTOMER-DATA (CUSTOMER-COUNT)
                    END-IF
           END-READ
           EXIT.

       PRINT-CUSTOMER.
           DISPLAY "========================================"
           DISPLAY CUSTOMERS(LOOP-INDEX)
           EXIT.

       ADD-ACCOUNT-TO-CUSTOMER.
           IF ACCOUNT-COUNT(CUSTOMER-INDEX) < 10
               ADD 1 TO ACCOUNT-COUNT(CUSTOMER-INDEX)
                   MOVE ACCOUNT-RECORD TO
                       CUSTOMER-ACCOUNTS(CUSTOMER-INDEX,
                           ACCOUNT-COUNT(CUSTOMER-INDEX))
               ELSE
                   DISPLAY "Customer "CUSTOMER-ID
                       IN CUSTOMER-DATA(CUSTOMER-INDEX)
                       " has reached the account limit."
               END-IF
           EXIT.

       READ-ACCOUNTS.
           READ ACCOUNTS-FILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END
                   SEARCH CUSTOMERS VARYING CUSTOMER-INDEX
                       WHEN CUSTOMER-ID IN CUSTOMER-DATA(CUSTOMER-INDEX)
                           = CUSTOMER-ID IN ACCOUNT-RECORD
                           PERFORM ADD-ACCOUNT-TO-CUSTOMER
                   END-SEARCH
           END-READ
           EXIT.
