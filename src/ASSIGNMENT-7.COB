       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-7.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "data/CUSTOMERS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DATABASE ASSIGN TO "build/A7.dat"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD      PIC X(211).

       FD  DATABASE.
       01  DB-RECORD            PIC X(100).

       WORKING-STORAGE SECTION.
       01  READ-LOOP-INFO.
           COPY A6-LOOP.

       01  CUSTOMER-TABLE.
           02 CUSTOMERS OCCURS 1 TO 1000 TIMES
                       DEPENDING ON CUSTOMER-COUNT
                       INDEXED BY CUSTOMER-INDEX.
               03 CUSTOMER-DATA.
                   COPY CUSTOMER.

       77  PRETTY-BALANCE       PIC Z(7).99.
       01  EOF-SWITCH           PIC X        VALUE "N".
           88  EOF                           VALUE "Y".


       PROCEDURE DIVISION.
       MAIN-RUNTIME.
           OPEN INPUT CUSTOMER-FILE.
           PERFORM READ-LOOP UNTIL EOF
           CLOSE CUSTOMER-FILE

           PERFORM PRINT-CUSTOMER VARYING LOOP-INDEX FROM 1 BY 1
                   UNTIL LOOP-INDEX > CUSTOMER-COUNT.
           MOVE CUSTOMER-COUNT TO FINAL-COUNT
           DISPLAY "Customers added: " FUNCTION TRIM(FINAL-COUNT)

           OPEN OUTPUT DATABASE.
           PERFORM WRITE-FILE VARYING LOOP-INDEX FROM 1 BY 1
                   UNTIL LOOP-INDEX > CUSTOMER-COUNT
           CLOSE DATABASE
           DISPLAY "Customers written to database file."

           STOP RUN.

       READ-LOOP.
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

       WRITE-FILE.
           MOVE SPACES TO DB-RECORD
           MOVE CUSTOMER-ID(LOOP-INDEX) TO DB-RECORD
           WRITE DB-RECORD
           MOVE SPACES TO DB-RECORD
           STRING
               FIRST-NAME(LOOP-INDEX) DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               LAST-NAME(LOOP-INDEX) DELIMITED BY SPACE
               INTO DB-RECORD
           WRITE DB-RECORD
           MOVE SPACES TO DB-RECORD
           STRING
               STREET-NAME(LOOP-INDEX) DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               BUILDING-NUMBER(LOOP-INDEX) DELIMITED BY SPACE
               ", " DELIMITED BY SIZE
               FLOOR(LOOP-INDEX) DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               SIDE(LOOP-INDEX) DELIMITED BY SPACE
               INTO DB-RECORD
           WRITE DB-RECORD
           MOVE SPACES TO DB-RECORD
           STRING
               POSTAL-CODE(LOOP-INDEX) DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               CITY(LOOP-INDEX) DELIMITED BY SPACE
               INTO DB-RECORD
           WRITE DB-RECORD
           MOVE SPACES TO DB-RECORD
           MOVE PHONE-NUMBER(LOOP-INDEX) TO DB-RECORD
           WRITE DB-RECORD
           MOVE SPACES TO DB-RECORD
           MOVE EMAIL-ADDRESS(LOOP-INDEX) TO DB-RECORD
           WRITE DB-RECORD
           MOVE SPACES TO DB-RECORD
           WRITE DB-RECORD
           EXIT.

       PRINT-CUSTOMER.
           MOVE ACCOUNT-BALANCE(LOOP-INDEX) TO PRETTY-BALANCE
           DISPLAY "========================================"
           DISPLAY " Customer ID     | " CUSTOMER-ID(LOOP-INDEX)
           DISPLAY " Customer Name   | "
           FUNCTION TRIM(FIRST-NAME(LOOP-INDEX))
           " " FUNCTION TRIM(LAST-NAME(LOOP-INDEX))
           DISPLAY " Account Number  | " ACCOUNT-NUMBER(LOOP-INDEX)
           DISPLAY " Account Balance | "
           FUNCTION TRIM(PRETTY-BALANCE) " " CURRENCY-CODE(LOOP-INDEX)
           DISPLAY "========================================"
           EXIT.
