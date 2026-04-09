       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-5.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CUSTOMER-RECORD.
           COPY "CUSTOMER".

       77  TRIMMED-BALANCE       PIC Z(7).99.
       77  FULL-NAME             PIC X(40).
       77  FULL-NAME-CLEAN       PIC X(40).

       01  LOOP-VARIABLES.
           COPY "A5-LOOP".

       PROCEDURE DIVISION.
      *    Assign values to the variables
       POPULATE-CUSTOMER-RECORD.
           MOVE "1235467890" TO CUSTOMER-ID.
           MOVE "Hans" TO FIRST-NAME.
           MOVE "Jørgensen" TO LAST-NAME.
           MOVE "DABADKKK991234567890" TO ACCOUNT-NUMBER.
           MOVE 500000.01 TO ACCOUNT-BALANCE.
           MOVE "DKK" TO CURRENCY-CODE.

           MOVE ACCOUNT-BALANCE TO TRIMMED-BALANCE.
           STRING FIRST-NAME DELIMITED BY SIZE
           " " DELIMITED BY SIZE
           LAST-NAME DELIMITED BY SIZE
           INTO FULL-NAME.

           MOVE FUNCTION TRIM(FULL-NAME) TO FULL-NAME.
       TRIM-FULL-NAME.
      *    MOVE 1 TO CLEAN-POS
           PERFORM VARYING CHAR-POS FROM 1 BY 1
           UNTIL CHAR-POS > LENGTH OF FULL-NAME
               MOVE FULL-NAME(CHAR-POS:1) TO CURRENT-CHAR
               IF SPACE NOT = CURRENT-CHAR OR PREVIOUS-CHAR, THEN
                   MOVE CURRENT-CHAR TO FULL-NAME-CLEAN(CLEAN-POS:1)
                   ADD 1 TO CLEAN-POS
               END-IF
               MOVE CURRENT-CHAR TO PREVIOUS-CHAR
           END-PERFORM.

       DISPLAY-CUSTOMER-RECORD.
           DISPLAY "========================================"
           DISPLAY " Customer ID     | " CUSTOMER-ID
           DISPLAY " Customer Name   | "FUNCTION TRIM(FULL-NAME-CLEAN)
           DISPLAY " Account Number  | " ACCOUNT-NUMBER
           DISPLAY " Account Balance | "
           FUNCTION TRIM(TRIMMED-BALANCE) " " CURRENCY-CODE
           DISPLAY "========================================"
           DISPLAY CUSTOMER-RECORD
           STOP RUN.
