       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-HANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORIGINAL-FILE ASSIGN TO "Kdrama.txt"
           ORGANIZATION IS SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "temp.txt" 
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ORIGINAL-FILE.
       01 KDRAMA-RECORD.
           02 KDRAMA-TITLE PIC X(26) .
           02 KDRAMA-UNWATCH PIC X(70) .
           02 KDRAMA-TITLE2 PIC X(25) .
           02 KDRAMA-WATCHED PIC X(70) .      

       FD TEMP-FILE.
       01 TEMP-RECORD PIC X(300).
       
       WORKING-STORAGE SECTION.
       01 WS-TITLE PIC A(70).
       01 WS-CHOICE PIC 9 .
       01 WS-EOF PIC X VALUE "N".
       01 WS-SEARCH-TITLE PIC A(70).
       01 WS-TITLE-ON-OFF PIC 9 VALUE 0 .
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-MENU UNTIL WS-CHOICE = 6
               STOP RUN.

       DISPLAY-MENU.
           DISPLAY "K-DRAMA LIST"
           DISPLAY "1. ADD K-DRAMA"
           DISPLAY "2. REMOVE K-DRAMA"
           DISPLAY "3. LIST K-DRAMA TO WATCH"
           DISPLAY "4. LIST OF WATCHED K-DRAMAS"
           DISPLAY "5. MARK K-DRAMA AS WATCHED"
           DISPLAY "6. EXIT"
           DISPLAY "ENTER YOUR CHOICE(1-6): " NO ADVANCING.
           ACCEPT WS-CHOICE.

           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM ADD-KDRAMA
               WHEN 2
                   PERFORM REMOVE-KDRAMA
               WHEN 3
                   PERFORM LIST-KDRAMAS
               WHEN 4
                   PERFORM LIST-WATCHED-KDRAMAS
               WHEN 5
                   PERFORM MARK-WATCHED
               WHEN 6
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
           END-EVALUATE.

       ADD-KDRAMA.
           DISPLAY "Enter the title of the K-Drama to add: " .
           ACCEPT WS-TITLE.
           
           IF WS-TITLE-ON-OFF = 0
               MOVE "LIST OF K-DRAMAS TO WATCH:" TO KDRAMA-TITLE
               MOVE WS-TITLE TO KDRAMA-UNWATCH
               MOVE "LIST OF WATCHED K-DRAMAS:" TO KDRAMA-TITLE2
               MOVE " " TO KDRAMA-WATCHED

               OPEN OUTPUT ORIGINAL-FILE
               WRITE KDRAMA-RECORD
               END-WRITE

               CLOSE ORIGINAL-FILE

               MOVE 1 TO WS-TITLE-ON-OFF

           ELSE IF WS-TITLE-ON-OFF = 1
               MOVE " " TO KDRAMA-TITLE
               MOVE WS-TITLE TO KDRAMA-UNWATCH
               MOVE " " TO KDRAMA-TITLE2
               MOVE " " TO KDRAMA-WATCHED
               
               OPEN EXTEND ORIGINAL-FILE
               WRITE KDRAMA-RECORD
               END-WRITE

               CLOSE ORIGINAL-FILE
           END-IF.
           
           OPEN OUTPUT ORIGINAL-FILE.
           WRITE KDRAMA-RECORD
           END-WRITE.

           CLOSE ORIGINAL-FILE.

       REMOVE-KDRAMA.
           DISPLAY "Enter the title to delete: ".
           ACCEPT WS-SEARCH-TITLE.
           OPEN INPUT ORIGINAL-FILE.
           OPEN OUTPUT TEMP-FILE.
           READ ORIGINAL-FILE
               AT END MOVE 'Y' TO WS-EOF
           END-READ.
           PERFORM UNTIL WS-EOF = 'Y'
               IF KDRAMA-RECORD NOT = WS-SEARCH-TITLE
                   WRITE TEMP-RECORD FROM KDRAMA-RECORD
               END-IF
               READ ORIGINAL-FILE
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM.
           CLOSE ORIGINAL-FILE.
           CLOSE  TEMP-FILE.
           CALL 'RENAME' USING 'tempt.txt', 'kdrama.txt'.

       LIST-KDRAMAS.
           DISPLAY KDRAMA-TITLE.
           DISPLAY KDRAMA-UNWATCH.

       LIST-WATCHED-KDRAMAS.
           DISPLAY KDRAMA-TITLE2.
           DISPLAY KDRAMA-WATCHED.

       MARK-WATCHED.
           DISPLAY "Enter the title to mark as watched: ".
           ACCEPT WS-SEARCH-TITLE.
           OPEN I-O ORIGINAL-FILE.
           READ ORIGINAL-FILE
               AT END MOVE 'Y' TO WS-EOF
           END-READ.
           PERFORM UNTIL WS-EOF = 'Y'
               IF WS-SEARCH-TITLE = KDRAMA-UNWATCH
                   MOVE KDRAMA-UNWATCH TO KDRAMA-WATCHED
                   REWRITE KDRAMA-RECORD
                   DISPLAY "K-Drama marked as watched."
                   EXIT PERFORM
               END-IF
               READ ORIGINAL-FILE
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM.
           CLOSE ORIGINAL-FILE.

       END PROGRAM  FILE-HANDLING.

