       IDENTIFICATION DIVISION.
       PROGRAM-ID. DLM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DramaFile ASSIGN TO 'dramalist.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FileStatus.

           SELECT TempDFile ASSIGN TO 'dramalistTEMP.dat'
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS FileStatus.

       DATA DIVISION.
       FILE SECTION.

       FD DramaFile.
       01  DramaRecord.
           05   DramaTitle         PIC X(50).
           05   WatchedStatus      PIC X(15).
       FD TempDFile.
       01  TempDRecord.
           05   DramaTitleTemp     PIC X(50).
           05   WatchedStatusTemp  PIC X(15).

       WORKING-STORAGE SECTION.

       01  UserChoice              PIC X.
       01  EndOfFile               PIC X        VALUE 'N'.
       01  FileStatus              PIC XX.
       01  SearchTitle             PIC X(50).
       01  Found                   PIC X        VALUE 'N'.
       01  WS-DramaTitle           PIC X(50).
       01  WS-WatchedStatus        PIC X(15).
       
       
       PROCEDURE DIVISION.
           PERFORM UNTIL UserChoice = 5
               DISPLAY 'DRAMA LIST'
               DISPLAY '1. ADD DRAMA'
               DISPLAY '2. READ DRAMA'
               DISPLAY '3. EDIT STATUS'
               DISPLAY '4. DELETE DRAMA'
               DISPLAY '5. EXIT'
               DISPLAY 'ENTER YOUR CHOICE: ' WITH NO ADVANCING
               ACCEPT UserChoice

               EVALUATE UserChoice
                   WHEN '1'
                       PERFORM WriteInput
                   WHEN '2'
                       PERFORM ViewDramas
                   WHEN '3'
                       PERFORM UpdateStatus
                   WHEN '4'
                       PERFORM DeleteDrama
                   WHEN '5'
                       DISPLAY 'Exiting program.'
                   WHEN OTHER 
                       DISPLAY 'Invalid choice. Please try again.'
               END-EVALUATE 
           END-PERFORM 

           STOP RUN.
       
       WriteInput.
           OPEN I-O DramaFile
           IF FileStatus = '35'
               DISPLAY 'Creating a file.'
               PERFORM FDrama
           ELSE 
               READ DramaFile
                   AT END 
                       DISPLAY 'CREATE BAGO KASI WALA LAMAN'
                       CLOSE DramaFile
                       PERFORM FDrama
                   NOT AT END 
                       CLOSE DramaFile
                       DISPLAY 'INCREMENT DRAMA LIST'
                       PERFORM AddDrama
               END-READ 
           END-IF 
           CLOSE DramaFile.

       FDrama.
           OPEN OUTPUT DramaFile

           DISPLAY 'Enter Drama Title (30 characters): ' NO ADVANCING
           ACCEPT DramaTitle
           DISPLAY 'Enter Status (Watched/On Going/Not Watched): '
           NO ADVANCING
           ACCEPT WatchedStatus 
           WRITE DramaRecord

           CLOSE DramaFile
           DISPLAY 'Drama added successfully.'.

       AddDrama.
           OPEN EXTEND DramaFile

           DISPLAY 'Enter Drama Title (30 characters): ' NO ADVANCING
           ACCEPT DramaTitle
           DISPLAY 'Enter Status (Watched/On Going/Not Watched): '
           NO ADVANCING
           ACCEPT WatchedStatus 
           WRITE DramaRecord

           CLOSE DramaFile
           DISPLAY 'Drama added successfully.'.

       ViewDramas.
           OPEN INPUT DramaFile
           IF FileStatus = '00'
               DISPLAY 'ERROR' FileStatus
           END-IF
           
           PERFORM UNTIL EndOfFile = 'Y'
               READ DramaFile INTO DramaRecord
                   AT END 
                       MOVE 'Y' TO EndOfFile
                   NOT AT END 
                       DISPLAY 'Title: ' DramaTitle
                       DISPLAY 'Status: ' WatchedStatus
               END-READ 
           END-PERFORM 
           CLOSE DramaFile
           DISPLAY 'Finished viewing dramas.'.
                       
       UpdateStatus.
           DISPLAY 'Enter Drama title to update status: '
           ACCEPT SearchTitle

           OPEN I-O DramaFile
           MOVE 'N' TO Found
           MOVE 'N' TO EndOfFile

             PERFORM UNTIL EndOfFile = 'Y'
                READ DramaFile INTO DramaRecord
                   AT END
                       MOVE 'Y' TO EndOfFile
                   NOT AT END 
                       IF DramaTitle = SearchTitle
                          MOVE 'Y' TO Found
                          DISPLAY 'Current Status: ' WatchedStatus
                          DISPLAY 'Enter new Status (Watched/On Going/No
      -                   't Watched)'
                          ACCEPT WatchedStatus
                          REWRITE DramaRecord
                          DISPLAY 'Watched status updated successfully.'
                       END-IF 
                END-READ
             END-PERFORM
           IF Found = 'N'
               DISPLAY 'Drama title not found.'
           END-IF
           CLOSE DramaFile.                          

       DeleteDrama.
           DISPLAY 'Enter Drama title to update status: '
           ACCEPT WS-DramaTitle

           OPEN INPUT DramaFile
           OPEN OUTPUT TempDFile

           MOVE 'N' TO Found
           MOVE 'N' TO EndOfFile

             PERFORM UNTIL EndOfFile = 'Y'
                READ DramaFile INTO DramaRecord
                   AT END
                       MOVE 'Y' TO EndOfFile
                   NOT AT END 
                       IF WS-DramaTitle = DramaTitle
                          MOVE 'Y' TO Found
                          DISPLAY 'Deleting Drama: '
                          DISPLAY 'Title: ' DramaTitle
                          DISPLAY 'Status: ' WatchedStatus
                          CONTINUE
                        ELSE
                          WRITE TempDRecord FROM DramaRecord
                       END-IF 
                END-READ
             END-PERFORM
            
           CLOSE DramaFile
           CLOSE TempDFile

           IF Found = 'Y'
               DISPLAY 'Drama deleted successfully.'
               CALL 'system' USING 'rm dramalist.dat'
               CALL 'system' USING 'mv dramalistTEMP.dat dramalist.dat'
           ELSE 
               DISPLAY 'Drama not found.'
           END-IF.

