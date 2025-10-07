      ******************************************************************
      * Author: Your Name
      * Date: 2025-10-07
      * Purpose: Sample strict COBOL program skeleton
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE-PROGRAM.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       FILE SECTION.
      * File definitions go here, if any

       WORKING-STORAGE SECTION.
           01 WS-MESSAGE     PIC X(30) VALUE 'Hello, COBOL world!'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY WS-MESSAGE.
           STOP RUN.
       END PROGRAM SAMPLE-PROGRAM.
