# COBOL

A project for learning the basics of COBOL (Common Business-Oriented Language).


## Software Installation and Setup

I'm using [this YouTube tutorial](https://www.youtube.com/watch?v=LImuEAmVkIw) by **Kumar ITChannel** as the basis of this project.

First I go https://pypi.org/project/OpenCobolIDE/ and from there open link https://launchpad.net/cobcide/+download for the Windows installer. I download **OpenCobolIDE-4.7.6_Setup.exe**, which is the Windows installer, run it as administrator, and go through the installation.

After the installation is complete, I open the **OpenCobolIDE** program, click **New file**, leave **Program** as the **Template:**, and name the file as **TEST**. I don't change any other parameters, and I select **OK**.

The terminal outputs the following:

```
      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
```

I run the program by clicking the ▶️ (play) button, and get the following output:

```
C:\Users\Business\bin\TEST.exe 
Hello world

Process finished with exit code 0
```

The program works correctly.

I put my first name as the author and change the text to be displayed.

```
      ******************************************************************
      * Author: Pekka
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "I'm learning COBOL for the first time."
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
```

I run the program and get the following output:

```
C:\Users\Business\bin\TEST.exe 
I'm learning COBOL for the first time.

Process finished with exit code 0
```

**OpenCobolIDE** seems to work correctly, so I can move on to more complex exercises.


## Placeholder Title
