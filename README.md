# COBOL

A project for learning the basics of COBOL (Common Business-Oriented Language). The purpose of this project is to learn to write production-grade COBOL in a way that is, in theory, applicable in business environments.


## Software Installation and Setup

I'm using [this YouTube tutorial](https://www.youtube.com/watch?v=LImuEAmVkIw) by **Kumar ITChannel** as the basis of this project. I'm also using **ChatGPT** as a teacher during this project.

First I go to https://pypi.org/project/OpenCobolIDE/ and from there open link https://launchpad.net/cobcide/+download for the Windows installer. I download **OpenCobolIDE-4.7.6_Setup.exe**, which is the Windows installer, run it as administrator, and go through the installation.

After the installation is complete, I open the **OpenCobolIDE** program, click **New file**, leave **Program** as the **Template:** and leave **.cbl** as the file extension, and name the file as **TEST**. I don't change any other parameters, and I select **OK**.

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

I put my first name as the author and change the date, purpose, **PROGRAM-ID.**, and the text to be displayed.

```
      ******************************************************************
      * Author: Pekka
      * Date: 2025-10-07
      * Purpose: Testing COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "I'm learning COBOL for the first time."
            STOP RUN.
       END PROGRAM TEST.


```

I run the program and get the following output:

```
C:\Users\Business\bin\TEST.exe 
I'm learning COBOL for the first time.

Process finished with exit code 0
```

**OpenCobolIDE** seems to work correctly, so I can move on to more complex exercises.


## Creating a Customer Greeting Program

Let's try to write a simple program for an imaginary bank that asks the customer to enter their name in the program. I start by creating a new program from scratch called **CUSTOMER-GREETING.cbl**.

First I iteratively brainstorm with **ChatGPT** about what is a business-grade header in COBOL. The following is acceptable according to **ChatGPT**.

```
      ******************************************************************
      * Author: Pekka Surname
      * Date: 2025-10-07
      * Version: 1.0
      * Purpose: A program that greets the customer.
      * Compiler: cobc
      * Remarks: Simple tutorial example for learning COBOL.
      *          Accepts customer name and displays a greeting.
      * Dependencies: None
      ******************************************************************
```

Next **ChatGPT** suggests to add **IDENTIFICATION DIVISION** and **PROGRAM-ID**. It is apparently very important in COBOL to add periods (**.**) after divisions, sections, paragraphs, and statements to terminate them.

I make the following additions to the program—**ChatGPT** approves:

```
      ******************************************************************
      * Author: Pekka Surname
      * Date: 2025-10-07
      * Version: 1.0
      * Purpose: A program that greets the customer.
      * Compiler: cobc
      * Remarks: Simple tutorial example for learning COBOL.
      *          Accepts customer name and displays a greeting.
      * Dependencies: None
      ******************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-GREETING.

```
