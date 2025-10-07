# COBOL

A project for learning the basics of **COBOL** (Common Business-Oriented Language). The purpose of this project is to learn to write production-grade **COBOL** in a way that is, in theory, applicable in business environments.


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

First I iteratively brainstorm with **ChatGPT** about what is a business-grade header in **COBOL**. The following is acceptable according to **ChatGPT**.

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

Next **ChatGPT** suggests to add **IDENTIFICATION DIVISION** and **PROGRAM-ID**. It is apparently very important in **COBOL** to add periods (**.**) after divisions, sections, paragraphs, and statements to terminate them.

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

Next we'll add **ENVIRONMENT DIVISION** which tells **COBOL** about the environment your program runs in, like input/output devices or files. Even if you don’t define files yet, including this division is standard practice in business **COBOL** programs.

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
       
       ENVIRONMENT DIVISION.

```

Per ChatGPT: even if your program doesn't use files yet, it's standard to include the **INPUT-OUTPUT SECTION** under **ENVIRONMENT DIVISION** to show where files or terminals would be defined. The asterisk (*) indicates a comment line, so it doesn't affect compilation. This prepares your program to scale later if you decide to read/write files.

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
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

```

Per ChatGPT: after **INPUT-OUTPUT SECTION**, add **DATA DIVISION**, where all program data is defined, and **WORKING-STORAGE SECTION** for variables that retain their values while the program runs (like the customer's name).

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
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.

```

Per **ChatGPT**: next we define **CUSTOMER-NAME** variable in **WORKING-STORAGE SECTION**. **01** = top-level variable (highest level in hierarchy), **CUSTOMER-NAME** = the name of the variable, and **PIC X(30)** = defines it as alphanumeric, up to 30 characters. This is the variable that will store the customer's input.

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
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-NAME        PIC X(30).

```

Per **ChatGPT**: after your **WORKING-STORAGE SECTION**, add **PROCEDURE DIVISION**, which starts the logic section of your program, and **MAIN-PROCEDURE**, which is a named paragraph where your main statements will go. Periods at the end of each line are required in **COBOL**. My program now looks like this:

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
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-NAME        PIC X(30).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

```

Per **ChatGPT**: next step is asking the customer for their name using **ACCEPT**, following professional **COBOL** style. **DISPLAY** = shows a message on the terminal, **ACCEPT** = waits for the user to type input and stores it in **CUSTOMER-NAME**. Proper indentation in **COBOL** (6–12 spaces after the paragraph name) = standard professional style. Though, I'll only use 4 spaces for the indentation because it works well with **OpenCobolIDE**.

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
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-NAME        PIC X(30).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Please enter your name: ".
           ACCEPT CUSTOMER-NAME.

```

Per **ChatGPT**: we'll now take the user's input and display a personalized greeting. It combines the literal "Hello, " with the value stored in **CUSTOMER-NAME**. **STOP RUN.** ends the program.

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
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-NAME        PIC X(30).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Please enter your name: ".
           ACCEPT CUSTOMER-NAME.
           DISPLAY "Hello, " CUSTOMER-NAME "!".
           STOP RUN.

```

**ChatGPT** claims the program is now enterprise-ready. We'll see about that, let's run it:

```
C:\Users\Business\bin\CUSTOMER-GREETING.exe 
Please enter your name: 

```

I enter my name, and get the following output:

```
C:\Users\Business\bin\CUSTOMER-GREETING.exe 
Please enter your name: 
Pekka
Hello, Pekka                         !

Process finished with exit code 0
```

Looks alright apart from the misplaced exclamation mark (**!**). **ChatGPT** says it's a classic **COBOL** string spacing issue: in **COBOL**, each **DISPLAY** argument is treated as a fixed-length field. **CUSTOMER-NAME** is defined as **01  CUSTOMER-NAME  PIC X(30)** and **PIC X(30)** always reserves 30 characters, even if the user types only 5 (Pekka). The literal **"Hello, "** is displayed, then the entire 30-character field of **CUSTOMER-NAME**, including trailing spaces, and then the **"!"**. That's why the **!** appears far to the right.

**ChatGPT** recommends: use **FUNCTION TRIM** to keep enterprise-style coding and allow any-length input up to 30 chars. I make the change to the program:

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
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-NAME        PIC X(30).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Please enter your name: ".
           ACCEPT CUSTOMER-NAME.
           DISPLAY "Hello, " FUNCTION TRIM(CUSTOMER-NAME) "!".
           STOP RUN.

```

Now when I run the program and enter my name, I get the following output:

```
C:\Users\Business\bin\CUSTOMER-GREETING.exe 
Please enter your name: 
Pekka
Hello, Pekka!

Process finished with exit code 0
```

The program now works correctly.

Finally, we add **END PROGRAM <program-name>** that explicitly marks the end of the program in **COBOL**, as per **ChatGPT**'s recommendation. It is optional in very simple programs but recommended in professional/enterprise **COBOL** for clarity and maintainability. I make the change to the code:

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
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-NAME        PIC X(30).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Please enter your name: ".
           ACCEPT CUSTOMER-NAME.
           DISPLAY "Hello, " FUNCTION TRIM(CUSTOMER-NAME) "!".
           STOP RUN.
       END PROGRAM CUSTOMER-GREETING.

```

The program still works correctly. A stunning success that concludes this exercise.


## Placeholder Title (developing the previous program further? Menus or loops, conditional greetings, age?)
