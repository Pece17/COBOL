# COBOL

A project for learning the basics of **COBOL** (Common Business-Oriented Language). The purpose of this project is to learn to write production-grade **COBOL** in a way that is, in theory, applicable in business environments.


## Table of Contents

- [Software Installation and Setup](https://github.com/Pece17/COBOL?tab=readme-ov-file#software-installation-and-setup)
- [Creating a Customer Greeting Program](https://github.com/Pece17/COBOL?tab=readme-ov-file#creating-a-customer-greeting-program)
- [Creating a "Mini Bank"](https://github.com/Pece17/COBOL/tree/main?tab=readme-ov-file#creating-a-mini-bank)
     - [Adding User Accounts and Balances in **WORKING-STORAGE**](https://github.com/Pece17/COBOL?tab=readme-ov-file#adding-user-accounts-and-balances-in-working-storage)


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


## Creating a "Mini Bank"

I want to build on the program I created in the previous section and make additions that in theory could have real-world business applications.

I ask **ChatGPT** for suggestions, and this is what I get:

- input validation
- a looping menu
- subroutines/paragraphs
- date and time handling
- file I/O (input/output)
- conditional logic based on data
- use of copybooks
- structured error handling and logging
- data storage

I'm personally also thinking about the abilities to withdraw and deposit money, to transfer money to other users' accounts, and to check balance, so I suppose the goal would be to make a "mini bank". I'm thinking a few users that require passwords for login. Let's get started.

I create a new file called **MINI-BANK.cbl** and paste the code of **CUSTOMER-GREETING.cbl** in it. I make a few changes to the header and change the program name:

```
      ******************************************************************
      * Author: Pekka Surname
      * Date: 2025-10-07
      * Version: 1.0
      * Purpose: A program that works as a miniature banking system.
      * Compiler: cobc
      * Remarks: A simple tutorial example for learning COBOL.
      *          A small banking system with various account operations.
      * Dependencies: None
      ******************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINI-BANK.
       
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
       END PROGRAM MINI-BANK.

```


### Adding User Accounts and Balances in **WORKING-STORAGE**

Per **ChatGPT**: since we don't have files yet, we'll keep a small, fixed set of users in memory. Each user will have:

- username (**CUSTOMER-NAME**)
- password (**CUSTOMER-PASSWORD**)
- balance (**ACCOUNT-BALANCE**)

Here's how we can define that in the **WORKING-STORAGE SECTION**:

```
      ******************************************************************
      * Author: Pekka Surname
      * Date: 2025-10-07
      * Version: 1.0
      * Purpose: A program that works as a miniature banking system.
      * Compiler: cobc
      * Remarks: A simple tutorial example for learning COBOL.
      *          A small banking system with various account operations.
      * Dependencies: None
      ******************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINI-BANK.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  NUM-USERS        PIC 9 VALUE 3.
       01  USER-INDEX       PIC 9 VALUE 1.
       
       01  USER-TABLE.
           05 USERS OCCURS 3 TIMES.
              10 CUSTOMER-NAME      PIC X(30).
              10 CUSTOMER-PASSWORD  PIC X(10).
              10 ACCOUNT-BALANCE    PIC 9(6)V99 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Please enter your name: ".
           ACCEPT CUSTOMER-NAME.
           DISPLAY "Hello, " FUNCTION TRIM(CUSTOMER-NAME) "!".
           STOP RUN.
       END PROGRAM MINI-BANK.

```

- **NUM-USERS** = how many users we have (3 for now).
- **USER-TABLE** is an array of 3 users.
- Each user has a name, password, and balance.
- **USER-INDEX** will help us know which user is currently logged in.

Now I get the following error: **'CUSTOMER-NAME' requires one subscript**.

**ChatGPT** says: you currently have a user table defined, but you’re trying to use **CUSTOMER-NAME** directly (without a subscript), and that causes the error. So before we add any new logic, let’s initialize your users properly so you can access them with subscripts like **CUSTOMER-NAME(1)**. I do that and comment out the main procedure of **CUSTOMER-GREETING.cbl**, because it might cause issues with this new program. It is somewhat difficult to try to use **ChatGPT** as an instructor when creating a program because the AI will sometimes change things without notifying, is occasionally inconsistent and doesn't remember my requests, or moves too fast with the coding. Here's my current code:

```
      ******************************************************************
      * Author: Pekka Surname
      * Date: 2025-10-07
      * Version: 1.0
      * Purpose: A program that works as a miniature banking system.
      * Compiler: cobc
      * Remarks: A simple tutorial example for learning COBOL.
      *          A small banking system with various account operations.
      * Dependencies: None
      ******************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINI-BANK.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
      * No external files used in this simple example

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  NUM-USERS        PIC 9 VALUE 3.
       01  USER-INDEX       PIC 9 VALUE 1.
       
       01  USER-TABLE.
           05 USERS OCCURS 3 TIMES.
              10 CUSTOMER-NAME      PIC X(30).
              10 CUSTOMER-PASSWORD  PIC X(10).
              10 ACCOUNT-BALANCE    PIC 9(6)V99 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           
      *    DISPLAY "Please enter your name: ".
      *    ACCEPT CUSTOMER-NAME.
      *    DISPLAY "Hello, " FUNCTION TRIM(CUSTOMER-NAME) "!".
       
           MOVE "PEKKA" TO CUSTOMER-NAME(1).
           MOVE "PASS1" TO CUSTOMER-PASSWORD(1).
           MOVE 1000.50 TO ACCOUNT-BALANCE(1).
           
           MOVE "ANNA" TO CUSTOMER-NAME(2).
           MOVE "PASS2" TO CUSTOMER-PASSWORD(2).
           MOVE 2500.00 TO ACCOUNT-BALANCE(2).
           
           MOVE "MIKA" TO CUSTOMER-NAME(3).
           MOVE "PASS3" TO CUSTOMER-PASSWORD(3).
           MOVE 1000.50 TO ACCOUNT-BALANCE(3).
           
           PERFORM VARYING USER-INDEX FROM 1 BY 1 UNTIL USER-INDEX > NUM-USERS
               DISPLAY "User: " CUSTOMER-NAME(USER-INDEX)
               DISPLAY "Balance: " ACCOUNT-BALANCE(USER-INDEX)
           END-PERFORM
       
           STOP RUN.
       END PROGRAM MINI-BANK.

```

I get the following error: **'NUM' is not defined**. **ChatGPT** can't seem to get past this issue even after multiple tries, even when it confidently claims that the code should now work perfectly after each iteration. My conclusion is that—as it stands—the free version of **ChatGPT** is good for creating simple **COBOL** codes, but it struggles with larger programs. At this point I feel like it's better to pivot to watching YouTube tutorials and reading manuals instead of spending time with something that doesn't seem to work, if I want to learn **COBOL**. Though, **ChatGPT** can still be useful with problem-solving in many cases.
