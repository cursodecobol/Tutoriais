      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECKSIZE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-SMALLINT               PIC S9(4) COMP.
           01 WS-INTEGER                PIC S9(9) COMP.
           01 WS-COMP-3                 PIC S9(17) COMP-3.
           01 WS-NUMERICO               PIC S9(17).
           01 ws-minuscula              pic x(250).
           01 tabela.
              03 tab-string occurs 1250459.
                 05 ws-string pic x.
           01 1ws pic x value 'A'.
           01 WS-QTDE_1 PIC 9(11) VALUE 12345678901.
           01 1IF PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "PROGRAMA: CHECKSIE.cbl"
            DISPLAY "Exibe os tamanhos das variáveis."
            DISPLAY "--------------------------------"
            DISPLAY 'TAMANHOS:'
            DISPLAY 'SMALLINT           = '  LENGTH OF WS-SMALLINT
            DISPLAY 'INTEGER            = '  LENGTH OF WS-INTEGER
            DISPLAY 'DECIMAL COMPACTADO = '  LENGTH OF WS-COMP-3
            DISPLAY 'NUMERICO           = '  LENGTH OF WS-NUMERICO
            display 'ws-minuscula       = '  length of ws-minuscula
            display 'tabela             = '  length of tabela
            DISPLAY '1ws=' 1ws '          TAM = '  LENGTH OF 1ws.
            DISPLAY 'WS-QTDE_1=' WS-QTDE_1 ' TAM = ' LENGTH OF WS-QTDE_1

            GO TO SAIDA.

       SAIDA.
            STOP RUN.
       END PROGRAM CHECKSIZE.
