       IDENTIFICATION DIVISION.
       PROGRAM-ID. SRMINIMAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 WK-PARM.
          03 WK-PARM-LEN PIC S9(04) COMP.
          03 WK-PARM-TXT PIC X(10).
       PROCEDURE DIVISION USING WK-PARM.
           DISPLAY 'SRMINIMAL.cbl - INICIO'
           DISPLAY 'TAMANHO: ' WK-PARM-LEN
           DISPLAY 'TEXTO  : ' WK-PARM-TXT
           DISPLAY 'SRMINIMAL.cbl - FIM'
           .