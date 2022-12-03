      ******************************************************************
      * SISTEMA    : FINANCEIRO
      * ANALISTA   : CURSO DE COBOL VIDEOAULAS - www.cursodecobol.com.br
      * PROGRAMADOR: Wesley Mendonça.
      * DATA       : 02/10/2020
      * OBJETIVO   : ATUALIZAR SALDO DE CONTAS
      *              ARQUIVOS DE ENTRADA: FINSLD   - SALDOS DE CONTAS
      *                                   FINTRAN  - TRANSAÇÕES DE
      *                                              DÉBITO / CREDITO
      *              IMPORTANTE:
      *              - ORDENAR ARQUIVOS DE ENTRADA P/ RESPECTIVAS CHAVES
      *
      *              ARQUIVOS DE SAIDA  : FINSLDS  - SALDOS ATUALIZADOS
      *                                   FINTRANS - TRANS.REJEITADAS
      *
      * BALANCE LINE
      * - Técnica de programação que consiste em pesquisa de dados entre
      *   dois ou mais arquivos sequenciais, classificados por uma
      *   chave em comum.
      * - Neste programa está sendo considerado o relacionamento
      *   1 x N
      *
      * ESPECIFICAÇÃO
      * O arquivo de Contas (SALDOS) deve ser atualizado
      * mediante transações de Débito e/ou Crédito.
      * Para cada conta:
      * - Pode-se ter zero, uma ou mais transações, de Débito ou Crédito
      * - Todas as contas devem ser gravadas no arquivo de saída.
      *   . As contas sem transação, devem ter o saldo inalterado.
      * Para cada transação:
      * - Pode haver ou não uma conta.
      * - Uma transação, sem conta correspondente, deve ser REJEITADA.
      ******************************************************************
      * VRS003 - 03/12/2022 - REVISÃO; INCLUSÃO DE COMENTÁRIOS;
      *                       ACRESCIMO DO SUFIXO ".dat" AOS ARQUIVOS
      * VRS002 - 30/10/2020 - MELHORIA DE PERFORMANCE
      * VRS001 - 16/10/2020 - IMPLANTAÇÃO
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID.      FINPB100.
       AUTHOR.          CURSO DE COBOL.
       DATE-COMPILED.   16/10/2020
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
      *-----------------------------------------------------------------
       FILE-CONTROL.
      *    ARQUIVO SALDOS - ENTRADA  ( READ )
           SELECT FINSLD   ASSIGN TO 'FINSLD.dat'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-FINSLD.
      *    ARQUIVO TRANSAÇÕES - ENTRADA (READ)
           SELECT FINTRAN  ASSIGN TO 'FINTRAN.dat'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-FINTRAN.
      *    ARQUIVO SALDOS - ATUALIZADO  - SAÍDA (WRITE)
           SELECT FINSLDS  ASSIGN TO 'FINSLDS.dat'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-FINSLDS.
      *    ARQUIVO TRANSAÇÕES REJEITADAS - SAÍDA (WRITE)
           SELECT FINTRANS ASSIGN TO 'FINTRANS.dat'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-FINTRANS.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      *-----------------------------------------------------------------
       FILE SECTION.
      *-----------------------------------------------------------------
      *
      *
       FD  FINSLD
           RECORDING MODE IS F
           RECORD CONTAINS 030 CHARACTERS.

       01  REG-FINSLD PIC X(30).
      *
      *
       FD  FINTRAN
           RECORDING MODE IS F
           RECORD CONTAINS 030 CHARACTERS.

       01  REG-FINTRAN PIC X(30).
      *
      *
       FD  FINSLDS
           RECORDING MODE IS F
           RECORD CONTAINS 030 CHARACTERS.

       01  REG-FINSLDS.
           03 FINSLDS-CHAVE.
              05  FINSLDS-NR-AGEN     PIC  9(004).
              05  FINSLDS-NR-CC       PIC  X(011).
           03  FINSLDS-VL-SLD         PIC  9(009)V99.
           03  FINSLDS-FILLER         PIC  X(003).
      *
      *
       FD  FINTRANS                                                                        FD  FINTRAN
           RECORDING MODE IS F
           RECORD CONTAINS 030 CHARACTERS.
      *
       01  REG-FINTRANS.
           03 FINTRANS-CHAVE.
              05  FINTRANS-NR-AGEN      PIC  9(004).
              05  FINTRANS-NR-CC        PIC  X(011).
           03  FINTRANS-VL-TRAN         PIC  9(009)V99.
      *    'D'=Débito, 'C'=Crédito
           03  FINTRANS-IND-TRAN        PIC  X(001).
           03  FINTRANS-FILLER          PIC  X(002).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      *
       01 WS-REG-FINSLD.
           03 FINSLD-CHAVE.
              05  FINSLD-NR-AGEN      PIC  9(004).
              05  FINSLD-NR-CC        PIC  X(011).
           03  FINSLD-VL-SLD          PIC  9(009)V99.
           03  FINSLD-FILLER          PIC  X(003).

       01 WS-REG-FINTRAN.
           03 FINTRAN-CHAVE.
              05  FINTRAN-NR-AGEN      PIC  9(004).
              05  FINTRAN-NR-CC        PIC  X(011).
           03  FINTRAN-VL-TRAN         PIC  9(009)V99.
      *    'D'=Débito, 'C'=Crédito
           03  FINTRAN-IND-TRAN        PIC  X(001).
           03  FINTRAN-FILLER          PIC  X(002).


       01  WS-AREA-AUXILIAR.
           05  WS-NOM-PROGRAMA        PIC X(008)  VALUE 'BBLPB100'.
           05  WS-NUM-VERSAO          PIC X(008)  VALUE '001'.
      *
           05  WS-FS-FINSLD           PIC X(002)  VALUE '00'.
           05  WS-FS-FINTRAN          PIC X(002)  VALUE '00'.
           05  WS-FS-FINSLDS          PIC X(002)  VALUE '00'.
           05  WS-FS-FINTRANS         PIC X(002)  VALUE '00'.
      *
           05  WS-QTD-LIDOS-SLD       PIC 9(010)  VALUE ZEROS.
           05  WS-QTD-LIDOS-TRAN      PIC 9(010)  VALUE ZEROS.
           05  WS-QTD-GRAV-SLD        PIC 9(010)  VALUE ZEROS.
           05  WS-QTD-GRAV-TRANS      PIC 9(009)  VALUE ZEROS.
      *
           05  WS-MSG                 PIC X(072)  VALUE SPACES.
           05  WS-NOM-PARAGRAFO       PIC X(070)  VALUE SPACES.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

           PERFORM P0000-INICIALIZA
           PERFORM P1000-PROCESSA
           PERFORM P9000-FINALIZA

           GOBACK.

      *-----------------------------------------------------------------
       P0000-INICIALIZA SECTION.
      *-----------------------------------------------------------------

           MOVE 'P0000-INICIAL' TO        WS-NOM-PARAGRAFO
      *
      * Abertura arquivo FINANCEIRO - SALDOS
           OPEN INPUT FINSLD
           IF WS-FS-FINSLD NOT EQUAL '00'
              MOVE SPACES TO WS-MSG
              STRING 'ERRO - OPEN INPUT FINSLD - FILE STATUS = '
                     WS-FS-FINSLD
                     DELIMITED BY SIZE  INTO WS-MSG
              END-STRING
              PERFORM P8000-ERRO
           END-IF
      *
      * Abertura arquivo FINANCEIRO - TRANSACOES (Débito/Crédito)
           OPEN INPUT FINTRAN
           IF WS-FS-FINTRAN NOT EQUAL '00'
              STRING 'ERRO - OPEN INPUT FINTRAN - FILE STATUS = '
                     WS-FS-FINTRAN
                     DELIMITED BY SIZE  INTO WS-MSG
              END-STRING
              PERFORM P8000-ERRO
           END-IF
      *
      * Abertura - arquivo FINANCEIRO - SALDOS ATUALIZADOS
           OPEN OUTPUT FINSLDS
           IF WS-FS-FINSLDS NOT EQUAL '00'
              STRING 'ERRO - OPEN OUTPUT FINSLDS - FILE STATUS: '
                     WS-FS-FINSLDS
                     DELIMITED BY SIZE  INTO WS-MSG
              END-STRING
              PERFORM P8000-ERRO
           END-IF

      * Abertura arquivo FINANCEIRO - TRANSACOES REJEITADAS
           OPEN OUTPUT FINTRANS
           IF WS-FS-FINTRANS NOT EQUAL '00'
              STRING 'ERRO - OPEN OUTPUT FINTRANS - FILE STATUS = '
                     WS-FS-FINTRANS
                     DELIMITED BY SIZE  INTO WS-MSG
              END-STRING
              PERFORM P8000-ERRO
           END-IF
      *
      *
           .


      *-----------------------------------------------------------------
       P1000-PROCESSA SECTION.
      *-----------------------------------------------------------------

           MOVE 'P1000-PRINCIPAL      ' TO  WS-NOM-PARAGRAFO


           PERFORM P2000-LER-FINSLD

           PERFORM P3000-LER-FINTRAN

           PERFORM UNTIL FINSLD-CHAVE  EQUAL '999999999999999'
                     AND FINTRAN-CHAVE EQUAL '999999999999999'
              EVALUATE TRUE
      *          Chaves iguais: Calcular saldo, e ler proxima transacao
                 WHEN FINSLD-CHAVE    EQUAL     FINTRAN-CHAVE
                      PERFORM P3500-CALCULA-SALDO
                      PERFORM P3000-LER-FINTRAN
      *          Chave SALDO < Chave TRANSAÇÃO:
      *          - Significa que não há mais transações para a conta;
      *          - Grava-se o arquivo de SALDOS, atualizado ou não.
                 WHEN FINSLD-CHAVE LESS THAN    FINTRAN-CHAVE
                      PERFORM P4000-GRAVA-FINSLDS
                      PERFORM P2000-LER-FINSLD
      *          Chave TRANSAÇÃO > Chave SALDO:
      *          - Significa que chegou transação sem Conta SALDO
      *            correspondente.
      *          - Neste caso deve-se gravar a transação em um arquivo
      *            de TRANSAÇÕES REJEITADAS para análise posterior.
                 WHEN FINSLD-CHAVE GREATER THAN FINTRAN-CHAVE
                      PERFORM P4500-GRAVA-FINTRANS
                      PERFORM P3000-LER-FINTRAN
              END-EVALUATE
           END-PERFORM



           .


      *-----------------------------------------------------------------
       P2000-LER-FINSLD SECTION.
      *-----------------------------------------------------------------

           MOVE 'P2000-LER-FINSLD' TO WS-NOM-PARAGRAFO

           READ FINSLD INTO WS-REG-FINSLD
              AT END
                MOVE '999999999999999' TO FINSLD-CHAVE
              NOT AT END

                IF WS-FS-FINSLD NOT EQUAL '00' AND '10'
                   MOVE SPACES TO WS-MSG
                   STRING 'ERRO - READ FINSLD - FILE STATUS = '
                           WS-FS-FINSLD
                           DELIMITED BY SIZE  INTO WS-MSG
                   END-STRING
                   PERFORM P8000-ERRO
                END-IF
                IF WS-FS-FINSLD EQUAL '00'
                   ADD 1 TO WS-QTD-LIDOS-SLD
                END-IF
           END-READ

           .



      *-----------------------------------------------------------------
       P3000-LER-FINTRAN SECTION.
      *-----------------------------------------------------------------
           MOVE 'P3000-LER-FINTRAN' TO WS-NOM-PARAGRAFO
           READ FINTRAN INTO WS-REG-FINTRAN
           AT END
                MOVE '999999999999999' TO FINTRAN-CHAVE
              NOT AT END
                IF WS-FS-FINTRAN NOT EQUAL '00' AND '10'
                   MOVE SPACES TO WS-MSG
                   STRING 'ERRO - READ FINTRAN - FILE STATUS: '
                           WS-FS-FINTRAN
                           DELIMITED BY SIZE  INTO WS-MSG
                   END-STRING
                   PERFORM P8000-ERRO
                END-IF
                IF WS-FS-FINTRAN EQUAL '00'
                   ADD 1 TO WS-QTD-LIDOS-TRAN
                END-IF
           END-READ


           .


      *
      *-----------------------------------------------------------------
       P3500-CALCULA-SALDO SECTION.
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN FINTRAN-IND-TRAN EQUAL 'D'
                    SUBTRACT FINTRAN-VL-TRAN FROM FINSLD-VL-SLD
               WHEN FINTRAN-IND-TRAN EQUAL 'C'
      ************* ADD      FINTRAN-VL-TRAN TO   FINSLD-VL-SLD
                    COMPUTE  FINSLD-VL-SLD = FINSLD-VL-SLD +
                                             FINTRAN-VL-TRAN
                         ON SIZE ERROR
                            MOVE SPACES TO WS-MSG
                            STRING 'ERRO - READ FINTRAN - FILE STATUS: '
                                    WS-FS-FINTRAN
                                    DELIMITED BY SIZE  INTO WS-MSG
                            END-STRING
                            PERFORM P8000-ERRO
                    END-COMPUTE
               WHEN OTHER
                    PERFORM P4500-GRAVA-FINTRANS
           END-EVALUATE

           .

      *-----------------------------------------------------------------
       P4000-GRAVA-FINSLDS SECTION.
      *-----------------------------------------------------------------

           MOVE 'P4000-GRAVA-FINSLDS' TO WS-NOM-PARAGRAFO

           MOVE WS-REG-FINSLD TO REG-FINSLDS
      *
           WRITE REG-FINSLDS    END-WRITE

           IF WS-FS-FINSLDS NOT EQUAL '00'
              MOVE SPACES TO WS-MSG
              STRING 'ERRO GRAVACAO ARQUIVO FINSLDS FILE STATUS: '
                      WS-FS-FINSLDS
                      DELIMITED BY SIZE  INTO WS-MSG
              END-STRING
              PERFORM P8000-ERRO
           END-IF

           ADD 1 TO WS-QTD-GRAV-SLD
      *
           .
      *


      *
      *-----------------------------------------------------------------
       P4500-GRAVA-FINTRANS SECTION.
      * Grava transacoes rejeitadas
      *-----------------------------------------------------------------
           MOVE 'P4500-GRAVA-FINTRANS' TO WS-NOM-PARAGRAFO

           MOVE WS-REG-FINTRAN TO REG-FINTRANS
      *
           WRITE REG-FINTRANS     END-WRITE

           IF WS-FS-FINTRANS NOT EQUAL '00'
              MOVE SPACES TO WS-MSG
              STRING 'ERRO - WRITE FINTRANS - FILE STATUS: '
                      WS-FS-FINTRANS
                      DELIMITED BY SIZE  INTO WS-MSG
              END-STRING
              PERFORM P8000-ERRO
           END-IF
      *
           ADD 1 TO WS-QTD-GRAV-TRANS
      *
           .
      *


      *
      *-----------------------------------------------------------------
       P8000-ERRO SECTION.
      *-----------------------------------------------------------------

           DISPLAY '***************************************************'
           DISPLAY 'PROGRAMA FINPB100 - TERMINO COM ERRO'
           DISPLAY '***************************************************'
           DISPLAY 'VERSAO      : ' WS-NUM-VERSAO
           DISPLAY 'PARAGRAFO   : ' WS-NOM-PARAGRAFO
           DISPLAY 'MENSAGEM    : ' WS-MSG
           DISPLAY '***************************************************'
           MOVE 99 TO RETURN-CODE
      *
           GOBACK.
      *

      *-----------------------------------------------------------------
       P9000-FINALIZA SECTION.
      *-----------------------------------------------------------------
      *
           CLOSE FINSLD
                 FINTRAN
                 FINSLDS
                 FINTRANS

           DISPLAY '***************************************************'
           DISPLAY 'PROGRAMA FINPB100 - TERMINO NORMAL'
           DISPLAY '***************************************************'
           DISPLAY 'QUANTIDADE LIDOS FINSLD      - ' WS-QTD-LIDOS-SLD
           DISPLAY 'QIAMTODADE LIDOS FINTRAN     - ' WS-QTD-LIDOS-TRAN
           DISPLAY 'QUANTIDADE GRAVADOS FINSLDS  - ' WS-QTD-GRAV-SLD
           DISPLAY 'QUANTIDADE GRAVADOS FINTRANS - ' WS-QTD-GRAV-TRANS
           DISPLAY '***************************************************'


           .
