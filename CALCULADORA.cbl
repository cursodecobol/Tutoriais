      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID.  CALCULADORA.
       AUTHOR.  cursodecobol.com.br.
       DATE-WRITTEN. 10/01/2021,
      * Neste programa serao demonstradas as 4 operações matemáticas
      * basicas:
      * - Instrução ADD     : adição
      * - Instrução SUBTRACT: subtração
      * - Instrução MULTIPLY: multiplicação
      * - Instrução DIVIDE  : divisão
      *
      * - Instrução COMPUTE: outra forma de realizar operações básicas.
      *
      * - Nivel 88: teste booleano (verdadeiro ou falso) relacionado
      *             à avaliação do conteúdo da variável.
      *
      * - Estrutura de repetição: PERFORM UNTIL - executa um laco ate
      *   condicao de encerramento = VERDADEIRA.
      *
      * - Além das instruções já conhecidas ACCEPT e DISPLAY.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *=================================================================
       WORKING-STORAGE                 SECTION.
      *=================================================================
       01  WS-NUMERO1                          PIC 9999  VALUE ZEROS.
           88 ENCERRAR-PROGRAMA                VALUE 9999.

       01  WS-NUMERO2                          PIC 9(4)  VALUE ZEROS.
       01  WS-SINAL                            PIC X     VALUE SPACES.
           88 SINAL-ADICAO                     VALUE '+'.
           88 SINAL-SUBTRACAO                  VALUE '-'.
           88 SINAL-MULTIPLICACAO              VALUE '*'.
           88 SINAL-DIVISAO                    VALUE '/'.
           88 SINAL-VALIDO                     VALUE '+' '-' '*' '/'.
       01  WS-FORMA                            PIC 9     VALUE 1.
           88 FORMA-VERBAL                               VALUE 1.
           88 FORMA-COMPUTE                              VALUE 2.
           88 FORMA-VALIDA                               VALUE 1 2.
       01  WS-RESULTADO-COM-SINAL              PIC S9(4)  VALUE +0.
       01  WS-RESULTADO-DISPLAY                PIC ----9.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           DISPLAY 'PROGRAMA: CALCULADORA  *** INICIO ***'

           PERFORM 1000-CALCULADORA
                   UNTIL  ENCERRAR-PROGRAMA.


           DISPLAY 'PROGRAMA: CALCULADORA  *** ENCERRAMENTO ***'

           GOBACK

           .

      ******************
       1000-CALCULADORA.
      * Recomendação: um PARÁGRAFO deve conter somente um ponto ao final
      ******************
      *    Este é um DISPLAY com continuação na linha seguinte. Observe
      *    o traço na coluna 7. A linha quebrada abre aspas mas
      *    não fecha aspas. A linha de continuação abre aspas.
      *    A linha final abre e fecha aspas.
           DISPLAY 'Demonstra as 4 operacoes basicas no COBOL, efetuadas
      -    'tanto na forma VERBAL - ADD, SUBTRACT, DIVIDE, MULTIPLY - qu
      -    'anto utilizando a instrucao COMPUTE.'
           DISPLAY SPACE
           DISPLAY 'Digite cada numero com o maximo de 4 digitos.'
           DISPLAY '(Para encerrar digite 9999 no primeiro numero.)'

           DISPLAY ' Primeiro numero : '   WITH NO ADVANCING
           ACCEPT WS-NUMERO1

      *    Caso o usuario digitar 9999, encerramos aqui
           IF WS-NUMERO1 NOT EQUAL 9999
              PERFORM 1100-CALCULADORA-PARTE2
           END-IF

           .

      *************************
       1100-CALCULADORA-PARTE2.
      * Continua calculos
      *************************


           DISPLAY ' Segundo numero  : '   WITH NO ADVANCING
           ACCEPT WS-NUMERO2

           DISPLAY ' Operacao: (+) adicao, (-) subtracao, (*) multiplica
      -    'cao, (/) divisao'
           ACCEPT WS-SINAL



           EVALUATE WS-SINAL
               WHEN SINAL-VALIDO
                    PERFORM 1200-ESCOLHER-FORMA
               WHEN OTHER
                    DISPLAY 'SINAL INVALIDO!!'
           END-EVALUATE
           MULTIPLY WS-NUMERO BY WS-MULTIPLICADOR GIVING Result.



           DISPLAY "Result is = ", Result.
