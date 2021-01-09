       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ENTRADA-DE-DADOS.
       AUTHOR.  cursodecobol.com.br.
      * Demonstra uso de ACCEPT e DISPLAY.
      * - Recebe entrada de dados (ACCEPT) de um registro de Aluno.
      * - Exibe (DISPLAY) dados separados.
      * - Demonstra que a mesma instrução ACCEPT consegue objet a data
      *   e a hora do sistema.
      *    . O formato YYYYMMDD determina que a data tenha o formato
      *      correto de ano com 4 dígitos, mes com 2 dígitos e dia
      *      com 2 dias.
      *    . A omissão desse formato faz com que a data obtida tenha
      *      um ano com 2 dígitos, o que pode causar problemas relativos
      *      ao 'Bug do milênio'.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 REGISTRO-ESTUDANTE.
          03  MATRICULA       PIC 9(05).
          03  NOME-SOBRENOME  PIC X(20).
          03  FILLER REDEFINES NOME-SOBRENOME.
              05 NOME         PIC X(10).
              05 SOBRENOME    PIC X(10).
          03  COD-CURSO       PIC X(04).
          03  SEXO            PIC X.

      * YYYYMMDD
       01 DATA-DO-DIA.
          02  DATA-ANO        PIC 9(04).
          02  DATA-MES        PIC 99.
          02  DATA-DIA        PIC 99.

      * YYYYDDD
       01 DIA-DO-ANO.
          02  FILLER          PIC 9(4).
          02  ANO-DIA         PIC 9(3).


      * HHMMSSss   s = S/100
       01 HORA-ATUAL.
          02  HORA-ATUAL-HH     PIC 99.
          02  HORA-ATUAL-MM   PIC 99.
          02  FILLER          PIC 9(4).


       PROCEDURE DIVISION.
       000000-INICIO.
           DISPLAY 'PROGRAMA: ENTRADA-DE-DADOS - INICIO'
           DISPLAY 'Informe os dados do estudante seguindo o template:'.
           DISPLAY '(M)ATRICULA/(N)OME/(S)OBRENOME/(C)OD.CURSO/SE(X)O'
           DISPLAY 'MMMMMNNNNNNNNNNSSSSSSSSSSCCCCX'.
           ACCEPT  REGISTRO-ESTUDANTE.
           ACCEPT  DATA-DO-DIA FROM DATE YYYYMMDD.
           ACCEPT  DIA-DO-ANO  FROM DAY YYYYDDD.
           ACCEPT  HORA-ATUAL  FROM TIME.
           DISPLAY SPACE ' '
           DISPLAY '*********'
           DISPLAY 'RELATORIO'
           DISPLAY '*********'
           DISPLAY 'NOME ......: ' NOME SPACE SOBRENOME.
           DISPLAY 'DATA ......: ' DATA-DIA '/' DATA-MES '/' DATA-ANO.
           DISPLAY 'DIA DO ANO : ' ANO-DIA ' (tambem conhecido como data
      -    ' Juliana'.
           DISPLAY 'HORA ATUAL : ' HORA-ATUAL-HH ':' HORA-ATUAL-MM.
           STOP RUN.
