/* Generated by           cobc 3.1.2.0 */
/* Generated from         c:\desenv\COBOL\tutorial\MINIMAL.cbl */
/* Generated at           nov 24 2022 22:00:52 */
/* GnuCOBOL build date    Aug 31 2022 20:58:34 */
/* GnuCOBOL package date  Dec 23 2020 12:04:58 UTC */
/* Compile command        c:\gc312vbi\bin\cobc -x -std=ibm -Wall -I$(workspaceFolder} -IC:\desenv\COBOL\tutorial\CopyBooks -IC:\desenv\COBOL\tutorial\CopyBooks\Public -tMINIMAL.LST -v -g -Xref -ftsymbols c:\desenv\COBOL\tutorial\MINIMAL.cbl */

/* Program local variables for 'MINIMAL' */

/* Module initialization indicator */
static unsigned int	initialized = 0;

/* Module structure pointer */
static cob_module	*module = NULL;

/* Global variable pointer */
cob_global		*cob_glob_ptr;


/* Call pointers */
static cob_call_union	call_SRMINIMAL;

/* Call parameters */
cob_field		*cob_procedure_params[1];

/* Perform frame stack */
struct cob_frame	*temp_index;
struct cob_frame	*frame_overflow;
struct cob_frame	*frame_ptr;
struct cob_frame	frame_stack[255];


/* Data storage */
static int	b_2;	/* RETURN-CODE */
static cob_u8_t	b_8[12] __attribute__((aligned));	/* WK-PARM */

/* End of local data storage */


/* Fields (local) */
static cob_field f_8	= {12, b_8, &a_2};	/* WK-PARM */

/* End of fields */

