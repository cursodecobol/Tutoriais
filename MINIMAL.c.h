/* Generated by           cobc 3.1.2.0 */
/* Generated from         c:\desenv\COBOL\tutorial\MINIMAL.cbl */
/* Generated at           nov 24 2022 22:00:52 */
/* GnuCOBOL build date    Aug 31 2022 20:58:34 */
/* GnuCOBOL package date  Dec 23 2020 12:04:58 UTC */
/* Compile command        c:\gc312vbi\bin\cobc -x -std=ibm -Wall -I$(workspaceFolder} -IC:\desenv\COBOL\tutorial\CopyBooks -IC:\desenv\COBOL\tutorial\CopyBooks\Public -tMINIMAL.LST -v -g -Xref -ftsymbols c:\desenv\COBOL\tutorial\MINIMAL.cbl */


/* Module path */
static const char		*cob_module_path = NULL;

/* Number of call parameters */
static int		cob_call_params = 0;

/* Attributes */

static const cob_field_attr a_1 =	{0x21,   0,   0, 0x1000, NULL};
static const cob_field_attr a_2 =	{0x01,   0,   0, 0x0000, NULL};


/* Constants */
static const cob_field c_1	= {43, (cob_u8_ptr)"A MINIMAL PROGRAM WRITTEN IN COBOL LANGUAGE", &a_1};
static const cob_field c_2	= {17, (cob_u8_ptr)"MINIMAL.cbl - FIM", &a_1};


/* Strings */
static const char st_1[]	= "DISPLAY";
static const char st_2[]	= "MOVE";
static const char st_3[]	= "CALL";


/* Source file names */
static const char *st_source_files[]	= { "" 
		,"c:\\desenv\\COBOL\\tutorial\\MINIMAL.cbl"};

static COB_INLINE COB_A_INLINE void
cob_setswp_s16 (void *p, const int val)
{
	short		n;
	n = val;
	*(short *)p = COB_BSWAP_16(n);
}

