SELECT
	*
FROM /*$TABLE_NAME*/ TST
/*BEGIN*/
WHERE
/*IF param11 != null*/
AND TST.PARAM1 = /*param11*/''
/*ELIF param11 == null and param12 != null*/
AND TST.PARAM1 = /*param12*/''
/*ELSE*/
/*IF SF.isNotEmpty(param21)*/
AND TST.PARAM2 = /*param21*/()
/*ELIF SF.isEmpty(param21) and SF.isNotEmpty(param22) or SF.isNotEmpty(param23)*/
AND TST.PARAM2 = /*param22*/()
AND TST.CONST_PARAM_STR = /*#CLS_STRING*/''
AND TST.CONST_PARAM_LONG = /*$CLS_LONG*/0
AND TST.CONST_PARAM_BYTES = /*$CLS_SQL_TIMESTAMP*/3
AND TST.CONST_ENUM_1 = /*$CLS_TEST_ENUM1_A*/'A'
AND TST.CONST_ENUM_2 = /*$CLS_TEST_ENUM1_D*/'D'
/*END*/
/*END*/
/*END*/

