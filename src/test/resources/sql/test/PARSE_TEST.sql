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
/*END*/
/*END*/
/*END*/

