SELECT
	*
FROM
	T_WORK	T
WHERE
	1	=	1
AND	T.ID								= 'X'									-- ＩＤ
/*IF "1" == param1 */
AND T.CD								= T.NCD									-- コード
AND T.CD2								= '0'									-- コード２
/*ELSE*/
AND T.CD3								= T.NCD									-- コード３
AND T.CD4								= '0'									-- コード４
/*END*/
