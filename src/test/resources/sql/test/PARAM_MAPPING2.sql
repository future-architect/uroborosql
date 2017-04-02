/**
 * jp.co.future.uroborosql.parameter.ParameterTest
 */
SELECT
	*
FROM
	(
		SELECT
			'123'	AS	TARGET_STR
		UNION ALL
		SELECT
			'456'	AS	TARGET_STR
		UNION ALL
		SELECT
			'789'	AS	TARGET_STR
	)	TBL
WHERE
	TBL.TARGET_STR	=	/*targetStr*/''