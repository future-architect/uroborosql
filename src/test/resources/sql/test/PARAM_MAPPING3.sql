/**
 * jp.co.future.uroborosql.parameter.ParameterTest
 */
SELECT
	*
FROM
	(
		SELECT
			'1'	AS	TARGET_STR
		UNION ALL
		SELECT
			'2'	AS	TARGET_STR
		UNION ALL
		SELECT
			'3'	AS	TARGET_STR
		UNION ALL
		SELECT
			'4'	AS	TARGET_STR
		UNION ALL
		SELECT
			'5'	AS	TARGET_STR
	)	TBL
WHERE
	TBL.TARGET_STR	IN	/*targetStrs*/('', '')
ORDER BY
	TBL.TARGET_STR