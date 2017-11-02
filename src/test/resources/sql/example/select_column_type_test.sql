SELECT
	CTT.COL_VARCHAR
,	CTT.COL_CHAR
,	CTT.COL_NUMERIC
,	CTT.COL_BOOLEAN
,	CTT.COL_TIMESTAMP
,	CTT.COL_DATE
,	CTT.COL_TIME
FROM	COLUMN_TYPE_TEST	CTT
WHERE 1 = 1
/*IF SF.isNotEmpty(col_varchar) */
AND	CTT.COL_VARCHAR = /*col_varchar*/'varchar'
/*END*/
/*IF SF.isNotEmpty(col_char) */
AND	CTT.COL_CHAR = /*col_char*/'char'
/*END*/
/*IF col_numeric != null */
AND	CTT.COL_NUMERIC = /*col_numeric*/123.45
/*END*/
/*IF col_boolean != null */
AND	CTT.COL_BOOLEAN = /*col_boolean*/True
/*END*/
/*IF col_timestamp != null */
AND	CTT.COL_TIMESTAMP = /*col_timestamp*/'2017-01-01T01:23:45'
/*END*/
/*IF col_date != null */
AND	CTT.COL_DATE = /*col_date*/'2017-01-01'
/*END*/
/*IF col_time != null */
AND	CTT.COL_TIME = /*col_time*/'01:23:45'
/*END*/
