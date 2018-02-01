SELECT /* _SQL_ID_ */
	*
FROM
	PRODUCT
WHERE 1 = 1
/*IF upd_datetime != null */
AND	UPD_DATETIME	=	/*upd_datetime*/''
/*END*/
ORDER BY PRODUCT_ID
