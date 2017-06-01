SELECT /* _SQL_ID_ */
	*
FROM
	PRODUCT
WHERE 1 = 1
/*IF product_id != null */
AND	PRODUCT_ID	IN	/*product_id*/(0, 2)
/*END*/
ORDER BY PRODUCT_ID
