SELECT
    *
FROM
    TEST    T
WHERE
    1       =   1
/*IF SF.isNotEmpty(id) */
    /*IF id < 100 */
AND T.ID    =   /*id*/0
    /*ELSE*/
AND T.ID    =   100
    /*END*/
/*END*/
/*IF SF.isNotEmpty(name) */
AND T.NAME  =   /*name*/''
/*END*/
ORDER BY
    T.ID