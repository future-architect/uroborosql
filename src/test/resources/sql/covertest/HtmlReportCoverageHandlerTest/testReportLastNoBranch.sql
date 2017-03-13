SELECT
    *
FROM
    TEST    T
/*IF SF.isNotEmpty(name) */
AND T.NAME  =   /*name*/''
/*END*/