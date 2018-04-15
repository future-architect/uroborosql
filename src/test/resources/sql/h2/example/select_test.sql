select -- H2DB file
	*
from test t
where 1 = 1
/*IF SF.isNotEmpty(id) */
and	t.id = /*id*/''
/*END*/
/*IF SF.isNotEmpty(name) */
and	t.name = /*name*/''
/*END*/
order by t.id, t.name