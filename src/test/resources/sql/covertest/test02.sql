select
	*
from test t
where 1 = 1
/*IF SF.isNotEmpty(id) */
	/*IF id < 100 */
and	t.id = /*id*/0
	/*ELSE*/
and	t.id = 100
	/*END*/
/*END*/
/*IF SF.isNotEmpty(name) */
and	t.name = /*name*/''
/*END*/
order by t.id