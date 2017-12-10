package jp.co.future.uroborosql.context;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

public class SqlContextImplTest {
	private static SqlConfig config = null;

	@BeforeClass
	public static void setUpClass() {
		config = UroboroSQL.builder("jdbc:h2:mem:SqlContextImplTest", "sa", "").build();
	}

	private SqlContext getSqlContext(String sql) {
		SqlContext ctx = config.contextWith(sql);
		ctx.addSqlPart(sql);
		return ctx;
	}

	private String replaceLineSep(String sql) {
		return sql.replaceAll("\\[LF\\]", System.lineSeparator());
	}

	@Test
	public void removeFirstAndKeyWorkWhenWhereClause() throws Exception {
		SqlContext ctx = getSqlContext("select * from test where\r\n\r\n\r\n and aaa = 1");
		assertEquals("select * from test where\r\n aaa = 1", ctx.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenSelectClause() throws Exception {
		SqlContext ctx1 = getSqlContext("select ,aaa,bbb,ccc from test");
		assertEquals("select aaa,bbb,ccc from test", ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("select , aaa, bbb, ccc from test");
		assertEquals("select  aaa, bbb, ccc from test", ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext( replaceLineSep("select[LF], aaa[LF], bbb[LF], ccc from test"));
		assertEquals(replaceLineSep("select[LF] aaa[LF], bbb[LF], ccc from test"), ctx3.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenOrderByClause() throws Exception {
		SqlContext ctx1 = getSqlContext("select * from test order by ,aaa, bbb");
		assertEquals("select * from test order by aaa, bbb", ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("select * from test order by , aaa, bbb");
		assertEquals("select * from test order by  aaa, bbb", ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext( replaceLineSep("select * from test order by[LF], aaa, bbb"));
		assertEquals(replaceLineSep("select * from test order by[LF] aaa, bbb"), ctx3.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenGroupByClause() throws Exception {
		SqlContext ctx1 = getSqlContext("select * from test group by ,aaa, bbb");
		assertEquals("select * from test group by aaa, bbb", ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("select * from test group by , aaa, bbb");
		assertEquals("select * from test group by  aaa, bbb", ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext( replaceLineSep("select * from test group by[LF], aaa, bbb"));
		assertEquals(replaceLineSep("select * from test group by[LF] aaa, bbb"), ctx3.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenStartBracket() throws Exception {
		SqlContext ctx1 = getSqlContext("insert into (,aaa,bbb,ccc) values (,111,222,333)");
		assertEquals("insert into (aaa,bbb,ccc) values (111,222,333)", ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("insert into (, aaa, bbb, ccc) values (,111 ,222 ,333)");
		assertEquals("insert into ( aaa, bbb, ccc) values (111 ,222 ,333)", ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext( replaceLineSep("insert into ([LF], aaa[LF], bbb[LF], ccc[LF]) values (,[LF]111,[LF]222,[LF]333[LF])"));
		assertEquals(replaceLineSep("insert into ([LF] aaa[LF], bbb[LF], ccc[LF]) values ([LF]111,[LF]222,[LF]333[LF])"), ctx3.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenSetClause() throws Exception {
		SqlContext ctx1 = getSqlContext("update test set ,aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals("update test set aaa = 111, bbb = 222, ccc = 333 where 1 = 1", ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("update test set , aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals("update test set  aaa = 111, bbb = 222, ccc = 333 where 1 = 1", ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext(replaceLineSep("update test set[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1"));
		assertEquals(replaceLineSep("update test set[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"), ctx3.getExecutableSql());
	}
}