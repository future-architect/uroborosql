package jp.co.future.uroborosql.context;

import static org.junit.Assert.*;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;

import org.junit.BeforeClass;
import org.junit.Test;

public class SqlContextImplTest {
	private static SqlConfig config = null;

	@BeforeClass
	public static void setUpClass() {
		config = UroboroSQL.builder("jdbc:h2:mem:SqlContextImplTest", "sa", "").build();
	}

	private SqlContext getSqlContext(final String sql) {
		String s = replaceLineSep(sql);
		SqlContext ctx = config.contextWith(s);
		ctx.addSqlPart(s);
		return ctx;
	}

	private String replaceLineSep(final String sql) {
		return sql.replaceAll("\\[LF\\]", System.lineSeparator());
	}

	@Test
	public void removeFirstAndKeyWordWhenWhereClause() throws Exception {
		SqlContext ctx1 = getSqlContext("select * from test where[LF][LF][LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test where[LF] aaa = 1"), ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("select * from test where[LF]      and aaa = 1");
		assertEquals(replaceLineSep("select * from test where[LF]      aaa = 1"), ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext("select * from test where /* comment */ and aaa = 1");
		assertEquals(replaceLineSep("select * from test where /* comment */ aaa = 1"), ctx3.getExecutableSql());

		SqlContext ctx4 = getSqlContext("select * from test where -- /* comment */  [LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test where -- /* comment */  [LF] aaa = 1"), ctx4.getExecutableSql());

		SqlContext ctx5 = getSqlContext("select * from test where -- /* comment */  [LF] order = 1");
		assertEquals(replaceLineSep("select * from test where -- /* comment */  [LF] order = 1"),
				ctx5.getExecutableSql());

		SqlContext ctx6 = getSqlContext("select * from test where /* comment */ --comment [LF] order = 1");
		assertEquals(replaceLineSep("select * from test where /* comment */ --comment [LF] order = 1"),
				ctx6.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenSelectClause() throws Exception {
		SqlContext ctx1 = getSqlContext("select ,aaa,bbb,ccc from test");
		assertEquals(replaceLineSep("select aaa,bbb,ccc from test"), ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("select , aaa, bbb, ccc from test");
		assertEquals(replaceLineSep("select  aaa, bbb, ccc from test"), ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext("select[LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep("select[LF] aaa[LF], bbb[LF], ccc from test"), ctx3.getExecutableSql());

		SqlContext ctx4 = getSqlContext("select /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep("select /* comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx4.getExecutableSql());

		SqlContext ctx5 = getSqlContext("select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep("select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx5.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenOrderByClause() throws Exception {
		SqlContext ctx1 = getSqlContext("select * from test order by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by aaa, bbb"), ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("select * from test order by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by  aaa, bbb"), ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext("select * from test order by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by[LF] aaa, bbb"), ctx3.getExecutableSql());

		SqlContext ctx4 = getSqlContext("select * from test order by /* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by /* comment */[LF] aaa, bbb"), ctx4.getExecutableSql());

		SqlContext ctx5 = getSqlContext("select * from test order by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by --/* comment */[LF] aaa, bbb"),
				ctx5.getExecutableSql());

		SqlContext ctx6 = getSqlContext("select * from test order     by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order     by --/* comment */[LF] aaa, bbb"),
				ctx6.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenGroupByClause() throws Exception {
		SqlContext ctx1 = getSqlContext("select * from test group by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by aaa, bbb"), ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("select * from test group by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by  aaa, bbb"), ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext("select * from test group by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by[LF] aaa, bbb"), ctx3.getExecutableSql());

		SqlContext ctx4 = getSqlContext("select * from test group by /* comment */  [LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by /* comment */  [LF] aaa, bbb"),
				ctx4.getExecutableSql());

		SqlContext ctx5 = getSqlContext("select * from test group by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by /* comment */ --aaa[LF] aaa, bbb"),
				ctx5.getExecutableSql());

		SqlContext ctx6 = getSqlContext("select * from test group     by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group     by /* comment */ --aaa[LF] aaa, bbb"),
				ctx6.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenStartBracket() throws Exception {
		SqlContext ctx1 = getSqlContext("insert into (,aaa,bbb,ccc) values (,111,222,333)");
		assertEquals(replaceLineSep("insert into (aaa,bbb,ccc) values (111,222,333)"), ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("insert into (, aaa, bbb, ccc) values (,111 ,222 ,333)");
		assertEquals(replaceLineSep("insert into ( aaa, bbb, ccc) values (111 ,222 ,333)"), ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext("insert into ([LF], aaa[LF], bbb[LF], ccc[LF]) values (,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep("insert into ([LF] aaa[LF], bbb[LF], ccc[LF]) values ([LF]111,[LF]222,[LF]333[LF])"),
				ctx3.getExecutableSql());

		SqlContext ctx4 = getSqlContext("insert into ([LF]/* comment */, aaa[LF], bbb[LF], ccc[LF]) values (/* comment */,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep("insert into ([LF]/* comment */ aaa[LF], bbb[LF], ccc[LF]) values (/* comment */[LF]111,[LF]222,[LF]333[LF])"),
				ctx4.getExecutableSql());

		SqlContext ctx5 = getSqlContext("insert into (--comment[LF], aaa[LF], bbb[LF], ccc[LF]) values (,/*comment*/[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep("insert into (--comment[LF] aaa[LF], bbb[LF], ccc[LF]) values (/*comment*/[LF]111,[LF]222,[LF]333[LF])"),
				ctx5.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenSetClause() throws Exception {
		SqlContext ctx1 = getSqlContext("update test set ,aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx1.getExecutableSql());

		SqlContext ctx2 = getSqlContext("update test set , aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set  aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx2.getExecutableSql());

		SqlContext ctx3 = getSqlContext("update test set[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx3.getExecutableSql());

		SqlContext ctx4 = getSqlContext("update test set /* comment */[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set /* comment */[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx4.getExecutableSql());

		SqlContext ctx5 = getSqlContext("update test set --comment[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set --comment[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx5.getExecutableSql());
	}
}