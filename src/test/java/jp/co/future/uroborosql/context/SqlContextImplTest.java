package jp.co.future.uroborosql.context;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.sql.JDBCType;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.parameter.ReaderParameter;
import jp.co.future.uroborosql.parameter.StreamParameter;
import jp.co.future.uroborosql.parser.ContextTransformer;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;

public class SqlContextImplTest {
	private static SqlConfig config = null;

	@BeforeAll
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
		SqlContext ctx11 = getSqlContext("select * from test where[LF][LF][LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test where[LF] aaa = 1"),
				ctx11.getExecutableSql());
		SqlContext ctx12 = getSqlContext("select * from test[LF]where[LF][LF][LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where[LF] aaa = 1"),
				ctx12.getExecutableSql());

		SqlContext ctx21 = getSqlContext("select * from test where[LF]      and aaa = 1");
		assertEquals(replaceLineSep("select * from test where[LF]      aaa = 1"),
				ctx21.getExecutableSql());
		SqlContext ctx22 = getSqlContext("select * from test[LF]where[LF]      and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where[LF]      aaa = 1"),
				ctx22.getExecutableSql());

		SqlContext ctx31 = getSqlContext("select * from test where /* comment */ and aaa = 1");
		assertEquals(replaceLineSep("select * from test where /* comment */ aaa = 1"),
				ctx31.getExecutableSql());
		SqlContext ctx32 = getSqlContext("select * from test[LF]where /* comment */ and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where /* comment */ aaa = 1"),
				ctx32.getExecutableSql());

		SqlContext ctx41 = getSqlContext("select * from test where -- /* comment */  [LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test where -- /* comment */  [LF] aaa = 1"),
				ctx41.getExecutableSql());
		SqlContext ctx42 = getSqlContext("select * from test[LF]where -- /* comment */  [LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where -- /* comment */  [LF] aaa = 1"),
				ctx42.getExecutableSql());

		SqlContext ctx51 = getSqlContext("select * from test where -- /* comment */  [LF] order = 1");
		assertEquals(replaceLineSep("select * from test where -- /* comment */  [LF] order = 1"),
				ctx51.getExecutableSql());
		SqlContext ctx52 = getSqlContext("select * from test[LF]where -- /* comment */  [LF] order = 1");
		assertEquals(replaceLineSep("select * from test[LF]where -- /* comment */  [LF] order = 1"),
				ctx52.getExecutableSql());

		SqlContext ctx61 = getSqlContext("select * from test where /* comment */ --comment [LF] order = 1");
		assertEquals(replaceLineSep("select * from test where /* comment */ --comment [LF] order = 1"),
				ctx61.getExecutableSql());
		SqlContext ctx62 = getSqlContext("select * from test[LF]where /* comment */ --comment [LF] order = 1");
		assertEquals(replaceLineSep("select * from test[LF]where /* comment */ --comment [LF] order = 1"),
				ctx62.getExecutableSql());
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

		SqlContext ctx6 = getSqlContext(
				"with dummy as ( select * from dummy ) select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep(
				"with dummy as ( select * from dummy ) select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx6.getExecutableSql());

		SqlContext ctx7 = getSqlContext(
				"with dummy as ( select * from dummy )[LF]select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep(
				"with dummy as ( select * from dummy )[LF]select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx7.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenOrderByClause() throws Exception {
		SqlContext ctx11 = getSqlContext("select * from test order by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by aaa, bbb"), ctx11.getExecutableSql());
		SqlContext ctx12 = getSqlContext("select * from test[LF]order by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by aaa, bbb"), ctx12.getExecutableSql());

		SqlContext ctx21 = getSqlContext("select * from test order by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by  aaa, bbb"), ctx21.getExecutableSql());
		SqlContext ctx22 = getSqlContext("select * from test[LF]order by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by  aaa, bbb"), ctx22.getExecutableSql());

		SqlContext ctx31 = getSqlContext("select * from test order by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by[LF] aaa, bbb"), ctx31.getExecutableSql());
		SqlContext ctx32 = getSqlContext("select * from test[LF]order by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by[LF] aaa, bbb"), ctx32.getExecutableSql());

		SqlContext ctx41 = getSqlContext("select * from test order by /* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by /* comment */[LF] aaa, bbb"),
				ctx41.getExecutableSql());
		SqlContext ctx42 = getSqlContext("select * from test[LF]order by /* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by /* comment */[LF] aaa, bbb"),
				ctx42.getExecutableSql());

		SqlContext ctx51 = getSqlContext("select * from test order by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by --/* comment */[LF] aaa, bbb"),
				ctx51.getExecutableSql());
		SqlContext ctx52 = getSqlContext("select * from test[LF]order by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by --/* comment */[LF] aaa, bbb"),
				ctx52.getExecutableSql());

		SqlContext ctx61 = getSqlContext("select * from test order     by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order     by --/* comment */[LF] aaa, bbb"),
				ctx61.getExecutableSql());
		SqlContext ctx62 = getSqlContext("select * from test[LF]order     by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order     by --/* comment */[LF] aaa, bbb"),
				ctx62.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenGroupByClause() throws Exception {
		SqlContext ctx11 = getSqlContext("select * from test group by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by aaa, bbb"), ctx11.getExecutableSql());
		SqlContext ctx12 = getSqlContext("select * from test[LF]group by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by aaa, bbb"), ctx12.getExecutableSql());

		SqlContext ctx21 = getSqlContext("select * from test group by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by  aaa, bbb"), ctx21.getExecutableSql());
		SqlContext ctx22 = getSqlContext("select * from test[LF]group by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by  aaa, bbb"), ctx22.getExecutableSql());

		SqlContext ctx31 = getSqlContext("select * from test group by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by[LF] aaa, bbb"), ctx31.getExecutableSql());
		SqlContext ctx32 = getSqlContext("select * from test[LF]group by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by[LF] aaa, bbb"), ctx32.getExecutableSql());

		SqlContext ctx41 = getSqlContext("select * from test group by /* comment */  [LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by /* comment */  [LF] aaa, bbb"),
				ctx41.getExecutableSql());
		SqlContext ctx42 = getSqlContext("select * from test[LF]group by /* comment */  [LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by /* comment */  [LF] aaa, bbb"),
				ctx42.getExecutableSql());

		SqlContext ctx51 = getSqlContext("select * from test group by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by /* comment */ --aaa[LF] aaa, bbb"),
				ctx51.getExecutableSql());
		SqlContext ctx52 = getSqlContext("select * from test[LF]group by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by /* comment */ --aaa[LF] aaa, bbb"),
				ctx52.getExecutableSql());

		SqlContext ctx61 = getSqlContext("select * from test group     by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group     by /* comment */ --aaa[LF] aaa, bbb"),
				ctx61.getExecutableSql());
		SqlContext ctx62 = getSqlContext("select * from test[LF]group     by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group     by /* comment */ --aaa[LF] aaa, bbb"),
				ctx62.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenStartBracket() throws Exception {
		SqlContext ctx11 = getSqlContext("insert into (,aaa,bbb,ccc) values (,111,222,333)");
		assertEquals(replaceLineSep("insert into (aaa,bbb,ccc) values (111,222,333)"), ctx11.getExecutableSql());
		SqlContext ctx12 = getSqlContext("insert into[LF](,aaa,bbb,ccc) values (,111,222,333)");
		assertEquals(replaceLineSep("insert into[LF](aaa,bbb,ccc) values (111,222,333)"), ctx12.getExecutableSql());

		SqlContext ctx21 = getSqlContext("insert into (, aaa, bbb, ccc) values (,111 ,222 ,333)");
		assertEquals(replaceLineSep("insert into ( aaa, bbb, ccc) values (111 ,222 ,333)"), ctx21.getExecutableSql());
		SqlContext ctx22 = getSqlContext("insert into[LF](, aaa, bbb, ccc) values (,111 ,222 ,333)");
		assertEquals(replaceLineSep("insert into[LF]( aaa, bbb, ccc) values (111 ,222 ,333)"),
				ctx22.getExecutableSql());

		SqlContext ctx31 = getSqlContext(
				"insert into ([LF], aaa[LF], bbb[LF], ccc[LF]) values (,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep("insert into ([LF] aaa[LF], bbb[LF], ccc[LF]) values ([LF]111,[LF]222,[LF]333[LF])"),
				ctx31.getExecutableSql());
		SqlContext ctx32 = getSqlContext(
				"insert into[LF]([LF], aaa[LF], bbb[LF], ccc[LF]) values (,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep("insert into[LF]([LF] aaa[LF], bbb[LF], ccc[LF]) values ([LF]111,[LF]222,[LF]333[LF])"),
				ctx32.getExecutableSql());

		SqlContext ctx41 = getSqlContext(
				"insert into ([LF]/* comment */, aaa[LF], bbb[LF], ccc[LF]) values (/* comment */,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep(
						"insert into ([LF]/* comment */ aaa[LF], bbb[LF], ccc[LF]) values (/* comment */[LF]111,[LF]222,[LF]333[LF])"),
				ctx41.getExecutableSql());
		SqlContext ctx42 = getSqlContext(
				"insert into[LF]([LF]/* comment */, aaa[LF], bbb[LF], ccc[LF]) values (/* comment */,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep(
						"insert into[LF]([LF]/* comment */ aaa[LF], bbb[LF], ccc[LF]) values (/* comment */[LF]111,[LF]222,[LF]333[LF])"),
				ctx42.getExecutableSql());

		SqlContext ctx51 = getSqlContext(
				"insert into (--comment[LF], aaa[LF], bbb[LF], ccc[LF]) values (,/*comment*/[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep(
						"insert into (--comment[LF] aaa[LF], bbb[LF], ccc[LF]) values (/*comment*/[LF]111,[LF]222,[LF]333[LF])"),
				ctx51.getExecutableSql());
		SqlContext ctx52 = getSqlContext(
				"insert into[LF](--comment[LF], aaa[LF], bbb[LF], ccc[LF]) values (,/*comment*/[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep(
						"insert into[LF](--comment[LF] aaa[LF], bbb[LF], ccc[LF]) values (/*comment*/[LF]111,[LF]222,[LF]333[LF])"),
				ctx52.getExecutableSql());
	}

	@Test
	public void removeFirstCommaWhenSetClause() throws Exception {
		SqlContext ctx11 = getSqlContext("update test set ,aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx11.getExecutableSql());
		SqlContext ctx12 = getSqlContext("update test[LF]set ,aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx12.getExecutableSql());

		SqlContext ctx21 = getSqlContext("update test set , aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set  aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx21.getExecutableSql());
		SqlContext ctx22 = getSqlContext("update test[LF]set , aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set  aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx22.getExecutableSql());

		SqlContext ctx31 = getSqlContext("update test set[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx31.getExecutableSql());
		SqlContext ctx32 = getSqlContext("update test[LF]set[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx32.getExecutableSql());

		SqlContext ctx41 = getSqlContext(
				"update test set /* comment */[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set /* comment */[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx41.getExecutableSql());
		SqlContext ctx42 = getSqlContext(
				"update test[LF]set /* comment */[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set /* comment */[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx42.getExecutableSql());

		SqlContext ctx51 = getSqlContext("update test set --comment[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set --comment[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx51.getExecutableSql());
		SqlContext ctx52 = getSqlContext(
				"update test[LF]set --comment[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set --comment[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx52.getExecutableSql());

		SqlContext ctx61 = getSqlContext("select , aaa, code_set, bbb, ccc from test where 1 = 1");
		assertEquals(replaceLineSep("select  aaa, code_set, bbb, ccc from test where 1 = 1"),
				ctx61.getExecutableSql());
		SqlContext ctx62 = getSqlContext("select[LF], aaa[LF], code_set[LF], bbb[LF], ccc[LF]from test[LF]where 1 = 1");
		assertEquals(replaceLineSep("select[LF] aaa[LF], code_set[LF], bbb[LF], ccc[LF]from test[LF]where 1 = 1"),
				ctx62.getExecutableSql());
		SqlContext ctx63 = getSqlContext(
				"select[LF], aaa,[LF]code_set,[LF]bbb,[LF]ccc[LF]from[LF]test[LF]where[LF]1 = 1");
		assertEquals(replaceLineSep("select[LF] aaa,[LF]code_set,[LF]bbb,[LF]ccc[LF]from[LF]test[LF]where[LF]1 = 1"),
				ctx63.getExecutableSql());
	}

	@Test
	public void testHasParam() throws Exception {
		SqlContext ctx = getSqlContext("select * from dummy");
		ctx.param("key1", "value1");
		assertTrue(ctx.hasParam("key1"));
		assertFalse(ctx.hasParam("key2"));
	}

	@SuppressWarnings("deprecation")
	@Test
	public void testParamList() throws Exception {
		SqlContext ctx = null;

		ctx = getSqlContext("select * from dummy");
		ctx.paramList("key1", "value1");
		assertThat(ctx.getParam("key1").getValue(), is(Arrays.asList("value1")));

		ctx = getSqlContext("select * from dummy");
		ctx.paramList("key1", "value1", "value2");
		assertThat(ctx.getParam("key1").getValue(), is(Arrays.asList("value1", "value2")));

		Set<String> values = new HashSet<>();
		values.add("value1");
		values.add("value2");

		ctx = getSqlContext("select * from dummy");
		ctx.param("key1", values);
		assertThat(ctx.getParam("key1").getValue(), is(values));

		ctx = getSqlContext("select * from dummy");
		ctx.paramList("key1", () -> {
			return values;
		});
		assertThat(ctx.getParam("key1").getValue(), is(values));
	}

	@SuppressWarnings("deprecation")
	@Test
	public void testIfAbsent() throws Exception {
		SqlContext ctx = null;

		ctx = getSqlContext("select * from dummy");
		ctx.paramIfAbsent("key1", "value1");
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.paramIfAbsent("key1", "value2");
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getSqlContext("select * from dummy");
		ctx.paramIfAbsent("key1", "value1", JDBCType.VARCHAR);
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.paramIfAbsent("key1", "value2", JDBCType.VARCHAR);
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getSqlContext("select * from dummy");
		ctx.paramIfAbsent("key1", "value1", JDBCType.VARCHAR.getVendorTypeNumber());
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.paramIfAbsent("key1", "value2", JDBCType.VARCHAR.getVendorTypeNumber());
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getSqlContext("select * from dummy");
		ctx.paramListIfAbsent("key1", "value1", "value2");
		assertThat(ctx.getParam("key1").getValue(), is(Arrays.asList("value1", "value2")));
		ctx.paramListIfAbsent("key1", "value11", "value22");
		assertThat(ctx.getParam("key1").getValue(), is(Arrays.asList("value1", "value2")));

		ctx = getSqlContext("select * from dummy");
		ctx.inOutParamIfAbsent("key1", "value1", JDBCType.VARCHAR);
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.inOutParamIfAbsent("key1", "value2", JDBCType.VARCHAR);
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getSqlContext("select * from dummy");
		ctx.inOutParamIfAbsent("key1", "value1", JDBCType.VARCHAR.getVendorTypeNumber());
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.inOutParamIfAbsent("key1", "value2", JDBCType.VARCHAR.getVendorTypeNumber());
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getSqlContext("select * from dummy");
		InputStream is1 = new ByteArrayInputStream("value1".getBytes());
		ctx.blobParamIfAbsent("key1", is1, "value1".length());
		StreamParameter stream1 = (StreamParameter) ctx.getParam("key1");
		assertThat(ctx.getParam("key1").getValue(), is("[BLOB]"));
		InputStream is2 = new ByteArrayInputStream("value2".getBytes());
		ctx.blobParamIfAbsent("key1", is2, "value2".length());
		assertThat(ctx.getParam("key1"), is(stream1));

		ctx = getSqlContext("select * from dummy");
		InputStream is11 = new ByteArrayInputStream("value1".getBytes());
		ctx.blobParamIfAbsent("key1", is11);
		StreamParameter stream11 = (StreamParameter) ctx.getParam("key1");
		assertThat(ctx.getParam("key1").getValue(), is("[BLOB]"));
		InputStream is22 = new ByteArrayInputStream("value2".getBytes());
		ctx.blobParamIfAbsent("key1", is22);
		assertThat(ctx.getParam("key1"), is(stream11));

		ctx = getSqlContext("select * from dummy");
		Reader r1 = new StringReader("value1");
		ctx.clobParamIfAbsent("key1", r1);
		ReaderParameter reader1 = (ReaderParameter) ctx.getParam("key1");
		assertThat(ctx.getParam("key1").getValue(), is("[CLOB]"));
		Reader r2 = new StringReader("value2");
		ctx.clobParamIfAbsent("key1", r2);
		assertThat(ctx.getParam("key1"), is(reader1));

		ctx = getSqlContext("select * from dummy");
		Reader r11 = new StringReader("value1");
		ctx.clobParamIfAbsent("key1", r11, "value1".length());
		ReaderParameter reader11 = (ReaderParameter) ctx.getParam("key1");
		assertThat(ctx.getParam("key1").getValue(), is("[CLOB]"));
		Reader r22 = new StringReader("value2");
		ctx.clobParamIfAbsent("key1", r22, "value1".length());
		assertThat(ctx.getParam("key1"), is(reader11));
	}

	@Test
	public void testParamOptionalHasValue() throws Exception {
		SqlContext ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		Optional<String> id = Optional.of("testId");
		ctx.param("id", id);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.of("testId");
		ctx.param("id", id, JDBCType.VARCHAR);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.of("testId");
		ctx.param("id", id, JDBCType.VARCHAR.getVendorTypeNumber());

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.of("testId");
		ctx.inOutParam("id", id, JDBCType.VARCHAR);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.of("testId");
		ctx.inOutParam("id", id, JDBCType.VARCHAR.getVendorTypeNumber());

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

	}

	@Test
	public void testParamOptionalNullValue() throws Exception {
		SqlContext ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		Optional<String> id = Optional.empty();
		ctx.param("id", id);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.empty();
		ctx.param("id", id, JDBCType.VARCHAR);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.empty();
		ctx.param("id", id, JDBCType.VARCHAR.getVendorTypeNumber());

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.empty();
		ctx.inOutParam("id", id, JDBCType.VARCHAR);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.empty();
		ctx.inOutParam("id", id, JDBCType.VARCHAR.getVendorTypeNumber());

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

	}

	@Test
	public void testParamOptionalBean() throws Exception {
		String sql = "insert into test ("
				+ "/*IF id > 0 */, id/*END*/"
				+ "/*IF name != null */, name/*END*/"
				+ "/*IF age > 0 */, age/*END*/"
				+ "/*IF memo != null */, memo/*END*/"
				+ ") values ("
				+ "/*IF id > 0 */, /*id*//*END*/"
				+ "/*IF name != null */, /*name*//*END*/"
				+ "/*IF age > 0 */, /*age*//*END*/"
				+ "/*IF memo != null */, /*memo*//*END*/"
				+ ")";
		SqlContext ctx = config.contextWith(sql);

		TestEntity entity = new TestEntity(10, "Taro", 20, Optional.of("memo1"));
		ctx.paramBean(entity);

		transform(ctx);

		assertThat(ctx.getExecutableSql(),
				is("insert into test ( id, name, age, memo) values ( ?/*id*/, ?/*name*/, ?/*age*/, ?/*memo*/)"));

		ctx = config.contextWith(sql);

		entity = new TestEntity(10, "Taro", 20, Optional.empty());
		ctx.paramBean(entity);

		transform(ctx);

		assertThat(ctx.getExecutableSql(),
				is("insert into test ( id, name, age, memo) values ( ?/*id*/, ?/*name*/, ?/*age*/, ?/*memo*/)"));

		ctx = config.contextWith(sql);

		ctx.paramBean(null);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("insert into test () values ()"));

	}

	@Test
	public void testParamOptionalMap() throws Exception {
		String sql = "insert into test ("
				+ "/*IF id > 0 */, id/*END*/"
				+ "/*IF name != null */, name/*END*/"
				+ "/*IF age > 0 */, age/*END*/"
				+ "/*IF memo != null */, memo/*END*/"
				+ ") values ("
				+ "/*IF id > 0 */, /*id*//*END*/"
				+ "/*IF name != null */, /*name*//*END*/"
				+ "/*IF age > 0 */, /*age*//*END*/"
				+ "/*IF memo != null */, /*memo*//*END*/"
				+ ")";
		SqlContext ctx = config.contextWith(sql);

		Map<String, Object> map = new HashMap<>();
		map.put("id", 10);
		map.put("name", "Taro");
		map.put("age", 20);
		map.put("memo", Optional.of("memo1"));

		ctx.paramMap(map);

		transform(ctx);

		assertThat(ctx.getExecutableSql(),
				is("insert into test ( id, name, age, memo) values ( ?/*id*/, ?/*name*/, ?/*age*/, ?/*memo*/)"));

		ctx = config.contextWith(sql);

		map = new HashMap<>();
		map.put("id", 10);
		map.put("name", "Taro");
		map.put("age", 20);
		map.put("memo", Optional.empty());
		ctx.paramMap(map);

		transform(ctx);

		assertThat(ctx.getExecutableSql(),
				is("insert into test ( id, name, age, memo) values ( ?/*id*/, ?/*name*/, ?/*age*/, ?/*memo*/)"));

		ctx = config.contextWith(sql);

		ctx.paramMap(null);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("insert into test () values ()"));

	}

	@Test
	public void testContext() {
		SqlContext ctx = config.contextWith("select * from test");
		assertEquals(ctx, ctx.context());
	}

	@Test
	public void testGetParameterNames() {
		SqlContext ctx = config.contextWith("select * from test")
				.param("param1", 1)
				.param("param2", "2");
		assertThat(((SqlContextImpl) ctx).getParameterNames().size(), is(2));
		assertThat(((SqlContextImpl) ctx).getParameterNames().iterator().hasNext(), is(true));
		assertThat(((SqlContextImpl) ctx).getParameterNames().contains("param1"), is(true));
	}

	@Test
	public void testSetRetry() {
		SqlContext ctx = config.contextWith("select * from test")
				.param("param1", 1)
				.param("param2", "2")
				.retry(3);
		assertThat(ctx.getMaxRetryCount(), is(3));
		assertThat(ctx.getRetryWaitTime(), is(0));

		ctx = config.contextWith("select * from test")
				.param("param1", 1)
				.param("param2", "2")
				.retry(4, 10);
		assertThat(ctx.getMaxRetryCount(), is(4));
		assertThat(ctx.getRetryWaitTime(), is(10));
	}

	@Test
	public void testGetDefineColumnType() {
		SqlContext ctx = config.contextWith("select * from test");
		ctx.addDefineColumnType(1, JDBCType.CHAR.getVendorTypeNumber());

		assertThat(ctx.getDefineColumnTypes().get(1), is(JDBCType.CHAR.getVendorTypeNumber()));
	}

	@Test
	public void testGetParameterMapperManager() {
		SqlContext ctx = config.contextWith("select * from test");
		assertThat(((SqlContextImpl) ctx).getParameterMapperManager(), not(nullValue()));
	}

	@Test
	public void testSqlId() {
		final String testSqlId = "TEST_SQL_ID";
		SqlContext ctx = config.contextWith("select * from test").sqlId(testSqlId);
		assertThat(ctx.getSqlId(), is(testSqlId));
	}

	private void transform(final SqlContext ctx) {
		SqlParser sqlParser = new SqlParserImpl(ctx.getSql(), config.getExpressionParser(),
				config.getDialect().isRemoveTerminator(), true);
		ContextTransformer contextTransformer = sqlParser.parse();
		contextTransformer.transform(ctx);
	}

	public static class TestEntity {
		private int id;
		private String name;
		private int age;
		private Optional<String> memo = Optional.empty();

		public TestEntity(final int id, final String name, final int age, final Optional<String> memo) {
			super();
			this.id = id;
			this.name = name;
			this.age = age;
			this.memo = memo;
		}

		/**
		 * id を取得します。
		 *
		 * @return id
		 */
		public int getId() {
			return id;
		}

		/**
		 * id を設定します。
		 *
		 * @param id id
		 */
		public void setId(final int id) {
			this.id = id;
		}

		/**
		 * name を取得します。
		 *
		 * @return name
		 */
		public String getName() {
			return name;
		}

		/**
		 * name を設定します。
		 *
		 * @param name name
		 */
		public void setName(final String name) {
			this.name = name;
		}

		/**
		 * age を取得します。
		 *
		 * @return age
		 */
		public int getAge() {
			return age;
		}

		/**
		 * age を設定します。
		 *
		 * @param age age
		 */
		public void setAge(final int age) {
			this.age = age;
		}

		/**
		 * memo を取得します。
		 *
		 * @return memo
		 */
		public Optional<String> getMemo() {
			return memo;
		}

		/**
		 * memo を設定します。
		 *
		 * @param memo memo
		 */
		public void setMemo(final Optional<String> memo) {
			this.memo = memo;
		}
	}
}