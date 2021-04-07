package jp.co.future.uroborosql.context;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.sql.JDBCType;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.parameter.ReaderParameter;
import jp.co.future.uroborosql.parameter.StreamParameter;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;

public class ExecutionContextImplTest {
	private static SqlConfig config = null;

	@BeforeAll
	public static void setUpClass() {
		config = UroboroSQL.builder("jdbc:h2:mem:ExecutionContextImplTest", "sa", "").build();
	}

	private ExecutionContext getExecutionContext(final String sql) {
		var s = replaceLineSep(sql);
		var ctx = config.contextWith(s);
		ctx.addSqlPart(s);
		return ctx;
	}

	private String replaceLineSep(final String sql) {
		return sql.replaceAll("\\[LF\\]", System.lineSeparator());
	}

	@Test
	public void removeFirstAndKeyWordWhenWhereClause() throws Exception {
		var ctx11 = getExecutionContext("select * from test where[LF][LF][LF] and aaa = 1");
		assertThat(ctx11.getExecutableSql(), is(replaceLineSep("select * from test where[LF] aaa = 1")));
		var ctx12 = getExecutionContext("select * from test[LF]where[LF][LF][LF] and aaa = 1");
		assertThat(ctx12.getExecutableSql(), is(replaceLineSep("select * from test[LF]where[LF] aaa = 1")));

		var ctx21 = getExecutionContext("select * from test where[LF]      and aaa = 1");
		assertThat(ctx21.getExecutableSql(), is(replaceLineSep("select * from test where[LF]      aaa = 1")));
		var ctx22 = getExecutionContext("select * from test[LF]where[LF]      and aaa = 1");
		assertThat(ctx22.getExecutableSql(), is(replaceLineSep("select * from test[LF]where[LF]      aaa = 1")));

		var ctx31 = getExecutionContext("select * from test where /* comment */ and aaa = 1");
		assertThat(ctx31.getExecutableSql(), is(replaceLineSep("select * from test where /* comment */ aaa = 1")));
		var ctx32 = getExecutionContext("select * from test[LF]where /* comment */ and aaa = 1");
		assertThat(ctx32.getExecutableSql(), is(replaceLineSep("select * from test[LF]where /* comment */ aaa = 1")));

		var ctx41 = getExecutionContext("select * from test where -- /* comment */  [LF] and aaa = 1");
		assertThat(ctx41.getExecutableSql(),
				is(replaceLineSep("select * from test where -- /* comment */  [LF] aaa = 1")));
		var ctx42 = getExecutionContext("select * from test[LF]where -- /* comment */  [LF] and aaa = 1");
		assertThat(ctx42.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]where -- /* comment */  [LF] aaa = 1")));

		var ctx51 = getExecutionContext("select * from test where -- /* comment */  [LF] order = 1");
		assertThat(ctx51.getExecutableSql(),
				is(replaceLineSep("select * from test where -- /* comment */  [LF] order = 1")));
		var ctx52 = getExecutionContext("select * from test[LF]where -- /* comment */  [LF] order = 1");
		assertThat(ctx52.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]where -- /* comment */  [LF] order = 1")));

		var ctx61 = getExecutionContext("select * from test where /* comment */ --comment [LF] order = 1");
		assertThat(ctx61.getExecutableSql(),
				is(replaceLineSep("select * from test where /* comment */ --comment [LF] order = 1")));
		var ctx62 = getExecutionContext("select * from test[LF]where /* comment */ --comment [LF] order = 1");
		assertThat(ctx62.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]where /* comment */ --comment [LF] order = 1")));
	}

	@Test
	public void removeFirstCommaWhenSelectClause() throws Exception {
		var ctx1 = getExecutionContext("select ,aaa,bbb,ccc from test");
		assertThat(ctx1.getExecutableSql(), is(replaceLineSep("select aaa,bbb,ccc from test")));

		var ctx2 = getExecutionContext("select , aaa, bbb, ccc from test");
		assertThat(ctx2.getExecutableSql(), is(replaceLineSep("select  aaa, bbb, ccc from test")));

		var ctx3 = getExecutionContext("select[LF], aaa[LF], bbb[LF], ccc from test");
		assertThat(ctx3.getExecutableSql(), is(replaceLineSep("select[LF] aaa[LF], bbb[LF], ccc from test")));

		var ctx4 = getExecutionContext("select /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertThat(ctx4.getExecutableSql(),
				is(replaceLineSep("select /* comment */ [LF] aaa[LF], bbb[LF], ccc from test")));

		var ctx5 = getExecutionContext("select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertThat(ctx5.getExecutableSql(),
				is(replaceLineSep("select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test")));

		var ctx6 = getExecutionContext(
				"with dummy as ( select * from dummy ) select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertThat(ctx6.getExecutableSql(),
				is(replaceLineSep(
						"with dummy as ( select * from dummy ) select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test")));

		var ctx7 = getExecutionContext(
				"with dummy as ( select * from dummy )[LF]select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertThat(ctx7.getExecutableSql(), is(replaceLineSep(
				"with dummy as ( select * from dummy )[LF]select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test")));
	}

	@Test
	public void removeFirstCommaWhenOrderByClause() throws Exception {
		var ctx11 = getExecutionContext("select * from test order by ,aaa, bbb");
		assertThat(ctx11.getExecutableSql(), is(replaceLineSep("select * from test order by aaa, bbb")));
		var ctx12 = getExecutionContext("select * from test[LF]order by ,aaa, bbb");
		assertThat(ctx12.getExecutableSql(), is(replaceLineSep("select * from test[LF]order by aaa, bbb")));

		var ctx21 = getExecutionContext("select * from test order by , aaa, bbb");
		assertThat(ctx21.getExecutableSql(), is(replaceLineSep("select * from test order by  aaa, bbb")));
		var ctx22 = getExecutionContext("select * from test[LF]order by , aaa, bbb");
		assertThat(ctx22.getExecutableSql(), is(replaceLineSep("select * from test[LF]order by  aaa, bbb")));

		var ctx31 = getExecutionContext("select * from test order by[LF], aaa, bbb");
		assertThat(ctx31.getExecutableSql(), is(replaceLineSep("select * from test order by[LF] aaa, bbb")));
		var ctx32 = getExecutionContext("select * from test[LF]order by[LF], aaa, bbb");
		assertThat(ctx32.getExecutableSql(), is(replaceLineSep("select * from test[LF]order by[LF] aaa, bbb")));

		var ctx41 = getExecutionContext("select * from test order by /* comment */[LF], aaa, bbb");
		assertThat(ctx41.getExecutableSql(),
				is(replaceLineSep("select * from test order by /* comment */[LF] aaa, bbb")));
		var ctx42 = getExecutionContext("select * from test[LF]order by /* comment */[LF], aaa, bbb");
		assertThat(ctx42.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]order by /* comment */[LF] aaa, bbb")));

		var ctx51 = getExecutionContext("select * from test order by --/* comment */[LF], aaa, bbb");
		assertThat(ctx51.getExecutableSql(),
				is(replaceLineSep("select * from test order by --/* comment */[LF] aaa, bbb")));
		var ctx52 = getExecutionContext("select * from test[LF]order by --/* comment */[LF], aaa, bbb");
		assertThat(ctx52.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]order by --/* comment */[LF] aaa, bbb")));

		var ctx61 = getExecutionContext("select * from test order     by --/* comment */[LF], aaa, bbb");
		assertThat(ctx61.getExecutableSql(),
				is(replaceLineSep("select * from test order     by --/* comment */[LF] aaa, bbb")));
		var ctx62 = getExecutionContext("select * from test[LF]order     by --/* comment */[LF], aaa, bbb");
		assertThat(ctx62.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]order     by --/* comment */[LF] aaa, bbb")));
	}

	@Test
	public void removeFirstCommaWhenGroupByClause() throws Exception {
		var ctx11 = getExecutionContext("select * from test group by ,aaa, bbb");
		assertThat(ctx11.getExecutableSql(), is(replaceLineSep("select * from test group by aaa, bbb")));
		var ctx12 = getExecutionContext("select * from test[LF]group by ,aaa, bbb");
		assertThat(ctx12.getExecutableSql(), is(replaceLineSep("select * from test[LF]group by aaa, bbb")));

		var ctx21 = getExecutionContext("select * from test group by , aaa, bbb");
		assertThat(ctx21.getExecutableSql(), is(replaceLineSep("select * from test group by  aaa, bbb")));
		var ctx22 = getExecutionContext("select * from test[LF]group by , aaa, bbb");
		assertThat(ctx22.getExecutableSql(), is(replaceLineSep("select * from test[LF]group by  aaa, bbb")));

		var ctx31 = getExecutionContext("select * from test group by[LF], aaa, bbb");
		assertThat(ctx31.getExecutableSql(), is(replaceLineSep("select * from test group by[LF] aaa, bbb")));
		var ctx32 = getExecutionContext("select * from test[LF]group by[LF], aaa, bbb");
		assertThat(ctx32.getExecutableSql(), is(replaceLineSep("select * from test[LF]group by[LF] aaa, bbb")));

		var ctx41 = getExecutionContext("select * from test group by /* comment */  [LF], aaa, bbb");
		assertThat(ctx41.getExecutableSql(),
				is(replaceLineSep("select * from test group by /* comment */  [LF] aaa, bbb")));
		var ctx42 = getExecutionContext("select * from test[LF]group by /* comment */  [LF], aaa, bbb");
		assertThat(ctx42.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]group by /* comment */  [LF] aaa, bbb")));

		var ctx51 = getExecutionContext("select * from test group by /* comment */ --aaa[LF], aaa, bbb");
		assertThat(ctx51.getExecutableSql(),
				is(replaceLineSep("select * from test group by /* comment */ --aaa[LF] aaa, bbb")));
		var ctx52 = getExecutionContext("select * from test[LF]group by /* comment */ --aaa[LF], aaa, bbb");
		assertThat(ctx52.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]group by /* comment */ --aaa[LF] aaa, bbb")));

		var ctx61 = getExecutionContext("select * from test group     by /* comment */ --aaa[LF], aaa, bbb");
		assertThat(ctx61.getExecutableSql(),
				is(replaceLineSep("select * from test group     by /* comment */ --aaa[LF] aaa, bbb")));
		var ctx62 = getExecutionContext("select * from test[LF]group     by /* comment */ --aaa[LF], aaa, bbb");
		assertThat(ctx62.getExecutableSql(),
				is(replaceLineSep("select * from test[LF]group     by /* comment */ --aaa[LF] aaa, bbb")));
	}

	@Test
	public void removeFirstCommaWhenStartBracket() throws Exception {
		var ctx11 = getExecutionContext("insert into (,aaa,bbb,ccc) values (,111,222,333)");
		assertThat(ctx11.getExecutableSql(), is(replaceLineSep("insert into (aaa,bbb,ccc) values (111,222,333)")));
		var ctx12 = getExecutionContext("insert into[LF](,aaa,bbb,ccc) values (,111,222,333)");
		assertThat(ctx12.getExecutableSql(), is(replaceLineSep("insert into[LF](aaa,bbb,ccc) values (111,222,333)")));

		var ctx21 = getExecutionContext("insert into (, aaa, bbb, ccc) values (,111 ,222 ,333)");
		assertThat(ctx21.getExecutableSql(), is(replaceLineSep("insert into ( aaa, bbb, ccc) values (111 ,222 ,333)")));
		var ctx22 = getExecutionContext("insert into[LF](, aaa, bbb, ccc) values (,111 ,222 ,333)");
		assertThat(ctx22.getExecutableSql(),
				is(replaceLineSep("insert into[LF]( aaa, bbb, ccc) values (111 ,222 ,333)")));

		var ctx31 = getExecutionContext(
				"insert into ([LF], aaa[LF], bbb[LF], ccc[LF]) values (,[LF]111,[LF]222,[LF]333[LF])");
		assertThat(ctx31.getExecutableSql(), is(
				replaceLineSep("insert into ([LF] aaa[LF], bbb[LF], ccc[LF]) values ([LF]111,[LF]222,[LF]333[LF])")));
		var ctx32 = getExecutionContext(
				"insert into[LF]([LF], aaa[LF], bbb[LF], ccc[LF]) values (,[LF]111,[LF]222,[LF]333[LF])");
		assertThat(ctx32.getExecutableSql(), is(replaceLineSep(
				"insert into[LF]([LF] aaa[LF], bbb[LF], ccc[LF]) values ([LF]111,[LF]222,[LF]333[LF])")));

		var ctx41 = getExecutionContext(
				"insert into ([LF]/* comment */, aaa[LF], bbb[LF], ccc[LF]) values (/* comment */,[LF]111,[LF]222,[LF]333[LF])");
		assertThat(ctx41.getExecutableSql(),
				is(replaceLineSep(
						"insert into ([LF]/* comment */ aaa[LF], bbb[LF], ccc[LF]) values (/* comment */[LF]111,[LF]222,[LF]333[LF])")));
		var ctx42 = getExecutionContext(
				"insert into[LF]([LF]/* comment */, aaa[LF], bbb[LF], ccc[LF]) values (/* comment */,[LF]111,[LF]222,[LF]333[LF])");
		assertThat(ctx42.getExecutableSql(),
				is(replaceLineSep(
						"insert into[LF]([LF]/* comment */ aaa[LF], bbb[LF], ccc[LF]) values (/* comment */[LF]111,[LF]222,[LF]333[LF])")));

		var ctx51 = getExecutionContext(
				"insert into (--comment[LF], aaa[LF], bbb[LF], ccc[LF]) values (,/*comment*/[LF]111,[LF]222,[LF]333[LF])");
		assertThat(ctx51.getExecutableSql(),
				is(replaceLineSep(
						"insert into (--comment[LF] aaa[LF], bbb[LF], ccc[LF]) values (/*comment*/[LF]111,[LF]222,[LF]333[LF])")));
		var ctx52 = getExecutionContext(
				"insert into[LF](--comment[LF], aaa[LF], bbb[LF], ccc[LF]) values (,/*comment*/[LF]111,[LF]222,[LF]333[LF])");
		assertThat(ctx52.getExecutableSql(),
				is(replaceLineSep(
						"insert into[LF](--comment[LF] aaa[LF], bbb[LF], ccc[LF]) values (/*comment*/[LF]111,[LF]222,[LF]333[LF])")));
	}

	@Test
	public void removeFirstCommaWhenSetClause() throws Exception {
		var ctx11 = getExecutionContext("update test set ,aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx11.getExecutableSql(),
				is(replaceLineSep("update test set aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));
		var ctx12 = getExecutionContext("update test[LF]set ,aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx12.getExecutableSql(),
				is(replaceLineSep("update test[LF]set aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));

		var ctx21 = getExecutionContext("update test set , aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx21.getExecutableSql(),
				is(replaceLineSep("update test set  aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));
		var ctx22 = getExecutionContext("update test[LF]set , aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx22.getExecutableSql(),
				is(replaceLineSep("update test[LF]set  aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));

		var ctx31 = getExecutionContext("update test set[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx31.getExecutableSql(),
				is(replaceLineSep("update test set[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));
		var ctx32 = getExecutionContext("update test[LF]set[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx32.getExecutableSql(),
				is(replaceLineSep("update test[LF]set[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));

		var ctx41 = getExecutionContext(
				"update test set /* comment */[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx41.getExecutableSql(),
				is(replaceLineSep("update test set /* comment */[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));
		var ctx42 = getExecutionContext(
				"update test[LF]set /* comment */[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx42.getExecutableSql(),
				is(replaceLineSep("update test[LF]set /* comment */[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));

		var ctx51 = getExecutionContext("update test set --comment[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx51.getExecutableSql(),
				is(replaceLineSep("update test set --comment[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));
		var ctx52 = getExecutionContext(
				"update test[LF]set --comment[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertThat(ctx52.getExecutableSql(),
				is(replaceLineSep("update test[LF]set --comment[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1")));

		var ctx61 = getExecutionContext("select , aaa, code_set, bbb, ccc from test where 1 = 1");
		assertThat(ctx61.getExecutableSql(),
				is(replaceLineSep("select  aaa, code_set, bbb, ccc from test where 1 = 1")));
		var ctx62 = getExecutionContext("select[LF], aaa[LF], code_set[LF], bbb[LF], ccc[LF]from test[LF]where 1 = 1");
		assertThat(ctx62.getExecutableSql(),
				is(replaceLineSep("select[LF] aaa[LF], code_set[LF], bbb[LF], ccc[LF]from test[LF]where 1 = 1")));
		var ctx63 = getExecutionContext(
				"select[LF], aaa,[LF]code_set,[LF]bbb,[LF]ccc[LF]from[LF]test[LF]where[LF]1 = 1");
		assertThat(ctx63.getExecutableSql(),
				is(replaceLineSep("select[LF] aaa,[LF]code_set,[LF]bbb,[LF]ccc[LF]from[LF]test[LF]where[LF]1 = 1")));
	}

	@Test
	public void testHasParam() throws Exception {
		var ctx = getExecutionContext("select * from dummy");
		ctx.param("key1", "value1");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.hasParam("key2"), is(false));
	}

	@Test
	public void testParamList() throws Exception {
		ExecutionContext ctx = null;

		ctx = getExecutionContext("select * from dummy");
		ctx.param("key1", List.of("value1"));
		assertThat(ctx.getParam("key1").getValue(), is(Arrays.asList("value1")));

		ctx = getExecutionContext("select * from dummy");
		ctx.param("key1", List.of("value1", "value2"));
		assertThat(ctx.getParam("key1").getValue(), is(Arrays.asList("value1", "value2")));

		Set<String> values = new HashSet<>();
		values.add("value1");
		values.add("value2");

		ctx = getExecutionContext("select * from dummy");
		ctx.param("key1", values);
		assertThat(ctx.getParam("key1").getValue(), is(values));

		ctx = getExecutionContext("select * from dummy");
		ctx.param("key1", () -> values);
		assertThat(ctx.getParam("key1").getValue(), is(values));
	}

	@Test
	public void testIfAbsent() throws Exception {
		ExecutionContext ctx = null;

		ctx = getExecutionContext("select * from dummy");
		ctx.paramIfAbsent("key1", "value1");
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.paramIfAbsent("key1", "value2");
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getExecutionContext("select * from dummy");
		ctx.paramIfAbsent("key1", "value1", JDBCType.VARCHAR);
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.paramIfAbsent("key1", "value2", JDBCType.VARCHAR);
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getExecutionContext("select * from dummy");
		ctx.paramIfAbsent("key1", "value1", JDBCType.VARCHAR.getVendorTypeNumber());
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.paramIfAbsent("key1", "value2", JDBCType.VARCHAR.getVendorTypeNumber());
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getExecutionContext("select * from dummy");
		ctx.inOutParamIfAbsent("key1", "value1", JDBCType.VARCHAR);
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.inOutParamIfAbsent("key1", "value2", JDBCType.VARCHAR);
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getExecutionContext("select * from dummy");
		ctx.inOutParamIfAbsent("key1", "value1", JDBCType.VARCHAR.getVendorTypeNumber());
		assertThat(ctx.getParam("key1").getValue(), is("value1"));
		ctx.inOutParamIfAbsent("key1", "value2", JDBCType.VARCHAR.getVendorTypeNumber());
		assertThat(ctx.getParam("key1").getValue(), is("value1"));

		ctx = getExecutionContext("select * from dummy");
		InputStream is1 = new ByteArrayInputStream("value1".getBytes());
		ctx.blobParamIfAbsent("key1", is1, "value1".length());
		var stream1 = (StreamParameter) ctx.getParam("key1");
		assertThat(ctx.getParam("key1").getValue(), is("[BLOB]"));
		InputStream is2 = new ByteArrayInputStream("value2".getBytes());
		ctx.blobParamIfAbsent("key1", is2, "value2".length());
		assertThat(ctx.getParam("key1"), is(stream1));

		ctx = getExecutionContext("select * from dummy");
		InputStream is11 = new ByteArrayInputStream("value1".getBytes());
		ctx.blobParamIfAbsent("key1", is11);
		var stream11 = (StreamParameter) ctx.getParam("key1");
		assertThat(ctx.getParam("key1").getValue(), is("[BLOB]"));
		InputStream is22 = new ByteArrayInputStream("value2".getBytes());
		ctx.blobParamIfAbsent("key1", is22);
		assertThat(ctx.getParam("key1"), is(stream11));

		ctx = getExecutionContext("select * from dummy");
		Reader r1 = new StringReader("value1");
		ctx.clobParamIfAbsent("key1", r1);
		var reader1 = (ReaderParameter) ctx.getParam("key1");
		assertThat(ctx.getParam("key1").getValue(), is("[CLOB]"));
		Reader r2 = new StringReader("value2");
		ctx.clobParamIfAbsent("key1", r2);
		assertThat(ctx.getParam("key1"), is(reader1));

		ctx = getExecutionContext("select * from dummy");
		Reader r11 = new StringReader("value1");
		ctx.clobParamIfAbsent("key1", r11, "value1".length());
		var reader11 = (ReaderParameter) ctx.getParam("key1");
		assertThat(ctx.getParam("key1").getValue(), is("[CLOB]"));
		Reader r22 = new StringReader("value2");
		ctx.clobParamIfAbsent("key1", r22, "value1".length());
		assertThat(ctx.getParam("key1"), is(reader11));
	}

	@Test
	public void testParamOptionalHasValue() throws Exception {
		var ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
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
		var ctx = config.contextWith("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
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
		var sql = "insert into test ("
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
		var ctx = config.contextWith(sql);

		var entity = new TestEntity(10, "Taro", 20, Optional.of("memo1"));
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
		var sql = "insert into test ("
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
		var ctx = config.contextWith(sql);

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
		var ctx = config.contextWith("select * from test");
		assertThat(ctx.context(), is(ctx));
	}

	@Test
	public void testGetParameterNames() {
		var ctx = config.contextWith("select * from test")
				.param("param1", 1)
				.param("param2", "2");
		assertThat(((ExecutionContextImpl) ctx).getParameterNames().size(), is(2));
		assertThat(((ExecutionContextImpl) ctx).getParameterNames().iterator().hasNext(), is(true));
		assertThat(((ExecutionContextImpl) ctx).getParameterNames().contains("param1"), is(true));
	}

	@Test
	public void testSetRetry() {
		var ctx = config.contextWith("select * from test")
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
		var ctx = config.contextWith("select * from test");
		ctx.addDefineColumnType(1, JDBCType.CHAR.getVendorTypeNumber());

		assertThat(ctx.getDefineColumnTypes().get(1), is(JDBCType.CHAR.getVendorTypeNumber()));
	}

	@Test
	public void testGetParameterMapperManager() {
		var ctx = config.contextWith("select * from test");
		assertThat(((ExecutionContextImpl) ctx).getParameterMapperManager(), not(nullValue()));
	}

	@Test
	public void testSqlId() {
		final var testSqlId = "TEST_SQL_ID";
		var ctx = config.contextWith("select * from test").sqlId(testSqlId);
		assertThat(ctx.getSqlId(), is(testSqlId));
	}

	private void transform(final ExecutionContext ctx) {
		SqlParser sqlParser = new SqlParserImpl(ctx.getSql(), config.getExpressionParser(),
				config.getDialect().isRemoveTerminator(), true);
		var contextTransformer = sqlParser.parse();
		contextTransformer.transform(ctx);
	}

	public static class TestEntity {
		private int id;
		private String name;
		private int age;
		private Optional<String> memo = Optional.empty();

		public TestEntity(final int id, final String name, final int age, final Optional<String> memo) {
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