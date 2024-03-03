package jp.co.future.uroborosql.context;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.sql.JDBCType;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.parameter.ReaderParameter;
import jp.co.future.uroborosql.parameter.StreamParameter;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;

public class ExecutionContextImplTest {
	/** ロガー */
	protected static final Logger log = LoggerFactory.getLogger(ExecutionContextImplTest.class);

	private static SqlConfig config = null;

	@BeforeAll
	public static void setUpClass() {
		config = UroboroSQL.builder("jdbc:h2:mem:ExecutionContextImplTest", "sa", "").build();
	}

	private ExecutionContext getExecutionContext(final String sql) {
		var s = replaceLineSep(sql);
		var ctx = config.context().setSql(s);
		ctx.addSqlPart(s);
		return ctx;
	}

	private String replaceLineSep(final String sql) {
		return sql.replace("[LF]", System.lineSeparator());
	}

	@Test
	void removeFirstAndKeyWordWhenWhereClause() throws Exception {
		var ctx11 = getExecutionContext("select * from test where[LF][LF][LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test where[LF] aaa = 1"),
				ctx11.getExecutableSql());
		var ctx12 = getExecutionContext("select * from test[LF]where[LF][LF][LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where[LF] aaa = 1"),
				ctx12.getExecutableSql());

		var ctx21 = getExecutionContext("select * from test where[LF]      and aaa = 1");
		assertEquals(replaceLineSep("select * from test where[LF]      aaa = 1"),
				ctx21.getExecutableSql());
		var ctx22 = getExecutionContext("select * from test[LF]where[LF]      and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where[LF]      aaa = 1"),
				ctx22.getExecutableSql());

		var ctx31 = getExecutionContext("select * from test where /* comment */ and aaa = 1");
		assertEquals(replaceLineSep("select * from test where /* comment */ aaa = 1"),
				ctx31.getExecutableSql());
		var ctx32 = getExecutionContext("select * from test[LF]where /* comment */ and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where /* comment */ aaa = 1"),
				ctx32.getExecutableSql());

		var ctx41 = getExecutionContext("select * from test where -- /* comment */  [LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test where -- /* comment */  [LF] aaa = 1"),
				ctx41.getExecutableSql());
		var ctx42 = getExecutionContext("select * from test[LF]where -- /* comment */  [LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where -- /* comment */  [LF] aaa = 1"),
				ctx42.getExecutableSql());

		var ctx51 = getExecutionContext("select * from test where -- /* comment */  [LF] order = 1");
		assertEquals(replaceLineSep("select * from test where -- /* comment */  [LF] order = 1"),
				ctx51.getExecutableSql());
		var ctx52 = getExecutionContext("select * from test[LF]where -- /* comment */  [LF] order = 1");
		assertEquals(replaceLineSep("select * from test[LF]where -- /* comment */  [LF] order = 1"),
				ctx52.getExecutableSql());

		var ctx61 = getExecutionContext("select * from test where /* comment */ --comment [LF] order = 1");
		assertEquals(replaceLineSep("select * from test where /* comment */ --comment [LF] order = 1"),
				ctx61.getExecutableSql());
		var ctx62 = getExecutionContext(
				"select * from test[LF]where /* comment */ --comment [LF] order = 1");
		assertEquals(replaceLineSep("select * from test[LF]where /* comment */ --comment [LF] order = 1"),
				ctx62.getExecutableSql());
		var ctx63 = getExecutionContext(
				"select * from test[LF]where /* [LF]comment[LF] */ --comment [LF] order = 1");
		assertEquals(replaceLineSep("select * from test[LF]where /* [LF]comment[LF] */ --comment [LF] order = 1"),
				ctx63.getExecutableSql());
		var ctx64 = getExecutionContext(
				"select * from test[LF]where /* comment */ --comment [LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where /* comment */ --comment [LF] aaa = 1"),
				ctx64.getExecutableSql());
		var ctx65 = getExecutionContext(
				"select * from test[LF]where /* [LF]comment[LF] */ --comment [LF] and aaa = 1");
		assertEquals(replaceLineSep("select * from test[LF]where /* [LF]comment[LF] */ --comment [LF] aaa = 1"),
				ctx65.getExecutableSql());

		var startTime = Instant.now(Clock.systemDefaultZone());
		var sql = replaceLineSep("select * from test[LF]where -- /* comment */  [LF] and aaa = 1");
		for (var i = 0; i < 1000000; i++) {
			var timeCtx = config.context().setSql(sql);
			timeCtx.addSqlPart(sql);
			timeCtx.getExecutableSql();
		}
		log.info("removeFirstAndKeyWordWhenWhereClause elapsed time. {}",
				DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS").format(
						LocalTime.MIDNIGHT.plus(Duration.between(startTime, Instant.now(Clock.systemDefaultZone())))));
	}

	@Test
	void removeFirstCommaWhenSelectClause() throws Exception {
		var ctx1 = getExecutionContext("select ,aaa,bbb,ccc from test");
		assertEquals(replaceLineSep("select aaa,bbb,ccc from test"), ctx1.getExecutableSql());

		var ctx2 = getExecutionContext("select , aaa, bbb, ccc from test");
		assertEquals(replaceLineSep("select  aaa, bbb, ccc from test"), ctx2.getExecutableSql());

		var ctx3 = getExecutionContext("select[LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep("select[LF] aaa[LF], bbb[LF], ccc from test"), ctx3.getExecutableSql());

		var ctx4 = getExecutionContext("select /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep("select /* comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx4.getExecutableSql());

		var ctx5 = getExecutionContext("select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep("select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx5.getExecutableSql());

		var ctx6 = getExecutionContext(
				"with dummy as ( select * from dummy ) select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep(
				"with dummy as ( select * from dummy ) select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx6.getExecutableSql());

		var ctx7 = getExecutionContext(
				"with dummy as ( select * from dummy )[LF]select -- /* comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep(
				"with dummy as ( select * from dummy )[LF]select -- /* comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx7.getExecutableSql());

		var ctx8 = getExecutionContext(
				"with dummy as ( select * from dummy )[LF]select /* コメント:japanese comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		assertEquals(replaceLineSep(
				"with dummy as ( select * from dummy )[LF]select /* コメント:japanese comment */ [LF] aaa[LF], bbb[LF], ccc from test"),
				ctx8.getExecutableSql());

		var startTime = Instant.now(Clock.systemDefaultZone());
		var sql = replaceLineSep(
				"with dummy as ( select * from dummy )[LF]select /* コメント:japanese comment */ [LF], aaa[LF], bbb[LF], ccc from test");
		for (var i = 0; i < 1000000; i++) {
			var timeCtx = config.context().setSql(sql);
			timeCtx.addSqlPart(sql);
			timeCtx.getExecutableSql();
		}
		log.info("removeFirstCommaWhenSelectClause elapsed time. {}",
				DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS").format(
						LocalTime.MIDNIGHT.plus(Duration.between(startTime, Instant.now(Clock.systemDefaultZone())))));
	}

	@Test
	void removeFirstCommaWhenOrderByClause() throws Exception {
		var ctx11 = getExecutionContext("select * from test order by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by aaa, bbb"), ctx11.getExecutableSql());
		var ctx12 = getExecutionContext("select * from test[LF]order by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by aaa, bbb"), ctx12.getExecutableSql());

		var ctx21 = getExecutionContext("select * from test order by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by  aaa, bbb"), ctx21.getExecutableSql());
		var ctx22 = getExecutionContext("select * from test[LF]order by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by  aaa, bbb"), ctx22.getExecutableSql());

		var ctx31 = getExecutionContext("select * from test order by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by[LF] aaa, bbb"), ctx31.getExecutableSql());
		var ctx32 = getExecutionContext("select * from test[LF]order by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by[LF] aaa, bbb"), ctx32.getExecutableSql());

		var ctx41 = getExecutionContext("select * from test order by /* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by /* comment */[LF] aaa, bbb"),
				ctx41.getExecutableSql());
		var ctx42 = getExecutionContext("select * from test[LF]order by /* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by /* comment */[LF] aaa, bbb"),
				ctx42.getExecutableSql());

		var ctx51 = getExecutionContext("select * from test order by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order by --/* comment */[LF] aaa, bbb"),
				ctx51.getExecutableSql());
		var ctx52 = getExecutionContext("select * from test[LF]order by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order by --/* comment */[LF] aaa, bbb"),
				ctx52.getExecutableSql());

		var ctx61 = getExecutionContext("select * from test order     by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test order     by --/* comment */[LF] aaa, bbb"),
				ctx61.getExecutableSql());
		var ctx62 = getExecutionContext(
				"select * from test[LF]order     by --/* comment */[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]order     by --/* comment */[LF] aaa, bbb"),
				ctx62.getExecutableSql());

		var startTime = Instant.now(Clock.systemDefaultZone());
		var sql = replaceLineSep("select * from test[LF]order     by --/* comment */[LF], aaa, bbb");
		for (var i = 0; i < 1000000; i++) {
			var timeCtx = config.context().setSql(sql);
			timeCtx.addSqlPart(sql);
			timeCtx.getExecutableSql();
		}
		log.info("removeFirstCommaWhenOrderByClause elapsed time. {}",
				DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS").format(
						LocalTime.MIDNIGHT.plus(Duration.between(startTime, Instant.now(Clock.systemDefaultZone())))));
	}

	@Test
	void removeFirstCommaWhenGroupByClause() throws Exception {
		var ctx11 = getExecutionContext("select * from test group by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by aaa, bbb"), ctx11.getExecutableSql());
		var ctx12 = getExecutionContext("select * from test[LF]group by ,aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by aaa, bbb"), ctx12.getExecutableSql());

		var ctx21 = getExecutionContext("select * from test group by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by  aaa, bbb"), ctx21.getExecutableSql());
		var ctx22 = getExecutionContext("select * from test[LF]group by , aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by  aaa, bbb"), ctx22.getExecutableSql());

		var ctx31 = getExecutionContext("select * from test group by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by[LF] aaa, bbb"), ctx31.getExecutableSql());
		var ctx32 = getExecutionContext("select * from test[LF]group by[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by[LF] aaa, bbb"), ctx32.getExecutableSql());

		var ctx41 = getExecutionContext("select * from test group by /* comment */  [LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by /* comment */  [LF] aaa, bbb"),
				ctx41.getExecutableSql());
		var ctx42 = getExecutionContext("select * from test[LF]group by /* comment */  [LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by /* comment */  [LF] aaa, bbb"),
				ctx42.getExecutableSql());

		var ctx51 = getExecutionContext("select * from test group by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group by /* comment */ --aaa[LF] aaa, bbb"),
				ctx51.getExecutableSql());
		var ctx52 = getExecutionContext(
				"select * from test[LF]group by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group by /* comment */ --aaa[LF] aaa, bbb"),
				ctx52.getExecutableSql());

		var ctx61 = getExecutionContext(
				"select * from test group     by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test group     by /* comment */ --aaa[LF] aaa, bbb"),
				ctx61.getExecutableSql());
		var ctx62 = getExecutionContext(
				"select * from test[LF]group     by /* comment */ --aaa[LF], aaa, bbb");
		assertEquals(replaceLineSep("select * from test[LF]group     by /* comment */ --aaa[LF] aaa, bbb"),
				ctx62.getExecutableSql());

		var startTime = Instant.now(Clock.systemDefaultZone());
		var sql = replaceLineSep("select * from test[LF]group     by /* comment */ --aaa[LF], aaa, bbb");
		for (var i = 0; i < 1000000; i++) {
			var timeCtx = config.context().setSql(sql);
			timeCtx.addSqlPart(sql);
			timeCtx.getExecutableSql();
		}
		log.info("removeFirstCommaWhenGroupByClause elapsed time. {}",
				DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS").format(
						LocalTime.MIDNIGHT.plus(Duration.between(startTime, Instant.now(Clock.systemDefaultZone())))));
	}

	@Test
	void removeFirstCommaWhenStartBracket() throws Exception {
		var ctx11 = getExecutionContext("insert into (,aaa,bbb,ccc) values (,111,222,333)");
		assertEquals(replaceLineSep("insert into (aaa,bbb,ccc) values (111,222,333)"), ctx11.getExecutableSql());
		var ctx12 = getExecutionContext("insert into[LF](,aaa,bbb,ccc) values (,111,222,333)");
		assertEquals(replaceLineSep("insert into[LF](aaa,bbb,ccc) values (111,222,333)"), ctx12.getExecutableSql());

		var ctx21 = getExecutionContext("insert into (, aaa, bbb, ccc) values (,111 ,222 ,333)");
		assertEquals(replaceLineSep("insert into ( aaa, bbb, ccc) values (111 ,222 ,333)"), ctx21.getExecutableSql());
		var ctx22 = getExecutionContext("insert into[LF](, aaa, bbb, ccc) values (,111 ,222 ,333)");
		assertEquals(replaceLineSep("insert into[LF]( aaa, bbb, ccc) values (111 ,222 ,333)"),
				ctx22.getExecutableSql());

		var ctx31 = getExecutionContext(
				"insert into ([LF], aaa[LF], bbb[LF], ccc[LF]) values (,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep("insert into ([LF] aaa[LF], bbb[LF], ccc[LF]) values ([LF]111,[LF]222,[LF]333[LF])"),
				ctx31.getExecutableSql());
		var ctx32 = getExecutionContext(
				"insert into[LF]([LF], aaa[LF], bbb[LF], ccc[LF]) values (,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep("insert into[LF]([LF] aaa[LF], bbb[LF], ccc[LF]) values ([LF]111,[LF]222,[LF]333[LF])"),
				ctx32.getExecutableSql());

		var ctx41 = getExecutionContext(
				"insert into ([LF]/* comment */, aaa[LF], bbb[LF], ccc[LF]) values (/* comment */,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep(
						"insert into ([LF]/* comment */ aaa[LF], bbb[LF], ccc[LF]) values (/* comment */[LF]111,[LF]222,[LF]333[LF])"),
				ctx41.getExecutableSql());
		var ctx42 = getExecutionContext(
				"insert into[LF]([LF]/* comment */, aaa[LF], bbb[LF], ccc[LF]) values (/* comment */,[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep(
						"insert into[LF]([LF]/* comment */ aaa[LF], bbb[LF], ccc[LF]) values (/* comment */[LF]111,[LF]222,[LF]333[LF])"),
				ctx42.getExecutableSql());

		var ctx51 = getExecutionContext(
				"insert into (--comment[LF], aaa[LF], bbb[LF], ccc[LF]) values (,/*comment*/[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep(
						"insert into (--comment[LF] aaa[LF], bbb[LF], ccc[LF]) values (/*comment*/[LF]111,[LF]222,[LF]333[LF])"),
				ctx51.getExecutableSql());
		var ctx52 = getExecutionContext(
				"insert into[LF](--comment[LF], aaa[LF], bbb[LF], ccc[LF]) values (,/*comment*/[LF]111,[LF]222,[LF]333[LF])");
		assertEquals(
				replaceLineSep(
						"insert into[LF](--comment[LF] aaa[LF], bbb[LF], ccc[LF]) values (/*comment*/[LF]111,[LF]222,[LF]333[LF])"),
				ctx52.getExecutableSql());

		var startTime = Instant.now(Clock.systemDefaultZone());
		var sql = replaceLineSep(
				"insert into[LF](--comment[LF], aaa[LF], bbb[LF], ccc[LF]) values (,/*comment*/[LF]111,[LF]222,[LF]333[LF])");
		for (var i = 0; i < 1000000; i++) {
			var timeCtx = config.context().setSql(sql);
			timeCtx.addSqlPart(sql);
			timeCtx.getExecutableSql();
		}
		log.info("removeFirstCommaWhenStartBracket elapsed time. {}",
				DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS").format(
						LocalTime.MIDNIGHT.plus(Duration.between(startTime, Instant.now(Clock.systemDefaultZone())))));
	}

	@Test
	void removeFirstCommaWhenSetClause() throws Exception {
		var ctx11 = getExecutionContext("update test set ,aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx11.getExecutableSql());
		var ctx12 = getExecutionContext("update test[LF]set ,aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx12.getExecutableSql());

		var ctx21 = getExecutionContext("update test set , aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set  aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx21.getExecutableSql());
		var ctx22 = getExecutionContext(
				"update test[LF]set , aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set  aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx22.getExecutableSql());

		var ctx31 = getExecutionContext("update test set[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx31.getExecutableSql());
		var ctx32 = getExecutionContext(
				"update test[LF]set[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx32.getExecutableSql());

		var ctx41 = getExecutionContext(
				"update test set /* comment */[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set /* comment */[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx41.getExecutableSql());
		var ctx42 = getExecutionContext(
				"update test[LF]set /* comment */[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set /* comment */[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx42.getExecutableSql());

		var ctx51 = getExecutionContext(
				"update test set --comment[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test set --comment[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx51.getExecutableSql());
		var ctx52 = getExecutionContext(
				"update test[LF]set --comment[LF],aaa = 111, bbb = 222, ccc = 333 where 1 = 1");
		assertEquals(replaceLineSep("update test[LF]set --comment[LF]aaa = 111, bbb = 222, ccc = 333 where 1 = 1"),
				ctx52.getExecutableSql());

		var ctx61 = getExecutionContext("select , aaa, code_set, bbb, ccc from test where 1 = 1");
		assertEquals(replaceLineSep("select  aaa, code_set, bbb, ccc from test where 1 = 1"),
				ctx61.getExecutableSql());
		var ctx62 = getExecutionContext(
				"select[LF], aaa[LF], code_set[LF], bbb[LF], ccc[LF]from test[LF]where 1 = 1");
		assertEquals(replaceLineSep("select[LF] aaa[LF], code_set[LF], bbb[LF], ccc[LF]from test[LF]where 1 = 1"),
				ctx62.getExecutableSql());
		var ctx63 = getExecutionContext(
				"select[LF], aaa,[LF]code_set,[LF]bbb,[LF]ccc[LF]from[LF]test[LF]where[LF]1 = 1");
		assertEquals(replaceLineSep("select[LF] aaa,[LF]code_set,[LF]bbb,[LF]ccc[LF]from[LF]test[LF]where[LF]1 = 1"),
				ctx63.getExecutableSql());

		var startTime = Instant.now(Clock.systemDefaultZone());
		var sql = replaceLineSep("select[LF], aaa,[LF]code_set,[LF]bbb,[LF]ccc[LF]from[LF]test[LF]where[LF]1 = 1");
		for (var i = 0; i < 1000000; i++) {
			var timeCtx = config.context().setSql(sql);
			timeCtx.addSqlPart(sql);
			timeCtx.getExecutableSql();
		}
		log.info("removeFirstCommaWhenSetClause elapsed time. {}",
				DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS").format(
						LocalTime.MIDNIGHT.plus(Duration.between(startTime, Instant.now(Clock.systemDefaultZone())))));
	}

	@Test
	void dontRemoveFirstComma() throws Exception {
		var ctx11 = getExecutionContext(
				"SELECT /* _SQL_ID_ */ setval(/*sequenceName*/'', /*initialValue*/1, false)");
		assertEquals("SELECT /* _SQL_ID_ */ setval(/*sequenceName*/'', /*initialValue*/1, false)",
				ctx11.getExecutableSql());

		var ctx12 = getExecutionContext(
				"SELECT /* _SQL_ID_ */ test.col1, TO_CHAR(FNC_ADD_MONTHS(TO_DATE(/*BIND_0*/'M', 'YYYYMMDD'), - 3), 'YYYYMMDD') AS ymd FROM test WHERE test.month = /*BIND_1*/'M'");
		assertEquals(
				"SELECT /* _SQL_ID_ */ test.col1, TO_CHAR(FNC_ADD_MONTHS(TO_DATE(/*BIND_0*/'M', 'YYYYMMDD'), - 3), 'YYYYMMDD') AS ymd FROM test WHERE test.month = /*BIND_1*/'M'",
				ctx12.getExecutableSql());
	}

	@Test
	void testHasParam() throws Exception {
		var ctx = getExecutionContext("select * from dummy");
		ctx.param("key1", "value1");
		assertTrue(ctx.hasParam("key1"));
		assertFalse(ctx.hasParam("key2"));
	}

	@Test
	void testIfAbsent() throws Exception {
		var ctx = getExecutionContext("select * from dummy");

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
	void testParamOptionalHasValue() throws Exception {
		var ctx = config
				.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		Optional<String> id = Optional.of("testId");
		ctx.param("id", id);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.of("testId");
		ctx.param("id", id, JDBCType.VARCHAR);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.of("testId");
		ctx.param("id", id, JDBCType.VARCHAR.getVendorTypeNumber());

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.of("testId");
		ctx.inOutParam("id", id, JDBCType.VARCHAR);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.of("testId");
		ctx.inOutParam("id", id, JDBCType.VARCHAR.getVendorTypeNumber());

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

	}

	@Test
	void testParamOptionalNullValue() throws Exception {
		var ctx = config
				.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		Optional<String> id = Optional.empty();
		ctx.param("id", id);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.empty();
		ctx.param("id", id, JDBCType.VARCHAR);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.empty();
		ctx.param("id", id, JDBCType.VARCHAR.getVendorTypeNumber());

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.empty();
		ctx.inOutParam("id", id, JDBCType.VARCHAR);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

		ctx = config.context().setSql("select * from test where 1 = 1/*IF id != null */ AND id = /*id*//*END*/");
		id = Optional.empty();
		ctx.inOutParam("id", id, JDBCType.VARCHAR.getVendorTypeNumber());

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("select * from test where 1 = 1 AND id = ?/*id*/"));

	}

	@Test
	void testParamOptionalBean() throws Exception {
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
		var ctx = config.context().setSql(sql);

		var entity = new TestEntity(10, "Taro", 20, Optional.of("memo1"));
		ctx.paramBean(entity);

		transform(ctx);

		assertThat(ctx.getExecutableSql(),
				is("insert into test ( id, name, age, memo) values ( ?/*id*/, ?/*name*/, ?/*age*/, ?/*memo*/)"));

		ctx = config.context().setSql(sql);

		entity = new TestEntity(10, "Taro", 20, Optional.empty());
		ctx.paramBean(entity);

		transform(ctx);

		assertThat(ctx.getExecutableSql(),
				is("insert into test ( id, name, age, memo) values ( ?/*id*/, ?/*name*/, ?/*age*/, ?/*memo*/)"));

		ctx = config.context().setSql(sql);

		ctx.paramBean(null);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("insert into test () values ()"));

	}

	@Test
	void testParamOptionalMap() throws Exception {
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
		var ctx = config.context().setSql(sql);

		Map<String, Object> map = new HashMap<>();
		map.put("id", 10);
		map.put("name", "Taro");
		map.put("age", 20);
		map.put("memo", Optional.of("memo1"));

		ctx.paramMap(map);

		transform(ctx);

		assertThat(ctx.getExecutableSql(),
				is("insert into test ( id, name, age, memo) values ( ?/*id*/, ?/*name*/, ?/*age*/, ?/*memo*/)"));

		ctx = config.context().setSql(sql);

		map = new HashMap<>();
		map.put("id", 10);
		map.put("name", "Taro");
		map.put("age", 20);
		map.put("memo", Optional.empty());
		ctx.paramMap(map);

		transform(ctx);

		assertThat(ctx.getExecutableSql(),
				is("insert into test ( id, name, age, memo) values ( ?/*id*/, ?/*name*/, ?/*age*/, ?/*memo*/)"));

		ctx = config.context().setSql(sql);

		ctx.paramMap(null);

		transform(ctx);

		assertThat(ctx.getExecutableSql(), is("insert into test () values ()"));

	}

	@Test
	void testContext() {
		var ctx = config.context().setSql("select * from test");
		assertEquals(ctx, ctx.context());
	}

	@Test
	void testGetParameterNames() {
		var ctx = config.context().setSql("select * from test")
				.param("param1", 1)
				.param("param2", "2");
		assertThat(((ExecutionContextImpl) ctx).getParameterNames().size(), is(2));
		assertThat(((ExecutionContextImpl) ctx).getParameterNames().iterator().hasNext(), is(true));
		assertThat(((ExecutionContextImpl) ctx).getParameterNames().contains("param1"), is(true));
	}

	@Test
	void testSetRetry() {
		var ctx = config.context().setSql("select * from test")
				.param("param1", 1)
				.param("param2", "2")
				.retry(3);
		assertThat(ctx.getMaxRetryCount(), is(3));
		assertThat(ctx.getRetryWaitTime(), is(0));

		ctx = config.context().setSql("select * from test")
				.param("param1", 1)
				.param("param2", "2")
				.retry(4, 10);
		assertThat(ctx.getMaxRetryCount(), is(4));
		assertThat(ctx.getRetryWaitTime(), is(10));
	}

	@Test
	void testGetDefineColumnType() {
		var ctx = config.context().setSql("select * from test");
		ctx.addDefineColumnType(1, JDBCType.CHAR.getVendorTypeNumber());

		assertThat(ctx.getDefineColumnTypes().get(1), is(JDBCType.CHAR.getVendorTypeNumber()));
	}

	@Test
	void testGetParameterMapperManager() {
		var ctx = config.context().setSql("select * from test");
		assertThat(((ExecutionContextImpl) ctx).getParameterMapperManager(), not(nullValue()));
	}

	@Test
	void testSqlId() {
		var testSqlId = "TEST_SQL_ID";
		var ctx = config.context().setSql("select * from test").sqlId(testSqlId);
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