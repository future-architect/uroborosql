package jp.co.future.uroborosql.expr.ognl;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.DriverManager;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.expr.Expression;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.parser.ContextTransformer;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;

public class OgnlExpressionParserTest {
	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(OgnlExpressionParserTest.class);

	private SqlConfig sqlConfig;
	private SqlContext ctx;
	private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS");

	@Before
	public void setUp() throws Exception {
		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.setSqlContextFactory(new SqlContextFactoryImpl()
						.setEnumConstantPackageNames(Arrays.asList(TestEnum1.class.getPackage().getName())))
				.setExpressionParser(new OgnlExpressionParser())
				.build();

		ctx = sqlConfig.getSqlContextFactory().createSqlContext();
	}

	private void sqlAssertion(final String original, final String expected) {
		SqlParser parser = new SqlParserImpl(original, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);

		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		String transformed = ctx.getExecutableSql().trim().replaceAll("\\s+", " ");
		assertThat("結果が一致しません。", transformed, is(expected));
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElif() throws Exception {
		String sql = "/*IF false*/1=1/*ELIF true*/2=2/*ELSE*/3=3/*END*/";
		String sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testFunction() throws Exception {
		ctx.param("param1", 1);
		String sql = "/*IF SF.isNotEmpty(param1)*//*param1*//*ELSE*/false/*END*/";
		String sql2 = "?/*param1*/";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testEscChar() throws Exception {
		ctx.param("param1", 1);
		ctx.param(Dialect.PARAM_KEY_ESCAPE_CHAR, sqlConfig.getDialect().getEscapeChar());
		String sql = "select * from test like 'a%' escape /*#ESC_CHAR*/";
		String sql2 = "select * from test like 'a%' escape '$'/*#ESC_CHAR*/";
		sqlAssertion(sql, sql2);
	}

	@Test
	public void testIsPropertyAccess() {
		ExpressionParser parser = sqlConfig.getExpressionParser();
		assertThat(parser.isPropertyAccess(null), is(true));
		assertThat(parser.isPropertyAccess(""), is(true));
		assertThat(parser.isPropertyAccess("SF.isEmpty(param1)"), is(false));
		assertThat(parser.isPropertyAccess("SF.isEmpty(param1"), is(true));
		assertThat(parser.isPropertyAccess("(ESC_CHAR)1"), is(false));
	}

	@Test
	public void testGetValue() {
		ctx.param("param1", 1);
		ExpressionParser parser = sqlConfig.getExpressionParser();
		assertThat(parser.parse("param1").getValue(ctx), is(1));
		assertThat(parser.parse("param2").getValue(ctx), is(nullValue()));
	}

	@Test
	public void testDumpNode() {
		ctx.param("param1", 1);
		ExpressionParser parser = sqlConfig.getExpressionParser();

		assertThat(parser.parse("param1 != null and param2 == null").dumpNode(ctx).toString(),
				is("param1:[1],param2:[null],"));
		assertThat(parser.parse("SF.isNotEmpty(param1) and SF.isEmpty(param2)").dumpNode(ctx).toString(),
				is("param1:[1],param2:[null],"));
	}

	@Test
	public void testCollectParams() {
		ctx.param("param1", 1);
		ExpressionParser parser = sqlConfig.getExpressionParser();

		Set<String> params = new HashSet<>();
		parser.parse("param1 != null and param2 == null").collectParams(params);
		assertThat(params, is(Matchers.contains("param1", "param2")));

		params = new HashSet<>();
		parser.parse("SF.isNotEmpty(param1) and SF.isEmpty(param2)").collectParams(params);
		assertThat(params, is(Matchers.contains("param1", "param2")));
	}

	@Test
	public void testPerformance() {
		if (log.isTraceEnabled()) {
			log.trace("\r\nOGNL");
			ExpressionParser parser = new OgnlExpressionParser();
			parser.setSqlConfig(sqlConfig);
			parser.initialize();
			for (int i = 0; i < 10; i++) {
				Instant start = Instant.now();
				for (int j = 0; j < 20000; j++) {
					SqlContext context = sqlConfig.getSqlContextFactory().createSqlContext();
					Expression expr = parser.parse("param" + j + " == null");
					expr.getValue(context);
				}
				log.trace("No" + i + ":"
						+ formatter.format(LocalTime.MIDNIGHT.plus(Duration.between(start, Instant.now()))));
			}
		}
	}

}
