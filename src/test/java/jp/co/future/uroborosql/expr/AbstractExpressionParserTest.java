package jp.co.future.uroborosql.expr;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.DriverManager;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.context.ExecutionContextProviderImpl;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.log.PerformanceLogger;
import jp.co.future.uroborosql.parser.SqlParser;
import jp.co.future.uroborosql.parser.SqlParserImpl;

/**
 * ExpressionParserのテスト親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractExpressionParserTest implements PerformanceLogger {
	protected SqlConfig sqlConfig;
	protected ExecutionContext ctx;
	protected static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS");

	@BeforeEach
	public void setUp() throws Exception {
		sqlConfig = UroboroSQL
				.builder(DriverManager
						.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1"))
				.setExecutionContextProvider(new ExecutionContextProviderImpl()
						.setEnumConstantPackageNames(List.of(TestEnum1.class.getPackage().getName())))
				.setExpressionParser(getExpressionParser())
				.build();

		ctx = sqlConfig.getExecutionContextProvider().createExecutionContext();
	}

	/**
	 * テスト対象のExpressionPerserを取得する.
	 *
	 * @return テスト対象のExpressionPerser
	 */
	protected abstract ExpressionParser getExpressionParser();

	/**
	 * パフォーマンステストのログに出力するヘッダ文字列を取得する.
	 *
	 * @return ヘッダ文字列
	 */
	protected abstract String getPerformanceHeader();

	private void sqlAssertion(final String original, final String expected) {
		SqlParser parser = new SqlParserImpl(original, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);

		var transformer = parser.parse();
		transformer.transform(ctx);
		var transformed = ctx.getExecutableSql().trim().replaceAll("\\s+", " ");
		assertThat("結果が一致しません。", transformed, is(expected));
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	void testElif() throws Exception {
		var sql = "/*IF false*/1=1/*ELIF true*/2=2/*ELSE*/3=3/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF TRUE ELSE FALSE
	@Test
	void testFunction() throws Exception {
		ctx.param("param1", 1);
		var sql = "/*IF SF.isNotEmpty(param1)*//*param1*//*ELSE*/false/*END*/";
		var sql2 = "?/*param1*/";
		sqlAssertion(sql, sql2);
	}

	// IF TRUE ELSE FALSE
	@Test
	void testFunctionWithOptional() throws Exception {
		ctx.param("param1", Optional.of("text"));
		var sql = "/*IF SF.isNotEmpty(param1)*//*param1*//*ELSE*/false/*END*/";
		var sql2 = "?/*param1*/";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELSE TRUE
	@Test
	void testFunctionWithOptionalNull() throws Exception {
		ctx.param("param1", Optional.ofNullable(null));
		var sql = "/*IF SF.isNotEmpty(param1)*//*param1*//*ELSE*/false/*END*/";
		var sql2 = "false";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELSE TRUE
	@Test
	void testFunctionWithOptionalEmpty() throws Exception {
		ctx.param("param1", Optional.of(""));
		var sql = "/*IF SF.isNotEmpty(param1)*//*param1*//*ELSE*/false/*END*/";
		var sql2 = "false";
		sqlAssertion(sql, sql2);
	}

	// IF TRUE ELSE FALSE
	@Test
	void testFunctionWithOptionalBlank() throws Exception {
		ctx.param("param1", Optional.of(" "));
		var sql = "/*IF SF.isNotEmpty(param1)*//*param1*//*ELSE*/false/*END*/";
		var sql2 = "?/*param1*/";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	void testEscChar() throws Exception {
		ctx.param(Dialect.PARAM_KEY_ESCAPE_CHAR, sqlConfig.getDialect().getEscapeChar());
		var sql = "select * from test like 'a%' escape /*#ESC_CHAR*/";
		var sql2 = "select * from test like 'a%' escape '$'/*#ESC_CHAR*/";
		sqlAssertion(sql, sql2);
	}

	@Test
	void testIsPropertyAccess() {
		var parser = sqlConfig.getExpressionParser();
		assertThat(parser.isPropertyAccess(null), is(true));
		assertThat(parser.isPropertyAccess(""), is(true));
		assertThat(parser.isPropertyAccess("SF.isEmpty(param1)"), is(false));
		assertThat(parser.isPropertyAccess("SF.isEmpty(param1"), is(true));
		assertThat(parser.isPropertyAccess("(ESC_CHAR)1"), is(false));
	}

	@Test
	void testGetValue() {
		ctx.param("param1", 1);
		var parser = sqlConfig.getExpressionParser();
		assertThat(parser.parse("param1").getValue(ctx), is(1));
		assertThat(parser.parse("param2").getValue(ctx), is(nullValue()));
	}

	@Test
	void testGetValueWithOptional() {
		ctx.param("paramNull", Optional.ofNullable(null));
		ctx.param("param1", Optional.of(1));
		ctx.param("paramEmpty", Optional.of(""));
		ctx.param("paramBlank", Optional.of(" "));
		var parser = sqlConfig.getExpressionParser();
		assertThat(parser.parse("paramNull").getValue(ctx), is(Optional.empty()));
		assertThat(parser.parse("param1").getValue(ctx), is(Optional.of(1)));
		assertThat(parser.parse("param2").getValue(ctx), is(nullValue()));
		assertThat(parser.parse("paramEmpty").getValue(ctx), is(Optional.of("")));
		assertThat(parser.parse("paramBlank").getValue(ctx), is(Optional.of(" ")));
	}

	@Test
	void testDumpNode() {
		ctx.param("param1", 1);
		var parser = sqlConfig.getExpressionParser();

		assertThat(parser.parse("param1 != null and param2 == null").dumpNode(ctx).toString(),
				is("param1:[1],param2:[null],"));
		assertThat(parser.parse("SF.isNotEmpty(param1) and SF.isEmpty(param2)").dumpNode(ctx).toString(),
				is("param1:[1],param2:[null],"));
	}

	@Test
	void testCollectParams() {
		ctx.param("param1", 1);
		var parser = sqlConfig.getExpressionParser();

		Set<String> params = new HashSet<>();
		parser.parse("param1 != null and param2 == null").collectParams(params);
		assertThat(params, is(Matchers.contains("param1", "param2")));

		params = new HashSet<>();
		parser.parse("SF.isNotEmpty(param1) and SF.isEmpty(param2)").collectParams(params);
		assertThat(params, is(Matchers.contains("param1", "param2")));
	}

	@Test
	void testPerformance() {
		if (PERFORMANCE_LOG.isDebugEnabled()) {
			atDebug(PERFORMANCE_LOG)
					.setMessage("\r\n{}")
					.addArgument(this::getPerformanceHeader)
					.log();
			var parser = getExpressionParser();
			parser.setSqlConfig(sqlConfig);
			assertThat(parser.getSqlConfig(), is(sqlConfig));

			parser.initialize();
			for (var i = 0; i < 10; i++) {
				var start = Instant.now();
				for (var j = 0; j < 20000; j++) {
					var context = sqlConfig.getExecutionContextProvider().createExecutionContext();
					var expr = parser.parse("param" + j + " == null");
					expr.getValue(context);
				}
				atDebug(PERFORMANCE_LOG)
						.setMessage("No{}:{}")
						.addArgument(i)
						.addArgument(
								() -> formatter.format(LocalTime.MIDNIGHT.plus(Duration.between(start, Instant.now()))))
						.log();
			}
		}
	}
}
