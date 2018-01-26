package jp.co.future.uroborosql.context;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.function.Consumer;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class SqlContextFactoryAutoParameterBinderTest {
	private static SqlConfig config;

	@BeforeClass
	public static void setUpClass() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:SqlContextFactoryAutoParameterBinderTest;DB_CLOSE_DELAY=-1", "sa",
				null).build();

		try (SqlAgent agent = config.agent()) {
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		} catch (UroborosqlSQLException ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	@Before
	public void setUp() throws Exception {
		try (SqlAgent agent = config.agent()) {
			LocalDateTime dt = LocalDateTime.of(2017, 1, 1, 0, 0, 0);

			agent.updateWith("truncate table PRODUCT").count();
			agent.update("example/insert_product")
					.param("product_id", 1)
					.param("product_name", "name")
					.param("product_kana_name", "kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "description")
					.param("ins_datetime", dt)
					.param("upd_datetime", dt)
					.param("version_no", 1)
					.count();
		}
	}

	@Test
	public void testSingleAutoParameterBinder() {
		final LocalDateTime insDate = LocalDateTime.of(2016, 12, 31, 0, 0, 0, 0);
		final LocalDateTime updDate = LocalDateTime.of(2017, 1, 2, 12, 23, 30, 0);
		Consumer<SqlContext> binder = (ctx) -> ctx.param("upd_datetime", updDate);
		config.getSqlContextFactory().addAutoParameterBinder(binder);

		try (SqlAgent agent = config.agent()) {
			int productId = 10;
			// insert
			agent.update("example/insert_product")
					.param("product_id", productId)
					.param("product_name", "name")
					.param("product_kana_name", "kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "description")
					.param("ins_datetime", insDate)
					.param("upd_datetime", insDate)
					.param("version_no", 1)
					.count();

			Map<String, Object> row = agent.query("example/select_product").param("product_id", productId).first();
			assertThat(row.get("INS_DATETIME"), is(Timestamp.valueOf(insDate)));
			// autoPrameterBinderのほうが後で設定されるため、上書きされる）
			assertThat(row.get("UPD_DATETIME"), is(Timestamp.valueOf(updDate)));

			// autoParameterBinderでupd_datetimeが設定される
			row = agent.queryWith("select * from PRODUCT where UPD_DATETIME = /*upd_datetime*/").first();
			assertThat(row.get("INS_DATETIME"), is(Timestamp.valueOf(insDate)));
			assertThat(row.get("UPD_DATETIME"), is(Timestamp.valueOf(updDate)));
		}

		config.getSqlContextFactory().removeAutoParameterBinder(binder);
	}

	@Test
	public void testMultiAutoParameterBinder() {
		final LocalDateTime insDate = LocalDateTime.of(2016, 12, 31, 0, 0, 0, 0);
		final LocalDateTime updDate = LocalDateTime.of(2017, 1, 2, 12, 23, 30, 0);

		SqlContextFactory factory = config.getSqlContextFactory();

		Consumer<SqlContext> binder1 = (ctx) -> ctx.param("ins_datetime", insDate);
		factory.addAutoParameterBinder(binder1);
		Consumer<SqlContext> binder2 = (ctx) -> ctx.param("upd_datetime", updDate);
		factory.addAutoParameterBinder(binder2);
		Consumer<SqlContext> binder3 = (ctx) -> ctx.param("upd_datetime", ((LocalDateTime) ctx.getParam("upd_datetime")
				.getValue()).plusDays(1));
		factory.addAutoParameterBinder(binder3);

		try (SqlAgent agent = config.agent()) {
			int productId = 10;
			// insert
			agent.update("example/insert_product")
					.param("product_id", productId)
					.param("product_name", "name")
					.param("product_kana_name", "kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "description")
					.param("version_no", 1)
					.count();

			Map<String, Object> row = agent.query("example/select_product").param("product_id", productId).first();
			assertThat(row.get("INS_DATETIME"), is(Timestamp.valueOf(insDate)));
			// autoPrameterBinderのほうが後で設定されるため、上書きされる）
			assertThat(row.get("UPD_DATETIME"), is(Timestamp.valueOf(updDate.plusDays(1))));

			// autoParameterBinderでupd_datetimeが設定される。binder3が後で実行されるためbinder3の実行結果になる
			row = agent.queryWith("select * from PRODUCT where UPD_DATETIME = /*upd_datetime*/").first();
			assertThat(row.get("INS_DATETIME"), is(Timestamp.valueOf(insDate)));
			assertThat(row.get("UPD_DATETIME"), is(Timestamp.valueOf(updDate.plusDays(1))));
		}

		factory.removeAutoParameterBinder(binder1);
		factory.removeAutoParameterBinder(binder2);
		factory.removeAutoParameterBinder(binder3);
	}

	@Test
	public void testMultiAutoParameterBinderIfAbsent() {
		final LocalDateTime insDate = LocalDateTime.of(2016, 12, 31, 0, 0, 0, 0);
		final LocalDateTime updDate = LocalDateTime.of(2017, 1, 2, 12, 23, 30, 0);

		SqlContextFactory factory = config.getSqlContextFactory();

		Consumer<SqlContext> binder1 = (ctx) -> ctx.param("ins_datetime", insDate);
		factory.addAutoParameterBinder(binder1);
		Consumer<SqlContext> binder2 = (ctx) -> ctx.param("upd_datetime", updDate);
		factory.addAutoParameterBinder(binder2);
		Consumer<SqlContext> binder3 = (ctx) -> ctx.paramIfAbsent("upd_datetime", updDate.plusDays(1));
		factory.addAutoParameterBinder(binder3);

		try (SqlAgent agent = config.agent()) {
			int productId = 10;
			// insert
			agent.update("example/insert_product")
					.param("product_id", productId)
					.param("product_name", "name")
					.param("product_kana_name", "kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "description")
					.param("version_no", 1)
					.param("ins_datetime", LocalDateTime.of(2016, 1, 1, 0, 0, 0, 0))
					.param("upd_datetime", LocalDateTime.of(2016, 1, 1, 0, 0, 0, 0))
					.count();

			Map<String, Object> row = agent.query("example/select_product").param("product_id", productId).first();
			assertThat(row.get("INS_DATETIME"), is(Timestamp.valueOf(insDate)));
			// autoPrameterBinderのほうが後で設定されるため、上書きされる）
			assertThat(row.get("UPD_DATETIME"), is(Timestamp.valueOf(updDate)));

			// autoParameterBinderでupd_datetimeが設定される。binder3が後で実行されるがIfAbsentなのでbinder2の実行結果になる
			row = agent.queryWith("select * from PRODUCT where UPD_DATETIME = /*upd_datetime*/").first();
			assertThat(row.get("INS_DATETIME"), is(Timestamp.valueOf(insDate)));
			assertThat(row.get("UPD_DATETIME"), is(Timestamp.valueOf(updDate)));
		}

		factory.removeAutoParameterBinder(binder1);
		factory.removeAutoParameterBinder(binder2);
		factory.removeAutoParameterBinder(binder3);
	}
}
