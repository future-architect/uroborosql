package jp.co.future.uroborosql.event;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.HashMap;
import java.util.stream.IntStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.utils.StringUtils;

public class SecretColumnEventSubscriberUseCbcTest extends BaseEventSubscriberTest {
	private SqlConfig config;

	private SqlAgent agent;

	private SecretColumnEventSubscriber subscriber = new SecretColumnEventSubscriber();

	@BeforeEach
	public void setUp() throws Exception {
		subscriber.setCryptColumnNames(Arrays.asList("PRODUCT_NAME"));
		// 下記コマンドでkeystoreファイル生成
		// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS
		// -alias testexample
		// -storepass password -keypass password -keyalg AES -keysize 128
		subscriber.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnEvent/keystore.jceks");
		subscriber.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		subscriber.setAlias("testexample");
		subscriber.setCharset("UTF-8");
		subscriber.setTransformationType("AES/CBC/PKCS5Padding");

		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SecretColumnEventSubscriberUseCbcTest"))
				.addSubscriber(subscriber)
				.build();

		agent = config.agent();
		try {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		} catch (UroborosqlSQLException ex) {
			ex.printStackTrace();
			assertThat(ex.getMessage(), false);
		}
	}

	@AfterEach
	public void tearDown() {
		agent.close();
	}

	@Test
	public void testSubscriberSettings() {
		assertThat(subscriber.getCharset(), is(StandardCharsets.UTF_8));
		assertThat(subscriber.getTransformationType(), is("AES/CBC/PKCS5Padding"));
		assertThat(subscriber.isSkip(), is(false));
		assertThat(subscriber.isUseIV(), is(true));
		assertThat(subscriber.getSecretKey().getAlgorithm(), is("AES"));
	}

	@Test
	public void testExecuteQuery() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// skip = falseの別のフィルター設定
		var skipSubscriber = new SecretColumnEventSubscriber();
		var skipConfig = UroboroSQL
				.builder(DriverManager.getConnection("jdbc:h2:mem:SecretColumnEventSubscriberUseCbcTest"))
				//				.addSubscriber(skipSubscriber)
				.addSubscriber(e -> e.doOnQuery(skipSubscriber::doQuery))
				.build();

		skipSubscriber.setCryptColumnNames(Arrays.asList("PRODUCT_NAME"));
		skipSubscriber.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnEvent/keystore.jceks");
		skipSubscriber.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		skipSubscriber.setAlias("testexample");
		skipSubscriber.setSkip(true);

		// 復号化しないで取得した場合 (skip = true)
		try (var skipAgent = skipConfig.agent()) {
			try (var result = skipAgent.query("example/select_product")
					.param("product_id", new BigDecimal(0))
					.resultSet()) {

				while (result.next()) {
					assertThat(result.getString("PRODUCT_NAME"), is(not("商品名0")));
				}
			}
		}

		// 復号化して取得した場合 (skip = false)
		try (var result = agent.query("example/select_product")
				.param("product_id", new BigDecimal(0))
				.resultSet()) {

			while (result.next()) {
				assertThat(result.getBigDecimal("PRODUCT_ID"), is(BigDecimal.ZERO));
				assertThat(result.getString("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(result.getString("JAN_CODE"), is("1234567890123"));
				assertThat(result.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
				assertThat(result.getTimestamp("INS_DATETIME"), is(Timestamp.valueOf("2005-12-12 10:10:10.0")));
				assertThat(result.getTimestamp("UPD_DATETIME"), is(Timestamp.valueOf("2005-12-12 10:10:10.0")));
				assertThat(result.getBigDecimal("VERSION_NO"), is(BigDecimal.ZERO));
			}
		}

	}

	@Test
	public void testSecretResultSet01() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var result = agent.query("example/select_product")
				.param("product_id", new BigDecimal(0)).resultSet()) {

			while (result.next()) {
				assertThat(result.getString("PRODUCT_ID"), is("0"));
				assertThat(result.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_KANA_NAME", String.class), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_ID"), is(BigDecimal.ZERO));
				assertThat(result.getObject("PRODUCT_ID", Integer.class), is(0));
			}
		}
	}

	@Test
	public void testSecretResultSet02() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.contextFrom("example/select_product").param("product_id", new BigDecimal(0));
		try (var result = agent.query(ctx)) {

			while (result.next()) {
				assertThat(result.getString("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getObject("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getObject("PRODUCT_NAME", String.class), is("商品名0"));
			}
		}
	}

	@Test
	public void testSecretResultSet03() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.contextFrom("example/select_product").param("product_id", new BigDecimal(0));
		ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

		try (var result = agent.query(ctx)) {
			while (result.next()) {
				result.first();
				assertThat(result.isFirst(), is(true));
				result.previous();
				assertThat(result.isBeforeFirst(), is(true));
				result.next();
				assertThat(result.isBeforeFirst(), is(false));
				result.last();
				assertThat(result.isLast(), is(true));
				result.next();
				assertThat(result.isAfterLast(), is(true));
				result.previous();
				assertThat(result.isAfterLast(), is(false));
				result.beforeFirst();
				assertThat(result.isBeforeFirst(), is(true));
				result.afterLast();
				assertThat(result.isAfterLast(), is(true));
				result.next();

				assertThat(result.isWrapperFor(SecretResultSet.class), is(true));
				assertThat(result.unwrap(SecretResultSet.class).getCharset(), is(Charset.forName("UTF-8")));
				assertThat(result.unwrap(SecretResultSet.class).getCryptColumnNames(),
						is(Arrays.asList("PRODUCT_NAME")));
			}
		}
	}

	@Test
	@Disabled
	public void testSecretResultSetPerformance01() throws Exception {
		for (var i = 0; i < 30; i++) {
			truncateTable(agent, "PRODUCT");
			var startTime = System.currentTimeMillis();
			agent.batch("example/insert_product")
					.paramStream(IntStream.range(1, 100000).mapToObj(count -> new HashMap<String, Object>() {
						{
							put("product_id", count);
							put("product_name", "商品名" + count);
							put("product_kana_name", "ショウヒンメイ" + count);
							put("jan_code", "1234567890123");
							put("product_description", count + "番目の商品");
							put("ins_datetime", "2005-12-12 10:10:10");
							put("upd_datetime", "2005-12-13 10:10:10");
							put("version_no", count);
						}
					})).count();

			var lapTime = System.currentTimeMillis();

			agent.query("example/select_product").stream()
					.forEach(m -> assertThat(m.get("PRODUCT_NAME").toString(), containsString("商品名")));

			var endTime = System.currentTimeMillis();

			System.out.printf("update\t%d\tquery\t%d\ttotal\t%d\r\n", lapTime - startTime, endTime - lapTime,
					endTime - startTime);
		}
	}
}
