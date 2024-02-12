package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.stream.IntStream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;

/**
 * Test case of SecretColumnEventSubscriber when using CBC mode
 *
 * @author hoshi
 *
 */
public class SecretColumnEventSubscriberUseCbcTest extends AbstractEventSubscriberTest {

	private SecretColumnEventSubscriber eventSubscriber;

	@BeforeEach
	public void setUpLocal() throws Exception {
		eventSubscriber = new SecretColumnEventSubscriber();

		eventSubscriber.setCryptColumnNames(List.of("PRODUCT_NAME"));
		// 下記コマンドでkeystoreファイル生成
		// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS
		// -alias testexample
		// -storepass password -keypass password -keyalg AES -keysize 128
		eventSubscriber
				.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
		eventSubscriber.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		eventSubscriber.setAlias("testexample");
		eventSubscriber.setCharset("UTF-8");
		eventSubscriber.setTransformationType("AES/CBC/PKCS5Padding");
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
	}

	@Test
	void testFilterSettings() {
		assertThat(eventSubscriber.getCharset(), is(StandardCharsets.UTF_8));
		assertThat(eventSubscriber.getTransformationType(), is("AES/CBC/PKCS5Padding"));
		assertThat(eventSubscriber.isSkip(), is(false));
		assertThat(eventSubscriber.isUseIV(), is(true));
		assertThat(eventSubscriber.getSecretKey().getAlgorithm(), is("AES"));
	}

	@Test
	void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// skipFilter = falseの別のフィルター設定
		var skipConfig = UroboroSQL
				.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.build();
		var skipEventSubscriber = new SecretColumnEventSubscriber();

		skipEventSubscriber.setCryptColumnNames(List.of("PRODUCT_NAME"));
		skipEventSubscriber
				.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
		skipEventSubscriber.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		skipEventSubscriber.setAlias("testexample");
		skipEventSubscriber.setSkip(true);
		skipConfig.getEventListenerHolder().addEventSubscriber(skipEventSubscriber);

		// 復号化しないで取得した場合 (skipFilter = true)
		try (var result = skipConfig.agent().query("example/select_product")
				.param("product_id", new BigDecimal(0))
				.resultSet()) {

			while (result.next()) {
				assertThat(result.getString("PRODUCT_NAME"), is(not("商品名0")));
			}
		}

		// 復号化して取得した場合 (skipFilter = false)
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
	void testSecretResultSet01() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var result = agent.query("example/select_product")
				.param("product_id", new BigDecimal(0))
				.resultSet()) {

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
	void testSecretResultSet02() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var result = agent.query(agent.context().setSqlName("example/select_product")
				.param("product_id", new BigDecimal(0)))) {
			while (result.next()) {
				assertThat(result.getString("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getObject("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getObject("PRODUCT_NAME", String.class), is("商品名0"));
			}
		}
	}

	@Test
	void testSecretResultSet03() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.context().setSqlName("example/select_product")
				.param("product_id", new BigDecimal(0));
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
						is(List.of("PRODUCT_NAME")));
			}
		}
	}

	@Test
	@Disabled
	void testSecretResultSetPerformance01() throws Exception {
		for (var i = 0; i < 30; i++) {
			truncateTable("PRODUCT");
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
