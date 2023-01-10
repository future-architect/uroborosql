package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.DriverManager;
import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.SecretColumnEventSubscriber;
import jp.co.future.uroborosql.event.subscriber.SqlFilterManager;
import jp.co.future.uroborosql.testlog.TestAppender;

public class SecretColumnSqlFilterInitializeTest {
	private SqlConfig config;
	private SqlFilterManager sqlFilterManager;
	private SecretColumnEventSubscriber filter;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterInitializeTest"))
				.build();
		sqlFilterManager = config.getSqlFilterManager();
		filter = new SecretColumnEventSubscriber();
		sqlFilterManager.addSqlFilter(filter);
	}

	@Test
	void testInitialize01() throws Exception {
		filter.setCryptColumnNames(null);
		sqlFilterManager.initialize();
		assertThat(filter.isSkip(), is(true));

		filter.setCryptColumnNames(Arrays.asList());
		sqlFilterManager.initialize();
		assertThat(filter.isSkip(), is(true));

		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		filter.setCryptParamKeys(Arrays.asList("product_id", "product_name"));
		sqlFilterManager.initialize();
		assertThat(filter.getCryptParamKeys(), is(Arrays.asList("productId", "productName")));
		assertThat(filter.getCryptColumnNames(), is(Arrays.asList("PRODUCT_ID", "PRODUCT_NAME")));
	}

	@Test
	void testInitialize02() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList("Invalid KeyStore file path. Path:null")));
		assertThat(filter.isSkip(), is(true));
	}

	@Test
	void testInitialize03() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEventSubscriber/fake.jks");
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Not found KeyStore file path. Path:src/test/resources/data/expected/SecretColumnEventSubscriber/fake.jks")));
		assertThat(filter.isSkip(), is(true));
	}

	@Test
	void testInitialize04() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEventSubscriber");
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Invalid KeyStore file path. Path:src/test/resources/data/expected/SecretColumnEventSubscriber")));
		assertThat(filter.isSkip(), is(true));
	}

	@Test
	void testInitialize05() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			// 下記コマンドでkeystoreファイル生成
			// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS -alias testexample
			// -storepass password -keypass password -keyalg AES -keysize 128
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
			filter.setStorePassword(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Invalid password for access KeyStore.")));
		assertThat(filter.isSkip(), is(true));
	}

	@Test
	void testInitialize06() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			// 下記コマンドでkeystoreファイル生成
			// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS -alias testexample
			// -storepass password -keypass password -keyalg AES -keysize 128
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
			filter.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
			filter.setAlias(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList("No alias for access KeyStore.")));
		assertThat(filter.isSkip(), is(true));
	}
}
