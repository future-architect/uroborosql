package jp.co.future.uroborosql.event;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.testlog.TestAppender;

public class SecretColumnEventInitializeTest {
	private SecretColumnEventSubscriber subscriber;

	@BeforeEach
	public void setUp() throws Exception {
		subscriber = new SecretColumnEventSubscriber();
	}

	@Test
	public void testInitialize01() throws Exception {
		subscriber.setCryptColumnNames(null);
		subscriber.initialize();
		assertThat(subscriber.isSkip(), is(true));

		subscriber.setCryptColumnNames(Arrays.asList());
		subscriber.initialize();
		assertThat(subscriber.isSkip(), is(true));

		subscriber.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		subscriber.setCryptParamKeys(Arrays.asList("product_id", "product_name"));
		subscriber.initialize();
		assertThat(subscriber.getCryptParamKeys(), is(Arrays.asList("productId", "productName")));
		assertThat(subscriber.getCryptColumnNames(), is(Arrays.asList("PRODUCT_ID", "PRODUCT_NAME")));
	}

	@Test
	public void testInitialize02() throws Exception {
		subscriber.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			subscriber.setKeyStoreFilePath(null);
			subscriber.initialize();
		});
		assertThat(log, is(Arrays.asList("Invalid KeyStore file path. Path:null")));
		assertThat(subscriber.isSkip(), is(true));
	}

	@Test
	public void testInitialize03() throws Exception {
		subscriber.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			subscriber.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEvent/fake.jks");
			subscriber.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Not found KeyStore file path. Path:src/test/resources/data/expected/SecretColumnEvent/fake.jks")));
		assertThat(subscriber.isSkip(), is(true));
	}

	@Test
	public void testInitialize04() throws Exception {
		subscriber.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			subscriber.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEvent");
			subscriber.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Invalid KeyStore file path. Path:src/test/resources/data/expected/SecretColumnEvent")));
		assertThat(subscriber.isSkip(), is(true));
	}

	@Test
	public void testInitialize05() throws Exception {
		subscriber.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			// 下記コマンドでkeystoreファイル生成
			// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS -alias testexample
			// -storepass password -keypass password -keyalg AES -keysize 128
			subscriber.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEvent/keystore.jceks");
			subscriber.setStorePassword(null);
			subscriber.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Invalid password for access KeyStore.")));
		assertThat(subscriber.isSkip(), is(true));
	}

	@Test
	public void testInitialize06() throws Exception {
		subscriber.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			// 下記コマンドでkeystoreファイル生成
			// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS -alias testexample
			// -storepass password -keypass password -keyalg AES -keysize 128
			subscriber.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEvent/keystore.jceks");
			subscriber.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
			subscriber.setAlias(null);
			subscriber.initialize();
		});
		assertThat(log, is(Arrays.asList("No alias for access KeyStore.")));
		assertThat(subscriber.isSkip(), is(true));
	}
}
