package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.testlog.TestAppender;

public class SecretColumnEventSubscriberInitializeTest extends AbstractDbTest {
	private SecretColumnEventSubscriber eventSubscriber;

	@BeforeEach
	public void setUpLocal() throws Exception {
		eventSubscriber = new SecretColumnEventSubscriber();
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
	}

	@AfterEach
	public void tearDownLocal() throws Exception {
		config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
	}

	@Test
	void testInitialize01() throws Exception {
		eventSubscriber.setCryptColumnNames(null);
		config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		assertThat(eventSubscriber.isSkip(), is(true));

		eventSubscriber.setCryptColumnNames(List.of());
		config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		assertThat(eventSubscriber.isSkip(), is(true));

		eventSubscriber.setCryptColumnNames(List.of("product_id", "product_name"));
		eventSubscriber.setCryptParamKeys(List.of("product_id", "product_name"));
		config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		assertThat(eventSubscriber.getCryptParamKeys(), is(List.of("productId", "productName")));
		assertThat(eventSubscriber.getCryptColumnNames(), is(List.of("PRODUCT_ID", "PRODUCT_NAME")));
	}

	@Test
	void testInitialize02() throws Exception {
		eventSubscriber.setCryptColumnNames(List.of("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			eventSubscriber.setKeyStoreFilePath(null);
			config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
			config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		});
		assertThat(log, is(List.of("Invalid KeyStore file path. Path:null")));
		assertThat(eventSubscriber.isSkip(), is(true));
	}

	@Test
	void testInitialize03() throws Exception {
		eventSubscriber.setCryptColumnNames(List.of("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			eventSubscriber.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEventSubscriber/fake.jks");
			config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
			config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		});
		assertThat(log, is(List.of(
				"Not found KeyStore file path. Path:src/test/resources/data/expected/SecretColumnEventSubscriber/fake.jks")));
		assertThat(eventSubscriber.isSkip(), is(true));
	}

	@Test
	void testInitialize04() throws Exception {
		eventSubscriber.setCryptColumnNames(List.of("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			eventSubscriber.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEventSubscriber");
			config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
			config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		});
		assertThat(log, is(List.of(
				"Invalid KeyStore file path. Path:src/test/resources/data/expected/SecretColumnEventSubscriber")));
		assertThat(eventSubscriber.isSkip(), is(true));
	}

	@Test
	void testInitialize05() throws Exception {
		eventSubscriber.setCryptColumnNames(List.of("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			// 下記コマンドでkeystoreファイル生成
			// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS -alias testexample
			// -storepass password -keypass password -keyalg AES -keysize 128
			eventSubscriber.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
			eventSubscriber.setStorePassword(null);
			config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
			config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		});
		assertThat(log, is(List.of(
				"Invalid password for access KeyStore.")));
		assertThat(eventSubscriber.isSkip(), is(true));
	}

	@Test
	void testInitialize06() throws Exception {
		eventSubscriber.setCryptColumnNames(List.of("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			// 下記コマンドでkeystoreファイル生成
			// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS -alias testexample
			// -storepass password -keypass password -keyalg AES -keysize 128
			eventSubscriber.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
			eventSubscriber.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
			eventSubscriber.setAlias(null);
			config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
			config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		});
		assertThat(log, is(List.of("No alias for access KeyStore.")));
		assertThat(eventSubscriber.isSkip(), is(true));
	}
}
