package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.DriverManager;
import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.testlog.TestAppender;

public class SecretColumnSqlFilterInitializeTest {
	private SqlConfig config;
	private SqlFilterManager sqlFilterManager;
	private SecretColumnSqlFilter filter;

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig
				.getConfig(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterInitializeTest"));
		sqlFilterManager = config.getSqlFilterManager();
		filter = new SecretColumnSqlFilter();
		sqlFilterManager.addSqlFilter(filter);
	}

	@Test
	public void testInitialize01() throws Exception {
		filter.setCryptColumnNames(null);
		sqlFilterManager.initialize();
		assertThat(filter.isSkipFilter(), is(true));

		filter.setCryptColumnNames(Arrays.asList());
		sqlFilterManager.initialize();
		assertThat(filter.isSkipFilter(), is(true));

		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		sqlFilterManager.initialize();
		assertThat(filter.getCryptParamKeys(), is(Arrays.asList("productId", "productName")));
		assertThat(filter.getCryptColumnNames(), is(Arrays.asList("PRODUCT_ID", "PRODUCT_NAME")));
	}

	@Test
	public void testInitialize02() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		List<String> log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList("Invalid KeyStore file path. Path:null")));
		assertThat(filter.isSkipFilter(), is(true));
	}

	@Test
	public void testInitialize03() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		List<String> log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnSqlFilterInitialize/fake.jks");
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Not found KeyStore file path. Path:src/test/resources/data/expected/SecretColumnSqlFilterInitialize/"
				+ "fake.jks")));
		assertThat(filter.isSkipFilter(), is(true));
	}

	@Test
	public void testInitialize04() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		List<String> log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnSqlFilterInitialize");
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Invalid KeyStore file path. Path:src/test/resources/data/expected/SecretColumnSqlFilterInitialize")));
		assertThat(filter.isSkipFilter(), is(true));
	}

	@Test
	public void testInitialize05() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		List<String> log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnSqlFilterInitialize/keystore.jceks");
			filter.setStorePassword(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Invalid password for access KeyStore.")));
		assertThat(filter.isSkipFilter(), is(true));
	}


	@Test
	public void testInitialize06() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		List<String> log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnSqlFilterInitialize/keystore.jceks");
			filter.setStorePassword("cGFzc3dvcmQ=");
			filter.setAlias(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"KeyStoreにアクセスするためのエイリアスが指定されていません。",
				"No alias for access KeyStore.")));
		assertThat(filter.isSkipFilter(), is(true));
	}

	@Test
	public void testInitialize07() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		filter.setKeyStoreFilePath(
				"src/test/resources/data/expected/SecretColumnSqlFilterInitialize/keystore.jceks");
		filter.setStorePassword("cGFzc3dvcmQ=");
		filter.setAlias("testexample");
		sqlFilterManager.initialize();

	}
}
