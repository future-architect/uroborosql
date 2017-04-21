package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.DriverManager;
import java.util.Arrays;

import org.junit.Test;

import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;

public class SecretColumnSqlFilterInitializeTest {
	private SqlConfig config;

	@Test
	public void testInitialize() throws Exception {
		config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterInitializeTest"));
		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		SecretColumnSqlFilter filter = new SecretColumnSqlFilter();
		sqlFilterManager.addSqlFilter(filter);
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
}
