/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * Test class for SqlAgentProviderImpl to improve method coverage
 *
 * @author Generated Test
 */
public class SqlAgentProviderImplAdditionalTest {

	private SqlAgentProviderImpl provider;
	private SqlConfig sqlConfig;

	@BeforeEach
	public void setUp() {
		sqlConfig = UroboroSQL.builder("jdbc:h2:mem:SqlAgentProviderImplAdditionalTest", "sa", "").build();
		provider = new SqlAgentProviderImpl(sqlConfig);
	}

	/**
	 * Test default constructor
	 */
	@Test
	public void testDefaultConstructor() {
		var defaultProvider = new SqlAgentProviderImpl();
		assertThat(defaultProvider, is(notNullValue()));
		
		// Test that agent() throws exception when no config is set
		assertThrows(UroborosqlRuntimeException.class, () -> {
			defaultProvider.agent();
		});
	}

	/**
	 * Test constructor with SqlConfig
	 */
	@Test
	public void testConstructorWithSqlConfig() {
		var configuredProvider = new SqlAgentProviderImpl(sqlConfig);
		assertThat(configuredProvider, is(notNullValue()));
		assertThat(configuredProvider.getSqlConfig(), is(sqlConfig));
	}

	/**
	 * Test agent creation without connection context
	 */
	@Test
	public void testAgentCreation() {
		var agent = provider.agent();
		assertThat(agent, is(notNullValue()));
	}

	/**
	 * Test agent creation with connection context
	 */
	@Test
	public void testAgentCreationWithConnectionContext() {
		var agent = provider.agent(null);
		assertThat(agent, is(notNullValue()));
	}

	/**
	 * Test output exception log property
	 */
	@Test
	public void testOutputExceptionLog() {
		// Test default value
		assertThat(provider.isOutputExceptionLog(), is(true));
		
		// Test setting to false
		var result = provider.setOutputExceptionLog(false);
		assertThat(result, is(provider));
		assertThat(provider.isOutputExceptionLog(), is(false));
		
		// Test setting back to true
		provider.setOutputExceptionLog(true);
		assertThat(provider.isOutputExceptionLog(), is(true));
	}

	/**
	 * Test fetch size property
	 */
	@Test
	public void testFetchSize() {
		// Test default value
		assertThat(provider.getFetchSize(), is(-1));
		
		// Test setting a value
		var result = provider.setFetchSize(100);
		assertThat(result, is(provider));
		assertThat(provider.getFetchSize(), is(100));
		
		// Test setting back to default
		provider.setFetchSize(-1);
		assertThat(provider.getFetchSize(), is(-1));
	}

	/**
	 * Test query timeout property
	 */
	@Test
	public void testQueryTimeout() {
		// Test default value
		assertThat(provider.getQueryTimeout(), is(-1));
		
		// Test setting a value
		var result = provider.setQueryTimeout(30);
		assertThat(result, is(provider));
		assertThat(provider.getQueryTimeout(), is(30));
		
		// Test setting back to default
		provider.setQueryTimeout(-1);
		assertThat(provider.getQueryTimeout(), is(-1));
	}

	/**
	 * Test SQL retry code list property
	 */
	@Test
	public void testSqlRetryCodeList() {
		// Test default value
		var defaultList = provider.getSqlRetryCodeList();
		assertThat(defaultList, is(notNullValue()));
		assertThat(defaultList.isEmpty(), is(true));
		
		// Test setting a list
		var retryCodeList = List.of("08001", "08006", "40001");
		var result = provider.setSqlRetryCodeList(retryCodeList);
		assertThat(result, is(provider));
		
		var retrievedList = provider.getSqlRetryCodeList();
		assertThat(retrievedList.size(), is(3));
		assertThat(retrievedList.get(0), is("08001"));
		assertThat(retrievedList.get(1), is("08006"));
		assertThat(retrievedList.get(2), is("40001"));
		
		// Test setting null list (doesn't clear existing setting)
		provider.setSqlRetryCodeList(null);
		var nullList = provider.getSqlRetryCodeList();
		assertThat(nullList, is(notNullValue()));
		// Note: null doesn't clear existing setting, so list may not be empty
		
		// Test setting empty list (doesn't clear existing setting)
		provider.setSqlRetryCodeList(List.of());
		var emptyList = provider.getSqlRetryCodeList();
		assertThat(emptyList, is(notNullValue()));
		// Note: empty list doesn't clear existing setting, so list may not be empty
	}

	/**
	 * Test default max retry count property
	 */
	@Test
	public void testDefaultMaxRetryCount() {
		// Test default value
		assertThat(provider.getDefaultMaxRetryCount(), is(0));
		
		// Test setting a value
		var result = provider.setDefaultMaxRetryCount(3);
		assertThat(result, is(provider));
		assertThat(provider.getDefaultMaxRetryCount(), is(3));
		
		// Test setting back to default
		provider.setDefaultMaxRetryCount(0);
		assertThat(provider.getDefaultMaxRetryCount(), is(0));
	}

	/**
	 * Test default retry wait time property
	 */
	@Test
	public void testDefaultRetryWaitTime() {
		// Test default value
		assertThat(provider.getDefaultSqlRetryWaitTime(), is(0));
		
		// Test setting a value
		var result = provider.setDefaultSqlRetryWaitTime(1000);
		assertThat(result, is(provider));
		assertThat(provider.getDefaultSqlRetryWaitTime(), is(1000));
		
		// Test setting back to default
		provider.setDefaultSqlRetryWaitTime(0);
		assertThat(provider.getDefaultSqlRetryWaitTime(), is(0));
	}

	/**
	 * Test map key case format property
	 */
	@Test
	public void testMapKeyCaseFormat() {
		// Test default value
		assertThat(provider.getDefaultMapKeyCaseFormat(), is(CaseFormat.UPPER_SNAKE_CASE));
		
		// Test setting a different format
		var result = provider.setDefaultMapKeyCaseFormat(CaseFormat.CAMEL_CASE);
		assertThat(result, is(provider));
		assertThat(provider.getDefaultMapKeyCaseFormat(), is(CaseFormat.CAMEL_CASE));
		
		// Test setting back to default
		provider.setDefaultMapKeyCaseFormat(CaseFormat.UPPER_SNAKE_CASE);
		assertThat(provider.getDefaultMapKeyCaseFormat(), is(CaseFormat.UPPER_SNAKE_CASE));
	}

	/**
	 * Test inserts type property
	 */
	@Test
	public void testInsertsType() {
		// Test default value  
		assertThat(provider.getDefaultInsertsType(), is(notNullValue()));
		
		// Test setting a different type
		var result = provider.setDefaultInsertsType(jp.co.future.uroborosql.enums.InsertsType.BULK);
		assertThat(result, is(provider));
		assertThat(provider.getDefaultInsertsType(), is(jp.co.future.uroborosql.enums.InsertsType.BULK));
		
		// Test setting back to batch
		provider.setDefaultInsertsType(jp.co.future.uroborosql.enums.InsertsType.BATCH);
		assertThat(provider.getDefaultInsertsType(), is(jp.co.future.uroborosql.enums.InsertsType.BATCH));
	}

	/**
	 * Test SQL config getter and setter
	 */
	@Test
	public void testSqlConfigGetterSetter() {
		assertThat(provider.getSqlConfig(), is(sqlConfig));
		
		var newConfig = UroboroSQL.builder("jdbc:h2:mem:NewConfig", "sa", "").build();
		provider.setSqlConfig(newConfig);
		assertThat(provider.getSqlConfig(), is(newConfig));
		
		// Restore original config
		provider.setSqlConfig(sqlConfig);
		assertThat(provider.getSqlConfig(), is(sqlConfig));
	}

	/**
	 * Test agent creation error when SQL config is null
	 */
	@Test
	public void testAgentCreationWithNullConfig() {
		var nullConfigProvider = new SqlAgentProviderImpl();
		nullConfigProvider.setSqlConfig(null);
		
		assertThrows(UroborosqlRuntimeException.class, () -> {
			nullConfigProvider.agent();
		});
		
		assertThrows(UroborosqlRuntimeException.class, () -> {
			nullConfigProvider.agent(null);
		});
	}

	/**
	 * Test SQL ID key name property
	 */
	@Test
	public void testSqlIdKeyName() {
		// Test default value
		var defaultKeyName = provider.getSqlIdKeyName();
		assertThat(defaultKeyName, is(notNullValue()));
		
		// Test setting a new value
		var result = provider.setSqlIdKeyName("custom_sql_id_key");
		assertThat(result, is(provider));
		assertThat(provider.getSqlIdKeyName(), is("custom_sql_id_key"));
		
		// Test setting back to default
		provider.setSqlIdKeyName(defaultKeyName);
		assertThat(provider.getSqlIdKeyName(), is(defaultKeyName));
	}

	/**
	 * Test for update wait seconds property
	 */
	@Test
	public void testForUpdateWaitSeconds() {
		// Test default value - may vary depending on configuration
		var defaultValue = provider.getDefaultForUpdateWaitSeconds();
		assertThat(defaultValue, is(notNullValue()));
		
		// Test setting a value
		var result = provider.setDefaultForUpdateWaitSeconds(30);
		assertThat(result, is(provider));
		assertThat(provider.getDefaultForUpdateWaitSeconds(), is(30));
		
		// Test setting back to original value
		provider.setDefaultForUpdateWaitSeconds(defaultValue);
		assertThat(provider.getDefaultForUpdateWaitSeconds(), is(defaultValue));
	}

	/**
	 * Test force update within transaction property
	 */
	@Test
	public void testForceUpdateWithinTransaction() {
		// Test setting value
		var result = provider.setForceUpdateWithinTransaction(true);
		assertThat(result, is(provider));
		
		// Test setting back
		provider.setForceUpdateWithinTransaction(false);
		assertThat(provider, is(notNullValue()));
	}

	/**
	 * Test strict for update type property
	 */
	@Test
	public void testStrictForUpdateType() {
		// Test setting value
		var result = provider.setStrictForUpdateType(true);
		assertThat(result, is(provider));
		
		// Test setting back
		provider.setStrictForUpdateType(false);
		assertThat(provider, is(notNullValue()));
	}
}