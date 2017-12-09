package jp.co.future.uroborosql;

import jp.co.future.uroborosql.config.SqlConfig;

import static org.junit.Assert.*;

import jp.co.future.uroborosql.utils.CaseFormat;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

public class SqlAgentFactoryImplTest {
	private static SqlConfig config = null;
	private static SqlAgentFactoryImpl factory = null;

	@BeforeClass
	public static void setUpClass() {
		config = UroboroSQL.builder("jdbc:h2:mem:SqlAgentFactoryImplTest", "sa", "").build();
		factory = (SqlAgentFactoryImpl) config.getSqlAgentFactory();
	}

	@Test
	public void createSqlAgent() throws Exception {
		assertNotNull(factory.createSqlAgent());
	}

	@Test
	public void getConnectionSupplier() throws Exception {
		assertNotNull(factory.getConnectionSupplier());
	}

	@Test
	public void getSqlManager() throws Exception {
		assertNotNull(factory.getSqlManager());
	}

	@Test
	public void getSqlFilterManager() throws Exception {
		assertNotNull(factory.getSqlFilterManager());
	}

	@Test
	public void getEntityHandler() throws Exception {
		assertNotNull(factory.getEntityHandler());
	}

	@Test
	public void setSqlCOnfig() throws Exception {
		SqlConfig workConfig = UroboroSQL.builder("jdbc:h2:mem:SqlAgentFactoryImplTest2", "sa", "").build();
		SqlAgentFactoryImpl workFactory = (SqlAgentFactoryImpl) workConfig.getSqlAgentFactory();
		workFactory.setSqlConfig(null);

		assertNull(workFactory.createSqlAgent());
		assertNull(workFactory.getConnectionSupplier());
		assertNull(workFactory.getSqlManager());
		assertNull(workFactory.getSqlFilterManager());
		assertNull(workFactory.getEntityHandler());
	}

	@Test
	public void setOutputExceptionLog() throws Exception {
		factory.setOutputExceptionLog(true);
		assertTrue(factory.isOutputExceptionLog());
	}

	@Test
	public void setFetchSize() throws Exception {
		factory.setFetchSize(1000);
		assertEquals(1000, factory.getFetchSize());
	}

	@Test
	public void setQueryTimeout() throws Exception {
		factory.setQueryTimeout(10);
		assertEquals(10, factory.getQueryTimeout());
	}

	@Test
	public void setSqlRetryCodeList() throws Exception {
		// normal
		List<String> retryCodes = Arrays.asList("1", "2", "3");
		factory.setSqlRetryCodeList(retryCodes);
		assertArrayEquals(retryCodes.toArray(), factory.getSqlRetryCodeList().toArray());

		// null
		factory.setSqlRetryCodeList(null);
		assertArrayEquals(retryCodes.toArray(), factory.getSqlRetryCodeList().toArray());

		// empty List
		factory.setSqlRetryCodeList(Collections.emptyList());
		assertArrayEquals(retryCodes.toArray(), factory.getSqlRetryCodeList().toArray());
	}

	@Test
	public void setSqlRetryCodeListNull() throws Exception {
		SqlConfig workConfig = UroboroSQL.builder("jdbc:h2:mem:SqlAgentFactoryImplTest3", "sa", "").build();
		SqlAgentFactoryImpl workFactory = (SqlAgentFactoryImpl) workConfig.getSqlAgentFactory();

		workFactory.getDefaultProps().remove(SqlAgentFactory.PROPS_KEY_SQL_RETRY_CODES);
		assertTrue(workFactory.getSqlRetryCodeList().isEmpty());
	}


	@Test
	public void setDefaultMaxRetryCount() throws Exception {
		factory.setDefaultMaxRetryCount(5);
		assertEquals(5, factory.getDefaultMaxRetryCount());
	}

	@Test
	public void setDefaultSqlRetryWaitTime() throws Exception {
		factory.setDefaultSqlRetryWaitTime(100);
		assertEquals(100, factory.getDefaultSqlRetryWaitTime());
	}

	@Test
	public void setSqlIdKeyName() throws Exception {
		factory.setSqlIdKeyName("test_key_name");
		assertEquals("test_key_name", factory.getSqlIdKeyName());
	}

	@Test
	public void setDefaultMapKeyCaseFormat() throws Exception {
		factory.setDefaultMapKeyCaseFormat(CaseFormat.PASCAL_CASE);
		assertEquals(CaseFormat.PASCAL_CASE, factory.getDefaultMapKeyCaseFormat());
	}
}