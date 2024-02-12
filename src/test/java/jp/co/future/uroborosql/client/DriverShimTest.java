package jp.co.future.uroborosql.client;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.fail;

import java.sql.Driver;
import java.util.ServiceLoader;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class DriverShimTest {

	private DriverShim driver;

	@BeforeEach
	public void setUp() throws Exception {
		ServiceLoader<Driver> loader = ServiceLoader.load(Driver.class);
		driver = new DriverShim(loader.iterator().next());
	}

	@Test
	void testConnect() throws Exception {
		var conn = driver.connect("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1", null);
		assertThat(conn, is(notNullValue()));
	}

	@Test
	void testAcceptsURL() throws Exception {
		assertThat(driver.acceptsURL("jdbc:h2:memtest1"), is(true));
		assertThat(driver.acceptsURL("jdbc:dummy:memtest1"), is(false));
	}

	@Test
	void testGetPropertyInfo() throws Exception {
		try {
			driver.getPropertyInfo("", null);
		} catch (Exception ex) {
			fail();
		}
	}

	@Test
	void testGetMajorVersion() throws Exception {
		assertThat(driver.getMajorVersion(), greaterThanOrEqualTo(1));
	}

	@Test
	void testGetMinorVersion() throws Exception {
		assertThat(driver.getMinorVersion(), greaterThan(1));
	}

	@Test
	void testJdbcCompliant() throws Exception {
		assertThat(driver.jdbcCompliant(), is(true));
	}

	@Test
	void testGetParentLogger() throws Exception {
		assertThat(driver.getParentLogger(), is(nullValue()));
	}

	@Test
	void testToString() throws Exception {
		assertThat(driver.toString(), is(notNullValue()));
	}

}
