package jp.co.future.uroborosql.client;

import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.sql.Driver;
import java.util.ServiceLoader;

import org.junit.Before;
import org.junit.Test;

public class DriverShimTest {

	private DriverShim driver;

	@Before
	public void setUp() throws Exception {
		ServiceLoader<Driver> loader = ServiceLoader.load(Driver.class);
		driver = new DriverShim(loader.iterator().next());
	}

	@Test
	public void testConnect() throws Exception {
		var conn = driver.connect("jdbc:h2:mem:" + this.getClass().getSimpleName(), null);
		assertThat(conn, is(notNullValue()));
	}

	@Test
	public void testAcceptsURL() throws Exception {
		assertThat(driver.acceptsURL("jdbc:h2:memtest1"), is(true));
		assertThat(driver.acceptsURL("jdbc:dummy:memtest1"), is(false));
	}

	@Test
	public void testGetPropertyInfo() throws Exception {
		try {
			driver.getPropertyInfo("", null);
		} catch (Exception ex) {
			fail();
		}
	}

	@Test
	public void testGetMajorVersion() throws Exception {
		assertThat(driver.getMajorVersion(), greaterThanOrEqualTo(1));
	}

	@Test
	public void testGetMinorVersion() throws Exception {
		assertThat(driver.getMinorVersion(), greaterThan(1));
	}

	@Test
	public void testJdbcCompliant() throws Exception {
		assertThat(driver.jdbcCompliant(), is(true));
	}

	@Test
	public void testGetParentLogger() throws Exception {
		assertThat(driver.getParentLogger(), is(nullValue()));
	}

	@Test
	public void testToString() throws Exception {
		assertThat(driver.toString(), is(notNullValue()));
	}

}
