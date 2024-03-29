package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;

public class TimestampOptimisticLockSupplierTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:TimestampOptimisticLockSupplierTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute("drop table if exists test_timestamp");
				stmt.execute(
						"create table if not exists test_timestamp( id NUMERIC(4) not null,name VARCHAR(10) not null, upd_datetime TIMESTAMP not null, primary key(id))");
				stmt.execute("drop table if exists test_timestamptz");
				stmt.execute(
						"create table if not exists test_timestamptz( id NUMERIC(4) not null,name VARCHAR(10) not null, upd_datetime TIMESTAMP WITH TIME ZONE not null, primary key(id))");
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("delete from test_timestamp").count();
			agent.updateWith("delete from test_timestamptz").count();
			agent.commit();
		}
	}

	@Test
	public void testInsertTimestamp() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test1);
				TestEntityTimestamp test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test2);
				TestEntityTimestamp test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test3);
				TestEntityTimestamp data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityTimestamp.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityTimestamp.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testQueryTimestamp() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test1);
				TestEntityTimestamp test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test2);
				TestEntityTimestamp test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test3);

				List<TestEntityTimestamp> list = agent.query(TestEntityTimestamp.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));
			});
		}
	}

	@Test
	public void testUpdateTimestamp() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test = new TestEntityTimestamp(1L, "name1");
				test.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityTimestamp data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	public void testUpdateTimestampWithTimezone() throws Exception {
		ZoneId zoneId = ZoneId.of("Asia/Singapore");
		Consumer<SqlContext> autoBinder = ctx -> ctx.param(TimestampOptimisticLockSupplier.PARAM_KEY_ZONE_ID, zoneId);
		try (SqlAgent agent = config.agent()) {
			config.getSqlContextFactory().addUpdateAutoParameterBinder(autoBinder);
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test = new TestEntityTimestamp(1L, "name1");
				test.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityTimestamp data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		} finally {
			config.getSqlContextFactory().removeUpdateAutoParameterBinder(autoBinder);
		}
	}

	@Test(expected = OptimisticLockException.class)
	public void testUpdateTimestampLockVersionError() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test = new TestEntityTimestamp(1L, "name1");
				test.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test);

				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				test.setName("updatename");
				test.setUpdDatetime(Timestamp.valueOf(LocalDateTime.now()));
				agent.update(test);
			});
		}
	}

	@Test
	public void testDeleteTimestamp() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test = new TestEntityTimestamp(1L, "name1");
				test.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test);

				TestEntityTimestamp data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testBatchInsertTimestamp() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				TestEntityTimestamp test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				TestEntityTimestamp test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntityTimestamp data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityTimestamp.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityTimestamp.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testBatchUpdateTimestamp() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				TestEntityTimestamp test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				TestEntityTimestamp test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				test1.setName("name1_upd");
				test2.setName("name2_upd");
				test3.setName("name3_upd");

				List<TestEntityTimestamp> updateEntities = agent.updatesAndReturn(Stream.of(test1, test2, test3))
						.collect(Collectors.toList());
				assertThat(updateEntities.size(), is(3));

				TestEntityTimestamp data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(updateEntities.get(0)));
				data = agent.find(TestEntityTimestamp.class, 2).orElse(null);
				assertThat(data, is(updateEntities.get(1)));
				data = agent.find(TestEntityTimestamp.class, 3).orElse(null);
				assertThat(data, is(updateEntities.get(2)));
			});
		}
	}

	@Test
	public void testBulkInsertTimestamp() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityTimestamp test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				TestEntityTimestamp test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				TestEntityTimestamp test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				TestEntityTimestamp data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityTimestamp.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityTimestamp.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	// ZonedDateTime

	@Test
	public void testInsertZonedDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				TestEntityZonedDateTime test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				TestEntityZonedDateTime test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);
				TestEntityZonedDateTime data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityZonedDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityZonedDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testQueryZonedDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				TestEntityZonedDateTime test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				TestEntityZonedDateTime test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);

				List<TestEntityZonedDateTime> list = agent.query(TestEntityZonedDateTime.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));
			});
		}
	}

	@Test
	public void testUpdateZonedDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test = new TestEntityZonedDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityZonedDateTime data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	public void testUpdateZonedDateTimeWithTimezone() throws Exception {
		ZoneId zoneId = ZoneId.of("Asia/Singapore");
		Consumer<SqlContext> autoBinder = ctx -> ctx.param(TimestampOptimisticLockSupplier.PARAM_KEY_ZONE_ID, zoneId);
		try (SqlAgent agent = config.agent()) {
			config.getSqlContextFactory().addUpdateAutoParameterBinder(autoBinder);
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test = new TestEntityZonedDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityZonedDateTime data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		} finally {
			config.getSqlContextFactory().removeUpdateAutoParameterBinder(autoBinder);
		}
	}

	@Test(expected = OptimisticLockException.class)
	public void testUpdateZonedDateTimeLockVersionError() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test = new TestEntityZonedDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				test.setName("updatename");
				test.setUpdDatetime(ZonedDateTime.now());
				agent.update(test);
			});
		}
	}

	@Test
	public void testDeleteZonedDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test = new TestEntityZonedDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				TestEntityZonedDateTime data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testBatchInsertZonedDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityZonedDateTime test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityZonedDateTime test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntityZonedDateTime data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityZonedDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityZonedDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testBatchUpdateZonedDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityZonedDateTime test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityZonedDateTime test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				test1.setName("name1_upd");
				test2.setName("name2_upd");
				test3.setName("name3_upd");

				List<TestEntityZonedDateTime> updateEntities = agent.updatesAndReturn(Stream.of(test1, test2, test3))
						.collect(Collectors.toList());
				assertThat(updateEntities.size(), is(3));

				TestEntityZonedDateTime data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(updateEntities.get(0)));
				data = agent.find(TestEntityZonedDateTime.class, 2).orElse(null);
				assertThat(data, is(updateEntities.get(1)));
				data = agent.find(TestEntityZonedDateTime.class, 3).orElse(null);
				assertThat(data, is(updateEntities.get(2)));
			});
		}
	}

	@Test
	public void testBulkInsertZonedDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedDateTime now = ZonedDateTime.now();
				TestEntityZonedDateTime test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityZonedDateTime test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityZonedDateTime test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				TestEntityZonedDateTime data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityZonedDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityZonedDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	// LocalDateTime

	@Test
	public void testInsertLocalDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				TestEntityLocalDateTime test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				TestEntityLocalDateTime test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);
				TestEntityLocalDateTime data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityLocalDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityLocalDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testQueryLocalDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				TestEntityLocalDateTime test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				TestEntityLocalDateTime test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);

				List<TestEntityLocalDateTime> list = agent.query(TestEntityLocalDateTime.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));
			});
		}
	}

	@Test
	public void testUpdateLocalDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test = new TestEntityLocalDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityLocalDateTime data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	public void testUpdateLocalDateTimeWithTimezone() throws Exception {
		ZoneId zoneId = ZoneId.of("Asia/Singapore");
		Consumer<SqlContext> autoBinder = ctx -> ctx.param(TimestampOptimisticLockSupplier.PARAM_KEY_ZONE_ID, zoneId);
		try (SqlAgent agent = config.agent()) {
			config.getSqlContextFactory().addUpdateAutoParameterBinder(autoBinder);
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test = new TestEntityLocalDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityLocalDateTime data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		} finally {
			config.getSqlContextFactory().removeUpdateAutoParameterBinder(autoBinder);
		}
	}

	@Test(expected = OptimisticLockException.class)
	public void testUpdateLocalDateTimeLockVersionError() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test = new TestEntityLocalDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				test.setName("updatename");
				test.setUpdDatetime(LocalDateTime.now());
				agent.update(test);
			});
		}
	}

	@Test
	public void testDeleteLocalDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test = new TestEntityLocalDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				TestEntityLocalDateTime data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testBatchInsertLocalDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityLocalDateTime test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityLocalDateTime test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntityLocalDateTime data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityLocalDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityLocalDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testBatchUpdateLocalDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityLocalDateTime test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityLocalDateTime test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				test1.setName("name1_upd");
				test2.setName("name2_upd");
				test3.setName("name3_upd");

				List<TestEntityLocalDateTime> updateEntities = agent.updatesAndReturn(Stream.of(test1, test2, test3))
						.collect(Collectors.toList());
				assertThat(updateEntities.size(), is(3));

				TestEntityLocalDateTime data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(updateEntities.get(0)));
				data = agent.find(TestEntityLocalDateTime.class, 2).orElse(null);
				assertThat(data, is(updateEntities.get(1)));
				data = agent.find(TestEntityLocalDateTime.class, 3).orElse(null);
				assertThat(data, is(updateEntities.get(2)));
			});
		}
	}

	@Test
	public void testBulkInsertLocalDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				LocalDateTime now = LocalDateTime.now();
				TestEntityLocalDateTime test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityLocalDateTime test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityLocalDateTime test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				TestEntityLocalDateTime data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityLocalDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityLocalDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	// OffsetDateTime

	@Test
	public void testInsertOffsetDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				TestEntityOffsetDateTime test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				TestEntityOffsetDateTime test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);
				TestEntityOffsetDateTime data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityOffsetDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityOffsetDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testQueryOffsetDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				TestEntityOffsetDateTime test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				TestEntityOffsetDateTime test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);

				List<TestEntityOffsetDateTime> list = agent.query(TestEntityOffsetDateTime.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));
			});
		}
	}

	@Test
	public void testUpdateOffsetDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test = new TestEntityOffsetDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityOffsetDateTime data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	public void testUpdateOffsetDateTimeWithTimezone() throws Exception {
		ZoneId zoneId = ZoneId.of("Asia/Singapore");
		Consumer<SqlContext> autoBinder = ctx -> ctx.param(TimestampOptimisticLockSupplier.PARAM_KEY_ZONE_ID, zoneId);
		try (SqlAgent agent = config.agent()) {
			config.getSqlContextFactory().addUpdateAutoParameterBinder(autoBinder);
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test = new TestEntityOffsetDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityOffsetDateTime data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		} finally {
			config.getSqlContextFactory().removeUpdateAutoParameterBinder(autoBinder);
		}
	}

	@Test(expected = OptimisticLockException.class)
	public void testUpdateOffsetDateTimeLockVersionError() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test = new TestEntityOffsetDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				test.setName("updatename");
				test.setUpdDatetime(OffsetDateTime.now());
				agent.update(test);
			});
		}
	}

	@Test
	public void testDeleteOffsetDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test = new TestEntityOffsetDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				TestEntityOffsetDateTime data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testBatchInsertOffsetDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityOffsetDateTime test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityOffsetDateTime test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntityOffsetDateTime data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityOffsetDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityOffsetDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testBatchUpdateOffsetDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityOffsetDateTime test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityOffsetDateTime test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				test1.setName("name1_upd");
				test2.setName("name2_upd");
				test3.setName("name3_upd");

				List<TestEntityOffsetDateTime> updateEntities = agent.updatesAndReturn(Stream.of(test1, test2, test3))
						.collect(Collectors.toList());
				assertThat(updateEntities.size(), is(3));

				TestEntityOffsetDateTime data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(updateEntities.get(0)));
				data = agent.find(TestEntityOffsetDateTime.class, 2).orElse(null);
				assertThat(data, is(updateEntities.get(1)));
				data = agent.find(TestEntityOffsetDateTime.class, 3).orElse(null);
				assertThat(data, is(updateEntities.get(2)));
			});
		}
	}

	@Test
	public void testBulkInsertOffsetDateTime() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetDateTime now = OffsetDateTime.now();
				TestEntityOffsetDateTime test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				TestEntityOffsetDateTime test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				TestEntityOffsetDateTime test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				TestEntityOffsetDateTime data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityOffsetDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityOffsetDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}
}
