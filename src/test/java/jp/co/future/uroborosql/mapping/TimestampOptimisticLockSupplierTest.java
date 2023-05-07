package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.sql.DriverManager;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.event.BeforeParseSqlEvent;
import jp.co.future.uroborosql.event.subscriber.AuditLogEventSubscriber;
import jp.co.future.uroborosql.exception.OptimisticLockException;

public class TimestampOptimisticLockSupplierTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:TimestampOptimisticLockSupplierTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
				stmt.execute("drop table if exists test_timestamp");
				stmt.execute(
						"create table if not exists test_timestamp( id NUMERIC(4) not null,name VARCHAR(10) not null, upd_datetime TIMESTAMP(9) not null, primary key(id))");
				stmt.execute("drop table if exists test_timestamptz");
				stmt.execute(
						"create table if not exists test_timestamptz( id NUMERIC(4) not null,name VARCHAR(10) not null, upd_datetime TIMESTAMP(9) WITH TIME ZONE not null, primary key(id))");
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.build();
		config.getEventListenerHolder().addEventSubscriber(new AuditLogEventSubscriber());

	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.updateWith("delete from test_timestamp").count();
			agent.updateWith("delete from test_timestamptz").count();
			agent.commit();
		}
	}

	@Test
	void testInsertTimestamp() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test1);
				var test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test2);
				var test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test3);
				var data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityTimestamp.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityTimestamp.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	void testQueryTimestamp() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test1);
				var test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test2);
				var test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test3);

				var list = agent.query(TestEntityTimestamp.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));
			});
		}
	}

	@Test
	void testUpdateTimestamp() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test = new TestEntityTimestamp(1L, "name1");
				test.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	void testUpdateTimestampWithTimezone() throws Exception {
		var zoneId = ZoneId.of("Asia/Singapore");
		Consumer<BeforeParseSqlEvent> listener = evt -> evt.getExecutionContext()
				.param(TimestampOptimisticLockSupplier.PARAM_KEY_ZONE_ID, zoneId);
		try (var agent = config.agent()) {
			config.getEventListenerHolder().addBeforeParseSqlListener(listener);
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test = new TestEntityTimestamp(1L, "name1");
				test.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		} finally {
			config.getEventListenerHolder().removeBeforeParseSqlListener(listener);
		}
	}

	@Test
	void testUpdateTimestampLockVersionError() throws Exception {
		assertThrows(OptimisticLockException.class, () -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					var now = LocalDateTime.now();
					var test = new TestEntityTimestamp(1L, "name1");
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
		});
	}

	@Test
	void testDeleteTimestamp() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test = new TestEntityTimestamp(1L, "name1");
				test.setUpdDatetime(Timestamp.valueOf(now));
				agent.insert(test);

				var data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testBatchInsertTimestamp() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				var test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				var test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityTimestamp.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityTimestamp.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	void testBatchUpdateTimestamp() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				var test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				var test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				test1.setName("name1_upd");
				test2.setName("name2_upd");
				test3.setName("name3_upd");

				List<TestEntityTimestamp> updateEntities = agent.updatesAndReturn(Stream.of(test1, test2, test3))
						.collect(Collectors.toList());
				assertThat(updateEntities.size(), is(3));

				var data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
				assertThat(data, is(updateEntities.get(0)));
				data = agent.find(TestEntityTimestamp.class, 2).orElse(null);
				assertThat(data, is(updateEntities.get(1)));
				data = agent.find(TestEntityTimestamp.class, 3).orElse(null);
				assertThat(data, is(updateEntities.get(2)));
			});
		}
	}

	@Test
	void testBulkInsertTimestamp() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityTimestamp(1L, "name1");
				test1.setUpdDatetime(Timestamp.valueOf(now));
				var test2 = new TestEntityTimestamp(2L, "name2");
				test2.setUpdDatetime(Timestamp.valueOf(now));
				var test3 = new TestEntityTimestamp(3L, "name3");
				test3.setUpdDatetime(Timestamp.valueOf(now));

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				var data = agent.find(TestEntityTimestamp.class, 1).orElse(null);
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
	void testInsertZonedDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = ZonedDateTime.now();
				var test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				var test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				var test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);
				var data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityZonedDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityZonedDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	void testQueryZonedDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = ZonedDateTime.now();
				var test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				var test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				var test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);

				var list = agent.query(TestEntityZonedDateTime.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));
			});
		}
	}

	@Test
	void testUpdateZonedDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = ZonedDateTime.now();
				var test = new TestEntityZonedDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	void testUpdateZonedDateTimeWithTimezone() throws Exception {
		var zoneId = ZoneId.of("Asia/Singapore");
		Consumer<BeforeParseSqlEvent> listener = evt -> evt.getExecutionContext()
				.param(TimestampOptimisticLockSupplier.PARAM_KEY_ZONE_ID, zoneId);
		try (var agent = config.agent()) {
			config.getEventListenerHolder().addBeforeParseSqlListener(listener);
			agent.required(() -> {
				var now = ZonedDateTime.now();
				var test = new TestEntityZonedDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		} finally {
			config.getEventListenerHolder().removeBeforeParseSqlListener(listener);
		}
	}

	@Test
	void testUpdateZonedDateTimeLockVersionError() throws Exception {
		assertThrows(OptimisticLockException.class, () -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					var now = ZonedDateTime.now();
					var test = new TestEntityZonedDateTime(1L, "name1");
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
		});
	}

	@Test
	void testDeleteZonedDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = ZonedDateTime.now();
				var test = new TestEntityZonedDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				var data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testBatchInsertZonedDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = ZonedDateTime.now();
				var test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityZonedDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityZonedDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	void testBatchUpdateZonedDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = ZonedDateTime.now();
				var test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				test1.setName("name1_upd");
				test2.setName("name2_upd");
				test3.setName("name3_upd");

				List<TestEntityZonedDateTime> updateEntities = agent.updatesAndReturn(Stream.of(test1, test2, test3))
						.collect(Collectors.toList());
				assertThat(updateEntities.size(), is(3));

				var data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
				assertThat(data, is(updateEntities.get(0)));
				data = agent.find(TestEntityZonedDateTime.class, 2).orElse(null);
				assertThat(data, is(updateEntities.get(1)));
				data = agent.find(TestEntityZonedDateTime.class, 3).orElse(null);
				assertThat(data, is(updateEntities.get(2)));
			});
		}
	}

	@Test
	void testBulkInsertZonedDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = ZonedDateTime.now();
				var test1 = new TestEntityZonedDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityZonedDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityZonedDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				var data = agent.find(TestEntityZonedDateTime.class, 1).orElse(null);
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
	void testInsertLocalDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				var test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				var test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);
				var data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityLocalDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityLocalDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	void testQueryLocalDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				var test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				var test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);

				var list = agent.query(TestEntityLocalDateTime.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));
			});
		}
	}

	@Test
	void testUpdateLocalDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test = new TestEntityLocalDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	void testUpdateLocalDateTimeWithTimezone() throws Exception {
		var zoneId = ZoneId.of("Asia/Singapore");
		Consumer<BeforeParseSqlEvent> listener = evt -> evt.getExecutionContext()
				.param(TimestampOptimisticLockSupplier.PARAM_KEY_ZONE_ID, zoneId);
		try (var agent = config.agent()) {
			config.getEventListenerHolder().addBeforeParseSqlListener(listener);
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test = new TestEntityLocalDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		} finally {
			config.getEventListenerHolder().removeBeforeParseSqlListener(listener);
		}
	}

	@Test
	void testUpdateLocalDateTimeLockVersionError() throws Exception {
		assertThrows(OptimisticLockException.class, () -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					var now = LocalDateTime.now();
					var test = new TestEntityLocalDateTime(1L, "name1");
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
		});
	}

	@Test
	void testDeleteLocalDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test = new TestEntityLocalDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				var data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testBatchInsertLocalDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityLocalDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityLocalDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	void testBatchUpdateLocalDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				test1.setName("name1_upd");
				test2.setName("name2_upd");
				test3.setName("name3_upd");

				List<TestEntityLocalDateTime> updateEntities = agent.updatesAndReturn(Stream.of(test1, test2, test3))
						.collect(Collectors.toList());
				assertThat(updateEntities.size(), is(3));

				var data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
				assertThat(data, is(updateEntities.get(0)));
				data = agent.find(TestEntityLocalDateTime.class, 2).orElse(null);
				assertThat(data, is(updateEntities.get(1)));
				data = agent.find(TestEntityLocalDateTime.class, 3).orElse(null);
				assertThat(data, is(updateEntities.get(2)));
			});
		}
	}

	@Test
	void testBulkInsertLocalDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = LocalDateTime.now();
				var test1 = new TestEntityLocalDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityLocalDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityLocalDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				var data = agent.find(TestEntityLocalDateTime.class, 1).orElse(null);
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
	void testInsertOffsetDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = OffsetDateTime.now();
				var test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				var test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				var test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);
				var data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityOffsetDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityOffsetDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	void testQueryOffsetDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = OffsetDateTime.now();
				var test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				agent.insert(test1);
				var test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				agent.insert(test2);
				var test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);
				agent.insert(test3);

				var list = agent.query(TestEntityOffsetDateTime.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));
			});
		}
	}

	@Test
	void testUpdateOffsetDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = OffsetDateTime.now();
				var test = new TestEntityOffsetDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	void testUpdateOffsetDateTimeWithTimezone() throws Exception {
		var zoneId = ZoneId.of("Asia/Singapore");
		Consumer<BeforeParseSqlEvent> listener = evt -> evt.getExecutionContext()
				.param(TimestampOptimisticLockSupplier.PARAM_KEY_ZONE_ID, zoneId);
		try (var agent = config.agent()) {
			config.getEventListenerHolder().addBeforeParseSqlListener(listener);
			agent.required(() -> {
				var now = OffsetDateTime.now();
				var test = new TestEntityOffsetDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		} finally {
			config.getEventListenerHolder().removeBeforeParseSqlListener(listener);
		}
	}

	@Test
	void testUpdateOffsetDateTimeLockVersionError() throws Exception {
		assertThrows(OptimisticLockException.class, () -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					var now = OffsetDateTime.now();
					var test = new TestEntityOffsetDateTime(1L, "name1");
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
		});
	}

	@Test
	void testDeleteOffsetDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = OffsetDateTime.now();
				var test = new TestEntityOffsetDateTime(1L, "name1");
				test.setUpdDatetime(now);
				agent.insert(test);

				var data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testBatchInsertOffsetDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = OffsetDateTime.now();
				var test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityOffsetDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityOffsetDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	void testBatchUpdateOffsetDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = OffsetDateTime.now();
				var test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				test1.setName("name1_upd");
				test2.setName("name2_upd");
				test3.setName("name3_upd");

				List<TestEntityOffsetDateTime> updateEntities = agent.updatesAndReturn(Stream.of(test1, test2, test3))
						.collect(Collectors.toList());
				assertThat(updateEntities.size(), is(3));

				var data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(updateEntities.get(0)));
				data = agent.find(TestEntityOffsetDateTime.class, 2).orElse(null);
				assertThat(data, is(updateEntities.get(1)));
				data = agent.find(TestEntityOffsetDateTime.class, 3).orElse(null);
				assertThat(data, is(updateEntities.get(2)));
			});
		}
	}

	@Test
	void testBulkInsertOffsetDateTime() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var now = OffsetDateTime.now();
				var test1 = new TestEntityOffsetDateTime(1L, "name1");
				test1.setUpdDatetime(now);
				var test2 = new TestEntityOffsetDateTime(2L, "name2");
				test2.setUpdDatetime(now);
				var test3 = new TestEntityOffsetDateTime(3L, "name3");
				test3.setUpdDatetime(now);

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				var data = agent.find(TestEntityOffsetDateTime.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityOffsetDateTime.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityOffsetDateTime.class, 3).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}
}