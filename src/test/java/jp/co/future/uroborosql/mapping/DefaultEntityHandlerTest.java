package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.Month;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.event.subscriber.AuditLogEventSubscriber;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.fluent.SqlEntityQuery.Nulls;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class DefaultEntityHandlerTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:DefaultEntityHandlerTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
				stmt.execute("drop table if exists test");
				stmt.execute(
						"create table if not exists test( \"ID\" NUMERIC(4),name VARCHAR(10),\"Age\" NUMERIC(5),birthday DATE,memo VARCHAR(500),lock_version INTEGER, primary key(id))");
				stmt.execute("comment on table test is 'test'");
				stmt.execute("comment on column test.\"ID\" is 'id'");
				stmt.execute("comment on column test.name is 'name'");
				stmt.execute("comment on column test.\"Age\" is 'age'");
				stmt.execute("comment on column test.birthday is 'birthday'");
				stmt.execute("comment on column test.memo is 'memo'");
				stmt.execute("comment on column test.lock_version is 'lockVersion'");

				stmt.execute("drop table if exists test_data_no_key");
				stmt.execute(
						"create table if not exists test_data_no_key( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500))");

				stmt.execute("drop table if exists test_data_multi_key");
				stmt.execute(
						"create table if not exists test_data_multi_key( id NUMERIC(4),key VARCHAR(10),name VARCHAR(10), primary key(id, key))");

				stmt.execute("drop table if exists test_data_lock_version");
				stmt.execute(
						"create table if not exists test_data_lock_version( id NUMERIC(4), name VARCHAR(10), lock_version NUMERIC(10))");

				stmt.execute("drop table if exists test_data_cyclic_lock_version");
				stmt.execute(
						"create table if not exists test_data_cyclic_lock_version( id NUMERIC(4), name VARCHAR(10), lock_version NUMERIC(10))");

				stmt.execute("drop table if exists test_data_timestamp_lock_version");
				stmt.execute(
						"create table if not exists test_data_timestamp_lock_version( id NUMERIC(4), name VARCHAR(10), lock_version BIGINT)");
				stmt.execute("drop table if exists test_data_field_increment_lock_version");
				stmt.execute(
						"create table if not exists test_data_field_increment_lock_version( id NUMERIC(4), name VARCHAR(10), lock_version SMALLINT)");
				stmt.execute("drop table if exists test_history");
				stmt.execute(
						"create table if not exists test_history( id NUMERIC(4), start_at DATE, finish_at DATE, name VARCHAR(10), primary key(id))");
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.build();
		config.getEventListenerHolder().addEventSubscriber(new AuditLogEventSubscriber());

		DefaultEntityHandler.clearCache();
		MappingUtils.clearCache();
	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.updateWith("delete from test").count();
			agent.updateWith("delete from test_data_no_key").count();
			agent.updateWith("delete from test_data_multi_key").count();
			agent.updateWith("delete from test_data_lock_version").count();
			agent.updateWith("delete from test_data_cyclic_lock_version").count();
			agent.updateWith("delete from test_data_timestamp_lock_version").count();
			agent.updateWith("delete from test_data_field_increment_lock_version").count();
			agent.updateWith("delete from test_history").count();
			agent.commit();
		}
	}

	@Test
	void testInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test1);
				var test2 = new TestEntity(2L, "name2", 21, LocalDate.of(1990, Month.APRIL, 2),
						Optional.empty());
				agent.insert(test2);
				var test3 = new TestEntity(3L, "name3", 22, LocalDate.of(1990, Month.APRIL, 3), Optional
						.of("memo3"));
				agent.insert(test3);
				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testInsert2() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity2(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity2(2L, "name2", 21, LocalDate.of(1990, Month.APRIL, 2));
				agent.insert(test2);
				var test3 = new TestEntity2(3L, "name3", 22, LocalDate.of(1990, Month.APRIL, 3));
				agent.insert(test3);
				var data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity2.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity2.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testInsert3() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity3(2L, "name2", 21, LocalDate.of(1990, Month.APRIL, 2));
				agent.insert(test2);
				var test3 = new TestEntity3(3L, "name3", 22, LocalDate.of(1990, Month.APRIL, 3));
				agent.insert(test3);
				assertThat(agent.find(TestEntity3.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity3.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity3.class, 3).orElse(null), is(test3));
			});
		}
	}

	@Test
	void testQuery1() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test1);
				var test2 = new TestEntity(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1), Optional
						.of("memo2"));
				agent.insert(test2);
				var test3 = new TestEntity(3L, "name3", 22, LocalDate.of(1990, Month.MAY, 1), Optional.empty());
				agent.insert(test3);

				var list = agent.query(TestEntity.class)
						.collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity.class)
						.equal("birthday", LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));
			});
		}
	}

	@Test
	void testQuery2() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity2(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity2(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				var test3 = new TestEntity2(3L, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);

				var list = agent.query(TestEntity2.class)
						.collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity2.class)
						.equal("birthday", LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));
			});
		}
	}

	@Test
	void testQuery3() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity3(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				var test3 = new TestEntity3(3L, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);

				var list = agent.query(TestEntity3.class)
						.collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity3.class)
						.equal("birthday", LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));
			});
		}
	}

	@Test
	void testQuery4() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity3(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				var test3 = new TestEntity3(3L, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);
				var test4 = new TestEntity3(4L, "name4", 23, null);
				agent.insert(test4);

				var count1 = agent.query(TestEntity3.class).count();
				assertThat(count1, is(4L));
				var count2 = agent.query(TestEntity3.class).count(TestEntity3.Names.Birthday);
				assertThat(count2, is(3L));

				int sum = agent.query(TestEntity3.class).sum(TestEntity3.Names.Age);
				assertThat(sum, is(86));

				int min = agent.query(TestEntity3.class).min(TestEntity3.Names.Age);
				assertThat(min, is(20));

				var minName = agent.query(TestEntity3.class).min(TestEntity3.Names.Name);
				assertThat(minName, is("name1"));

				long max = agent.query(TestEntity3.class).max(TestEntity3.Names.Id);
				assertThat(max, is(4L));

				var maxDate = agent.query(TestEntity3.class).max(TestEntity3.Names.Birthday);
				assertThat(maxDate, is(LocalDate.of(1990, Month.MAY, 1)));

				try {
					agent.query(TestEntity3.class).equal(TestEntity3.Names.Id, 1).exists(() -> {
						throw new UroborosqlRuntimeException("exists");
					});
					fail();
				} catch (UroborosqlRuntimeException ex) {
					assertThat(ex.getMessage(), is("exists"));
				}

				try {
					agent.query(TestEntity3.class).equal(TestEntity3.Names.Id, 0).exists(() -> {
						throw new UroborosqlRuntimeException("exists");
					});
				} catch (UroborosqlRuntimeException ex) {
					fail();
				}

				try {
					agent.query(TestEntity3.class).equal(TestEntity3.Names.Id, 0).notExists(() -> {
						throw new UroborosqlRuntimeException("not exists");
					});
					fail();
				} catch (UroborosqlRuntimeException ex) {
					assertThat(ex.getMessage(), is("not exists"));
				}

				try {
					agent.query(TestEntity3.class).equal(TestEntity3.Names.Id, 1).notExists(() -> {
						throw new UroborosqlRuntimeException("not exists");
					});
				} catch (UroborosqlRuntimeException ex) {
					fail();
				}
			});
		}
	}

	@Test
	void testQuery5() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity4(1L, "name1", new BigDecimal("20"),
						LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity4(2L, "name2", new BigDecimal("21"),
						LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				var test3 = new TestEntity4(3L, "name3", new BigDecimal("22"),
						LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);
				var test4 = new TestEntity4(4L, "name4", new BigDecimal("23"), null);
				agent.insert(test4);

				var count1 = agent.query(TestEntity4.class).count();
				assertThat(count1, is(4L));
				var count2 = agent.query(TestEntity4.class).count(TestEntity4.Names.Birthday);
				assertThat(count2, is(3L));

				var sum = agent.query(TestEntity4.class).sum(TestEntity4.Names.Age);
				assertThat(sum, is(new BigDecimal("86")));

				var min = agent.query(TestEntity4.class).min(TestEntity4.Names.Age);
				assertThat(min, is(new BigDecimal("20")));

				var minName = agent.query(TestEntity4.class).min(TestEntity4.Names.Name);
				assertThat(minName, is("name1"));

				long max = agent.query(TestEntity4.class).max(TestEntity4.Names.Id);
				assertThat(max, is(4L));

				var maxDate = agent.query(TestEntity4.class).max(TestEntity4.Names.Birthday);
				assertThat(maxDate, is(LocalDate.of(1990, Month.MAY, 1)));
			});
		}
	}

	@Test
	void testQuery6() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity5(1L, "name1", Optional.ofNullable(new BigDecimal("20")),
						LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity5(2L, "name2", Optional.ofNullable(new BigDecimal("21")),
						LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				var test3 = new TestEntity5(3L, "name3", Optional.ofNullable(new BigDecimal("22")),
						LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);
				var test4 = new TestEntity5(4L, "name4", Optional.empty(), null);
				agent.insert(test4);

				var count1 = agent.query(TestEntity5.class).count();
				assertThat(count1, is(4L));
				var count2 = agent.query(TestEntity5.class).count(TestEntity5.Names.Birthday);
				assertThat(count2, is(3L));

				Optional<BigDecimal> sum = agent.query(TestEntity5.class).sum(TestEntity5.Names.Age);
				assertThat(sum.orElseThrow(IllegalStateException::new), is(new BigDecimal("63")));

				Optional<BigDecimal> min = agent.query(TestEntity5.class).min(TestEntity5.Names.Age);
				assertThat(min.orElseThrow(IllegalStateException::new), is(new BigDecimal("20")));

				var minName = agent.query(TestEntity5.class).min(TestEntity5.Names.Name);
				assertThat(minName, is("name1"));

				long max = agent.query(TestEntity5.class).max(TestEntity5.Names.Id);
				assertThat(max, is(4L));

				var maxDate = agent.query(TestEntity5.class).max(TestEntity5.Names.Birthday);
				assertThat(maxDate, is(LocalDate.of(1990, Month.MAY, 1)));

			});
		}
	}

	@Test
	void testQuery7() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity5(1L, "name1", Optional.empty(),
						LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity5(2L, "name2", Optional.empty(),
						LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);

				Optional<BigDecimal> sum = agent.query(TestEntity5.class).sum(TestEntity5.Names.Age);
				assertThat(sum.isPresent(), is(false));

				Optional<BigDecimal> min = agent.query(TestEntity5.class).min(TestEntity5.Names.Age);
				assertThat(min.isPresent(), is(false));

				Optional<BigDecimal> max = agent.query(TestEntity5.class).max(TestEntity5.Names.Age);
				assertThat(max.isPresent(), is(false));

			});
		}
	}

	@Test
	void testQueryCountUnmatchColumn() throws Exception {
		assertThrows(UroborosqlRuntimeException.class, () -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					var test1 = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
					agent.insert(test1);
					var test2 = new TestEntity3(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
					agent.insert(test2);
					var test3 = new TestEntity3(3L, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
					agent.insert(test3);
					var test4 = new TestEntity3(4L, "name4", 23, null);
					agent.insert(test4);

					agent.query(TestEntity3.class).count("unmatch");
				});
			}
		});
	}

	@Test
	void testQuerySumUnmatchColumn() throws Exception {
		assertThrows(UroborosqlRuntimeException.class, () -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					var test1 = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
					agent.insert(test1);
					var test2 = new TestEntity3(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
					agent.insert(test2);
					var test3 = new TestEntity3(3L, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
					agent.insert(test3);
					var test4 = new TestEntity3(4L, "name4", 23, null);
					agent.insert(test4);

					agent.query(TestEntity3.class).sum("unmatch");
				});
			}
		});
	}

	@Test
	void testQuerySumNoNumberColumn() throws Exception {
		assertThrows(UroborosqlRuntimeException.class, () -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					var test1 = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
					agent.insert(test1);
					var test2 = new TestEntity3(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
					agent.insert(test2);
					var test3 = new TestEntity3(3L, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
					agent.insert(test3);
					var test4 = new TestEntity3(4L, "name4", 23, null);
					agent.insert(test4);

					agent.query(TestEntity3.class).sum("birthday");
				});
			}
		});
	}

	@Test
	void testQueryWithCondition() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1", 22, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo"));
				agent.insert(test1);
				var test2 = new TestEntity(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1),
						Optional.of("memo2"));
				agent.insert(test2);
				var test3 = new TestEntity(3L, "name3", 20, LocalDate.of(1990, Month.JUNE, 1), Optional.empty());
				agent.insert(test3);

				// Equal
				var list = agent.query(TestEntity.class)
						.equal("id", 2)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));

				list = agent.query(TestEntity.class)
						.equal("id", null)
						.collect();
				assertThat(list.size(), is(0));

				// EqualIfNotEmpty
				list = agent.query(TestEntity.class)
						.equalIfNotEmpty("id", 2)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));

				list = agent.query(TestEntity.class)
						.equalIfNotEmpty("id", null)
						.collect();
				assertThat(list.size(), is(3));

				// Not Equal
				list = agent.query(TestEntity.class)
						.notEqual("id", 2)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));

				list = agent.query(TestEntity.class)
						.notEqual("id", null)
						.collect();
				assertThat(list.size(), is(0));

				// Not Equal IfNotEmpty
				list = agent.query(TestEntity.class)
						.notEqualIfNotEmpty("id", 2)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));

				list = agent.query(TestEntity.class)
						.notEqualIfNotEmpty("id", null)
						.collect();
				assertThat(list.size(), is(3));

				// Greater Than
				list = agent.query(TestEntity.class)
						.greaterThan("age", 21)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				list = agent.query(TestEntity.class)
						.greaterThan("age", null)
						.collect();
				assertThat(list.size(), is(0));

				// Greater Equal
				list = agent.query(TestEntity.class)
						.greaterEqual("age", 21)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				list = agent.query(TestEntity.class)
						.greaterEqual("age", null)
						.collect();
				assertThat(list.size(), is(0));

				// Less Than
				list = agent.query(TestEntity.class)
						.lessThan("age", 21)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				list = agent.query(TestEntity.class)
						.lessThan("age", null)
						.collect();
				assertThat(list.size(), is(0));

				// Greater Equal
				list = agent.query(TestEntity.class)
						.lessEqual("age", 21)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				list = agent.query(TestEntity.class)
						.lessEqual("age", null)
						.collect();
				assertThat(list.size(), is(0));

				// In (array)
				list = agent.query(TestEntity.class)
						.in("id", 1, 2)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// In (list)
				list = agent.query(TestEntity.class)
						.in("id", List.of(2, 3))
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				assertThrows(ParameterNotFoundRuntimeException.class, () -> {
					agent.query(TestEntity.class)
							.in("id")
							.collect();
				});

				// Not In (array)
				list = agent.query(TestEntity.class)
						.notIn("id", 1, 2)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// Not In (list)
				list = agent.query(TestEntity.class)
						.notIn("id", List.of(2, 3))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				assertThrows(ParameterNotFoundRuntimeException.class, () -> {
					agent.query(TestEntity.class)
							.notIn("id")
							.collect();
				});

				// Like
				list = agent.query(TestEntity.class).like("name", "name3")
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				list = agent.query(TestEntity.class).like("name", null)
						.collect();
				assertThat(list.size(), is(3));

				// Like with wildcards (_)
				list = agent.query(TestEntity.class).like("name", "n_me_")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// Like with wildcards (%)
				list = agent.query(TestEntity.class).like("name", "name%")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// startsWith
				list = agent.query(TestEntity.class).startsWith("name", "name")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity.class).startsWith("name", null)
						.collect();
				assertThat(list.size(), is(3));

				// startsWith wildcards
				list = agent.query(TestEntity.class).startsWith("name", "%ame")
						.collect();
				assertThat(list.size(), is(0));

				// endsWith
				list = agent.query(TestEntity.class).endsWith("name", "3")
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				list = agent.query(TestEntity.class).endsWith("name", null)
						.collect();
				assertThat(list.size(), is(3));

				// endsWith wildcards
				list = agent.query(TestEntity.class).endsWith("name", "%3")
						.collect();
				assertThat(list.size(), is(0));

				// contains
				list = agent.query(TestEntity.class).contains("name", "me")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity.class).contains("name", null)
						.collect();
				assertThat(list.size(), is(3));

				list = agent.query(TestEntity.class).contains("name", "%me_")
						.collect();
				assertThat(list.size(), is(0));

				// Not Like
				list = agent.query(TestEntity.class).notLike("name", "name3")
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				list = agent.query(TestEntity.class).notLike("name", null)
						.collect();
				assertThat(list.size(), is(0));

				// Not Like with wildcards (_)
				list = agent.query(TestEntity.class).notLike("name", "name_")
						.collect();
				assertThat(list.size(), is(0));

				// Not Like with wildcards (%)
				list = agent.query(TestEntity.class).notLike("name", "name%")
						.collect();
				assertThat(list.size(), is(0));

				// notStartsWith
				list = agent.query(TestEntity.class).notStartsWith("name", "name")
						.collect();
				assertThat(list.size(), is(0));

				list = agent.query(TestEntity.class).notStartsWith("name", null)
						.collect();
				assertThat(list.size(), is(0));

				// notStartsWith wildcards
				list = agent.query(TestEntity.class).notStartsWith("name", "%name")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// notEndsWith
				list = agent.query(TestEntity.class).notEndsWith("name", "3")
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				list = agent.query(TestEntity.class).notEndsWith("name", null)
						.collect();
				assertThat(list.size(), is(0));

				// notEndsWith wildcards
				list = agent.query(TestEntity.class).notEndsWith("name", "%3")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// notContains
				list = agent.query(TestEntity.class).notContains("name", "2")
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));

				list = agent.query(TestEntity.class).notContains("name", null)
						.collect();
				assertThat(list.size(), is(0));

				// notContains wildcards
				list = agent.query(TestEntity.class).notContains("name", "_2")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// between
				list = agent.query(TestEntity.class)
						.between("birthday", LocalDate.of(1990, Month.APRIL, 15), LocalDate.of(1990, Month.MAY, 15))
						.collect(); // 4/15 <= birthday and birthday <= 5/15
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));
				list = agent.query(TestEntity.class)
						.between("birthday", LocalDate.of(1990, Month.APRIL, 1), LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				list = agent.query(TestEntity.class)
						.between("birthday", null, LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.size(), is(0));

				list = agent.query(TestEntity.class)
						.between("birthday", LocalDate.of(1990, Month.APRIL, 1), null)
						.collect();
				assertThat(list.size(), is(0));

				list = agent.query(TestEntity.class)
						.between("birthday", null, null)
						.collect();
				assertThat(list.size(), is(0));

				// not between
				list = agent.query(TestEntity.class)
						.notBetween("birthday", LocalDate.of(1990, Month.APRIL, 15), LocalDate.of(1990, Month.MAY, 15))
						.asc("id")
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));
				list = agent.query(TestEntity.class)
						.notBetween("birthday", LocalDate.of(1990, Month.APRIL, 1), LocalDate.of(1990, Month.MAY, 1))
						.asc("id")
						.collect(); // birthday < 4/1 or birthday > 5/1
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				list = agent.query(TestEntity.class)
						.notBetween("birthday", null, LocalDate.of(1990, Month.MAY, 1))
						.asc("id")
						.collect(); // birthday < null or birthday > 5/1
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				list = agent.query(TestEntity.class)
						.notBetween("birthday", LocalDate.of(1990, Month.APRIL, 1), null)
						.asc("id")
						.collect(); // birthday < 4/1 or birthday > null
				assertThat(list.size(), is(0));

				list = agent.query(TestEntity.class)
						.notBetween("birthday", null, null)
						.asc("id")
						.collect(); // birthday < 4/1 or birthday > 5/1
				assertThat(list.size(), is(0));

				// is null
				list = agent.query(TestEntity.class).isNull("memo")
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// is not null
				list = agent.query(TestEntity.class).isNotNull("memo")
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// where with param
				list = agent.query(TestEntity.class)
						.where("birthday < /*birthday1*/", "birthday1", LocalDate.of(1990, Month.APRIL, 15))
						.where("\"Age\" < /*age*/", "age", 21)
						.collect();
				assertThat(list.size(), is(0));

				list = agent.query(TestEntity.class)
						.where(null, "birthday1", LocalDate.of(1990, Month.APRIL, 15))
						.collect();
				assertThat(list.size(), is(3));

				// where with params
				Map<String, Object> paramMap = new HashMap<>();
				paramMap.put("birthday1", LocalDate.of(1990, Month.APRIL, 15));
				paramMap.put("birthday2", LocalDate.of(1990, Month.MAY, 15));
				list = agent.query(TestEntity.class)
						.where("birthday < /*birthday1*/ or BIRTHDAY > /*birthday2*/", paramMap)
						.where("\"Age\" < /*age*/", "age", 21)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// where
				list = agent.query(TestEntity.class)
						.where("birthday < /*birthday1*/ or BIRTHDAY > /*birthday2*/", paramMap)
						.where("\"Age\" < 21")
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// where
				list = agent.query(TestEntity.class)
						.where(null)
						.collect();
				assertThat(list.size(), is(3));

				// where
				list = agent.query(TestEntity.class)
						.where(null, paramMap)
						.collect();
				assertThat(list.size(), is(3));

				// where if not empty
				list = agent.query(TestEntity.class)
						.whereIfNotEmpty("birthday < /*birthday1*/", "birthday1", LocalDate.of(1990, Month.APRIL, 15))
						.collect();
				assertThat(list.size(), is(1));

				list = agent.query(TestEntity.class)
						.whereIfNotEmpty("birthday < /*birthday1*/", "birthday1", null)
						.collect();
				assertThat(list.size(), is(3));

				// where if not empty
				list = agent.query(TestEntity.class)
						.whereIfNotEmpty(null, "birthday", 1)
						.collect();
				assertThat(list.size(), is(3));

				// order by asc
				list = agent.query(TestEntity.class)
						.asc("age")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// order by asc
				list = agent.query(TestEntity.class)
						.asc("age", Nulls.FIRST)
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// order by desc
				list = agent.query(TestEntity.class)
						.desc("birthday")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// order by asc and desc
				list = agent.query(TestEntity.class)
						.asc("age")
						.desc("birthday", Nulls.FIRST)
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// limit, offset
				list = agent.query(TestEntity.class)
						.limit(2)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				list = agent.query(TestEntity.class)
						.limit(2)
						.offset(1)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				// count
				var count1 = agent.query(TestEntity.class).asc("age").count();
				assertThat(count1, is(3L));

				var count2 = agent.query(TestEntity.class).lessEqual("age", 21).count();
				assertThat(count2, is(2L));

				var count3 = agent.query(TestEntity.class).lessEqual("age", 21).count("memo");
				assertThat(count3, is(1L));

			});
		}
	}

	@Test
	void testQueryWithBetweenColumns() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestHistoryEntity(1L,
						LocalDate.of(1990, Month.APRIL, 1),
						LocalDate.of(1990, Month.APRIL, 28),
						"name1");
				agent.insert(test1);
				var test2 = new TestHistoryEntity(2L,
						LocalDate.of(1990, Month.APRIL, 15),
						LocalDate.of(1990, Month.MAY, 15),
						"name2");
				agent.insert(test2);
				var test3 = new TestHistoryEntity(3L,
						LocalDate.of(1990, Month.MARCH, 1),
						LocalDate.of(1990, Month.JUNE, 30),
						"name3");
				agent.insert(test3);

				// Between
				var list = agent.query(TestHistoryEntity.class)
						.betweenColumns(LocalDate.of(1990, Month.APRIL, 15), "start_at", "finish_at")
						.asc("id")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				list = agent.query(TestHistoryEntity.class)
						.betweenColumns(LocalDate.of(1990, Month.APRIL, 1), "start_at", "finish_at")
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));
			});
		}
	}

	@Test
	void testQueryWithNotBetweenColumns() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestHistoryEntity(1L,
						LocalDate.of(1990, Month.APRIL, 1),
						LocalDate.of(1990, Month.APRIL, 28),
						"name1");
				agent.insert(test1);
				var test2 = new TestHistoryEntity(2L,
						LocalDate.of(1990, Month.APRIL, 15),
						LocalDate.of(1990, Month.MAY, 15),
						"name2");
				agent.insert(test2);
				var test3 = new TestHistoryEntity(3L,
						LocalDate.of(1990, Month.MARCH, 1),
						LocalDate.of(1990, Month.JUNE, 30),
						"name3");
				agent.insert(test3);

				// Between
				var list = agent.query(TestHistoryEntity.class)
						.notBetweenColumns(LocalDate.of(1990, Month.APRIL, 15), "start_at", "finish_at")
						.asc("id")
						.collect(); // 4/15 < start_at or 4/15 > finish_at
				assertThat(list.size(), is(0));
				list = agent.query(TestHistoryEntity.class)
						.notBetweenColumns(LocalDate.of(1990, Month.APRIL, 1), "start_at", "finish_at")
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));
			});
		}
	}

	@Test
	void testQueryForUpdate() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				var test2 = new TestEntity3(2L, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				var test3 = new TestEntity3(3L, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);
				var test4 = new TestEntity3(4L, "name4", 23, null);
				agent.insert(test4);

				assertThat(config.getSqlAgentProvider().getDefaultForUpdateWaitSeconds(), is(10));
				config.getSqlAgentProvider().setDefaultForUpdateWaitSeconds(30);
				assertThat(config.getSqlAgentProvider().getDefaultForUpdateWaitSeconds(), is(30));

				assertThat(config.getSqlAgentProvider().isStrictForUpdateType(), is(false));
				config.getSqlAgentProvider().setStrictForUpdateType(true);
				assertThat(config.getSqlAgentProvider().isStrictForUpdateType(), is(true));

				agent.required(() -> {
					var list = agent.query(TestEntity3.class).forUpdate()
							.collect();
					assertThat(list.size(), is(4));
				});

				try {
					agent.required(() -> {
						agent.query(TestEntity3.class).forUpdateNoWait()
								.first();
					});
					fail();
				} catch (UroborosqlRuntimeException ex) {
					assertThat(ex.getMessage(), is("Unsupported for update nowait clause."));
				} catch (Throwable th) {
					fail();
				}

				try {
					agent.required(() -> {
						agent.query(TestEntity3.class).forUpdateWait().stream();
					});
					fail();
				} catch (UroborosqlRuntimeException ex) {
					assertThat(ex.getMessage(), is("Unsupported for update wait clause."));
				} catch (Throwable th) {
					fail();
				}

				try {
					agent.required(() -> {
						agent.query(TestEntity3.class).forUpdateWait(30).count();
					});
					fail();
				} catch (UroborosqlRuntimeException ex) {
					assertThat(ex.getMessage(), is("Unsupported for update wait clause."));
				} catch (Throwable th) {
					fail();
				}

				config.getSqlAgentProvider().setStrictForUpdateType(false);
				try {
					assertThat(agent.query(TestEntity3.class).forUpdateNoWait()
							.first().isPresent(), is(true));
				} catch (Throwable th) {
					fail();
				}

				try {
					assertThat(agent.query(TestEntity3.class).forUpdateWait()
							.first().isPresent(), is(true));
				} catch (Throwable th) {
					fail();
				}

				try {
					assertThat(agent.query(TestEntity3.class).forUpdateWait(10)
							.first().isPresent(), is(true));
				} catch (Throwable th) {
					fail();
				}

				try {
					agent.query(TestEntity3.class)
							.equal("id", 3).forUpdate()
							.notExists(() -> {
								throw new IllegalStateException();
							});
				} catch (Throwable th) {
					fail();
				}

			});
		}
	}

	@Test
	void testUpdate1() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));

				// Optionalなフィールドにnullを設定（更新されない）
				test.setMemo(null);
				agent.update(test);

				data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data.getMemo(), is(Optional.of("memo1")));

				// null へ更新
				test.setMemo(Optional.empty());
				agent.update(test);

				data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getMemo(), is(Optional.empty()));
			});
		}
	}

	@Test
	void testUpdate2() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity2(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	void testUpdateNoKey() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestDataNoKeyEntity.class).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	void testUpdateLockVersionSuccess() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntity3.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	void testUpdateLockVersionError() throws Exception {
		assertThrows(OptimisticLockException.class, () -> {

			try (var agent = config.agent()) {
				agent.required(() -> {
					var test = new TestEntity3(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
					test.setLockVersion(1);
					agent.insert(test);

					test.setName("updatename");
					test.setLockVersion(0);
					agent.update(test);
				});
			}
		});
	}

	@Test
	void testUpdateLockVersionOnly() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntityLockVersion(1L, "name1");
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityLockVersion.class).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	void testUpdateCyclicLockVersionMin() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntityCyclicLockVersion(1L, "name1");
				assertThat(test.getLockVersion(), is(0));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityCyclicLockVersion.class).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
				assertThat(data.getLockVersion(), is(1));
			});
		}
	}

	@Test
	void testUpdateCyclicLockVersionMax() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntityCyclicLockVersion(1L, "name1");
				test.setLockVersion(1000000000);
				assertThat(test.getLockVersion(), is(1000000000));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityCyclicLockVersion.class).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
				assertThat(data.getLockVersion(), is(1));
			});
		}
	}

	@Test
	void testUpdateTimestampLockVersion() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntityTimestampLockVersion(1L, "name1");
				assertThat(test.getLockVersion(), is(0L));
				agent.insert(test);

				var insertedData = agent.find(TestEntityTimestampLockVersion.class)
						.orElse(null);

				test.setName("updatename");
				agent.update(test);

				var updatedData = agent.find(TestEntityTimestampLockVersion.class)
						.orElse(null);
				assertThat(updatedData, is(test));
				assertThat(updatedData.getName(), is("updatename"));
				assertThat(updatedData.getLockVersion(), not(is(insertedData.getLockVersion())));
			});
		}
	}

	@Test
	void testUpdateFieldIncrementLockVersionMin() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntityFieldIncrementLockVersion(1L, "name1");
				assertThat(test.getLockVersion(), is((short) 0));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityFieldIncrementLockVersion.class)
						.orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
				assertThat(data.getLockVersion(), is((short) 1));
			});
		}
	}

	@Test
	void testUpdateFieldIncrementLockVersionMax() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntityFieldIncrementLockVersion(1L, "name1");
				test.setLockVersion(Short.MAX_VALUE);
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntityFieldIncrementLockVersion.class)
						.orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
				assertThat(data.getLockVersion(), is(Short.MIN_VALUE));
			});
		}
	}

	@Test
	void testUpdateCustomLockVersionWithNoEntry() throws Exception {
		assertThrows(UroborosqlRuntimeException.class, () -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					var test = new TestEntityCustomLockVersion(1L, "name1");
					assertThat(test.getLockVersion(), is(0L));
					agent.insert(test);

					test.setName("updatename");
					agent.update(test);
				});
			}
		});
	}

	@Test
	void testDelete1() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test);

				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testDelete2() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity2(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test);

				var data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testDeleteNothingKey() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				agent.insert(test);

				var data = agent.find(TestDataNoKeyEntity.class).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestDataNoKeyEntity.class).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testDeleteLockVersionOnly() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntityLockVersion(1L, "name1");
				agent.insert(test);

				var data = agent.find(TestEntityLockVersion.class).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityLockVersion.class).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testDeleteWithKeys() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				var test2 = new TestEntity(2L, "name2", 30, LocalDate.of(1980, Month.MAY, 1), Optional
						.of("memo2"));
				var test3 = new TestEntity(3L, "name3", 40, LocalDate.of(1970, Month.JUNE, 1), Optional
						.empty());
				agent.insert(test1);
				agent.insert(test2);
				agent.insert(test3);

				assertThat(agent.delete(TestEntity.class, 1, 3), is(2));

				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
			});
		}
	}

	@Test
	void testDeleteWithKeysForNothingKey() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				var test2 = new TestDataNoKeyEntity(2, "name2", 30, LocalDate.of(1980, Month.MAY, 1),
						Optional.of("memo2"));
				var test3 = new TestDataNoKeyEntity(3, "name3", 40, LocalDate.of(1970, Month.JUNE, 1),
						Optional.empty());
				agent.insert(test1);
				agent.insert(test2);
				agent.insert(test3);

				assertThat(agent.delete(TestDataNoKeyEntity.class, 1, 3), is(2));

				assertThat(agent.query(TestDataNoKeyEntity.class).equal("id", 1)
						.first().orElse(null), is(nullValue()));
				assertThat(agent.query(TestDataNoKeyEntity.class).equal("id", 2)
						.first().orElse(null), is(test2));
				assertThat(agent.query(TestDataNoKeyEntity.class).equal("id", 3)
						.first().orElse(null), is(nullValue()));
			});
		}
	}

	@Test
	void testDeleteWithKeyForMultiKey() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestDataMultiKeyEntity(1, "key1", "name1");
				var test2 = new TestDataMultiKeyEntity(1, "key2", "name2");
				var test3 = new TestDataMultiKeyEntity(2, "key1", "name3");
				agent.insert(test1);
				agent.insert(test2);
				agent.insert(test3);

				try {
					agent.delete(TestDataMultiKeyEntity.class, 1, 2);
					fail();
				} catch (IllegalArgumentException ex) {
					assertThat(ex.getMessage(), is("Entity has multiple keys"));
				} catch (Exception ex) {
					fail(ex.getMessage());
				}
			});
		}
	}

	@Test
	void testDeleteWithCondition() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				var test2 = new TestEntity(2L, "name2", 30, LocalDate.of(1980, Month.MAY, 1), Optional
						.of("memo2"));
				var test3 = new TestEntity(3L, "name3", 40, LocalDate.of(1970, Month.JUNE, 1), Optional
						.empty());
				agent.insert(test1);
				agent.insert(test2);
				agent.insert(test3);

				agent.setSavepoint("sp-equal");
				assertThat(agent.delete(TestEntity.class).equal("name", "name1").count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-equal");

				agent.setSavepoint("sp-notEqual");
				assertThat(agent.delete(TestEntity.class).notEqual("name", "name1").count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-notEqual");

				agent.setSavepoint("sp-greaterThan");
				assertThat(agent.delete(TestEntity.class).greaterThan("age", 30).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-greaterThan");

				agent.setSavepoint("sp-lessThan");
				assertThat(agent.delete(TestEntity.class).lessThan("age", 30).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-lessThan");

				agent.setSavepoint("sp-greaterEqual");
				assertThat(agent.delete(TestEntity.class).greaterEqual("age", 30).count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-greaterEqual");

				agent.setSavepoint("sp-lessEqual");
				assertThat(agent.delete(TestEntity.class).lessEqual("age", 30).count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-lessEqual");

				agent.setSavepoint("sp-in-array");
				assertThat(agent.delete(TestEntity.class).in("id", 1, 3).count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-in-array");

				agent.setSavepoint("sp-in-list");
				assertThat(agent.delete(TestEntity.class).in("id", List.of(1, 3)).count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-in-list");

				agent.setSavepoint("sp-notIn-array");
				assertThat(agent.delete(TestEntity.class).notIn("id", 1, 3).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-notIn-array");

				agent.setSavepoint("sp-notIn-list");
				assertThat(agent.delete(TestEntity.class).notIn("id", List.of(1, 3)).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-notIn-list");

				agent.setSavepoint("sp-like");
				assertThat(agent.delete(TestEntity.class).like("name", "name%").count(), is(3));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-like");

				agent.setSavepoint("sp-startsWith");
				assertThat(agent.delete(TestEntity.class).startsWith("name", "name").count(), is(3));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-startsWith");

				agent.setSavepoint("sp-endsWith");
				assertThat(agent.delete(TestEntity.class).endsWith("name", "2").count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-endsWith");

				agent.setSavepoint("sp-contains");
				assertThat(agent.delete(TestEntity.class).contains("name", "name").count(), is(3));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-contains");

				agent.setSavepoint("sp-notLike");
				assertThat(agent.delete(TestEntity.class).notLike("name", "name%").count(), is(0));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-notLike");

				agent.setSavepoint("sp-notStartsWith");
				assertThat(agent.delete(TestEntity.class).notStartsWith("name", "name").count(), is(0));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-notStartsWith");

				agent.setSavepoint("sp-notEndsWith");
				assertThat(agent.delete(TestEntity.class).notEndsWith("name", "2").count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-notEndsWith");

				agent.setSavepoint("sp-notContains");
				assertThat(agent.delete(TestEntity.class).notContains("name", "2").count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-notContains");

				agent.setSavepoint("sp-between");
				assertThat(agent.delete(TestEntity.class)
						.between("birthday", LocalDate.of(1980, Month.MAY, 1), LocalDate.of(1990, Month.APRIL, 1))
						.count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-between");

				agent.setSavepoint("sp-isNull");
				assertThat(agent.delete(TestEntity.class).isNull("memo").count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-isNull");

				agent.setSavepoint("sp-isNotNull");
				assertThat(agent.delete(TestEntity.class).isNotNull("memo").count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-isNotNull");

				agent.setSavepoint("sp-where");
				Map<String, Object> paramMap = new HashMap<>();
				paramMap.put("birthdayFrom", LocalDate.of(1980, Month.MAY, 1));
				paramMap.put("birthdayTo", LocalDate.of(1990, Month.APRIL, 1));
				assertThat(agent.delete(TestEntity.class)
						.where("BIRTHDAY > /*birthdayFrom*/ and BIRTHDAY <= /*birthdayTo*/", paramMap)
						.count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-where");
			});
		}
	}

	@Test
	void testBatchInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityForInserts(1L, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				var test2 = new TestEntityForInserts(2L, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				var test3 = new TestEntityForInserts(3L, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testBatchInsert2() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityForInserts(1L, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				var test2 = new TestEntityForInserts(2L, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				var test3 = new TestEntityForInserts(3L, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				var count = agent.inserts(TestEntityForInserts.class, Stream.of(test1, test2, test3),
						InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testBatchInsert3() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityForInserts(1L, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				var test2 = new TestEntityForInserts(2L, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				var test3 = new TestEntityForInserts(3L, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				var count = agent.inserts(Stream.of(test1, test2, test3), (ctx, cnt, r) -> cnt > 0,
						InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testBatchInsert4() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityForInserts(1L, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				var test2 = new TestEntityForInserts(2L, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				var test3 = new TestEntityForInserts(3L, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");
				var test4 = new TestEntityForInserts(4L, "name4", 23,
						LocalDate.of(1990, Month.APRIL, 4),
						"memo4");

				var count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, cnt, r) -> cnt == 3,
						InsertsType.BATCH);
				assertThat(count, is(4));

				var data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityForInserts.class, 4).orElse(null);
				assertThat(data, is(test4));

			});
		}
	}

	@Test
	void testBatchInsertWithOptional() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				var test2 = new TestEntity(2L, "name2", 21, LocalDate.of(1990, Month.APRIL, 2),
						Optional.empty());
				var test3 = new TestEntity(3L, "name3", 22, LocalDate.of(1990, Month.APRIL, 3),
						Optional.of("memo3"));
				var test4 = new TestEntity(4L, "name4", 23, LocalDate.of(1990, Month.APRIL, 4),
						Optional.of("memo4"));

				var count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, cnt, r) -> cnt == 3,
						InsertsType.BATCH);
				assertThat(count, is(4));

				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntity.class, 4).orElse(null);
				assertThat(data, is(test4));

			});
		}
	}

	@Test
	void testBatchInsertEmpty() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var count = agent.inserts(Stream.empty(), InsertsType.BATCH);
				assertThat(count, is(0));

				assertThat(agent.query(TestEntityForInserts.class)
						.collect().size(), is(0));
			});
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	void testBatchInsertTypeError1() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> {

			try (var agent = config.agent()) {
				agent.required(() -> {
					var test1 = new TestEntityForInserts();

					agent.inserts((Class) TestEntity.class, Stream.of(test1), InsertsType.BATCH);

				});
			}
		});
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	void testBatchInsertTypeError2() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> {

			try (var agent = config.agent()) {
				agent.required(() -> {
					var test1 = new TestEntityForInserts();

					agent.inserts((Class) int.class, Stream.of(test1), InsertsType.BATCH);

				});
			}
		});
	}

	@Test
	void testBulkInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityForInserts(1L, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				var test2 = new TestEntityForInserts(2L, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				var test3 = new TestEntityForInserts(3L, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				var data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testBulkInsert2() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityForInserts(1L, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				var test2 = new TestEntityForInserts(2L, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				var test3 = new TestEntityForInserts(3L, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				var count = agent.inserts(TestEntityForInserts.class, Stream.of(test1, test2, test3), InsertsType.BULK);
				assertThat(count, is(3));

				var data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testBulkInsert3() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityForInserts(1L, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				var test2 = new TestEntityForInserts(2L, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				var test3 = new TestEntityForInserts(3L, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				var count = agent.inserts(Stream.of(test1, test2, test3), (ctx, cnt, r) -> cnt > 0, InsertsType.BULK);
				assertThat(count, is(3));

				var data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testBulkInsert4() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityForInserts(1L, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				var test2 = new TestEntityForInserts(2L, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				var test3 = new TestEntityForInserts(3L, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");
				var test4 = new TestEntityForInserts(4L, "name4", 23,
						LocalDate.of(1990, Month.APRIL, 4),
						"memo4");

				var count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, cnt, r) -> cnt == 3,
						InsertsType.BULK);
				assertThat(count, is(4));

				var data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityForInserts.class, 4).orElse(null);
				assertThat(data, is(test4));

			});
		}
	}

	@Test
	void testBulkInsert5() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var count = agent.inserts(IntStream.range(1, 20)
						.mapToObj(i -> new TestEntityForInserts(Long.valueOf(String.valueOf(i)), "name" + i, 20 + i,
								LocalDate.of(1990, Month.APRIL, i), "memo" + i)),
						InsertsType.BULK);
				assertThat(count, is(19));

				var data = agent.find(TestEntityForInserts.class, 1).orElseThrow();
				assertThat(data.getName(), is("name1"));
				data = agent.find(TestEntityForInserts.class, 2).orElseThrow();
				assertThat(data.getName(), is("name2"));
				data = agent.find(TestEntityForInserts.class, 3).orElseThrow();
				assertThat(data.getName(), is("name3"));

			});
		}
	}

	@Test
	void testBulkInsertWithOptional() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				var test2 = new TestEntity(2L, "name2", 21, LocalDate.of(1990, Month.APRIL, 2),
						Optional.empty());
				var test3 = new TestEntity(3L, "name3", 22, LocalDate.of(1990, Month.APRIL, 3),
						Optional.of("memo3"));
				var test4 = new TestEntity(4L, "name4", 23, LocalDate.of(1990, Month.APRIL, 4),
						Optional.of("memo4"));

				var count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, cnt, r) -> cnt == 3);
				assertThat(count, is(4));

				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntity.class, 4).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	void testBulkInsertEmpty() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var count = agent.inserts(Stream.empty());
				assertThat(count, is(0));

				assertThat(agent.query(TestEntityForInserts.class)
						.collect().size(), is(0));
			});
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	void testBulkInsertTypeError1() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> {

			try (var agent = config.agent()) {
				agent.required(() -> {
					var test1 = new TestEntityForInserts();

					agent.inserts((Class) TestEntity.class, Stream.of(test1));

				});
			}
		});
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	void testBulkInsertTypeError2() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> {

			try (var agent = config.agent()) {
				agent.required(() -> {
					var test1 = new TestEntityForInserts();

					agent.inserts((Class) int.class, Stream.of(test1));

				});
			}
		});
	}

	@Test
	void testQueryNothingKey() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				agent.insert(test1);
				var test2 = new TestDataNoKeyEntity(2, "name2", 21, LocalDate.of(1990, Month.APRIL, 2),
						Optional.empty());
				agent.insert(test2);
				var test3 = new TestDataNoKeyEntity(3, "name3", 22, LocalDate.of(1990, Month.APRIL, 3),
						Optional.of("memo3"));
				agent.insert(test3);

				var list = agent.query(TestDataNoKeyEntity.class)
						.collect();
				assertThat(list.size(), is(3));
			});
		}
	}

	@Test
	void testQueryMultiKeyEntity() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestDataMultiKeyEntity(1, "key1", "name1");
				agent.insert(test1);
				var test2 = new TestDataMultiKeyEntity(1, "key2", "name2");
				agent.insert(test2);
				var test3 = new TestDataMultiKeyEntity(2, "key1", "name3");
				agent.insert(test3);

				var list = agent.query(TestDataMultiKeyEntity.class)
						.collect();
				assertThat(list.size(), is(3));
			});
		}
	}

	@Test
	void testCreateSelectContext() throws Exception {
		try (var agent = config.agent()) {
			var test = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			var metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			var ctx = handler.createSelectContext(agent, metadata, null, true);

			var sql = ctx.getSql();
			assertThat(sql, containsString("IF memo != null"));

			try (var rs = agent.query(ctx)) {
				assertThat(rs.next(), is(true));
			}
		}
	}

	@Test
	void testCreateSelectContextEmptyNotEqualsNull() throws Exception {
		try (var agent = config.agent()) {
			var test = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			handler.setEmptyStringEqualsNull(false);
			var metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			var ctx = handler.createSelectContext(agent, metadata, null, true);

			var sql = ctx.getSql();
			assertThat(sql, not(containsString("SF.isNotEmpty")));

			try (var rs = agent.query(ctx)) {
				assertThat(rs.next(), is(true));
			}

			handler.setEmptyStringEqualsNull(true);
		}
	}

	@Test
	void testCreateInsertContext() throws Exception {
		try (var agent = config.agent()) {
			EntityHandler<?> handler = config.getEntityHandler();
			var metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			var ctx = handler.createInsertContext(agent, metadata, null);

			var sql = ctx.getSql();
			assertThat(sql, containsString("IF memo != null"));

			ctx.param("id", 1)
					.param("name", "name1")
					.param("age", 20)
					.param("birthday", LocalDate.of(1990, Month.APRIL, 1))
					.param("memo", Optional.of("memo1"));
			assertThat(agent.update(ctx), is(1));
		}
	}

	@Test
	void testCreateInsertContextEmptyNotEqualsNull() throws Exception {
		try (var agent = config.agent()) {
			EntityHandler<?> handler = config.getEntityHandler();
			handler.setEmptyStringEqualsNull(false);
			var metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			var ctx = handler.createInsertContext(agent, metadata, null);

			var sql = ctx.getSql();
			assertThat(sql, not(containsString("SF.isNotEmpty")));

			ctx.param("id", 1)
					.param("name", "name1")
					.param("age", 20)
					.param("birthday", LocalDate.of(1990, Month.APRIL, 1))
					.param("memo", Optional.of("memo1"))
					.param("lockVersion", 1);
			assertThat(agent.update(ctx), is(1));

			handler.setEmptyStringEqualsNull(true);
		}
	}

	@Test
	void testCreateUpdateContext() throws Exception {
		try (var agent = config.agent()) {
			var test = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			var metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			var ctx = handler.createUpdateContext(agent, metadata, null, true);

			var sql = ctx.getSql();
			assertThat(sql, containsString("IF memo != null"));

			ctx.param("id", 1)
					.param("name", "updatename");
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).equal("id", 1)
					.first().orElse(null).getName(), is("updatename"));
		}
	}

	@Test
	void testCreateUpdateContextEmptyStringEqualsNull() throws Exception {
		try (var agent = config.agent()) {
			var test = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			handler.setEmptyStringEqualsNull(false);
			var metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			var ctx = handler.createUpdateContext(agent, metadata, null, true);

			var sql = ctx.getSql();
			assertThat(sql, not(containsString("SF.isNotEmpty")));

			ctx.param("id", 1)
					.param("name", "updatename");
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).equal("id", 1)
					.first().orElse(null).getName(), is("updatename"));

			handler.setEmptyStringEqualsNull(true);
		}
	}

	@Test
	void testCreateDeleteContext() throws Exception {
		try (var agent = config.agent()) {
			var test = new TestEntity(1L, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			var metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			var ctx = handler.createDeleteContext(agent, metadata, null, true);
			ctx.param("id", 1);
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).equal("id", 1)
					.first().orElse(null), is(nullValue()));
		}
	}

	@Test
	void testAddAndRemovePropertyMapper() throws Exception {
		EntityHandler<?> handler = config.getEntityHandler();

		var customMapper = new CustomMapper();
		handler.addPropertyMapper(customMapper);

		Class<?> clazz = handler.getClass();
		var mapperField = clazz.getDeclaredField("propertyMapperManager");
		mapperField.setAccessible(true);
		Class<?> mapperClazz = mapperField.getType();
		var mapperObj = mapperField.get(handler);
		var mappersField = mapperClazz.getDeclaredField("mappers");
		mappersField.setAccessible(true);
		@SuppressWarnings("unchecked")
		var instance = (List<PropertyMapper<?>>) mappersField.get(mapperObj);

		assertThat(instance.contains(customMapper), is(true));

		handler.removePropertyMapper(customMapper);

		assertThat(instance.contains(customMapper), is(false));

	}

	@Test
	void testClearCache() throws Exception {
		EntityHandler<?> handler = config.getEntityHandler();

		if (handler instanceof DefaultEntityHandler) {
			DefaultEntityHandler.clearCache();
		} else {
			fail();
		}

	}

	public static class Name {
		private final String s;

		public Name(final String s) {
			this.s = s;
		}
	}

	private static class CustomMapper implements PropertyMapper<Name>, BindParameterMapper<Name> {
		@Override
		public Object toJdbc(final Name original, final Connection connection,
				final BindParameterMapperManager parameterMapperManager) {
			return "-" + original.s.toLowerCase() + "-";
		}

		@Override
		public Name getValue(final JavaType type, final ResultSet rs, final int columnIndex,
				final PropertyMapperManager mapperManager)
				throws SQLException {
			var s = rs.getString(columnIndex);
			return s != null ? new Name(s.toUpperCase().replaceAll("^-", "").replaceAll("-$", "")) : null;
		}
	}

}
