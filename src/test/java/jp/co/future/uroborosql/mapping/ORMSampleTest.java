package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Mapperのサンプル実装
 *
 * @author ota
 */
public class ORMSampleTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:ORMSampleTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute(
						"drop table if exists test");
				stmt.execute(
						"create table if not exists test( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500), primary key(id))");
			}
		}

		config = DefaultSqlConfig.getConfig(url, user, password);

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.createAgent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	@Test
	public void testGetFromKey() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				// 準備
				for (int i = 0; i < 24; i++) {
					TestEntity test = new TestEntity();
					test.setId(i + 1);
					test.setName("name" + (i + 1));
					test.setAge(20 + i);
					test.setBirthday(LocalDate.of(1990, Month.of(i % 12 + 1), 1));
					test.setMemo(Optional.of("memo text"));
					agent.insert(test);
				}
			});

			// KEYを指定して取得
			TestEntity data = agent.getFromKey(TestEntity.class, /* KEY */2).orElse(null);
			assertThat(data.getName(), is("name2"));

		}
	}

	@Test
	public void testQuery() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				// 準備
				for (int i = 0; i < 24; i++) {
					TestEntity test = new TestEntity();
					test.setId(i + 1);
					test.setName("name" + (i + 1));
					test.setAge(20 + i);
					test.setBirthday(LocalDate.of(1990, Month.of(i % 12 + 1), 1));
					test.setMemo(Optional.of("memo text"));
					agent.insert(test);
				}
			});

			// 条件なし
			List<TestEntity> list = agent.query(TestEntity.class).collect(Collectors.toList());
			assertThat(list.size(), is(24));

			// 条件あり
			Map<String, LocalDate> params = new HashMap<>();
			params.put("birthday", LocalDate.of(1990, Month.MAY, 1));
			list = agent.query(TestEntity.class, params).collect(Collectors.toList());
			assertThat(list.size(), is(2));
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				// INSERT
				TestEntity test = new TestEntity();
				test.setId(1);
				test.setName("name1");
				test.setAge(20);
				test.setBirthday(LocalDate.of(1990, Month.APRIL, 1));
				test.setMemo(Optional.of("memo text"));
				agent.insert(test);

				TestEntity data = agent.getFromKey(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	public void testUpdate() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity();
				test.setId(1);
				test.setName("name1");
				test.setAge(20);
				test.setBirthday(LocalDate.of(1990, Month.APRIL, 1));
				test.setMemo(Optional.of("memo text"));
				agent.insert(test);

				// UPDATE
				test.setName("update!!");
				agent.update(test);

				TestEntity data = agent.getFromKey(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("update!!"));
			});
		}
	}

	@Test
	public void testDelete() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity();
				test.setId(1);
				test.setName("name1");
				test.setAge(20);
				test.setBirthday(LocalDate.of(1990, Month.APRIL, 1));
				test.setMemo(Optional.of("memo text"));
				agent.insert(test);

				// DELETE
				agent.delete(test);

				TestEntity data = agent.getFromKey(TestEntity.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}
}
